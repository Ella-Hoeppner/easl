//! The bytecode VM: `Op`, `Instruction`, `Function`, `Code`,
//! `BytecodeProgram`, and the `execute()` dispatch loop. The compiler that
//! produces these instructions lives in `crate::vm::compile`.

use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
  // Control & memory
  Move,
  InvokeFunction,
  Return,
  Constant,
  JumpWhen,
  JumpWhenNot,
  Jump,

  // Arithmetic
  PlusF32,
  PlusI32,
  PlusU32,
  MinusF32,
  MinusI32,
  MinusU32,
  MultiplyF32,
  MultiplyI32,
  MultiplyU32,
  DivideF32,
  DivideI32,
  DivideU32,
  RemainderF32,
  RemainderI32,
  RemainderU32,
  NegateF32,
  NegateI32,

  // Comparisons
  IsEqualU32,
  IsEqualBool,
  IsEqualF32,
  IsNotEqualU32,
  IsNotEqualBool,
  IsNotEqualF32,
  GreaterThanU32,
  GreaterThanI32,
  GreaterThanF32,
  GreaterEqualU32,
  GreaterEqualI32,
  GreaterEqualF32,
  LessThanU32,
  LessThanI32,
  LessThanF32,
  LessEqualU32,
  LessEqualI32,
  LessEqualF32,

  // Logical
  LogicalAnd,
  LogicalOr,
  LogicalNot,

  // Bit ops (treat as u32; for i32 the bit pattern is identical)
  BitAnd,
  BitOr,
  BitXor,
  BitNot,
  ShiftLeft,
  ShiftRightU32,
  ShiftRightI32,
  CountOneBits,
  CountLeadingZeros,
  CountTrailingZeros,
  ReverseBits,
  FirstLeadingBitU32,
  FirstLeadingBitI32,
  FirstTrailingBit,
  ExtractBitsU32,
  ExtractBitsI32,
  InsertBits,

  // f32 unary math
  Acos,
  Cos,
  Sin,
  Tan,
  Atan,
  Asin,
  Floor,
  Ceil,
  Round,
  Trunc,
  Fract,
  Exp,
  Exp2,
  Log,
  Log2,
  Sinh,
  Cosh,
  Tanh,
  Asinh,
  Atanh,
  Acosh,
  Sqrt,
  InvSqrt,
  AbsF32,
  AbsI32,
  SignF32,
  SignI32,
  Saturate,
  Degrees,
  Radians,

  // f32 binary math
  Pow,
  Atan2,
  Step,
  MinF32,
  MinI32,
  MinU32,
  MaxF32,
  MaxI32,
  MaxU32,
  Ldexp,

  // f32 ternary math
  Fma,
  Smoothstep,
  Mix,
  ClampF32,
  ClampI32,
  ClampU32,

  // Type conversions
  U32ToF32,
  I32ToF32,
  F32ToU32,
  F32ToI32,
  U32ToI32,
  I32ToU32,
  U32ToBool,
  I32ToBool,
  F32ToBool,
  BoolToU32,
  BoolToI32,
  BoolToF32,

  // Array access
  ArrayLookup,
  ArrayStore,
}

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
  pub op: Op,
  pub arg_positions: [u16; 3],
  pub return_position: u16,
}

impl Instruction {
  fn f32_unary(&self, stack: &mut [u32], f: impl Fn(f32) -> f32) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) = f(
        f32::from_bits(*stack.get_unchecked(self.arg_positions[0] as usize)),
      )
      .to_bits();
    }
  }
  fn f32_binary(&self, stack: &mut [u32], f: impl Fn(f32, f32) -> f32) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) = f(
        f32::from_bits(*stack.get_unchecked(self.arg_positions[0] as usize)),
        f32::from_bits(*stack.get_unchecked(self.arg_positions[1] as usize)),
      )
      .to_bits();
    }
  }
  fn f32_ternary(&self, stack: &mut [u32], f: impl Fn(f32, f32, f32) -> f32) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) = f(
        f32::from_bits(*stack.get_unchecked(self.arg_positions[0] as usize)),
        f32::from_bits(*stack.get_unchecked(self.arg_positions[1] as usize)),
        f32::from_bits(*stack.get_unchecked(self.arg_positions[2] as usize)),
      )
      .to_bits();
    }
  }
  fn i32_unary(&self, stack: &mut [u32], f: impl Fn(i32) -> i32) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) =
        f((*stack.get_unchecked(self.arg_positions[0] as usize)) as i32) as u32;
    }
  }
  fn i32_binary(&self, stack: &mut [u32], f: impl Fn(i32, i32) -> i32) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) = f(
        (*stack.get_unchecked(self.arg_positions[0] as usize)) as i32,
        (*stack.get_unchecked(self.arg_positions[1] as usize)) as i32,
      ) as u32;
    }
  }
  fn i32_ternary(&self, stack: &mut [u32], f: impl Fn(i32, i32, i32) -> i32) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) = f(
        (*stack.get_unchecked(self.arg_positions[0] as usize)) as i32,
        (*stack.get_unchecked(self.arg_positions[1] as usize)) as i32,
        (*stack.get_unchecked(self.arg_positions[2] as usize)) as i32,
      ) as u32;
    }
  }
  fn u32_unary(&self, stack: &mut [u32], f: impl Fn(u32) -> u32) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) =
        f(*stack.get_unchecked(self.arg_positions[0] as usize));
    }
  }
  fn u32_binary(&self, stack: &mut [u32], f: impl Fn(u32, u32) -> u32) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) = f(
        *stack.get_unchecked(self.arg_positions[0] as usize),
        *stack.get_unchecked(self.arg_positions[1] as usize),
      );
    }
  }
  fn u32_ternary(&self, stack: &mut [u32], f: impl Fn(u32, u32, u32) -> u32) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) = f(
        *stack.get_unchecked(self.arg_positions[0] as usize),
        *stack.get_unchecked(self.arg_positions[1] as usize),
        *stack.get_unchecked(self.arg_positions[2] as usize),
      );
    }
  }
  fn f32_cmp(&self, stack: &mut [u32], f: impl Fn(f32, f32) -> bool) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) = f(
        f32::from_bits(*stack.get_unchecked(self.arg_positions[0] as usize)),
        f32::from_bits(*stack.get_unchecked(self.arg_positions[1] as usize)),
      ) as u32;
    }
  }
  fn i32_cmp(&self, stack: &mut [u32], f: impl Fn(i32, i32) -> bool) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) = f(
        (*stack.get_unchecked(self.arg_positions[0] as usize)) as i32,
        (*stack.get_unchecked(self.arg_positions[1] as usize)) as i32,
      ) as u32;
    }
  }
  fn u32_cmp(&self, stack: &mut [u32], f: impl Fn(u32, u32) -> bool) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) = f(
        *stack.get_unchecked(self.arg_positions[0] as usize),
        *stack.get_unchecked(self.arg_positions[1] as usize),
      ) as u32;
    }
  }
  fn max_touched_index(&self) -> u16 {
    match self.op {
      Op::InvokeFunction | Op::Return | Op::Jump => 0,
      Op::Constant => self.return_position,
      Op::Move => {
        if self.arg_positions[1] == 0 {
          0
        } else {
          self.return_position.max(self.arg_positions[0])
            + self.arg_positions[1]
            - 1
        }
      }
      Op::ArrayLookup | Op::ArrayStore => self
        .return_position
        .max(self.arg_positions[0])
        .max(self.arg_positions[1]),
      Op::JumpWhen | Op::JumpWhenNot => self.arg_positions[0],
      _ => self
        .return_position
        .max(self.arg_positions.iter().copied().max().unwrap()),
    }
  }
}

pub struct Function {
  pub instructions: Range<u32>,
  pub return_position: u16,
}

pub struct Code {
  pub function_instructions: Vec<Instruction>,
  pub functions: Vec<Function>,
  /// Index of a synthetic function that populates global var slots with
  /// their initial values. `Some` when the program has at least one
  /// initialized global; `from_code` runs it once on construction so the
  /// globals are live before any user function executes. `None` when there
  /// are no initializers (no setup work needed).
  pub init_function_index: Option<usize>,
}

pub struct BytecodeProgram {
  pub code: Code,
  pub stack: Vec<u32>,
  pub call_stack: Vec<Range<u32>>,
}

impl BytecodeProgram {
  pub fn from_code(code: Code) -> Self {
    let max_stack_size = code
      .function_instructions
      .iter()
      .map(Instruction::max_touched_index)
      .max()
      .map_or(0, |i| i as usize + 1);
    let mut program = Self {
      stack: vec![0u32; max_stack_size],
      call_stack: Vec::with_capacity(code.functions.len()),
      code,
    };
    if let Some(init_idx) = program.code.init_function_index {
      program.prepare_to_run_function(init_idx);
      program.execute();
    }
    program
  }
  pub fn prepare_to_run_function(&mut self, function_index: usize) {
    self.call_stack.clear();
    self
      .call_stack
      .push(self.code.functions[function_index].instructions.clone());
  }
  pub fn get_function_return_position(&self, function_index: usize) -> u16 {
    self.code.functions[function_index].return_position
  }
  pub fn execute(&mut self) {
    let Self {
      code,
      stack,
      call_stack,
    } = self;
    let Some(mut ip) = call_stack.pop() else {
      return;
    };
    loop {
      let Some(instruction_index) = ip.next() else {
        let Some(return_ip) = call_stack.pop() else {
          return;
        };
        ip = return_ip;
        continue;
      };
      let instruction = unsafe {
        *code
          .function_instructions
          .get_unchecked(instruction_index as usize)
      };
      match instruction.op {
        Op::InvokeFunction => unsafe {
          call_stack.push(ip.clone());
          ip = code
            .functions
            .get_unchecked(instruction.arg_positions[0] as usize)
            .instructions
            .clone();
        },
        Op::Return => {
          let Some(return_ip) = call_stack.pop() else {
            return;
          };
          ip = return_ip;
        }
        Op::Move => unsafe {
          let base = stack.as_mut_ptr();
          std::ptr::copy(
            base.add(instruction.arg_positions[0] as usize),
            base.add(instruction.return_position as usize),
            instruction.arg_positions[1] as usize,
          );
        },
        Op::Constant => unsafe {
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            ((instruction.arg_positions[0] as u32) << 16u32)
              | instruction.arg_positions[1] as u32;
        },
        Op::JumpWhen => unsafe {
          let condition =
            *stack.get_unchecked(instruction.arg_positions[0] as usize);
          if condition != 0 {
            let new_pos = (instruction.arg_positions[1] as u32) << 16u32
              | (instruction.arg_positions[2] as u32);
            ip.start = new_pos;
          }
        },
        Op::JumpWhenNot => unsafe {
          let condition =
            *stack.get_unchecked(instruction.arg_positions[0] as usize);
          if condition == 0 {
            let new_pos = (instruction.arg_positions[1] as u32) << 16u32
              | (instruction.arg_positions[2] as u32);
            ip.start = new_pos;
          }
        },
        Op::Jump => {
          let new_pos = (instruction.arg_positions[0] as u32) << 16u32
            | (instruction.arg_positions[1] as u32);
          ip.start = new_pos;
        }

        // --- Arithmetic ---
        Op::PlusF32 => instruction.f32_binary(stack, |a, b| a + b),
        Op::PlusI32 => instruction.i32_binary(stack, |a, b| a.wrapping_add(b)),
        Op::PlusU32 => instruction.u32_binary(stack, |a, b| a.wrapping_add(b)),
        Op::MinusF32 => instruction.f32_binary(stack, |a, b| a - b),
        Op::MinusI32 => instruction.i32_binary(stack, |a, b| a.wrapping_sub(b)),
        Op::MinusU32 => instruction.u32_binary(stack, |a, b| a.wrapping_sub(b)),
        Op::MultiplyF32 => instruction.f32_binary(stack, |a, b| a * b),
        Op::MultiplyI32 => {
          instruction.i32_binary(stack, |a, b| a.wrapping_mul(b))
        }
        Op::MultiplyU32 => {
          instruction.u32_binary(stack, |a, b| a.wrapping_mul(b))
        }
        Op::DivideF32 => instruction.f32_binary(stack, |a, b| a / b),
        Op::DivideI32 => {
          instruction.i32_binary(stack, |a, b| a.wrapping_div(b))
        }
        Op::DivideU32 => instruction.u32_binary(stack, |a, b| a / b),
        Op::RemainderF32 => instruction.f32_binary(stack, |a, b| a % b),
        Op::RemainderI32 => {
          instruction.i32_binary(stack, |a, b| a.wrapping_rem(b))
        }
        Op::RemainderU32 => instruction.u32_binary(stack, |a, b| a % b),
        Op::NegateF32 => instruction.f32_unary(stack, |a| -a),
        Op::NegateI32 => instruction.i32_unary(stack, |a| a.wrapping_neg()),

        // --- Comparisons ---
        Op::IsEqualU32 => instruction.u32_cmp(stack, |a, b| a == b),
        Op::IsEqualBool => {
          instruction.u32_cmp(stack, |a, b| (a == 0) == (b == 0))
        }
        Op::IsEqualF32 => instruction.f32_cmp(stack, |a, b| a == b),
        Op::IsNotEqualU32 => instruction.u32_cmp(stack, |a, b| a != b),
        Op::IsNotEqualBool => {
          instruction.u32_cmp(stack, |a, b| (a == 0) != (b == 0))
        }
        Op::IsNotEqualF32 => instruction.f32_cmp(stack, |a, b| a != b),
        Op::GreaterThanU32 => instruction.u32_cmp(stack, |a, b| a > b),
        Op::GreaterThanI32 => instruction.i32_cmp(stack, |a, b| a > b),
        Op::GreaterThanF32 => instruction.f32_cmp(stack, |a, b| a > b),
        Op::GreaterEqualU32 => instruction.u32_cmp(stack, |a, b| a >= b),
        Op::GreaterEqualI32 => instruction.i32_cmp(stack, |a, b| a >= b),
        Op::GreaterEqualF32 => instruction.f32_cmp(stack, |a, b| a >= b),
        Op::LessThanU32 => instruction.u32_cmp(stack, |a, b| a < b),
        Op::LessThanI32 => instruction.i32_cmp(stack, |a, b| a < b),
        Op::LessThanF32 => instruction.f32_cmp(stack, |a, b| a < b),
        Op::LessEqualU32 => instruction.u32_cmp(stack, |a, b| a <= b),
        Op::LessEqualI32 => instruction.i32_cmp(stack, |a, b| a <= b),
        Op::LessEqualF32 => instruction.f32_cmp(stack, |a, b| a <= b),

        // --- Logical ---
        Op::LogicalAnd => instruction.u32_cmp(stack, |a, b| a != 0 && b != 0),
        Op::LogicalOr => instruction.u32_cmp(stack, |a, b| a != 0 || b != 0),
        Op::LogicalNot => instruction.u32_unary(stack, |a| (a == 0) as u32),

        // --- Bit ops ---
        Op::BitAnd => instruction.u32_binary(stack, |a, b| a & b),
        Op::BitOr => instruction.u32_binary(stack, |a, b| a | b),
        Op::BitXor => instruction.u32_binary(stack, |a, b| a ^ b),
        Op::BitNot => instruction.u32_unary(stack, |a| !a),
        Op::ShiftLeft => instruction.u32_binary(stack, |a, b| a << (b & 31)),
        Op::ShiftRightU32 => {
          instruction.u32_binary(stack, |a, b| a >> (b & 31))
        }
        Op::ShiftRightI32 => {
          instruction.i32_binary(stack, |a, b| a >> ((b as u32 & 31) as i32))
        }
        Op::CountOneBits => instruction.u32_unary(stack, |a| a.count_ones()),
        Op::CountLeadingZeros => {
          instruction.u32_unary(stack, |a| a.leading_zeros())
        }
        Op::CountTrailingZeros => {
          instruction.u32_unary(stack, |a| a.trailing_zeros())
        }
        Op::ReverseBits => instruction.u32_unary(stack, |a| a.reverse_bits()),
        Op::FirstLeadingBitU32 => instruction.u32_unary(stack, |a| {
          if a == 0 {
            u32::MAX
          } else {
            31 - a.leading_zeros()
          }
        }),
        Op::FirstLeadingBitI32 => instruction.i32_unary(stack, |a| {
          // WGSL: highest bit that differs from the sign bit, or -1 if none.
          if a == 0 || a == -1 {
            -1
          } else if a > 0 {
            31 - (a as u32).leading_zeros() as i32
          } else {
            31 - ((!a) as u32).leading_zeros() as i32
          }
        }),
        Op::FirstTrailingBit => instruction.u32_unary(stack, |a| {
          if a == 0 { u32::MAX } else { a.trailing_zeros() }
        }),
        Op::ExtractBitsU32 => {
          instruction.u32_ternary(stack, |e, offset, count| {
            let offset = offset & 31;
            let count = count & 31;
            let total = offset + count;
            if count == 0 {
              0
            } else if total > 32 {
              // matching WGSL semantics: if offset+count > 32, behavior is
              // a shift that effectively gives 0 high bits.
              (e >> offset)
                & ((1u64.checked_shl(count).unwrap_or(0).wrapping_sub(1))
                  as u32)
            } else if count == 32 {
              e >> offset
            } else {
              (e >> offset) & ((1u32 << count) - 1)
            }
          })
        }
        Op::ExtractBitsI32 => {
          instruction.i32_ternary(stack, |e, offset, count| {
            let offset = (offset as u32) & 31;
            let count = (count as u32) & 31;
            if count == 0 {
              0
            } else if count == 32 {
              e >> offset
            } else {
              let shifted = (e as u32) >> offset;
              let mask = (1u32 << count) - 1;
              let bits = shifted & mask;
              // sign-extend from `count` bits
              let sign_bit = 1u32 << (count - 1);
              if bits & sign_bit != 0 {
                (bits | !mask) as i32
              } else {
                bits as i32
              }
            }
          })
        }
        Op::InsertBits => instruction.u32_ternary(stack, |_, _, _| {
          // InsertBits is 4-arg (e, newbits, offset, count); we only have 3
          // slots so this op is unused in current emission. Placeholder.
          0
        }),

        // --- f32 unary math ---
        Op::Cos => instruction.f32_unary(stack, f32::cos),
        Op::Sin => instruction.f32_unary(stack, f32::sin),
        Op::Tan => instruction.f32_unary(stack, f32::tan),
        Op::Acos => instruction.f32_unary(stack, f32::acos),
        Op::Asin => instruction.f32_unary(stack, f32::asin),
        Op::Atan => instruction.f32_unary(stack, f32::atan),
        Op::Sinh => instruction.f32_unary(stack, f32::sinh),
        Op::Cosh => instruction.f32_unary(stack, f32::cosh),
        Op::Tanh => instruction.f32_unary(stack, f32::tanh),
        Op::Asinh => instruction.f32_unary(stack, f32::asinh),
        Op::Acosh => instruction.f32_unary(stack, f32::acosh),
        Op::Atanh => instruction.f32_unary(stack, f32::atanh),
        Op::Floor => instruction.f32_unary(stack, f32::floor),
        Op::Ceil => instruction.f32_unary(stack, f32::ceil),
        Op::Round => instruction.f32_unary(stack, |a| {
          // WGSL round is round-half-to-even (banker's rounding); Rust's
          // f32::round is round-half-away-from-zero. Use round_ties_even.
          a.round_ties_even()
        }),
        Op::Trunc => instruction.f32_unary(stack, f32::trunc),
        Op::Fract => instruction.f32_unary(stack, |a| a - a.floor()),
        Op::Exp => instruction.f32_unary(stack, f32::exp),
        Op::Exp2 => instruction.f32_unary(stack, f32::exp2),
        Op::Log => instruction.f32_unary(stack, f32::ln),
        Op::Log2 => instruction.f32_unary(stack, f32::log2),
        Op::Sqrt => instruction.f32_unary(stack, f32::sqrt),
        Op::InvSqrt => instruction.f32_unary(stack, |a| 1.0 / a.sqrt()),
        Op::AbsF32 => instruction.f32_unary(stack, f32::abs),
        Op::AbsI32 => instruction.i32_unary(stack, |a| a.wrapping_abs()),
        Op::SignF32 => instruction.f32_unary(stack, |a| {
          if a > 0.0 {
            1.0
          } else if a < 0.0 {
            -1.0
          } else {
            // includes +0.0, -0.0, NaN; WGSL sign(0) == 0, sign(NaN) == 0
            0.0
          }
        }),
        Op::SignI32 => instruction.i32_unary(stack, |a| a.signum()),
        Op::Saturate => instruction.f32_unary(stack, |a| a.clamp(0.0, 1.0)),
        Op::Degrees => {
          instruction.f32_unary(stack, |a| a * (180.0 / std::f32::consts::PI))
        }
        Op::Radians => {
          instruction.f32_unary(stack, |a| a * (std::f32::consts::PI / 180.0))
        }

        // --- f32 binary math ---
        Op::Pow => instruction.f32_binary(stack, f32::powf),
        Op::Atan2 => instruction.f32_binary(stack, f32::atan2),
        Op::Step => instruction
          .f32_binary(stack, |edge, x| if x < edge { 0.0 } else { 1.0 }),
        Op::MinF32 => instruction.f32_binary(stack, f32::min),
        Op::MinI32 => instruction.i32_binary(stack, i32::min),
        Op::MinU32 => instruction.u32_binary(stack, u32::min),
        Op::MaxF32 => instruction.f32_binary(stack, f32::max),
        Op::MaxI32 => instruction.i32_binary(stack, i32::max),
        Op::MaxU32 => instruction.u32_binary(stack, u32::max),
        Op::Ldexp => unsafe {
          let x = f32::from_bits(
            *stack.get_unchecked(instruction.arg_positions[0] as usize),
          );
          let e = (*stack.get_unchecked(instruction.arg_positions[1] as usize))
            as i32;
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            (x * (2.0f32).powi(e)).to_bits();
        },

        // --- f32 ternary math ---
        Op::Fma => instruction.f32_ternary(stack, |a, b, c| a.mul_add(b, c)),
        Op::Smoothstep => instruction.f32_ternary(stack, |edge0, edge1, x| {
          let t = ((x - edge0) / (edge1 - edge0)).clamp(0.0, 1.0);
          t * t * (3.0 - 2.0 * t)
        }),
        Op::Mix => {
          instruction.f32_ternary(stack, |a, b, t| a * (1.0 - t) + b * t)
        }
        Op::ClampF32 => {
          instruction.f32_ternary(stack, |x, lo, hi| x.clamp(lo, hi))
        }
        Op::ClampI32 => {
          instruction.i32_ternary(stack, |x, lo, hi| x.clamp(lo, hi))
        }
        Op::ClampU32 => {
          instruction.u32_ternary(stack, |x, lo, hi| x.clamp(lo, hi))
        }

        // --- Conversions ---
        Op::U32ToF32 => instruction.u32_unary(stack, |a| (a as f32).to_bits()),
        Op::I32ToF32 => {
          instruction.i32_unary(stack, |a| (a as f32).to_bits() as i32)
        }
        Op::F32ToU32 => unsafe {
          let x = f32::from_bits(
            *stack.get_unchecked(instruction.arg_positions[0] as usize),
          );
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            x as u32;
        },
        Op::F32ToI32 => unsafe {
          let x = f32::from_bits(
            *stack.get_unchecked(instruction.arg_positions[0] as usize),
          );
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            (x as i32) as u32;
        },
        Op::U32ToI32 => instruction.u32_unary(stack, |a| a),
        Op::I32ToU32 => instruction.u32_unary(stack, |a| a),
        Op::U32ToBool => instruction.u32_unary(stack, |a| (a != 0) as u32),
        Op::I32ToBool => {
          instruction.u32_unary(stack, |a| ((a as i32) != 0) as u32)
        }
        Op::F32ToBool => {
          instruction.u32_unary(stack, |a| (f32::from_bits(a) != 0.0) as u32)
        }
        Op::BoolToU32 => instruction.u32_unary(stack, |a| (a != 0) as u32),
        Op::BoolToI32 => instruction.u32_unary(stack, |a| (a != 0) as u32),
        Op::BoolToF32 => instruction.u32_unary(stack, |a| {
          if a != 0 {
            1.0f32.to_bits()
          } else {
            0.0f32.to_bits()
          }
        }),

        Op::ArrayLookup => unsafe {
          let [arr_pos, index_pos, inner_data_size] = instruction.arg_positions;
          let idx = *stack.get_unchecked(index_pos as usize);
          let src =
            (arr_pos as usize) + (idx as usize) * (inner_data_size as usize);
          let base = stack.as_mut_ptr();
          std::ptr::copy(
            base.add(src),
            base.add(instruction.return_position as usize),
            inner_data_size as usize,
          );
        },

        Op::ArrayStore => unsafe {
          let [src_pos, index_pos, inner_data_size] = instruction.arg_positions;
          let idx = *stack.get_unchecked(index_pos as usize);
          let dst = (instruction.return_position as usize)
            + (idx as usize) * (inner_data_size as usize);
          let base = stack.as_mut_ptr();
          std::ptr::copy(
            base.add(src_pos as usize),
            base.add(dst),
            inner_data_size as usize,
          );
        },
      }
    }
  }
}
