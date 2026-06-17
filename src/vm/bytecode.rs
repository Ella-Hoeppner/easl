use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
  Move,
  InvokeFunction,
  Return,
  Constant,
  JumpWhen,
  JumpWhenNot,
  Jump,
  PlusF32,
  PlusI32,
  PlusU32,
  MinusF32,
  MinusI32,
  MinusU32,
  IsEqualU32,
  IsEqualBool,
  IsEqualF32,
  GreaterThanU32,
  GreaterThanI32,
  GreaterThanF32,
  ArrayLookup,
  LessThanU32,
  LessThanI32,
  LessThanF32,
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
  Exp,
  Exp2,
  Log,
  Log2,
  Pow,
  Sinh,
  Cosh,
  Tanh,
  Asinh,
  Atanh,
  Acosh,
  Fma,
  Ldexp,
  Sqrt,
  Atan2,
  InvSqrt,
  Step,
  Smoothstep,
  U32ToF32,
  I32ToF32,
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
  fn i32_binary(&self, stack: &mut [u32], f: impl Fn(i32, i32) -> i32) {
    unsafe {
      *stack.get_unchecked_mut(self.return_position as usize) = f(
        (*stack.get_unchecked(self.arg_positions[0] as usize)) as i32,
        (*stack.get_unchecked(self.arg_positions[1] as usize)) as i32,
      ) as u32;
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
  fn max_touched_index(&self) -> u16 {
    match self.op {
      Op::InvokeFunction | Op::Jump => 0,
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
      Op::ArrayLookup => self
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
    Self {
      stack: vec![0u32; max_stack_size],
      call_stack: Vec::with_capacity(code.functions.len()),
      code,
    }
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
        Op::IsEqualU32 => unsafe {
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            (stack.get_unchecked(instruction.arg_positions[0] as usize)
              == stack.get_unchecked(instruction.arg_positions[1] as usize))
              as u32;
        },
        Op::IsEqualBool => unsafe {
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            ((*stack.get_unchecked(instruction.arg_positions[0] as usize) == 0)
              == (*stack.get_unchecked(instruction.arg_positions[1] as usize)
                == 0)) as u32;
        },
        Op::IsEqualF32 => unsafe {
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            (f32::from_bits(
              *stack.get_unchecked(instruction.arg_positions[0] as usize),
            ) == f32::from_bits(
              *stack.get_unchecked(instruction.arg_positions[1] as usize),
            )) as u32;
        },
        Op::Cos => instruction.f32_unary(stack, f32::cos),
        Op::PlusF32 => instruction.f32_binary(stack, |a, b| a + b),
        Op::PlusU32 => instruction.u32_binary(stack, |a, b| a + b),
        Op::PlusI32 => instruction.i32_binary(stack, |a, b| a + b),
        Op::MinusF32 => instruction.f32_binary(stack, |a, b| a - b),
        Op::MinusU32 => instruction.u32_binary(stack, |a, b| a - b),
        Op::MinusI32 => instruction.i32_binary(stack, |a, b| a - b),
        Op::GreaterThanU32 => unsafe {
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            (*stack.get_unchecked(instruction.arg_positions[0] as usize)
              > *stack.get_unchecked(instruction.arg_positions[1] as usize))
              as u32;
        },
        Op::LessThanU32 => unsafe {
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            ((*stack.get_unchecked(instruction.arg_positions[0] as usize))
              < (*stack.get_unchecked(instruction.arg_positions[1] as usize)))
              as u32;
        },
        Op::LessThanI32 => unsafe {
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            ((*stack.get_unchecked(instruction.arg_positions[0] as usize)
              as i32)
              < (*stack.get_unchecked(instruction.arg_positions[1] as usize)
                as i32)) as u32;
        },
        Op::U32ToF32 => unsafe {
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            (*stack.get_unchecked(instruction.arg_positions[0] as usize)
              as f32)
              .to_bits();
        },
        Op::ArrayLookup => unsafe {
          let [arr_pos, index_pos, inner_data_size] = instruction.arg_positions;
          let idx = *stack.get_unchecked(index_pos as usize);
          let src = (arr_pos as usize)
            + (idx as usize) * (inner_data_size as usize);
          let base = stack.as_mut_ptr();
          std::ptr::copy(
            base.add(src),
            base.add(instruction.return_position as usize),
            inner_data_size as usize,
          );
        },
        other => todo!("haven't implemented VM op \"{other:?}\" yet"),
      }
    }
  }
}
