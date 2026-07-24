//! The bytecode compiler: lowers a fully-validated `TypedExp` tree into
//! `Instruction`s for the VM in `crate::vm::bytecode`. The two halves only
//! meet through `Code` / `BytecodeProgram`.

use std::collections::HashMap;
use std::ops::Range;
use std::sync::{Arc, RwLock};

use crate::compiler::enums::Enum;
use crate::compiler::expression::{Accessor, ExpKind, Number, TypedExp};
use crate::compiler::functions::{
  FunctionImplementationKind, Ownership, TopLevelFunction,
  extract_mat_size as parse_mat_size,
};
use crate::compiler::types::{ConcreteArraySize, Type, TypeState};
use crate::vm::bytecode::{
  BytecodeProgram, Code, Function, HostBinding, HostBindingStorage,
  HostDispatch, HostOp, Instruction, Op, WindowQueryKind,
};

// =============================================================================
// Bytecode compilation
// =============================================================================
//
// Everything below this point is the bytecode compiler. The VM (above) and the
// compiler (below) only meet through the `Code` / `BytecodeProgram` types: the
// compiler emits a Code, the VM executes it.

#[derive(Debug, Clone)]
pub struct IntermediateBytecodeFunction {
  pub name: Arc<str>,
  pub instructions: Range<u32>,
  pub stack_frame_start: u16,
  pub arg_positions: Vec<u16>,
  pub arg_sizes: Vec<u16>,
}

pub struct PendingRefFnUsage {
  pub name: Arc<str>,
  pub fn_dispatch_position: u32,
  pub arg_move_positions: Vec<u32>,
  pub return_move_position: u32,
  pub arg_positions: Vec<u16>,
}

pub struct BytecodeCompilationState {
  pub consumed_stack_space: u16,
  /// CPU-runtime mode: compile the `@cpu` entry and its callees, lowering
  /// CPU-exclusive builtins to host ops and emitting sync-check
  /// instructions. False for audio-mode compilation (the current default),
  /// which never emits `Op::HostCall`.
  pub cpu_mode: bool,
  pub host_ops: Vec<HostOp>,
  pub host_types: Vec<Type>,
  pub host_strings: Vec<Arc<str>>,
  pub host_bindings: Vec<HostBinding>,
  pub host_dispatches: Vec<HostDispatch>,
  /// Name → index into `host_bindings` for GPU-bound globals.
  pub binding_indices: HashMap<Arc<str>, u16>,
  /// Globals whose values live host-side (unsized arrays, textures) rather
  /// than in VM stack slots.
  pub dynamic_globals: HashMap<Arc<str>, u16>,
  pub globals: HashMap<Arc<str>, u16>,
  /// Name, base stack slot, and size in u32 slots of each global, in
  /// declaration order. Carried into `Code::globals` by `finalize` so
  /// external integrations can locate globals in the running program.
  pub global_slots: Vec<(Arc<str>, u16, u16)>,
  pub locals: HashMap<Arc<str>, u16>,
  pub instructions: Vec<Instruction>,
  pub finished_functions: Vec<IntermediateBytecodeFunction>,
  pub loop_start_instructions: Vec<u32>,
  pub break_jump_instruction_positions: Vec<Vec<u32>>,
  pub continue_jump_instruction_positions: Vec<Vec<u32>>,
  pub ref_arg_functions: Vec<(Arc<str>, Arc<RwLock<TopLevelFunction>>)>,
  pub current_function: Option<IntermediateBytecodeFunction>,
  pub pending_ref_arg_function_usages: Vec<PendingRefFnUsage>,
  pub array_mut_ref_store_instructions: Vec<Vec<Instruction>>,
}

impl BytecodeCompilationState {
  pub fn new() -> Self {
    Self {
      consumed_stack_space: 0,
      cpu_mode: false,
      host_ops: vec![],
      host_types: vec![],
      host_strings: vec![],
      host_bindings: vec![],
      host_dispatches: vec![],
      binding_indices: HashMap::new(),
      dynamic_globals: HashMap::new(),
      globals: HashMap::new(),
      global_slots: vec![],
      locals: HashMap::new(),
      instructions: vec![],
      finished_functions: vec![],
      current_function: None,
      loop_start_instructions: vec![],
      break_jump_instruction_positions: vec![],
      continue_jump_instruction_positions: vec![],
      ref_arg_functions: vec![],
      pending_ref_arg_function_usages: vec![],
      array_mut_ref_store_instructions: vec![],
    }
  }
  pub fn take_stack_slot(&mut self, size: u16) -> u16 {
    let i = self.consumed_stack_space;
    self.consumed_stack_space += size;
    i as u16
  }
  pub fn open_function(&mut self, name: Arc<str>) {
    let instruction_start = self.instructions.len() as u32;
    self.current_function = Some(IntermediateBytecodeFunction {
      name,
      instructions: instruction_start..instruction_start,
      stack_frame_start: self.consumed_stack_space,
      arg_positions: vec![],
      arg_sizes: vec![],
    });
  }
  pub fn close_function(&mut self) {
    let mut f = self.current_function.take().unwrap();
    f.instructions.end = self.instructions.len() as u32;
    self.finished_functions.push(f);
  }
  pub fn push_instruction(&mut self, instruction: Instruction) {
    self.instructions.push(instruction);
  }

  // --- Basic emit helpers ---

  /// Emit a 1-slot unary op and return the result slot.
  pub fn emit_unary(&mut self, op: Op, arg: u16) -> u16 {
    let result = self.take_stack_slot(1);
    self.push_instruction(Instruction {
      op,
      arg_positions: [arg, 0, 0],
      return_position: result,
    });
    result
  }
  /// Emit a 1-slot binary op and return the result slot.
  pub fn emit_binary(&mut self, op: Op, arg0: u16, arg1: u16) -> u16 {
    let result = self.take_stack_slot(1);
    self.push_instruction(Instruction {
      op,
      arg_positions: [arg0, arg1, 0],
      return_position: result,
    });
    result
  }
  /// Emit a u32 Constant into a fresh slot and return that slot.
  pub fn emit_u32_constant(&mut self, value: u32) -> u16 {
    let slot = self.take_stack_slot(1);
    self.push_instruction(Instruction {
      op: Op::Constant,
      arg_positions: [(value >> 16) as u16, value as u16, 0],
      return_position: slot,
    });
    slot
  }
  /// Emit an f32 Constant into a fresh slot and return that slot.
  pub fn emit_f32_constant(&mut self, value: f32) -> u16 {
    self.emit_u32_constant(value.to_bits())
  }
  /// Emit a 1-slot ternary op and return the result slot.
  pub fn emit_ternary(
    &mut self,
    op: Op,
    arg0: u16,
    arg1: u16,
    arg2: u16,
  ) -> u16 {
    let result = self.take_stack_slot(1);
    self.push_instruction(Instruction {
      op,
      arg_positions: [arg0, arg1, arg2],
      return_position: result,
    });
    result
  }
  /// Fan-out unary op across `size` consecutive slots, return base of result.
  pub fn emit_fanout_unary(&mut self, op: Op, arg: u16, size: u16) -> u16 {
    let result = self.take_stack_slot(size);
    for i in 0..size {
      self.push_instruction(Instruction {
        op,
        arg_positions: [arg + i, 0, 0],
        return_position: result + i,
      });
    }
    result
  }
  /// Fan-out binary op across `size` consecutive slots, return base of result.
  pub fn emit_fanout_binary(
    &mut self,
    op: Op,
    arg0: u16,
    arg1: u16,
    size: u16,
  ) -> u16 {
    let result = self.take_stack_slot(size);
    for i in 0..size {
      self.push_instruction(Instruction {
        op,
        arg_positions: [arg0 + i, arg1 + i, 0],
        return_position: result + i,
      });
    }
    result
  }
  /// Fan-out binary op where the right-hand side is a single scalar broadcast
  /// across all `size` element positions on the left.
  pub fn emit_fanout_binary_scalar_rhs(
    &mut self,
    op: Op,
    arg0: u16,
    scalar_rhs: u16,
    size: u16,
  ) -> u16 {
    let result = self.take_stack_slot(size);
    for i in 0..size {
      self.push_instruction(Instruction {
        op,
        arg_positions: [arg0 + i, scalar_rhs, 0],
        return_position: result + i,
      });
    }
    result
  }
  /// Fan-out binary op where the left-hand side is a single scalar broadcast.
  /// Binary op whose operands may independently be vec/mat or scalar,
  /// matching WGSL's broadcast semantics: (vec,vec) elementwise,
  /// (vec,scalar)/(scalar,vec) broadcast, (scalar,scalar) plain.
  pub fn emit_mixed_shape_binary(
    &mut self,
    arg_types: &[Type],
    arg_positions: &[u16],
    op_for: impl Fn(&Type) -> Op,
  ) -> u16 {
    match (vec_kind(&arg_types[0]), vec_kind(&arg_types[1])) {
      (Some((n, e)), Some(_)) => self.emit_fanout_binary(
        op_for(&e),
        arg_positions[0],
        arg_positions[1],
        n,
      ),
      (Some((n, e)), None) => self.emit_fanout_binary_scalar_rhs(
        op_for(&e),
        arg_positions[0],
        arg_positions[1],
        n,
      ),
      (None, Some((n, e))) => self.emit_fanout_binary_scalar_lhs(
        op_for(&e),
        arg_positions[0],
        arg_positions[1],
        n,
      ),
      (None, None) => self.emit_binary(
        op_for(&arg_types[0]),
        arg_positions[0],
        arg_positions[1],
      ),
    }
  }
  pub fn emit_fanout_binary_scalar_lhs(
    &mut self,
    op: Op,
    scalar_lhs: u16,
    arg1: u16,
    size: u16,
  ) -> u16 {
    let result = self.take_stack_slot(size);
    for i in 0..size {
      self.push_instruction(Instruction {
        op,
        arg_positions: [scalar_lhs, arg1 + i, 0],
        return_position: result + i,
      });
    }
    result
  }
  /// Fan-out ternary op across `size` slots (each operand is also `size` slots).
  pub fn emit_fanout_ternary(
    &mut self,
    op: Op,
    arg0: u16,
    arg1: u16,
    arg2: u16,
    size: u16,
  ) -> u16 {
    let result = self.take_stack_slot(size);
    for i in 0..size {
      self.push_instruction(Instruction {
        op,
        arg_positions: [arg0 + i, arg1 + i, arg2 + i],
        return_position: result + i,
      });
    }
    result
  }

  // --- Element-wise (scalar-or-vec dispatch) emit helpers ---

  /// Compile an element-wise unary op, scalar or vector. The element-type → op
  /// resolver picks the scalar op based on the element type.
  fn emit_elementwise_unary(
    &mut self,
    arg_type: &Type,
    arg_pos: u16,
    op_for_element: impl Fn(&Type) -> Op,
  ) -> u16 {
    if let Some((count, elem)) = vec_kind(arg_type) {
      self.emit_fanout_unary(op_for_element(&elem), arg_pos, count)
    } else {
      self.emit_unary(op_for_element(arg_type), arg_pos)
    }
  }

  /// Compile an element-wise binary op, scalar or vector. Both args must have
  /// the same shape (same vec width or both scalars).
  fn emit_elementwise_binary(
    &mut self,
    arg_type: &Type,
    arg0: u16,
    arg1: u16,
    op_for_element: impl Fn(&Type) -> Op,
  ) -> u16 {
    if let Some((count, elem)) = vec_kind(arg_type) {
      self.emit_fanout_binary(op_for_element(&elem), arg0, arg1, count)
    } else {
      self.emit_binary(op_for_element(arg_type), arg0, arg1)
    }
  }

  /// Compile an element-wise ternary op, scalar or vector. All args must have
  /// the same shape.
  fn emit_elementwise_ternary(
    &mut self,
    arg_type: &Type,
    arg0: u16,
    arg1: u16,
    arg2: u16,
    op_for_element: impl Fn(&Type) -> Op,
  ) -> u16 {
    if let Some((count, elem)) = vec_kind(arg_type) {
      self.emit_fanout_ternary(op_for_element(&elem), arg0, arg1, arg2, count)
    } else {
      self.emit_ternary(op_for_element(arg_type), arg0, arg1, arg2)
    }
  }

  /// Compile an in-place element-wise binary op (e.g. `+=`): the result is
  /// written back to `arg0`. Returns the slot (same as arg0).
  fn emit_elementwise_binary_inplace(
    &mut self,
    arg_type: &Type,
    arg0: u16,
    arg1: u16,
    op_for_element: impl Fn(&Type) -> Op,
  ) -> u16 {
    let count = vec_kind(arg_type).map(|(c, _)| c).unwrap_or(1);
    let elem_type = vec_kind(arg_type)
      .map(|(_, e)| e)
      .unwrap_or_else(|| arg_type.clone());
    let op = op_for_element(&elem_type);
    for i in 0..count {
      self.push_instruction(Instruction {
        op,
        arg_positions: [arg0 + i, arg1 + i, 0],
        return_position: arg0 + i,
      });
    }
    arg0
  }

  /// Compile a vector reduction "dot" op: sum of componentwise products.
  /// Both args must be the same vec type, returns a single-slot scalar of the
  /// element type.
  fn emit_dot(&mut self, elem: &Type, count: u16, a: u16, b: u16) -> u16 {
    let mul_op = arithmetic_op_for(elem, "*");
    let add_op = arithmetic_op_for(elem, "+");
    let products = self.emit_fanout_binary(mul_op, a, b, count);
    let mut acc = products;
    for i in 1..count {
      acc = self.emit_binary(add_op, acc, products + i);
    }
    acc
  }

  /// Compile matrix multiplication A * B in column-major flat storage.
  ///   A is matAC x AR (A_cols=AC, A_rows=AR — math RA x AC matrix).
  ///   B is matBC x BR (B_cols=BC, B_rows=BR — math RB x BC matrix).
  ///   Inner dim must match: AC == BR — so we don't take B's row count as a
  ///   parameter; it's implied to equal `a_cols`.
  ///   Result is matBC x AR (BC cols, AR rows).
  fn emit_mat_mul(
    &mut self,
    a_cols: u16,
    a_rows: u16,
    b_cols: u16,
    a: u16,
    b: u16,
    elem: &Type,
  ) -> u16 {
    let mul_op = arithmetic_op_for(elem, "*");
    let add_op = arithmetic_op_for(elem, "+");
    let result = self.take_stack_slot(b_cols * a_rows);
    for c in 0..b_cols {
      for r in 0..a_rows {
        let mut acc: Option<u16> = None;
        for k in 0..a_cols {
          let a_slot = a + k * a_rows + r;
          let b_slot = b + c * a_cols + k;
          let prod = self.emit_binary(mul_op, a_slot, b_slot);
          acc = Some(match acc {
            None => prod,
            Some(p) => self.emit_binary(add_op, p, prod),
          });
        }
        self.push_instruction(Instruction {
          op: Op::Move,
          arg_positions: [acc.unwrap(), 1, 0],
          return_position: result + c * a_rows + r,
        });
      }
    }
    result
  }

  /// Push either a Move (if op == Op::Move) or a single-slot conversion op.
  fn push_conv_or_move(&mut self, op: Op, src: u16, dest: u16) {
    if op == Op::Move {
      self.push_instruction(Instruction {
        op: Op::Move,
        arg_positions: [src, 1, 0],
        return_position: dest,
      });
    } else {
      self.push_instruction(Instruction {
        op,
        arg_positions: [src, 0, 0],
        return_position: dest,
      });
    }
  }

  /// Emit a type conversion for either a scalar or vector arg. Both src and
  /// dest can be scalar or vec (same width if vec→vec); we fan out element by
  /// element.
  fn emit_type_conversion(
    &mut self,
    src_type: &Type,
    src_pos: u16,
    target_elem: &Type,
  ) -> u16 {
    if let Some((count, src_elem)) = vec_kind(src_type) {
      let op = conversion_op(&src_elem, target_elem);
      let result = self.take_stack_slot(count);
      for i in 0..count {
        self.push_conv_or_move(op, src_pos + i, result + i);
      }
      result
    } else {
      let op = conversion_op(src_type, target_elem);
      let result = self.take_stack_slot(1);
      self.push_conv_or_move(op, src_pos, result);
      result
    }
  }

  // --- Builtin dispatch ---

  /// Compile a builtin function call into bytecode.
  fn compile_builtin(
    &mut self,
    f_name: &str,
    args: &Vec<TypedExp>,
    arg_positions: &[u16],
    arg_types: &[Type],
    return_type: &Type,
  ) -> Option<u16> {
    // Matrix constructors (e.g. mat2x2f, mat3x4i, ...). Two arg shapes:
    //   - N*M scalars laid out column-major.
    //   - N column-vecs (vecM) of the element type.
    // Stored as a flat N*M-slot block.
    if f_name.starts_with("mat")
      && let Some((cols, rows)) = parse_mat_size(f_name)
    {
      let total = (cols * rows) as u16;
      let target_elem = match f_name.as_bytes().get(6).copied() {
        Some(b'f') => Type::F32,
        Some(b'i') => Type::I32,
        Some(b'u') => Type::U32,
        _ => Type::F32,
      };
      let result_pos = self.take_stack_slot(total);
      let mut out_offset: u16 = 0;
      for (arg_i, t) in arg_types.iter().enumerate() {
        if let Some((n, src_elem)) = vec_kind(t) {
          let conv_op = conversion_op(&src_elem, &target_elem);
          for j in 0..n {
            self.push_conv_or_move(
              conv_op,
              arg_positions[arg_i] + j,
              result_pos + out_offset + j,
            );
          }
          out_offset += n;
        } else {
          let conv_op = conversion_op(t, &target_elem);
          self.push_conv_or_move(
            conv_op,
            arg_positions[arg_i],
            result_pos + out_offset,
          );
          out_offset += 1;
        }
      }
      return Some(result_pos);
    }

    // Vec constructors (e.g. vec2f, vec3u, vec4i, ...). These are NOT
    // StructConstructor in easl — they're builtin functions with several
    // possible arg shapes (N scalars, single scalar broadcast, or shorter-vec
    // + scalars).
    if let Some(c) = f_name.chars().nth(0)
      && c == 'v'
      && let Some(elem_char) = f_name.chars().nth(4)
      && f_name.len() == 5
      && &f_name[0..3] == "vec"
      && let Some(size_char) = f_name.chars().nth(3)
      && (size_char == '2' || size_char == '3' || size_char == '4')
      && matches!(elem_char, 'f' | 'i' | 'u' | 'b')
    {
      let target_count = size_char.to_digit(10).unwrap() as u16;
      let target_elem = match elem_char {
        'f' => Type::F32,
        'i' => Type::I32,
        'u' => Type::U32,
        'b' => Type::Bool,
        _ => unreachable!(),
      };
      let result_pos = self.take_stack_slot(target_count);
      let total_arg_slots: u16 = arg_types
        .iter()
        .map(|t| if let Some((n, _)) = vec_kind(t) { n } else { 1 })
        .sum();
      if args.len() == 1 && vec_kind(&arg_types[0]).is_none() {
        // Broadcast.
        let src_elem = arg_types[0].clone();
        let conv_op = conversion_op(&src_elem, &target_elem);
        for i in 0..target_count {
          self.push_conv_or_move(conv_op, arg_positions[0], result_pos + i);
        }
      } else if args.len() == 1
        && vec_kind(&arg_types[0])
          .map(|(n, _)| n == target_count)
          .unwrap_or(false)
      {
        // Vec→vec same width: maybe element type conversion.
        let (_, src_elem) = vec_kind(&arg_types[0]).unwrap();
        let conv_op = conversion_op(&src_elem, &target_elem);
        for i in 0..target_count {
          self.push_conv_or_move(conv_op, arg_positions[0] + i, result_pos + i);
        }
      } else if total_arg_slots == target_count {
        // Concatenation of scalars and shorter vecs.
        let mut out_offset: u16 = 0;
        for (arg_i, t) in arg_types.iter().enumerate() {
          if let Some((n, src_elem)) = vec_kind(t) {
            let conv_op = conversion_op(&src_elem, &target_elem);
            for j in 0..n {
              self.push_conv_or_move(
                conv_op,
                arg_positions[arg_i] + j,
                result_pos + out_offset + j,
              );
            }
            out_offset += n;
          } else {
            let conv_op = conversion_op(t, &target_elem);
            self.push_conv_or_move(
              conv_op,
              arg_positions[arg_i],
              result_pos + out_offset,
            );
            out_offset += 1;
          }
        }
      } else {
        todo!("vec constructor: unhandled arg shape for {f_name}");
      }
      return Some(result_pos);
    }

    match f_name {
      // --- Element-wise unary f32 math ---
      "cos" | "sin" | "tan" | "acos" | "asin" | "atan" | "sinh" | "cosh"
      | "tanh" | "asinh" | "acosh" | "atanh" | "floor" | "ceil" | "round"
      | "trunc" | "fract" | "exp" | "exp2" | "log" | "log2" | "sqrt"
      | "inverse-sqrt" | "saturate" | "degrees" | "radians" => {
        let op = match f_name {
          "cos" => Op::Cos,
          "sin" => Op::Sin,
          "tan" => Op::Tan,
          "acos" => Op::Acos,
          "asin" => Op::Asin,
          "atan" => Op::Atan,
          "sinh" => Op::Sinh,
          "cosh" => Op::Cosh,
          "tanh" => Op::Tanh,
          "asinh" => Op::Asinh,
          "acosh" => Op::Acosh,
          "atanh" => Op::Atanh,
          "floor" => Op::Floor,
          "ceil" => Op::Ceil,
          "round" => Op::Round,
          "trunc" => Op::Trunc,
          "fract" => Op::Fract,
          "exp" => Op::Exp,
          "exp2" => Op::Exp2,
          "log" => Op::Log,
          "log2" => Op::Log2,
          "sqrt" => Op::Sqrt,
          "inverse-sqrt" => Op::InvSqrt,
          "saturate" => Op::Saturate,
          "degrees" => Op::Degrees,
          "radians" => Op::Radians,
          _ => unreachable!(),
        };
        Some(self.emit_elementwise_unary(
          &arg_types[0],
          arg_positions[0],
          |_| op,
        ))
      }

      // --- Abs / Sign (signed, type-aware) ---
      "abs" => {
        let elem_for_dispatch = vec_kind(&arg_types[0])
          .map(|(_, e)| e)
          .unwrap_or_else(|| arg_types[0].clone());
        if elem_for_dispatch == Type::U32 {
          // abs on u32 is identity. Just Move the arg's slots into a fresh dest.
          let size = arg_types[0]
            .data_size_in_u32s(&args[0].source_trace)
            .unwrap() as u16;
          let result = self.take_stack_slot(size);
          self.push_instruction(Instruction {
            op: Op::Move,
            arg_positions: [arg_positions[0], size, 0],
            return_position: result,
          });
          Some(result)
        } else {
          Some(self.emit_elementwise_unary(
            &arg_types[0],
            arg_positions[0],
            abs_op_for,
          ))
        }
      }
      "sign" => Some(self.emit_elementwise_unary(
        &arg_types[0],
        arg_positions[0],
        sign_op_for,
      )),

      // --- Binary f32 math ---
      "pow" => Some(self.emit_elementwise_binary(
        &arg_types[0],
        arg_positions[0],
        arg_positions[1],
        |_| Op::Pow,
      )),
      "atan2" => Some(self.emit_elementwise_binary(
        &arg_types[0],
        arg_positions[0],
        arg_positions[1],
        |_| Op::Atan2,
      )),
      "step" => Some(self.emit_elementwise_binary(
        &arg_types[0],
        arg_positions[0],
        arg_positions[1],
        |_| Op::Step,
      )),
      "ldexp" => Some(self.emit_elementwise_binary(
        &arg_types[0],
        arg_positions[0],
        arg_positions[1],
        |_| Op::Ldexp,
      )),

      // --- Ternary f32 math ---
      "fma" => Some(self.emit_elementwise_ternary(
        &arg_types[0],
        arg_positions[0],
        arg_positions[1],
        arg_positions[2],
        |_| Op::Fma,
      )),
      "smoothstep" => Some(self.emit_elementwise_ternary(
        &arg_types[0],
        arg_positions[0],
        arg_positions[1],
        arg_positions[2],
        |_| Op::Smoothstep,
      )),
      "mix" => Some(self.emit_elementwise_ternary(
        &arg_types[0],
        arg_positions[0],
        arg_positions[1],
        arg_positions[2],
        |_| Op::Mix,
      )),

      // --- min / max / clamp ---
      "min" => Some(self.emit_elementwise_binary(
        &arg_types[0],
        arg_positions[0],
        arg_positions[1],
        min_op_for,
      )),
      "max" => Some(self.emit_elementwise_binary(
        &arg_types[0],
        arg_positions[0],
        arg_positions[1],
        max_op_for,
      )),
      "clamp" => Some(self.emit_elementwise_ternary(
        &arg_types[0],
        arg_positions[0],
        arg_positions[1],
        arg_positions[2],
        clamp_op_for,
      )),

      // --- Arithmetic ---
      "+" => Some(self.emit_mixed_shape_binary(
        arg_types,
        arg_positions,
        |e| arithmetic_op_for(e, "+"),
      )),
      "-" => {
        if args.len() == 1 {
          let neg_op = |e: &Type| match e {
            Type::F32 => Op::NegateF32,
            Type::I32 => Op::NegateI32,
            _ => unreachable!(),
          };
          Some(self.emit_elementwise_unary(
            &arg_types[0],
            arg_positions[0],
            neg_op,
          ))
        } else {
          Some(self.emit_mixed_shape_binary(
            arg_types,
            arg_positions,
            |e| arithmetic_op_for(e, "-"),
          ))
        }
      }
      "*" => {
        // Dispatch table for multiplication:
        //   mat * mat  → matrix product
        //   mat * vec  → mat * col-vec
        //   vec * mat  → row-vec * mat
        //   mat * scalar / scalar * mat / vec * scalar / scalar * vec → broadcast
        //   vec * vec  → componentwise
        //   scalar * scalar → scalar
        let a_mat = mat_kind(&arg_types[0]);
        let b_mat = mat_kind(&arg_types[1]);
        let a_vec_only = vec_kind(&arg_types[0]).filter(|_| a_mat.is_none());
        let b_vec_only = vec_kind(&arg_types[1]).filter(|_| b_mat.is_none());
        match (a_mat, b_mat, a_vec_only, b_vec_only) {
          (Some((ac, ar, e)), Some((bc, _, _)), _, _) => {
            Some(self.emit_mat_mul(
              ac,
              ar,
              bc,
              arg_positions[0],
              arg_positions[1],
              &e,
            ))
          }
          (Some((ac, ar, e)), None, _, Some(_)) => Some(self.emit_mat_mul(
            ac,
            ar,
            1,
            arg_positions[0],
            arg_positions[1],
            &e,
          )),
          (None, Some((bc, br, e)), Some(_), _) => Some(self.emit_mat_mul(
            br,
            1,
            bc,
            arg_positions[0],
            arg_positions[1],
            &e,
          )),
          (Some((_, _, e)), None, _, None) => {
            let size = arg_types[0]
              .data_size_in_u32s(&args[0].source_trace)
              .unwrap() as u16;
            Some(self.emit_fanout_binary_scalar_rhs(
              arithmetic_op_for(&e, "*"),
              arg_positions[0],
              arg_positions[1],
              size,
            ))
          }
          (None, Some((_, _, e)), None, _) => {
            let size = arg_types[1]
              .data_size_in_u32s(&args[1].source_trace)
              .unwrap() as u16;
            Some(self.emit_fanout_binary_scalar_lhs(
              arithmetic_op_for(&e, "*"),
              arg_positions[0],
              arg_positions[1],
              size,
            ))
          }
          (None, None, Some((n, e)), Some(_)) => Some(self.emit_fanout_binary(
            arithmetic_op_for(&e, "*"),
            arg_positions[0],
            arg_positions[1],
            n,
          )),
          (None, None, Some((n, e)), None) => {
            Some(self.emit_fanout_binary_scalar_rhs(
              arithmetic_op_for(&e, "*"),
              arg_positions[0],
              arg_positions[1],
              n,
            ))
          }
          (None, None, None, Some((n, e))) => {
            Some(self.emit_fanout_binary_scalar_lhs(
              arithmetic_op_for(&e, "*"),
              arg_positions[0],
              arg_positions[1],
              n,
            ))
          }
          (None, None, None, None) => Some(self.emit_binary(
            arithmetic_op_for(&arg_types[0], "*"),
            arg_positions[0],
            arg_positions[1],
          )),
        }
      }
      "/" => {
        if args.len() == 1 {
          // Unary `/` is reciprocal: `(/ x)` ≡ `(/ 1 x)`.
          let (count, elem) = vec_kind(&arg_types[0])
            .unwrap_or_else(|| (1, arg_types[0].clone()));
          let one = match elem {
            Type::F32 => self.emit_f32_constant(1.0),
            Type::I32 | Type::U32 => self.emit_u32_constant(1),
            _ => unreachable!(),
          };
          let div_op = arithmetic_op_for(&elem, "/");
          if vec_kind(&arg_types[0]).is_some() {
            Some(self.emit_fanout_binary_scalar_lhs(
              div_op,
              one,
              arg_positions[0],
              count,
            ))
          } else {
            Some(self.emit_binary(div_op, one, arg_positions[0]))
          }
        } else {
          let a_vec = vec_kind(&arg_types[0]);
          let b_vec = vec_kind(&arg_types[1]);
          match (a_vec, b_vec) {
            (Some((n, e)), Some((_, _))) => Some(self.emit_fanout_binary(
              arithmetic_op_for(&e, "/"),
              arg_positions[0],
              arg_positions[1],
              n,
            )),
            (Some((n, e)), None) => Some(self.emit_fanout_binary_scalar_rhs(
              arithmetic_op_for(&e, "/"),
              arg_positions[0],
              arg_positions[1],
              n,
            )),
            (None, Some((n, e))) => Some(self.emit_fanout_binary_scalar_lhs(
              arithmetic_op_for(&e, "/"),
              arg_positions[0],
              arg_positions[1],
              n,
            )),
            (None, None) => Some(self.emit_binary(
              arithmetic_op_for(&arg_types[0], "/"),
              arg_positions[0],
              arg_positions[1],
            )),
          }
        }
      }
      "%" => Some(self.emit_mixed_shape_binary(
        arg_types,
        arg_positions,
        |e| arithmetic_op_for(e, "%"),
      )),

      // --- Compound assignment ---
      "+=" | "-=" | "*=" | "/=" | "%=" => {
        let base = &f_name[..f_name.len() - 1];
        // `(op= vec scalar)` broadcasts the scalar across the target's
        // components; same-shape operands go elementwise.
        if let Some((n, e)) = vec_kind(&arg_types[0])
          && vec_kind(&arg_types[1]).is_none()
        {
          let op = arithmetic_op_for(&e, base);
          for i in 0..n {
            self.push_instruction(Instruction {
              op,
              arg_positions: [arg_positions[0] + i, arg_positions[1], 0],
              return_position: arg_positions[0] + i,
            });
          }
          Some(arg_positions[0])
        } else {
          Some(self.emit_elementwise_binary_inplace(
            &arg_types[0],
            arg_positions[0],
            arg_positions[1],
            |e| arithmetic_op_for(e, base),
          ))
        }
      }

      // --- Plain assignment ---
      "=" => {
        let arg_size = arg_types[0]
          .data_size_in_u32s(&args[0].source_trace)
          .unwrap() as u16;
        self.push_instruction(Instruction {
          op: Op::Move,
          arg_positions: [arg_positions[1], arg_size, 0],
          return_position: arg_positions[0],
        });
        None
      }

      // --- Comparisons ---
      "==" | "!=" | "<" | ">" | "<=" | ">=" => {
        Some(self.emit_elementwise_binary(
          &arg_types[0],
          arg_positions[0],
          arg_positions[1],
          |e| compare_op_for(e, f_name),
        ))
      }

      // --- Boolean ---
      "&&" => Some(self.emit_binary(
        Op::LogicalAnd,
        arg_positions[0],
        arg_positions[1],
      )),
      "||" => Some(self.emit_binary(
        Op::LogicalOr,
        arg_positions[0],
        arg_positions[1],
      )),
      "!" => Some(self.emit_unary(Op::LogicalNot, arg_positions[0])),

      // --- Bitwise ---
      "bit-and" | "&" => {
        Some(self.emit_binary(Op::BitAnd, arg_positions[0], arg_positions[1]))
      }
      "bit-or" | "|" => {
        Some(self.emit_binary(Op::BitOr, arg_positions[0], arg_positions[1]))
      }
      "bit-xor" | "^" => {
        Some(self.emit_binary(Op::BitXor, arg_positions[0], arg_positions[1]))
      }
      "bit-not" | "~" => Some(self.emit_unary(Op::BitNot, arg_positions[0])),
      "<<" => Some(self.emit_binary(
        Op::ShiftLeft,
        arg_positions[0],
        arg_positions[1],
      )),
      ">>" => {
        let op = match arg_types[0] {
          Type::I32 => Op::ShiftRightI32,
          _ => Op::ShiftRightU32,
        };
        Some(self.emit_binary(op, arg_positions[0], arg_positions[1]))
      }
      "count-one-bits" => Some(self.emit_elementwise_unary(
        &arg_types[0],
        arg_positions[0],
        |_| Op::CountOneBits,
      )),
      "count-leading-zeros" => Some(self.emit_elementwise_unary(
        &arg_types[0],
        arg_positions[0],
        |_| Op::CountLeadingZeros,
      )),
      "count-trailing-zeros" => Some(self.emit_elementwise_unary(
        &arg_types[0],
        arg_positions[0],
        |_| Op::CountTrailingZeros,
      )),
      "reverse-bits" => Some(self.emit_elementwise_unary(
        &arg_types[0],
        arg_positions[0],
        |_| Op::ReverseBits,
      )),
      "first-leading-bit" => Some(self.emit_elementwise_unary(
        &arg_types[0],
        arg_positions[0],
        |e| match e {
          Type::I32 => Op::FirstLeadingBitI32,
          _ => Op::FirstLeadingBitU32,
        },
      )),
      "first-trailing-bit" => Some(self.emit_elementwise_unary(
        &arg_types[0],
        arg_positions[0],
        |_| Op::FirstTrailingBit,
      )),
      "extract-bits" => Some(self.emit_elementwise_ternary(
        &arg_types[0],
        arg_positions[0],
        arg_positions[1],
        arg_positions[2],
        |e| match e {
          Type::I32 => Op::ExtractBitsI32,
          _ => Op::ExtractBitsU32,
        },
      )),

      // --- Type conversions / casts ---
      "f32" => Some(self.emit_type_conversion(
        &arg_types[0],
        arg_positions[0],
        &Type::F32,
      )),
      "i32" => Some(self.emit_type_conversion(
        &arg_types[0],
        arg_positions[0],
        &Type::I32,
      )),
      "u32" => Some(self.emit_type_conversion(
        &arg_types[0],
        arg_positions[0],
        &Type::U32,
      )),
      "bool" => Some(self.emit_type_conversion(
        &arg_types[0],
        arg_positions[0],
        &Type::Bool,
      )),

      // --- bitcast (often called via bitcast<T> with the type baked in) ---
      n if n == "bitcast" || n.starts_with("bitcast<") => {
        let size = return_type
          .data_size_in_u32s(&args[0].source_trace)
          .unwrap() as u16;
        let result = self.take_stack_slot(size);
        self.push_instruction(Instruction {
          op: Op::Move,
          arg_positions: [arg_positions[0], size, 0],
          return_position: result,
        });
        Some(result)
      }

      // --- Vector reductions / vector-only ops ---
      "dot" => {
        let (count, elem) = vec_kind(&arg_types[0]).unwrap();
        Some(self.emit_dot(&elem, count, arg_positions[0], arg_positions[1]))
      }
      "length" => {
        if let Some((count, elem)) = vec_kind(&arg_types[0]) {
          let dot_pos =
            self.emit_dot(&elem, count, arg_positions[0], arg_positions[0]);
          Some(self.emit_unary(Op::Sqrt, dot_pos))
        } else {
          Some(self.emit_unary(Op::AbsF32, arg_positions[0]))
        }
      }
      "distance" => {
        if let Some((count, elem)) = vec_kind(&arg_types[0]) {
          let sub_op = arithmetic_op_for(&elem, "-");
          let diff = self.emit_fanout_binary(
            sub_op,
            arg_positions[0],
            arg_positions[1],
            count,
          );
          let dot_pos = self.emit_dot(&elem, count, diff, diff);
          Some(self.emit_unary(Op::Sqrt, dot_pos))
        } else {
          let diff =
            self.emit_binary(Op::MinusF32, arg_positions[0], arg_positions[1]);
          Some(self.emit_unary(Op::AbsF32, diff))
        }
      }
      "normalize" => {
        // v / length(v)
        let (count, elem) = vec_kind(&arg_types[0]).unwrap();
        let dot_pos =
          self.emit_dot(&elem, count, arg_positions[0], arg_positions[0]);
        let len = self.emit_unary(Op::Sqrt, dot_pos);
        Some(self.emit_fanout_binary_scalar_rhs(
          arithmetic_op_for(&elem, "/"),
          arg_positions[0],
          len,
          count,
        ))
      }
      "cross" => {
        // 3D vec only.
        // result.x = a.y * b.z - a.z * b.y
        // result.y = a.z * b.x - a.x * b.z
        // result.z = a.x * b.y - a.y * b.x
        let result = self.take_stack_slot(3);
        let a = arg_positions[0];
        let b = arg_positions[1];
        let mul = Op::MultiplyF32;
        let sub = Op::MinusF32;
        let ayz = self.emit_binary(mul, a + 1, b + 2);
        let azy = self.emit_binary(mul, a + 2, b + 1);
        self.push_instruction(Instruction {
          op: sub,
          arg_positions: [ayz, azy, 0],
          return_position: result,
        });
        let azx = self.emit_binary(mul, a + 2, b);
        let axz = self.emit_binary(mul, a, b + 2);
        self.push_instruction(Instruction {
          op: sub,
          arg_positions: [azx, axz, 0],
          return_position: result + 1,
        });
        let axy = self.emit_binary(mul, a, b + 1);
        let ayx = self.emit_binary(mul, a + 1, b);
        self.push_instruction(Instruction {
          op: sub,
          arg_positions: [axy, ayx, 0],
          return_position: result + 2,
        });
        Some(result)
      }
      "reflect" => {
        // reflect(I, N) = I - 2 * dot(N, I) * N
        let (count, elem) = vec_kind(&arg_types[0]).unwrap();
        let dot_pos =
          self.emit_dot(&elem, count, arg_positions[1], arg_positions[0]);
        let two_slot = self.emit_f32_constant(2.0);
        let two_dot = self.emit_binary(Op::MultiplyF32, two_slot, dot_pos);
        let scaled = self.emit_fanout_binary_scalar_lhs(
          Op::MultiplyF32,
          two_dot,
          arg_positions[1],
          count,
        );
        Some(self.emit_fanout_binary(
          Op::MinusF32,
          arg_positions[0],
          scaled,
          count,
        ))
      }
      "refract" => {
        // WGSL refract(I, N, eta):
        //   k = 1 - eta*eta*(1 - dot(N,I)*dot(N,I))
        //   if k < 0 return vec(0)
        //   else return eta*I - (eta*dot(N,I) + sqrt(k))*N
        let (count, elem) = vec_kind(&arg_types[0]).unwrap();
        let i_pos = arg_positions[0];
        let n_pos = arg_positions[1];
        let eta = arg_positions[2];
        let ni = self.emit_dot(&elem, count, n_pos, i_pos);
        let ni_sq = self.emit_binary(Op::MultiplyF32, ni, ni);
        let one_slot = self.emit_f32_constant(1.0);
        let one_minus_nisq = self.emit_binary(Op::MinusF32, one_slot, ni_sq);
        let eta_sq = self.emit_binary(Op::MultiplyF32, eta, eta);
        let etasq_term =
          self.emit_binary(Op::MultiplyF32, eta_sq, one_minus_nisq);
        let k = self.emit_binary(Op::MinusF32, one_slot, etasq_term);
        let zero_slot = self.emit_f32_constant(0.0);
        let k_neg = self.emit_binary(Op::LessThanF32, k, zero_slot);
        let k_neg_f32 = self.emit_unary(Op::BoolToF32, k_neg);
        let safe_k = self.emit_binary(Op::MaxF32, k, zero_slot);
        let sqrt_k = self.emit_unary(Op::Sqrt, safe_k);
        let eta_ni = self.emit_binary(Op::MultiplyF32, eta, ni);
        let scalar = self.emit_binary(Op::PlusF32, eta_ni, sqrt_k);
        let eta_i = self.emit_fanout_binary_scalar_lhs(
          Op::MultiplyF32,
          eta,
          i_pos,
          count,
        );
        let scaled_n = self.emit_fanout_binary_scalar_lhs(
          Op::MultiplyF32,
          scalar,
          n_pos,
          count,
        );
        let result_when_ok =
          self.emit_fanout_binary(Op::MinusF32, eta_i, scaled_n, count);
        let zero_vec = self.take_stack_slot(count);
        for i in 0..count {
          self.push_instruction(Instruction {
            op: Op::Constant,
            arg_positions: [0, 0, 0],
            return_position: zero_vec + i,
          });
        }
        // (vec zero loop kept inline since it writes to consecutive slots,
        //  not a single fresh slot — emit_*_constant wouldn't fit cleanly.)
        let result = self.take_stack_slot(count);
        for i in 0..count {
          self.push_instruction(Instruction {
            op: Op::Mix,
            arg_positions: [result_when_ok + i, zero_vec + i, k_neg_f32],
            return_position: result + i,
          });
        }
        Some(result)
      }
      "transpose" => {
        let (cols, rows, _elem) = mat_kind(&arg_types[0]).unwrap();
        let src = arg_positions[0];
        let result = self.take_stack_slot(cols * rows);
        for c_dst in 0..rows {
          for r_dst in 0..cols {
            let c_src = r_dst;
            let r_src = c_dst;
            self.push_instruction(Instruction {
              op: Op::Move,
              arg_positions: [src + c_src * rows + r_src, 1, 0],
              return_position: result + c_dst * cols + r_dst,
            });
          }
        }
        Some(result)
      }
      "determinant" => self.emit_determinant(&arg_types[0], arg_positions[0]),
      "face-forward" => {
        // WGSL faceForward(e1, e2, e3) = if dot(e2,e3) < 0 then e1 else -e1
        let (count, elem) = vec_kind(&arg_types[0]).unwrap();
        let dot_pos =
          self.emit_dot(&elem, count, arg_positions[1], arg_positions[2]);
        let zero_slot = self.emit_f32_constant(0.0);
        let lt_zero = self.emit_binary(Op::LessThanF32, dot_pos, zero_slot);
        let lt_zero_f32 = self.emit_unary(Op::BoolToF32, lt_zero);
        let neg_e1 =
          self.emit_fanout_unary(Op::NegateF32, arg_positions[0], count);
        let result = self.take_stack_slot(count);
        for i in 0..count {
          self.push_instruction(Instruction {
            op: Op::Mix,
            arg_positions: [neg_e1 + i, arg_positions[0] + i, lt_zero_f32],
            return_position: result + i,
          });
        }
        Some(result)
      }
      "zeroed-array" => {
        // Statically-sized zeroed array: allocate and zero-fill. (The
        // runtime-sized form only appears in positions the CPU-mode
        // interceptions handle.)
        let total = return_type
          .data_size_in_u32s(&crate::compiler::error::SourceTrace::empty())
          .expect("zeroed-array of runtime-sized type outside an intercepted position")
          as u16;
        let result = self.take_stack_slot(total);
        for i in 0..total {
          self.push_instruction(Instruction {
            op: Op::Constant,
            arg_positions: [0, 0, 0],
            return_position: result + i,
          });
        }
        Some(result)
      }
      "any" | "all" => {
        // OR (any) / AND (all) reduction over a boolean vector's
        // components; the scalar-bool overload is the identity.
        let op = if f_name == "any" {
          Op::LogicalOr
        } else {
          Op::LogicalAnd
        };
        if let Some((n, _)) = vec_kind(&arg_types[0]) {
          let result = self.take_stack_slot(1);
          self.push_instruction(Instruction {
            op: Op::Move,
            arg_positions: [arg_positions[0], 1, 0],
            return_position: result,
          });
          for i in 1..n {
            self.push_instruction(Instruction {
              op,
              arg_positions: [result, arg_positions[0] + i, 0],
              return_position: result,
            });
          }
          Some(result)
        } else {
          Some(arg_positions[0])
        }
      }
      "pack-4x8-snorm" => Some(self.emit_unary(Op::PackSnorm4x8, arg_positions[0])),
      "unpack-4x8-snorm" => {
        let result = self.take_stack_slot(4);
        self.push_instruction(Instruction {
          op: Op::UnpackSnorm4x8,
          arg_positions: [arg_positions[0], 0, 0],
          return_position: result,
        });
        Some(result)
      }
      "pack-4x8-unorm" => Some(self.emit_unary(Op::PackUnorm4x8, arg_positions[0])),
      "unpack-4x8-unorm" => {
        let result = self.take_stack_slot(4);
        self.push_instruction(Instruction {
          op: Op::UnpackUnorm4x8,
          arg_positions: [arg_positions[0], 0, 0],
          return_position: result,
        });
        Some(result)
      }
      "pack-2x16-snorm" => Some(self.emit_unary(Op::PackSnorm2x16, arg_positions[0])),
      "unpack-2x16-snorm" => {
        let result = self.take_stack_slot(2);
        self.push_instruction(Instruction {
          op: Op::UnpackSnorm2x16,
          arg_positions: [arg_positions[0], 0, 0],
          return_position: result,
        });
        Some(result)
      }
      "pack-2x16-unorm" => Some(self.emit_unary(Op::PackUnorm2x16, arg_positions[0])),
      "unpack-2x16-unorm" => {
        let result = self.take_stack_slot(2);
        self.push_instruction(Instruction {
          op: Op::UnpackUnorm2x16,
          arg_positions: [arg_positions[0], 0, 0],
          return_position: result,
        });
        Some(result)
      }
      "pack-4x8-u8" => Some(self.emit_unary(Op::Pack4xU8, arg_positions[0])),
      "unpack-4x8-u8" => {
        let result = self.take_stack_slot(4);
        self.push_instruction(Instruction {
          op: Op::Unpack4xU8,
          arg_positions: [arg_positions[0], 0, 0],
          return_position: result,
        });
        Some(result)
      }
      "pack-4x8-i8" => Some(self.emit_unary(Op::Pack4xI8, arg_positions[0])),
      "unpack-4x8-i8" => {
        let result = self.take_stack_slot(4);
        self.push_instruction(Instruction {
          op: Op::Unpack4xI8,
          arg_positions: [arg_positions[0], 0, 0],
          return_position: result,
        });
        Some(result)
      }
      "dot-4-u8-packed" => Some(self.emit_binary(
        Op::Dot4U8Packed,
        arg_positions[0],
        arg_positions[1],
      )),
      "dot-4-i8-packed" => Some(self.emit_binary(
        Op::Dot4I8Packed,
        arg_positions[0],
        arg_positions[1],
      )),
      _ => todo!("haven't implemented builtin fn \"{f_name}\" for the VM yet"),
    }
  }

  /// determinant — supports square 2x2, 3x3, 4x4 f32 matrices.
  fn emit_determinant(&mut self, arg_type: &Type, m: u16) -> Option<u16> {
    let (cols, rows, _elem) = mat_kind(arg_type).unwrap();
    assert_eq!(cols, rows);
    let n = cols;
    let s = |row: u16, col: u16| m + col * n + row;
    match n {
      2 => {
        let ad = self.emit_binary(Op::MultiplyF32, s(0, 0), s(1, 1));
        let bc = self.emit_binary(Op::MultiplyF32, s(1, 0), s(0, 1));
        Some(self.emit_binary(Op::MinusF32, ad, bc))
      }
      3 => {
        let a = s(0, 0);
        let b = s(0, 1);
        let c = s(0, 2);
        let d = s(1, 0);
        let e = s(1, 1);
        let f = s(1, 2);
        let g = s(2, 0);
        let h = s(2, 1);
        let i = s(2, 2);
        Some(self.emit_det3(a, b, c, d, e, f, g, h, i))
      }
      4 => {
        let elem: Vec<Vec<u16>> =
          (0..4).map(|r| (0..4).map(|c| s(r, c)).collect()).collect();
        let mut signed_terms: Vec<(bool, u16)> = vec![];
        for j in 0..4 {
          let cols_kept: Vec<u16> = (0..4).filter(|c| *c != j).collect();
          let r_kept: Vec<u16> = (1..4).collect();
          let a = elem[r_kept[0] as usize][cols_kept[0] as usize];
          let b = elem[r_kept[0] as usize][cols_kept[1] as usize];
          let c = elem[r_kept[0] as usize][cols_kept[2] as usize];
          let d = elem[r_kept[1] as usize][cols_kept[0] as usize];
          let e = elem[r_kept[1] as usize][cols_kept[1] as usize];
          let f = elem[r_kept[1] as usize][cols_kept[2] as usize];
          let g = elem[r_kept[2] as usize][cols_kept[0] as usize];
          let h = elem[r_kept[2] as usize][cols_kept[1] as usize];
          let i = elem[r_kept[2] as usize][cols_kept[2] as usize];
          let minor = self.emit_det3(a, b, c, d, e, f, g, h, i);
          let term =
            self.emit_binary(Op::MultiplyF32, elem[0][j as usize], minor);
          signed_terms.push((j % 2 == 0, term));
        }
        let mut acc: Option<u16> = None;
        for (positive, term) in signed_terms {
          acc = Some(match acc {
            None => {
              if positive {
                term
              } else {
                let zero = self.emit_f32_constant(0.0);
                self.emit_binary(Op::MinusF32, zero, term)
              }
            }
            Some(p) => {
              if positive {
                self.emit_binary(Op::PlusF32, p, term)
              } else {
                self.emit_binary(Op::MinusF32, p, term)
              }
            }
          });
        }
        Some(acc.unwrap())
      }
      _ => unreachable!("determinant only defined for 2x2, 3x3, 4x4"),
    }
  }

  /// 3x3 determinant via standard cofactor expansion: a(ei-fh) - b(di-fg) + c(dh-eg).
  fn emit_det3(
    &mut self,
    a: u16,
    b: u16,
    c: u16,
    d: u16,
    e: u16,
    f: u16,
    g: u16,
    h: u16,
    i: u16,
  ) -> u16 {
    let ei = self.emit_binary(Op::MultiplyF32, e, i);
    let fh = self.emit_binary(Op::MultiplyF32, f, h);
    let m1 = self.emit_binary(Op::MinusF32, ei, fh);
    let am1 = self.emit_binary(Op::MultiplyF32, a, m1);
    let di = self.emit_binary(Op::MultiplyF32, d, i);
    let fg = self.emit_binary(Op::MultiplyF32, f, g);
    let m2 = self.emit_binary(Op::MinusF32, di, fg);
    let bm2 = self.emit_binary(Op::MultiplyF32, b, m2);
    let dh = self.emit_binary(Op::MultiplyF32, d, h);
    let eg = self.emit_binary(Op::MultiplyF32, e, g);
    let m3 = self.emit_binary(Op::MinusF32, dh, eg);
    let cm3 = self.emit_binary(Op::MultiplyF32, c, m3);
    let am1_minus_bm2 = self.emit_binary(Op::MinusF32, am1, bm2);
    self.emit_binary(Op::PlusF32, am1_minus_bm2, cm3)
  }

  pub fn finalize(
    self,
    init_function_index: Option<usize>,
  ) -> (BytecodeProgram, Vec<Arc<str>>) {
    let code: Code = Code {
      function_instructions: self.instructions,
      functions: self
        .finished_functions
        .iter()
        .map(|f| Function {
          instructions: f.instructions.clone(),
          return_position: f.stack_frame_start,
        })
        .collect(),
      init_function_index,
      globals: self.global_slots,
      min_stack_size: self.consumed_stack_space as usize,
      host_ops: self.host_ops,
      host_types: self.host_types,
      host_strings: self.host_strings,
      host_bindings: self.host_bindings,
      host_dispatches: self.host_dispatches,
    };
    (
      BytecodeProgram::from_code(code),
      self
        .finished_functions
        .iter()
        .map(|f| f.name.clone())
        .collect(),
    )
  }

  // --- CPU-runtime-mode helpers ---

  /// Append `op` to the host-op table and emit a `HostCall` instruction
  /// referencing it.
  pub fn emit_host_op(&mut self, op: HostOp) {
    let index = self.host_ops.len() as u16;
    self.host_ops.push(op);
    self.push_instruction(Instruction {
      op: Op::HostCall,
      arg_positions: [index, 0, 0],
      return_position: 0,
    });
  }
  pub fn host_type_index(&mut self, ty: &Type) -> u16 {
    if let Some(i) = self.host_types.iter().position(|t| t == ty) {
      i as u16
    } else {
      self.host_types.push(ty.clone());
      (self.host_types.len() - 1) as u16
    }
  }
  pub fn host_string_index(&mut self, s: &str) -> u16 {
    if let Some(i) = self.host_strings.iter().position(|t| &**t == s) {
      i as u16
    } else {
      self.host_strings.push(s.into());
      (self.host_strings.len() - 1) as u16
    }
  }
  /// Emits `CheckGpuToCpu` for each GPU-bound global in `names` — the VM
  /// equivalent of the tree-walker's `check_cpu_readable` before an
  /// application evaluates.
  pub fn emit_sync_checks(&mut self, names: &[Arc<str>]) {
    for name in names {
      if let Some(binding) = self.binding_indices.get(name).copied() {
        self.emit_host_op(HostOp::CheckGpuToCpu { binding });
      }
    }
  }
  /// Emits `MarkCpuWritten` for each GPU-bound global in `names` — the VM
  /// equivalent of the tree-walker's `mark_cpu_written` after an
  /// application evaluates.
  pub fn emit_write_marks(&mut self, names: &[Arc<str>]) {
    for name in names {
      if let Some(binding) = self.binding_indices.get(name).copied() {
        self.emit_host_op(HostOp::MarkCpuWritten { binding });
      }
    }
  }
  /// Resolves a function-reference argument of a dispatch builtin: returns
  /// the entry point's name and its GPU read/write sets as binding indices.
  /// If the reference is a scope construction (a dispatched closure with
  /// captured locals), also compiles the captured values into the closure's
  /// implicit scope binding global and marks it CPU-written — the VM
  /// equivalent of `upload_dispatched_closure_scope`.
  fn resolve_dispatched_fn(
    &mut self,
    arg: &TypedExp,
  ) -> (Arc<str>, Vec<u16>, Vec<u16>) {
    let Type::Function(signature) = arg.data.unwrap_known() else {
      panic!("dispatched function argument had a non-function type")
    };
    let ancestor = signature
      .abstract_ancestor
      .clone()
      .expect("dispatched function had no abstract ancestor");
    let ancestor = ancestor.read().unwrap();
    let entry_name = ancestor.name.clone();
    let FunctionImplementationKind::Composite(implementation) =
      &ancestor.implementation
    else {
      panic!("dispatched function {entry_name} wasn't composite")
    };
    let effects = implementation.read().unwrap().effects();
    let (reads, writes) = effects.gpu_read_and_written_globals();
    let read_indices: Vec<u16> = reads
      .iter()
      .filter_map(|n| self.binding_indices.get(n).copied())
      .collect();
    let write_indices: Vec<u16> = writes
      .iter()
      .filter_map(|n| self.binding_indices.get(n).copied())
      .collect();
    // Scope construction: write the captured values into the closure's
    // implicit scope binding (created by extract_dispatched_closure_scopes)
    // so the dispatch's pre-upload ships them to the GPU.
    if let ExpKind::Application(_, captured) = &arg.kind {
      let scope_struct = ancestor
        .captured_scope
        .as_ref()
        .expect("dispatched closure construction without captured scope");
      let scope_global_name = format!("{}_data", scope_struct.name.0);
      let scope_binding = *self
        .binding_indices
        .get(&Arc::<str>::from(scope_global_name.as_str()))
        .expect("dispatched closure scope binding not found");
      let HostBindingStorage::Slots { position, .. } =
        self.host_bindings[scope_binding as usize].storage
      else {
        panic!("dispatched closure scope binding wasn't slot-backed")
      };
      let mut offset = 0u16;
      for captured_value in captured {
        let value_pos = captured_value.compile_to_bytecode(false, self).unwrap();
        let value_size = vm_type_size(&captured_value.data.unwrap_known());
        if value_size > 0 {
          self.push_instruction(Instruction {
            op: Op::Move,
            arg_positions: [value_pos, value_size, 0],
            return_position: position + offset,
          });
        }
        offset += value_size;
      }
      self.emit_host_op(HostOp::MarkCpuWritten {
        binding: scope_binding,
      });
    }
    (entry_name, read_indices, write_indices)
  }
}

// =============================================================================
// Free utility functions (don't take state)
// =============================================================================

/// Slot count of `t` in the VM's flat layout. Function-typed values are
/// represented by their captured scope's data (zero slots for scope-less
/// closures) — the code part of a closure is static, so only its captured
/// state occupies memory.
pub fn vm_type_size(t: &Type) -> u16 {
  if let Type::Function(f) = t {
    if let Some(ancestor) = &f.abstract_ancestor
      && let Some(scope) = &ancestor.read().unwrap().captured_scope
    {
      scope
        .fields
        .iter()
        .map(|field| {
          let crate::compiler::types::AbstractType::Type(field_type) =
            &field.field_type
          else {
            panic!("captured scope field with non-concrete type")
          };
          vm_type_size(field_type)
        })
        .sum()
    } else {
      0
    }
  } else {
    t.data_size_in_u32s(&crate::compiler::error::SourceTrace::empty())
      .unwrap() as u16
  }
}

/// Returns Some((element_count, element_type)) if `t` is a vec2/vec3/vec4 or
/// matNxM, or None otherwise. For matrices the count is N*M flat scalars.
fn vec_kind(t: &Type) -> Option<(u16, Type)> {
  if let Type::Struct(s) = t {
    let n = &*s.name;
    if (n == "vec2" || n == "vec3" || n == "vec4") && !s.fields.is_empty() {
      let count = s.fields.len() as u16;
      let elem = s.fields[0].field_type.unwrap_known();
      return Some((count, elem));
    }
    if let Some((cols, rows)) = parse_mat_size(n) {
      let elem = s.fields[0].field_type.unwrap_known();
      return Some((cols as u16 * rows as u16, elem));
    }
  }
  None
}

/// Returns Some((cols, rows, element_type)) for matNxM types.
fn mat_kind(t: &Type) -> Option<(u16, u16, Type)> {
  if let Type::Struct(s) = t
    && let Some((cols, rows)) = parse_mat_size(&s.name)
  {
    let elem = s.fields[0].field_type.unwrap_known();
    return Some((cols as u16, rows as u16, elem));
  }
  None
}

fn arithmetic_op_for(elem: &Type, base: &str) -> Op {
  match (base, elem) {
    ("+", Type::F32) => Op::PlusF32,
    ("+", Type::I32) => Op::PlusI32,
    ("+", Type::U32) => Op::PlusU32,
    ("-", Type::F32) => Op::MinusF32,
    ("-", Type::I32) => Op::MinusI32,
    ("-", Type::U32) => Op::MinusU32,
    ("*", Type::F32) => Op::MultiplyF32,
    ("*", Type::I32) => Op::MultiplyI32,
    ("*", Type::U32) => Op::MultiplyU32,
    ("/", Type::F32) => Op::DivideF32,
    ("/", Type::I32) => Op::DivideI32,
    ("/", Type::U32) => Op::DivideU32,
    ("%", Type::F32) => Op::RemainderF32,
    ("%", Type::I32) => Op::RemainderI32,
    ("%", Type::U32) => Op::RemainderU32,
    _ => unreachable!("no scalar op for {base} on {elem:?}"),
  }
}

fn compare_op_for(elem: &Type, base: &str) -> Op {
  match (base, elem) {
    ("==", Type::F32) => Op::IsEqualF32,
    ("==", Type::I32) | ("==", Type::U32) => Op::IsEqualU32,
    ("==", Type::Bool) => Op::IsEqualBool,
    ("!=", Type::F32) => Op::IsNotEqualF32,
    ("!=", Type::I32) | ("!=", Type::U32) => Op::IsNotEqualU32,
    ("!=", Type::Bool) => Op::IsNotEqualBool,
    (">", Type::F32) => Op::GreaterThanF32,
    (">", Type::I32) => Op::GreaterThanI32,
    (">", Type::U32) => Op::GreaterThanU32,
    (">=", Type::F32) => Op::GreaterEqualF32,
    (">=", Type::I32) => Op::GreaterEqualI32,
    (">=", Type::U32) => Op::GreaterEqualU32,
    ("<", Type::F32) => Op::LessThanF32,
    ("<", Type::I32) => Op::LessThanI32,
    ("<", Type::U32) => Op::LessThanU32,
    ("<=", Type::F32) => Op::LessEqualF32,
    ("<=", Type::I32) => Op::LessEqualI32,
    ("<=", Type::U32) => Op::LessEqualU32,
    _ => unreachable!("no scalar compare op for {base} on {elem:?}"),
  }
}

fn min_op_for(elem: &Type) -> Op {
  match elem {
    Type::F32 => Op::MinF32,
    Type::I32 => Op::MinI32,
    Type::U32 => Op::MinU32,
    _ => unreachable!(),
  }
}
fn max_op_for(elem: &Type) -> Op {
  match elem {
    Type::F32 => Op::MaxF32,
    Type::I32 => Op::MaxI32,
    Type::U32 => Op::MaxU32,
    _ => unreachable!(),
  }
}
fn clamp_op_for(elem: &Type) -> Op {
  match elem {
    Type::F32 => Op::ClampF32,
    Type::I32 => Op::ClampI32,
    Type::U32 => Op::ClampU32,
    _ => unreachable!(),
  }
}
fn abs_op_for(elem: &Type) -> Op {
  match elem {
    Type::F32 => Op::AbsF32,
    Type::I32 => Op::AbsI32,
    Type::U32 => Op::Move, // u32 abs is identity; caller should special-case
    _ => unreachable!(),
  }
}
fn sign_op_for(elem: &Type) -> Op {
  match elem {
    Type::F32 => Op::SignF32,
    Type::I32 => Op::SignI32,
    _ => unreachable!(),
  }
}

/// Returns the conversion op needed to convert from src to dest element type.
/// Returns Op::Move when src == dest (just a copy).
fn conversion_op(src: &Type, dest: &Type) -> Op {
  match (src, dest) {
    (a, b) if a == b => Op::Move,
    (Type::F32, Type::U32) => Op::F32ToU32,
    (Type::F32, Type::I32) => Op::F32ToI32,
    (Type::F32, Type::Bool) => Op::F32ToBool,
    (Type::I32, Type::F32) => Op::I32ToF32,
    (Type::I32, Type::U32) => Op::I32ToU32,
    (Type::I32, Type::Bool) => Op::I32ToBool,
    (Type::U32, Type::F32) => Op::U32ToF32,
    (Type::U32, Type::I32) => Op::U32ToI32,
    (Type::U32, Type::Bool) => Op::U32ToBool,
    (Type::Bool, Type::F32) => Op::BoolToF32,
    (Type::Bool, Type::I32) => Op::BoolToI32,
    (Type::Bool, Type::U32) => Op::BoolToU32,
    _ => unreachable!("no conversion from {src:?} to {dest:?}"),
  }
}

/// Given a match-arm pattern for an enum scrutinee, return the variant index
/// that the pattern matches against. Patterns can be:
///   - `Name(variant_name)` — unit variant by name.
///   - `Application(constructor_fn, [bind_name])` — data variant.
fn enum_pattern_variant_index(pattern: &TypedExp, enum_type: &Enum) -> u32 {
  let variant_name: Arc<str> = match &pattern.kind {
    ExpKind::Name(name) => name.clone(),
    ExpKind::Application(f_box, _) => {
      if let TypeState::Known(Type::Function(f)) = &*f_box.data
        && let Some(abstract_ancestor) = &f.abstract_ancestor
      {
        let abstract_signature = abstract_ancestor.read().unwrap();
        if let FunctionImplementationKind::EnumConstructor(variant_name) =
          &abstract_signature.implementation
        {
          variant_name.clone()
        } else {
          panic!(
            "enum match arm Application pattern's function isn't an \
             EnumConstructor"
          )
        }
      } else {
        panic!("enum match arm Application pattern lacks Function type info")
      }
    }
    _ => panic!("unexpected enum match arm pattern kind: {:?}", pattern.kind),
  };
  enum_type
    .variants
    .iter()
    .position(|v| v.name == variant_name)
    .unwrap_or_else(|| {
      panic!(
        "no variant named {variant_name:?} in enum {:?}",
        enum_type.name
      )
    }) as u32
}

// =============================================================================
// TypedExp::compile_to_bytecode — the recursive tree walker that emits the
// bytecode for an entire program.
// =============================================================================

impl TypedExp {
  /// CPU-runtime-mode lowering for builtins the pure VM can't express: GPU
  /// orchestration, printing, windowing, and dynamic-array access. Returns
  /// `None` when `f_name` isn't such a builtin, letting the generic path
  /// handle it. Must run before generic argument compilation, since several
  /// of these take arguments (function references, dynamic globals, string
  /// literals) that can't be compiled as ordinary values.
  fn try_compile_cpu_builtin(
    &self,
    f_name: &str,
    args: &[TypedExp],
    state: &mut BytecodeCompilationState,
  ) -> Option<Option<u16>> {
    match f_name {
      "print" => {
        let arg = &args[0];
        if let ExpKind::StringLiteral(text) = &arg.kind {
          let string = state.host_string_index(&text.to_string());
          state.emit_host_op(HostOp::PrintString { string });
        } else if let ExpKind::Name(name) = &arg.kind
          && let Some(binding) = state.dynamic_globals.get(name).copied()
        {
          state.emit_host_op(HostOp::PrintBinding { binding });
        } else if let Type::Array(Some(ConcreteArraySize::Unsized), _) =
          arg.data.unwrap_known()
          && let ExpKind::Application(inner_f, inner_args) = &arg.kind
          && let ExpKind::Name(inner_name) = &inner_f.kind
          && &**inner_name == "zeroed-array"
        {
          // Print a runtime-sized zeroed array without materializing it.
          let len_slot =
            inner_args[0].compile_to_bytecode(false, state).unwrap();
          let ty = state.host_type_index(&arg.data.unwrap_known());
          state.emit_host_op(HostOp::PrintZeroed { len_slot, ty });
        } else {
          let slot = arg.compile_to_bytecode(false, state).unwrap();
          let ty = state.host_type_index(&arg.data.unwrap_known());
          state.emit_host_op(HostOp::Print { slot, ty });
        }
        Some(None)
      }
      "dispatch-compute-shader" => {
        let (entry_name, reads, writes) =
          state.resolve_dispatched_fn(&args[0]);
        let entry = state.host_string_index(&entry_name);
        let sets = state.host_dispatches.len() as u16;
        state.host_dispatches.push(HostDispatch { reads, writes });
        let workgroup_slot = args[1].compile_to_bytecode(false, state).unwrap();
        state.emit_host_op(HostOp::DispatchCompute {
          entry,
          sets,
          workgroup_slot,
        });
        Some(None)
      }
      "dispatch-render-shaders" => {
        let (vert_name, mut reads, mut writes) =
          state.resolve_dispatched_fn(&args[0]);
        let (frag_name, frag_reads, frag_writes) =
          state.resolve_dispatched_fn(&args[1]);
        for r in frag_reads {
          if !reads.contains(&r) {
            reads.push(r);
          }
        }
        for w in frag_writes {
          if !writes.contains(&w) {
            writes.push(w);
          }
        }
        let vert = state.host_string_index(&vert_name);
        let frag = state.host_string_index(&frag_name);
        let sets = state.host_dispatches.len() as u16;
        state.host_dispatches.push(HostDispatch { reads, writes });
        let vert_count_slot =
          args[2].compile_to_bytecode(false, state).unwrap();
        let additive_slot = args
          .get(3)
          .map(|a| a.compile_to_bytecode(false, state).unwrap());
        state.emit_host_op(HostOp::DispatchRender {
          vert,
          frag,
          sets,
          vert_count_slot,
          additive_slot,
        });
        Some(None)
      }
      "spawn-window" => {
        let Type::Function(signature) = args[0].data.unwrap_known() else {
          panic!("spawn-window argument had a non-function type")
        };
        let frame_name = signature
          .abstract_ancestor
          .as_ref()
          .expect("spawn-window callback had no abstract ancestor")
          .read()
          .unwrap()
          .name
          .clone();
        let frame_fn = state
          .finished_functions
          .iter()
          .position(|f| f.name == frame_name)
          .unwrap_or_else(|| {
            panic!("spawn-window frame fn {frame_name} not compiled")
          }) as u16;
        // Scope construction: write the captured values into the frame
        // function's trailing scope-argument slots. VM slots persist, so
        // mutations to captured state persist across frames exactly like
        // the tree-walker's Function::Scoped semantics.
        if let ExpKind::Application(_, captured) = &args[0].kind {
          let target = state.finished_functions[frame_fn as usize].clone();
          let scope_position = *target
            .arg_positions
            .last()
            .expect("scoped frame fn has no scope argument");
          let mut offset = 0u16;
          for captured_value in captured {
            let value_pos =
              captured_value.compile_to_bytecode(false, state).unwrap();
            let value_size = vm_type_size(&captured_value.data.unwrap_known());
            if value_size > 0 {
              state.push_instruction(Instruction {
                op: Op::Move,
                arg_positions: [value_pos, value_size, 0],
                return_position: scope_position + offset,
              });
            }
            offset += value_size;
          }
        }
        state.emit_host_op(HostOp::SpawnWindow { frame_fn });
        Some(None)
      }
      "close-window" => {
        state.emit_host_op(HostOp::CloseWindow);
        Some(None)
      }
      "start-audio" => {
        let Type::Function(signature) = args[0].data.unwrap_known() else {
          panic!("start-audio argument had a non-function type")
        };
        let entry_name = signature
          .abstract_ancestor
          .as_ref()
          .expect("start-audio argument had no abstract ancestor")
          .read()
          .unwrap()
          .name
          .clone();
        let entry = state.host_string_index(&entry_name);
        state.emit_host_op(HostOp::StartAudio { entry });
        Some(None)
      }
      "window-resolution" | "mouse-coords" => {
        let dest = state.take_stack_slot(2);
        state.emit_host_op(HostOp::WindowQuery {
          kind: if f_name == "window-resolution" {
            WindowQueryKind::Resolution
          } else {
            WindowQueryKind::MouseCoords
          },
          dest,
        });
        Some(Some(dest))
      }
      "window-time" | "window-delta-time" | "window-frame-index"
      | "mouse-present?" | "mouse-down?" | "mouse-just-down?" => {
        let dest = state.take_stack_slot(1);
        state.emit_host_op(HostOp::WindowQuery {
          kind: match f_name {
            "window-time" => WindowQueryKind::Time,
            "window-delta-time" => WindowQueryKind::DeltaTime,
            "window-frame-index" => WindowQueryKind::FrameIndex,
            "mouse-present?" => WindowQueryKind::MousePresent,
            "mouse-down?" => WindowQueryKind::MouseDown,
            _ => WindowQueryKind::MouseJustDown,
          },
          dest,
        });
        Some(Some(dest))
      }
      "key-down?" | "key-just-down?" => {
        let ExpKind::StringLiteral(key) = &args[0].kind else {
          panic!("key query argument must be a string literal")
        };
        let key = state.host_string_index(&key.to_string());
        let dest = state.take_stack_slot(1);
        state.emit_host_op(HostOp::KeyQuery {
          just: f_name == "key-just-down?",
          key,
          dest,
        });
        Some(Some(dest))
      }
      "array-length" => {
        if let ExpKind::Name(name) = &args[0].kind
          && let Some(binding) = state.dynamic_globals.get(name).copied()
        {
          let dest = state.take_stack_slot(1);
          state.emit_host_op(HostOp::DynLen { binding, dest });
          Some(Some(dest))
        } else if let Type::Array(Some(ConcreteArraySize::Literal(n)), _) =
          args[0].data.unwrap_known()
        {
          Some(Some(state.emit_u32_constant(n as u32)))
        } else {
          panic!("array-length of unsupported array kind in VM CPU runtime")
        }
      }
      "texture-dimensions" => {
        let ExpKind::Name(name) = &args[0].kind else {
          panic!("texture-dimensions argument must be a texture global")
        };
        let binding = *state
          .dynamic_globals
          .get(name)
          .expect("texture-dimensions argument isn't a texture binding");
        let dest = state.take_stack_slot(2);
        state.emit_host_op(HostOp::TextureDims { binding, dest });
        Some(Some(dest))
      }
      "set-render-target" => {
        let ExpKind::Name(name) = &args[0].kind else {
          panic!("set-render-target argument must be a texture global")
        };
        let binding = *state
          .dynamic_globals
          .get(name)
          .expect("set-render-target argument isn't a texture binding");
        state.emit_host_op(HostOp::SetRenderTarget { binding });
        Some(None)
      }
      "clear-render-target" => {
        state.emit_host_op(HostOp::ClearRenderTarget);
        Some(None)
      }
      "=" => {
        // Only intercepted when a dynamic (host-side) global is involved;
        // slot-backed assignment uses the generic path.
        let lhs = &args[0];
        let rhs = &args[1];
        if let ExpKind::Name(name) = &lhs.kind
          && let Some(binding) = state.dynamic_globals.get(name).copied()
        {
          let ExpKind::Application(rhs_f, rhs_args) = &rhs.kind else {
            panic!(
              "unsupported assignment to dynamic global in VM CPU runtime"
            )
          };
          let ExpKind::Name(rhs_f_name) = &rhs_f.kind else {
            panic!()
          };
          match &**rhs_f_name {
            "zeroed-array" => {
              let len_slot =
                rhs_args[0].compile_to_bytecode(false, state).unwrap();
              state.emit_host_op(HostOp::AssignDynZeroed { binding, len_slot });
            }
            "into-dynamic-array" => {
              let source = &rhs_args[0];
              let Type::Array(Some(ConcreteArraySize::Literal(count)), _) =
                source.data.unwrap_known()
              else {
                panic!("into-dynamic-array argument wasn't a sized array")
              };
              let src_slot = source.compile_to_bytecode(false, state).unwrap();
              state.emit_host_op(HostOp::AssignDynFromSlots {
                binding,
                src_slot,
                count: count as u16,
              });
            }
            "load-image" => {
              let ExpKind::StringLiteral(path) = &rhs_args[0].kind else {
                panic!("load-image argument must be a string literal")
              };
              let path = state.host_string_index(&path.to_string());
              state.emit_host_op(HostOp::AssignTextureFromImage {
                binding,
                path,
              });
            }
            "blank-texture" => {
              // Both arg shapes compile to two consecutive u32 slots: (w h)
              // scalars are contiguous when compiled in sequence; a vec2u is
              // contiguous by construction.
              let size_slot = if rhs_args.len() == 2 {
                let w = rhs_args[0].compile_to_bytecode(false, state).unwrap();
                let h = rhs_args[1].compile_to_bytecode(false, state).unwrap();
                debug_assert_eq!(h, w + 1);
                w
              } else {
                rhs_args[0].compile_to_bytecode(false, state).unwrap()
              };
              state.emit_host_op(HostOp::AssignTextureBlank {
                binding,
                size_slot,
              });
            }
            other => panic!(
              "unsupported assignment of `{other}` result to dynamic global \
               in VM CPU runtime"
            ),
          }
          Some(None)
        } else if let ExpKind::Access(Accessor::ArrayIndex(index_exp), inner) =
          &lhs.kind
          && let ExpKind::Name(name) = &inner.kind
          && let Some(binding) = state.dynamic_globals.get(name).copied()
        {
          let index_slot =
            index_exp.compile_to_bytecode(false, state).unwrap();
          let src_slot = rhs.compile_to_bytecode(false, state).unwrap();
          state.emit_host_op(HostOp::DynStore {
            binding,
            index_slot,
            src_slot,
          });
          Some(None)
        } else {
          None
        }
      }
      _ => None,
    }
  }

  pub fn compile_to_bytecode(
    &self,
    is_ref_arg_position: bool,
    state: &mut BytecodeCompilationState,
  ) -> Option<u16> {
    use ExpKind::*;
    match &self.kind {
      NumberLiteral(number) => {
        let u = match number {
          Number::Int(i) => match self.data.unwrap_known() {
            Type::U32 => *i as u32,
            Type::I32 => (*i as i32) as u32,
            Type::F32 => (*i as f32).to_bits(),
            _ => panic!(),
          },
          Number::Float(f) => (*f as f32).to_bits(),
        };
        Some(state.emit_u32_constant(u))
      }
      BooleanLiteral(b) => Some(state.emit_u32_constant(*b as u32)),
      Block(exps) => exps
        .iter()
        .map(|exp| exp.compile_to_bytecode(false, state))
        .last()
        .unwrap_or(None),
      Application(f_exp, args) => {
        let ExpKind::Name(f_name) = &f_exp.kind else {
          panic!()
        };
        let f_name = f_name.clone();
        // CPU-runtime mode: mirror the tree-walker's sync behavior at the
        // same granularity — `check_cpu_readable` for the application's
        // read set before it evaluates, `mark_cpu_written` for its write
        // set after. All name resolution happens here at compile time.
        let cpu_write_marks: Option<Vec<Arc<str>>> = if state.cpu_mode {
          let effects = self.effects();
          let (reads, writes) = effects.read_and_written_globals();
          state.emit_sync_checks(&reads);
          Some(writes)
        } else {
          None
        };
        if state.cpu_mode
          && let Some(result) =
            self.try_compile_cpu_builtin(&f_name, args, state)
        {
          if let Some(writes) = &cpu_write_marks {
            state.emit_write_marks(writes);
          }
          return result;
        }
        let Type::Function(f_signature) = f_exp.data.unwrap_known() else {
          panic!()
        };
        if f_signature.abstract_ancestor.is_none() {
          // Scope construction for a closure value (produced by
          // extract_inner_functions): a closure's VM representation is just
          // its captured scope data, laid out like the scope struct.
          let total: u16 = args
            .iter()
            .map(|a| vm_type_size(&a.data.unwrap_known()))
            .sum();
          let result = state.take_stack_slot(total);
          let mut offset = 0u16;
          for arg in args {
            let arg_pos = arg.compile_to_bytecode(false, state).unwrap();
            let size = vm_type_size(&arg.data.unwrap_known());
            if size > 0 {
              state.push_instruction(Instruction {
                op: Op::Move,
                arg_positions: [arg_pos, size, 0],
                return_position: result + offset,
              });
            }
            offset += size;
          }
          if let Some(writes) = &cpu_write_marks {
            state.emit_write_marks(writes);
          }
          return Some(result);
        }
        state.array_mut_ref_store_instructions.push(vec![]);
        let Type::Function(f) = f_exp.data.unwrap_known() else {
          panic!()
        };
        let abstract_f = f.abstract_ancestor.unwrap();
        let abstract_f = abstract_f.read().unwrap();
        let arg_positions: Vec<u16> = args
          .iter()
          .zip(abstract_f.arg_types.iter())
          .map(|(arg, (_, ownership))| {
            arg
              .compile_to_bytecode(*ownership != Ownership::Owned, state)
              .unwrap()
          })
          .collect();
        // Use the application expression's own type rather than the
        // signature's return type: it's always at least as resolved (e.g. a
        // const-generic `[N: T]` return annotated concrete at the call
        // site).
        let return_type = self.data.unwrap_known();
        let arg_types: Vec<Type> = f
          .args
          .iter()
          .map(|(arg, _)| arg.var_type.unwrap_known())
          .collect();
        let result_pos = match &abstract_f.implementation {
          FunctionImplementationKind::Builtin { .. } => state.compile_builtin(
            &f_name,
            args,
            &arg_positions,
            &arg_types,
            &return_type,
          ),
          FunctionImplementationKind::StructConstructor => {
            let struct_slot_pos = state.take_stack_slot(
              self
                .data
                .unwrap_known()
                .data_size_in_u32s(&self.source_trace)
                .unwrap() as u16,
            );
            let mut offset = 0u16;
            for arg in args {
              let arg_pos = arg.compile_to_bytecode(false, state).unwrap();
              let arg_size = arg
                .data
                .unwrap_known()
                .data_size_in_u32s(&arg.source_trace)
                .unwrap() as u16;
              state.push_instruction(Instruction {
                op: Op::Move,
                arg_positions: [arg_pos, arg_size, 0],
                return_position: struct_slot_pos + offset,
              });
              offset += arg_size;
            }
            Some(struct_slot_pos)
          }
          FunctionImplementationKind::EnumConstructor(variant_name) => {
            // Enum layout: [discriminant: u32, data: u32; N] where N is the
            // max variant inner size.
            let Type::Enum(ref enum_type) = return_type else {
              panic!("EnumConstructor: return type isn't Enum")
            };
            let variant_index = enum_type
              .variants
              .iter()
              .position(|v| v.name == *variant_name)
              .expect("EnumConstructor variant not found in enum")
              as u32;
            let total_size =
              return_type.data_size_in_u32s(&self.source_trace).unwrap() as u16;
            let result = state.take_stack_slot(total_size);
            // Discriminant at result[0] — first slot of the freshly allocated
            // region. We write it directly rather than using emit_u32_constant
            // because emit_* takes a fresh slot; we want to write into the
            // already-reserved `result`.
            state.push_instruction(Instruction {
              op: Op::Constant,
              arg_positions: [
                (variant_index >> 16) as u16,
                variant_index as u16,
                0,
              ],
              return_position: result,
            });
            let mut offset = 1u16;
            for (arg_i, arg) in args.iter().enumerate() {
              let arg_size = arg
                .data
                .unwrap_known()
                .data_size_in_u32s(&arg.source_trace)
                .unwrap() as u16;
              state.push_instruction(Instruction {
                op: Op::Move,
                arg_positions: [arg_positions[arg_i], arg_size, 0],
                return_position: result + offset,
              });
              offset += arg_size;
            }
            Some(result)
          }
          FunctionImplementationKind::Composite(_) => {
            let return_size = return_type
              .data_size_in_u32s(&f_exp.source_trace)
              .unwrap() as u16;
            let result_position = state.take_stack_slot(return_size);
            if state
              .ref_arg_functions
              .iter()
              .find(|(ref_fn_name, _)| *ref_fn_name == f_name)
              .is_some()
            {
              let mut arg_move_positions = vec![];
              for _ in 0..arg_positions.len() {
                arg_move_positions.push(state.instructions.len() as u32);
                state.push_instruction(Instruction {
                  op: Op::Move,
                  arg_positions: [0, 0, 0],
                  return_position: 0,
                });
              }
              let fn_dispatch_position = state.instructions.len() as u32;
              state.push_instruction(Instruction {
                op: Op::InvokeFunction,
                arg_positions: [0, 0, 0],
                return_position: 0,
              });
              let return_move_position = state.instructions.len() as u32;
              state.push_instruction(Instruction {
                op: Op::Move,
                arg_positions: [0, return_size, 0],
                return_position: result_position,
              });
              state
                .pending_ref_arg_function_usages
                .push(PendingRefFnUsage {
                  name: f_name,
                  arg_move_positions,
                  fn_dispatch_position,
                  return_move_position,
                  arg_positions,
                });
              Some(result_position)
            } else {
              let (fn_index, bytecode_fn) = state
                .finished_functions
                .iter()
                .enumerate()
                .find_map(|(i, f)| {
                  (f.name == f_name).then(|| (i as u16, f.clone()))
                })
                .unwrap();

              for (arg_pos, (fn_arg_pos, arg_size)) in
                arg_positions.iter().copied().zip(
                  bytecode_fn
                    .arg_positions
                    .iter()
                    .copied()
                    .zip(bytecode_fn.arg_sizes.iter().copied()),
                )
              {
                state.push_instruction(Instruction {
                  op: Op::Move,
                  arg_positions: [arg_pos, arg_size, 0],
                  return_position: fn_arg_pos,
                });
              }
              state.push_instruction(Instruction {
                op: Op::InvokeFunction,
                arg_positions: [fn_index, 0, 0],
                return_position: 0,
              });
              state.push_instruction(Instruction {
                op: Op::Move,
                arg_positions: [bytecode_fn.stack_frame_start, return_size, 0],
                return_position: result_position,
              });
              Some(result_position)
            }
          }
        };
        let array_mut_ref_store_instructions =
          state.array_mut_ref_store_instructions.pop().unwrap();
        for instruction in array_mut_ref_store_instructions {
          state.push_instruction(instruction);
        }
        if let Some(writes) = &cpu_write_marks {
          state.emit_write_marks(writes);
        }
        result_pos
      }
      Name(name) => {
        if state.dynamic_globals.contains_key(name) {
          panic!(
            "dynamic global {name:?} used in a position the VM CPU runtime \
             doesn't support (only element access, array-length, \
             assignment, and print)"
          );
        }
        if let Some(slot) = state
          .globals
          .get(name)
          .copied()
          .or_else(|| state.locals.get(name).copied())
        {
          return Some(slot);
        }
        // Unit enum variant referenced by name: emit discriminant constant
        // into the first slot of a `total_size`-sized region (rest is unused
        // padding for the data portion).
        if let Type::Enum(enum_type) = self.data.unwrap_known()
          && let Some(idx) =
            enum_type.variants.iter().position(|v| v.name == *name)
        {
          let total_size = self
            .data
            .unwrap_known()
            .data_size_in_u32s(&self.source_trace)
            .unwrap() as u16;
          let result = state.take_stack_slot(total_size);
          state.push_instruction(Instruction {
            op: Op::Constant,
            arg_positions: [(idx as u32 >> 16) as u16, idx as u16, 0],
            return_position: result,
          });
          return Some(result);
        }
        panic!("Name {name:?} not found in scope")
      }
      Function(args, exp) => {
        let Type::Function(f) = self.data.unwrap_known() else {
          panic!()
        };
        for ((arg_name, _), arg_position) in args.iter().zip(
          state
            .current_function
            .as_ref()
            .unwrap()
            .arg_positions
            .iter(),
        ) {
          state.locals.insert(arg_name.clone(), *arg_position);
        }
        if let Some(result_position) = exp.compile_to_bytecode(false, state) {
          state.push_instruction(Instruction {
            op: Op::Move,
            arg_positions: [
              result_position,
              f.return_type
                .unwrap_known()
                .data_size_in_u32s(&exp.source_trace)
                .unwrap() as u16,
              0,
            ],
            return_position: state
              .current_function
              .as_ref()
              .unwrap()
              .stack_frame_start,
          });
        }
        None
      }
      Let(bindings, body) => {
        for (name, _, _, value) in bindings.iter() {
          // If the value is an Uninitialized placeholder (introduced by
          // deexpressionification when lifting match/if into statements), we
          // need to allocate a fresh slot of the binding's type so subsequent
          // assignments have a destination.
          if matches!(value.kind, ExpKind::Uninitialized) {
            let size = value
              .data
              .unwrap_known()
              .data_size_in_u32s(&value.source_trace)
              .unwrap() as u16;
            let slot = state.take_stack_slot(size);
            state.locals.insert(name.clone(), slot);
          } else {
            let value_pos = value.compile_to_bytecode(false, state).unwrap();
            state.locals.insert(name.clone(), value_pos);
          }
        }
        body.compile_to_bytecode(false, state)
      }
      Match(scrutinee, arms) => {
        let result_type = self.data.unwrap_known();
        let result_type_size =
          result_type.data_size_in_u32s(&self.source_trace).unwrap() as u16;
        let scrutinee_pos =
          scrutinee.compile_to_bytecode(false, state).unwrap();
        let scrutinee_type = scrutinee.data.unwrap_known();
        if scrutinee_type == Type::Bool
          && arms[0].0.kind == ExpKind::BooleanLiteral(true)
        {
          let result_pos = if result_type_size > 0 {
            Some(state.take_stack_slot(result_type_size))
          } else {
            None
          };
          let true_branch_jump_instruction_pos = state.instructions.len();
          state.push_instruction(Instruction {
            op: Op::JumpWhen,
            arg_positions: [scrutinee_pos, 0, 0],
            return_position: 0,
          });
          if let Some(false_branch_result_pos) =
            arms[1].1.compile_to_bytecode(false, state)
          {
            state.push_instruction(Instruction {
              op: Op::Move,
              arg_positions: [false_branch_result_pos, result_type_size, 0],
              return_position: result_pos.unwrap(),
            });
          }
          let false_branch_end_jump_instruction_pos = state.instructions.len();
          state.push_instruction(Instruction {
            op: Op::Jump,
            arg_positions: [0, 0, 0],
            return_position: 0,
          });
          let true_branch_start_instruction_pos =
            state.instructions.len() as u32;
          state.instructions[true_branch_jump_instruction_pos].arg_positions
            [1] = (true_branch_start_instruction_pos >> 16) as u16;
          state.instructions[true_branch_jump_instruction_pos].arg_positions
            [2] = true_branch_start_instruction_pos as u16;
          if let Some(true_branch_result_pos) =
            arms[0].1.compile_to_bytecode(false, state)
          {
            state.push_instruction(Instruction {
              op: Op::Move,
              arg_positions: [true_branch_result_pos, result_type_size, 0],
              return_position: result_pos.unwrap(),
            });
          }
          let true_branch_end_pos = state.instructions.len() as u32;
          state.instructions[false_branch_end_jump_instruction_pos]
            .arg_positions[0] = (true_branch_end_pos >> 16) as u16;
          state.instructions[false_branch_end_jump_instruction_pos]
            .arg_positions[1] = true_branch_end_pos as u16;
          result_pos
        } else {
          match scrutinee_type {
            Type::F32 | Type::I32 | Type::U32 | Type::Bool => {
              let result_pos = if result_type_size > 0 {
                Some(state.take_stack_slot(result_type_size))
              } else {
                None
              };
              let mut arms = arms.clone();
              let last_arm_body = arms.pop().unwrap().1;
              let mut jump_into_block_instruction_positions = vec![];
              for (pattern, _) in arms.iter() {
                let pattern_pos =
                  pattern.compile_to_bytecode(false, state).unwrap();
                let equality_check_pos = state.take_stack_slot(1);
                state.push_instruction(Instruction {
                  op: match scrutinee_type {
                    Type::F32 => Op::IsEqualF32,
                    Type::I32 | Type::U32 => Op::IsEqualU32,
                    Type::Bool => Op::IsEqualBool,
                    _ => unreachable!(),
                  },
                  arg_positions: [scrutinee_pos, pattern_pos, 0],
                  return_position: equality_check_pos,
                });
                jump_into_block_instruction_positions
                  .push(state.instructions.len());
                state.push_instruction(Instruction {
                  op: Op::JumpWhen,
                  arg_positions: [equality_check_pos, 0, 0],
                  return_position: 0,
                });
              }
              if let Some(last_arm_result_pos) =
                last_arm_body.compile_to_bytecode(false, state)
              {
                state.push_instruction(Instruction {
                  op: Op::Move,
                  arg_positions: [last_arm_result_pos, result_type_size, 0],
                  return_position: result_pos.unwrap(),
                });
              }
              let mut arm_end_instruction_positions =
                vec![state.instructions.len()];
              state.push_instruction(Instruction {
                op: Op::Jump,
                arg_positions: [0, 0, 0],
                return_position: 0,
              });
              for (i, (_, body)) in arms.into_iter().enumerate() {
                let start_pos = state.instructions.len();
                state.instructions[jump_into_block_instruction_positions[i]]
                  .arg_positions[1] = (start_pos >> 16) as u16;
                state.instructions[jump_into_block_instruction_positions[i]]
                  .arg_positions[2] = start_pos as u16;
                if let Some(arm_result_pos) =
                  body.compile_to_bytecode(false, state)
                {
                  state.push_instruction(Instruction {
                    op: Op::Move,
                    arg_positions: [arm_result_pos, result_type_size, 0],
                    return_position: result_pos.unwrap(),
                  });
                }
                arm_end_instruction_positions.push(state.instructions.len());
                state.push_instruction(Instruction {
                  op: Op::Jump,
                  arg_positions: [0, 0, 0],
                  return_position: 0,
                });
              }
              let match_block_end_pos = state.instructions.len();
              for arm_end_pos in arm_end_instruction_positions {
                state.instructions[arm_end_pos].arg_positions[0] =
                  (match_block_end_pos >> 16) as u16;
                state.instructions[arm_end_pos].arg_positions[1] =
                  match_block_end_pos as u16;
              }
              result_pos
            }
            Type::Enum(ref enum_type) => {
              // Enum scrutinee layout: [discriminant, data...].
              // scrutinee_pos points to discriminant.
              // Inner data begins at scrutinee_pos + 1.
              let result_pos = if result_type_size > 0 {
                Some(state.take_stack_slot(result_type_size))
              } else {
                None
              };
              let mut arms = arms.clone();
              let last_arm = arms.pop().unwrap();
              let last_arm_pattern = last_arm.0;
              let last_arm_body = last_arm.1;
              // Determine if the last arm has a data binding (e.g. `(Some x)`).
              // If so, bind it before compiling the body.
              if let ExpKind::Application(f_box, pattern_args) =
                &last_arm_pattern.kind
              {
                if let TypeState::Known(Type::Function(_)) = &*f_box.data {
                  if let Some((_, _, inner_name)) =
                    TypedExp::try_deconstruct_enum_pattern(f_box, pattern_args)
                  {
                    state.locals.insert(inner_name.clone(), scrutinee_pos + 1);
                  }
                }
              }
              let mut jump_into_block_instruction_positions = vec![];
              for (pattern, _) in arms.iter() {
                let variant_index =
                  enum_pattern_variant_index(pattern, enum_type);
                let const_slot = state.emit_u32_constant(variant_index);
                let eq_pos =
                  state.emit_binary(Op::IsEqualU32, scrutinee_pos, const_slot);
                jump_into_block_instruction_positions
                  .push(state.instructions.len());
                state.push_instruction(Instruction {
                  op: Op::JumpWhen,
                  arg_positions: [eq_pos, 0, 0],
                  return_position: 0,
                });
              }
              if let Some(last_arm_result_pos) =
                last_arm_body.compile_to_bytecode(false, state)
              {
                state.push_instruction(Instruction {
                  op: Op::Move,
                  arg_positions: [last_arm_result_pos, result_type_size, 0],
                  return_position: result_pos.unwrap(),
                });
              }
              let mut arm_end_instruction_positions =
                vec![state.instructions.len()];
              state.push_instruction(Instruction {
                op: Op::Jump,
                arg_positions: [0, 0, 0],
                return_position: 0,
              });
              for (i, (pattern, body)) in arms.into_iter().enumerate() {
                let start_pos = state.instructions.len();
                state.instructions[jump_into_block_instruction_positions[i]]
                  .arg_positions[1] = (start_pos >> 16) as u16;
                state.instructions[jump_into_block_instruction_positions[i]]
                  .arg_positions[2] = start_pos as u16;
                if let ExpKind::Application(f_box, pattern_args) = &pattern.kind
                {
                  if let TypeState::Known(Type::Function(_)) = &*f_box.data {
                    if let Some((_, _, inner_name)) =
                      TypedExp::try_deconstruct_enum_pattern(
                        f_box,
                        pattern_args,
                      )
                    {
                      state
                        .locals
                        .insert(inner_name.clone(), scrutinee_pos + 1);
                    }
                  }
                }
                if let Some(arm_result_pos) =
                  body.compile_to_bytecode(false, state)
                {
                  state.push_instruction(Instruction {
                    op: Op::Move,
                    arg_positions: [arm_result_pos, result_type_size, 0],
                    return_position: result_pos.unwrap(),
                  });
                }
                arm_end_instruction_positions.push(state.instructions.len());
                state.push_instruction(Instruction {
                  op: Op::Jump,
                  arg_positions: [0, 0, 0],
                  return_position: 0,
                });
              }
              let match_block_end_pos = state.instructions.len();
              for arm_end_pos in arm_end_instruction_positions {
                state.instructions[arm_end_pos].arg_positions[0] =
                  (match_block_end_pos >> 16) as u16;
                state.instructions[arm_end_pos].arg_positions[1] =
                  match_block_end_pos as u16;
              }
              result_pos
            }
            Type::String => todo!(),
            Type::Struct(_) => todo!(),
            Type::Function(_) => todo!(),
            Type::Array(_, _) => todo!(),
            Type::Skolem(_, _) | Type::Unit => panic!(),
          }
        }
      }
      WhileLoop {
        condition_expression,
        body_expression,
      } => {
        let loop_start_pos = state.instructions.len() as u32;
        state.loop_start_instructions.push(loop_start_pos);
        state.break_jump_instruction_positions.push(vec![]);
        state.continue_jump_instruction_positions.push(vec![]);
        let cond_pos = condition_expression
          .compile_to_bytecode(false, state)
          .unwrap();
        let jump_out_instruction_pos = state.instructions.len();
        state.push_instruction(Instruction {
          op: Op::JumpWhenNot,
          arg_positions: [cond_pos, 0, 0],
          return_position: 0,
        });
        body_expression.compile_to_bytecode(false, state);
        state.push_instruction(Instruction {
          op: Op::Jump,
          arg_positions: [
            (loop_start_pos >> 16) as u16,
            loop_start_pos as u16,
            0,
          ],
          return_position: 0,
        });
        let loop_end_pos = state.instructions.len();
        state.instructions[jump_out_instruction_pos].arg_positions[1] =
          (loop_end_pos >> 16) as u16;
        state.instructions[jump_out_instruction_pos].arg_positions[2] =
          loop_end_pos as u16;
        state.loop_start_instructions.pop();
        for break_jump_pos in
          state.break_jump_instruction_positions.pop().unwrap()
        {
          state.instructions[break_jump_pos as usize].arg_positions[0] =
            (loop_end_pos >> 16) as u16;
          state.instructions[break_jump_pos as usize].arg_positions[1] =
            loop_end_pos as u16;
        }
        for continue_jump_pos in
          state.continue_jump_instruction_positions.pop().unwrap()
        {
          state.instructions[continue_jump_pos as usize].arg_positions[0] =
            (loop_start_pos >> 16) as u16;
          state.instructions[continue_jump_pos as usize].arg_positions[1] =
            loop_start_pos as u16;
        }
        None
      }
      ForLoop {
        increment_variable_name,
        increment_variable_initial_value_expression,
        continue_condition_expression,
        update_expression,
        body_expression,
        increment_variable_type,
      } => {
        let increment_var_initial_value_pos =
          increment_variable_initial_value_expression
            .compile_to_bytecode(false, state)
            .unwrap();
        let increment_var_size = increment_variable_type
          .unwrap_known()
          .data_size_in_u32s(&self.source_trace)
          .unwrap() as u16;
        let increment_var_pos = state.take_stack_slot(increment_var_size);
        state.push_instruction(Instruction {
          op: Op::Move,
          arg_positions: [
            increment_var_initial_value_pos,
            increment_var_size,
            0,
          ],
          return_position: increment_var_pos,
        });
        state
          .locals
          .insert(increment_variable_name.0.clone(), increment_var_pos);
        let loop_start_pos = state.instructions.len() as u32;
        state.loop_start_instructions.push(loop_start_pos);
        state.break_jump_instruction_positions.push(vec![]);
        state.continue_jump_instruction_positions.push(vec![]);
        let cond_pos = continue_condition_expression
          .compile_to_bytecode(false, state)
          .unwrap();
        let jump_out_instruction_pos = state.instructions.len();
        state.push_instruction(Instruction {
          op: Op::JumpWhenNot,
          arg_positions: [cond_pos, 0, 0],
          return_position: 0,
        });
        body_expression.compile_to_bytecode(false, state);
        let pre_update_pos = state.instructions.len();
        if let Some(update_expression) = update_expression {
          update_expression.compile_to_bytecode(false, state);
        }
        state.push_instruction(Instruction {
          op: Op::Jump,
          arg_positions: [
            (loop_start_pos >> 16) as u16,
            loop_start_pos as u16,
            0,
          ],
          return_position: 0,
        });
        let loop_end_pos = state.instructions.len();
        state.instructions[jump_out_instruction_pos].arg_positions[1] =
          (loop_end_pos >> 16) as u16;
        state.instructions[jump_out_instruction_pos].arg_positions[2] =
          loop_end_pos as u16;
        state.loop_start_instructions.pop();
        for break_jump_pos in
          state.break_jump_instruction_positions.pop().unwrap()
        {
          state.instructions[break_jump_pos as usize].arg_positions[0] =
            (loop_end_pos >> 16) as u16;
          state.instructions[break_jump_pos as usize].arg_positions[1] =
            loop_end_pos as u16;
        }
        for continue_jump_pos in
          state.continue_jump_instruction_positions.pop().unwrap()
        {
          state.instructions[continue_jump_pos as usize].arg_positions[0] =
            (pre_update_pos >> 16) as u16;
          state.instructions[continue_jump_pos as usize].arg_positions[1] =
            pre_update_pos as u16;
        }
        None
      }
      Break => {
        let jump_pos = state.instructions.len() as u32;
        state.push_instruction(Instruction {
          op: Op::Jump,
          arg_positions: [0, 0, 0],
          return_position: 0,
        });
        state
          .break_jump_instruction_positions
          .last_mut()
          .unwrap()
          .push(jump_pos);
        None
      }
      Continue => {
        let jump_pos = state.instructions.len() as u32;
        state.push_instruction(Instruction {
          op: Op::Jump,
          arg_positions: [0, 0, 0],
          return_position: 0,
        });
        state
          .continue_jump_instruction_positions
          .last_mut()
          .unwrap()
          .push(jump_pos);
        None
      }
      Return(exp) => {
        if let Some(return_value_pos) = exp.compile_to_bytecode(false, state) {
          state.push_instruction(Instruction {
            op: Op::Move,
            arg_positions: [
              return_value_pos,
              exp
                .data
                .unwrap_known()
                .data_size_in_u32s(&exp.source_trace)
                .unwrap() as u16,
              0,
            ],
            return_position: state
              .current_function
              .as_ref()
              .unwrap()
              .stack_frame_start,
          });
        }
        state.push_instruction(Instruction {
          op: Op::Return,
          arg_positions: [0, 0, 0],
          return_position: 0,
        });
        None
      }
      ArrayLiteral(inner_expressions) => {
        let inner_data_size = inner_expressions[0]
          .data
          .unwrap_known()
          .data_size_in_u32s(&self.source_trace)
          .unwrap() as u16;
        let array_pos = state
          .take_stack_slot(inner_data_size * (inner_expressions.len() as u16));
        for (i, inner_exp) in inner_expressions.iter().enumerate() {
          let inner_exp_pos =
            inner_exp.compile_to_bytecode(false, state).unwrap();
          state.push_instruction(Instruction {
            op: Op::Move,
            arg_positions: [inner_exp_pos, inner_data_size, 0],
            return_position: array_pos + (i as u16 * inner_data_size),
          });
        }
        Some(array_pos)
      }
      Access(accessor, exp) => {
        match accessor {
          Accessor::ArrayIndex(index_exp) => {
            // Element read of a dynamic (host-side) global array.
            if state.cpu_mode
              && let ExpKind::Name(name) = &exp.kind
              && let Some(binding) = state.dynamic_globals.get(name).copied()
            {
              if is_ref_arg_position {
                panic!(
                  "passing dynamic array elements by reference is not \
                   supported in the VM CPU runtime"
                );
              }
              let element_size = self
                .data
                .unwrap_known()
                .data_size_in_u32s(&self.source_trace)
                .unwrap() as u16;
              let dest = state.take_stack_slot(element_size);
              let index_slot =
                index_exp.compile_to_bytecode(false, state).unwrap();
              state.emit_host_op(HostOp::DynLoad {
                binding,
                index_slot,
                dest,
              });
              return Some(dest);
            }
            let inner_exp_pos = exp.compile_to_bytecode(false, state).unwrap();
            let inner_data_size = self
              .data
              .unwrap_known()
              .data_size_in_u32s(&self.source_trace)
              .unwrap() as u16;
            let result_position = state.take_stack_slot(inner_data_size);
            let index_pos =
              index_exp.compile_to_bytecode(false, state).unwrap();
            state.push_instruction(Instruction {
              op: Op::ArrayLookup,
              arg_positions: [inner_exp_pos, index_pos, inner_data_size],
              return_position: result_position,
            });
            if is_ref_arg_position {
              state
                .array_mut_ref_store_instructions
                .last_mut()
                .unwrap()
                .push(Instruction {
                  op: Op::ArrayStore,
                  arg_positions: [result_position, index_pos, inner_data_size],
                  return_position: inner_exp_pos,
                });
            }
            Some(result_position)
          }
          Accessor::Field(field_name) => {
            let inner_exp_pos = exp.compile_to_bytecode(false, state).unwrap();
            let Type::Struct(s) = exp.data.unwrap_known() else {
              panic!()
            };
            let mut offset = 0;
            for field in s.fields.iter() {
              if field.name == *field_name {
                break;
              }
              offset += field
                .field_type
                .unwrap_known()
                .data_size_in_u32s(&exp.source_trace)
                .unwrap();
            }
            Some(inner_exp_pos + (offset as u16))
          }
          Accessor::Swizzle(swizzle_fields) => {
            // For swizzles we just copy the values to a new location so that
            // we have the resulting vector as a contiguous chunk of memory.
            // This would cause problems if there was any kind of assignment
            // into a swizzle, but that should have always been desugared away
            // by an earlier stage of the compiler
            let inner_exp_pos = exp.compile_to_bytecode(false, state).unwrap();
            let inner_data_size = self
              .data
              .unwrap_known()
              .data_size_in_u32s(&self.source_trace)
              .unwrap() as u16;
            let result_position = state.take_stack_slot(inner_data_size);
            for (i, field) in swizzle_fields.iter().enumerate() {
              state.push_instruction(Instruction {
                op: Op::Move,
                arg_positions: [inner_exp_pos + field.index() as u16, 1, 0],
                return_position: result_position + i as u16,
              });
            }
            Some(result_position)
          }
        }
      }
      StringLiteral(_) => panic!("bytecode vm can't handle strings yet!"),
      Discard => panic!("bytecode vm can't handle discard statements"),
      Uninitialized | Wildcard | Unit => None,
    }
  }
}
