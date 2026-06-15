use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
  Move,
  InvokeFunction,
  Constant,
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
}

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
  fn max_touched_index(&self) -> u16 {
    match self.op {
      Op::InvokeFunction => 0,
      Op::Constant => self.return_position,
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
        code
          .function_instructions
          .get_unchecked(instruction_index as usize)
      };
      match instruction.op {
        Op::InvokeFunction => {
          call_stack.push(ip.clone());
          ip = code.functions[instruction.arg_positions[0] as usize]
            .instructions
            .clone();
        }
        Op::Move => unsafe {
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            *stack.get_unchecked(instruction.arg_positions[0] as usize)
        },
        Op::Constant => unsafe {
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            ((instruction.arg_positions[0] as u32) << 16u32)
              | instruction.arg_positions[1] as u32;
        },
        Op::Cos => instruction.f32_unary(stack, f32::cos),
        _ => todo!(),
      }
    }
  }
}
