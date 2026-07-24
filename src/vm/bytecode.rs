//! The bytecode VM: `Op`, `Instruction`, `Function`, `Code`,
//! `BytecodeProgram`, and the `execute()` dispatch loop. The compiler that
//! produces these instructions lives in `crate::vm::compile`.

use std::ops::Range;
use std::sync::Arc;

use crate::compiler::{types::Type, vars::VariableAddressSpace};

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

  // Data packing (multi-slot operands start at arg_positions[0]; the slots
  // they span are covered by Code::min_stack_size)
  PackSnorm4x8,
  UnpackSnorm4x8,
  PackUnorm4x8,
  UnpackUnorm4x8,
  PackSnorm2x16,
  UnpackSnorm2x16,
  PackUnorm2x16,
  UnpackUnorm2x16,
  Pack4xU8,
  Unpack4xU8,
  Pack4xI8,
  Unpack4xI8,
  Dot4U8Packed,
  Dot4I8Packed,

  // Host interface (CPU-runtime mode only; never emitted for audio).
  // `arg_positions[0]` indexes `Code::host_ops`. Everything the CPU
  // orchestration layer needs (print, GPU dispatch, sync checks, dynamic
  // arrays, window queries) goes through this single opcode so the dispatch
  // loop stays small and the audio hot path is untouched.
  HostCall,
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
      // HostCall operands are metadata-table indices, not slots; the slots
      // host ops touch are covered by `Code::min_stack_size`.
      Op::InvokeFunction | Op::Return | Op::Jump | Op::HostCall => 0,
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

/// How the CPU-side value of a GPU-bound global is stored in the VM runtime.
#[derive(Debug, Clone, Copy)]
pub enum HostBindingStorage {
  /// Fixed-size value living directly in VM stack slots.
  Slots { position: u16, size: u16 },
  /// Runtime-sized value (unsized array, texture) held host-side as a
  /// `Value`; VM code accesses it through host ops.
  Dynamic,
}

/// Compile-time record of a top-level var the host tracks: every GPU-bound
/// global (for sync bookkeeping) and every runtime-sized global (unsized
/// arrays / textures live host-side even without a GPU binding). The name is
/// kept only for logging and sync-trace parity with the tree-walking
/// interpreter — the runtime addresses entries exclusively by table index.
#[derive(Debug, Clone)]
pub struct HostBinding {
  pub name: Arc<str>,
  pub ty: Type,
  pub storage: HostBindingStorage,
  /// `(group, binding, address_space)` when GPU-bound; `None` for plain
  /// (e.g. private) runtime-sized globals.
  pub gpu: Option<(u8, u8, VariableAddressSpace)>,
}

/// Read/write binding-index sets for one GPU dispatch site, resolved from
/// the dispatched entry point's effects at compile time.
#[derive(Debug, Clone)]
pub struct HostDispatch {
  /// Bindings the dispatched shader(s) read (including length-only reads —
  /// `arrayLength()` derives from buffer size, so uploads must count them).
  pub reads: Vec<u16>,
  /// Bindings the dispatched shader(s) write.
  pub writes: Vec<u16>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WindowQueryKind {
  /// vec2f, 2 slots.
  Resolution,
  Time,
  DeltaTime,
  FrameIndex,
  /// vec2u, 2 slots.
  MouseCoords,
  MousePresent,
  MouseDown,
  MouseJustDown,
}

/// One entry in `Code::host_ops`. All `u16` fields are either VM stack slots
/// (`*_slot`, `dest`) or indices into the `Code` metadata tables (`ty` →
/// `host_types`, string-ish fields → `host_strings`, `binding` →
/// `host_bindings`, `sets` → `host_dispatches`).
#[derive(Debug, Clone)]
pub enum HostOp {
  /// Print the value starting at `slot`, of type `host_types[ty]`, with the
  /// same formatting as the tree-walking interpreter.
  Print { slot: u16, ty: u16 },
  /// Print a lazily-zeroed unsized array (`(print (zeroed-array n): [T])`)
  /// without materializing it; `ty` is the array type.
  PrintZeroed { len_slot: u16, ty: u16 },
  /// Print the host-side value of a dynamic binding.
  PrintBinding { binding: u16 },
  /// If the binding is CPUOutOfDate (GPU wrote it), flush queued compute and
  /// read it back before CPU code reads the value. Early-returns when synced.
  CheckGpuToCpu { binding: u16 },
  /// CPU code just wrote this binding; its GPU copy is now stale.
  MarkCpuWritten { binding: u16 },
  DispatchCompute {
    entry: u16,
    sets: u16,
    /// vec3u, 3 slots.
    workgroup_slot: u16,
  },
  DispatchRender {
    vert: u16,
    frag: u16,
    sets: u16,
    vert_count_slot: u16,
    additive_slot: Option<u16>,
  },
  /// `(= dyn-global (zeroed-array n))`
  AssignDynZeroed { binding: u16, len_slot: u16 },
  /// `(= dyn-global (into-dynamic-array fixed))`: copy `count` elements
  /// starting at `src_slot` into the host-side array.
  AssignDynFromSlots { binding: u16, src_slot: u16, count: u16 },
  /// `(array-length dyn-global)` → u32 at `dest`.
  DynLen { binding: u16, dest: u16 },
  /// `(dyn-global i)` → element value at `dest`.
  DynLoad { binding: u16, index_slot: u16, dest: u16 },
  /// `(= (dyn-global i) v)` ← element value from `src_slot`.
  DynStore { binding: u16, index_slot: u16, src_slot: u16 },
  WindowQuery { kind: WindowQueryKind, dest: u16 },
  KeyQuery { just: bool, key: u16, dest: u16 },
  /// Suspends execution; the driver runs the window frame loop, invoking
  /// function `frame_fn` once per frame, then resumes.
  SpawnWindow { frame_fn: u16 },
  /// Suspends the current (frame) execution; the frame loop stops.
  CloseWindow,
  StartAudio { entry: u16 },
  /// `(= texture-global (load-image "path"))`
  AssignTextureFromImage { binding: u16, path: u16 },
  /// `(= texture-global (blank-texture w h))` — 2 u32 slots at `size_slot`.
  AssignTextureBlank { binding: u16, size_slot: u16 },
  /// `(texture-dimensions tex)` → vec2u (2 slots) at `dest`.
  TextureDims { binding: u16, dest: u16 },
  SetRenderTarget { binding: u16 },
  ClearRenderTarget,
}

/// Why `execute_with_host` returned before running to completion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HostSuspendReason {
  SpawnWindow { frame_fn: u16 },
  CloseWindow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunResult {
  Finished,
  Suspended(HostSuspendReason),
}

/// The CPU-orchestration side of the VM. `execute_with_host` calls
/// `host_call` for each `Op::HostCall` instruction; everything else in the
/// dispatch loop is untouched. The audio path uses `NoopHost` (audio-mode
/// compilation never emits `HostCall`), so after monomorphization it pays
/// nothing for this interface.
pub trait VmHost {
  type Error;
  fn host_call(
    &mut self,
    op: &HostOp,
    stack: &mut [u32],
    code: &Code,
  ) -> Result<Option<HostSuspendReason>, Self::Error>;
}

/// Host for pure-math execution (audio, tests): host calls never occur.
pub struct NoopHost;
impl VmHost for NoopHost {
  type Error = std::convert::Infallible;
  fn host_call(
    &mut self,
    _op: &HostOp,
    _stack: &mut [u32],
    _code: &Code,
  ) -> Result<Option<HostSuspendReason>, Self::Error> {
    unreachable!("host call encountered in hostless VM execution")
  }
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
  /// Name, base stack slot, and size (in u32 stack slots) of each top-level
  /// var. Globals live at the bottom of the stack, below all function
  /// frames, and persist across `execute` calls; external integrations can
  /// use these to read or write globals in a running program (e.g.
  /// mirroring live uniform values into an audio VM).
  pub globals: Vec<(Arc<str>, u16, u16)>,
  /// Total stack slots the compiler's bump allocator consumed. Some slots
  /// are only ever written by host ops (whose operands aren't visible to
  /// `max_touched_index`), so stack sizing takes the max of this and the
  /// instruction-derived bound.
  pub min_stack_size: usize,
  /// Cold metadata for `Op::HostCall` (CPU-runtime mode; all empty for
  /// audio-mode programs).
  pub host_ops: Vec<HostOp>,
  pub host_types: Vec<Type>,
  pub host_strings: Vec<Arc<str>>,
  pub host_bindings: Vec<HostBinding>,
  pub host_dispatches: Vec<HostDispatch>,
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
      .map_or(0, |i| i as usize + 1)
      .max(code.min_stack_size);
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
  /// Base stack slot and size (in u32 slots) of the global named `name`.
  pub fn get_global_slot(&self, name: &str) -> Option<(u16, u16)> {
    self
      .code
      .globals
      .iter()
      .find(|(n, _, _)| &**n == name)
      .map(|&(_, position, size)| (position, size))
  }
  /// Overwrite the global named `name` with raw u32 data (e.g. f32 bits),
  /// writing at most the global's size. Returns the number of stack slots
  /// written, or `None` if no such global exists.
  pub fn write_global(&mut self, name: &str, data: &[u32]) -> Option<usize> {
    let (position, size) = self.get_global_slot(name)?;
    let n = data.len().min(size as usize);
    self.stack[position as usize..position as usize + n]
      .copy_from_slice(&data[..n]);
    Some(n)
  }
  pub fn execute(&mut self) {
    match self.execute_with_host(&mut NoopHost) {
      Ok(_) => {}
      Err(never) => match never {},
    }
  }
  pub fn execute_with_host<H: VmHost>(
    &mut self,
    host: &mut H,
  ) -> Result<RunResult, H::Error> {
    let Self {
      code,
      stack,
      call_stack,
    } = self;
    let Some(mut ip) = call_stack.pop() else {
      return Ok(RunResult::Finished);
    };
    loop {
      let Some(instruction_index) = ip.next() else {
        let Some(return_ip) = call_stack.pop() else {
          return Ok(RunResult::Finished);
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
            return Ok(RunResult::Finished);
          };
          ip = return_ip;
        }
        Op::PackSnorm4x8 => unsafe {
          let a = instruction.arg_positions[0] as usize;
          let mut result: u32 = 0;
          for i in 0..4 {
            let v = f32::from_bits(*stack.get_unchecked(a + i));
            let packed = (0.5 + 127.0 * v.clamp(-1.0, 1.0)).floor() as i8 as u8;
            result |= (packed as u32) << (i * 8);
          }
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            result;
        },
        Op::UnpackSnorm4x8 => unsafe {
          let e = *stack.get_unchecked(instruction.arg_positions[0] as usize);
          let r = instruction.return_position as usize;
          for i in 0..4 {
            let byte = ((e >> (i * 8)) & 0xFF) as u8 as i8;
            *stack.get_unchecked_mut(r + i) =
              (byte as f32 / 127.0).max(-1.0).to_bits();
          }
        },
        Op::PackUnorm4x8 => unsafe {
          let a = instruction.arg_positions[0] as usize;
          let mut result: u32 = 0;
          for i in 0..4 {
            let v = f32::from_bits(*stack.get_unchecked(a + i));
            let packed = (0.5 + 255.0 * v.clamp(0.0, 1.0)).floor() as u8;
            result |= (packed as u32) << (i * 8);
          }
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            result;
        },
        Op::UnpackUnorm4x8 => unsafe {
          let e = *stack.get_unchecked(instruction.arg_positions[0] as usize);
          let r = instruction.return_position as usize;
          for i in 0..4 {
            *stack.get_unchecked_mut(r + i) =
              (((e >> (i * 8)) & 0xFF) as f32 / 255.0).to_bits();
          }
        },
        Op::PackSnorm2x16 => unsafe {
          let a = instruction.arg_positions[0] as usize;
          let mut result: u32 = 0;
          for i in 0..2 {
            let v = f32::from_bits(*stack.get_unchecked(a + i));
            let packed =
              (0.5 + 32767.0 * v.clamp(-1.0, 1.0)).floor() as i16 as u16;
            result |= (packed as u32) << (i * 16);
          }
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            result;
        },
        Op::UnpackSnorm2x16 => unsafe {
          let e = *stack.get_unchecked(instruction.arg_positions[0] as usize);
          let r = instruction.return_position as usize;
          for i in 0..2 {
            let half = ((e >> (i * 16)) & 0xFFFF) as u16 as i16;
            *stack.get_unchecked_mut(r + i) =
              (half as f32 / 32767.0).max(-1.0).to_bits();
          }
        },
        Op::PackUnorm2x16 => unsafe {
          let a = instruction.arg_positions[0] as usize;
          let mut result: u32 = 0;
          for i in 0..2 {
            let v = f32::from_bits(*stack.get_unchecked(a + i));
            let packed = (0.5 + 65535.0 * v.clamp(0.0, 1.0)).floor() as u16;
            result |= (packed as u32) << (i * 16);
          }
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            result;
        },
        Op::UnpackUnorm2x16 => unsafe {
          let e = *stack.get_unchecked(instruction.arg_positions[0] as usize);
          let r = instruction.return_position as usize;
          for i in 0..2 {
            *stack.get_unchecked_mut(r + i) =
              (((e >> (i * 16)) & 0xFFFF) as f32 / 65535.0).to_bits();
          }
        },
        Op::Pack4xU8 => unsafe {
          let a = instruction.arg_positions[0] as usize;
          let mut result: u32 = 0;
          for i in 0..4 {
            result |= (*stack.get_unchecked(a + i) & 0xFF) << (i * 8);
          }
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            result;
        },
        Op::Unpack4xU8 => unsafe {
          let e = *stack.get_unchecked(instruction.arg_positions[0] as usize);
          let r = instruction.return_position as usize;
          for i in 0..4 {
            *stack.get_unchecked_mut(r + i) = (e >> (i * 8)) & 0xFF;
          }
        },
        Op::Pack4xI8 => unsafe {
          let a = instruction.arg_positions[0] as usize;
          let mut result: u32 = 0;
          for i in 0..4 {
            result |= (*stack.get_unchecked(a + i) & 0xFF) << (i * 8);
          }
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            result;
        },
        Op::Unpack4xI8 => unsafe {
          let e = *stack.get_unchecked(instruction.arg_positions[0] as usize);
          let r = instruction.return_position as usize;
          for i in 0..4 {
            *stack.get_unchecked_mut(r + i) =
              (((e >> (i * 8)) & 0xFF) as u8 as i8 as i32) as u32;
          }
        },
        Op::Dot4U8Packed => unsafe {
          let e1 = *stack.get_unchecked(instruction.arg_positions[0] as usize);
          let e2 = *stack.get_unchecked(instruction.arg_positions[1] as usize);
          let mut acc: u32 = 0;
          for i in 0..4 {
            acc = acc
              .wrapping_add(((e1 >> (i * 8)) & 0xFF) * ((e2 >> (i * 8)) & 0xFF));
          }
          *stack.get_unchecked_mut(instruction.return_position as usize) = acc;
        },
        Op::Dot4I8Packed => unsafe {
          let e1 = *stack.get_unchecked(instruction.arg_positions[0] as usize);
          let e2 = *stack.get_unchecked(instruction.arg_positions[1] as usize);
          let mut acc: i32 = 0;
          for i in 0..4u32 {
            let a = (((e1 >> (i * 8)) & 0xFF) as i32) << 24 >> 24;
            let b = (((e2 >> (i * 8)) & 0xFF) as i32) << 24 >> 24;
            acc += a * b;
          }
          *stack.get_unchecked_mut(instruction.return_position as usize) =
            acc as u32;
        },
        Op::HostCall => {
          let op =
            &code.host_ops[instruction.arg_positions[0] as usize];
          if let Some(reason) = host.host_call(op, stack, code)? {
            // Resume point: `ip` has already advanced past this
            // instruction, so pushing it back means resuming continues with
            // the next instruction.
            call_stack.push(ip);
            return Ok(RunResult::Suspended(reason));
          }
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
