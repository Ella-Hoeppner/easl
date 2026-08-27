use std::{
  collections::{HashMap, HashSet},
  path::PathBuf,
  vec,
};
use take_mut::take;
use thiserror::Error;

use std::sync::Arc;

#[cfg(all(feature = "window", feature = "c_audio"))]
use crate::compiler::core::compile_easl_file_to_target;
use crate::compiler::effects::{
  EffectType, WindowInfoBindingSource, WindowInfoKind,
};
use crate::compiler::entry::EntryPoint;
use crate::compiler::{
  builtins::{ASSIGNMENT_OPS, ATOMIC_MUTATION_OPS},
  error::CompileError,
  expression::{Accessor, Exp, ExpKind, Number, SwizzleField},
  functions::{
    AbstractFunctionSignature, FunctionImplementationKind, FunctionSignature,
    Ownership,
  },
  program::{CompilerTarget, LiftedCapture, LiftedCaptures, Program},
  structs::AbstractStruct,
  types::{
    AbstractType, ConcreteArraySize, ConstGenericResolution, ExpTypeInfo, Type,
    TypeState,
  },
  vars::{GroupAndBinding, TopLevelVariableKind, VariableAddressSpace},
};
use crate::external::ExternalVars;
use crate::thread_sync::participant;
use crate::vm::bytecode::{
  DynMemory, HeapCell, alloc_heap_cell, heap_string_words, release_heap_id,
  string_to_words, words_to_string,
};
use crate::vm::compile::vm_stack_size;

#[derive(Clone, PartialEq, Debug)]
pub enum Primitive {
  F32(f32),
  U32(u32),
  I32(i32),
  Bool(bool),
}

impl Primitive {
  fn as_num(self) -> f64 {
    match self {
      Primitive::F32(f) => f as f64,
      Primitive::U32(u) => u as f64,
      Primitive::I32(i) => i as f64,
      Primitive::Bool(b) => {
        if b {
          1.
        } else {
          0.
        }
      }
    }
  }
  fn cast(self, t: &Type) -> Self {
    let value = self.as_num();
    match t {
      Type::F32 => Self::F32(value as f32),
      Type::I32 => Self::I32(value as i32),
      Type::U32 => Self::U32(value as u32),
      Type::Bool => Self::Bool(value != 0.),
      _ => panic!("tried to cast primitive to non-primitive"),
    }
  }
}

#[derive(Clone, PartialEq, Debug, Error)]
pub enum InternalEvalError {
  #[error("Encountered a wildcard expression")]
  EncounteredWildcard,
  #[error("Invalid type for number literal expression")]
  InvalidNumberLiteralType,
  #[error("Floating point value encountered in integer-typed expression")]
  FloatInIntLiteral,
  #[error("Unbound name")]
  UnboundName(Arc<str>),
  #[error("Attempted to apply a non-name expression as a function")]
  AppliedNonName,
  #[error("Wrong arity. Expected {0} arguments, got {1}")]
  WrongArity(usize, usize),
  #[error("Attempted to access a field `{0}` on a non-struct")]
  AccessedFieldOnNonStruct(Arc<str>),
  #[error("Attempted to access non-existent field {0}")]
  NoSuchField(Arc<str>),
  #[error("No arm of match block matched scrutinee value")]
  NoMatchingArm,
  #[error("Attempted to create a zeroed function")]
  CantCreateZeroedFunction,
  #[error("Attempted to create a zeroed skolem")]
  CantCreateZeroedSkolem,
  #[error("Attempted to create a zeroed skolem-sized array")]
  CantCreateZeroedSkolemSizedArray,
  #[error("Attempted to array with invalid size")]
  InvalidArraySize,
  #[error("Loop conditional didn't have a boolean-typed value")]
  NonBooleanLoopCondition,
  #[error("Unrecognized struct name `{0}`")]
  UnrecognizedStructName(Arc<str>),
  #[error("Missing implementation for builtin function `{0}`")]
  UnimplementedBuiltin(Arc<str>),
  #[error("Deriviative function invoked on CPU")]
  DerivativeFunctionCantBeUsed,
  #[error("`discard` invoked on CPU")]
  Discard,
  #[error("Control flow exception `{0}` escaped to top-level")]
  ControlFlowExceptionEscapedToTopLevel(Arc<str>),
}

#[derive(Clone, PartialEq, Debug, Error)]
pub enum UserspaceEvalError {
  #[error("Compilation error: {0}")]
  CompilationError(CompileError),
  #[error("Array index out of bounds: index {0} in array of size {1}")]
  ArrayIndexOutOfBounds(usize, usize),
  #[error("Negative array index: {0}")]
  NegativeArrayIndex(isize),
  #[error("`window` feature not enabled")]
  WindowFeatureNotEnabled,
  #[error(
    "`start-audio` was called but no audio source was compiled for the \
     program; this run was started without audio support"
  )]
  AudioSourceMissing,
  #[error("Audio runtime error: {0}")]
  AudioRuntimeError(String),
  #[error("No `@cpu` entry point found")]
  NoCpuEntryPoint,
  #[error("Multiple `@cpu` entry points found, must specify a particle one")]
  MultipleCpuEntryPoints,
  #[error("Couldn't find `@cpu` entry point named {0}")]
  CpuEntryPointNotFound(Arc<str>),
  #[error("{0}")]
  RuntimeError(String),
}

#[derive(Clone, PartialEq, Debug, Error)]
pub enum EvalError {
  #[error(
    "Internal Interpreter Error: {0}\n\n\
     Please report this issue on github!"
  )]
  Internal(InternalEvalError),
  #[error("{0}")]
  Userspace(UserspaceEvalError),
}

impl From<CompileError> for EvalError {
  fn from(e: CompileError) -> Self {
    Self::Userspace(UserspaceEvalError::CompilationError(e))
  }
}

impl From<UserspaceEvalError> for EvalError {
  fn from(e: UserspaceEvalError) -> Self {
    Self::Userspace(e)
  }
}

impl From<InternalEvalError> for EvalError {
  fn from(e: InternalEvalError) -> Self {
    Self::Internal(e)
  }
}

impl From<UserspaceEvalError> for EvalException {
  fn from(e: UserspaceEvalError) -> Self {
    EvalError::Userspace(e).into()
  }
}

impl From<InternalEvalError> for EvalException {
  fn from(e: InternalEvalError) -> Self {
    EvalError::Internal(e).into()
  }
}

use InternalEvalError::*;
use UserspaceEvalError::*;

#[derive(Clone, PartialEq, Debug)]
pub enum Function {
  StructConstructor(Vec<Arc<str>>),
  EnumConstructor(Arc<str>),
  Builtin(Arc<str>),
  Composite {
    arg_names: Vec<Arc<str>>,
    expression: Exp<ExpTypeInfo>,
  },
  /// A closure with a captured scope struct. `inner` is the extracted composite
  /// function (which takes the scope struct as its first argument), and `scope`
  /// is the already-evaluated scope struct value.
  Scoped {
    inner: Box<Function>,
    scope: Box<Value>,
  },
}

impl Function {
  fn from_abstract_signature(
    f: &AbstractFunctionSignature,
    name: &Arc<str>,
    env: &EvaluationEnvironment<impl IOManager>,
  ) -> Result<Self, EvalError> {
    match &f.implementation {
      FunctionImplementationKind::Builtin { .. } => {
        Ok(Function::Builtin(name.clone()))
      }
      FunctionImplementationKind::StructConstructor => {
        Ok(Function::StructConstructor({
          let s = match env.structs.get(&f.name) {
            Some(s) => s,
            None => return Err(UnrecognizedStructName(f.name.clone()).into()),
          };
          s.fields.iter().map(|field| field.name.clone()).collect()
        }))
      }
      FunctionImplementationKind::EnumConstructor(variant_name) => {
        Ok(Function::EnumConstructor(variant_name.clone()))
      }
      FunctionImplementationKind::Composite(f) => {
        let f = f.read().unwrap();
        Ok(Function::Composite {
          arg_names: f
            .arg_names
            .iter()
            .map(|(arg_name, _)| arg_name.clone())
            .collect(),
          expression: f.expression.clone(),
        })
      }
    }
  }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
  Unit,
  Prim(Primitive),
  Struct(HashMap<Arc<str>, Value>),
  Enum(Arc<str>, Box<Value>),
  Fun(Function),
  Array(Vec<Value>),
  /// A lazily-materialized array of a fixed number of zero elements.
  /// Created by `zeroed-array` to avoid allocating huge zero-filled Vecs.
  /// When uploaded to the GPU the window system clears the buffer efficiently
  /// instead of copying bytes from the CPU.
  ZeroedArray {
    length: usize,
  },
  Uninitialized,
  String(String),
  /// A CPU-loaded texture, created by `load-image` or `blank-texture`. Holds
  /// RGBA8 pixel data. Uploaded to the GPU as a `wgpu::Texture` (not a
  /// buffer) when needed. `binding` is set when the texture is assigned to a
  /// GPU binding var, so `set-render-target` can identify the GPU slot.
  Texture {
    width: u32,
    height: u32,
    data: Vec<u8>,
    binding: Option<GroupAndBinding>,
  },
}

impl Value {
  fn map_primitive_or_vec_components(
    &self,
    f: impl Fn(Primitive) -> Primitive,
  ) -> Self {
    match self {
      Value::Prim(p) => Value::Prim(f(p.clone())),
      Value::Struct(fields) => Value::Struct(
        fields
          .iter()
          .map(|(name, value)| {
            let Value::Prim(p) = value else { panic!() };
            (name.clone(), Value::Prim(f(p.clone())))
          })
          .collect(),
      ),
      _ => panic!(),
    }
  }
  fn multi_map_primitive_or_vec_components(
    values: &Vec<Value>,
    f: impl Fn(Vec<Primitive>) -> Primitive,
  ) -> Self {
    let vector_size = values
      .iter()
      .filter_map(|v| {
        if let Value::Struct(fields) = v {
          Some(fields.len())
        } else {
          None
        }
      })
      .max();
    if let Some(vector_size) = vector_size {
      Value::Struct(
        ["x", "y", "z", "w"]
          .into_iter()
          .take(vector_size)
          .map(|field_name| {
            (
              field_name.into(),
              Value::Prim(f(values
                .iter()
                .map(|value| match value {
                  Value::Prim(p) => p.clone(),
                  Value::Struct(vector) => {
                    vector.get(field_name).cloned().unwrap().unwrap_primitive()
                  }
                  _ => panic!(),
                })
                .collect())),
            )
          })
          .collect(),
      )
    } else {
      Value::Prim(f(values
        .iter()
        .map(|v| v.clone().unwrap_primitive())
        .collect()))
    }
  }
}

fn primitive_arithmetic(
  a: Primitive,
  b: Primitive,
  f32_op: impl Fn(f32, f32) -> f32,
  i32_op: impl Fn(i32, i32) -> i32,
  u32_op: impl Fn(u32, u32) -> u32,
) -> Value {
  match (a, b) {
    (Primitive::F32(a), Primitive::F32(b)) => {
      Value::Prim(Primitive::F32(f32_op(a, b)))
    }
    (Primitive::I32(a), Primitive::I32(b)) => {
      Value::Prim(Primitive::I32(i32_op(a, b)))
    }
    (Primitive::U32(a), Primitive::U32(b)) => {
      Value::Prim(Primitive::U32(u32_op(a, b)))
    }
    _ => panic!(),
  }
}

fn apply_builtin_fn<IO: IOManager>(
  f_name: Arc<str>,
  mut args: Vec<(Value, Type)>,
  return_type: Type,
  env: &mut EvaluationEnvironment<IO>,
) -> Result<Value, EvalException> {
  let return_type_clone = return_type.clone();
  let construct_vec = |vec_length: usize| {
    let Type::Struct(return_struct) = &return_type else {
      panic!()
    };
    let inner_type = return_struct.fields[0].field_type.unwrap_known();
    let mut inner_values: Vec<&Value> = args
      .iter()
      .flat_map(|(value, _)| match value {
        Value::Struct(fields) => ["x", "y", "z", "w"]
          .into_iter()
          .filter_map(|field| fields.get(field))
          .collect(),
        value => vec![value],
      })
      .collect();
    if inner_values.len() == 1 {
      inner_values = std::iter::repeat(inner_values[0])
        .take(vec_length)
        .collect();
    }
    Ok(Value::Struct(
      ["x", "y", "z", "w"]
        .into_iter()
        .zip(inner_values)
        .map(|(name, value)| {
          (name.into(), {
            let Value::Prim(primitive) = value else {
              panic!()
            };
            primitive.clone().cast(&inner_type).into()
          })
        })
        .collect(),
    ))
  };
  match &*f_name {
    "=" => Ok(args.remove(1).0),
    "<" | ">" | "<=" | ">=" => {
      let a = args.remove(0).0.unwrap_primitive().as_num();
      let b = args.remove(0).0.unwrap_primitive().as_num();
      Ok(Value::Prim(Primitive::Bool(match &*f_name {
        "<" => a < b,
        ">" => a > b,
        "<=" => a <= b,
        ">=" => a >= b,
        _ => unreachable!(),
      })))
    }
    "==" => {
      if let (Value::String(a), Value::String(b)) = (&args[0].0, &args[1].0) {
        return Ok(Value::Prim(Primitive::Bool(a == b)));
      }
      let values = vec![args.remove(0).0, args.remove(0).0];
      Ok(Value::multi_map_primitive_or_vec_components(
        &values,
        |prims| Primitive::Bool(prims[0] == prims[1]),
      ))
    }
    "!=" => {
      if let (Value::String(a), Value::String(b)) = (&args[0].0, &args[1].0) {
        return Ok(Value::Prim(Primitive::Bool(a != b)));
      }
      let values = vec![args.remove(0).0, args.remove(0).0];
      Ok(Value::multi_map_primitive_or_vec_components(
        &values,
        |prims| Primitive::Bool(prims[0] != prims[1]),
      ))
    }
    "not" | "!" => match args.remove(0).0.unwrap_primitive() {
      Primitive::Bool(b) => Ok(Value::Prim(Primitive::Bool(!b))),
      _ => panic!(),
    },
    "&&" | "||" | "and" | "or" => {
      match (
        args.remove(0).0.unwrap_primitive(),
        args.remove(0).0.unwrap_primitive(),
      ) {
        (Primitive::Bool(a), Primitive::Bool(b)) => {
          Ok(Value::Prim(Primitive::Bool(match &*f_name {
            "&&" | "and" => a && b,
            "||" | "or" => a || b,
            _ => unreachable!(),
          })))
        }
        _ => panic!(),
      }
    }
    "atan2" => {
      let Value::Prim(Primitive::F32(y)) = args[0].0 else {
        panic!()
      };
      let Value::Prim(Primitive::F32(x)) = args[1].0 else {
        panic!()
      };
      Ok(Value::Prim(Primitive::F32(y.atan2(x))))
    }
    "sin" | "cos" | "tan" | "sinh" | "cosh" | "tanh" | "asin" | "acos"
    | "atan" | "asinh" | "acosh" | "atanh" => Ok(
      Value::map_primitive_or_vec_components(&args[0].0, |value| {
        let Primitive::F32(x) = value else { panic!() };
        Primitive::F32(match &*f_name {
          "sin" => x.sin(),
          "cos" => x.cos(),
          "tan" => x.tan(),
          "sinh" => x.sinh(),
          "cosh" => x.cosh(),
          "tanh" => x.tanh(),
          "asin" => x.asin(),
          "acos" => x.acos(),
          "atan" => x.atan(),
          "asinh" => x.asinh(),
          "acosh" => x.acosh(),
          "atanh" => x.atanh(),
          _ => unreachable!(),
        })
      }),
    ),
    "-" if args.len() == 1 => Ok(
      args
        .remove(0)
        .0
        .map_primitive_or_vec_components(|p| match p {
          Primitive::F32(f) => Primitive::F32(-f),
          Primitive::I32(i) => Primitive::I32(-i),
          _ => panic!(),
        }),
    ),
    "/" if args.len() == 1 => Ok(
      args
        .remove(0)
        .0
        .map_primitive_or_vec_components(|p| match p {
          Primitive::F32(f) => Primitive::F32(1. / f),
          _ => panic!(),
        }),
    ),
    "+" | "+=" | "-" | "-=" | "*" | "*=" | "/" | "/=" | "%" | "%=" | "min"
    | "max" => {
      let operator = |a: &Primitive, b: &Primitive| {
        primitive_arithmetic(
          a.clone(),
          b.clone(),
          match &*f_name {
            "+" | "+=" => |a, b| a + b,
            "-" | "-=" => |a, b| a - b,
            "*" | "*=" => |a, b| a * b,
            "/" | "/=" => |a, b| a / b,
            "%" | "%=" => |a, b| a % b,
            "min" => |a: f32, b: f32| a.min(b),
            "max" => |a: f32, b: f32| a.max(b),
            _ => unreachable!(),
          },
          match &*f_name {
            "+" | "+=" => |a, b| a + b,
            "-" | "-=" => |a, b| a - b,
            "*" | "*=" => |a, b| a * b,
            "/" | "/=" => |a, b| a / b,
            "%" | "%=" => |a, b| a % b,
            "min" => |a: i32, b: i32| a.min(b),
            "max" => |a: i32, b: i32| a.max(b),
            _ => unreachable!(),
          },
          match &*f_name {
            "+" | "+=" => |a, b| a + b,
            "-" | "-=" => |a, b| a - b,
            "*" | "*=" => |a, b| a * b,
            "/" | "/=" => |a, b| a / b,
            "%" | "%=" => |a, b| a % b,
            "min" => |a: u32, b: u32| a.min(b),
            "max" => |a: u32, b: u32| a.max(b),
            _ => unreachable!(),
          },
        )
      };
      match (&args[0].0, &args[1].0) {
        (Value::Prim(a), Value::Prim(b)) => Ok(operator(a, b)),
        (Value::Prim(scalar), Value::Struct(vector)) => Ok(Value::Struct(
          vector
            .iter()
            .map(|(name, value)| {
              (name.clone(), {
                operator(scalar, &value.clone().unwrap_primitive())
              })
            })
            .collect(),
        )),
        (Value::Struct(vector), Value::Prim(scalar)) => Ok(Value::Struct(
          vector
            .iter()
            .map(|(name, value)| {
              (name.clone(), {
                operator(&value.clone().unwrap_primitive(), scalar)
              })
            })
            .collect(),
        )),
        (Value::Struct(a), Value::Struct(b)) => Ok(Value::Struct(
          a.iter()
            .map(|(name, value)| {
              (
                name.clone(),
                operator(
                  &value.clone().unwrap_primitive(),
                  &b.get(name).unwrap().clone().unwrap_primitive(),
                ),
              )
            })
            .collect(),
        )),
        // Matrix + matrix, matrix - matrix (component-wise on columns)
        (Value::Array(a), Value::Array(b))
          if matches!(&*f_name, "+" | "+=" | "-" | "-=") =>
        {
          Ok(Value::Array(
            a.iter()
              .zip(b.iter())
              .map(|(col_a, col_b)| {
                let (Value::Struct(va), Value::Struct(vb)) = (col_a, col_b)
                else {
                  panic!()
                };
                Value::Struct(
                  va.iter()
                    .map(|(name, value)| {
                      (
                        name.clone(),
                        operator(
                          &value.clone().unwrap_primitive(),
                          &vb.get(name).unwrap().clone().unwrap_primitive(),
                        ),
                      )
                    })
                    .collect(),
                )
              })
              .collect(),
          ))
        }
        // Scalar * matrix, matrix * scalar
        (Value::Prim(scalar), Value::Array(cols))
        | (Value::Array(cols), Value::Prim(scalar)) => Ok(Value::Array(
          cols
            .iter()
            .map(|col| {
              let Value::Struct(v) = col else { panic!() };
              Value::Struct(
                v.iter()
                  .map(|(name, value)| {
                    (
                      name.clone(),
                      operator(scalar, &value.clone().unwrap_primitive()),
                    )
                  })
                  .collect(),
              )
            })
            .collect(),
        )),
        // Matrix * vector: result[i] = sum_j(mat[j][i] * v[j])
        (Value::Array(cols), Value::Struct(v)) => {
          let row_fields: Vec<&str> = ["x", "y", "z", "w"]
            .into_iter()
            .filter(|f| cols[0].as_struct().contains_key(*f))
            .collect();
          let v_fields: Vec<&str> = ["x", "y", "z", "w"]
            .into_iter()
            .filter(|f| v.contains_key(*f))
            .collect();
          Ok(Value::Struct(
            row_fields
              .iter()
              .map(|row_field| {
                let mut acc: Option<Value> = None;
                for (col, &v_field) in cols.iter().zip(v_fields.iter()) {
                  let product = operator(
                    &col
                      .as_struct()
                      .get(*row_field)
                      .unwrap()
                      .clone()
                      .unwrap_primitive(),
                    &v.get(v_field).unwrap().clone().unwrap_primitive(),
                  );
                  acc = Some(match acc {
                    None => product,
                    Some(a) => primitive_arithmetic(
                      a.unwrap_primitive(),
                      product.unwrap_primitive(),
                      |a, b| a + b,
                      |a, b| a + b,
                      |a, b| a + b,
                    ),
                  });
                }
                ((*row_field).into(), acc.unwrap())
              })
              .collect(),
          ))
        }
        // Vector * matrix: result[j] = dot(v, col_j)
        (Value::Struct(v), Value::Array(cols)) => {
          let v_fields: Vec<&str> = ["x", "y", "z", "w"]
            .into_iter()
            .filter(|f| v.contains_key(*f))
            .collect();
          Ok(Value::Struct(
            ["x", "y", "z", "w"]
              .into_iter()
              .zip(cols.iter())
              .map(|(out_field, col)| {
                let col = col.as_struct();
                let mut acc: Option<Value> = None;
                for &v_field in &v_fields {
                  let product = operator(
                    &v.get(v_field).unwrap().clone().unwrap_primitive(),
                    &col.get(v_field).unwrap().clone().unwrap_primitive(),
                  );
                  acc = Some(match acc {
                    None => product,
                    Some(a) => primitive_arithmetic(
                      a.unwrap_primitive(),
                      product.unwrap_primitive(),
                      |a, b| a + b,
                      |a, b| a + b,
                      |a, b| a + b,
                    ),
                  });
                }
                (out_field.into(), acc.unwrap())
              })
              .collect(),
          ))
        }
        // Matrix * matrix: result_col_j = mat_a * col_j(mat_b)
        (Value::Array(a_cols), Value::Array(b_cols)) => {
          let a_row_fields: Vec<&str> = ["x", "y", "z", "w"]
            .into_iter()
            .filter(|f| a_cols[0].as_struct().contains_key(*f))
            .collect();
          Ok(Value::Array(
            b_cols
              .iter()
              .map(|b_col| {
                let b_col = b_col.as_struct();
                let b_fields: Vec<&str> = ["x", "y", "z", "w"]
                  .into_iter()
                  .filter(|f| b_col.contains_key(*f))
                  .collect();
                Value::Struct(
                  a_row_fields
                    .iter()
                    .map(|row_field| {
                      let mut acc: Option<Value> = None;
                      for (a_col, &b_field) in
                        a_cols.iter().zip(b_fields.iter())
                      {
                        let product = operator(
                          &a_col
                            .as_struct()
                            .get(*row_field)
                            .unwrap()
                            .clone()
                            .unwrap_primitive(),
                          &b_col
                            .get(b_field)
                            .unwrap()
                            .clone()
                            .unwrap_primitive(),
                        );
                        acc = Some(match acc {
                          None => product,
                          Some(a) => primitive_arithmetic(
                            a.unwrap_primitive(),
                            product.unwrap_primitive(),
                            |a, b| a + b,
                            |a, b| a + b,
                            |a, b| a + b,
                          ),
                        });
                      }
                      ((*row_field).into(), acc.unwrap())
                    })
                    .collect(),
                )
              })
              .collect(),
          ))
        }
        _ => panic!(),
      }
    }
    "vec2" | "vec2f" | "vec2i" | "vec2u" | "vec2b" => construct_vec(2),
    "vec3" | "vec3f" | "vec3i" | "vec3u" | "vec3b" => construct_vec(3),
    "vec4" | "vec4f" | "vec4i" | "vec4u" | "vec4b" => construct_vec(4),
    name if name.starts_with("mat") => {
      // Parse matNxM dimensions from the name
      let dims: Vec<usize> = name
        .trim_start_matches("mat")
        .trim_end_matches(|c: char| c.is_alphabetic())
        .split('x')
        .map(|s| s.parse().unwrap())
        .collect();
      let (num_cols, num_rows) = (dims[0], dims[1]);
      let values: Vec<Value> = args.into_iter().map(|(v, _)| v).collect();
      if values.len() == num_cols * num_rows {
        // Scalar constructor: group scalars into column vectors
        let columns: Vec<Value> = values
          .chunks(num_rows)
          .map(|col_scalars| {
            Value::Struct(
              ["x", "y", "z", "w"]
                .iter()
                .zip(col_scalars)
                .map(|(name, value)| ((*name).into(), value.clone()))
                .collect(),
            )
          })
          .collect();
        Ok(Value::Array(columns))
      } else if values.len() == num_cols {
        // Column vector constructor
        Ok(Value::Array(values))
      } else {
        panic!("invalid matrix constructor argument count")
      }
    }
    "length" => {
      if let Value::String(s) = &args[0].0 {
        return Ok(Value::Prim(Primitive::U32(s.chars().count() as u32)));
      }
      let Value::Struct(s) = &args[0].0 else {
        panic!()
      };
      let mut sum = 0.;
      for f in ["x", "y", "z", "w"] {
        if let Some(value) = s.get(f) {
          let value = value.clone().unwrap_primitive().as_num();
          sum += value * value;
        }
      }
      Ok(Value::Prim(Primitive::F32(sum.sqrt() as f32)))
    }
    "distance" => {
      let Value::Struct(a) = &args[0].0 else {
        panic!()
      };
      let Value::Struct(b) = &args[1].0 else {
        panic!()
      };
      let mut sum = 0.;
      for f in ["x", "y", "z", "w"] {
        if let Some(value_a) = a.get(f) {
          let value_a = value_a.clone().unwrap_primitive().as_num();
          let value_b = b.get(f).unwrap().clone().unwrap_primitive().as_num();
          let diff = value_a - value_b;
          sum += diff * diff;
        }
      }
      Ok(Value::Prim(Primitive::F32(sum.sqrt() as f32)))
    }
    "normalize" => {
      let Value::Struct(s) = &args[0].0 else {
        panic!()
      };
      let mut sum = 0.;
      for f in ["x", "y", "z", "w"] {
        if let Some(value) = s.get(f) {
          let value = value.clone().unwrap_primitive().as_num();
          sum += value * value;
        }
      }
      let length = sum.sqrt();
      Ok(Value::Struct(
        s.iter()
          .map(|(f, value)| {
            (
              f.clone(),
              Value::Prim(Primitive::F32(
                (value.clone().unwrap_primitive().as_num() / length) as f32,
              )),
            )
          })
          .collect(),
      ))
    }
    "dot" => {
      let Value::Struct(a) = &args[0].0 else {
        panic!()
      };
      let Value::Struct(b) = &args[1].0 else {
        panic!()
      };
      let mut sum = 0.;
      for f in ["x", "y", "z", "w"] {
        if let Some(value_a) = a.get(f) {
          let value_a = value_a.clone().unwrap_primitive().as_num();
          let value_b = b.get(f).unwrap().clone().unwrap_primitive().as_num();
          sum += value_a * value_b;
        }
      }
      Ok(Value::Prim(Primitive::F32(sum as f32)))
    }
    "cross" => {
      let Value::Struct(a) = &args[0].0 else {
        panic!()
      };
      let Value::Struct(b) = &args[1].0 else {
        panic!()
      };
      let get = |s: &HashMap<Arc<str>, Value>, f: &str| -> f64 {
        s.get(f).unwrap().clone().unwrap_primitive().as_num()
      };
      let ax = get(a, "x");
      let ay = get(a, "y");
      let az = get(a, "z");
      let bx = get(b, "x");
      let by = get(b, "y");
      let bz = get(b, "z");
      Ok(Value::Struct(
        [
          (
            Arc::from("x"),
            Value::Prim(Primitive::F32((ay * bz - az * by) as f32)),
          ),
          (
            Arc::from("y"),
            Value::Prim(Primitive::F32((az * bx - ax * bz) as f32)),
          ),
          (
            Arc::from("z"),
            Value::Prim(Primitive::F32((ax * by - ay * bx) as f32)),
          ),
        ]
        .into_iter()
        .collect(),
      ))
    }
    "reflect" => {
      let Value::Struct(e1) = &args[0].0 else {
        panic!()
      };
      let Value::Struct(e2) = &args[1].0 else {
        panic!()
      };
      // reflect(e1, e2) = e1 - 2 * dot(e2, e1) * e2
      let mut dot_val = 0.0_f64;
      for f in ["x", "y", "z", "w"] {
        if let Some(v1) = e1.get(f) {
          let v1 = v1.clone().unwrap_primitive().as_num();
          let v2 = e2.get(f).unwrap().clone().unwrap_primitive().as_num();
          dot_val += v2 * v1;
        }
      }
      Ok(Value::Struct(
        e1.iter()
          .map(|(f, value)| {
            let v1 = value.clone().unwrap_primitive().as_num();
            let v2 = e2.get(f).unwrap().clone().unwrap_primitive().as_num();
            (
              f.clone(),
              Value::Prim(Primitive::F32((v1 - 2.0 * dot_val * v2) as f32)),
            )
          })
          .collect(),
      ))
    }
    "refract" => {
      let Value::Struct(e1) = &args[0].0 else {
        panic!()
      };
      let Value::Struct(e2) = &args[1].0 else {
        panic!()
      };
      let Value::Prim(Primitive::F32(eta)) = &args[2].0 else {
        panic!()
      };
      let eta = *eta as f64;
      // dot(e2, e1)
      let mut dot_val = 0.0_f64;
      for f in ["x", "y", "z", "w"] {
        if let Some(v2) = e2.get(f) {
          let v2 = v2.clone().unwrap_primitive().as_num();
          let v1 = e1.get(f).unwrap().clone().unwrap_primitive().as_num();
          dot_val += v2 * v1;
        }
      }
      let k = 1.0 - eta * eta * (1.0 - dot_val * dot_val);
      if k < 0.0 {
        // Total internal reflection: return zero vector
        Ok(Value::Struct(
          e1.iter()
            .map(|(f, _)| (f.clone(), Value::Prim(Primitive::F32(0.0))))
            .collect(),
        ))
      } else {
        let coeff = eta * dot_val + k.sqrt();
        Ok(Value::Struct(
          e1.iter()
            .map(|(f, value)| {
              let v1 = value.clone().unwrap_primitive().as_num();
              let v2 = e2.get(f).unwrap().clone().unwrap_primitive().as_num();
              (
                f.clone(),
                Value::Prim(Primitive::F32((eta * v1 - coeff * v2) as f32)),
              )
            })
            .collect(),
        ))
      }
    }
    "face-forward" => {
      let Value::Struct(e1) = &args[0].0 else {
        panic!()
      };
      let Value::Struct(e2) = &args[1].0 else {
        panic!()
      };
      let Value::Struct(e3) = &args[2].0 else {
        panic!()
      };
      let mut dot_val = 0.0_f64;
      for f in ["x", "y", "z", "w"] {
        if let Some(v2) = e2.get(f) {
          let v2 = v2.clone().unwrap_primitive().as_num();
          let v3 = e3.get(f).unwrap().clone().unwrap_primitive().as_num();
          dot_val += v2 * v3;
        }
      }
      if dot_val < 0.0 {
        Ok(Value::Struct(e1.clone()))
      } else {
        Ok(Value::Struct(
          e1.iter()
            .map(|(f, v)| {
              let Primitive::F32(x) = v.clone().unwrap_primitive() else {
                panic!()
              };
              (f.clone(), Value::Prim(Primitive::F32(-x)))
            })
            .collect(),
        ))
      }
    }
    "determinant" => {
      let Value::Array(cols) = &args[0].0 else {
        panic!()
      };
      let n = cols.len();
      let get = |col: usize, row: &str| -> f64 {
        cols[col]
          .as_struct()
          .get(row)
          .unwrap()
          .clone()
          .unwrap_primitive()
          .as_num()
      };
      let det = if n == 2 {
        get(0, "x") * get(1, "y") - get(0, "y") * get(1, "x")
      } else if n == 3 {
        let (a00, a10, a20) = (get(0, "x"), get(0, "y"), get(0, "z"));
        let (a01, a11, a21) = (get(1, "x"), get(1, "y"), get(1, "z"));
        let (a02, a12, a22) = (get(2, "x"), get(2, "y"), get(2, "z"));
        a00 * (a11 * a22 - a12 * a21) - a01 * (a10 * a22 - a12 * a20)
          + a02 * (a10 * a21 - a11 * a20)
      } else if n == 4 {
        let g = |c: usize, r: &str| get(c, r);
        let s0 = g(0, "x") * g(1, "y") - g(1, "x") * g(0, "y");
        let s1 = g(0, "x") * g(2, "y") - g(2, "x") * g(0, "y");
        let s2 = g(0, "x") * g(3, "y") - g(3, "x") * g(0, "y");
        let s3 = g(1, "x") * g(2, "y") - g(2, "x") * g(1, "y");
        let s4 = g(1, "x") * g(3, "y") - g(3, "x") * g(1, "y");
        let s5 = g(2, "x") * g(3, "y") - g(3, "x") * g(2, "y");
        let c5 = g(2, "z") * g(3, "w") - g(3, "z") * g(2, "w");
        let c4 = g(1, "z") * g(3, "w") - g(3, "z") * g(1, "w");
        let c3 = g(1, "z") * g(2, "w") - g(2, "z") * g(1, "w");
        let c2 = g(0, "z") * g(3, "w") - g(3, "z") * g(0, "w");
        let c1 = g(0, "z") * g(2, "w") - g(2, "z") * g(0, "w");
        let c0 = g(0, "z") * g(1, "w") - g(1, "z") * g(0, "w");
        s0 * c5 - s1 * c4 + s2 * c3 + s3 * c2 - s4 * c1 + s5 * c0
      } else {
        panic!("determinant: unsupported matrix size {n}")
      };
      Ok(Value::Prim(Primitive::F32(det as f32)))
    }
    "transpose" => {
      let Value::Array(cols) = &args[0].0 else {
        panic!()
      };
      let num_cols = cols.len();
      let row_fields: Vec<&str> = ["x", "y", "z", "w"]
        .into_iter()
        .filter(|f| cols[0].as_struct().contains_key(*f))
        .collect();
      let col_fields: Vec<&str> = ["x", "y", "z", "w"][..num_cols].to_vec();
      let new_cols: Vec<Value> = row_fields
        .iter()
        .map(|row_field| {
          Value::Struct(
            col_fields
              .iter()
              .enumerate()
              .map(|(col_idx, col_field)| {
                let v =
                  cols[col_idx].as_struct().get(*row_field).unwrap().clone();
                ((*col_field).into(), v)
              })
              .collect(),
          )
        })
        .collect();
      Ok(Value::Array(new_cols))
    }
    "f32" => {
      let Value::Prim(x) = &args[0].0 else { panic!() };
      Ok(Value::Prim(Primitive::F32(x.clone().as_num() as f32)))
    }
    "i32" => {
      let Value::Prim(x) = &args[0].0 else { panic!() };
      let x = x.clone().as_num();
      Ok(Value::Prim(Primitive::I32(if x <= i32::MIN as f64 {
        i32::MIN
      } else if x >= i32::MAX as f64 {
        i32::MAX
      } else {
        x as i32
      })))
    }
    "u32" => {
      let Value::Prim(x) = &args[0].0 else { panic!() };
      let x = x.clone().as_num();
      Ok(Value::Prim(Primitive::U32(if x < 0. {
        0u32
      } else if x >= u32::MAX as f64 {
        u32::MAX
      } else {
        x as u32
      })))
    }
    "bool" => {
      let Value::Prim(x) = &args[0].0 else { panic!() };
      Ok(Value::Prim(Primitive::Bool(x.clone().as_num() != 0.)))
    }
    "abs" => Ok(args[0].0.map_primitive_or_vec_components(|p| match p {
      Primitive::F32(f) => Primitive::F32(f.abs()),
      Primitive::I32(i) => Primitive::I32(i.abs()),
      other => other,
    })),
    "sign" => Ok(args[0].0.map_primitive_or_vec_components(|p| match p {
      Primitive::F32(f) => Primitive::F32(if f == 0. {
        0.
      } else if f > 0. {
        1.
      } else {
        -1.
      }),
      Primitive::I32(i) => Primitive::I32(if i == 0 {
        0
      } else if i > 0 {
        1
      } else {
        -1
      }),
      other => other,
    })),
    "floor" | "ceil" | "round" | "fract" | "sqrt" | "trunc" | "saturate"
    | "degrees" | "radians" | "inverse-sqrt" => {
      Ok(args[0].0.map_primitive_or_vec_components(|p| {
        let Primitive::F32(x) = p else { panic!() };
        Primitive::F32(match &*f_name {
          "floor" => x.floor(),
          "ceil" => x.ceil(),
          "round" => x.round(),
          "fract" => {
            if x > 0. {
              x.fract()
            } else {
              1. + x.fract()
            }
          }
          "sqrt" => x.sqrt(),
          "trunc" => x.trunc(),
          "saturate" => x.clamp(0., 1.),
          "degrees" => x.to_degrees(),
          "radians" => x.to_radians(),
          "inverse-sqrt" => 1.0 / x.sqrt(),
          _ => panic!(),
        })
      }))
    }
    "&" | "&=" | "|" | "|=" | "^" | "^=" | "<<" | "<<=" | ">>" | ">>=" => {
      let operator = |a: &Primitive, b: &Primitive| match (a, b) {
        (Primitive::I32(a), Primitive::I32(b)) => {
          Value::Prim(Primitive::I32(match &*f_name {
            "&" | "&=" => a & b,
            "|" | "|=" => a | b,
            "^" | "^=" => a ^ b,
            "<<" | "<<=" => a << (b & 31),
            ">>" | ">>=" => a >> (b & 31),
            _ => unreachable!(),
          }))
        }
        (Primitive::U32(a), Primitive::U32(b)) => {
          Value::Prim(Primitive::U32(match &*f_name {
            "&" | "&=" => a & b,
            "|" | "|=" => a | b,
            "^" | "^=" => a ^ b,
            "<<" | "<<=" => a << (b & 31),
            ">>" | ">>=" => a >> (b & 31),
            _ => unreachable!(),
          }))
        }
        _ => panic!("bitwise ops require integer types"),
      };
      match (&args[0].0, &args[1].0) {
        (Value::Prim(a), Value::Prim(b)) => Ok(operator(a, b)),
        (Value::Prim(scalar), Value::Struct(vector)) => Ok(Value::Struct(
          vector
            .iter()
            .map(|(name, value)| {
              (
                name.clone(),
                operator(scalar, &value.clone().unwrap_primitive()),
              )
            })
            .collect(),
        )),
        (Value::Struct(vector), Value::Prim(scalar)) => Ok(Value::Struct(
          vector
            .iter()
            .map(|(name, value)| {
              (
                name.clone(),
                operator(&value.clone().unwrap_primitive(), scalar),
              )
            })
            .collect(),
        )),
        (Value::Struct(a), Value::Struct(b)) => Ok(Value::Struct(
          a.iter()
            .map(|(name, value)| {
              (
                name.clone(),
                operator(
                  &value.clone().unwrap_primitive(),
                  &b.get(name).unwrap().clone().unwrap_primitive(),
                ),
              )
            })
            .collect(),
        )),
        _ => panic!(),
      }
    }
    "pow" => Ok(Value::multi_map_primitive_or_vec_components(
      &args.into_iter().map(|(v, _)| v).collect(),
      |mut values| {
        let pow = values.remove(1);
        let base = values.remove(0);
        match (base, pow) {
          (Primitive::F32(base), Primitive::F32(pow)) => {
            Primitive::F32(base.powf(pow))
          }
          (Primitive::U32(base), Primitive::U32(pow)) => {
            Primitive::U32(base.pow(pow))
          }
          (Primitive::I32(base), Primitive::I32(pow)) => {
            Primitive::I32(base.pow(pow.abs() as u32))
          }
          _ => panic!(),
        }
      },
    )),
    "mix" => Ok(Value::multi_map_primitive_or_vec_components(
      &args.into_iter().map(|(v, _)| v).collect(),
      |mut values| {
        let p = values.remove(2);
        let b = values.remove(1);
        let a = values.remove(0);
        match (a, b, p) {
          (Primitive::F32(a), Primitive::F32(b), Primitive::F32(p)) => {
            Primitive::F32(a * (1. - p) + b * p)
          }
          _ => panic!(),
        }
      },
    )),
    "clamp" => Ok(Value::multi_map_primitive_or_vec_components(
      &args.into_iter().map(|(v, _)| v).collect(),
      |mut values| {
        let max = values.remove(2);
        let min = values.remove(1);
        let x = values.remove(0);
        match (x, min, max) {
          (Primitive::F32(x), Primitive::F32(min), Primitive::F32(max)) => {
            Primitive::F32(x.min(max).max(min))
          }
          (Primitive::I32(x), Primitive::I32(min), Primitive::I32(max)) => {
            Primitive::I32(x.min(max).max(min))
          }
          (Primitive::U32(x), Primitive::U32(min), Primitive::U32(max)) => {
            Primitive::U32(x.min(max).max(min))
          }
          _ => panic!(),
        }
      },
    )),
    "smoothstep" => Ok(Value::multi_map_primitive_or_vec_components(
      &args.into_iter().map(|(v, _)| v).collect(),
      |mut values| {
        let x = values.remove(2);
        let max = values.remove(1);
        let min = values.remove(0);
        match (min, max, x) {
          (Primitive::F32(min), Primitive::F32(max), Primitive::F32(x)) => {
            let t = ((x - min) / (max - min)).clamp(0., 1.);
            Primitive::F32(t * t * (3. - 2. * t))
          }
          _ => panic!(),
        }
      },
    )),
    "step" => Ok(Value::multi_map_primitive_or_vec_components(
      &args.into_iter().map(|(v, _)| v).collect(),
      |mut values| {
        let x = values.remove(1);
        let edge = values.remove(0);
        match (edge, x) {
          (Primitive::F32(edge), Primitive::F32(x)) => {
            Primitive::F32(if edge <= x { 1. } else { 0. })
          }
          _ => panic!(),
        }
      },
    )),
    "fma" => Ok(Value::multi_map_primitive_or_vec_components(
      &args.into_iter().map(|(v, _)| v).collect(),
      |mut values| {
        let c = values.remove(2);
        let b = values.remove(1);
        let a = values.remove(0);
        match (a, b, c) {
          (Primitive::F32(a), Primitive::F32(b), Primitive::F32(c)) => {
            Primitive::F32(a * b + c)
          }
          _ => panic!(),
        }
      },
    )),
    "exp" | "exp2" | "log" | "log2" => Ok(Value::Prim(Primitive::F32(
      match args.remove(0).0.unwrap_primitive() {
        Primitive::F32(x) => match &*f_name {
          "exp" => x.exp(),
          "exp2" => x.exp2(),
          "log" => x.ln(),
          "log2" => x.log2(),
          _ => unreachable!(),
        },
        _ => panic!(),
      },
    ))),
    "ldexp" => {
      let Value::Prim(Primitive::F32(e)) = args[0].0 else {
        panic!()
      };
      let Value::Prim(Primitive::I32(exp)) = args[1].0 else {
        panic!()
      };
      Ok(Value::Prim(Primitive::F32(e * (2.0f32).powi(exp))))
    }
    "any" | "all" => Ok(Value::Prim(Primitive::Bool(match args.remove(0).0 {
      Value::Struct(fields) => ["x", "y", "z", "w"]
        .into_iter()
        .filter_map(|name| {
          fields.get(name).map(|value| {
            let Primitive::Bool(b) = value.clone().unwrap_primitive() else {
              panic!()
            };
            b
          })
        })
        .reduce(|a, b| match &*f_name {
          "any" => a || b,
          "all" => a && b,
          _ => panic!(),
        })
        .unwrap(),
      _ => panic!(),
    }))),
    name if name.starts_with("bitcast") => {
      let target_scalar_type = match &return_type_clone {
        Type::F32 | Type::I32 | Type::U32 => return_type_clone.clone(),
        Type::Struct(s) => s.fields[0].field_type.unwrap_known(),
        _ => panic!("bitcast to unsupported type"),
      };
      let bitcast_prim = |p: Primitive| -> Primitive {
        let bits: u32 = match p {
          Primitive::F32(f) => f.to_bits(),
          Primitive::I32(i) => i as u32,
          Primitive::U32(u) => u,
          _ => panic!("bitcast from unsupported type"),
        };
        match &target_scalar_type {
          Type::F32 => Primitive::F32(f32::from_bits(bits)),
          Type::I32 => Primitive::I32(bits as i32),
          Type::U32 => Primitive::U32(bits),
          _ => panic!("bitcast to unsupported scalar type"),
        }
      };
      Ok(args[0].0.map_primitive_or_vec_components(bitcast_prim))
    }
    "array-length" => {
      let len = match &args[0].0 {
        Value::Array(arr) => arr.len(),
        Value::ZeroedArray { length } => *length,
        _ => panic!("array-length called on non-array"),
      };
      Ok(Value::Prim(Primitive::U32(len as u32)))
    }
    "dpdx" | "dpdy" | "dpdx-coarse" | "dpdy-coarse" | "dpdx-fine"
    | "dpdy-fine" => {
      Err(DerivativeFunctionCantBeUsed.into())
      // These should have an effect that isn't handled by default on the CPU
      // so that this is a compile-time error instead
    }
    // --- bit_manipulation_functions ---
    "count-leading-zeros" => {
      Ok(args[0].0.map_primitive_or_vec_components(|p| match p {
        Primitive::U32(v) => Primitive::U32(v.leading_zeros()),
        Primitive::I32(v) => Primitive::I32(v.leading_zeros() as i32),
        _ => panic!(),
      }))
    }
    "count-trailing-zeros" => {
      Ok(args[0].0.map_primitive_or_vec_components(|p| match p {
        Primitive::U32(v) => Primitive::U32(v.trailing_zeros()),
        Primitive::I32(v) => Primitive::I32(v.trailing_zeros() as i32),
        _ => panic!(),
      }))
    }
    "count-one-bits" => {
      Ok(args[0].0.map_primitive_or_vec_components(|p| match p {
        Primitive::U32(v) => Primitive::U32(v.count_ones()),
        Primitive::I32(v) => Primitive::I32(v.count_ones() as i32),
        _ => panic!(),
      }))
    }
    "reverse-bits" => {
      Ok(args[0].0.map_primitive_or_vec_components(|p| match p {
        Primitive::U32(v) => Primitive::U32(v.reverse_bits()),
        Primitive::I32(v) => Primitive::I32((v as u32).reverse_bits() as i32),
        _ => panic!(),
      }))
    }
    "first-leading-bit" => {
      Ok(args[0].0.map_primitive_or_vec_components(|p| match p {
        Primitive::U32(v) => Primitive::U32(if v == 0 {
          u32::MAX
        } else {
          31 - v.leading_zeros()
        }),
        Primitive::I32(v) => Primitive::I32(if v == 0 || v == -1 {
          -1
        } else if v > 0 {
          (31 - v.leading_zeros()) as i32
        } else {
          (31 - (!v).leading_zeros()) as i32
        }),
        _ => panic!(),
      }))
    }
    "first-trailing-bit" => {
      Ok(args[0].0.map_primitive_or_vec_components(|p| match p {
        Primitive::U32(v) => {
          Primitive::U32(if v == 0 { u32::MAX } else { v.trailing_zeros() })
        }
        Primitive::I32(v) => Primitive::I32(if v == 0 {
          -1
        } else {
          v.trailing_zeros() as i32
        }),
        _ => panic!(),
      }))
    }
    "extract-bits" => {
      let offset = match args[1].0 {
        Value::Prim(Primitive::U32(v)) => v,
        _ => panic!(),
      };
      let count = match args[2].0 {
        Value::Prim(Primitive::U32(v)) => v,
        _ => panic!(),
      };
      let o = offset.min(32);
      let c = count.min(32 - o);
      Ok(args[0].0.map_primitive_or_vec_components(|p| match p {
        Primitive::U32(e) => Primitive::U32(if c == 0 {
          0
        } else {
          (e >> o) & ((1u32 << c) - 1)
        }),
        Primitive::I32(e) => Primitive::I32(if c == 0 {
          0
        } else {
          let extracted = ((e as u32) >> o) & ((1u32 << c) - 1);
          let shift = 32 - c;
          ((extracted << shift) as i32) >> shift
        }),
        _ => panic!(),
      }))
    }
    "dot-4-u8-packed" => {
      let (Value::Prim(Primitive::U32(e1)), Value::Prim(Primitive::U32(e2))) =
        (&args[0].0, &args[1].0)
      else {
        panic!()
      };
      let mut acc: u32 = 0;
      for i in 0..4 {
        acc =
          acc.wrapping_add(((e1 >> (i * 8)) & 0xFF) * ((e2 >> (i * 8)) & 0xFF));
      }
      Ok(Value::Prim(Primitive::U32(acc)))
    }
    "dot-4-i8-packed" => {
      let (Value::Prim(Primitive::U32(e1)), Value::Prim(Primitive::U32(e2))) =
        (&args[0].0, &args[1].0)
      else {
        panic!()
      };
      let mut acc: i32 = 0;
      for i in 0..4u32 {
        let a = (((*e1 >> (i * 8)) & 0xFF) as i32) << 24 >> 24;
        let b = (((*e2 >> (i * 8)) & 0xFF) as i32) << 24 >> 24;
        acc += a * b;
      }
      Ok(Value::Prim(Primitive::I32(acc)))
    }
    // --- data_packing_functions ---
    "pack-4x8-snorm" => {
      let Value::Struct(v) = &args[0].0 else {
        panic!()
      };
      let mut result: u32 = 0;
      for (i, f) in ["x", "y", "z", "w"].iter().enumerate() {
        let Value::Prim(Primitive::F32(val)) = v.get(*f).unwrap() else {
          panic!()
        };
        let packed = (0.5 + 127.0 * val.clamp(-1.0, 1.0)).floor() as i8 as u8;
        result |= (packed as u32) << (i * 8);
      }
      Ok(Value::Prim(Primitive::U32(result)))
    }
    "unpack-4x8-snorm" => {
      let Value::Prim(Primitive::U32(e)) = &args[0].0 else {
        panic!()
      };
      Ok(Value::Struct(
        ["x", "y", "z", "w"]
          .iter()
          .enumerate()
          .map(|(i, f)| {
            let byte = ((e >> (i * 8)) & 0xFF) as u8 as i8;
            (
              (*f).into(),
              Value::Prim(Primitive::F32((byte as f32 / 127.0).max(-1.0))),
            )
          })
          .collect(),
      ))
    }
    "pack-4x8-unorm" => {
      let Value::Struct(v) = &args[0].0 else {
        panic!()
      };
      let mut result: u32 = 0;
      for (i, f) in ["x", "y", "z", "w"].iter().enumerate() {
        let Value::Prim(Primitive::F32(val)) = v.get(*f).unwrap() else {
          panic!()
        };
        let packed = (0.5 + 255.0 * val.clamp(0.0, 1.0)).floor() as u8;
        result |= (packed as u32) << (i * 8);
      }
      Ok(Value::Prim(Primitive::U32(result)))
    }
    "unpack-4x8-unorm" => {
      let Value::Prim(Primitive::U32(e)) = &args[0].0 else {
        panic!()
      };
      Ok(Value::Struct(
        ["x", "y", "z", "w"]
          .iter()
          .enumerate()
          .map(|(i, f)| {
            (
              (*f).into(),
              Value::Prim(Primitive::F32(
                ((e >> (i * 8)) & 0xFF) as f32 / 255.0,
              )),
            )
          })
          .collect(),
      ))
    }
    "pack-4x8-i8" => {
      let Value::Struct(v) = &args[0].0 else {
        panic!()
      };
      let mut result: u32 = 0;
      for (i, f) in ["x", "y", "z", "w"].iter().enumerate() {
        let Value::Prim(Primitive::I32(val)) = v.get(*f).unwrap() else {
          panic!()
        };
        result |= ((*val as u32) & 0xFF) << (i * 8);
      }
      Ok(Value::Prim(Primitive::U32(result)))
    }
    "unpack-4x8-i8" => {
      let Value::Prim(Primitive::U32(e)) = &args[0].0 else {
        panic!()
      };
      Ok(Value::Struct(
        ["x", "y", "z", "w"]
          .iter()
          .enumerate()
          .map(|(i, f)| {
            let byte = ((e >> (i * 8)) & 0xFF) as u8 as i8;
            ((*f).into(), Value::Prim(Primitive::I32(byte as i32)))
          })
          .collect(),
      ))
    }
    "pack-4x8-u8" => {
      let Value::Struct(v) = &args[0].0 else {
        panic!()
      };
      let mut result: u32 = 0;
      for (i, f) in ["x", "y", "z", "w"].iter().enumerate() {
        let Value::Prim(Primitive::U32(val)) = v.get(*f).unwrap() else {
          panic!()
        };
        result |= (val & 0xFF) << (i * 8);
      }
      Ok(Value::Prim(Primitive::U32(result)))
    }
    "unpack-4x8-u8" => {
      let Value::Prim(Primitive::U32(e)) = &args[0].0 else {
        panic!()
      };
      Ok(Value::Struct(
        ["x", "y", "z", "w"]
          .iter()
          .enumerate()
          .map(|(i, f)| {
            (
              (*f).into(),
              Value::Prim(Primitive::U32((e >> (i * 8)) & 0xFF)),
            )
          })
          .collect(),
      ))
    }
    "pack-4x8-i8-clamp" => {
      let Value::Struct(v) = &args[0].0 else {
        panic!()
      };
      let mut result: u32 = 0;
      for (i, f) in ["x", "y", "z", "w"].iter().enumerate() {
        let Value::Prim(Primitive::I32(val)) = v.get(*f).unwrap() else {
          panic!()
        };
        result |= (*val.clamp(&-128, &127) as u8 as u32) << (i * 8);
      }
      Ok(Value::Prim(Primitive::U32(result)))
    }
    "pack-4x8-u8-clamp" => {
      let Value::Struct(v) = &args[0].0 else {
        panic!()
      };
      let mut result: u32 = 0;
      for (i, f) in ["x", "y", "z", "w"].iter().enumerate() {
        let Value::Prim(Primitive::U32(val)) = v.get(*f).unwrap() else {
          panic!()
        };
        result |= val.min(&255) << (i * 8);
      }
      Ok(Value::Prim(Primitive::U32(result)))
    }
    "pack-2x16-snorm" => {
      let Value::Struct(v) = &args[0].0 else {
        panic!()
      };
      let mut result: u32 = 0;
      for (i, f) in ["x", "y"].iter().enumerate() {
        let Value::Prim(Primitive::F32(val)) = v.get(*f).unwrap() else {
          panic!()
        };
        let packed =
          (0.5 + 32767.0 * val.clamp(-1.0, 1.0)).floor() as i16 as u16;
        result |= (packed as u32) << (i * 16);
      }
      Ok(Value::Prim(Primitive::U32(result)))
    }
    "unpack-2x16-snorm" => {
      let Value::Prim(Primitive::U32(e)) = &args[0].0 else {
        panic!()
      };
      Ok(Value::Struct(
        ["x", "y"]
          .iter()
          .enumerate()
          .map(|(i, f)| {
            let half = ((e >> (i * 16)) & 0xFFFF) as u16 as i16;
            (
              (*f).into(),
              Value::Prim(Primitive::F32((half as f32 / 32767.0).max(-1.0))),
            )
          })
          .collect(),
      ))
    }
    "pack-2x16-unorm" => {
      let Value::Struct(v) = &args[0].0 else {
        panic!()
      };
      let mut result: u32 = 0;
      for (i, f) in ["x", "y"].iter().enumerate() {
        let Value::Prim(Primitive::F32(val)) = v.get(*f).unwrap() else {
          panic!()
        };
        let packed = (0.5 + 65535.0 * val.clamp(0.0, 1.0)).floor() as u16;
        result |= (packed as u32) << (i * 16);
      }
      Ok(Value::Prim(Primitive::U32(result)))
    }
    "unpack-2x16-unorm" => {
      let Value::Prim(Primitive::U32(e)) = &args[0].0 else {
        panic!()
      };
      Ok(Value::Struct(
        ["x", "y"]
          .iter()
          .enumerate()
          .map(|(i, f)| {
            (
              (*f).into(),
              Value::Prim(Primitive::F32(
                ((e >> (i * 16)) & 0xFFFF) as f32 / 65535.0,
              )),
            )
          })
          .collect(),
      ))
    }
    "pack-2x16-float" | "unpack-2x16-float" => {
      todo!("pack/unpack-2x16-float requires f16 conversion")
    }
    // todo!() matrix access (column indexing, etc.)
    // todo!() texture functions, for now I guess these should just error
    "print" => {
      if let Some(arg) = args.get(0) {
        env.io.println(&arg.0.format_for_print(&args[0].1, env)?);
      } else {
        env.io.println("()");
      }
      Ok(Value::Unit)
    }
    "string" => {
      let s = args[0].0.format_for_print(&args[0].1, env)?;
      Ok(Value::String(s))
    }
    "concat" => {
      let (Value::String(a), Value::String(b)) = (&args[0].0, &args[1].0)
      else {
        panic!()
      };
      Ok(Value::String(format!("{a}{b}")))
    }
    "substr" => {
      let Value::String(s) = &args[0].0 else {
        panic!()
      };
      let start = args[1].0.clone().unwrap_primitive().as_num() as usize;
      let end = args[2].0.clone().unwrap_primitive().as_num() as usize;
      let chars: Vec<char> = s.chars().collect();
      let start = start.min(chars.len());
      let end = end.min(chars.len());
      Ok(Value::String(if start >= end {
        String::new()
      } else {
        chars[start..end].iter().collect()
      }))
    }
    "dispatch-render-shaders" => {
      let (vert_value, Type::Function(vert_f)) = &args[0] else {
        panic!()
      };
      env.upload_dispatched_closure_scope(vert_value, vert_f);
      let vert_effects = vert_f.effects();
      let (vert_read_global_variable_names, vert_written_global_variable_names) =
        vert_effects.gpu_read_and_written_globals();
      let vert_f_name = vert_f
        .abstract_ancestor
        .as_ref()
        .unwrap()
        .read()
        .unwrap()
        .name
        .clone();
      let (frag_value, Type::Function(frag_f)) = &args[1] else {
        panic!()
      };
      env.upload_dispatched_closure_scope(frag_value, frag_f);
      let frag_effects = frag_f.effects();
      let (frag_read_global_variable_names, frag_written_global_variable_names) =
        frag_effects.gpu_read_and_written_globals();
      let frag_f_name = frag_f
        .abstract_ancestor
        .as_ref()
        .unwrap()
        .read()
        .unwrap()
        .name
        .clone();
      let read_global_variable_names: Vec<Arc<str>> =
        vert_read_global_variable_names
          .into_iter()
          .chain(frag_read_global_variable_names.into_iter())
          .collect();
      let written_global_variable_names: Vec<Arc<str>> =
        vert_written_global_variable_names
          .into_iter()
          .chain(frag_written_global_variable_names.into_iter())
          .collect();
      let (Value::Prim(Primitive::U32(vert_count)), _) = &args[2] else {
        panic!()
      };
      let additive =
        if let Some((Value::Prim(Primitive::Bool(b)), _)) = args.get(3) {
          *b
        } else {
          false
        };
      env.setup_gpu_if_needed();
      let mut pre_upload =
        env.collect_dirty_uploads(&read_global_variable_names);
      let render_target =
        env.current_render_target.map(|gb| (gb.group, gb.binding));
      // If rendering to an offscreen texture, also upload it now (even though
      // the shader doesn't read it) so the GPU has the correctly-sized texture
      // to render into.  Collect its upload separately to avoid marking it
      // Synced before the render has run.
      if let Some((rt_group, rt_binding)) = render_target {
        if let Some((_, name, _, _)) =
          env.binding_vars.iter().find(|(gb, _, _, addr)| {
            gb.group == rt_group
              && gb.binding == rt_binding
              && *addr == VariableAddressSpace::Handle
          })
        {
          let name = name.clone();
          // Only upload if the CPU has a value that the GPU doesn't know about yet.
          if env.buffer_states.get(&name)
            == Some(&SharedBufferState::GPUOutOfDate)
          {
            let extra = env.collect_dirty_uploads(&[name.clone()]);
            pre_upload.extend(extra);
          }
        }
      }
      env.io.record_draw(
        env.gpu_entry_id(&vert_f_name),
        env.gpu_entry_id(&frag_f_name),
        &vert_f_name,
        &frag_f_name,
        *vert_count,
        pre_upload,
        additive,
        render_target,
      )?;
      env.mark_gpu_written(&written_global_variable_names);
      // The render writes to the offscreen texture on the GPU, so the CPU's
      // value is now stale.  Mark it CPUOutOfDate so subsequent compute
      // dispatches don't re-upload the CPU value and overwrite the result.
      if let Some((rt_group, rt_binding)) = render_target {
        if let Some((_, name, _, _)) =
          env.binding_vars.iter().find(|(gb, _, _, _)| {
            gb.group == rt_group && gb.binding == rt_binding
          })
        {
          let name = name.clone();
          env.mark_gpu_written(&[name]);
        }
      }
      Ok(Value::Unit)
    }
    "dispatch-compute-shader" => {
      let (compute_value, Type::Function(compute_f)) = &args[0] else {
        panic!()
      };
      let entry_name = compute_f
        .abstract_ancestor
        .as_ref()
        .unwrap()
        .read()
        .unwrap()
        .name
        .clone();
      env.upload_dispatched_closure_scope(compute_value, compute_f);
      let effects = compute_f.effects();
      let (read_global_variable_names, written_global_variable_names) =
        effects.gpu_read_and_written_globals();
      let (Value::Struct(wg), _) = &args[1] else {
        panic!()
      };
      let get_u32 = |field: &str| {
        let Value::Prim(Primitive::U32(v)) = wg[field] else {
          panic!()
        };
        v
      };
      let workgroup_count = (get_u32("x"), get_u32("y"), get_u32("z"));
      env.setup_gpu_if_needed();
      let pre_upload = env.collect_dirty_uploads(&read_global_variable_names);
      env.io.record_compute(
        env.gpu_entry_id(&entry_name),
        &entry_name,
        workgroup_count,
        pre_upload,
      )?;
      env.mark_gpu_written(&written_global_variable_names);
      Ok(Value::Unit)
    }
    "close-window" => {
      env.io.record_close_window();
      Err(EvalException::CloseWindow)
    }
    "start-audio" => {
      let (audio_value, Type::Function(audio_f)) = &args[0] else {
        panic!()
      };
      let (original_name, scope_struct) = {
        let ancestor =
          audio_f.abstract_ancestor.as_ref().unwrap().read().unwrap();
        (ancestor.name.clone(), ancestor.captured_scope.clone())
      };
      // A scoped closure runs on the audio thread as its audio clone
      // (see `extract_audio_closure_scopes`), whose captures live in
      // lifted thread-shared globals.
      let entry_name: Arc<str> = if scope_struct.is_some() {
        format!("{original_name}_audio").into()
      } else {
        original_name.clone()
      };
      #[cfg(feature = "window")]
      {
        // A typical program puts `(start-audio ...)` inside a
        // `spawn-window` callback that fires every frame, so this builtin
        // gets called repeatedly. We only have one `AudioSource` to spend
        // (the bytecode program / C source) — we move it to the IO manager
        // on the first call, then pass `None` on subsequent calls. The IO
        // manager decides what to do with each (StdoutIO noops on the
        // already-running case; StringIO records every event).
        let mut source = env.audio_source.take();
        // Seed the lifted capture globals from the closure value's scope
        // before the publish machinery ships them to the audio replica.
        // On EVERY call: `start-audio` hands off the value it's passed,
        // so each execution re-seeds the lifted capture globals from the
        // just-constructed closure. On the thread-starting call the
        // bootstrap publish below ships them; on later calls
        // `mark_cpu_written` inside the seeding sets the shared-dirty
        // flags and the ordinary frame-end publish ships them.
        if let Some(scope_struct) = &scope_struct
          && let Value::Fun(Function::Scoped { scope, .. }) = audio_value
        {
          let scope_value = (**scope).clone();
          let captures = env
            .lifted_audio_captures
            .get(&original_name)
            .unwrap_or_else(|| {
              panic!(
                "compiler bug: no lifted-capture record for audio entry \
                 `{original_name}`"
              )
            })
            .clone();
          env.seed_audio_scope_globals(scope_struct, &scope_value, &captures);
        }
        // First call (we hold the source): activate the shared table and
        // bootstrap-publish every shared global, so the new replica's first
        // adopt sees the current state of everything (e.g. `load-wav`ed
        // sample buffers). From here on both threads publish/adopt at their
        // iteration boundaries — later writes on either side propagate.
        if let Some(crate::audio::AudioSource::Bytecode {
          shared_table, ..
        }) = &mut source
        {
          env.shared_table.join(participant::AUDIO);
          env.publish_shared_globals(participant::AUDIO);
          *shared_table = Some(env.shared_table.clone());
        }
        env
          .io
          .start_audio(&entry_name, source)
          .map_err(EvalException::Error)?;
        Ok(Value::Unit)
      }
      #[cfg(not(feature = "window"))]
      {
        let _ = (entry_name, audio_value);
        Err(EvalException::Error(WindowFeatureNotEnabled.into()))
      }
    }
    "spawn-window" => {
      let (callback, _) = args.remove(0);
      let (body, scope_binding) = match callback {
        Value::Fun(Function::Composite {
          arg_names: _,
          expression,
        }) => {
          let ExpKind::Function(_, body) = expression.kind else {
            panic!()
          };
          (*body, None)
        }
        Value::Fun(Function::Scoped { inner, scope }) => {
          let Function::Composite {
            arg_names,
            expression,
          } = *inner
          else {
            panic!("spawn-window: scoped inner must be composite")
          };
          let ExpKind::Function(_, body) = expression.kind else {
            panic!()
          };
          let scope_arg = arg_names
            .into_iter()
            .next()
            .expect("scoped function must have a scope arg");
          (*body, Some((scope_arg, *scope)))
        }
        _ => panic!("spawn-window: callback must be a function"),
      };
      if let Some((ref scope_arg, ref scope_val)) = scope_binding {
        env.bind(scope_arg.clone(), scope_val.clone(), Type::Unit);
      }
      let reload = {
        let mut driver = AstFrameDriver { body, env };
        IO::run_spawn_window_driver(&mut driver)?
      };
      if let Some((scope_arg, _)) = scope_binding {
        let _ = env.unbind(&scope_arg);
      }
      if reload {
        return Err(EvalException::ReloadRequested);
      }
      Ok(Value::Unit)
    }
    "into-dynamic-array" => Ok(args.remove(0).0),
    "load-wav" => {
      let Value::String(path) = args.remove(0).0 else {
        panic!("load-wav: expected string path argument")
      };
      Ok(Value::Array(
        load_wav_samples(&path, &env.source_dir)?
          .into_iter()
          .map(|sample| Value::Prim(Primitive::F32(sample)))
          .collect(),
      ))
    }
    "load-image" => {
      let Value::String(path) = args.remove(0).0 else {
        panic!("load-image: expected string path argument")
      };
      Ok(load_image_value(&path, &env.source_dir)?)
    }
    "blank-texture" => {
      let (width, height) = if args.len() == 2 {
        let Value::Prim(Primitive::U32(w)) = args.remove(0).0 else {
          panic!("blank-texture: expected u32 width")
        };
        let Value::Prim(Primitive::U32(h)) = args.remove(0).0 else {
          panic!("blank-texture: expected u32 height")
        };
        (w, h)
      } else {
        let Value::Struct(dims) = args.remove(0).0 else {
          panic!("blank-texture: expected vec2u")
        };
        let Value::Prim(Primitive::U32(w)) = dims["x"] else {
          panic!()
        };
        let Value::Prim(Primitive::U32(h)) = dims["y"] else {
          panic!()
        };
        (w, h)
      };
      let data = vec![0u8; (width * height * 4) as usize];
      Ok(Value::Texture {
        width,
        height,
        data,
        binding: None,
      })
    }
    "texture-dimensions" => {
      // Both overloads: (tex) and (tex level). The mip level arg is ignored on
      // CPU since Value::Texture always holds the base level.
      let Value::Texture { width, height, .. } = args.remove(0).0 else {
        panic!("texture-dimensions: expected Texture argument")
      };
      Ok(Value::Struct(
        [
          ("x".into(), Value::Prim(Primitive::U32(width))),
          ("y".into(), Value::Prim(Primitive::U32(height))),
        ]
        .into_iter()
        .collect(),
      ))
    }
    "set-render-target" => {
      let Value::Texture {
        binding: Some(gb), ..
      } = args.remove(0).0
      else {
        panic!(
          "set-render-target: texture must be assigned to a binding variable \
           before it can be used as a render target"
        )
      };
      env.current_render_target = Some(gb);
      Ok(Value::Unit)
    }
    "clear-render-target" => {
      env.current_render_target = None;
      Ok(Value::Unit)
    }
    "save-png" => {
      let texture = args.remove(0).0;
      let Value::String(path) = args.remove(0).0 else {
        panic!("save-png: expected string path argument")
      };
      let Value::Texture {
        width,
        height,
        data,
        ..
      } = env.refresh_texture_from_gpu(texture)?
      else {
        panic!("save-png: expected Texture argument")
      };
      save_png_file(&path, width, height, &data, &env.source_dir)?;
      Ok(Value::Unit)
    }
    "window-resolution" => {
      let (w, h) = env.io.window_size();
      Ok(Value::Struct(
        [
          ("x".into(), Value::Prim(Primitive::U32(w))),
          ("y".into(), Value::Prim(Primitive::U32(h))),
        ]
        .into_iter()
        .collect(),
      ))
    }
    "window-time" => Ok(Value::Prim(Primitive::F32(env.io.window_time()))),
    "window-delta-time" => {
      Ok(Value::Prim(Primitive::F32(env.io.window_delta_time())))
    }
    "window-frame-index" => {
      Ok(Value::Prim(Primitive::U32(env.io.window_frame_index())))
    }
    "key-down?" => {
      let Value::String(key) = &args[0].0 else {
        panic!()
      };
      Ok(Value::Prim(Primitive::Bool(env.io.key_down(key))))
    }
    "key-just-down?" => {
      let Value::String(key) = &args[0].0 else {
        panic!()
      };
      Ok(Value::Prim(Primitive::Bool(env.io.key_just_down(key))))
    }
    "mouse-coords" => {
      let (x, y) = env.io.mouse_coords();
      Ok(Value::Struct(
        [
          ("x".into(), Value::Prim(Primitive::U32(x))),
          ("y".into(), Value::Prim(Primitive::U32(y))),
        ]
        .into_iter()
        .collect(),
      ))
    }
    "mouse-present?" => {
      Ok(Value::Prim(Primitive::Bool(env.io.mouse_present())))
    }
    "mouse-down?" => Ok(Value::Prim(Primitive::Bool(env.io.mouse_down()))),
    "mouse-just-down?" => {
      Ok(Value::Prim(Primitive::Bool(env.io.mouse_just_down())))
    }
    "zeroed-array" => {
      let Type::Array(size, _) = return_type else {
        panic!()
      };
      let size = if let Some(arg) = args.get(0) {
        // dynamically sized
        let Value::Prim(Primitive::U32(size)) = arg.0 else {
          panic!()
        };
        size
      } else {
        // statically sized
        size.unwrap().as_literal().unwrap()
      };
      Ok(Value::ZeroedArray {
        length: size as usize,
      })
    }
    "atomic-load" => {
      let Value::Struct(fields) = args.remove(0).0 else {
        panic!()
      };
      Ok(fields["_"].clone())
    }
    "atomic-store" | "atomic-exchange" => {
      let val = args.remove(1).0;
      Ok(Value::Struct([("_".into(), val)].into_iter().collect()))
    }
    "atomic-add" | "atomic-sub" | "atomic-max" | "atomic-min"
    | "atomic-and" | "atomic-or" | "atomic-xor" => {
      let Value::Struct(ref fields) = args[0].0 else {
        panic!()
      };
      let old = fields["_"].clone().unwrap_primitive();
      let val = args[1].0.clone().unwrap_primitive();
      let new_prim = match (old, val) {
        (Primitive::U32(a), Primitive::U32(b)) => {
          Primitive::U32(match &*f_name {
            "atomic-add" => a.wrapping_add(b),
            "atomic-sub" => a.wrapping_sub(b),
            "atomic-max" => a.max(b),
            "atomic-min" => a.min(b),
            "atomic-and" => a & b,
            "atomic-or" => a | b,
            "atomic-xor" => a ^ b,
            _ => unreachable!(),
          })
        }
        (Primitive::I32(a), Primitive::I32(b)) => {
          Primitive::I32(match &*f_name {
            "atomic-add" => a.wrapping_add(b),
            "atomic-sub" => a.wrapping_sub(b),
            "atomic-max" => a.max(b),
            "atomic-min" => a.min(b),
            "atomic-and" => a & b,
            "atomic-or" => a | b,
            "atomic-xor" => a ^ b,
            _ => unreachable!(),
          })
        }
        _ => panic!("atomic operations require integer types"),
      };
      Ok(Value::Struct(
        [("_".into(), Value::Prim(new_prim))].into_iter().collect(),
      ))
    }
    _ => Err(UnimplementedBuiltin(f_name.into()).into()),
  }
}

impl Value {
  fn format_for_print(
    &self,
    t: &Type,
    env: &EvaluationEnvironment<impl IOManager>,
  ) -> Result<String, EvalError> {
    Ok(match (self, t) {
      (Value::Prim(Primitive::F32(f)), _) => {
        let s = f.to_string();
        if s.contains('.') { s } else { format!("{s}.") }
      }
      (Value::Prim(Primitive::I32(i)), _) => i.to_string(),
      (Value::Prim(Primitive::U32(u)), _) => format!("{u}u"),
      (Value::Prim(Primitive::Bool(b)), _) => b.to_string(),
      (Value::Array(cols), Type::Struct(s)) if s.name.starts_with("mat") => {
        let scalar_type = s.fields[0].field_type.kind.unwrap_known();
        let suffix = match &scalar_type {
          Type::F32 => "f",
          Type::I32 => "i",
          Type::U32 => "u",
          _ => "",
        };
        // Format each column vector using its own value structure
        let formatted_cols: Vec<String> = cols
          .iter()
          .map(|col| {
            let Value::Struct(fields) = col else { panic!() };
            let formatted: Vec<String> = ["x", "y", "z", "w"]
              .iter()
              .filter_map(|f| fields.get(*f))
              .map(|v| v.format_for_print(&scalar_type, env))
              .collect::<Result<_, EvalError>>()?;
            Ok(format!(
              "(vec{}{suffix} {})",
              formatted.len(),
              formatted.join(" ")
            ))
          })
          .collect::<Result<_, EvalError>>()?;
        format!("({}{suffix} {})", s.name, formatted_cols.join(" "))
      }
      (Value::Struct(fields), Type::Struct(s)) => {
        let formatted_fields: Vec<String> = s
          .fields
          .iter()
          .map(|field| {
            let value = &fields[&field.name];
            let field_type = field.field_type.kind.unwrap_known();
            value.format_for_print(&field_type, env)
          })
          .collect::<Result<_, _>>()?;
        let name = match &*s.name {
          "vec2" | "vec3" | "vec4" => {
            let suffix = match s.fields[0].field_type.kind.unwrap_known() {
              Type::F32 => "f",
              Type::I32 => "i",
              Type::U32 => "u",
              Type::Bool => "b",
              _ => "",
            };
            format!("{}{suffix}", s.name)
          }
          _ => s.name.to_string(),
        };
        format!("({name} {})", formatted_fields.join(" "))
      }
      (Value::Enum(variant, inner), Type::Enum(e)) => {
        let variant_type = e
          .variants
          .iter()
          .find(|v| &*v.name == &**variant)
          .map(|v| v.inner_type.kind.unwrap_known())
          .unwrap_or(Type::Unit);
        match &variant_type {
          Type::Unit => variant.to_string(),
          t => format!("({} {})", variant, inner.format_for_print(t, env)?),
        }
      }
      (Value::Array(items), Type::Array(_, inner_type)) => {
        let inner = inner_type.kind.unwrap_known();
        let formatted: Vec<String> = items
          .iter()
          .map(|item| item.format_for_print(&inner, env))
          .collect::<Result<_, _>>()?;
        format!("[{}]", formatted.join(" "))
      }
      (Value::ZeroedArray { length }, Type::Array(_, inner_type)) => {
        let inner = inner_type.kind.unwrap_known();
        let zero_val = Value::zeroed(inner.clone(), env)?;
        let items: Vec<String> =
          std::iter::repeat(zero_val.format_for_print(&inner, env))
            .take(*length)
            .collect::<Result<_, _>>()?;
        format!("[{}]", items.join(" "))
      }
      (Value::Unit, _) => "()".to_string(),
      (Value::String(s), _) => s.clone(),
      _ => format!("{:?}", self),
    })
  }

  fn zeroed(
    t: Type,
    env: &EvaluationEnvironment<impl IOManager>,
  ) -> Result<Self, EvalError> {
    Ok(match t {
      Type::Unit => Value::Unit,
      Type::F32 => Primitive::F32(0.).into(),
      Type::I32 => Primitive::I32(0).into(),
      Type::U32 => Primitive::U32(0).into(),
      Type::Bool => Primitive::Bool(false).into(),
      Type::Struct(s) => Value::Struct({
        let mut map = HashMap::new();
        for field in s.fields {
          map.insert(
            field.name,
            Value::zeroed(field.field_type.kind.unwrap_known(), env)?,
          );
        }
        map
      }),
      Type::Array(array_size, inner_type) => Value::Array(
        std::iter::repeat(Value::zeroed(inner_type.kind.unwrap_known(), env)?)
          .take(match array_size.unwrap() {
            ConcreteArraySize::Literal(size) => size as usize,
            ConcreteArraySize::Constant(name) => {
              let value = env.lookup(&(&*name).into())?;
              match value {
                Value::Prim(primitive) => match primitive {
                  Primitive::U32(u) => *u as usize,
                  Primitive::I32(i) => *i as usize,
                  _ => return Err(InvalidArraySize.into()),
                },
                _ => return Err(InvalidArraySize.into()),
              }
            }
            ConcreteArraySize::Unsized => 0,
            ConcreteArraySize::Skolem(_) => {
              return Err(CantCreateZeroedSkolemSizedArray.into());
            }
            ConcreteArraySize::UnificationVariable(const_generic_value) => {
              match &*const_generic_value.value.read().unwrap() {
                Some(ConstGenericResolution::Literal(x)) => *x as usize,
                _ => return Err(CantCreateZeroedSkolemSizedArray.into()),
              }
            }
          })
          .collect(),
      ),
      Type::Function(_) => return Err(CantCreateZeroedFunction.into()),
      Type::Skolem(_, _) => return Err(CantCreateZeroedSkolem.into()),
      Type::Enum(e) => {
        let first_variant = &e.variants[0];
        Value::Enum(
          first_variant.name.clone(),
          Self::zeroed(first_variant.inner_type.unwrap_known(), env)?.into(),
        )
      }
      Type::String => Value::String(String::new()),
    })
  }
  fn unwrap_primitive(self) -> Primitive {
    match self {
      Value::Prim(p) => p,
      _ => panic!(),
    }
  }
  fn as_struct(&self) -> &HashMap<Arc<str>, Value> {
    match self {
      Value::Struct(s) => s,
      _ => panic!(),
    }
  }

  /// Serializes this value to raw bytes for uploading to a uniform buffer.
  /// The `ty` parameter is used to determine field ordering for struct types.
  pub fn to_uniform_bytes(&self, ty: &Type) -> Vec<u8> {
    match self {
      Value::Prim(Primitive::F32(f)) => f.to_bits().to_ne_bytes().to_vec(),
      Value::Prim(Primitive::U32(u)) => u.to_ne_bytes().to_vec(),
      Value::Prim(Primitive::I32(i)) => i.to_ne_bytes().to_vec(),
      Value::Prim(Primitive::Bool(b)) => (*b as u32).to_ne_bytes().to_vec(),
      Value::Struct(fields) => {
        let Type::Struct(s) = ty else { panic!() };
        let struct_size_u32s = ty.wgsl_flat_data_size_in_u32s();
        let mut bytes = vec![];
        let mut offset_u32s = 0usize;
        for field in &s.fields {
          let ft = field.field_type.unwrap_known();
          let align = ft.wgsl_alignment_in_u32s();
          let field_size = ft.wgsl_flat_data_size_in_u32s();
          let target = ((offset_u32s + align - 1) / align) * align;
          bytes.extend(std::iter::repeat(0u8).take((target - offset_u32s) * 4));
          offset_u32s = target;
          if let Some(v) = fields.get(&field.name) {
            bytes.extend(v.to_uniform_bytes(&ft));
          } else {
            bytes.extend(std::iter::repeat(0u8).take(field_size * 4));
          }
          offset_u32s += field_size;
        }
        bytes.extend(
          std::iter::repeat(0u8).take((struct_size_u32s - offset_u32s) * 4),
        );
        bytes
      }
      Value::Array(inner_values) => {
        let Type::Array(_, inner_type) = &ty else {
          panic!()
        };
        let inner_ty = inner_type.unwrap_known();
        let elem_size = inner_ty.wgsl_flat_data_size_in_u32s();
        let stride = ((elem_size + inner_ty.wgsl_alignment_in_u32s() - 1)
          / inner_ty.wgsl_alignment_in_u32s())
          * inner_ty.wgsl_alignment_in_u32s();
        let mut bytes = vec![];
        for value in inner_values {
          bytes.extend(value.to_uniform_bytes(&inner_ty));
          bytes.extend(std::iter::repeat(0u8).take((stride - elem_size) * 4));
        }
        bytes
      }
      Value::ZeroedArray { length } => {
        let Type::Array(_, inner_type) = &ty else {
          panic!()
        };
        let inner_ty = inner_type.unwrap_known();
        let elem_size = inner_ty.wgsl_flat_data_size_in_u32s();
        let align = inner_ty.wgsl_alignment_in_u32s();
        let stride = ((elem_size + align - 1) / align) * align;
        vec![0u8; length * stride * 4]
      }
      Value::Enum(variant, inner) => {
        let Type::Enum(e) = ty else {
          return vec![];
        };
        let discriminant = e
          .variants
          .iter()
          .position(|v| v.name == *variant)
          .unwrap_or(0) as u32;
        let max_inner_size = e.inner_flat_data_size_in_u32s().unwrap_or(0);
        let mut bytes = discriminant.to_ne_bytes().to_vec();
        if max_inner_size > 0 {
          if let Some(variant_def) =
            e.variants.iter().find(|v| v.name == *variant)
          {
            let inner_ty = variant_def.inner_type.unwrap_known();
            if inner_ty != Type::Unit {
              bytes.extend(inner.to_uniform_bytes(&inner_ty));
            }
          }
          // Pad inner data to max_inner_size u32s
          bytes.resize((1 + max_inner_size) * 4, 0);
        }
        bytes
      }
      // A captured closure inside a dispatched-closure scope: the function
      // part is static, so only its captured scope is data. `ty` is the
      // representative scope-struct type (see
      // `Program::substitute_scope_representative_types`).
      Value::Fun(Function::Scoped { scope, .. }) => scope.to_uniform_bytes(ty),
      _ => vec![],
    }
  }

  /// Deserializes a `Value` from the bytecode VM's flat stack layout (no
  /// alignment padding, matrices as cols*rows column-major scalars). The
  /// value shapes produced match the tree-walking interpreter's exactly
  /// (e.g. matrices as arrays of column vectors), so printing and GPU
  /// serialization behave identically across both runtimes.
  pub fn from_vm_words(t: &Type, words: &[u32]) -> Value {
    fn words_of(t: &Type) -> usize {
      t.flat_data_size_in_u32s(&crate::compiler::error::SourceTrace::empty())
        .unwrap() as usize
    }
    match t {
      Type::F32 => Primitive::F32(f32::from_bits(words[0])).into(),
      Type::U32 => Primitive::U32(words[0]).into(),
      Type::I32 => Primitive::I32(words[0] as i32).into(),
      Type::Bool => Primitive::Bool(words[0] != 0).into(),
      Type::Unit => Value::Unit,
      Type::Struct(s)
        if s.name.starts_with("mat") && s.name.as_bytes().len() >= 6 =>
      {
        let cols = (s.name.as_bytes()[3] - b'0') as usize;
        let rows = (s.name.as_bytes()[5] - b'0') as usize;
        let elem = s.fields[0].field_type.kind.unwrap_known();
        let field_names = ["x", "y", "z", "w"];
        Value::Array(
          (0..cols)
            .map(|c| {
              Value::Struct(
                (0..rows)
                  .map(|r| {
                    (
                      Arc::<str>::from(field_names[r]),
                      Value::from_vm_words(
                        &elem,
                        &words[c * rows + r..c * rows + r + 1],
                      ),
                    )
                  })
                  .collect(),
              )
            })
            .collect(),
        )
      }
      Type::Struct(s) => {
        let mut map = HashMap::new();
        let mut offset = 0usize;
        for field in &s.fields {
          let field_type = field.field_type.kind.unwrap_known();
          let n = words_of(&field_type);
          map.insert(
            field.name.clone(),
            Value::from_vm_words(&field_type, &words[offset..offset + n]),
          );
          offset += n;
        }
        Value::Struct(map)
      }
      Type::Enum(e) => {
        let discriminant = words[0] as usize;
        let variant = &e.variants[discriminant];
        let inner_type = variant.inner_type.kind.unwrap_known();
        let inner = if inner_type == Type::Unit {
          Value::Unit
        } else {
          let n = words_of(&inner_type);
          Value::from_vm_words(&inner_type, &words[1..1 + n])
        };
        Value::Enum(variant.name.clone(), Box::new(inner))
      }
      Type::Array(Some(size), inner_type) if size.as_literal().is_some() => {
        let count = size.as_literal().unwrap();
        let inner = inner_type.kind.unwrap_known();
        let stride = words_of(&inner);
        Value::Array(
          (0..count as usize)
            .map(|i| {
              Value::from_vm_words(&inner, &words[i * stride..(i + 1) * stride])
            })
            .collect(),
        )
      }
      _ => panic!("from_vm_words: unsupported type {t:?}"),
    }
  }

  /// Serializes a `Value` into the bytecode VM's flat stack layout — the
  /// inverse of `from_vm_words`.
  pub fn to_vm_words(&self, t: &Type) -> Vec<u32> {
    fn words_of(t: &Type) -> usize {
      t.flat_data_size_in_u32s(&crate::compiler::error::SourceTrace::empty())
        .unwrap() as usize
    }
    match (self, t) {
      (Value::Prim(Primitive::F32(f)), _) => vec![f.to_bits()],
      (Value::Prim(Primitive::U32(u)), _) => vec![*u],
      (Value::Prim(Primitive::I32(i)), _) => vec![*i as u32],
      (Value::Prim(Primitive::Bool(b)), _) => vec![*b as u32],
      (Value::Unit, _) => vec![],
      (Value::Array(cols), Type::Struct(s))
        if s.name.starts_with("mat") && s.name.as_bytes().len() >= 6 =>
      {
        let elem = s.fields[0].field_type.kind.unwrap_known();
        let field_names = ["x", "y", "z", "w"];
        let mut words = vec![];
        for col in cols {
          let Value::Struct(fields) = col else {
            panic!("matrix column wasn't a struct")
          };
          for name in field_names {
            if let Some(v) = fields.get(name) {
              words.extend(v.to_vm_words(&elem));
            }
          }
        }
        words
      }
      (Value::Struct(fields), Type::Struct(s)) => {
        let mut words = vec![];
        for field in &s.fields {
          let field_type = field.field_type.kind.unwrap_known();
          words.extend(fields[&field.name].to_vm_words(&field_type));
        }
        words
      }
      (Value::Enum(variant, inner), Type::Enum(e)) => {
        let discriminant = e
          .variants
          .iter()
          .position(|v| v.name == *variant)
          .expect("enum variant not found") as u32;
        let mut words = vec![discriminant];
        let inner_type = e
          .variants
          .iter()
          .find(|v| v.name == *variant)
          .unwrap()
          .inner_type
          .kind
          .unwrap_known();
        if inner_type != Type::Unit {
          words.extend(inner.to_vm_words(&inner_type));
        }
        words.resize(words_of(t), 0);
        words
      }
      (Value::Array(items), Type::Array(_, inner_type)) => {
        let inner = inner_type.kind.unwrap_known();
        let mut words = vec![];
        for item in items {
          words.extend(item.to_vm_words(&inner));
        }
        words
      }
      (Value::ZeroedArray { length }, Type::Array(_, inner_type)) => {
        let inner = inner_type.kind.unwrap_known();
        vec![0u32; length * words_of(&inner)]
      }
      _ => panic!("to_vm_words: unsupported value/type pair"),
    }
  }

  /// Deserializes a `Value` from raw GPU bytes, given the expected `Type`.
  pub fn from_gpu_bytes(bytes: &[u8], ty: &Type) -> Value {
    use crate::compiler::types::ConcreteArraySize;
    // Unsized arrays: the type carries no element count, so derive it from
    // how many whole elements fit in the returned byte slice.
    if let Type::Array(Some(ConcreteArraySize::Unsized), inner_type) = ty {
      let inner_ty = inner_type.unwrap_known();
      let elem_size = inner_ty.wgsl_flat_data_size_in_u32s();
      let align = inner_ty.wgsl_alignment_in_u32s();
      let stride = ((elem_size + align - 1) / align) * align;
      let stride_bytes = stride * 4;
      let count = if stride_bytes > 0 {
        bytes.len() / stride_bytes
      } else {
        0
      };
      let mut offset = 0;
      return Value::Array(
        (0..count)
          .map(|_| {
            let start = offset;
            let v = Self::from_gpu_bytes_at(bytes, &inner_ty, &mut offset);
            offset = start + stride_bytes;
            v
          })
          .collect(),
      );
    }
    Self::from_gpu_bytes_at(bytes, ty, &mut 0)
  }

  fn from_gpu_bytes_at(bytes: &[u8], ty: &Type, offset: &mut usize) -> Value {
    fn read_u32(bytes: &[u8], offset: &mut usize) -> u32 {
      let v = u32::from_ne_bytes(
        bytes[*offset..*offset + 4].try_into().unwrap_or([0; 4]),
      );
      *offset += 4;
      v
    }
    match ty {
      Type::U32 => Value::Prim(Primitive::U32(read_u32(bytes, offset))),
      Type::I32 => Value::Prim(Primitive::I32(read_u32(bytes, offset) as i32)),
      Type::F32 => {
        Value::Prim(Primitive::F32(f32::from_bits(read_u32(bytes, offset))))
      }
      Type::Bool => Value::Prim(Primitive::Bool(read_u32(bytes, offset) != 0)),
      Type::Struct(s) => {
        let struct_start = *offset;
        let struct_size_u32s = ty.wgsl_flat_data_size_in_u32s();
        let mut field_cursor = 0usize;
        let mut fields_map = HashMap::new();
        for field in &s.fields {
          let inner_ty = field.field_type.unwrap_known();
          let align = inner_ty.wgsl_alignment_in_u32s();
          let size = inner_ty.wgsl_flat_data_size_in_u32s();
          field_cursor = ((field_cursor + align - 1) / align) * align;
          *offset = struct_start + field_cursor * 4;
          let v = Self::from_gpu_bytes_at(bytes, &inner_ty, offset);
          field_cursor += size;
          fields_map.insert(field.name.clone(), v);
        }
        *offset = struct_start + struct_size_u32s * 4;
        Value::Struct(fields_map)
      }
      Type::Array(Some(size), inner_type) => {
        let inner_ty = inner_type.unwrap_known();
        let elem_size = inner_ty.wgsl_flat_data_size_in_u32s();
        let align = inner_ty.wgsl_alignment_in_u32s();
        let stride = ((elem_size + align - 1) / align) * align;
        let count = size.as_literal().map(|n| n as usize).unwrap_or(0);
        Value::Array(
          (0..count)
            .map(|_| {
              let start = *offset;
              let v = Self::from_gpu_bytes_at(bytes, &inner_ty, offset);
              *offset = start + stride * 4;
              v
            })
            .collect(),
        )
      }
      Type::Enum(e) => {
        // GPU layout: { discriminant: u32, data: array<u32, N> }
        // where N = inner_flat_data_size_in_u32s() (max inner size across variants).
        let inner_size = e.inner_flat_data_size_in_u32s().unwrap_or(0);
        let discriminant = read_u32(bytes, offset) as usize;
        let inner_data_start = *offset;
        *offset += inner_size * 4;
        if let Some(variant) = e.variants.get(discriminant) {
          let inner_ty = variant.inner_type.unwrap_known();
          let inner_value = if inner_ty == Type::Unit {
            Value::Unit
          } else {
            let mut inner_offset = inner_data_start;
            Self::from_gpu_bytes_at(bytes, &inner_ty, &mut inner_offset)
          };
          Value::Enum(variant.name.clone(), inner_value.into())
        } else {
          Value::Uninitialized
        }
      }
      _ => Value::Uninitialized,
    }
  }
}

impl From<Primitive> for Value {
  fn from(primitive: Primitive) -> Self {
    Self::Prim(primitive)
  }
}

/// Describes what data to write into a GPU buffer binding before a dispatch.
#[derive(Debug, Clone, PartialEq)]
pub enum BufferUpload {
  /// Upload the given bytes verbatim.
  Data(Vec<u8>),
  /// Zero-fill `byte_count` bytes on the GPU side (no CPU allocation needed).
  Clear { byte_count: u64 },
  /// Upload RGBA8 pixel data to a texture binding.
  TextureData {
    width: u32,
    height: u32,
    data: Vec<u8>,
  },
}

/// Internal frame-level GPU command, used by StdoutIO to pass commands to
/// the wgpu renderer in window.rs. Entry points are referenced by dense id
/// (index into the env's GPU entry table, resolved once at dispatch-record
/// time) so the GPU frame loop never does string work.
#[derive(Debug, Clone, PartialEq)]
pub enum WindowEvent {
  RenderShaders {
    vert: u16,
    frag: u16,
    vert_count: u32,
    pre_upload: Vec<((u8, u8), BufferUpload)>,
    additive: bool,
    /// If Some, render to this texture binding instead of the screen.
    render_target: Option<(u8, u8)>,
  },
  ComputeShader {
    entry: u16,
    workgroup_count: (u32, u32, u32),
    pre_upload: Vec<((u8, u8), BufferUpload)>,
  },
}

/// One GPU entry point, as `window.rs` needs it: the WGSL-compiled entry
/// name and the (group, binding) keys of every buffer binding the entry's
/// code (transitively) references. Indexed by dense entry id — the ids
/// `WindowEvent` carries. Each pipeline's bind group layouts are built from
/// exactly these sets, so a pipeline's per-stage binding budget only pays
/// for what its own entry points use.
#[derive(Debug, Clone, PartialEq)]
pub struct GpuEntryInfo {
  pub name: String,
  pub used_bindings: Vec<(u8, u8)>,
}

/// A single observable event emitted by the interpreter, recorded by StringIO
/// in order of occurrence.
#[derive(Debug, Clone, PartialEq)]
pub enum IOEvent {
  Print(String),
  SpawnWindow,
  DispatchShaders {
    vert: String,
    frag: String,
    vert_count: u32,
  },
  DispatchComputeShader {
    entry: String,
    workgroup_count: (u32, u32, u32),
  },
  StartAudio {
    entry: String,
  },
  CloseWindow,
}

/// Spoofed ambient window/input values for tests: when set on an IO
/// manager, its window-info accessors report these instead of real state,
/// so tests can assert that values plumb through the runtime (including
/// into GPU-side window-info bindings) deterministically.
#[derive(Debug, Clone, PartialEq)]
pub struct SpoofedWindowInfo {
  pub size: (u32, u32),
  pub time: f32,
  pub delta_time: f32,
  pub frame_index: u32,
  pub mouse_coords: (u32, u32),
  pub mouse_present: bool,
  pub mouse_down: bool,
  pub mouse_just_down: bool,
  /// Keys reported as currently held down.
  pub keys_down: Vec<String>,
  /// Keys reported as pressed this frame.
  pub keys_just_down: Vec<String>,
}

/// The current value of one window-info binding source, as flat u32 words
/// in the VM/GPU layout (shared by both CPU runtimes so their refreshes
/// agree).
pub fn window_info_words<IO: IOManager>(
  source: &WindowInfoBindingSource,
  io: &IO,
) -> Vec<u32> {
  match source {
    WindowInfoBindingSource::Simple(kind) => match kind {
      WindowInfoKind::Resolution => {
        let (w, h) = io.window_size();
        vec![w, h]
      }
      WindowInfoKind::Time => vec![io.window_time().to_bits()],
      WindowInfoKind::DeltaTime => vec![io.window_delta_time().to_bits()],
      WindowInfoKind::FrameIndex => vec![io.window_frame_index()],
      WindowInfoKind::MouseCoords => {
        let (x, y) = io.mouse_coords();
        vec![x, y]
      }
      WindowInfoKind::MousePresent => vec![io.mouse_present() as u32],
      WindowInfoKind::MouseDown => vec![io.mouse_down() as u32],
      WindowInfoKind::MouseJustDown => vec![io.mouse_just_down() as u32],
      WindowInfoKind::KeyDown | WindowInfoKind::KeyJustDown => {
        unreachable!(
          "key queries are recorded as KeyDown/KeyJustDown sources, never \
           Simple"
        )
      }
    },
    WindowInfoBindingSource::KeyDown(key) => {
      vec![io.key_down(key) as u32]
    }
    WindowInfoBindingSource::KeyJustDown(key) => {
      vec![io.key_just_down(key) as u32]
    }
  }
}

/// Which shader stages actually reference a binding, derived from the
/// transitive effects of each GPU entry point. `window.rs` maps this to
/// `wgpu::ShaderStages` when building bind group layouts — declaring only
/// genuinely-used stages matters because per-stage binding budgets are
/// limited (Metal caps the vertex stage at 16 buffer slots total, counting
/// every vertex-visible layout binding whether or not the shader uses it).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct BindingStages {
  pub vertex: bool,
  pub fragment: bool,
  pub compute: bool,
}

/// Everything `window.rs` needs to create and validate the wgpu binding for
/// one GPU-bound top-level variable.
#[derive(Debug, Clone)]
pub struct GpuBindingInfo {
  pub group: u8,
  pub binding: u8,
  /// Source-level variable name, for human-readable diagnostics.
  pub name: Arc<str>,
  pub kind: GpuBufferKind,
  /// Static size in bytes; 0 for dynamically-sized (unsized array) bindings.
  pub byte_size: u64,
  pub stages: BindingStages,
}

/// Describes the GPU buffer type for a top-level variable binding.
/// Used by `window.rs` to create the correct wgpu binding.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GpuBufferKind {
  Uniform,
  StorageReadOnly,
  StorageReadWrite,
  /// A 2D texture bound via `@group/@binding` with `Handle` address space.
  /// Backed by a `wgpu::Texture` rather than a `wgpu::Buffer`.
  Texture2D,
}

/// Tracks whether a GPU-bound buffer is in sync between CPU and GPU.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SharedBufferState {
  /// CPU and GPU hold the same data.
  Synced,
  /// The GPU wrote to this buffer; the CPU value is stale.
  CPUOutOfDate,
  /// The CPU wrote to this buffer; the GPU copy needs to be uploaded.
  GPUOutOfDate,
}

/// The GPU-facing interface of a validated program: everything `GpuCore`
/// creation needs, derivable without constructing an evaluation
/// environment. Environment construction goes through this too, so
/// embedders that create the `GpuCore` themselves before spawning the
/// runtime (e.g. easl_studio) can never diverge from the runtime's own
/// derivation.
pub struct DerivedGpuInterface {
  pub binding_vars:
    Vec<(GroupAndBinding, Arc<str>, Type, VariableAddressSpace)>,
  pub binding_stages: HashMap<Arc<str>, BindingStages>,
  pub gpu_entries: Vec<GpuEntryInfo>,
  /// Source-level entry name -> dense id into `gpu_entries`.
  pub gpu_entry_ids: HashMap<Arc<str>, u16>,
}

impl DerivedGpuInterface {
  pub fn binding_infos(&self) -> Vec<GpuBindingInfo> {
    gpu_binding_infos_from(&self.binding_vars, &self.binding_stages)
  }
}

fn gpu_binding_infos_from(
  binding_vars: &[(GroupAndBinding, Arc<str>, Type, VariableAddressSpace)],
  binding_stages: &HashMap<Arc<str>, BindingStages>,
) -> Vec<GpuBindingInfo> {
  binding_vars
    .iter()
    .map(|(gb, name, ty, addr)| {
      let kind = match addr {
        VariableAddressSpace::Uniform => GpuBufferKind::Uniform,
        VariableAddressSpace::StorageRead => GpuBufferKind::StorageReadOnly,
        VariableAddressSpace::StorageReadWrite => {
          GpuBufferKind::StorageReadWrite
        }
        VariableAddressSpace::Handle => GpuBufferKind::Texture2D,
        _ => unreachable!(),
      };
      // Textures have no buffer size (handled separately in window.rs).
      // 0 for unsized arrays → size handled dynamically in window.rs.
      let size = if kind == GpuBufferKind::Texture2D {
        0
      } else {
        let u32s = ty.wgsl_flat_data_size_in_u32s();
        if u32s == 0 {
          0
        } else {
          ((u32s as u64 * 4).max(4) + 15) & !15
        }
      };
      GpuBindingInfo {
        group: gb.group,
        binding: gb.binding,
        name: name.clone(),
        kind,
        byte_size: size,
        stages: binding_stages.get(name).copied().unwrap_or_default(),
      }
    })
    .collect()
}

/// Derives the [`DerivedGpuInterface`] of a validated program. Only vars
/// actually referenced by GPU code (`Program::gpu_used_globals`) become
/// binding vars: a GPU-space var no shader touches is an ordinary CPU
/// value with no buffer, no uploads, and no readbacks — its declaration
/// exists only in the emitted WGSL.
pub fn derive_gpu_interface(program: &Program) -> DerivedGpuInterface {
  let gpu_used = program.gpu_used_globals();
  let binding_vars: Vec<(
    GroupAndBinding,
    Arc<str>,
    Type,
    VariableAddressSpace,
  )> = program
    .top_level_vars
    .iter()
    .filter_map(|var| {
      if let TopLevelVariableKind::Var {
        address_space,
        group_and_binding: Some(binding_spec),
      } = var.kind
        && gpu_used.contains(&var.name)
      {
        matches!(
          address_space,
          VariableAddressSpace::Uniform
            | VariableAddressSpace::StorageRead
            | VariableAddressSpace::StorageReadWrite
            | VariableAddressSpace::Handle
        )
        .then(|| {
          (
            binding_spec.specified(),
            var.name.clone(),
            var.var_type.clone(),
            address_space,
          )
        })
      } else {
        None
      }
    })
    .collect();
  // Which shader stages actually touch each GPU-bound global: union the
  // transitive effect read/write sets of every GPU entry point (this
  // covers implicit dispatched-closure scope bindings too — the scope
  // rewrite marks their reads as global). `gpu_read_and_written_globals`
  // rather than `read_and_written_globals`: length-only reads still
  // require the binding to be visible, since WGSL's arrayLength derives
  // from buffer size.
  let mut binding_stages: HashMap<Arc<str>, BindingStages> = HashMap::new();
  // Dense GPU entry table: every vertex/fragment/compute entry point gets
  // an id (its index), its WGSL-compiled name, and the (group, binding)
  // keys of the buffer bindings its code transitively references — the
  // basis for per-pipeline bind group layouts in window.rs. Sorted by
  // name so ids are deterministic across runs.
  let group_and_binding_by_name: HashMap<&Arc<str>, (u8, u8)> = binding_vars
    .iter()
    .map(|(gb, name, _, _)| (name, (gb.group, gb.binding)))
    .collect();
  let mut gpu_entries: Vec<GpuEntryInfo> = vec![];
  let mut gpu_entry_ids: HashMap<Arc<str>, u16> = HashMap::new();
  let mut entry_fns: Vec<(Arc<str>, EntryPoint, EffectType)> = vec![];
  for f in program.abstract_functions_iter() {
    let f = f.read().unwrap();
    let Some(entry) = f.entry_point else {
      continue;
    };
    let FunctionImplementationKind::Composite(implementation) =
      &f.implementation
    else {
      continue;
    };
    let effects = implementation.read().unwrap().effects();
    if !matches!(entry, EntryPoint::Cpu | EntryPoint::Audio) {
      entry_fns.push((f.name.clone(), entry, effects.clone()));
    }
    let (reads, writes) = effects.gpu_read_and_written_globals();
    for name in reads.into_iter().chain(writes.into_iter()) {
      let stages = binding_stages.entry(name).or_default();
      match entry {
        EntryPoint::Vertex => stages.vertex = true,
        EntryPoint::Fragment => stages.fragment = true,
        EntryPoint::Compute(_) => stages.compute = true,
        EntryPoint::Cpu | EntryPoint::Audio => {}
      }
    }
  }
  entry_fns.sort_by(|(a, _, _), (b, _, _)| a.cmp(b));
  for (name, _, effects) in entry_fns {
    let (reads, writes) = effects.gpu_read_and_written_globals();
    let mut used_bindings: Vec<(u8, u8)> = vec![];
    for global in reads.into_iter().chain(writes.into_iter()) {
      if let Some(key) = group_and_binding_by_name.get(&global) {
        if !used_bindings.contains(key) {
          used_bindings.push(*key);
        }
      }
    }
    gpu_entry_ids.insert(name.clone(), gpu_entries.len() as u16);
    gpu_entries.push(GpuEntryInfo {
      name: name.replace('-', "_"),
      used_bindings,
    });
  }
  DerivedGpuInterface {
    binding_vars,
    binding_stages,
    gpu_entries,
    gpu_entry_ids,
  }
}

/// One frame of a `spawn-window` loop, abstracted over the executing
/// backend. The tree-walking interpreter and the bytecode-VM CPU runtime
/// each implement this; the per-IO-manager frame loops (real winit window,
/// StringIO's simulated frames, CaptureIO's headless frames) are written
/// once against this trait and work for both.
pub trait FrameDriver {
  type IO: IOManager;
  fn io_mut(&mut self) -> &mut Self::IO;
  fn wgsl(&self) -> &str;
  fn binding_infos(&self) -> Vec<GpuBindingInfo>;
  fn gpu_entries(&self) -> Vec<GpuEntryInfo>;
  /// Runs the frame body once. `Err(EvalException::CloseWindow)` stops the
  /// frame loop.
  fn run_frame(&mut self) -> Result<(), EvalException>;
  /// Overwrites the CPU-side value of the GPU-bound global at (`group`,
  /// `binding`) from raw GPU-layout bytes, marking it synced. For embedders
  /// whose IO manager owns a binding's GPU buffer and streams its live
  /// value into the running program each frame (easl_studio's sliders).
  /// Default: no-op.
  fn overwrite_binding_bytes(
    &mut self,
    _group: u8,
    _binding: u8,
    _bytes: &[u8],
  ) {
  }
}

/// Tree-walking-interpreter frame driver: evaluates the callback body once
/// per frame.
pub struct AstFrameDriver<'a, IO: IOManager> {
  pub body: Exp<ExpTypeInfo>,
  pub env: &'a mut EvaluationEnvironment<IO>,
}

impl<IO: IOManager> FrameDriver for AstFrameDriver<'_, IO> {
  type IO = IO;
  fn io_mut(&mut self) -> &mut IO {
    &mut self.env.io
  }
  fn wgsl(&self) -> &str {
    self.env.wgsl()
  }
  fn binding_infos(&self) -> Vec<GpuBindingInfo> {
    self.env.binding_infos()
  }
  fn gpu_entries(&self) -> Vec<GpuEntryInfo> {
    self.env.gpu_entries.clone()
  }
  fn run_frame(&mut self) -> Result<(), EvalException> {
    self.env.adopt_shared_globals();
    self.env.refresh_window_info_bindings();
    let result = eval(self.body.clone(), self.env).map(|_| ());
    // Publish on success and on close-window (the frame's writes are still
    // real); genuine errors abort the run, so skip the publish.
    if matches!(result, Ok(()) | Err(EvalException::CloseWindow)) {
      self.env.publish_shared_globals(0);
    }
    result
  }
  fn overwrite_binding_bytes(&mut self, group: u8, binding: u8, bytes: &[u8]) {
    self
      .env
      .overwrite_binding_from_gpu_bytes(group, binding, bytes);
  }
}

pub trait IOManager: Sized {
  fn println(&mut self, s: &str);
  /// `vert`/`frag` are dense GPU entry ids (see `GpuEntryInfo`); the name
  /// parameters carry the source-level names for event-log IO managers.
  fn record_draw(
    &mut self,
    vert: u16,
    frag: u16,
    vert_name: &str,
    frag_name: &str,
    vert_count: u32,
    pre_upload: Vec<((u8, u8), BufferUpload)>,
    additive: bool,
    render_target: Option<(u8, u8)>,
  ) -> Result<(), EvalError>;
  fn record_compute(
    &mut self,
    entry: u16,
    entry_name: &str,
    workgroup_count: (u32, u32, u32),
    pre_upload: Vec<((u8, u8), BufferUpload)>,
  ) -> Result<(), EvalError>;
  fn take_frame_draw_calls(&mut self) -> Vec<WindowEvent>;
  fn record_close_window(&mut self);
  /// Copy a GPU-written buffer back to CPU. Returns Some(bytes) on success,
  /// None if GPU readback is not available (e.g. in StringIO).
  fn sync_gpu_to_cpu(
    &mut self,
    group: u8,
    binding: u8,
    size: u64,
  ) -> Option<Vec<u8>>;
  /// Copy a GPU-written binding texture back to CPU as
  /// `(width, height, rgba8_pixels)`. Returns None when GPU readback isn't
  /// available or no texture exists at the binding. Used by `save-png` for
  /// textures the GPU has rendered into.
  fn sync_texture_to_cpu(
    &mut self,
    _group: u8,
    _binding: u8,
  ) -> Option<(u32, u32, Vec<u8>)> {
    None
  }
  /// Called after each implicit GPU→CPU readback with the synced variable's
  /// name. These blocking syncs are the most expensive implicit operation in
  /// the runtime, so test IO managers override this to record a log that
  /// tests can assert on — catching both spurious syncs and missing ones.
  /// Production implementations keep this default no-op.
  fn record_gpu_to_cpu_sync(&mut self, _name: &Arc<str>) {}
  /// Called for each implicit CPU→GPU upload of a dirty binding var with the
  /// uploaded variable's name (at collection time — the bytes reach the GPU
  /// when the dispatch/draw they're attached to executes). Like
  /// `record_gpu_to_cpu_sync`, test IO managers override this to assert on
  /// upload behavior; production implementations keep this default no-op.
  fn record_cpu_to_gpu_sync(&mut self, _name: &Arc<str>) {}
  /// Called when this thread publishes a thread-shared variable's snapshot
  /// at an iteration boundary. Test IO managers override this to pin
  /// exactly when cross-thread syncs happen (spurious publications are
  /// silent performance bugs); production implementations keep the no-op.
  fn record_shared_publish(&mut self, _name: &Arc<str>) {}
  /// Called when this thread adopts a newer published snapshot of a
  /// thread-shared variable at an iteration boundary. See
  /// `record_shared_publish`.
  fn record_shared_adopt(&mut self, _name: &Arc<str>) {}
  /// Returns the current window dimensions in pixels, or (1, 1) if no window
  /// is open.
  fn window_size(&self) -> (u32, u32) {
    (1, 1)
  }
  /// Returns the time in seconds since the window was opened, or 0.0 if no
  /// window is open.
  fn window_time(&self) -> f32 {
    0.0
  }
  /// Returns the time in seconds between the previous frame and the current
  /// frame, or 0.0 if no window is open.
  fn window_delta_time(&self) -> f32 {
    0.0
  }
  /// Returns the number of frames rendered since the window opened (0 on the
  /// first frame), or 0 if no window is open.
  fn window_frame_index(&self) -> u32 {
    0
  }
  /// Returns true if the given key is currently held down. `key` should be a
  /// lowercase single character like `"a"` or `"b"`. Always returns false
  /// outside of a real window (e.g. in tests).
  fn key_down(&self, _key: &str) -> bool {
    false
  }
  /// Returns true if the given key was pressed this frame (not held from a
  /// previous frame). Always returns false outside of a real window.
  fn key_just_down(&self, _key: &str) -> bool {
    false
  }
  /// Returns the pixel position of the mouse cursor relative to the window.
  /// Defaults to (0, 0) when outside a real window or before any mouse event.
  fn mouse_coords(&self) -> (u32, u32) {
    (0, 0)
  }
  /// Returns true if the mouse cursor is currently inside the window.
  /// Always returns false outside of a real window.
  fn mouse_present(&self) -> bool {
    false
  }
  /// Returns true if the left mouse button is currently held down.
  /// Always returns false outside of a real window.
  fn mouse_down(&self) -> bool {
    false
  }
  /// Returns true if the left mouse button was pressed this frame (not held
  /// from a previous frame). Always returns false outside of a real window.
  fn mouse_just_down(&self) -> bool {
    false
  }
  /// Returns the current GPU, if any. Used by `App::resumed` to detect an
  /// existing headless GPU so it can be reused rather than replaced.
  /// Default returns `None`; overridden by IO managers with real GPU access.
  #[cfg(feature = "window")]
  fn get_gpu(
    &self,
  ) -> Option<std::sync::Arc<std::sync::RwLock<crate::window::GpuCore>>> {
    None
  }
  /// Called after the wgpu device is created so compute dispatches can run
  /// synchronously. Default no-op for IO managers without real GPU access.
  #[cfg(feature = "window")]
  fn set_gpu(
    &mut self,
    _gpu: std::sync::Arc<std::sync::RwLock<crate::window::GpuCore>>,
  ) {
  }
  /// Called before a compute dispatch to lazily initialize a headless GPU
  /// context if no window-provided GPU is available yet. Default no-op.
  #[cfg(feature = "window")]
  fn ensure_gpu_ready(
    &mut self,
    _wgsl: &str,
    _binding_infos: &[GpuBindingInfo],
    _gpu_entries: &[GpuEntryInfo],
  ) {
  }
  /// Returns the current allocated byte size of the GPU buffer for the given
  /// binding. Used to determine readback size for dynamically-sized buffers
  /// (unsized arrays) whose size can't be derived from the type alone.
  /// Default returns `None`; overridden by IO managers with real GPU access.
  fn get_buffer_byte_size(&self, _group: u8, _binding: u8) -> Option<u64> {
    None
  }
  /// Returns true if a hot-reload has been requested (e.g. the source file
  /// changed). Checked by the window loop after each frame. Default: false.
  fn reload_requested(&self) -> bool {
    false
  }
  /// Called before re-running the program after a reload. Resets transient
  /// state (GPU handle, reload flag) so the new run starts clean.
  fn reset_for_reload(&mut self) {}
  /// Executes all queued compute events immediately, keeping render events
  /// deferred for end of frame. Called by `check_cpu_readable` when a
  /// CPU instruction needs to read a GPU-written variable mid-frame.
  /// Default no-op (used by StringIO and non-GPU paths).
  fn flush_queued_compute(&mut self) {}
  /// Returns a preferred window size and whether the window should steal
  /// focus.  Defaults to `None` (OS-default size, window takes focus).
  /// Override to create a small non-intrusive window during tests.
  fn preferred_window_hints(&self) -> Option<((u32, u32), bool)> {
    None
  }
  /// Runs the spawn-window event loop, invoking `driver.run_frame()` once
  /// per frame. Returns `true` if the loop exited because a hot-reload was
  /// requested, `false` for a normal exit.
  fn run_spawn_window_driver<D: FrameDriver<IO = Self>>(
    driver: &mut D,
  ) -> Result<bool, EvalError>;
  /// Start a background audio playback thread that calls the function named
  /// `entry_name` at the audio sample rate. The function must have signature
  /// `(t: f32, rate: f32) -> f32`. `source` carries the compiled artifact —
  /// either a bytecode program (default) or C source (with the `c_audio`
  /// feature) — and is consumed by the IO manager (typically moved onto the
  /// audio thread). `source` is `None` when the run was started without
  /// audio support; IO managers that do real audio playback should return
  /// `AudioSourceMissing` in that case, while testing managers (e.g.
  /// `StringIO`) may simply record the event.
  /// Default: not supported (returns an error). Overridden by IO managers
  /// that actually link to an audio backend.
  #[cfg(feature = "window")]
  fn start_audio(
    &mut self,
    _entry_name: &str,
    _source: Option<crate::audio::AudioSource>,
  ) -> Result<(), EvalError> {
    Err(
      UserspaceEvalError::AudioRuntimeError(
        "start-audio not supported by this IO manager".to_string(),
      )
      .into(),
    )
  }
  #[cfg(not(feature = "window"))]
  fn start_audio(&mut self, _entry_name: &str) -> Result<(), EvalError> {
    Err(WindowFeatureNotEnabled.into())
  }
  /// The sample rate the audio stream runs (or would run) at, readable
  /// before any stream exists — main-thread `(sample-rate)` calls report
  /// this (the audio thread's copy is written by the driver from the live
  /// stream instead). The default matches the fixed rate
  /// `build_audio_stream_batched` opens streams with; test managers
  /// override it with their harness rate.
  fn sample_rate(&self) -> f32 {
    44_100.
  }
}

pub struct StdoutIO {
  frame_draw_calls: Vec<WindowEvent>,
  #[cfg(feature = "window")]
  gpu: Option<std::sync::Arc<std::sync::RwLock<crate::window::GpuCore>>>,
  #[cfg(feature = "window")]
  reload_flag: Option<std::sync::Arc<std::sync::atomic::AtomicBool>>,
  /// When set, the window will be created at this size without taking focus.
  window_hints: Option<((u32, u32), bool)>,
}

impl StdoutIO {
  pub fn new() -> Self {
    Self {
      frame_draw_calls: vec![],
      #[cfg(feature = "window")]
      gpu: None,
      #[cfg(feature = "window")]
      reload_flag: None,
      window_hints: None,
    }
  }

  /// Creates a `StdoutIO` that will signal hot-reload readiness via `flag`.
  /// When `flag` is set to `true` the window loop exits and the program
  /// returns `ReloadRequested` so the caller can recompile and rerun.
  #[cfg(feature = "window")]
  pub fn with_reload_flag(
    flag: std::sync::Arc<std::sync::atomic::AtomicBool>,
  ) -> Self {
    Self {
      frame_draw_calls: vec![],
      gpu: None,
      reload_flag: Some(flag),
      window_hints: None,
    }
  }

  /// Sets window hints: a preferred size and whether the window should take
  /// focus.  Used by the test suite to create small non-intrusive windows.
  pub fn set_window_hints(&mut self, size: (u32, u32), activate: bool) {
    self.window_hints = Some((size, activate));
  }
}

impl IOManager for StdoutIO {
  fn println(&mut self, s: &str) {
    println!("{s}");
  }

  fn record_draw(
    &mut self,
    vert: u16,
    frag: u16,
    _vert_name: &str,
    _frag_name: &str,
    vert_count: u32,
    pre_upload: Vec<((u8, u8), BufferUpload)>,
    additive: bool,
    render_target: Option<(u8, u8)>,
  ) -> Result<(), EvalError> {
    self.frame_draw_calls.push(WindowEvent::RenderShaders {
      vert,
      frag,
      vert_count,
      pre_upload,
      additive,
      render_target,
    });
    Ok(())
  }

  fn record_compute(
    &mut self,
    entry: u16,
    _entry_name: &str,
    workgroup_count: (u32, u32, u32),
    pre_upload: Vec<((u8, u8), BufferUpload)>,
  ) -> Result<(), EvalError> {
    self.frame_draw_calls.push(WindowEvent::ComputeShader {
      entry,
      workgroup_count,
      pre_upload,
    });
    Ok(())
  }

  fn take_frame_draw_calls(&mut self) -> Vec<WindowEvent> {
    std::mem::take(&mut self.frame_draw_calls)
  }

  fn record_close_window(&mut self) {}

  fn sync_gpu_to_cpu(
    &mut self,
    #[allow(unused_variables)] group: u8,
    #[allow(unused_variables)] binding: u8,
    #[allow(unused_variables)] size: u64,
  ) -> Option<Vec<u8>> {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return Some(gpu.read().unwrap().read_buffer(group, binding, size));
    }
    None
  }

  fn sync_texture_to_cpu(
    &mut self,
    #[allow(unused_variables)] group: u8,
    #[allow(unused_variables)] binding: u8,
  ) -> Option<(u32, u32, Vec<u8>)> {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return gpu.read().unwrap().read_texture(group, binding);
    }
    None
  }

  #[cfg(feature = "window")]
  fn get_gpu(
    &self,
  ) -> Option<std::sync::Arc<std::sync::RwLock<crate::window::GpuCore>>> {
    self.gpu.clone()
  }

  #[cfg(feature = "window")]
  fn set_gpu(
    &mut self,
    gpu: std::sync::Arc<std::sync::RwLock<crate::window::GpuCore>>,
  ) {
    self.gpu = Some(gpu);
  }

  #[cfg(feature = "window")]
  fn ensure_gpu_ready(
    &mut self,
    wgsl: &str,
    binding_infos: &[GpuBindingInfo],
    gpu_entries: &[GpuEntryInfo],
  ) {
    if self.gpu.is_none() {
      // On hot-reload, reuse the GPU that's already in PERSISTENT_RELOAD_STATE
      // rather than creating a brand-new headless GPU.  Without this, a
      // pre-spawn-window compute dispatch (e.g. one-shot init shader) would run
      // on a throwaway GPU and its results would be lost when setup_window()
      // replaces self.gpu with the persistent one.
      if let Some(gpu) = crate::window::persistent_gpu() {
        gpu.write().unwrap().update_for_reload(
          wgsl,
          binding_infos,
          gpu_entries,
        );
        self.gpu = Some(gpu);
      } else {
        self.gpu = Some(crate::window::create_headless_gpu_core(
          wgsl,
          binding_infos,
          gpu_entries,
        ));
      }
    }
  }

  #[cfg(feature = "window")]
  fn get_buffer_byte_size(&self, group: u8, binding: u8) -> Option<u64> {
    self.gpu.as_ref().and_then(|gpu| {
      gpu
        .read()
        .unwrap()
        .binding_buffer_sizes
        .get(&(group, binding))
        .copied()
    })
  }

  fn window_size(&self) -> (u32, u32) {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return gpu.read().unwrap().window_size;
    }
    (1, 1)
  }

  fn window_time(&self) -> f32 {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return gpu.read().unwrap().window_time;
    }
    0.0
  }

  fn window_delta_time(&self) -> f32 {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return gpu.read().unwrap().window_delta_time;
    }
    0.0
  }

  fn window_frame_index(&self) -> u32 {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return gpu.read().unwrap().window_frame_index;
    }
    0
  }

  fn key_down(&self, _key: &str) -> bool {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return gpu.read().unwrap().keys_down.contains(_key);
    }
    false
  }

  fn key_just_down(&self, _key: &str) -> bool {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return gpu.read().unwrap().keys_just_down.contains(_key);
    }
    false
  }

  fn mouse_coords(&self) -> (u32, u32) {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return gpu.read().unwrap().mouse_coords;
    }
    (0, 0)
  }

  fn mouse_present(&self) -> bool {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return gpu.read().unwrap().mouse_present;
    }
    false
  }

  fn mouse_down(&self) -> bool {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return gpu.read().unwrap().mouse_down;
    }
    false
  }

  fn mouse_just_down(&self) -> bool {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return gpu.read().unwrap().mouse_just_down;
    }
    false
  }

  fn reload_requested(&self) -> bool {
    #[cfg(feature = "window")]
    if let Some(flag) = &self.reload_flag {
      return flag.load(std::sync::atomic::Ordering::Relaxed);
    }
    false
  }

  fn preferred_window_hints(&self) -> Option<((u32, u32), bool)> {
    self.window_hints
  }

  fn reset_for_reload(&mut self) {
    #[cfg(feature = "window")]
    {
      self.gpu = None;
      if let Some(flag) = &self.reload_flag {
        flag.store(false, std::sync::atomic::Ordering::Relaxed);
      }
    }
  }

  fn flush_queued_compute(&mut self) {
    #[cfg(feature = "window")]
    {
      let gpu = match self.gpu.as_ref().map(std::sync::Arc::clone) {
        Some(gpu) => gpu,
        None => return,
      };
      let all = std::mem::take(&mut self.frame_draw_calls);
      if all.is_empty() {
        return;
      }
      // Execute all queued texture-targeted work — compute dispatches and
      // render-to-texture passes — in program order, through the same
      // implementation the end-of-frame path uses, then block until it
      // completes so the caller can read results back. Screen renders
      // execute afterwards (nothing on the GPU can read the surface, so
      // their placement is unobservable): on the real window path they
      // render into the acquired surface texture, saved as
      // `pending_present` so end-of-frame can just present it. Their
      // pre_uploads were already applied by `execute_frame_gpu_work`, so
      // they are passed along without uploads (re-applying would overwrite
      // GPU output).
      let mut g = gpu.write().unwrap();
      g.execute_frame_gpu_work(&all);
      g.wait_idle();
      let screen_renders: Vec<_> = all
        .into_iter()
        .filter_map(|event| match event {
          WindowEvent::RenderShaders {
            vert,
            frag,
            vert_count,
            pre_upload: _,
            additive,
            render_target: None,
          } => Some((vert, frag, vert_count, vec![], additive, None)),
          _ => None,
        })
        .collect();
      g.execute_render_batch(screen_renders);
    }
  }

  fn run_spawn_window_driver<D: FrameDriver<IO = Self>>(
    driver: &mut D,
  ) -> Result<bool, EvalError> {
    #[cfg(feature = "window")]
    return crate::window::run_window_loop(driver);
    #[cfg(not(feature = "window"))]
    {
      let _ = driver;
      Err(WindowFeatureNotEnabled.into())
    }
  }

  #[cfg(feature = "window")]
  fn start_audio(
    &mut self,
    entry_name: &str,
    source: Option<crate::audio::AudioSource>,
  ) -> Result<(), EvalError> {
    let Some(source) = source else {
      // Repeated call: the audio source was already consumed on the first
      // call. If a stream is actually running, re-point it at `entry_name`
      // — a cheap no-op when the name is unchanged (a typical program puts
      // `start-audio` inside a frame callback), a live entry switch when
      // it isn't. If no stream is running, the run was started without
      // audio support in the first place.
      return if crate::audio::is_audio_thread_started() {
        crate::audio::switch_vm_audio_entry(entry_name)
          .map_err(|e| UserspaceEvalError::AudioRuntimeError(e).into())
      } else {
        Err(UserspaceEvalError::AudioSourceMissing.into())
      };
    };
    match source {
      crate::audio::AudioSource::Bytecode {
        program,
        function_names,
        shared_table,
      } => crate::audio::start_audio_thread_vm(
        entry_name,
        program,
        function_names,
        shared_table,
      )
      .map_err(|e| UserspaceEvalError::AudioRuntimeError(e).into()),
      crate::audio::AudioSource::C(c_source) => {
        crate::audio::start_audio_thread_c(entry_name, &c_source)
          .map_err(|e| UserspaceEvalError::AudioRuntimeError(e).into())
      }
    }
  }
  #[cfg(not(feature = "window"))]
  fn start_audio(&mut self, _entry_name: &str) -> Result<(), EvalError> {
    Err(WindowFeatureNotEnabled.into())
  }
}

/// Test/debug IO manager. Records all IO events in a single ordered log and
/// simulates the window loop by running the frame callback `frame_count` times.
pub struct StringIO {
  pub events: Vec<IOEvent>,
  pub frame_count: usize,
  /// Tracks which frame is currently being evaluated (0-indexed). Used to
  /// return deterministic values from `window_time` and `window_delta_time`.
  pub frame_index: usize,
}

impl Default for StringIO {
  fn default() -> Self {
    Self {
      events: vec![],
      frame_count: 10,
      frame_index: 0,
    }
  }
}

impl StringIO {
  pub fn new() -> Self {
    Self::default()
  }
}

impl IOManager for StringIO {
  fn println(&mut self, s: &str) {
    self.events.push(IOEvent::Print(s.to_string()));
  }

  /// Matches the thread-sync test harness's rate, so `t` values and
  /// rate-derived math stay exactly representable in goldens.
  fn sample_rate(&self) -> f32 {
    8.
  }

  fn record_draw(
    &mut self,
    _vert: u16,
    _frag: u16,
    vert_name: &str,
    frag_name: &str,
    vert_count: u32,
    _pre_upload: Vec<((u8, u8), BufferUpload)>,
    _additive: bool,
    _render_target: Option<(u8, u8)>,
  ) -> Result<(), EvalError> {
    self.events.push(IOEvent::DispatchShaders {
      vert: vert_name.to_string(),
      frag: frag_name.to_string(),
      vert_count,
    });
    Ok(())
  }

  fn record_compute(
    &mut self,
    _entry: u16,
    entry_name: &str,
    workgroup_count: (u32, u32, u32),
    _pre_upload: Vec<((u8, u8), BufferUpload)>,
  ) -> Result<(), EvalError> {
    self.events.push(IOEvent::DispatchComputeShader {
      entry: entry_name.to_string(),
      workgroup_count,
    });
    Ok(())
  }

  fn take_frame_draw_calls(&mut self) -> Vec<WindowEvent> {
    vec![] // Events already logged directly; no rendering needed for StringIO
  }

  fn record_close_window(&mut self) {
    self.events.push(IOEvent::CloseWindow);
  }

  fn sync_gpu_to_cpu(
    &mut self,
    _group: u8,
    _binding: u8,
    _size: u64,
  ) -> Option<Vec<u8>> {
    None
  }

  fn window_size(&self) -> (u32, u32) {
    (800, 600)
  }

  fn window_time(&self) -> f32 {
    self.frame_index as f32 / 60.0
  }

  fn window_delta_time(&self) -> f32 {
    if self.frame_index == 0 {
      0.0
    } else {
      1.0 / 60.0
    }
  }

  fn window_frame_index(&self) -> u32 {
    self.frame_index as u32
  }

  fn run_spawn_window_driver<D: FrameDriver<IO = Self>>(
    driver: &mut D,
  ) -> Result<bool, EvalError> {
    driver.io_mut().events.push(IOEvent::SpawnWindow);
    let frame_count = driver.io_mut().frame_count;
    for i in 0..frame_count {
      driver.io_mut().frame_index = i;
      match driver.run_frame() {
        Ok(_) => {}
        Err(EvalException::CloseWindow) => break,
        Err(e) => return Err(e.into()),
      }
    }
    Ok(false)
  }

  #[cfg(feature = "window")]
  fn start_audio(
    &mut self,
    entry_name: &str,
    _source: Option<crate::audio::AudioSource>,
  ) -> Result<(), EvalError> {
    self.events.push(IOEvent::StartAudio {
      entry: entry_name.to_string(),
    });
    Ok(())
  }
  #[cfg(not(feature = "window"))]
  fn start_audio(&mut self, entry_name: &str) -> Result<(), EvalError> {
    self.events.push(IOEvent::StartAudio {
      entry: entry_name.to_string(),
    });
    Ok(())
  }
}

/// IO manager for buffer/GPU integration tests: does real GPU dispatches
/// (same as `StdoutIO`) but also captures all `print` calls into a
/// `Vec<String>`. All behavior is delegated to the inner `StdoutIO`.
pub struct CaptureIO {
  pub prints: Vec<String>,
  /// Ordered trace of implicit GPU↔CPU transfers interleaved with prints:
  /// `upload: <name>` (CPU→GPU, via `record_cpu_to_gpu_sync`),
  /// `readback: <name>` (GPU→CPU, via `record_gpu_to_cpu_sync`), and
  /// `print: <text>` lines. Sync-behavior tests golden-match this against a
  /// `.sync.txt` file to assert exactly when transfers happen.
  pub sync_trace: Vec<String>,
  pub inner: StdoutIO,
  /// Test hook: when set, the window-info accessors report these values
  /// instead of deferring to the real window state. Lives here rather than
  /// on `StdoutIO` so production accessors stay branch-free — CaptureIO is
  /// the test/capture wrapper.
  pub spoofed_window_info: Option<SpoofedWindowInfo>,
}

impl CaptureIO {
  pub fn new() -> Self {
    Self {
      prints: vec![],
      sync_trace: vec![],
      inner: StdoutIO::new(),
      spoofed_window_info: None,
    }
  }
}

impl IOManager for CaptureIO {
  fn println(&mut self, s: &str) {
    self.prints.push(s.to_string());
    self.sync_trace.push(format!("print: {s}"));
    self.inner.println(s);
  }

  fn record_gpu_to_cpu_sync(&mut self, name: &Arc<str>) {
    self.sync_trace.push(format!("readback: {name}"));
  }

  fn record_cpu_to_gpu_sync(&mut self, name: &Arc<str>) {
    self.sync_trace.push(format!("upload: {name}"));
  }

  fn record_draw(
    &mut self,
    vert: u16,
    frag: u16,
    vert_name: &str,
    frag_name: &str,
    vert_count: u32,
    pre_upload: Vec<((u8, u8), BufferUpload)>,
    additive: bool,
    render_target: Option<(u8, u8)>,
  ) -> Result<(), EvalError> {
    self.inner.record_draw(
      vert,
      frag,
      vert_name,
      frag_name,
      vert_count,
      pre_upload,
      additive,
      render_target,
    )
  }

  fn record_compute(
    &mut self,
    entry: u16,
    entry_name: &str,
    workgroup_count: (u32, u32, u32),
    pre_upload: Vec<((u8, u8), BufferUpload)>,
  ) -> Result<(), EvalError> {
    self
      .inner
      .record_compute(entry, entry_name, workgroup_count, pre_upload)
  }

  fn take_frame_draw_calls(&mut self) -> Vec<WindowEvent> {
    self.inner.take_frame_draw_calls()
  }

  fn record_close_window(&mut self) {
    self.inner.record_close_window()
  }

  fn sync_gpu_to_cpu(
    &mut self,
    group: u8,
    binding: u8,
    size: u64,
  ) -> Option<Vec<u8>> {
    self.inner.sync_gpu_to_cpu(group, binding, size)
  }

  fn sync_texture_to_cpu(
    &mut self,
    group: u8,
    binding: u8,
  ) -> Option<(u32, u32, Vec<u8>)> {
    self.inner.sync_texture_to_cpu(group, binding)
  }

  #[cfg(feature = "window")]
  fn get_gpu(
    &self,
  ) -> Option<std::sync::Arc<std::sync::RwLock<crate::window::GpuCore>>> {
    self.inner.get_gpu()
  }

  #[cfg(feature = "window")]
  fn set_gpu(
    &mut self,
    gpu: std::sync::Arc<std::sync::RwLock<crate::window::GpuCore>>,
  ) {
    self.inner.set_gpu(gpu)
  }

  #[cfg(feature = "window")]
  fn ensure_gpu_ready(
    &mut self,
    wgsl: &str,
    binding_infos: &[GpuBindingInfo],
    gpu_entries: &[GpuEntryInfo],
  ) {
    self
      .inner
      .ensure_gpu_ready(wgsl, binding_infos, gpu_entries)
  }

  #[cfg(feature = "window")]
  fn get_buffer_byte_size(&self, group: u8, binding: u8) -> Option<u64> {
    self.inner.get_buffer_byte_size(group, binding)
  }

  fn window_size(&self) -> (u32, u32) {
    if let Some(spoof) = &self.spoofed_window_info {
      return spoof.size;
    }
    self.inner.window_size()
  }

  fn window_time(&self) -> f32 {
    if let Some(spoof) = &self.spoofed_window_info {
      return spoof.time;
    }
    self.inner.window_time()
  }

  fn window_delta_time(&self) -> f32 {
    if let Some(spoof) = &self.spoofed_window_info {
      return spoof.delta_time;
    }
    self.inner.window_delta_time()
  }

  fn window_frame_index(&self) -> u32 {
    if let Some(spoof) = &self.spoofed_window_info {
      return spoof.frame_index;
    }
    self.inner.window_frame_index()
  }

  fn key_down(&self, key: &str) -> bool {
    if let Some(spoof) = &self.spoofed_window_info {
      return spoof.keys_down.iter().any(|k| k == key);
    }
    self.inner.key_down(key)
  }

  fn key_just_down(&self, key: &str) -> bool {
    if let Some(spoof) = &self.spoofed_window_info {
      return spoof.keys_just_down.iter().any(|k| k == key);
    }
    self.inner.key_just_down(key)
  }

  fn mouse_coords(&self) -> (u32, u32) {
    if let Some(spoof) = &self.spoofed_window_info {
      return spoof.mouse_coords;
    }
    self.inner.mouse_coords()
  }

  fn mouse_present(&self) -> bool {
    if let Some(spoof) = &self.spoofed_window_info {
      return spoof.mouse_present;
    }
    self.inner.mouse_present()
  }

  fn mouse_down(&self) -> bool {
    if let Some(spoof) = &self.spoofed_window_info {
      return spoof.mouse_down;
    }
    self.inner.mouse_down()
  }

  fn mouse_just_down(&self) -> bool {
    if let Some(spoof) = &self.spoofed_window_info {
      return spoof.mouse_just_down;
    }
    self.inner.mouse_just_down()
  }

  fn flush_queued_compute(&mut self) {
    self.inner.flush_queued_compute();
  }

  fn run_spawn_window_driver<D: FrameDriver<IO = Self>>(
    driver: &mut D,
  ) -> Result<bool, EvalError> {
    loop {
      match driver.run_frame() {
        Ok(_) => {}
        Err(EvalException::CloseWindow) => break,
        Err(e) => return Err(e.into()),
      }
      // Execute the frame's remaining queued events through the same frame
      // path the real winit loop uses (`GpuCore::execute_frame_gpu_work` +
      // `execute_frame_screen_renders`), so that tests exercise production
      // behavior. Screen-targeted renders are skipped — there's no surface
      // in headless mode.
      #[cfg(feature = "window")]
      {
        let events = driver.io_mut().take_frame_draw_calls();
        if !events.is_empty()
          && let Some(gpu) = driver.io_mut().get_gpu()
        {
          let mut gpu = gpu.write().unwrap();
          gpu.execute_frame_gpu_work(&events);
          gpu.execute_frame_screen_renders(&events, None);
        }
      }
      #[cfg(not(feature = "window"))]
      {
        driver.io_mut().flush_queued_compute();
        let _ = driver.io_mut().take_frame_draw_calls();
      }
    }
    Ok(false)
  }

  #[cfg(feature = "window")]
  fn start_audio(
    &mut self,
    entry_name: &str,
    source: Option<crate::audio::AudioSource>,
  ) -> Result<(), EvalError> {
    self.inner.start_audio(entry_name, source)
  }
  #[cfg(not(feature = "window"))]
  fn start_audio(&mut self, entry_name: &str) -> Result<(), EvalError> {
    self.inner.start_audio(entry_name)
  }
}

pub struct EvaluationEnvironment<IO: IOManager> {
  bindings: HashMap<Arc<str>, Vec<(Value, Type)>>,
  structs: HashMap<Arc<str>, AbstractStruct>,
  wgsl: String,
  pub io: IO,
  /// GPU-bound top-level vars (uniform + storage), in declaration order.
  binding_vars: Vec<(GroupAndBinding, Arc<str>, Type, VariableAddressSpace)>,
  /// Which shader stages reference each GPU-bound var, derived from entry
  /// point effects at construction.
  binding_stages: HashMap<Arc<str>, BindingStages>,
  /// Dense GPU entry table (see `GpuEntryInfo`); ids index this vec.
  gpu_entries: Vec<GpuEntryInfo>,
  /// Source-level entry name -> dense id into `gpu_entries`.
  gpu_entry_ids: HashMap<Arc<str>, u16>,
  /// Implicit uniform vars for window-info queries, refreshed from the IO
  /// manager at the start of each frame. The type is carried here (rather
  /// than looked up in `binding_vars`) because a query used only by CPU
  /// code is not GPU-used and so has no binding — its env value still
  /// needs the per-frame refresh.
  window_info_bindings: Vec<(WindowInfoBindingSource, Arc<str>, Type)>,
  /// Sync state for each GPU-bound variable, keyed by name.
  buffer_states: HashMap<Arc<str>, SharedBufferState>,
  /// Directory of the source .easl file, used to resolve relative paths.
  source_dir: Option<PathBuf>,
  /// Render target for subsequent `dispatch-render-shaders` calls. None means
  /// render to the screen. Set by `set-render-target`, cleared by
  /// `clear-render-target`.
  current_render_target: Option<GroupAndBinding>,
  /// Pre-compiled audio source for `start-audio`. Required for the
  /// `start-audio` builtin to actually start a stream; `None` means the run
  /// was started without audio support. Consumed (via `take`) on first
  /// `start-audio` invocation; later calls pass `None` and the IO manager
  /// decides what to do (noop if a stream is already running, error if not).
  #[cfg(feature = "window")]
  audio_source: Option<crate::audio::AudioSource>,
  /// Scope-struct names of audio closures whose lifted capture globals
  /// have already been seeded, so each closure's state is handed off
  /// exactly once (the tree-walker sibling of the VM's per-call-site
  /// one-shot seed guard).
  #[cfg(feature = "window")]
  /// Lifted-capture records by entry fn name, copied from the validated
  /// `Program` — the seed walkers read global names from these instead of
  /// reconstructing them (see `LiftedCaptures`).
  #[cfg(feature = "window")]
  lifted_audio_captures: HashMap<Arc<str>, Arc<LiftedCaptures>>,
  lifted_gpu_captures: HashMap<Arc<str>, Arc<LiftedCaptures>>,
  /// Thread-shared globals (see `Program::thread_shared_globals`) with
  /// their audience masks, sorted by name; index-aligned with
  /// `shared_table` slots and every compiled artifact's
  /// `Code::shared_vars`.
  shared_globals: Vec<(Arc<str>, Type, u32)>,
  /// The cross-thread coordination table for this run. Handed to the audio
  /// thread at `start-audio`.
  shared_table: Arc<crate::thread_sync::ThreadSharedTable>,
  /// Per-shared-variable "written since my last publish" flags for the
  /// tree-walker's replica (the env `Value`s). The VM runtime's replica
  /// tracks its own flags on the `BytecodeProgram`.
  shared_dirty: Vec<bool>,
  /// Per-shared-variable version last adopted, for the tree-walker replica.
  shared_adopted: Vec<u64>,
  /// Name → shared index, for the write-marking hook.
  shared_indices: HashMap<Arc<str>, usize>,
}

impl<IO: IOManager> EvaluationEnvironment<IO> {
  pub fn from_program(
    program: Program,
    io: IO,
    source_dir: Option<PathBuf>,
  ) -> Result<Self, EvalError> {
    #[cfg(feature = "window")]
    return Self::from_program_with_audio_source(program, io, source_dir, None);
    #[cfg(not(feature = "window"))]
    return Self::build_inner(program, io, source_dir, None);
  }
  #[cfg(feature = "window")]
  pub fn from_program_with_audio_source(
    program: Program,
    io: IO,
    source_dir: Option<PathBuf>,
    audio_source: Option<crate::audio::AudioSource>,
  ) -> Result<Self, EvalError> {
    Self::build_inner(program, io, source_dir, audio_source, None)
  }
  #[cfg(feature = "window")]
  pub fn from_program_with_audio_source_and_external(
    program: Program,
    io: IO,
    source_dir: Option<PathBuf>,
    audio_source: Option<crate::audio::AudioSource>,
    external_vars: Option<Arc<ExternalVars>>,
  ) -> Result<Self, EvalError> {
    Self::build_inner(program, io, source_dir, audio_source, external_vars)
  }
  fn build_inner(
    program: Program,
    io: IO,
    source_dir: Option<PathBuf>,
    #[cfg(feature = "window")] audio_source: Option<crate::audio::AudioSource>,
    external_vars: Option<Arc<ExternalVars>>,
  ) -> Result<Self, EvalError> {
    let DerivedGpuInterface {
      binding_vars,
      binding_stages,
      gpu_entries,
      gpu_entry_ids,
    } = derive_gpu_interface(&program);
    let shared_globals: Vec<(Arc<str>, Type, u32)> = program
      .thread_shared_globals()
      .into_iter()
      .map(|(name, audience)| {
        let ty = program
          .top_level_vars
          .iter()
          .find(|v| v.name == name)
          .expect("thread-shared global missing from top-level vars")
          .var_type
          .clone();
        (name, ty, audience)
      })
      .collect();
    let shared_indices: HashMap<Arc<str>, usize> = shared_globals
      .iter()
      .enumerate()
      .map(|(index, (name, _, _))| (name.clone(), index))
      .collect();
    let shared_table = external_vars
      .as_ref()
      .map(|handle| handle.table_for_env(shared_globals.len()))
      .unwrap_or_else(|| {
        Arc::new(crate::thread_sync::ThreadSharedTable::new(
          shared_globals.len(),
        ))
      });
    let buffer_states = binding_vars
      .iter()
      .map(|(_, name, _, _)| (name.clone(), SharedBufferState::GPUOutOfDate))
      .collect();
    let mut env = Self {
      wgsl: String::new(),
      bindings: HashMap::new(),
      structs: program
        .typedefs
        .structs
        .iter()
        .map(|s| (s.name.0.clone(), (&*s).clone()))
        .collect(),
      io,
      binding_vars,
      binding_stages,
      gpu_entries,
      gpu_entry_ids,
      window_info_bindings: program
        .window_info_bindings
        .iter()
        .map(|(source, name)| {
          let ty = program
            .top_level_vars
            .iter()
            .find(|v| v.name == *name)
            .map(|v| v.var_type.clone())
            .expect("window-info binding without a top-level var");
          (source.clone(), name.clone(), ty)
        })
        .collect(),
      buffer_states,
      source_dir,
      current_render_target: None,
      #[cfg(feature = "window")]
      audio_source,
      #[cfg(feature = "window")]
      #[cfg(feature = "window")]
      lifted_audio_captures: program.lifted_audio_captures.clone(),
      lifted_gpu_captures: program.lifted_gpu_captures.clone(),
      shared_dirty: vec![false; shared_globals.len()],
      shared_adopted: vec![0; shared_globals.len()],
      shared_globals,
      shared_table,
      shared_indices,
    };
    for var in program.top_level_vars.iter() {
      let value = match &var.value {
        Some(exp) => eval(exp.clone(), &mut env)?,
        None => {
          // Texture (Handle) bindings must be loaded via load-image; start
          // Uninitialized so no spurious zeroed struct is created for them.
          let is_handle = matches!(
            var.kind,
            TopLevelVariableKind::Var {
              address_space: VariableAddressSpace::Handle,
              ..
            }
          );
          if is_handle {
            Value::Uninitialized
          } else {
            // Unsized arrays and other unzeroable types fall back to
            // Uninitialized; the user must assign before use.
            Value::zeroed(var.var_type.clone(), &env)
              .unwrap_or(Value::Uninitialized)
          }
        }
      };
      env.bind(var.name.clone(), value, var.var_type.clone());
    }
    let mut unit_variant_names: HashSet<Arc<str>> = HashSet::new();
    for e in program.typedefs.enums.iter() {
      for v in e.variants.iter() {
        if v.inner_type == AbstractType::Type(Type::Unit) {
          unit_variant_names.insert(v.name.clone());
          env.bind(
            v.name.clone(),
            Value::Enum(v.name.clone(), Value::Unit.into()),
            Type::Unit, // enum unit constructors are never ZeroedArray
          );
        }
      }
    }
    // Generic enums' unit variants are referenced by monomorphized constant
    // names (e.g. `None_Option_f32`) — bind each such alias to the same
    // base-named value, so both value uses and match-pattern comparisons
    // (which evaluate the pattern name) resolve. The value keeps the base
    // variant name, matching how data-variant patterns compare via their
    // `EnumConstructor` ancestors' base names.
    for (monomorphized, base) in
      program.names.read().unwrap().monomorphized_to_base_names()
    {
      if unit_variant_names.contains(&base) {
        env.bind(
          monomorphized,
          Value::Enum(base, Value::Unit.into()),
          Type::Unit,
        );
      }
    }
    // Initial window-info values: dispatches that happen outside a frame
    // loop should see the IO manager's defaults rather than zeros.
    env.refresh_window_info_bindings();
    // Main's copy of the implicit `easl_sample_rate` local (see
    // `Program::extract_audio_info`): the stream rate is fixed and
    // knowable before any stream exists, so main-thread `(sample-rate)`
    // calls — including closure constructors sizing delay buffers during
    // `start-audio` argument evaluation — read a real value. The audio
    // thread's own copy is written by the driver instead.
    let main_sample_rate = env.io.sample_rate();
    if let Some(slot) = env
      .bindings
      .get_mut("easl_sample_rate")
      .and_then(|v| v.last_mut())
    {
      slot.0 = Value::Prim(Primitive::F32(main_sample_rate));
    }
    let wgsl = program.compile_to_target(CompilerTarget::WGSL)?;
    env.wgsl = wgsl;
    Ok(env)
  }
  pub fn wgsl(&self) -> &str {
    &self.wgsl
  }
  /// Refreshes the implicit window-info uniform bindings from the IO
  /// manager and marks them CPU-written, so the next GPU dispatch uploads
  /// the fresh values. Called at the start of every frame (and once at
  /// environment setup, so dispatches outside a frame loop see the IO
  /// manager's defaults rather than zeros).
  pub fn refresh_window_info_bindings(&mut self) {
    if self.window_info_bindings.is_empty() {
      return;
    }
    let infos = self.window_info_bindings.clone();
    for (source, name, ty) in infos {
      let words = window_info_words(&source, &self.io);
      let value = Value::from_vm_words(&ty, &words);
      if let Some(binding) =
        self.bindings.get_mut(&name).and_then(|v| v.last_mut())
      {
        binding.0 = value;
      }
      self.mark_cpu_written(&[name]);
    }
  }
  /// Resolves a source-level GPU entry point name to its dense entry id.
  fn gpu_entry_id(&self, name: &str) -> u16 {
    *self.gpu_entry_ids.get(name).unwrap_or_else(|| {
      panic!(
        "easl internal error: dispatched entry point \"{name}\" is missing \
         from the GPU entry table"
      )
    })
  }

  /// Returns a `GpuBindingInfo` for each GPU-bound variable. `byte_size` is
  /// 0 for dynamically-sized (unsized array) bindings.
  pub fn binding_infos(&self) -> Vec<GpuBindingInfo> {
    gpu_binding_infos_from(&self.binding_vars, &self.binding_stages)
  }

  /// Returns the current byte representation of each GPU-bound variable,
  /// padded to a 16-byte multiple.
  pub fn binding_buffer_data(&self) -> Vec<((u8, u8), BufferUpload)> {
    self
      .binding_vars
      .iter()
      .filter_map(|(gb, name, ty, addr)| {
        let value = self
          .bindings
          .get(name)
          .and_then(|v| v.last())
          .map(|(v, _)| v);
        let upload = match value {
          Some(Value::ZeroedArray { length }) => {
            let Type::Array(_, inner_type_info) = ty else {
              panic!()
            };
            let inner_ty = inner_type_info.unwrap_known();
            let elem_size = inner_ty.wgsl_flat_data_size_in_u32s();
            let align = inner_ty.wgsl_alignment_in_u32s();
            let stride = ((elem_size + align - 1) / align) * align;
            let raw_bytes =
              (*length as u64 * stride as u64 * 4).max(stride as u64 * 4);
            let padded = ((raw_bytes + 15) / 16) * 16;
            BufferUpload::Clear { byte_count: padded }
          }
          Some(Value::Texture {
            width,
            height,
            data,
            ..
          }) => BufferUpload::TextureData {
            width: *width,
            height: *height,
            data: data.clone(),
          },
          _ if *addr == VariableAddressSpace::Handle => {
            // Uninitialized texture — skip; placeholder texture is used on GPU.
            return None;
          }
          _ => {
            let mut bytes = value
              .map(|v| v.to_uniform_bytes(ty))
              .unwrap_or(vec![0u8; 4]);
            while bytes.len() % 16 != 0 {
              bytes.push(0);
            }
            BufferUpload::Data(bytes)
          }
        };
        Some(((gb.group, gb.binding), upload))
      })
      .collect()
  }

  fn is_binding_var(&self, name: &Arc<str>) -> bool {
    self.binding_vars.iter().any(|(_, n, _, _)| n == name)
  }

  /// If a dispatched shader entry is a closure with a captured scope, write
  /// the scope struct into the implicit storage binding created by
  /// `Program::extract_dispatched_closure_scopes` (named
  /// `<scope-struct-name>_data`) and mark it CPU-written, so the dirty-upload
  /// machinery ships the captured values to the GPU with the dispatch.
  fn upload_dispatched_closure_scope(
    &mut self,
    dispatched_value: &Value,
    dispatched_f: &FunctionSignature,
  ) {
    let Value::Fun(Function::Scoped { scope, .. }) = dispatched_value else {
      return;
    };
    let Some(ancestor) = &dispatched_f.abstract_ancestor else {
      return;
    };
    let Some(scope_struct) = ancestor.read().unwrap().captured_scope.clone()
    else {
      return;
    };
    // Each captured var has its own binding, and captured closures
    // recurse into their own scope's bindings (see
    // `extract_dispatched_closure_scopes`).
    let entry_name = ancestor.read().unwrap().name.clone();
    let captures = self
      .lifted_gpu_captures
      .get(&entry_name)
      .unwrap_or_else(|| {
        panic!(
          "compiler bug: no lifted-capture record for dispatched entry \
           `{entry_name}`"
        )
      })
      .clone();
    let mut written: Vec<Arc<str>> = vec![];
    let scope_value = (**scope).clone();
    self.write_scope_capture_bindings(
      &scope_struct,
      &scope_value,
      &captures,
      &mut written,
    );
    self.mark_cpu_written(&written);
  }
  /// Writes a `start-audio`d closure's captured scope values into the
  /// lifted `<scope>_audio_data_<capture>` globals (see
  /// `extract_audio_closure_scopes`), recursing into captured closures'
  /// scopes. The audio-scope sibling of `write_scope_capture_bindings`.
  #[cfg(feature = "window")]
  fn seed_audio_scope_globals(
    &mut self,
    scope_struct: &AbstractStruct,
    scope_value: &Value,
    captures: &LiftedCaptures,
  ) {
    let Value::Struct(scope_fields) = scope_value else {
      return;
    };
    assert_eq!(
      scope_struct.fields.len(),
      captures.fields.len(),
      "lifted-capture record misaligned with scope struct"
    );
    let mut written: Vec<Arc<str>> = vec![];
    for (field, capture) in
      scope_struct.fields.iter().zip(captures.fields.iter())
    {
      let Some(field_value) = scope_fields.get(&field.name) else {
        continue;
      };
      match capture {
        LiftedCapture::Closure(nested_captures) => {
          let AbstractType::Type(Type::Function(signature)) = &field.field_type
          else {
            panic!("closure capture record on a non-function field")
          };
          let nested_struct = signature
            .abstract_ancestor
            .as_ref()
            .and_then(|a| a.read().unwrap().captured_scope.clone())
            .expect("captured closure without a scope struct");
          // A captured closure's value is its own scope data.
          let inner = match field_value {
            Value::Fun(Function::Scoped { scope, .. }) => (**scope).clone(),
            other => other.clone(),
          };
          self.seed_audio_scope_globals(
            &nested_struct,
            &inner,
            nested_captures,
          );
        }
        LiftedCapture::Global(global_name) => {
          let field_value = field_value.clone();
          if let Some(bindings) = self.bindings.get_mut(global_name)
            && let Some((value, _)) = bindings.last_mut()
          {
            *value = field_value;
            written.push(global_name.clone());
          }
        }
      }
    }
    self.mark_cpu_written(&written);
  }
  fn write_scope_capture_bindings(
    &mut self,
    scope_struct: &AbstractStruct,
    scope_value: &Value,
    captures: &LiftedCaptures,
    written: &mut Vec<Arc<str>>,
  ) {
    let Value::Struct(scope_fields) = scope_value else {
      return;
    };
    assert_eq!(
      scope_struct.fields.len(),
      captures.fields.len(),
      "lifted-capture record misaligned with scope struct"
    );
    for (field, capture) in
      scope_struct.fields.iter().zip(captures.fields.iter())
    {
      let Some(field_value) = scope_fields.get(&field.name) else {
        continue;
      };
      match capture {
        LiftedCapture::Closure(nested_captures) => {
          let AbstractType::Type(Type::Function(signature)) = &field.field_type
          else {
            panic!("closure capture record on a non-function field")
          };
          let nested_struct = signature
            .abstract_ancestor
            .as_ref()
            .and_then(|a| a.read().unwrap().captured_scope.clone())
            .expect("captured closure without a scope struct");
          // A captured closure's value is its own scope data.
          let inner = match field_value {
            Value::Fun(Function::Scoped { scope, .. }) => (**scope).clone(),
            other => other.clone(),
          };
          self.write_scope_capture_bindings(
            &nested_struct,
            &inner,
            nested_captures,
            written,
          );
        }
        LiftedCapture::Global(global_name) => {
          if !self.is_binding_var(global_name) {
            continue;
          }
          let field_value = field_value.clone();
          if let Some(bindings) = self.bindings.get_mut(global_name)
            && let Some((value, _)) = bindings.last_mut()
          {
            *value = field_value;
          }
          written.push(global_name.clone());
        }
      }
    }
  }

  /// Serializes the current CPU value of each GPUOutOfDate binding var whose
  /// name appears in `names`, marks those buffers Synced, and returns the data
  /// ready for upload.
  fn collect_dirty_uploads(
    &mut self,
    names: &[Arc<str>],
  ) -> Vec<((u8, u8), BufferUpload)> {
    let mut result = vec![];
    for (gb, name, ty, addr) in &self.binding_vars {
      if !names.contains(name) {
        continue;
      }
      if self.buffer_states.get(name) != Some(&SharedBufferState::GPUOutOfDate)
      {
        continue;
      }
      let value = self
        .bindings
        .get(name)
        .and_then(|v| v.last())
        .map(|(v, _)| v);
      let upload = match value {
        Some(Value::ZeroedArray { length }) => {
          let Type::Array(_, inner_type_info) = ty else {
            panic!()
          };
          let inner_ty = inner_type_info.unwrap_known();
          let elem_size = inner_ty.wgsl_flat_data_size_in_u32s();
          let align = inner_ty.wgsl_alignment_in_u32s();
          let stride = ((elem_size + align - 1) / align) * align;
          let raw_bytes =
            (*length as u64 * stride as u64 * 4).max(stride as u64 * 4);
          let padded = ((raw_bytes + 15) / 16) * 16;
          BufferUpload::Clear { byte_count: padded }
        }
        Some(Value::Texture {
          width,
          height,
          data,
          ..
        }) => BufferUpload::TextureData {
          width: *width,
          height: *height,
          data: data.clone(),
        },
        _ if *addr == VariableAddressSpace::Handle => {
          // Uninitialized texture — skip upload; the placeholder texture
          // created during GPU init remains bound.
          continue;
        }
        _ => {
          let mut bytes = value
            .map(|v| v.to_uniform_bytes(ty))
            .unwrap_or(vec![0u8; 4]);
          while bytes.len() % 16 != 0 {
            bytes.push(0);
          }
          BufferUpload::Data(bytes)
        }
      };
      result.push(((gb.group, gb.binding), upload));
      self.io.record_cpu_to_gpu_sync(name);
      self
        .buffer_states
        .insert(name.clone(), SharedBufferState::Synced);
    }
    result
  }

  /// Marks GPU-bound vars in `names` as CPUOutOfDate (GPU wrote them).
  fn mark_gpu_written(&mut self, names: &[Arc<str>]) {
    for name in names {
      if self.is_binding_var(name) {
        self
          .buffer_states
          .insert(name.clone(), SharedBufferState::CPUOutOfDate);
      }
    }
  }

  /// Marks GPU-bound vars in `names` as GPUOutOfDate (CPU wrote them).
  /// Also tags any texture value with its binding location so that
  /// `set-render-target` can later identify the GPU slot.
  fn mark_cpu_written(&mut self, names: &[Arc<str>]) {
    for name in names {
      if let Some(shared_index) = self.shared_indices.get(name).copied() {
        self.shared_dirty[shared_index] = true;
      }
      if self.is_binding_var(name) {
        self
          .buffer_states
          .insert(name.clone(), SharedBufferState::GPUOutOfDate);
        // Tag texture values with their binding so set-render-target can
        // identify the GPU slot without scanning all binding vars.
        let gb = self
          .binding_vars
          .iter()
          .find(|(_, n, _, addr)| {
            n == name && *addr == VariableAddressSpace::Handle
          })
          .map(|(gb, _, _, _)| *gb);
        if let Some(gb) = gb {
          if let Some(slot) =
            self.bindings.get_mut(name).and_then(|s| s.last_mut())
          {
            if let Value::Texture { binding, .. } = &mut slot.0 {
              *binding = Some(gb);
            }
          }
        }
      }
    }
  }

  /// Publishes snapshots of dirty thread-shared globals to the shared
  /// table, acting as the main-thread participant — plus any var whose
  /// audience intersects `force_mask` regardless of dirtiness (the
  /// `start-audio` bootstrap passes `participant::AUDIO` so the new
  /// replica can adopt the current state of everything it can see).
  /// Called at the end of every frame. A var is published only when some
  /// *other* live participant is in its audience, so programs with no
  /// second participant pay one atomic load per boundary and nothing
  /// else.
  ///
  /// The GPU participates in the sharing system through this boundary: it
  /// has no publish loop of its own, so when the newest value of a shared
  /// binding lives on the GPU (buffer state CPUOutOfDate), main acts as
  /// its proxy — reading the buffer back here (which flushes this frame's
  /// queued GPU work first) and publishing the fresh value. This is what
  /// makes GPU writes visible to the audio thread. The readback is a real
  /// cost, paid only for genuinely-shared GPU-written variables and only
  /// while another thread is live; the `gpu_write_audio_read` /
  /// `gpu_write_no_audio_no_readback` goldens pin both sides.
  pub fn publish_shared_globals(&mut self, force_mask: u32) {
    let live_others = self.shared_table.live_others(participant::MAIN);
    if live_others == 0 {
      return;
    }
    for index in 0..self.shared_globals.len() {
      let audience = self.shared_globals[index].2;
      if audience & live_others == 0 {
        continue;
      }
      let gpu_fresh = {
        let name = &self.shared_globals[index].0;
        self.buffer_states.get(name) == Some(&SharedBufferState::CPUOutOfDate)
      };
      if gpu_fresh {
        let name = self.shared_globals[index].0.clone();
        self.check_cpu_readable(&[name]);
      }
      // Forced (bootstrap) publishes are gap-filling only: restricted to
      // vars in main's own audience, and skipped when a snapshot already
      // exists — overwriting one would clobber another participant's
      // state (see the matching guard in `vm::shared_sync::publish_shared`).
      let forced = audience & force_mask != 0
        && audience & participant::MAIN != 0
        && !self.shared_table.slots[index].has_published();
      if !(forced || gpu_fresh || self.shared_dirty[index]) {
        continue;
      }
      let (name, ty, _) = self.shared_globals[index].clone();
      let Some((value, _)) =
        self.bindings.get(&name).and_then(|stack| stack.last())
      else {
        continue;
      };
      let words = if matches!(value, Value::Uninitialized) {
        if matches!(ty, Type::Array(_, _)) {
          // An unsized array before its first assignment is empty — publish
          // it as such, matching what the VM runtime (whose dynamic-memory
          // regions have no uninitialized state) publishes.
          Vec::new()
        } else {
          // Non-array Uninitialized (a texture that was never loaded);
          // textures can't be audio-reachable, so this is defensive.
          self.shared_dirty[index] = false;
          continue;
        }
      } else {
        value_to_shared_words(value, &ty)
      };
      let (version, _reusable_buffer) =
        self.shared_table.slots[index].publish(words);
      // Adopting our own publish would be a wasted copy; record its version
      // as already adopted.
      self.shared_adopted[index] = version;
      self.shared_dirty[index] = false;
      self.io.record_shared_publish(&name);
    }
  }

  /// Entry-start bootstrap for embedder-facing (`@external`) vars: adopt
  /// anything the handle pre-seeded, so even code before the first frame
  /// boundary (and frame 0's dispatches) sees the embedder's values.
  /// `@external` vars live in GPU-space address spaces and so can't have
  /// initializers — seeding is the embedder's job, through the handle.
  /// No-op unless an external handle is live.
  pub fn bootstrap_external_globals(&mut self) {
    if self.shared_table.live_others(participant::MAIN) & participant::EXTERNAL
      == 0
    {
      return;
    }
    self.adopt_shared_globals();
  }

  /// Adopts any shared-global snapshots published by other participants
  /// since we last looked, skipping vars outside the main thread's own
  /// audience (an audio↔embedder var flows between those two directly).
  /// Called at the start of every frame. For GPU-bound globals the
  /// adopted value marks the buffer GPUOutOfDate directly (not via
  /// `mark_cpu_written`, which would re-dirty the shared flag and
  /// ping-pong the value back at the next boundary).
  pub fn adopt_shared_globals(&mut self) {
    if self.shared_table.live_others(participant::MAIN) == 0 {
      return;
    }
    for index in 0..self.shared_globals.len() {
      if self.shared_globals[index].2 & participant::MAIN == 0 {
        continue;
      }
      let Some(snapshot) = self.shared_table.slots[index]
        .adopt_if_newer(self.shared_adopted[index])
      else {
        continue;
      };
      let (name, ty, _) = self.shared_globals[index].clone();
      let value = shared_words_to_value(&snapshot.words, &ty);
      self.shared_adopted[index] = snapshot.version;
      drop(snapshot);
      if let Some(slot) = self
        .bindings
        .get_mut(&name)
        .and_then(|stack| stack.last_mut())
      {
        slot.0 = value;
      }
      if self.is_binding_var(&name) {
        self
          .buffer_states
          .insert(name.clone(), SharedBufferState::GPUOutOfDate);
      }
      self.io.record_shared_adopt(&name);
    }
  }

  /// If `texture` is bound to a GPU binding whose newest content lives on
  /// the GPU (buffer state CPUOutOfDate — the texture was rendered into),
  /// flushes queued GPU work, reads the texture back as RGBA8, updates the
  /// CPU-side binding value, and returns the fresh texture. Otherwise
  /// returns `texture` unchanged. The texture analog of
  /// `check_cpu_readable`; used by `save-png`.
  fn refresh_texture_from_gpu(
    &mut self,
    texture: Value,
  ) -> Result<Value, EvalError> {
    let Value::Texture {
      binding: Some(gb), ..
    } = &texture
    else {
      return Ok(texture);
    };
    let gb = *gb;
    let Some(name) = self
      .binding_vars
      .iter()
      .find(|(binding_gb, _, _, _)| *binding_gb == gb)
      .map(|(_, name, _, _)| name.clone())
    else {
      return Ok(texture);
    };
    if self.buffer_states.get(&name) != Some(&SharedBufferState::CPUOutOfDate) {
      return Ok(texture);
    }
    // Run the frame's queued GPU work (the render into this texture may
    // still be pending) before reading back.
    self.io.flush_queued_compute();
    let Some((width, height, data)) =
      self.io.sync_texture_to_cpu(gb.group, gb.binding)
    else {
      // No GPU available (e.g. StringIO): the stale CPU copy is the best
      // we have.
      return Ok(texture);
    };
    let fresh = Value::Texture {
      width,
      height,
      data,
      binding: Some(gb),
    };
    if let Some(slot) = self
      .bindings
      .get_mut(&name)
      .and_then(|stack| stack.last_mut())
    {
      slot.0 = fresh.clone();
    }
    self
      .buffer_states
      .insert(name.clone(), SharedBufferState::Synced);
    self.io.record_gpu_to_cpu_sync(&name);
    Ok(fresh)
  }

  /// For any GPU-bound var in `names` that is CPUOutOfDate, reads the buffer
  /// back from GPU and updates the CPU-side binding.
  fn check_cpu_readable(&mut self, names: &[Arc<str>]) {
    let vars: Vec<(GroupAndBinding, Arc<str>, Type)> = self
      .binding_vars
      .iter()
      .filter(|(_, name, _, addr)| {
        // Textures (Handle) are written to by render passes and cannot be read
        // back as buffers — skip them.
        *addr != VariableAddressSpace::Handle
          && names.contains(name)
          && self.buffer_states.get(name)
            == Some(&SharedBufferState::CPUOutOfDate)
      })
      .map(|(gb, name, ty, _)| (*gb, name.clone(), ty.clone()))
      .collect();
    if !vars.is_empty() {
      // Flush any queued compute before reading back, so the GPU has actually
      // run and the buffers contain up-to-date values.
      self.io.flush_queued_compute();
    }
    for (gb, name, ty) in vars {
      // For statically-sized types, derive the readback size from the type.
      // For dynamically-sized types (unsized arrays), compute from the
      // CPU-side element count rather than the padded GPU allocation size:
      // the buffer is padded to a 16-byte multiple on upload, and using that
      // padded size for readback would cause from_gpu_bytes to count the
      // padding bytes as extra elements.
      let size = ty
        .flat_data_size_in_u32s(&crate::compiler::error::SourceTrace::empty())
        .ok()
        .map(|u32s| ((u32s as u64 * 4 + 15) & !15).max(16))
        .or_else(|| {
          if let Type::Array(
            Some(crate::compiler::types::ConcreteArraySize::Unsized),
            inner,
          ) = &ty
          {
            let inner_ty = inner.unwrap_known();
            let elem_size = inner_ty.wgsl_flat_data_size_in_u32s();
            let align = inner_ty.wgsl_alignment_in_u32s();
            let stride = ((elem_size + align - 1) / align) * align;
            let count = self
              .bindings
              .get(&name)
              .and_then(|v| v.last())
              .map(|(v, _)| match v {
                Value::ZeroedArray { length } => *length,
                Value::Array(elems) => elems.len(),
                _ => 0,
              })
              .unwrap_or(0);
            Some((count as u64 * stride as u64 * 4).max(16))
          } else {
            self.io.get_buffer_byte_size(gb.group, gb.binding)
          }
        })
        .unwrap_or(16);
      if let Some(bytes) = self.io.sync_gpu_to_cpu(gb.group, gb.binding, size) {
        let value = Value::from_gpu_bytes(&bytes, &ty);
        if let Some(stack) = self.bindings.get_mut(&name) {
          if let Some(slot) = stack.last_mut() {
            slot.0 = value;
          }
        }
        self.io.record_gpu_to_cpu_sync(&name);
        self
          .buffer_states
          .insert(name.clone(), SharedBufferState::Synced);
      }
    }
  }

  /// Overwrites the CPU-side value of the GPU-bound variable at
  /// `(group, binding)` with a value decoded from `bytes` (using the same
  /// byte layout as the variable's GPU buffer), and marks it `Synced`.
  /// Intended for host applications that write a binding's GPU buffer
  /// externally and need the interpreter's CPU-side copy to match without a
  /// GPU readback.  Returns false if no variable is bound at
  /// `(group, binding)`.
  pub fn overwrite_binding_from_gpu_bytes(
    &mut self,
    group: u8,
    binding: u8,
    bytes: &[u8],
  ) -> bool {
    let Some((_, name, ty, _)) = self
      .binding_vars
      .iter()
      .find(|(gb, _, _, _)| gb.group == group && gb.binding == binding)
    else {
      return false;
    };
    let name = name.clone();
    let ty = ty.clone();
    let value = Value::from_gpu_bytes(bytes, &ty);
    if let Some(stack) = self.bindings.get_mut(&name) {
      if let Some(slot) = stack.last_mut() {
        slot.0 = value;
      }
    }
    self.buffer_states.insert(name, SharedBufferState::Synced);
    true
  }

  /// Ensures a GPU context is available for compute dispatches. If the IO
  /// manager supports real GPU but none has been set yet (i.e. no window was
  /// opened), this lazily creates a headless GPU core.
  fn setup_gpu_if_needed(&mut self) {
    #[cfg(feature = "window")]
    {
      let wgsl = self.wgsl.clone();
      let binding_infos = self.binding_infos();
      let gpu_entries = self.gpu_entries.clone();
      self
        .io
        .ensure_gpu_ready(&wgsl, &binding_infos, &gpu_entries);
    }
  }

  /// Syncs all GPU-written (CPUOutOfDate) buffers back to CPU, then marks
  /// them GPUOutOfDate so that when a new GPU is created (e.g. transitioning
  /// from headless to windowed), it will receive the correct data on next use.
  pub fn sync_gpu_written_to_cpu(&mut self) {
    let names: Vec<Arc<str>> = self
      .buffer_states
      .iter()
      .filter(|(_, s)| **s == SharedBufferState::CPUOutOfDate)
      .map(|(n, _)| n.clone())
      .collect();
    if names.is_empty() {
      return;
    }
    self.check_cpu_readable(&names);
    for name in &names {
      if self.buffer_states.get(name) == Some(&SharedBufferState::Synced) {
        self
          .buffer_states
          .insert(name.clone(), SharedBufferState::GPUOutOfDate);
      }
    }
  }

  fn bind(&mut self, name: Arc<str>, value: Value, ty: Type) {
    if let Some(bindings) = self.bindings.get_mut(&name) {
      bindings.push((value, ty));
    } else {
      self.bindings.insert(name, vec![(value, ty)]);
    }
  }
  /// Pop the topmost binding for `name`, returning the popped `(value, type)`
  /// so the caller can inspect or move it back into the surrounding env
  /// (used for mutable-reference write-back after a function call).
  fn unbind(&mut self, name: &Arc<str>) -> (Value, Type) {
    let bindings = self.bindings.get_mut(name).unwrap();
    let popped = bindings.pop().unwrap();
    if bindings.is_empty() {
      self.bindings.remove(name);
    }
    popped
  }
  fn lookup(&self, name: &Arc<str>) -> Result<&Value, EvalError> {
    self
      .bindings
      .get(name)
      .map(|values| &values.last().unwrap().0)
      .ok_or(UnboundName(name.clone()).into())
  }
}

#[derive(Debug, Clone)]
pub enum EvalException {
  Error(EvalError),
  Break,
  Continue,
  Return(Value),
  CloseWindow,
  ReloadRequested,
}

impl EvalException {
  fn name(&self) -> &str {
    match self {
      EvalException::Error(_) => "Evaluation Error",
      EvalException::Break => "break",
      EvalException::Continue => "continue",
      EvalException::Return(_) => "return",
      EvalException::CloseWindow => "close-window",
      EvalException::ReloadRequested => "reload-requested",
    }
  }
}

impl From<EvalError> for EvalException {
  fn from(e: EvalError) -> Self {
    Self::Error(e)
  }
}

impl From<EvalException> for EvalError {
  fn from(e: EvalException) -> Self {
    match e {
      EvalException::Error(err) => err,
      other => {
        ControlFlowExceptionEscapedToTopLevel(other.name().into()).into()
      }
    }
  }
}

/// Write `new_value` back into the env at the location described by `lhs`.
/// Used to propagate mutations through a function's mutable-reference args
/// after the call returns: the callee mutates a *copy* of the value during
/// evaluation, and this walks the callsite expression backward (Name, Field,
/// ArrayIndex via Access or call-style) to find where the original lived and
/// overwrites it.
///
/// Bails out (returns `Ok(())` without writing) on swizzle accesses for now
/// — handling those needs the same component-shuffle logic the assignment
/// path has, and the assignment path will need to be reused/refactored to
/// avoid duplication. Until then, a `(helper v.zy)` callsite will see its
/// post-call mutations dropped silently. That's intentional: those tests
/// will continue to fail the same way they did before this change, which
/// is the deferral the user asked for.
fn write_back_through_lhs<IO: IOManager>(
  env: &mut EvaluationEnvironment<IO>,
  lhs: Exp<ExpTypeInfo>,
  new_value: Value,
) -> Result<(), EvalException> {
  enum AccessKind {
    Index(i64),
    Field(Arc<str>),
  }
  let mut accesses: Vec<AccessKind> = vec![];
  let mut cur = lhs;
  let accessed_name = loop {
    match cur.kind {
      ExpKind::Name(name) => break name,
      ExpKind::Application(callee, mut index_args) => {
        // A function-typed callee is a closure's scope construction
        // inlined at the call site: the scope struct is a temporary with
        // no named source, so there's nothing to write back to (matching
        // C/WGSL, where it's materialized as a caller-frame temporary).
        if matches!(callee.data.unwrap_known(), Type::Function(_)) {
          return Ok(());
        }
        // `(v 0u)` call-style indexing (used for vec/mat element access).
        let Value::Prim(index) = eval(index_args.remove(0), env)? else {
          panic!("expected scalar index in call-style access");
        };
        let index = match index {
          Primitive::U32(u) => u as i64,
          Primitive::I32(i) => i as i64,
          _ => panic!("non-integer index"),
        };
        accesses.push(AccessKind::Index(index));
        cur = *callee;
      }
      ExpKind::Access(accessor, inner) => {
        match accessor {
          Accessor::Field(field_name) => {
            accesses.push(AccessKind::Field(field_name))
          }
          Accessor::ArrayIndex(index_exp) => {
            let Value::Prim(index) = eval(*index_exp, env)? else {
              panic!("expected scalar array index");
            };
            let index = match index {
              Primitive::U32(u) => u as i64,
              Primitive::I32(i) => i as i64,
              _ => panic!("non-integer array index"),
            };
            accesses.push(AccessKind::Index(index));
          }
          Accessor::Swizzle(_) => {
            // Deferred — see fn doc comment.
            return Ok(());
          }
        }
        cur = *inner;
      }
      _ => {
        // Any other LHS shape (literal, block, etc.) wasn't a real
        // reference target to begin with; nothing to write back to.
        return Ok(());
      }
    }
  };
  // Descend through the env's binding with &mut and overwrite at the end.
  let mut slot = &mut env
    .bindings
    .get_mut(&*accessed_name)
    .unwrap()
    .last_mut()
    .unwrap()
    .0;
  for access in accesses.into_iter().rev() {
    match access {
      AccessKind::Index(i) => match slot {
        Value::Array(a) => {
          let length = a.len() as i64;
          slot = &mut a[(((i % length) + length) % length) as usize];
        }
        Value::Struct(s) => {
          // Vector stored as Struct with x/y/z/w fields.
          let field_name = ["x", "y", "z", "w"][i as usize];
          slot = s.get_mut(field_name).unwrap();
        }
        _ => panic!("indexed access into non-array, non-struct value"),
      },
      AccessKind::Field(name) => {
        let Value::Struct(s) = slot else {
          panic!("field access on non-struct value");
        };
        slot = s.get_mut(&name).unwrap();
      }
    }
  }
  // If the source binding holds a closure (Function::Scoped) and the
  // incoming value is its bare mutated scope struct, update the wrapper's
  // captured scope in place rather than replacing the closure with data.
  if let Value::Fun(Function::Scoped { scope, .. }) = slot
    && !matches!(new_value, Value::Fun(_))
  {
    **scope = new_value;
  } else {
    *slot = new_value;
  }
  Ok(())
}

pub fn eval(
  exp: Exp<ExpTypeInfo>,
  env: &mut EvaluationEnvironment<impl IOManager>,
) -> Result<Value, EvalException> {
  let exp_effects = exp.effects();
  Ok(match exp.kind {
    ExpKind::Wildcard => return Err(EncounteredWildcard.into()),
    ExpKind::Unit => Value::Unit,
    ExpKind::Name(name) => env.lookup(&name)?.clone(),
    ExpKind::NumberLiteral(number) => match exp.data.kind.unwrap_known() {
      Type::F32 => Primitive::F32(match number {
        Number::Int(i) => i as f32,
        Number::Float(f) => f as f32,
      }),
      Type::I32 => Primitive::I32(match number {
        Number::Int(i) => i as i32,
        Number::Float(_) => return Err(FloatInIntLiteral.into()),
      }),
      Type::U32 => Primitive::U32(match number {
        Number::Int(i) => i as u32,
        Number::Float(_) => return Err(FloatInIntLiteral.into()),
      }),
      _ => return Err(InvalidNumberLiteralType.into()),
    }
    .into(),
    ExpKind::BooleanLiteral(b) => Primitive::Bool(b).into(),
    ExpKind::StringLiteral(s) => Value::String(s.to_string()),
    ExpKind::Function(arg_names, expression) => {
      Value::Fun(Function::Composite {
        arg_names: arg_names
          .iter()
          .map(|(arg_name, _)| arg_name.clone())
          .collect(),
        expression: *expression,
      })
    }
    ExpKind::Application(f, mut args) => match f.data.unwrap_known() {
      Type::Function(f_signature) => {
        // Capture per-parameter ownership before f_signature gets partially
        // moved by the abstract_ancestor pattern below. Needed at the end of
        // the Composite call path to decide which args want write-back.
        let param_ownerships: Vec<Ownership> = f_signature
          .args
          .iter()
          .map(|(v, _)| v.var_type.ownership)
          .collect();
        let name = match f.kind {
          ExpKind::Name(name) => name,
          _ => return Err(AppliedNonName.into()),
        };
        let f_arc = match f_signature.abstract_ancestor {
          Some(arc) => arc,
          None => panic!(
            "application of \"{name}\" reached the interpreter with no \
             abstract ancestor; every fully-lowered application callee must \
             carry one (closure scope constructions get the scope struct's \
             constructor attached in extract_inner_functions)"
          ),
        };
        // A struct-constructor application whose own type is a function is a
        // closure's scope construction (extract_inner_functions attaches the
        // scope struct's constructor as the callee's ancestor, and the
        // extracted inner fn as the expression type's ancestor). Evaluate the
        // struct directly and wrap it as Function::Scoped.
        if matches!(
          f_arc.read().unwrap().implementation,
          FunctionImplementationKind::StructConstructor
        ) && matches!(exp.data.kind, TypeState::Known(Type::Function(_)))
        {
          let field_names: Vec<Arc<str>> = {
            let s = env
              .structs
              .get(&name)
              .unwrap_or_else(|| panic!("unknown struct: {name}"));
            s.fields.iter().map(|f| f.name.clone()).collect()
          };
          let arg_values: Vec<Value> = args
            .into_iter()
            .map(|arg| eval(arg, env))
            .collect::<Result<_, _>>()?;
          let scope_struct =
            Value::Struct(field_names.into_iter().zip(arg_values).collect());
          if let Type::Function(outer_sig) = exp.data.unwrap_known()
            && let Some(inner_fn_arc) = outer_sig.abstract_ancestor
          {
            let inner_fn = {
              let sig = inner_fn_arc.read().unwrap();
              Function::from_abstract_signature(&sig, &sig.name.clone(), env)?
            };
            return Ok(Value::Fun(Function::Scoped {
              inner: Box::new(inner_fn),
              scope: Box::new(scope_struct),
            }));
          }
          return Ok(scope_struct);
        }
        let f = Function::from_abstract_signature(
          &*f_arc.read().unwrap(),
          &name,
          env,
        )?;
        // For composite callees, re-derive the per-parameter ownerships
        // from the *implementation's* own function type — the same
        // signature-level source the VM's ref-arg detection uses — rather
        // than trusting the call site's signature view captured above.
        // The lowering passes that append trailing scope args are
        // supposed to keep that view aligned (and now do), but
        // bookkeeping driven by a stale view silently dropped the scope
        // write-back and leaked the binding, so the runtime prefers the
        // authoritative source (pinned by `closure_seeded_capture_read`).
        let param_ownerships: Vec<Ownership> =
          if let FunctionImplementationKind::Composite(implementation) =
            &f_arc.read().unwrap().implementation
            && let Type::Function(impl_signature) = implementation
              .read()
              .unwrap()
              .expression
              .data
              .unwrap_known()
          {
            impl_signature
              .args
              .iter()
              .map(|(v, _)| v.var_type.ownership)
              .collect()
          } else {
            param_ownerships
          };
        let arg_types: Vec<Type> =
          args.iter().map(|a| a.data.kind.unwrap_known()).collect();
        let return_type = exp.data.unwrap_known();
        let is_assignment_op = ASSIGNMENT_OPS.contains(&*name);
        let is_atomic_op = ATOMIC_MUTATION_OPS.contains(&*name);
        let accessed_expression =
          (is_assignment_op || is_atomic_op).then(|| args[0].clone());
        // Sync any GPU-written globals before evaluating args so we see the
        // updated values when the args are looked up.
        let (read_global_variable_names, written_global_variable_names) =
          exp_effects.read_and_written_globals();
        env.check_cpu_readable(&read_global_variable_names);
        // For args bound to a reference-typed parameter, save a clone of the
        // callsite expression so we can write the (possibly mutated) post-
        // call value back into its source location once the call returns.
        // Owned args get `None` and skip write-back.
        let ref_arg_lhs_exprs: Vec<Option<Exp<ExpTypeInfo>>> = args
          .iter()
          .zip(param_ownerships.iter())
          .map(|(arg, ownership)| match ownership {
            Ownership::Reference
            | Ownership::MutableReference
            | Ownership::Pointer(_) => Some(arg.clone()),
            Ownership::Owned => None,
          })
          .collect();
        let arg_values: Vec<Value> = args
          .into_iter()
          .map(|arg| {
            if let Type::Function(f) = arg.data.unwrap_known()
              && let Some(f) = f.abstract_ancestor
              && let ExpKind::Name(f_name) = arg.kind
            {
              // A local binding may hold a closure value carrying captured
              // scope (Function::Scoped) — use it if present. Only rebuild
              // from the signature when the name isn't bound in the env,
              // i.e. it refers to a top-level function.
              if let Ok(value) = env.lookup(&f_name) {
                Ok(value.clone())
              } else {
                Ok(Value::Fun(Function::from_abstract_signature(
                  &f.read().unwrap(),
                  &f_name,
                  env,
                )?))
              }
            } else {
              eval(arg, env)
            }
          })
          .collect::<Result<_, _>>()?;
        let mut return_value = match f {
          Function::Builtin(name) => apply_builtin_fn(
            name,
            arg_values.into_iter().zip(arg_types.into_iter()).collect(),
            return_type,
            env,
          )?,
          Function::StructConstructor(field_names) => Value::Struct(
            field_names
              .into_iter()
              .zip(arg_values.into_iter())
              .collect(),
          ),
          Function::EnumConstructor(variant_name) => Value::Enum(
            variant_name,
            arg_values.into_iter().next().unwrap().into(),
          ),
          Function::Composite {
            arg_names,
            expression,
          } => {
            let ExpKind::Function(_, body) = expression.kind else {
              panic!()
            };
            if arg_names.len() != arg_values.len() {
              return Err(WrongArity(arg_names.len(), arg_values.len()).into());
            }
            assert_eq!(
              arg_names.len(),
              ref_arg_lhs_exprs.len(),
              "compiler bug: call-site argument bookkeeping misaligned with \
               `{name}`'s parameters — a silent zip truncation here drops \
               reference write-backs and leaks bindings"
            );
            for (name, (value, ty)) in arg_names
              .iter()
              .zip(arg_values.into_iter().zip(arg_types.into_iter()))
            {
              // A closure value (Function::Scoped) arriving at a parameter
              // that statically expects the scope struct itself — the
              // trailing scope arg added by higher-order inlining — binds
              // the bare scope struct; write_back_through_lhs re-wraps the
              // mutated struct into the closure at the source binding.
              let value = match value {
                Value::Fun(Function::Scoped { scope, .. })
                  if !matches!(ty, Type::Function(_)) =>
                {
                  *scope
                }
                other => other,
              };
              env.bind(name.clone(), value, ty);
            }
            let value = match eval(*body, env) {
              Ok(value) => Ok(value),
              Err(exception) => match exception {
                EvalException::Return(value) => Ok(value),
                other => Err(other),
              },
            };
            // Mutable-reference write-back. For each arg bound to a
            // reference-typed parameter, pop its (possibly mutated) value
            // off the env and copy it back to wherever it came from in
            // the caller's environment, using the callsite LHS expression
            // we saved before the call. Owned args just get unbound and
            // the value dropped, same as before.
            for (name, lhs) in
              arg_names.iter().zip(ref_arg_lhs_exprs.into_iter()).rev()
            {
              let (binding_value, _) = env.unbind(name);
              if let Some(lhs_exp) = lhs {
                write_back_through_lhs(env, lhs_exp, binding_value)?;
              }
            }
            value?
          }
          Function::Scoped { inner, scope } => {
            // Call the inner function with scope prepended to args.
            let scoped_values: Vec<Value> =
              std::iter::once(*scope).chain(arg_values).collect();
            let scoped_types: Vec<Type> =
              std::iter::once(Type::Unit).chain(arg_types).collect();
            let Function::Composite {
              arg_names,
              expression,
            } = *inner
            else {
              panic!("Scoped inner must be Composite")
            };
            let ExpKind::Function(_, body) = expression.kind else {
              panic!()
            };
            if arg_names.len() != scoped_values.len() {
              return Err(
                WrongArity(arg_names.len(), scoped_values.len()).into(),
              );
            }
            for (name, (value, ty)) in arg_names
              .iter()
              .zip(scoped_values.into_iter().zip(scoped_types))
            {
              env.bind(name.clone(), value, ty);
            }
            let value = match eval(*body, env) {
              Ok(value) => Ok(value),
              Err(exception) => match exception {
                EvalException::Return(value) => Ok(value),
                other => Err(other),
              },
            };
            for name in arg_names.iter() {
              let _ = env.unbind(name);
            }
            value?
          }
        };
        return_value = if is_assignment_op {
          enum AccessKind {
            Index(i64),
            Field(Arc<str>),
            Swizzle(Vec<SwizzleField>),
          }
          let mut accesses: Vec<AccessKind> = vec![];
          let mut accessed_expression = accessed_expression.unwrap();
          let accessed_name = loop {
            match accessed_expression.kind {
              ExpKind::Name(name) => break name,
              ExpKind::Application(exp, mut index) => {
                let Value::Prim(index) = eval(index.remove(0), env)? else {
                  panic!()
                };
                let index = match index {
                  Primitive::U32(u) => u as i64,
                  Primitive::I32(i) => i as i64,
                  _ => panic!(),
                };
                accesses.push(AccessKind::Index(index));
                accessed_expression = *exp;
              }
              ExpKind::Access(accessor, exp) => {
                match accessor {
                  Accessor::Field(field_name) => {
                    accesses.push(AccessKind::Field(field_name))
                  }
                  Accessor::Swizzle(swizzle_fields) => {
                    accesses.push(AccessKind::Swizzle(swizzle_fields))
                  }
                  Accessor::ArrayIndex(index_exp) => {
                    let Value::Prim(index) = eval(*index_exp, env)? else {
                      panic!()
                    };
                    let index = match index {
                      Primitive::U32(u) => u as i64,
                      Primitive::I32(i) => i as i64,
                      _ => panic!(),
                    };
                    accesses.push(AccessKind::Index(index));
                  }
                }
                accessed_expression = *exp;
              }
              _ => panic!(),
            }
          };
          // Pre-expand any ZeroedArray at the top-level binding before taking a
          // mutable reference. Value::zeroed needs an immutable &env borrow,
          // which would conflict with &mut env.bindings during traversal.
          if let Some((Value::ZeroedArray { length }, Type::Array(_, inner))) =
            env.bindings.get(&*accessed_name).and_then(|s| s.last())
          {
            let length = *length;
            let elem_ty = inner.unwrap_known();
            let zero_val = Value::zeroed(elem_ty, env)?;
            take(
              &mut env
                .bindings
                .get_mut(&*accessed_name)
                .unwrap()
                .last_mut()
                .unwrap()
                .0,
              |_| Value::Array(vec![zero_val; length]),
            );
          }
          let mut accessed_value = &mut env
            .bindings
            .get_mut(&*accessed_name)
            .unwrap()
            .last_mut()
            .unwrap()
            .0;
          let mut active_swizzle_fields: Option<Vec<usize>> = None;
          for access in accesses.into_iter().rev() {
            match access {
              AccessKind::Index(i) => match accessed_value {
                Value::Array(a) => {
                  let length = a.len() as i64;
                  accessed_value =
                    &mut a[(((i % length) + length) % length) as usize];
                }
                Value::Struct(s) => {
                  // Vector stored as Struct with x/y/z/w fields.
                  let field_name = ["x", "y", "z", "w"][i as usize];
                  accessed_value = s.get_mut(field_name).unwrap();
                }
                _ => panic!(),
              },
              AccessKind::Field(name) => {
                if let Some(previous_swizzle_fields) = active_swizzle_fields {
                  active_swizzle_fields = Some(vec![
                    previous_swizzle_fields
                      [SwizzleField::from_name(&*name).index()],
                  ]);
                } else {
                  let Value::Struct(s) = accessed_value else {
                    panic!()
                  };
                  accessed_value = s.get_mut(&name).unwrap();
                }
              }
              AccessKind::Swizzle(swizzle_fields) => {
                if let Some(previous_swizzle_fields) = active_swizzle_fields {
                  active_swizzle_fields = Some(
                    swizzle_fields
                      .into_iter()
                      .map(|f| previous_swizzle_fields[f.index()])
                      .collect(),
                  );
                } else {
                  active_swizzle_fields = Some(
                    swizzle_fields.into_iter().map(|f| f.index()).collect(),
                  );
                }
              }
            }
          }
          if let Some(active_swizzle_fields) = active_swizzle_fields {
            if active_swizzle_fields.len() == 1 {
              let Value::Struct(s) = accessed_value else {
                panic!()
              };
              *s.get_mut(
                SwizzleField::from_index(active_swizzle_fields[0]).name(),
              )
              .unwrap() = return_value;
            } else {
              let Value::Struct(s) = accessed_value else {
                panic!()
              };
              let Value::Struct(mut return_s) = return_value else {
                panic!()
              };
              for (target_field, source_field) in
                active_swizzle_fields.into_iter().zip(["x", "y", "z", "w"])
              {
                *s.get_mut(SwizzleField::from_index(target_field).name())
                  .unwrap() = return_s.remove(source_field).unwrap();
              }
            }
          } else {
            *accessed_value = return_value;
          }
          Value::Unit
        } else if is_atomic_op {
          enum AccessKind {
            Index(i64),
            Field(Arc<str>),
          }
          let mut accesses: Vec<AccessKind> = vec![];
          let mut accessed_expression = accessed_expression.unwrap();
          let accessed_name = loop {
            match accessed_expression.kind {
              ExpKind::Name(name) => break name,
              ExpKind::Application(exp, mut index) => {
                let Ok(Value::Prim(index)) = eval(index.remove(0), env) else {
                  panic!()
                };
                let index = match index {
                  Primitive::U32(u) => u as i64,
                  Primitive::I32(i) => i as i64,
                  _ => panic!(),
                };
                accesses.push(AccessKind::Index(index));
                accessed_expression = *exp;
              }
              ExpKind::Access(accessor, exp) => {
                match accessor {
                  Accessor::Field(field_name) => {
                    accesses.push(AccessKind::Field(field_name));
                  }
                  Accessor::ArrayIndex(index_exp) => {
                    let Ok(Value::Prim(index)) = eval(*index_exp, env) else {
                      panic!()
                    };
                    let index = match index {
                      Primitive::U32(u) => u as i64,
                      Primitive::I32(i) => i as i64,
                      _ => panic!(),
                    };
                    accesses.push(AccessKind::Index(index));
                  }
                  Accessor::Swizzle(_) => panic!(),
                }
                accessed_expression = *exp;
              }
              _ => panic!(),
            }
          };
          let mut accessed_value = &mut env
            .bindings
            .get_mut(&*accessed_name)
            .unwrap()
            .last_mut()
            .unwrap()
            .0;
          for access in accesses.into_iter().rev() {
            match access {
              AccessKind::Index(i) => {
                let Value::Array(a) = accessed_value else {
                  panic!()
                };
                let length = a.len() as i64;
                accessed_value =
                  &mut a[(((i % length) + length) % length) as usize];
              }
              AccessKind::Field(field_name) => {
                let Value::Struct(s) = accessed_value else {
                  panic!()
                };
                accessed_value = s.get_mut(&field_name).unwrap();
              }
            }
          }
          let old_inner = match &*accessed_value {
            Value::Struct(fields) => fields["_"].clone(),
            _ => panic!("atomic op applied to non-atomic value"),
          };
          *accessed_value = return_value;
          if &*name == "atomic-store" {
            Value::Unit
          } else {
            old_inner
          }
        } else {
          return_value
        };
        env.mark_cpu_written(&written_global_variable_names);
        return_value
      }
      Type::Array(_, inner_type) => {
        if args.len() != 1 {
          panic!();
        }
        let elem_type = inner_type.unwrap_known();
        let array = eval(*f, env)?;
        let index_value = eval(args.remove(0), env)?;
        let Value::Prim(primitive) = index_value else {
          panic!();
        };
        let u = match primitive {
          Primitive::U32(u) => u as usize,
          Primitive::I32(i) => {
            if i < 0 {
              return Err(NegativeArrayIndex(i as isize).into());
            } else {
              i as usize
            }
          }
          _ => panic!(),
        };
        match array {
          Value::Array(array_values) => {
            if u < array_values.len() {
              array_values[u].clone()
            } else {
              return Err(ArrayIndexOutOfBounds(u, array_values.len()).into());
            }
          }
          Value::ZeroedArray { length } => {
            if u < length {
              Value::zeroed(elem_type, env)?
            } else {
              return Err(ArrayIndexOutOfBounds(u, length).into());
            }
          }
          _ => panic!(),
        }
      }
      _ => panic!(),
    },
    ExpKind::Access(accessor, exp) => {
      let exp_type = exp.data.unwrap_known();
      let value = eval(*exp, env)?;
      match accessor {
        Accessor::Field(field_name) => match value {
          Value::Struct(s) => s
            .get(&field_name)
            .ok_or_else(|| NoSuchField(field_name.clone()))?
            .clone(),
          _ => return Err(AccessedFieldOnNonStruct(field_name.clone()).into()),
        },
        Accessor::Swizzle(swizzle_fields) => {
          let map = match value {
            Value::Struct(map) => map,
            _ => {
              return Err(
                AccessedFieldOnNonStruct(
                  swizzle_fields
                    .iter()
                    .map(|f| f.name())
                    .fold(String::new(), |mut acc, name| {
                      acc += name;
                      acc
                    })
                    .into(),
                )
                .into(),
              );
            }
          };
          let values: Vec<Value> = swizzle_fields
            .into_iter()
            .map(|field| {
              map
                .get(match field {
                  SwizzleField::X => "x",
                  SwizzleField::Y => "y",
                  SwizzleField::Z => "z",
                  SwizzleField::W => "w",
                } as &str)
                .map(|v| v.clone())
                .ok_or_else(|| NoSuchField(field.name().into()))
            })
            .collect::<Result<Vec<Value>, _>>()?;
          Value::Struct(
            ["x", "y", "z", "w"]
              .into_iter()
              .map(|n| n.into())
              .zip(values.into_iter())
              .collect(),
          )
        }
        Accessor::ArrayIndex(exp) => {
          let index = eval(*exp, env)?;
          let u_index = |len: usize| match index.unwrap_primitive() {
            Primitive::U32(u) => u as usize % len,
            Primitive::I32(i) => i.rem_euclid(len as i32) as usize,
            _ => panic!(),
          };
          match value {
            Value::Array(values) => {
              let i = u_index(values.len());
              values[i].clone()
            }
            Value::ZeroedArray { .. } => {
              let Type::Array(_, inner_type) = exp_type else {
                panic!()
              };
              Value::zeroed(inner_type.unwrap_known(), env)?
            }
            // Vector indexing: stored as a Struct with x/y/z/w fields.
            Value::Struct(fields) if exp_type.is_vector() => {
              let len = fields.len();
              let i = u_index(len);
              fields
                .get(["x", "y", "z", "w"][i])
                .expect("vector index out of bounds")
                .clone()
            }
            _ => panic!(),
          }
        }
      }
    }
    ExpKind::Let(items, exp) => {
      let names: Vec<Arc<str>> =
        items.iter().map(|(name, _, _, _)| name.clone()).collect();
      for (name, _, _, exp) in items {
        let ty = exp.data.kind.unwrap_known();
        let value = eval(exp, env)?;
        env.bind(name, value, ty);
      }
      let value = eval(*exp, env)?;
      for name in names {
        let _ = env.unbind(&name);
      }
      value
    }
    ExpKind::Match(scrutinee, arms) => {
      let scrutinee = eval(*scrutinee, env)?;
      for (match_exp, body_exp) in arms {
        if match_exp.kind == ExpKind::Wildcard {
          return eval(body_exp, env);
        }
        let enum_pattern_variant: Option<Arc<str>> = match &match_exp.kind {
          ExpKind::Application(f, _) => {
            if let Type::Function(f_sig) = f.data.unwrap_known()
              && let Some(abstract_f) = f_sig.abstract_ancestor
              && let FunctionImplementationKind::EnumConstructor(v) =
                &abstract_f.read().unwrap().implementation
            {
              Some(v.clone())
            } else {
              None
            }
          }
          _ => None,
        };
        if let Some(pattern_variant) = enum_pattern_variant {
          let ExpKind::Application(_, args) = match_exp.kind else {
            unreachable!()
          };
          if let Value::Enum(scrutinee_variant, inner_value) = &scrutinee
            && pattern_variant == *scrutinee_variant
          {
            let inner_pattern = args.into_iter().next().unwrap();
            let inner_ty = inner_pattern.data.kind.unwrap_known();
            let ExpKind::Name(inner_name) = inner_pattern.kind else {
              unreachable!()
            };
            env.bind(inner_name.clone(), (**inner_value).clone(), inner_ty);
            let result = eval(body_exp, env);
            let _ = env.unbind(&inner_name);
            return result;
          }
          continue;
        }
        if eval(match_exp, env)? == scrutinee {
          return eval(body_exp, env);
        }
      }
      return Err(NoMatchingArm.into());
    }
    ExpKind::Block(exps) => exps
      .into_iter()
      .map(|exp| eval(exp, env))
      .collect::<Result<Vec<Value>, _>>()?
      .pop()
      .unwrap_or_else(|| Value::Unit),
    ExpKind::ForLoop {
      increment_variable_name,
      increment_variable_initial_value_expression,
      continue_condition_expression,
      update_expression,
      body_expression,
      ..
    } => {
      let initial_ty = increment_variable_initial_value_expression
        .data
        .kind
        .unwrap_known();
      let initial_value =
        eval(*increment_variable_initial_value_expression, env)?;
      env.bind(increment_variable_name.0.clone(), initial_value, initial_ty);
      loop {
        let should_continue =
          eval(*continue_condition_expression.clone(), env)?;
        match should_continue {
          Value::Prim(Primitive::Bool(b)) => {
            if !b {
              break;
            }
          }
          _ => return Err(NonBooleanLoopCondition.into()),
        }
        let mut broke = false;
        for maybe_exp in [
          Some(*body_expression.clone()),
          update_expression.as_ref().map(|x| (**x).clone()),
        ] {
          if let Some(exp) = maybe_exp {
            match eval(exp, env) {
              Ok(_) | Err(EvalException::Continue) => {}
              Err(EvalException::Break) => {
                broke = true;
                break;
              }
              Err(e) => {
                return Err(e);
              }
            }
          }
        }
        if broke {
          break;
        }
      }
      let _ = env.unbind(&increment_variable_name.0);
      Value::Unit
    }
    ExpKind::WhileLoop {
      condition_expression,
      body_expression,
    } => {
      loop {
        let should_continue = eval(*condition_expression.clone(), env)?;
        match should_continue {
          Value::Prim(Primitive::Bool(b)) => {
            if !b {
              break;
            }
          }
          _ => return Err(NonBooleanLoopCondition.into()),
        }
        match eval(*body_expression.clone(), env) {
          Ok(_) | Err(EvalException::Continue) => {}
          Err(EvalException::Break) => break,
          Err(e) => {
            return Err(e);
          }
        }
      }
      Value::Unit
    }
    ExpKind::Break => return Err(EvalException::Break),
    ExpKind::Continue => return Err(EvalException::Continue),
    ExpKind::Return(exp) => {
      return Err(EvalException::Return(eval(*exp, env)?));
    }
    ExpKind::Discard => return Err(Discard.into()),
    ExpKind::ArrayLiteral(exps) => Value::Array(
      exps
        .into_iter()
        .map(|exp| eval(exp, env))
        .collect::<Result<_, _>>()?,
    ),
    ExpKind::Uninitialized => Value::zeroed(exp.data.unwrap_known(), env)?,
  })
}

fn run_program_with<IO: IOManager>(
  program: Program,
  entry_point_name: Option<&str>,
  io: IO,
  source_dir: Option<PathBuf>,
) -> Result<(IO, bool), EvalError> {
  let body = pick_entry_point_body(&program, entry_point_name)?;
  let mut env = EvaluationEnvironment::from_program(program, io, source_dir)?;
  env.bootstrap_external_globals();
  match eval(body, &mut env) {
    Ok(_) => Ok((env.io, false)),
    Err(EvalException::ReloadRequested) => Ok((env.io, true)),
    Err(e) => Err(e.into()),
  }
}

#[cfg(feature = "window")]
fn run_program_with_audio_source<IO: IOManager>(
  program: Program,
  entry_point_name: Option<&str>,
  io: IO,
  source_dir: Option<PathBuf>,
  audio_source: Option<crate::audio::AudioSource>,
  external_vars: Option<Arc<ExternalVars>>,
) -> Result<(IO, bool), EvalError> {
  let body = pick_entry_point_body(&program, entry_point_name)?;
  let mut env =
    EvaluationEnvironment::from_program_with_audio_source_and_external(
      program,
      io,
      source_dir,
      audio_source,
      external_vars,
    )?;
  env.bootstrap_external_globals();
  match eval(body, &mut env) {
    Ok(_) => Ok((env.io, false)),
    Err(EvalException::ReloadRequested) => Ok((env.io, true)),
    Err(e) => Err(e.into()),
  }
}

/// Pick the right `@cpu` entry point function body to evaluate. Shared by
/// the audio-aware and non-audio runners.
fn pick_entry_point_body(
  program: &Program,
  entry_point_name: Option<&str>,
) -> Result<Exp<ExpTypeInfo>, EvalError> {
  let mut cpu_fns = program.cpu_entry_points();
  let entry_fn = match entry_point_name {
    Some(name) => {
      let pos = cpu_fns
        .iter()
        .position(|f| &*f.read().unwrap().name == name)
        .ok_or_else(|| CpuEntryPointNotFound(name.into()))?;
      cpu_fns.remove(pos)
    }
    None => match cpu_fns.len() {
      0 => return Err(NoCpuEntryPoint.into()),
      1 => cpu_fns.remove(0),
      _ => return Err(MultipleCpuEntryPoints.into()),
    },
  };
  let FunctionImplementationKind::Composite(f) =
    &entry_fn.read().unwrap().implementation
  else {
    panic!("cpu entry point wasn't a composite function")
  };
  let f = f.read().unwrap();
  let ExpKind::Function(_, body) = &f.expression.kind else {
    panic!()
  };
  Ok(*body.clone())
}

/// Which engine executes `@cpu` code. GPU orchestration (dispatch, sync
/// tracking, windowing) behaves identically in both; the bytecode VM is much
/// faster at the actual computation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CpuRuntime {
  TreeWalking,
  #[default]
  BytecodeVm,
}

/// Resolve the `@cpu` entry point's *name* with the same selection rules as
/// `pick_entry_point_body`.
fn pick_entry_point_name(
  program: &Program,
  entry_point_name: Option<&str>,
) -> Result<Arc<str>, EvalError> {
  let cpu_fns = program.cpu_entry_points();
  let entry_fn = match entry_point_name {
    Some(name) => cpu_fns
      .iter()
      .find(|f| &*f.read().unwrap().name == name)
      .ok_or_else(|| CpuEntryPointNotFound(name.into()))?,
    None => match cpu_fns.len() {
      0 => return Err(NoCpuEntryPoint.into()),
      1 => &cpu_fns[0],
      _ => return Err(MultipleCpuEntryPoints.into()),
    },
  };
  let name = entry_fn.read().unwrap().name.clone();
  Ok(name)
}

/// The bytecode-VM CPU runtime. Wraps a real `EvaluationEnvironment` — which
/// supplies all GPU-sync, upload/readback, printing, and audio machinery
/// unchanged — around a `BytecodeProgram` that executes the actual `@cpu`
/// code. Fixed-size GPU-bound globals live authoritatively in VM stack
/// slots (mirrored lazily into the env's `Value`s only when an upload needs
/// serialization); runtime-sized globals (unsized arrays, textures) live in
/// the env as `Value`s and are accessed from VM code through host ops.
pub struct VmCpuRuntime<IO: IOManager> {
  pub program: crate::vm::bytecode::BytecodeProgram,
  pub env: EvaluationEnvironment<IO>,
  /// Per-binding flag: VM slots have been written since the env's `Value`
  /// copy was last refreshed. Kept out of the hot path — set only by
  /// compiler-inserted `MarkCpuWritten` host ops.
  slots_dirty: Vec<bool>,
  function_names: Vec<Arc<str>>,
}

struct VmHostView<'a, IO: IOManager> {
  env: &'a mut EvaluationEnvironment<IO>,
  slots_dirty: &'a mut Vec<bool>,
}

impl<IO: IOManager> crate::vm::bytecode::VmHost for VmHostView<'_, IO> {
  type Error = EvalError;
  fn host_call(
    &mut self,
    op: &crate::vm::bytecode::HostOp,
    stack: &mut [u32],
    dyn_memory: &mut [crate::vm::bytecode::DynMemory],
    heap: &mut Vec<Option<Arc<HeapCell>>>,
    heap_free: &mut Vec<u32>,
    shared: crate::vm::bytecode::SharedStateParts<'_>,
    code: &crate::vm::bytecode::Code,
  ) -> Result<Option<crate::vm::bytecode::HostSuspendReason>, EvalError> {
    vm_host_call(
      self.env,
      self.slots_dirty,
      op,
      stack,
      dyn_memory,
      heap,
      heap_free,
      shared,
      code,
    )
  }
}

/// Loads an image file into a `Value::Texture`, resolving relative paths
/// against `source_dir`. Shared by the `load-image` builtin and the VM
/// runtime's `AssignTextureFromImage` host op.
fn load_image_value(
  path: &str,
  source_dir: &Option<PathBuf>,
) -> Result<Value, EvalError> {
  let resolved = if std::path::Path::new(path).is_absolute() {
    std::path::PathBuf::from(path)
  } else if let Some(dir) = source_dir {
    dir.join(path)
  } else {
    std::path::PathBuf::from(path)
  };
  let img = image::open(&resolved)
    .map_err(|e| {
      UserspaceEvalError::RuntimeError(format!(
        "load-image: failed to open \"{path}\": {e}"
      ))
    })?
    .into_rgba8();
  let (width, height) = img.dimensions();
  Ok(Value::Texture {
    width,
    height,
    data: img.into_raw(),
    binding: None,
  })
}

/// Writes RGBA8 pixels to `path` as a PNG (always PNG, whatever the
/// extension), resolving relative paths against `source_dir` like
/// `load_image_value` and creating missing parent directories. Shared by
/// the `save-png` builtin's tree-walker arm and the VM runtime's `SavePng`
/// host op.
fn save_png_file(
  path: &str,
  width: u32,
  height: u32,
  data: &[u8],
  source_dir: &Option<PathBuf>,
) -> Result<(), EvalError> {
  let resolved = if std::path::Path::new(path).is_absolute() {
    std::path::PathBuf::from(path)
  } else if let Some(dir) = source_dir {
    dir.join(path)
  } else {
    std::path::PathBuf::from(path)
  };
  if let Some(parent) = resolved.parent()
    && !parent.as_os_str().is_empty()
  {
    std::fs::create_dir_all(parent).map_err(|e| {
      UserspaceEvalError::RuntimeError(format!(
        "save-png: failed to create directory for \"{path}\": {e}"
      ))
    })?;
  }
  let img = image::RgbaImage::from_raw(width, height, data.to_vec())
    .expect("texture data length doesn't match its dimensions");
  img
    .save_with_format(&resolved, image::ImageFormat::Png)
    .map_err(|e| {
      UserspaceEvalError::RuntimeError(format!(
        "save-png: failed to write \"{path}\": {e}"
      ))
    })?;
  Ok(())
}

/// Loads a `.wav` file as mono f32 samples at the file's native sample
/// rate, resolving relative paths against `source_dir` (multi-channel files
/// are mixed down by averaging). Shared by the `load-wav` builtin's
/// tree-walker arm and the VM runtime's `LoadWav` host op.
fn load_wav_samples(
  path: &str,
  source_dir: &Option<PathBuf>,
) -> Result<Vec<f32>, EvalError> {
  let resolved = if std::path::Path::new(path).is_absolute() {
    std::path::PathBuf::from(path)
  } else if let Some(dir) = source_dir {
    dir.join(path)
  } else {
    std::path::PathBuf::from(path)
  };
  let wav_error = |e: hound::Error| {
    EvalError::from(UserspaceEvalError::RuntimeError(format!(
      "load-wav: failed to read \"{path}\": {e}"
    )))
  };
  let mut reader = hound::WavReader::open(&resolved).map_err(wav_error)?;
  let spec = reader.spec();
  let channels = (spec.channels as usize).max(1);
  let interleaved: Vec<f32> = match spec.sample_format {
    hound::SampleFormat::Float => reader
      .samples::<f32>()
      .collect::<Result<_, _>>()
      .map_err(wav_error)?,
    hound::SampleFormat::Int => {
      let scale = (1i64 << (spec.bits_per_sample - 1)) as f32;
      reader
        .samples::<i32>()
        .map(|s| s.map(|v| v as f32 / scale))
        .collect::<Result<_, _>>()
        .map_err(wav_error)?
    }
  };
  Ok(
    interleaved
      .chunks(channels)
      .map(|frame| frame.iter().sum::<f32>() / channels as f32)
      .collect(),
  )
}

/// Number of u32 words a type occupies in the VM's flat layout.
pub(crate) fn vm_words_of(t: &Type) -> usize {
  t.flat_data_size_in_u32s(&crate::compiler::error::SourceTrace::empty())
    .unwrap() as usize
}

/// Flattens a global's `Value` into the VM word layout used by shared
/// snapshots — arrays element-by-element (a `ZeroedArray` materializes as
/// zero words), everything else via `to_vm_words`.
pub(crate) fn value_to_shared_words(value: &Value, ty: &Type) -> Vec<u32> {
  if let Type::Array(_, element_type) = ty {
    let element_type = element_type.kind.unwrap_known();
    let stride = vm_words_of(&element_type).max(1);
    match value {
      Value::ZeroedArray { length } => vec![0u32; length * stride],
      Value::Array(items) => items
        .iter()
        .flat_map(|item| item.to_vm_words(&element_type))
        .collect(),
      other => panic!("can't publish {other:?} as a shared array"),
    }
  } else {
    value.to_vm_words(ty)
  }
}

/// The reverse of `value_to_shared_words`.
pub(crate) fn shared_words_to_value(words: &[u32], ty: &Type) -> Value {
  if let Type::Array(_, element_type) = ty {
    let element_type = element_type.kind.unwrap_known();
    let stride = vm_words_of(&element_type).max(1);
    Value::Array(
      words
        .chunks(stride)
        .map(|chunk| Value::from_vm_words(&element_type, chunk))
        .collect(),
    )
  } else {
    Value::from_vm_words(ty, words)
  }
}

/// Decodes one heap cell (a runtime-sized array or String payload) into a
/// `Value`. `Cells` children are self-contained `Arc`s, so no heap table
/// is needed — this covers every embedded (container-element) shape.
fn value_from_heap_cell(t: &Type, cell: Option<&Arc<HeapCell>>) -> Value {
  match t {
    Type::String => Value::String(match cell {
      Some(c) => match &c.memory {
        DynMemory::Words(words) => words_to_string(words),
        _ => String::new(),
      },
      None => String::new(),
    }),
    Type::Array(Some(ConcreteArraySize::Unsized), element_type) => {
      let element_type = element_type.kind.unwrap_known();
      let Some(c) = cell else {
        return Value::Array(vec![]);
      };
      match &c.memory {
        DynMemory::Zeroed { elements } => Value::ZeroedArray {
          length: *elements as usize,
        },
        DynMemory::Words(words) => {
          let stride = (vm_stack_size(&element_type) as usize).max(1);
          Value::Array(
            words
              .chunks(stride)
              .map(|chunk| Value::from_vm_words(&element_type, chunk))
              .collect(),
          )
        }
        DynMemory::Cells(children) => Value::Array(
          children
            .iter()
            .map(|child| value_from_heap_cell(&element_type, child.as_ref()))
            .collect(),
        ),
      }
    }
    other => panic!("value_from_heap_cell: unsupported cell type {other:?}"),
  }
}

/// Builds a `Value` view of a dynamic-memory array region. Only used at the
/// boundaries that genuinely need `Value`s — printing, and mirroring into
/// the env for GPU upload serialization; element accesses never build one.
fn dyn_memory_value(
  memory: &crate::vm::bytecode::DynMemory,
  ty: &Type,
) -> Value {
  use crate::vm::bytecode::DynMemory;
  let Type::Array(_, element_type) = ty else {
    panic!("dynamic-memory binding isn't an array")
  };
  let element_type = element_type.kind.unwrap_known();
  match memory {
    DynMemory::Zeroed { elements } => Value::ZeroedArray {
      length: *elements as usize,
    },
    DynMemory::Words(words) => {
      let stride = vm_words_of(&element_type).max(1);
      Value::Array(
        words
          .chunks(stride)
          .map(|chunk| Value::from_vm_words(&element_type, chunk))
          .collect(),
      )
    }
    DynMemory::Cells(children) => Value::Array(
      children
        .iter()
        .map(|child| value_from_heap_cell(&element_type, child.as_ref()))
        .collect(),
    ),
  }
}

/// The reverse of `dyn_memory_value`: flattens an env-side array `Value`
/// (e.g. a fresh GPU readback) into dynamic-memory words.
fn value_into_dyn_memory(
  value: &Value,
  ty: &Type,
) -> crate::vm::bytecode::DynMemory {
  use crate::vm::bytecode::DynMemory;
  let Type::Array(_, element_type) = ty else {
    panic!("dynamic-memory binding isn't an array")
  };
  let element_type = element_type.kind.unwrap_known();
  match value {
    Value::ZeroedArray { length } => DynMemory::Zeroed {
      elements: *length as u32,
    },
    Value::Uninitialized => DynMemory::Zeroed { elements: 0 },
    Value::Array(items) => DynMemory::Words(
      items
        .iter()
        .flat_map(|item| item.to_vm_words(&element_type))
        .collect(),
    ),
    other => panic!("can't store {other:?} in dynamic array memory"),
  }
}

/// Refresh the env's `Value` copies of slot-backed bindings from VM stack
/// slots, for any binding in `bindings` whose slots are dirty. Called before
/// upload serialization so `collect_dirty_uploads` sees current data.
fn refresh_dirty_slots<IO: IOManager>(
  env: &mut EvaluationEnvironment<IO>,
  slots_dirty: &mut [bool],
  bindings: &[u16],
  stack: &[u32],
  dyn_memory: &[crate::vm::bytecode::DynMemory],
  code: &crate::vm::bytecode::Code,
) {
  use crate::vm::bytecode::HostBindingStorage;
  for &index in bindings {
    if !slots_dirty[index as usize] {
      continue;
    }
    let binding = &code.host_bindings[index as usize];
    let value = match binding.storage {
      HostBindingStorage::Slots { position, size } => Value::from_vm_words(
        &binding.ty,
        &stack[position as usize..(position + size) as usize],
      ),
      HostBindingStorage::DynamicMemory { memory } => {
        dyn_memory_value(&dyn_memory[memory as usize], &binding.ty)
      }
      HostBindingStorage::Dynamic => continue,
    };
    if let Some(stack_entry) = env.bindings.get_mut(&binding.name)
      && let Some(slot) = stack_entry.last_mut()
    {
      slot.0 = value;
    }
    slots_dirty[index as usize] = false;
  }
}

/// If the GPU holds the newest value of the given binding (buffer state
/// CPUOutOfDate), reads it back into the env (via `check_cpu_readable`,
/// which flushes queued GPU work first) and mirrors the fresh value into
/// the VM's authoritative storage. Shared by the `CheckGpuToCpu` host op
/// (CPU code about to read the variable) and the frame-boundary shared
/// publish (main proxying GPU writes to other threads).
fn readback_binding_into_vm<IO: IOManager>(
  env: &mut EvaluationEnvironment<IO>,
  slots_dirty: &mut [bool],
  binding: u16,
  stack: &mut [u32],
  dyn_memory: &mut [crate::vm::bytecode::DynMemory],
  code: &crate::vm::bytecode::Code,
) -> Result<(), EvalError> {
  use crate::vm::bytecode::HostBindingStorage;
  let b = &code.host_bindings[binding as usize];
  if env.buffer_states.get(&b.name) == Some(&SharedBufferState::CPUOutOfDate) {
    env.check_cpu_readable(&[b.name.clone()]);
    // Readback landed in the env's Value; mirror it into the VM slots
    // where CPU code actually reads it.
    if env.buffer_states.get(&b.name) == Some(&SharedBufferState::Synced) {
      match b.storage {
        HostBindingStorage::Slots { position, size } => {
          let words = env.lookup(&b.name)?.to_vm_words(&b.ty);
          stack[position as usize..(position + size) as usize]
            .copy_from_slice(&words[..size as usize]);
          slots_dirty[binding as usize] = false;
        }
        HostBindingStorage::DynamicMemory { memory } => {
          dyn_memory[memory as usize] =
            value_into_dyn_memory(env.lookup(&b.name)?, &b.ty);
          slots_dirty[binding as usize] = false;
        }
        HostBindingStorage::Dynamic => {}
      }
    }
  }
  Ok(())
}

/// Builds a `Value` from VM stack words, dereferencing heap ids where the
/// type contains runtime-sized arrays — the heap-aware sibling of
/// `Value::from_vm_words`, needed because a dynamic-array *value* occupies
/// one stack word (a heap id) that only the heap can decode. Types with no
/// runtime-sized content delegate to `from_vm_words`, so flat values print
/// byte-identically to the tree-walking runtime.
fn value_from_vm_words_heap(
  t: &Type,
  words: &[u32],
  heap: &[Option<Arc<HeapCell>>],
) -> Value {
  if !t.involves_runtime_sized_array() && !t.involves_string() {
    return Value::from_vm_words(t, words);
  }
  match t {
    Type::String => {
      Value::String(words_to_string(heap_string_words(heap, words[0])))
    }
    Type::Array(Some(ConcreteArraySize::Unsized), element_type) => {
      let element_type = element_type.kind.unwrap_known();
      let id = words[0];
      if id == 0 {
        return Value::Array(vec![]);
      }
      let cell = heap[id as usize - 1]
        .as_ref()
        .expect("printed heap id references a freed cell");
      match &cell.memory {
        DynMemory::Zeroed { elements } => Value::ZeroedArray {
          length: *elements as usize,
        },
        DynMemory::Words(cell_words) => {
          let stride = (vm_stack_size(&element_type) as usize).max(1);
          Value::Array(
            cell_words
              .chunks(stride)
              .map(|chunk| value_from_vm_words_heap(&element_type, chunk, heap))
              .collect(),
          )
        }
        DynMemory::Cells(children) => Value::Array(
          children
            .iter()
            .map(|child| value_from_heap_cell(&element_type, child.as_ref()))
            .collect(),
        ),
      }
    }
    Type::Array(Some(size), element_type) if size.as_literal().is_some() => {
      let count = size.as_literal().unwrap();
      let element_type = element_type.kind.unwrap_known();
      let stride = (vm_stack_size(&element_type) as usize).max(1);
      Value::Array(
        (0..count as usize)
          .map(|i| {
            value_from_vm_words_heap(
              &element_type,
              &words[i * stride..(i + 1) * stride],
              heap,
            )
          })
          .collect(),
      )
    }
    Type::Struct(s) => {
      let mut offset = 0usize;
      Value::Struct(
        s.fields
          .iter()
          .map(|field| {
            let field_type = field.field_type.unwrap_known();
            let size = vm_stack_size(&field_type) as usize;
            let value = value_from_vm_words_heap(
              &field_type,
              &words[offset..offset + size],
              heap,
            );
            offset += size;
            (field.name.clone(), value)
          })
          .collect(),
      )
    }
    Type::Enum(e) => {
      let discriminant = words[0] as usize;
      let variant = &e.variants[discriminant];
      let inner_type = variant.inner_type.kind.unwrap_known();
      let inner = if inner_type == Type::Unit {
        Value::Unit
      } else {
        let n = vm_stack_size(&inner_type) as usize;
        value_from_vm_words_heap(&inner_type, &words[1..1 + n], heap)
      };
      Value::Enum(variant.name.clone(), Box::new(inner))
    }
    other => panic!(
      "value_from_vm_words_heap: unsupported runtime-sized type {other:?}"
    ),
  }
}

fn vm_host_call<IO: IOManager>(
  env: &mut EvaluationEnvironment<IO>,
  slots_dirty: &mut Vec<bool>,
  op: &crate::vm::bytecode::HostOp,
  stack: &mut [u32],
  dyn_memory: &mut [crate::vm::bytecode::DynMemory],
  heap: &mut Vec<Option<Arc<HeapCell>>>,
  heap_free: &mut Vec<u32>,
  mut shared: crate::vm::bytecode::SharedStateParts<'_>,
  code: &crate::vm::bytecode::Code,
) -> Result<Option<crate::vm::bytecode::HostSuspendReason>, EvalError> {
  let _ = &mut shared;
  use crate::vm::bytecode::{HostBindingStorage, HostOp, HostSuspendReason};
  match op {
    HostOp::Print { slot, ty } => {
      let t = &code.host_types[*ty as usize];
      let n = vm_stack_size(t) as usize;
      let value = value_from_vm_words_heap(
        t,
        &stack[*slot as usize..*slot as usize + n],
        heap,
      );
      let formatted = value.format_for_print(t, env)?;
      env.io.println(&formatted);
    }
    HostOp::Stringify { slot, ty, dest } => {
      let t = &code.host_types[*ty as usize];
      let n = vm_stack_size(t) as usize;
      let value = value_from_vm_words_heap(
        t,
        &stack[*slot as usize..*slot as usize + n],
        heap,
      );
      let formatted = value.format_for_print(t, env)?;
      let cell = Arc::new(HeapCell {
        memory: DynMemory::Words(string_to_words(&formatted)),
        stride: 1,
      });
      release_heap_id(heap, heap_free, stack[*dest as usize]);
      stack[*dest as usize] = alloc_heap_cell(heap, heap_free, cell);
    }
    HostOp::PrintBinding { binding } => {
      let b = &code.host_bindings[*binding as usize];
      let value = match b.storage {
        HostBindingStorage::DynamicMemory { memory } => {
          dyn_memory_value(&dyn_memory[memory as usize], &b.ty)
        }
        _ => env.lookup(&b.name)?.clone(),
      };
      let formatted = value.format_for_print(&b.ty, env)?;
      env.io.println(&formatted);
    }
    HostOp::PrintString { string } => {
      let text = code.host_strings[*string as usize].clone();
      env.io.println(&text);
    }
    HostOp::PrintZeroed { len_slot, ty } => {
      let t = &code.host_types[*ty as usize];
      let value = Value::ZeroedArray {
        length: stack[*len_slot as usize] as usize,
      };
      let formatted = value.format_for_print(t, env)?;
      env.io.println(&formatted);
    }
    HostOp::CheckGpuToCpu { binding } => {
      readback_binding_into_vm(
        env,
        slots_dirty,
        *binding,
        stack,
        dyn_memory,
        code,
      )?;
    }
    HostOp::MarkCpuWritten { binding } => {
      let b = &code.host_bindings[*binding as usize];
      if matches!(
        b.storage,
        HostBindingStorage::Slots { .. }
          | HostBindingStorage::DynamicMemory { .. }
      ) {
        slots_dirty[*binding as usize] = true;
      }
      let name = b.name.clone();
      env.mark_cpu_written(&[name]);
    }
    HostOp::DispatchCompute {
      entry,
      sets,
      workgroup_slot,
    } => {
      let dispatch = &code.host_dispatches[*sets as usize];
      refresh_dirty_slots(
        env,
        slots_dirty,
        &dispatch.reads,
        stack,
        dyn_memory,
        code,
      );
      let read_names: Vec<Arc<str>> = dispatch
        .reads
        .iter()
        .map(|&i| code.host_bindings[i as usize].name.clone())
        .collect();
      let written_names: Vec<Arc<str>> = dispatch
        .writes
        .iter()
        .map(|&i| code.host_bindings[i as usize].name.clone())
        .collect();
      let ws = *workgroup_slot as usize;
      let workgroup_count = (stack[ws], stack[ws + 1], stack[ws + 2]);
      env.setup_gpu_if_needed();
      let pre_upload = env.collect_dirty_uploads(&read_names);
      let entry_name = &code.host_strings[*entry as usize];
      env.io.record_compute(
        env.gpu_entry_id(entry_name),
        entry_name,
        workgroup_count,
        pre_upload,
      )?;
      env.mark_gpu_written(&written_names);
    }
    HostOp::DispatchRender {
      vert,
      frag,
      sets,
      vert_count_slot,
      additive_slot,
    } => {
      let dispatch = &code.host_dispatches[*sets as usize];
      refresh_dirty_slots(
        env,
        slots_dirty,
        &dispatch.reads,
        stack,
        dyn_memory,
        code,
      );
      let read_names: Vec<Arc<str>> = dispatch
        .reads
        .iter()
        .map(|&i| code.host_bindings[i as usize].name.clone())
        .collect();
      let written_names: Vec<Arc<str>> = dispatch
        .writes
        .iter()
        .map(|&i| code.host_bindings[i as usize].name.clone())
        .collect();
      let vert_count = stack[*vert_count_slot as usize];
      let additive = additive_slot
        .map(|slot| stack[slot as usize] != 0)
        .unwrap_or(false);
      env.setup_gpu_if_needed();
      let mut pre_upload = env.collect_dirty_uploads(&read_names);
      let render_target =
        env.current_render_target.map(|gb| (gb.group, gb.binding));
      // If rendering to an offscreen texture, also upload it now so the GPU
      // has the correctly-sized texture to render into (mirrors the
      // tree-walking handler).
      if let Some((rt_group, rt_binding)) = render_target {
        if let Some((_, name, _, _)) =
          env.binding_vars.iter().find(|(gb, _, _, addr)| {
            gb.group == rt_group
              && gb.binding == rt_binding
              && *addr == VariableAddressSpace::Handle
          })
        {
          let name = name.clone();
          if env.buffer_states.get(&name)
            == Some(&SharedBufferState::GPUOutOfDate)
          {
            let extra = env.collect_dirty_uploads(&[name.clone()]);
            pre_upload.extend(extra);
          }
        }
      }
      let vert_name = &code.host_strings[*vert as usize];
      let frag_name = &code.host_strings[*frag as usize];
      env.io.record_draw(
        env.gpu_entry_id(vert_name),
        env.gpu_entry_id(frag_name),
        vert_name,
        frag_name,
        vert_count,
        pre_upload,
        additive,
        render_target,
      )?;
      env.mark_gpu_written(&written_names);
      if let Some((rt_group, rt_binding)) = render_target {
        if let Some((_, name, _, _)) =
          env.binding_vars.iter().find(|(gb, _, _, _)| {
            gb.group == rt_group && gb.binding == rt_binding
          })
        {
          let name = name.clone();
          env.mark_gpu_written(&[name]);
        }
      }
    }
    HostOp::WindowQuery { kind, dest } => {
      use crate::vm::bytecode::WindowQueryKind;
      let d = *dest as usize;
      match kind {
        WindowQueryKind::Resolution => {
          let (w, h) = env.io.window_size();
          stack[d] = w;
          stack[d + 1] = h;
        }
        WindowQueryKind::Time => stack[d] = env.io.window_time().to_bits(),
        WindowQueryKind::DeltaTime => {
          stack[d] = env.io.window_delta_time().to_bits()
        }
        WindowQueryKind::FrameIndex => stack[d] = env.io.window_frame_index(),
        WindowQueryKind::MouseCoords => {
          let (x, y) = env.io.mouse_coords();
          stack[d] = x;
          stack[d + 1] = y;
        }
        WindowQueryKind::MousePresent => {
          stack[d] = env.io.mouse_present() as u32
        }
        WindowQueryKind::MouseDown => stack[d] = env.io.mouse_down() as u32,
        WindowQueryKind::MouseJustDown => {
          stack[d] = env.io.mouse_just_down() as u32
        }
      }
    }
    HostOp::KeyQuery { just, key, dest } => {
      let key = &code.host_strings[*key as usize];
      stack[*dest as usize] = if *just {
        env.io.key_just_down(key)
      } else {
        env.io.key_down(key)
      } as u32;
    }
    HostOp::SpawnWindow { frame_fn } => {
      return Ok(Some(HostSuspendReason::SpawnWindow {
        frame_fn: *frame_fn,
      }));
    }
    HostOp::CloseWindow => {
      env.io.record_close_window();
      return Ok(Some(HostSuspendReason::CloseWindow));
    }
    HostOp::StartAudio { entry } => {
      let entry_name = code.host_strings[*entry as usize].clone();
      #[cfg(feature = "window")]
      {
        let mut source = env.audio_source.take();
        // First call (we hold the source): activate the shared table and
        // bootstrap-publish every shared global from the VM replica, so the
        // new replica's first adopt sees the current state of everything
        // (e.g. `load-wav`ed sample buffers). From here on both threads
        // publish/adopt at their iteration boundaries — later writes on
        // either side propagate.
        if let Some(crate::audio::AudioSource::Bytecode {
          shared_table, ..
        }) = &mut source
        {
          let table = env.shared_table.clone();
          table.join(participant::AUDIO);
          // GPU-proxy pass: a shared binding whose newest value lives on
          // the GPU must be read back before the bootstrap publish, so the
          // audio replica's first adopt sees the true current state (the
          // tree-walker bootstrap gets this from `publish_shared_globals`).
          for info in code.shared_vars.iter() {
            if env.buffer_states.get(&info.name)
              != Some(&SharedBufferState::CPUOutOfDate)
            {
              continue;
            }
            if let Some(binding) =
              code.host_bindings.iter().position(|b| b.name == info.name)
            {
              readback_binding_into_vm(
                env,
                slots_dirty,
                binding as u16,
                stack,
                dyn_memory,
                code,
              )?;
            }
          }
          crate::vm::shared_sync::publish_shared(
            stack,
            dyn_memory,
            &mut shared,
            &code.shared_vars,
            &table,
            participant::MAIN,
            participant::AUDIO,
            |index| {
              env
                .io
                .record_shared_publish(&code.shared_vars[index as usize].name)
            },
          );
          *shared_table = Some(table);
        }
        env.io.start_audio(&entry_name, source)?;
      }
      #[cfg(not(feature = "window"))]
      {
        let _ = entry_name;
        return Err(WindowFeatureNotEnabled.into());
      }
    }
    HostOp::LoadWav { path, dest } => {
      let path = code.host_strings[*path as usize].clone();
      let samples = load_wav_samples(&path, &env.source_dir)?;
      let cell = Arc::new(HeapCell {
        memory: DynMemory::Words(
          samples.into_iter().map(f32::to_bits).collect(),
        ),
        stride: 1,
      });
      release_heap_id(heap, heap_free, stack[*dest as usize]);
      stack[*dest as usize] = alloc_heap_cell(heap, heap_free, cell);
    }
    HostOp::AssignTextureFromImage { binding, path } => {
      let b = &code.host_bindings[*binding as usize];
      let path = code.host_strings[*path as usize].clone();
      let value = load_image_value(&path, &env.source_dir)?;
      let name = b.name.clone();
      if let Some(stack_entry) = env.bindings.get_mut(&name)
        && let Some(slot) = stack_entry.last_mut()
      {
        slot.0 = value;
      }
    }
    HostOp::AssignTextureBlank { binding, size_slot } => {
      let b = &code.host_bindings[*binding as usize];
      let width = stack[*size_slot as usize];
      let height = stack[*size_slot as usize + 1];
      let name = b.name.clone();
      if let Some(stack_entry) = env.bindings.get_mut(&name)
        && let Some(slot) = stack_entry.last_mut()
      {
        slot.0 = Value::Texture {
          width,
          height,
          data: vec![0u8; (width * height * 4) as usize],
          binding: None,
        };
      }
    }
    HostOp::TextureDims { binding, dest } => {
      let b = &code.host_bindings[*binding as usize];
      let Value::Texture { width, height, .. } = env.lookup(&b.name)? else {
        panic!("texture-dimensions: expected Texture value")
      };
      stack[*dest as usize] = *width;
      stack[*dest as usize + 1] = *height;
    }
    HostOp::SetRenderTarget { binding } => {
      let b = &code.host_bindings[*binding as usize];
      let (group, binding, _) =
        b.gpu.expect("set-render-target on an unbound texture");
      env.current_render_target = Some(GroupAndBinding { group, binding });
    }
    HostOp::ClearRenderTarget => {
      env.current_render_target = None;
    }
    HostOp::SavePng { binding, path } => {
      let b = &code.host_bindings[*binding as usize];
      let path = code.host_strings[*path as usize].clone();
      let texture = env.lookup(&b.name)?.clone();
      let Value::Texture {
        width,
        height,
        data,
        ..
      } = env.refresh_texture_from_gpu(texture)?
      else {
        panic!("save-png: expected Texture value")
      };
      save_png_file(&path, width, height, &data, &env.source_dir)?;
    }
  }
  Ok(None)
}

/// Bytecode-VM frame driver: runs the compiled frame function once per
/// frame. The frame closure's captured scope lives in the function's
/// argument slots, which persist across frames.
/// VM-runtime counterpart of `refresh_window_info_bindings`: window-info
/// bindings are fixed-size GPU-bound globals, so their authoritative values
/// live in VM stack slots. Writes the fresh words into the slots, marks the
/// binding's env mirror stale, and marks it CPU-written so the next
/// dispatch uploads it.
fn refresh_vm_window_info<IO: IOManager>(
  program: &mut crate::vm::bytecode::BytecodeProgram,
  env: &mut EvaluationEnvironment<IO>,
  slots_dirty: &mut Vec<bool>,
) {
  if env.window_info_bindings.is_empty() {
    return;
  }
  let infos = env.window_info_bindings.clone();
  for (source, name, _) in infos {
    let words = window_info_words(&source, &env.io);
    program.write_global(&name, &words);
    if let Some(index) = program
      .code
      .host_bindings
      .iter()
      .position(|binding| *binding.name == *name)
      && let Some(flag) = slots_dirty.get_mut(index)
    {
      *flag = true;
    }
    env.mark_cpu_written(&[name]);
  }
}

struct VmFrameDriver<'a, IO: IOManager> {
  program: &'a mut crate::vm::bytecode::BytecodeProgram,
  env: &'a mut EvaluationEnvironment<IO>,
  slots_dirty: &'a mut Vec<bool>,
  frame_fn: usize,
}

/// Adopts newer cross-thread snapshots into the VM replica at a main-thread
/// boundary, keeping the env's GPU sync state coherent: an adopted
/// GPU-bound global means the GPU's copy is now stale (GPUOutOfDate — set
/// directly, not via `mark_cpu_written`, which would re-dirty the shared
/// flag and ping-pong the value back at the next boundary) and the env's
/// mirror `Value` is stale relative to the VM's authoritative storage
/// (`slots_dirty`). Shared by the frame driver and the entry-start
/// bootstrap.
fn vm_adopt_shared<IO: IOManager>(
  program: &mut crate::vm::bytecode::BytecodeProgram,
  env: &mut EvaluationEnvironment<IO>,
  slots_dirty: &mut Vec<bool>,
) {
  let table = env.shared_table.clone();
  if table.live_others(participant::MAIN) == 0 {
    return;
  }
  // shared index -> (name, host-binding index, GPU-bound?), resolved
  // before the adopt call so the hook can touch env state while the
  // program is mutably borrowed. Only runs once another participant
  // exists.
  let coherence: Vec<(Arc<str>, Option<(usize, bool)>)> = program
    .code
    .shared_vars
    .iter()
    .map(|info| {
      (
        info.name.clone(),
        program
          .code
          .host_bindings
          .iter()
          .position(|b| b.name == info.name)
          .map(|j| (j, program.code.host_bindings[j].gpu.is_some())),
      )
    })
    .collect();
  program.adopt_shared(&table, participant::MAIN, |index| {
    let (name, host_binding) = &coherence[index as usize];
    if let Some((j, gpu_bound)) = host_binding {
      slots_dirty[*j] = true;
      if *gpu_bound {
        env
          .buffer_states
          .insert(name.clone(), SharedBufferState::GPUOutOfDate);
      }
    }
    env.io.record_shared_adopt(name);
  });
}

/// Publishes the VM replica's dirty shared globals at a main-thread
/// boundary (plus any var whose audience intersects `force_mask`). First
/// runs the GPU-proxy pass: any *audible* shared binding whose newest
/// value lives on the GPU (CPUOutOfDate) is read back into the VM replica
/// and force-published — the GPU has no boundary of its own, so this is
/// what makes GPU writes visible to the audio thread (see
/// `publish_shared_globals` for the tree-walker equivalent).
fn vm_publish_shared<IO: IOManager>(
  program: &mut crate::vm::bytecode::BytecodeProgram,
  env: &mut EvaluationEnvironment<IO>,
  slots_dirty: &mut Vec<bool>,
  force_mask: u32,
) -> Result<(), EvalError> {
  let table = env.shared_table.clone();
  let live_others = table.live_others(participant::MAIN);
  if live_others == 0 {
    return Ok(());
  }
  for index in 0..program.code.shared_vars.len() {
    let info = &program.code.shared_vars[index];
    if info.audience & live_others == 0 {
      continue;
    }
    let name = &info.name;
    if env.buffer_states.get(name) != Some(&SharedBufferState::CPUOutOfDate) {
      continue;
    }
    let Some(binding) = program
      .code
      .host_bindings
      .iter()
      .position(|b| &b.name == name)
    else {
      continue;
    };
    readback_binding_into_vm(
      env,
      slots_dirty,
      binding as u16,
      &mut program.stack,
      &mut program.dyn_memory,
      &program.code,
    )?;
    program.shared_dirty[index] = true;
  }
  let shared_names: Vec<Arc<str>> = program
    .code
    .shared_vars
    .iter()
    .map(|info| info.name.clone())
    .collect();
  program.publish_shared(&table, participant::MAIN, force_mask, |index| {
    env.io.record_shared_publish(&shared_names[index as usize]);
  });
  Ok(())
}

/// Entry-start bootstrap for embedder-facing (`@external`) vars, VM side:
/// adopt anything the handle pre-seeded, so even code before the first
/// frame boundary (and frame 0's dispatches) sees the embedder's values.
/// `@external` vars live in GPU-space address spaces and so can't have
/// initializers — seeding is the embedder's job, through the handle.
/// No-op unless an external handle is live.
fn vm_bootstrap_external<IO: IOManager>(
  program: &mut crate::vm::bytecode::BytecodeProgram,
  env: &mut EvaluationEnvironment<IO>,
  slots_dirty: &mut Vec<bool>,
) -> Result<(), EvalError> {
  let table = env.shared_table.clone();
  if table.live_others(participant::MAIN) & participant::EXTERNAL == 0 {
    return Ok(());
  }
  vm_adopt_shared(program, env, slots_dirty);
  Ok(())
}

impl<IO: IOManager> FrameDriver for VmFrameDriver<'_, IO> {
  type IO = IO;
  fn io_mut(&mut self) -> &mut IO {
    &mut self.env.io
  }
  fn wgsl(&self) -> &str {
    self.env.wgsl()
  }
  fn binding_infos(&self) -> Vec<GpuBindingInfo> {
    self.env.binding_infos()
  }
  fn gpu_entries(&self) -> Vec<GpuEntryInfo> {
    self.env.gpu_entries.clone()
  }
  fn run_frame(&mut self) -> Result<(), EvalException> {
    use crate::vm::bytecode::{HostSuspendReason, RunResult};
    vm_adopt_shared(self.program, self.env, self.slots_dirty);
    refresh_vm_window_info(self.program, self.env, self.slots_dirty);
    self.program.prepare_to_run_function(self.frame_fn);
    let mut host = VmHostView {
      env: self.env,
      slots_dirty: self.slots_dirty,
    };
    let result = match self.program.execute_with_host(&mut host) {
      Ok(RunResult::Finished) => Ok(()),
      Ok(RunResult::Suspended(HostSuspendReason::CloseWindow)) => {
        Err(EvalException::CloseWindow)
      }
      Ok(RunResult::Suspended(HostSuspendReason::SpawnWindow { .. })) => {
        Err(EvalException::Error(
          UserspaceEvalError::RuntimeError(
            "nested spawn-window is not supported".to_string(),
          )
          .into(),
        ))
      }
      Err(e) => Err(EvalException::Error(e)),
    };
    // Publish on success and on close-window (the frame's writes are still
    // real); genuine errors abort the run, so skip the publish.
    if matches!(result, Ok(()) | Err(EvalException::CloseWindow))
      && let Err(e) =
        vm_publish_shared(self.program, self.env, self.slots_dirty, 0)
    {
      return Err(EvalException::Error(e));
    }
    result
  }
  fn overwrite_binding_bytes(&mut self, group: u8, binding: u8, bytes: &[u8]) {
    use crate::vm::bytecode::HostBindingStorage;
    if !self
      .env
      .overwrite_binding_from_gpu_bytes(group, binding, bytes)
    {
      return;
    }
    // The env's `Value` copy is a mirror; write through to the
    // VM-authoritative storage too (the same direction as a GPU readback).
    for (index, host_binding) in
      self.program.code.host_bindings.iter().enumerate()
    {
      let Some((g, b, _)) = host_binding.gpu else {
        continue;
      };
      if g != group || b != binding {
        continue;
      }
      let Ok(value) = self.env.lookup(&host_binding.name) else {
        continue;
      };
      match host_binding.storage {
        HostBindingStorage::Slots { position, size } => {
          let words = value.to_vm_words(&host_binding.ty);
          let n = (size as usize).min(words.len());
          self.program.stack[position as usize..position as usize + n]
            .copy_from_slice(&words[..n]);
          self.slots_dirty[index] = false;
        }
        HostBindingStorage::DynamicMemory { memory } => {
          self.program.dyn_memory[memory as usize] =
            value_into_dyn_memory(value, &host_binding.ty);
          self.slots_dirty[index] = false;
        }
        HostBindingStorage::Dynamic => {}
      }
    }
  }
}

impl<IO: IOManager> VmCpuRuntime<IO> {
  /// Builds the VM CPU runtime from an already-validated `Program`. The
  /// program is compiled twice from the same validated form: once to WGSL +
  /// an `EvaluationEnvironment` (for all GPU/host machinery) and once to
  /// CPU-mode bytecode (for execution).
  pub fn new(
    program: Program,
    io: IO,
    source_dir: Option<PathBuf>,
    #[cfg(feature = "window")] audio_source: Option<crate::audio::AudioSource>,
  ) -> Result<Self, EvalError> {
    Self::new_with_external(
      program,
      io,
      source_dir,
      #[cfg(feature = "window")]
      audio_source,
      None,
    )
  }

  /// [`Self::new`] with an [`ExternalVars`] handle whose
  /// table the runtime will share with the embedder.
  pub fn new_with_external(
    program: Program,
    io: IO,
    source_dir: Option<PathBuf>,
    #[cfg(feature = "window")] audio_source: Option<crate::audio::AudioSource>,
    external_vars: Option<Arc<ExternalVars>>,
  ) -> Result<Self, EvalError> {
    let env_program = program.clone();
    #[cfg(feature = "window")]
    let env =
      EvaluationEnvironment::from_program_with_audio_source_and_external(
        env_program,
        io,
        source_dir,
        audio_source,
        external_vars,
      )?;
    #[cfg(not(feature = "window"))]
    let env = EvaluationEnvironment::build_inner(
      env_program,
      io,
      source_dir,
      external_vars,
    )?;
    let (mut vm_program, function_names) =
      program.compile_to_bytecode_program_cpu();
    // Main's copy of the implicit `easl_sample_rate` local (see
    // `Program::extract_audio_info`) — the tree-walker env's copy is
    // seeded at env construction, but the VM reads its own slots.
    let main_sample_rate = env.io.sample_rate();
    vm_program.write_global("easl_sample_rate", &[main_sample_rate.to_bits()]);
    let slots_dirty = vec![false; vm_program.code.host_bindings.len()];
    Ok(Self {
      program: vm_program,
      env,
      slots_dirty,
      function_names,
    })
  }

  /// Runs the `@cpu` entry point named `entry_name` to completion,
  /// including any `spawn-window` frame loops. Returns `true` if a
  /// hot-reload was requested.
  pub fn run(&mut self, entry_name: &str) -> Result<bool, EvalError> {
    use crate::vm::bytecode::{HostSuspendReason, RunResult};
    let entry_index = self
      .function_names
      .iter()
      .position(|n| &**n == entry_name)
      .ok_or_else(|| CpuEntryPointNotFound(entry_name.into()))?;
    vm_bootstrap_external(
      &mut self.program,
      &mut self.env,
      &mut self.slots_dirty,
    )?;
    refresh_vm_window_info(
      &mut self.program,
      &mut self.env,
      &mut self.slots_dirty,
    );
    self.program.prepare_to_run_function(entry_index);
    loop {
      let result = {
        let mut host = VmHostView {
          env: &mut self.env,
          slots_dirty: &mut self.slots_dirty,
        };
        self.program.execute_with_host(&mut host)?
      };
      match result {
        RunResult::Finished => return Ok(false),
        RunResult::Suspended(HostSuspendReason::CloseWindow) => {
          // close-window outside a window: nothing left to do.
          return Ok(false);
        }
        RunResult::Suspended(HostSuspendReason::SpawnWindow { frame_fn }) => {
          // Stash the suspended continuation of the entry function, run the
          // window loop (each frame re-executes the frame function), then
          // restore and resume.
          let saved_continuation = std::mem::take(&mut self.program.call_stack);
          let reload = {
            let mut driver = VmFrameDriver {
              program: &mut self.program,
              env: &mut self.env,
              slots_dirty: &mut self.slots_dirty,
              frame_fn: frame_fn as usize,
            };
            IO::run_spawn_window_driver(&mut driver)?
          };
          self.program.call_stack = saved_continuation;
          if reload {
            return Ok(true);
          }
        }
      }
    }
  }
}

/// Runs a validated program's `@cpu` entry on the bytecode VM. The
/// counterpart of `run_program_with` for `CpuRuntime::BytecodeVm`.
fn run_program_vm_with<IO: IOManager>(
  program: Program,
  entry_point_name: Option<&str>,
  io: IO,
  source_dir: Option<PathBuf>,
  #[cfg(feature = "window")] audio_source: Option<crate::audio::AudioSource>,
) -> Result<(IO, bool), EvalError> {
  run_program_vm_with_external(
    program,
    entry_point_name,
    io,
    source_dir,
    #[cfg(feature = "window")]
    audio_source,
    None,
  )
}

fn run_program_vm_with_external<IO: IOManager>(
  program: Program,
  entry_point_name: Option<&str>,
  io: IO,
  source_dir: Option<PathBuf>,
  #[cfg(feature = "window")] audio_source: Option<crate::audio::AudioSource>,
  external_vars: Option<Arc<ExternalVars>>,
) -> Result<(IO, bool), EvalError> {
  let entry_name = pick_entry_point_name(&program, entry_point_name)?;
  let mut runtime = VmCpuRuntime::new_with_external(
    program,
    io,
    source_dir,
    #[cfg(feature = "window")]
    audio_source,
    external_vars,
  )?;
  let reload = runtime.run(&entry_name)?;
  Ok((runtime.env.io, reload))
}

/// Runs a validated program's `@cpu` entry on the chosen runtime.
pub fn run_program_with_runtime<IO: IOManager>(
  program: Program,
  entry_point_name: Option<&str>,
  io: IO,
  source_dir: Option<PathBuf>,
  runtime: CpuRuntime,
) -> Result<(IO, bool), EvalError> {
  match runtime {
    CpuRuntime::TreeWalking => {
      run_program_with(program, entry_point_name, io, source_dir)
    }
    CpuRuntime::BytecodeVm => run_program_vm_with(
      program,
      entry_point_name,
      io,
      source_dir,
      #[cfg(feature = "window")]
      None,
    ),
  }
}

/// Runs on the default runtime with a pre-compiled audio source (the path
/// the CLI-facing entry points take).
#[cfg(feature = "window")]
fn run_program_default_runtime_with_audio<IO: IOManager>(
  program: Program,
  entry_point_name: Option<&str>,
  io: IO,
  source_dir: Option<PathBuf>,
  audio_source: Option<crate::audio::AudioSource>,
) -> Result<(IO, bool), EvalError> {
  match CpuRuntime::default() {
    CpuRuntime::TreeWalking => run_program_with_audio_source(
      program,
      entry_point_name,
      io,
      source_dir,
      audio_source,
      None,
    ),
    CpuRuntime::BytecodeVm => run_program_vm_with(
      program,
      entry_point_name,
      io,
      source_dir,
      audio_source,
    ),
  }
}

/// `run_program_with_capture_from_path` with an explicit runtime choice.
pub fn run_program_with_capture_and_runtime_from_path(
  program: Program,
  source_path: &std::path::Path,
  runtime: CpuRuntime,
) -> Result<Vec<String>, EvalError> {
  let source_dir = source_path.parent().map(|p| p.to_path_buf());
  let (io, _) = run_program_with_runtime(
    program,
    None,
    CaptureIO::new(),
    source_dir,
    runtime,
  )?;
  Ok(io.prints)
}

/// `run_program_capturing_io_from_path` with an explicit runtime choice.
pub fn run_program_capturing_io_with_runtime_from_path(
  program: Program,
  source_path: &std::path::Path,
  runtime: CpuRuntime,
) -> Result<CaptureIO, EvalError> {
  let source_dir = source_path.parent().map(|p| p.to_path_buf());
  Ok(
    run_program_with_runtime(
      program,
      None,
      CaptureIO::new(),
      source_dir,
      runtime,
    )?
    .0,
  )
}

/// `run_program_test_io` with an explicit runtime choice.
pub fn run_program_test_io_with_runtime(
  program: Program,
  runtime: CpuRuntime,
) -> Result<StringIO, EvalError> {
  Ok(run_program_with_runtime(program, None, StringIO::new(), None, runtime)?.0)
}

pub fn run_program(program: Program) -> Result<(), EvalError> {
  run_program_with_runtime(
    program,
    None,
    StdoutIO::new(),
    None,
    CpuRuntime::default(),
  )?;
  Ok(())
}

pub fn run_program_entry(
  program: Program,
  entry: Option<&str>,
) -> Result<(), EvalError> {
  run_program_with_runtime(
    program,
    entry,
    StdoutIO::new(),
    None,
    CpuRuntime::default(),
  )?;
  Ok(())
}

pub fn run_program_entry_from_path(
  program: Program,
  entry: Option<&str>,
  source_path: &std::path::Path,
) -> Result<(), EvalError> {
  let source_dir = source_path.parent().map(|p| p.to_path_buf());
  #[cfg(feature = "window")]
  {
    let audio_source = try_compile_audio_source(
      &program,
      source_path,
      crate::audio::AudioBackend::default(),
    );
    run_program_default_runtime_with_audio(
      program,
      entry,
      StdoutIO::new(),
      source_dir,
      audio_source,
    )?;
  }
  #[cfg(not(feature = "window"))]
  {
    let _ = source_path;
    run_program_with_runtime(
      program,
      entry,
      StdoutIO::new(),
      source_dir,
      CpuRuntime::default(),
    )?;
  }
  Ok(())
}

/// If the validated `program` contains at least one `@audio` entry point,
/// compile it for `audio_backend`. Returns `None` if there are no audio
/// entry points, or if compilation fails — failures are logged to stderr so
/// the rest of the run can proceed.
#[cfg(feature = "window")]
fn try_compile_audio_source(
  program: &Program,
  #[cfg_attr(not(feature = "c_audio"), allow(unused_variables))]
  source_path: &std::path::Path,
  audio_backend: crate::audio::AudioBackend,
) -> Option<crate::audio::AudioSource> {
  let has_audio_entry = !program
    .find_fn_names_by_entry_point(|e| e == EntryPoint::Audio)
    .is_empty();
  if !has_audio_entry {
    return None;
  }
  match audio_backend {
    crate::audio::AudioBackend::VM => try_compile_audio_bytecode(program),
    crate::audio::AudioBackend::C => {
      #[cfg(not(feature = "c_audio"))]
      panic!(
        "AudioBackend::C was selected, but the `c_audio` cargo feature is \
         not enabled. Either enable `c_audio` or use AudioBackend::VM (the \
         default)."
      );
      #[cfg(feature = "c_audio")]
      {
        // Closure audio entries rely on the thread-shared table to carry
        // their captured state (seeded lifted globals); the C path has
        // compile-time-baked globals and no table, so the captures would
        // be silent zeros. The lifted vars' `_audio_data_` infix is
        // compiler-generated, so its presence identifies a closure entry.
        if program
          .top_level_vars
          .iter()
          .any(|v| v.name.contains("_audio_data_"))
        {
          panic!(
            "The C audio backend doesn't support closure audio entries \
             (their captured state travels through the thread-shared \
             table, which the C path doesn't have); use AudioBackend::VM."
          );
        }
        try_compile_audio_c_source(program, source_path)
          .map(crate::audio::AudioSource::C)
      }
    }
  }
}

/// Clone `program` and compile it to a `BytecodeProgram` so the
/// `start-audio` builtin can run the audio entry function on the VM. The
/// program is assumed to have already been validated by the caller — every
/// public `run_program_entry_*` entry point hands us a Program that the
/// interpreter is already prepared to walk, which is the same precondition
/// `compile_to_bytecode_program` needs. (`validate_raw_program` is not
/// idempotent — its later passes lower `Ownership::Reference` to
/// `Ownership::Pointer(_)`, and an earlier pass on a re-run would panic on
/// the resulting Pointer.)
#[cfg(feature = "window")]
fn try_compile_audio_bytecode(
  program: &Program,
) -> Option<crate::audio::AudioSource> {
  let p = program.clone();
  let (bytecode_program, function_names) = p.compile_to_bytecode_program();
  Some(crate::audio::AudioSource::Bytecode {
    program: bytecode_program,
    function_names,
    // Attached by the `start-audio` builtin, which owns the activation +
    // bootstrap-publish sequence.
    shared_table: None,
  })
}

/// Appends one fixed-signature wrapper per audio entry to the compiled C
/// source: the C driver always calls a `fn(float, float) -> float`
/// pointer, so the wrapper stores the ambient audio info into the
/// implicit `easl_audio_time`/`easl_sample_rate` globals (when the
/// program uses them — the fixed names are what make this generable
/// against a separate compilation of the same source) and forwards `t`
/// to the actual 0- or 1-arg entry.
#[cfg(all(feature = "window", feature = "c_audio"))]
fn append_c_audio_wrappers(program: &Program, c_source: &mut String) {
  let has_var =
    |name: &str| program.top_level_vars.iter().any(|v| &*v.name == name);
  let stores = format!(
    "{}{}",
    if has_var("easl_audio_time") {
      "  easl_audio_time = t;\n"
    } else {
      ""
    },
    if has_var("easl_sample_rate") {
      "  easl_sample_rate = rate;\n"
    } else {
      ""
    }
  );
  for name in program.find_fn_names_by_entry_point(|e| e == EntryPoint::Audio) {
    let arg_count = program
      .abstract_functions
      .get(name.as_str())
      .into_iter()
      .flatten()
      .next()
      .map(|signature| signature.read().unwrap().arg_types.len())
      .unwrap_or(0);
    let entry_c = name.replace('-', "_");
    let call = if arg_count > 0 {
      format!("{entry_c}(t)")
    } else {
      format!("{entry_c}()")
    };
    c_source.push_str(&format!(
      "\nfloat {entry_c}_easl_audio_wrapper(float t, float rate) {{\n\
       {stores}  (void)rate;\n  return {call};\n}}\n"
    ));
  }
}

#[cfg(all(feature = "window", feature = "c_audio"))]
fn try_compile_audio_c_source(
  program: &Program,
  source_path: &std::path::Path,
) -> Option<String> {
  match compile_easl_file_to_target(source_path, CompilerTarget::C) {
    Ok(Ok(Ok(mut c_source))) => {
      append_c_audio_wrappers(program, &mut c_source);
      Some(c_source)
    }
    Ok(Ok(Err((documents, errors)))) => {
      eprintln!(
        "Note: failed to compile program to C for audio support:\n{}",
        errors.describe(&documents)
      );
      None
    }
    Ok(Err(_)) => {
      eprintln!(
        "Note: failed to re-parse source for audio C compilation (parse error)"
      );
      None
    }
    Err(e) => {
      eprintln!("Note: failed to re-read source for audio C compilation: {e}");
      None
    }
  }
}

/// Runs the program with a caller-supplied IO manager and returns it together
/// with a flag indicating whether a hot-reload was requested (`true`) or the
/// program finished normally (`false`).
pub fn run_program_entry_with_io<IO: IOManager>(
  program: Program,
  entry: Option<&str>,
  io: IO,
) -> Result<(IO, bool), EvalError> {
  run_program_with(program, entry, io, None)
}

pub fn run_program_entry_with_io_from_path<IO: IOManager>(
  program: Program,
  entry: Option<&str>,
  io: IO,
  source_path: &std::path::Path,
) -> Result<(IO, bool), EvalError> {
  let source_dir = source_path.parent().map(|p| p.to_path_buf());
  #[cfg(feature = "window")]
  {
    let audio_source = try_compile_audio_source(
      &program,
      source_path,
      crate::audio::AudioBackend::default(),
    );
    run_program_default_runtime_with_audio(
      program,
      entry,
      io,
      source_dir,
      audio_source,
    )
  }
  #[cfg(not(feature = "window"))]
  {
    let _ = source_path;
    run_program_with_runtime(
      program,
      entry,
      io,
      source_dir,
      CpuRuntime::default(),
    )
  }
}

/// `run_program_entry_with_io_from_path` with an explicit runtime choice.
/// Like it — and unlike the source-string-based runners — this compiles the
/// audio source eagerly when the program has an `@audio` entry point.
pub fn run_program_entry_with_io_and_runtime_from_path<IO: IOManager>(
  program: Program,
  entry: Option<&str>,
  io: IO,
  source_path: &std::path::Path,
  runtime: CpuRuntime,
) -> Result<(IO, bool), EvalError> {
  run_program_entry_with_io_runtime_and_external_from_path(
    program,
    entry,
    io,
    source_path,
    runtime,
    None,
  )
}

/// `run_program_entry_with_io_and_runtime_from_path` with an
/// [`ExternalVars`] handle, letting an embedder read and
/// write the program's `@external` globals while it runs. The handle must
/// have been created (via `ExternalVars::new`) from the same validated
/// `Program`.
pub fn run_program_entry_with_io_runtime_and_external_from_path<
  IO: IOManager,
>(
  program: Program,
  entry: Option<&str>,
  io: IO,
  source_path: &std::path::Path,
  runtime: CpuRuntime,
  external_vars: Option<Arc<ExternalVars>>,
) -> Result<(IO, bool), EvalError> {
  let source_dir = source_path.parent().map(|p| p.to_path_buf());
  #[cfg(feature = "window")]
  {
    let audio_source = try_compile_audio_source(
      &program,
      source_path,
      crate::audio::AudioBackend::default(),
    );
    match runtime {
      CpuRuntime::TreeWalking => run_program_with_audio_source(
        program,
        entry,
        io,
        source_dir,
        audio_source,
        external_vars,
      ),
      CpuRuntime::BytecodeVm => run_program_vm_with_external(
        program,
        entry,
        io,
        source_dir,
        audio_source,
        external_vars,
      ),
    }
  }
  #[cfg(not(feature = "window"))]
  {
    let _ = source_path;
    let _ = external_vars;
    run_program_with_runtime(program, entry, io, source_dir, runtime)
  }
}

/// Like `run_program_entry_with_io_from_path`, but also accepts an explicit
/// [`AudioBackend`] choice (defaults to `VM`). Use this when the caller wants
/// to opt into the C audio backend (requires `c_audio` feature) instead of
/// the default bytecode VM.
#[cfg(feature = "window")]
pub fn run_program_entry_with_io_and_audio_backend_from_path<IO: IOManager>(
  program: Program,
  entry: Option<&str>,
  io: IO,
  source_path: &std::path::Path,
  audio_backend: crate::audio::AudioBackend,
) -> Result<(IO, bool), EvalError> {
  let source_dir = source_path.parent().map(|p| p.to_path_buf());
  let audio_source =
    try_compile_audio_source(&program, source_path, audio_backend);
  run_program_default_runtime_with_audio(
    program,
    entry,
    io,
    source_dir,
    audio_source,
  )
}

pub fn run_program_capturing_output(
  program: Program,
) -> Result<String, EvalError> {
  run_program_capturing_output_with_runtime(program, CpuRuntime::default())
}

pub fn run_program_capturing_output_with_runtime(
  program: Program,
  runtime: CpuRuntime,
) -> Result<String, EvalError> {
  let (io, _) =
    run_program_with_runtime(program, None, StringIO::new(), None, runtime)?;
  let mut output = String::new();
  for event in &io.events {
    if let IOEvent::Print(s) = event {
      output.push_str(s);
      output.push('\n');
    }
  }
  Ok(output)
}

pub fn run_program_test_io(program: Program) -> Result<StringIO, EvalError> {
  run_program_test_io_with_runtime(program, CpuRuntime::default())
}

pub fn run_program_with_capture(
  program: Program,
) -> Result<Vec<String>, EvalError> {
  let (io, _) = run_program_with_runtime(
    program,
    None,
    CaptureIO::new(),
    None,
    CpuRuntime::default(),
  )?;
  Ok(io.prints)
}

pub fn run_program_with_capture_from_path(
  program: Program,
  source_path: &std::path::Path,
) -> Result<Vec<String>, EvalError> {
  run_program_with_capture_and_runtime_from_path(
    program,
    source_path,
    CpuRuntime::default(),
  )
}

/// Like `run_program_with_capture_from_path`, but returns the whole
/// `CaptureIO`, giving access to the captured prints alongside the ordered
/// logs of implicit GPU→CPU readbacks and CPU→GPU uploads the run performed.
/// Used by tests that assert exactly when the interpreter syncs — both that
/// spurious syncs don't happen and that genuine ones still do.
pub fn run_program_capturing_io_from_path(
  program: Program,
  source_path: &std::path::Path,
) -> Result<CaptureIO, EvalError> {
  run_program_capturing_io_with_runtime_from_path(
    program,
    source_path,
    CpuRuntime::default(),
  )
}

/// Re-export so downstream crates (e.g. `easl_cli`) can call this without
/// reaching into the `pub(crate)` `window` module.
#[cfg(feature = "window")]
pub use crate::window::close_persistent_window;
