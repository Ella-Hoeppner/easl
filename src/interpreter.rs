use std::{collections::HashMap, vec};
use take_mut::take;

use std::sync::Arc;

use crate::compiler::{
  builtins::ASSIGNMENT_OPS,
  error::{CompileError, SourceTrace},
  expression::{Accessor, Exp, ExpKind, Number, SwizzleField},
  functions::{AbstractFunctionSignature, FunctionImplementationKind},
  program::Program,
  structs::AbstractStruct,
  types::{AbstractType, ConcreteArraySize, ExpTypeInfo, Type},
  vars::{GroupAndBinding, TopLevelVariableKind, VariableAddressSpace},
};

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

#[derive(Clone, PartialEq, Debug)]
pub enum EvalError {
  EncounteredWildcard,
  InvalidNumberLiteralType,
  FloatInIntLiteral,
  UnboundName(Arc<str>),
  UnboundFunctionName,
  AppliedNonName,
  WrongArity,
  AccessedFieldOnNonStruct,
  AccessedIndexOnNonArray,
  AccessorIndexMustBeInteger,
  NoSuchField,
  NoMatchingArm,
  CantCreateZeroedFunction,
  CantCreateZeroedSkolem,
  CantCreateZeroedReference,
  CantCreateZeroedUnsizedArray,
  CantCreateZeroedSkolemSizedArray,
  InvalidArraySize,
  NonBooleanLoopCondition,
  UnrecognizedStructName,
  UnimplementedBuiltin(String),
  WindowFeatureNotEnabled,
  NoCpuEntryPoint,
  MultipleCpuEntryPoints,
  CpuEntryPointNotFound(String),
  DerivativeFunctionCantBeUsed,
  Discard,
  CloseWindow,
  BuiltinError(&'static str),
  NoMainFn,
  ControlFlowExceptionEscapedToTopLevel,
  CompilationError(CompileError),
  ArrayIndexOutOfBounds(usize, usize),
  NegativeArrayIndex(isize),
}

impl From<CompileError> for EvalError {
  fn from(e: CompileError) -> Self {
    Self::CompilationError(e)
  }
}

use EvalError::*;

#[derive(Clone, PartialEq, Debug)]
pub enum Function {
  StructConstructor(Vec<Arc<str>>),
  EnumConstructor(Arc<str>),
  Builtin(Arc<str>),
  Composite {
    arg_names: Vec<Arc<str>>,
    expression: Exp<ExpTypeInfo>,
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
          let s = match env.structs.get(name) {
            Some(s) => s,
            None => return Err(UnrecognizedStructName),
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
  Uninitialized,
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
) -> Result<Value, EvalError> {
  let return_type_clone = return_type.clone();
  let construct_vec = |vec_length: usize| {
    let Type::Struct(return_struct) = return_type else {
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
    "==" => Ok(Value::Prim(Primitive::Bool(args[0] == args[1]))),
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
    "sin" | "cos" | "tan" | "sinh" | "cosh" | "tanh" | "asin" | "acos"
    | "atan" | "asinh" | "acosh" | "atanh" => {
      let Value::Prim(Primitive::F32(x)) = args[0].0 else {
        panic!()
      };
      Ok(Value::Prim(Primitive::F32(match &*f_name {
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
      })))
    }
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
      let Value::Struct(b) = &args[0].0 else {
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
      let Value::Struct(b) = &args[0].0 else {
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
    "floor" | "ceil" | "round" | "fract" | "sqrt" | "trunc" | "saturate" => {
      Ok(args[0].0.map_primitive_or_vec_components(|p| {
        let Primitive::F32(x) = p else { panic!() };
        Primitive::F32(match &*f_name {
          "floor" => x.floor(),
          "ceil" => x.ceil(),
          "round" => x.round(),
          "fract" => x.fract(),
          "sqrt" => x.sqrt(),
          "trunc" => x.trunc(),
          "saturate" => x.clamp(0., 1.),
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
        let max = values.remove(2);
        let min = values.remove(1);
        let x = values.remove(0);
        match (x, min, max) {
          (Primitive::F32(x), Primitive::F32(min), Primitive::F32(max)) => {
            let x = ((x - min) / (max - min)).clamp(0., 1.);
            Primitive::F32(x * x * (3. - 2. * x))
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
      let Value::Array(arr) = &args[0].0 else {
        panic!("array-length called on non-array")
      };
      Ok(Value::Prim(Primitive::U32(arr.len() as u32)))
    }
    "dpdx" | "dpdy" | "dpdx-coarse" | "dpdy-coarse" | "dpdx-fine"
    | "dpdy-fine" => {
      Err(DerivativeFunctionCantBeUsed)
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
      let formatted = args
        .get(0)
        .map(|arg| arg.0.format_for_print(&args[0].1))
        .unwrap_or_else(|| "()".to_string());
      env.io.println(&formatted);
      Ok(Value::Unit)
    }
    "dispatch-render-shaders" => {
      let (_, Type::Function(vert_f)) = &args[0] else {
        panic!()
      };
      let vert_effects = vert_f.effects();
      let (vert_read_global_variable_names, vert_written_global_variable_names) =
        vert_effects.read_and_written_globals();
      let vert_f_name = vert_f
        .abstract_ancestor
        .as_ref()
        .unwrap()
        .read()
        .unwrap()
        .name
        .clone();
      let (_, Type::Function(frag_f)) = &args[1] else {
        panic!()
      };
      let frag_effects = frag_f.effects();
      let (frag_read_global_variable_names, frag_written_global_variable_names) =
        frag_effects.read_and_written_globals();
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
      let pre_upload = env.collect_dirty_uploads(&read_global_variable_names);
      env.io.record_draw(
        &vert_f_name,
        &frag_f_name,
        *vert_count,
        pre_upload,
      )?;
      env.mark_gpu_written(&written_global_variable_names);
      Ok(Value::Unit)
    }
    "dispatch-compute-shader" => {
      let (_, Type::Function(compute_f)) = &args[0] else {
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
      let effects = compute_f.effects();
      let (read_global_variable_names, written_global_variable_names) =
        effects.read_and_written_globals();
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
      let pre_upload = env.collect_dirty_uploads(&read_global_variable_names);
      env
        .io
        .record_compute(&entry_name, workgroup_count, pre_upload)?;
      env.mark_gpu_written(&written_global_variable_names);
      Ok(Value::Unit)
    }
    "close-window" => {
      env.io.record_close_window();
      Err(CloseWindow)
    }
    "spawn-window" => {
      let (
        Value::Fun(Function::Composite {
          arg_names: _,
          expression,
        }),
        _,
      ) = args.remove(0)
      else {
        panic!("spawn-window: callback must be a composite function")
      };
      let ExpKind::Function(_, body) = expression.kind else {
        panic!()
      };
      let body = *body;
      let mut result: Result<(), EvalError> = Ok(());
      take(env, |env| match IO::run_spawn_window(body, env) {
        Ok(env) => env,
        Err((env, e)) => {
          result = Err(e);
          env
        }
      });
      result?;
      Ok(Value::Unit)
    }
    "into-dynamic-array" => Ok(args.remove(0).0),
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
    _ => Err(UnimplementedBuiltin(f_name.to_string())),
  }
}

impl Value {
  fn format_for_print(&self, t: &Type) -> String {
    match (self, t) {
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
              .map(|v| v.format_for_print(&scalar_type))
              .collect();
            format!("(vec{}{suffix} {})", formatted.len(), formatted.join(" "))
          })
          .collect();
        format!("({}{suffix} {})", s.name, formatted_cols.join(" "))
      }
      (Value::Struct(fields), Type::Struct(s)) => {
        let formatted_fields: Vec<String> = s
          .fields
          .iter()
          .map(|field| {
            let value = &fields[&field.name];
            let field_type = field.field_type.kind.unwrap_known();
            value.format_for_print(&field_type)
          })
          .collect();
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
          t => format!("({} {})", variant, inner.format_for_print(t)),
        }
      }
      (Value::Array(items), Type::Array(_, inner_type)) => {
        let inner = inner_type.kind.unwrap_known();
        let formatted: Vec<String> = items
          .iter()
          .map(|item| item.format_for_print(&inner))
          .collect();
        format!("[{}]", formatted.join(" "))
      }
      (Value::Unit, _) => "()".to_string(),
      _ => format!("{:?}", self),
    }
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
                  _ => return Err(InvalidArraySize),
                },
                _ => return Err(InvalidArraySize),
              }
            }
            ConcreteArraySize::Unsized => 0,
            ConcreteArraySize::Skolem(_) => {
              return Err(CantCreateZeroedSkolemSizedArray);
            }
            ConcreteArraySize::UnificationVariable(const_generic_value) => {
              match &*const_generic_value.value.read().unwrap() {
                Some(x) => *x as usize,
                None => return Err(CantCreateZeroedSkolemSizedArray),
              }
            }
          })
          .collect(),
      ),
      Type::Function(_) => return Err(CantCreateZeroedFunction),
      Type::Skolem(_, _) => return Err(CantCreateZeroedSkolem),
      Type::Enum(e) => {
        let first_variant = &e.variants[0];
        Value::Enum(
          first_variant.name.clone(),
          Self::zeroed(first_variant.inner_type.unwrap_known(), env)?.into(),
        )
      }
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
        let mut bytes = vec![];
        for field in &s.fields {
          if let Some(v) = fields.get(&field.name) {
            bytes.extend(v.to_uniform_bytes(&field.field_type.unwrap_known()));
          }
        }
        bytes
      }
      Value::Array(inner_values) => {
        let Type::Array(_, inner_type) = &ty else {
          panic!()
        };
        let mut bytes = vec![];
        for value in inner_values {
          bytes.extend(value.to_uniform_bytes(&inner_type.unwrap_known()));
        }
        bytes
      }
      _ => vec![],
    }
  }

  /// Deserializes a `Value` from raw GPU bytes, given the expected `Type`.
  pub fn from_gpu_bytes(bytes: &[u8], ty: &Type) -> Value {
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
        let mut fields = HashMap::new();
        for field in &s.fields {
          let inner_ty = field.field_type.unwrap_known();
          let v = Self::from_gpu_bytes_at(bytes, &inner_ty, offset);
          fields.insert(field.name.clone(), v);
        }
        Value::Struct(fields)
      }
      Type::Array(Some(size), inner_type) => {
        let inner_ty = inner_type.unwrap_known();
        let count = match size {
          crate::compiler::types::ConcreteArraySize::Literal(n) => *n as usize,
          _ => 0,
        };
        Value::Array(
          (0..count)
            .map(|_| Self::from_gpu_bytes_at(bytes, &inner_ty, offset))
            .collect(),
        )
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

/// Internal frame-level GPU command, used by StdoutIO to pass commands to
/// the wgpu renderer in window.rs.
#[derive(Debug, Clone, PartialEq)]
pub enum WindowEvent {
  RenderShaders {
    vert: String,
    frag: String,
    vert_count: u32,
    pre_upload: Vec<((u8, u8), Vec<u8>)>,
  },
  ComputeShader {
    entry: String,
    workgroup_count: (u32, u32, u32),
    pre_upload: Vec<((u8, u8), Vec<u8>)>,
  },
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
  CloseWindow,
}

/// Describes the GPU buffer type for a top-level variable binding.
/// Used by `window.rs` to create the correct wgpu binding.
#[derive(Clone, Copy, PartialEq)]
pub enum GpuBufferKind {
  Uniform,
  StorageReadOnly,
  StorageReadWrite,
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

pub trait IOManager: Sized {
  fn println(&mut self, s: &str);
  fn record_draw(
    &mut self,
    vert: &str,
    frag: &str,
    vert_count: u32,
    pre_upload: Vec<((u8, u8), Vec<u8>)>,
  ) -> Result<(), EvalError>;
  fn record_compute(
    &mut self,
    entry: &str,
    workgroup_count: (u32, u32, u32),
    pre_upload: Vec<((u8, u8), Vec<u8>)>,
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
  /// Returns the current window dimensions in pixels, or (1, 1) if no window
  /// is open.
  fn window_size(&self) -> (u32, u32) {
    (1, 1)
  }
  /// Called after the wgpu device is created so compute dispatches can run
  /// synchronously. Default no-op for IO managers without real GPU access.
  #[cfg(feature = "window")]
  fn set_gpu(
    &mut self,
    _gpu: std::sync::Arc<std::sync::RwLock<crate::window::GpuCore>>,
  ) {
  }
  fn run_spawn_window(
    body: Exp<ExpTypeInfo>,
    env: EvaluationEnvironment<Self>,
  ) -> Result<
    EvaluationEnvironment<Self>,
    (EvaluationEnvironment<Self>, EvalError),
  >;
}

pub struct StdoutIO {
  frame_draw_calls: Vec<WindowEvent>,
  #[cfg(feature = "window")]
  gpu: Option<std::sync::Arc<std::sync::RwLock<crate::window::GpuCore>>>,
}

impl StdoutIO {
  pub fn new() -> Self {
    Self {
      frame_draw_calls: vec![],
      #[cfg(feature = "window")]
      gpu: None,
    }
  }
}

impl IOManager for StdoutIO {
  fn println(&mut self, s: &str) {
    println!("{s}");
  }

  fn record_draw(
    &mut self,
    vert: &str,
    frag: &str,
    vert_count: u32,
    pre_upload: Vec<((u8, u8), Vec<u8>)>,
  ) -> Result<(), EvalError> {
    self.frame_draw_calls.push(WindowEvent::RenderShaders {
      vert: vert.to_string(),
      frag: frag.to_string(),
      vert_count,
      pre_upload,
    });
    Ok(())
  }

  fn record_compute(
    &mut self,
    entry: &str,
    workgroup_count: (u32, u32, u32),
    pre_upload: Vec<((u8, u8), Vec<u8>)>,
  ) -> Result<(), EvalError> {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      gpu
        .write()
        .unwrap()
        .execute_compute(entry, workgroup_count, pre_upload);
      return Ok(());
    }
    self.frame_draw_calls.push(WindowEvent::ComputeShader {
      entry: entry.to_string(),
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

  #[cfg(feature = "window")]
  fn set_gpu(
    &mut self,
    gpu: std::sync::Arc<std::sync::RwLock<crate::window::GpuCore>>,
  ) {
    self.gpu = Some(gpu);
  }

  fn window_size(&self) -> (u32, u32) {
    #[cfg(feature = "window")]
    if let Some(gpu) = &self.gpu {
      return gpu.read().unwrap().window_size;
    }
    (1, 1)
  }

  fn run_spawn_window(
    body: Exp<ExpTypeInfo>,
    env: EvaluationEnvironment<Self>,
  ) -> Result<
    EvaluationEnvironment<Self>,
    (EvaluationEnvironment<Self>, EvalError),
  > {
    #[cfg(feature = "window")]
    return crate::window::run_window_loop(body, env);
    #[cfg(not(feature = "window"))]
    {
      let _ = body;
      Err((env, WindowFeatureNotEnabled))
    }
  }
}

/// Test/debug IO manager. Records all IO events in a single ordered log and
/// simulates the window loop by running the frame callback `frame_count` times.
pub struct StringIO {
  pub events: Vec<IOEvent>,
  pub frame_count: usize,
}

impl Default for StringIO {
  fn default() -> Self {
    Self {
      events: vec![],
      frame_count: 10,
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

  fn record_draw(
    &mut self,
    vert: &str,
    frag: &str,
    vert_count: u32,
    _pre_upload: Vec<((u8, u8), Vec<u8>)>,
  ) -> Result<(), EvalError> {
    self.events.push(IOEvent::DispatchShaders {
      vert: vert.to_string(),
      frag: frag.to_string(),
      vert_count,
    });
    Ok(())
  }

  fn record_compute(
    &mut self,
    entry: &str,
    workgroup_count: (u32, u32, u32),
    _pre_upload: Vec<((u8, u8), Vec<u8>)>,
  ) -> Result<(), EvalError> {
    self.events.push(IOEvent::DispatchComputeShader {
      entry: entry.to_string(),
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

  fn run_spawn_window(
    body: Exp<ExpTypeInfo>,
    mut env: EvaluationEnvironment<Self>,
  ) -> Result<
    EvaluationEnvironment<Self>,
    (EvaluationEnvironment<Self>, EvalError),
  > {
    env.io.events.push(IOEvent::SpawnWindow);
    let frame_count = env.io.frame_count;
    for _ in 0..frame_count {
      match eval(body.clone(), &mut env) {
        Ok(_) => {}
        Err(EvalException::Error(CloseWindow)) => break,
        Err(e) => return Err((env, e.into())),
      }
    }
    Ok(env)
  }
}

pub struct EvaluationEnvironment<IO: IOManager> {
  bindings: HashMap<Arc<str>, Vec<Value>>,
  structs: HashMap<Arc<str>, AbstractStruct>,
  wgsl: String,
  pub io: IO,
  /// GPU-bound top-level vars (uniform + storage), in declaration order.
  binding_vars: Vec<(GroupAndBinding, Arc<str>, Type, VariableAddressSpace)>,
  /// Sync state for each GPU-bound variable, keyed by name.
  buffer_states: HashMap<Arc<str>, SharedBufferState>,
}

impl<IO: IOManager> EvaluationEnvironment<IO> {
  pub fn from_program(program: Program, io: IO) -> Result<Self, EvalError> {
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
          group_and_binding: Some(gb),
        } = var.kind
        {
          matches!(
            address_space,
            VariableAddressSpace::Uniform
              | VariableAddressSpace::StorageRead
              | VariableAddressSpace::StorageReadWrite
          )
          .then(|| (gb, var.name.clone(), var.var_type.clone(), address_space))
        } else {
          None
        }
      })
      .collect();
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
      buffer_states,
    };
    for var in program.top_level_vars.iter() {
      let value = match &var.value {
        Some(exp) => eval(exp.clone(), &mut env)?,
        // Unsized arrays and other unzeroable types (e.g. dynamic storage
        // buffers) fall back to Uninitialized; the user must assign before use.
        None => Value::zeroed(var.var_type.clone(), &env)
          .unwrap_or(Value::Uninitialized),
      };
      env.bind(var.name.clone(), value);
    }
    for e in program.typedefs.enums.iter() {
      for v in e.variants.iter() {
        if v.inner_type == AbstractType::Type(Type::Unit) {
          env.bind(
            v.name.clone(),
            Value::Enum(v.name.clone(), Value::Unit.into()),
          );
        }
      }
    }
    let wgsl = program.compile_to_wgsl()?;
    env.wgsl = wgsl;
    Ok(env)
  }
  pub fn wgsl(&self) -> &str {
    &self.wgsl
  }

  /// Returns `(group, binding, kind, static_size_bytes)` for each GPU-bound
  /// variable.  `size` is 0 for dynamically-sized (unsized array) bindings.
  pub fn binding_infos(&self) -> Vec<((u8, u8), GpuBufferKind, u64)> {
    self
      .binding_vars
      .iter()
      .map(|(gb, _, ty, addr)| {
        let kind = match addr {
          VariableAddressSpace::Uniform => GpuBufferKind::Uniform,
          VariableAddressSpace::StorageRead => GpuBufferKind::StorageReadOnly,
          VariableAddressSpace::StorageReadWrite => {
            GpuBufferKind::StorageReadWrite
          }
          _ => unreachable!(),
        };
        // unwrap_or(0): unsized arrays → size 0, handled dynamically in window.rs
        let u32s = ty.data_size_in_u32s(&SourceTrace::empty()).unwrap_or(0);
        let size = if u32s == 0 {
          0
        } else {
          ((u32s as u64 * 4).max(4) + 15) & !15
        };
        ((gb.group, gb.binding), kind, size)
      })
      .collect()
  }

  /// Returns the current byte representation of each GPU-bound variable,
  /// padded to a 16-byte multiple.
  pub fn binding_buffer_data(&self) -> Vec<((u8, u8), Vec<u8>)> {
    self
      .binding_vars
      .iter()
      .map(|(gb, name, ty, _)| {
        let value = self.bindings.get(name).and_then(|v| v.last());
        let mut bytes = value
          .map(|v| v.to_uniform_bytes(ty))
          .unwrap_or(vec![0u8; 4]);
        while bytes.len() % 16 != 0 {
          bytes.push(0);
        }
        ((gb.group, gb.binding), bytes)
      })
      .collect()
  }

  fn is_binding_var(&self, name: &Arc<str>) -> bool {
    self.binding_vars.iter().any(|(_, n, _, _)| n == name)
  }

  /// Serializes the current CPU value of each GPUOutOfDate binding var whose
  /// name appears in `names`, marks those buffers Synced, and returns the data
  /// ready for upload.
  fn collect_dirty_uploads(
    &mut self,
    names: &[Arc<str>],
  ) -> Vec<((u8, u8), Vec<u8>)> {
    let mut result = vec![];
    for (gb, name, ty, _) in &self.binding_vars {
      if !names.contains(name) {
        continue;
      }
      if self.buffer_states.get(name) != Some(&SharedBufferState::GPUOutOfDate)
      {
        continue;
      }
      let value = self.bindings.get(name).and_then(|v| v.last());
      let mut bytes = value
        .map(|v| v.to_uniform_bytes(ty))
        .unwrap_or(vec![0u8; 4]);
      while bytes.len() % 16 != 0 {
        bytes.push(0);
      }
      result.push(((gb.group, gb.binding), bytes));
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
  fn mark_cpu_written(&mut self, names: &[Arc<str>]) {
    for name in names {
      if self.is_binding_var(name) {
        self
          .buffer_states
          .insert(name.clone(), SharedBufferState::GPUOutOfDate);
      }
    }
  }

  /// For any GPU-bound var in `names` that is CPUOutOfDate, reads the buffer
  /// back from GPU and updates the CPU-side binding.
  fn check_cpu_readable(&mut self, names: &[Arc<str>]) {
    let vars: Vec<(GroupAndBinding, Arc<str>, Type)> = self
      .binding_vars
      .iter()
      .filter(|(_, name, _, _)| {
        names.contains(name)
          && self.buffer_states.get(name)
            == Some(&SharedBufferState::CPUOutOfDate)
      })
      .map(|(gb, name, ty, _)| (*gb, name.clone(), ty.clone()))
      .collect();
    for (gb, name, ty) in vars {
      let u32s = ty
        .data_size_in_u32s(&crate::compiler::error::SourceTrace::empty())
        .unwrap_or(0);
      let size = ((u32s as u64 * 4).max(4) + 15) & !15;
      if let Some(bytes) = self.io.sync_gpu_to_cpu(gb.group, gb.binding, size) {
        let value = Value::from_gpu_bytes(&bytes, &ty);
        if let Some(stack) = self.bindings.get_mut(&name) {
          if let Some(slot) = stack.last_mut() {
            *slot = value;
          }
        }
        self
          .buffer_states
          .insert(name.clone(), SharedBufferState::Synced);
      }
    }
  }

  fn bind(&mut self, name: Arc<str>, value: Value) {
    if let Some(bindings) = self.bindings.get_mut(&name) {
      bindings.push(value);
    } else {
      self.bindings.insert(name, vec![value]);
    }
  }
  fn unbind(&mut self, name: &Arc<str>) {
    let bindings = self.bindings.get_mut(name).unwrap();
    if bindings.len() == 1 {
      self.bindings.remove(name);
    } else {
      bindings.pop();
    }
  }
  fn lookup(&self, name: &Arc<str>) -> Result<&Value, EvalError> {
    self
      .bindings
      .get(name)
      .map(|values| values.last().unwrap())
      .ok_or(UnboundName(name.clone()))
  }
}

#[derive(Debug, Clone)]
pub enum EvalException {
  Error(EvalError),
  Break,
  Continue,
  Return(Value),
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
      _ => EvalError::ControlFlowExceptionEscapedToTopLevel,
    }
  }
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
        let name = match f.kind {
          ExpKind::Name(name) => name,
          _ => return Err(AppliedNonName.into()),
        };
        let f = f_signature.abstract_ancestor.unwrap_or_else(|| {
          panic!(
            "couldn't find abstract ancestor for {name}, {:?}",
            args
              .clone()
              .into_iter()
              .map(|arg| {
                if let Type::Function(f) = arg.data.unwrap_known()
                  && let Some(f) = f.abstract_ancestor
                  && let ExpKind::Name(f_name) = arg.kind
                {
                  Ok(Value::Fun(Function::from_abstract_signature(
                    &f.read().unwrap(),
                    &f_name,
                    env,
                  )?))
                } else {
                  eval(arg, env)
                }
              })
              .collect::<Result<Vec<Value>, _>>()
              .unwrap()
          );
        });
        let f =
          Function::from_abstract_signature(&*f.read().unwrap(), &name, env)?;
        let arg_types: Vec<Type> =
          args.iter().map(|a| a.data.kind.unwrap_known()).collect();
        let return_type = exp.data.unwrap_known();
        let is_assignment_op = ASSIGNMENT_OPS.contains(&*name);
        let accessed_expression = is_assignment_op.then(|| args[0].clone());
        // Sync any GPU-written globals before evaluating args so we see the
        // updated values when the args are looked up.
        let (read_global_variable_names, written_global_variable_names) =
          exp_effects.read_and_written_globals();
        env.check_cpu_readable(&read_global_variable_names);
        let arg_values: Vec<Value> = args
          .into_iter()
          .map(|arg| {
            if let Type::Function(f) = arg.data.unwrap_known()
              && let Some(f) = f.abstract_ancestor
              && let ExpKind::Name(f_name) = arg.kind
            {
              Ok(Value::Fun(Function::from_abstract_signature(
                &f.read().unwrap(),
                &f_name,
                env,
              )?))
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
              return Err(WrongArity.into());
            }
            for (name, value) in arg_names.iter().zip(arg_values.into_iter()) {
              env.bind(name.clone(), value);
            }
            let value = match eval(*body, env) {
              Ok(value) => Ok(value),
              Err(exception) => match exception {
                EvalException::Return(value) => Ok(value),
                other => Err(other),
              },
            };
            for name in arg_names.iter() {
              env.unbind(name);
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
                    accesses.push(AccessKind::Field(field_name))
                  }
                  Accessor::Swizzle(swizzle_fields) => {
                    accesses.push(AccessKind::Swizzle(swizzle_fields))
                  }
                }
                accessed_expression = *exp;
              }
              _ => panic!(),
            }
          };
          let mut accessed_value = env
            .bindings
            .get_mut(&*accessed_name)
            .unwrap()
            .last_mut()
            .unwrap();
          let mut active_swizzle_fields: Option<Vec<usize>> = None;
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
        } else {
          return_value
        };
        env.mark_cpu_written(&written_global_variable_names);
        return_value
      }
      Type::Array(_, _) => {
        if args.len() != 1 {
          panic!();
        }
        let array = eval(*f, env)?;
        let index_value = eval(args.remove(0), env)?;
        let Value::Array(array_values) = array else {
          panic!()
        };
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
        if u < array_values.len() {
          array_values[u].clone()
        } else {
          return Err(ArrayIndexOutOfBounds(u, array_values.len()).into());
        }
      }
      _ => panic!(),
    },
    ExpKind::Access(accessor, exp) => {
      let value = eval(*exp, env)?;
      match accessor {
        Accessor::Field(field_name) => match value {
          Value::Struct(s) => s.get(&field_name).ok_or(NoSuchField)?.clone(),
          _ => return Err(AccessedFieldOnNonStruct.into()),
        },
        Accessor::Swizzle(swizzle_fields) => {
          let map = match value {
            Value::Struct(map) => map,
            _ => return Err(AccessedFieldOnNonStruct.into()),
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
                .ok_or(NoSuchField)
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
      }
    }
    ExpKind::Let(items, exp) => {
      let names: Vec<Arc<str>> =
        items.iter().map(|(name, _, _, _)| name.clone()).collect();
      for (name, _, _, exp) in items {
        let value = eval(exp, env)?;
        env.bind(name, value);
      }
      let value = eval(*exp, env)?;
      for name in names {
        env.unbind(&name);
      }
      value
    }
    ExpKind::Match(scrutinee, arms) => {
      let scrutinee = eval(*scrutinee, env)?;
      for (match_exp, body_exp) in arms {
        if match_exp.kind == ExpKind::Wildcard
          || eval(match_exp, env)? == scrutinee
        {
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
      .unwrap(),
    ExpKind::ForLoop {
      increment_variable_name,
      increment_variable_initial_value_expression,
      continue_condition_expression,
      update_expression,
      body_expression,
      ..
    } => {
      let initial_value =
        eval(*increment_variable_initial_value_expression, env)?;
      env.bind(increment_variable_name.0.clone(), initial_value);
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
              Err(EvalException::Error(e)) => return Err(e.into()),
              Err(EvalException::Break) => {
                broke = true;
                break;
              }
              Err(EvalException::Return(return_value)) => {
                return Err(EvalException::Return(return_value));
              }
            }
          }
        }
        if broke {
          break;
        }
      }
      env.unbind(&increment_variable_name.0);
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
          Err(EvalException::Error(e)) => return Err(e.into()),
          Err(EvalException::Break) => break,
          Err(EvalException::Return(return_value)) => {
            return Err(EvalException::Return(return_value));
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
) -> Result<IO, EvalError> {
  let mut cpu_fns = program.cpu_entry_points();
  let entry_fn = match entry_point_name {
    Some(name) => {
      let pos = cpu_fns
        .iter()
        .position(|f| &*f.read().unwrap().name == name)
        .ok_or_else(|| CpuEntryPointNotFound(name.to_string()))?;
      cpu_fns.remove(pos)
    }
    None => match cpu_fns.len() {
      0 => return Err(NoCpuEntryPoint),
      1 => cpu_fns.remove(0),
      _ => return Err(MultipleCpuEntryPoints),
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
  let body = *body.clone();
  drop(f);
  let mut env = EvaluationEnvironment::from_program(program, io)?;
  eval(body, &mut env)?;
  Ok(env.io)
}

pub fn run_program(program: Program) -> Result<(), EvalError> {
  run_program_with(program, None, StdoutIO::new())?;
  Ok(())
}

pub fn run_program_entry(
  program: Program,
  entry: Option<&str>,
) -> Result<(), EvalError> {
  run_program_with(program, entry, StdoutIO::new())?;
  Ok(())
}

pub fn run_program_capturing_output(
  program: Program,
) -> Result<String, EvalError> {
  let io = run_program_with(program, None, StringIO::new())?;
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
  run_program_with(program, None, StringIO::new())
}
