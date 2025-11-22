use std::{collections::HashMap, rc::Rc, vec};

use crate::compiler::{
  builtins::ASSIGNMENT_OPS,
  expression::{Accessor, Exp, ExpKind, Number, SwizzleField},
  functions::{AbstractFunctionSignature, FunctionImplementationKind},
  program::Program,
  structs::AbstractStruct,
  types::{AbstractType, ArraySize, ExpTypeInfo, Type},
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
pub enum EvaluationError {
  EncounteredWildcard,
  InvalidNumberLiteralType,
  FloatInIntLiteral,
  UnboundName(Rc<str>),
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
  InvalidArraySize,
  NonBooleanLoopCondition,
  UnrecognizedStructName,
  UnimplementedBuiltin(String),
  DerivativeFunctionCantBeUsed,
  Discard,
  BuiltinError(&'static str),
  NoMainFn,
}

use EvaluationError::*;

#[derive(Clone, PartialEq, Debug)]
pub enum Function {
  StructConstructor(Vec<Rc<str>>),
  EnumConstructor(Rc<str>),
  Builtin(Rc<str>),
  Composite {
    arg_names: Vec<Rc<str>>,
    expression: Exp<ExpTypeInfo>,
  },
}

impl Function {
  fn from_abstract_signature(
    f: &AbstractFunctionSignature,
    name: &Rc<str>,
    env: &EvaluationEnvironment,
  ) -> Result<Self, EvaluationError> {
    match &f.implementation {
      FunctionImplementationKind::Builtin(_) => {
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
        let f = f.borrow();
        Ok(Function::Composite {
          arg_names: f.arg_names.clone(),
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
  Struct(HashMap<Rc<str>, Value>),
  Enum(Rc<str>, Box<Value>),
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

fn apply_builtin_fn(
  f_name: Rc<str>,
  mut args: Vec<(Value, Type)>,
  return_type: Type,
) -> Result<Value, EvaluationError> {
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
    "==" => {
      todo!()
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
        _ => panic!(),
      }
    }
    "vec2" | "vec2f" | "vec2i" | "vec2u" | "vec2b" => construct_vec(2),
    "vec3" | "vec3f" | "vec3i" | "vec3u" | "vec3b" => construct_vec(3),
    "vec4" | "vec4f" | "vec4i" | "vec4u" | "vec4b" => construct_vec(4),
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
    "reflect" => todo!(),
    "refract" => todo!(),
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
    "&" | "|" | "^" | "<<" | ">>" => todo!(),
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
    "bitcast" => todo!(),
    "array-length" => todo!(),
    "dpdx" | "dpdy" | "dpdx-coarse" | "dpdy-coarse" | "dpdx-fine"
    | "dpdy-fine" => {
      Err(DerivativeFunctionCantBeUsed)
      // These should have an effect that isn't handled by default on the CPU
      // so that this is a compile-time error instead
    }
    // todo!() data_packing_functions
    // todo!() bit_manipulation_functions
    // todo!() matrix stuff
    // todo!() texture functions, for now I guess these should just error
    "print" => {
      println!("{:?}", args[0].0);
      Ok(Value::Unit)
    }
    _ => Err(UnimplementedBuiltin(f_name.to_string())),
  }
}

impl Value {
  fn zeroed(
    t: Type,
    env: &EvaluationEnvironment,
  ) -> Result<Self, EvaluationError> {
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
          .take(match array_size.ok_or(CantCreateZeroedUnsizedArray)? {
            ArraySize::Literal(size) => size as usize,
            ArraySize::Constant(name) => {
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
            ArraySize::Unsized => return Err(CantCreateZeroedUnsizedArray),
          })
          .collect(),
      ),
      Type::Function(_) => return Err(CantCreateZeroedFunction),
      Type::Skolem(_) => return Err(CantCreateZeroedSkolem),
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
}

impl From<Primitive> for Value {
  fn from(primitive: Primitive) -> Self {
    Self::Prim(primitive)
  }
}

pub struct EvaluationEnvironment {
  bindings: HashMap<Rc<str>, Vec<Value>>,
  structs: HashMap<Rc<str>, AbstractStruct>,
}

impl EvaluationEnvironment {
  pub fn from_program(program: Program) -> Result<Self, EvaluationError> {
    let mut env = Self {
      bindings: HashMap::new(),
      structs: program
        .typedefs
        .structs
        .iter()
        .map(|s| (s.name.0.clone(), (&*s).clone()))
        .collect(),
    };
    for var in program.top_level_vars {
      let value = match var.value {
        Some(exp) => eval(exp, &mut env)?,
        None => Value::Uninitialized,
      };
      env.bind(var.name.clone(), value);
    }
    for e in program.typedefs.enums {
      for v in e.variants.iter() {
        if v.inner_type == AbstractType::Type(Type::Unit) {
          env.bind(
            v.name.clone(),
            Value::Enum(v.name.clone(), Value::Unit.into()),
          );
        }
      }
    }
    Ok(env)
  }
  fn bind(&mut self, name: Rc<str>, value: Value) {
    if let Some(bindings) = self.bindings.get_mut(&name) {
      bindings.push(value);
    } else {
      self.bindings.insert(name, vec![value]);
    }
  }
  fn unbind(&mut self, name: &Rc<str>) {
    let bindings = self.bindings.get_mut(name).unwrap();
    if bindings.len() == 1 {
      self.bindings.remove(name);
    } else {
      bindings.pop();
    }
  }
  fn lookup(&self, name: &Rc<str>) -> Result<&Value, EvaluationError> {
    self
      .bindings
      .get(name)
      .map(|values| values.last().unwrap())
      .ok_or(UnboundName(name.clone()))
  }
}

pub fn eval(
  exp: Exp<ExpTypeInfo>,
  env: &mut EvaluationEnvironment,
) -> Result<Value, EvaluationError> {
  Ok(match exp.kind {
    ExpKind::Wildcard => return Err(EncounteredWildcard),
    ExpKind::Unit => Value::Unit,
    ExpKind::Name(name) => env.lookup(&name)?.clone(),
    ExpKind::NumberLiteral(number) => match exp.data.kind.unwrap_known() {
      Type::F32 => Primitive::F32(match number {
        Number::Int(i) => i as f32,
        Number::Float(f) => f as f32,
      }),
      Type::I32 => Primitive::I32(match number {
        Number::Int(i) => i as i32,
        Number::Float(_) => return Err(FloatInIntLiteral),
      }),
      Type::U32 => Primitive::U32(match number {
        Number::Int(i) => i as u32,
        Number::Float(_) => return Err(FloatInIntLiteral),
      }),
      _ => return Err(InvalidNumberLiteralType),
    }
    .into(),
    ExpKind::BooleanLiteral(b) => Primitive::Bool(b).into(),
    ExpKind::Function(arg_names, expression) => {
      Value::Fun(Function::Composite {
        arg_names,
        expression: *expression,
      })
    }
    ExpKind::Application(f, args) => {
      let name = match f.kind {
        ExpKind::Name(name) => name,
        _ => return Err(AppliedNonName),
      };
      let Type::Function(f) = f.data.unwrap_known() else {
        panic!()
      };
      let f = f.abstract_ancestor.unwrap();
      let f = Function::from_abstract_signature(&*f, &name, env)?;
      let arg_types: Vec<Type> =
        args.iter().map(|a| a.data.kind.unwrap_known()).collect();
      let return_type = exp.data.unwrap_known();
      let is_assignment_op = ASSIGNMENT_OPS.contains(&*name);
      let accessed_expression = is_assignment_op.then(|| args[0].clone());
      let arg_values: Vec<Value> = args
        .into_iter()
        .map(|arg| eval(arg, env))
        .collect::<Result<_, _>>()?;
      let return_value = match f {
        Function::Builtin(name) => apply_builtin_fn(
          name,
          arg_values.into_iter().zip(arg_types.into_iter()).collect(),
          return_type,
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
            return Err(WrongArity);
          }
          for (name, value) in arg_names.iter().zip(arg_values.into_iter()) {
            env.bind(name.clone(), value);
          }
          let value = eval(*body, env)?;
          for name in arg_names.iter() {
            env.unbind(name);
          }
          value
        }
      };
      if is_assignment_op {
        enum AccessKind {
          Index(i64),
          Field(Rc<str>),
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
                active_swizzle_fields =
                  Some(swizzle_fields.into_iter().map(|f| f.index()).collect());
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
      }
    }
    ExpKind::Access(accessor, exp) => {
      let value = eval(*exp, env)?;
      match accessor {
        Accessor::Field(field_name) => match value {
          Value::Struct(s) => s.get(&field_name).ok_or(NoSuchField)?.clone(),
          _ => return Err(AccessedFieldOnNonStruct),
        },
        Accessor::Swizzle(swizzle_fields) => {
          let map = match value {
            Value::Struct(map) => map,
            _ => return Err(AccessedFieldOnNonStruct),
          };
          let values: Vec<Value> = swizzle_fields
            .into_iter()
            .map(|field| {
              map
                .get(match field {
                  SwizzleField::X => "x".into(),
                  SwizzleField::Y => "y".into(),
                  SwizzleField::Z => "z".into(),
                  SwizzleField::W => "w".into(),
                })
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
      let names: Vec<Rc<str>> =
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
      return Err(NoMatchingArm);
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
      update_condition_expression,
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
          _ => return Err(NonBooleanLoopCondition),
        }
        eval(*body_expression.clone(), env)?;
        eval(*update_condition_expression.clone(), env)?;
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
          _ => return Err(NonBooleanLoopCondition),
        }
        eval(*body_expression.clone(), env)?;
      }
      Value::Unit
    }
    ExpKind::Break => todo!("can't handle breaks in interpreter yet"),
    ExpKind::Continue => todo!("can't handle continue in interpreter yet"),
    ExpKind::Return(_) => todo!("can't handle return in interpreter yet"),
    ExpKind::Discard => return Err(Discard),
    ExpKind::ArrayLiteral(exps) => Value::Array(
      exps
        .into_iter()
        .map(|exp| eval(exp, env))
        .collect::<Result<_, _>>()?,
    ),
    ExpKind::Uninitialized => Value::Uninitialized,
    ExpKind::ZeroedArray => Value::zeroed(exp.data.kind.unwrap_known(), env)?,
  })
}

pub fn run_program(program: Program) -> Result<(), EvaluationError> {
  if let Some(main_fn) = program.main_fn() {
    let FunctionImplementationKind::Composite(f) =
      &main_fn.borrow().implementation
    else {
      panic!("main fn wasn't a composite function")
    };
    let f = f.borrow();
    let ExpKind::Function(_, body) = &f.expression.kind else {
      panic!()
    };
    eval(
      *body.clone(),
      &mut EvaluationEnvironment::from_program(program)?,
    )?;
    Ok(())
  } else {
    Err(EvaluationError::NoMainFn)
  }
}
