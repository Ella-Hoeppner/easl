use std::{collections::HashMap, rc::Rc};

use crate::compiler::{
  expression::{Accessor, Exp, ExpKind, Number, SwizzleField},
  functions::{AbstractFunctionSignature, FunctionImplementationKind},
  program::Program,
  structs::AbstractStruct,
  types::{AbstractType, ArraySize, ExpTypeInfo, Type},
};

#[derive(Clone, PartialEq, Debug)]
pub enum Primitive {
  Unit,
  F32(f32),
  U32(u32),
  I32(i32),
  Bool(bool),
}

#[derive(Clone, PartialEq, Debug)]
pub enum EvaluationError {
  EncounteredWildcard,
  InvalidNumberLiteralType,
  FloatInIntLiteral,
  UnboundName,
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
  UnimplementedBuiltin,
  BuiltinError(&'static str),
}

use EvaluationError::*;

#[derive(Clone, PartialEq, Debug)]
pub enum Function {
  Constructor(Vec<Rc<str>>),
  Builtin(Rc<str>),
  Composite {
    arg_names: Vec<Rc<str>>,
    body: Exp<ExpTypeInfo>,
  },
}

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
  Prim(Primitive),
  Struct(HashMap<Rc<str>, Value>),
  Fun(Function),
  Array(Vec<Value>),
  Uninitialized,
}

fn apply_builtin_fn(
  f_name: Rc<str>,
  args: Vec<(Value, Type)>,
) -> Result<Value, EvaluationError> {
  match &*f_name {
    "+" => match args[0].1 {
      Type::F32 => Ok(
        Primitive::F32(
          args
            .into_iter()
            .map(|(value, t)| {
              if t == Type::F32 {
                let Value::Prim(Primitive::F32(value)) = value else {
                  unreachable!();
                };
                Ok(value)
              } else {
                Err(BuiltinError("incompatible types in +"))
              }
            })
            .collect::<Result<Vec<_>, EvaluationError>>()?
            .iter()
            .sum(),
        )
        .into(),
      ),
      Type::I32 => todo!(),
      Type::U32 => todo!(),
      _ => return Err(BuiltinError("invalid type in +")),
    },
    _ => Err(UnimplementedBuiltin),
  }
}

impl Value {
  fn zeroed(
    t: Type,
    env: &EvaluationEnvironment,
  ) -> Result<Self, EvaluationError> {
    Ok(match t {
      Type::Unit => Primitive::Unit.into(),
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
      Type::Reference(_) => return Err(CantCreateZeroedReference),
      Type::Enum(_) => todo!("don't know how to zero enums yet"),
    })
  }
}

impl From<Primitive> for Value {
  fn from(primitive: Primitive) -> Self {
    Self::Prim(primitive)
  }
}

pub struct EvaluationEnvironment {
  bindings: HashMap<Rc<str>, Vec<Value>>,
  top_level_functions: HashMap<Rc<str>, Vec<AbstractFunctionSignature>>,
  structs: HashMap<Rc<str>, AbstractStruct>,
}

impl EvaluationEnvironment {
  pub fn from_program(program: Program) -> Self {
    Self {
      bindings: HashMap::default(),
      top_level_functions: program
        .abstract_functions
        .into_iter()
        .map(|(name, fns)| {
          (
            name,
            fns
              .into_iter()
              .map(|f| f.borrow().clone())
              .collect::<Vec<AbstractFunctionSignature>>(),
          )
        })
        .collect(),
      structs: program
        .typedefs
        .structs
        .iter()
        .map(|s| (s.name.clone(), (&*s).clone()))
        .collect(),
    }
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
      .ok_or(UnboundName)
  }
  fn lookup_top_level_fn(
    &self,
    name: &Rc<str>,
    arg_types: Vec<Type>,
  ) -> Result<Function, EvaluationError> {
    let result = self
      .top_level_functions
      .get(name)
      .map(|fns| {
        fns.iter().find_map(|f| {
          if f
            .arg_types
            .iter()
            .zip(arg_types.iter())
            .find(|(expected_t, t)| {
              AbstractType::Type((*t).clone()) == **expected_t
            })
            .is_none()
          {
            Some(match &f.implementation {
              FunctionImplementationKind::Builtin(_) => {
                Ok(Function::Builtin(name.clone()))
              }
              FunctionImplementationKind::StructConstructor => {
                Ok(Function::Constructor({
                  let s = match self.structs.get(name) {
                    Some(s) => s,
                    None => return Some(Err(UnrecognizedStructName)),
                  };
                  s.fields.iter().map(|field| field.name.clone()).collect()
                }))
              }
              FunctionImplementationKind::EnumConstructor(_) => todo!(),
              FunctionImplementationKind::Composite(f) => {
                let f = f.borrow();
                Ok(Function::Composite {
                  arg_names: f.arg_names.clone(),
                  body: f.body.clone(),
                })
              }
            })
          } else {
            None
          }
        })
      })
      .flatten();
    match result {
      Some(Ok(f)) => Ok(f),
      Some(Err(e)) => Err(e),
      None => Err(UnboundFunctionName),
    }
  }
}

pub fn eval(
  exp: Exp<ExpTypeInfo>,
  env: &mut EvaluationEnvironment,
) -> Result<Value, EvaluationError> {
  Ok(match exp.kind {
    ExpKind::Wildcard => return Err(EncounteredWildcard),
    ExpKind::Unit => Primitive::Unit.into(),
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
    ExpKind::Function(arg_names, body) => Value::Fun(Function::Composite {
      arg_names,
      body: *body,
    }),
    ExpKind::Application(f, args) => {
      let f = match f.kind {
        ExpKind::Name(name) => env.lookup_top_level_fn(
          &name,
          args
            .iter()
            .map(|arg| arg.data.kind.unwrap_known())
            .collect(),
        )?,
        _ => return Err(AppliedNonName),
      };
      let arg_types: Vec<Type> =
        args.iter().map(|a| a.data.kind.unwrap_known()).collect();
      let arg_values: Vec<Value> = args
        .into_iter()
        .map(|arg| eval(arg, env))
        .collect::<Result<_, _>>()?;
      match f {
        Function::Builtin(name) => apply_builtin_fn(
          name,
          arg_values.into_iter().zip(arg_types.into_iter()).collect(),
        )?,
        Function::Constructor(field_names) => Value::Struct(
          field_names
            .into_iter()
            .zip(arg_values.into_iter())
            .collect(),
        ),
        Function::Composite { arg_names, body } => {
          if arg_names.len() != arg_values.len() {
            return Err(WrongArity);
          }
          for (name, value) in arg_names.iter().zip(arg_values.into_iter()) {
            env.bind(name.clone(), value);
          }
          let value = eval(body, env)?;
          for name in arg_names.iter() {
            env.unbind(name);
          }
          value
        }
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
      env.bind(increment_variable_name.clone(), initial_value);
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
      env.unbind(&increment_variable_name);
      Primitive::Unit.into()
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
      Primitive::Unit.into()
    }
    ExpKind::Break => todo!("can't handle breaks in interpreter yet"),
    ExpKind::Continue => todo!("can't handle continue in interpreter yet"),
    ExpKind::Discard => todo!("can't handle discard in interpreter yet"),
    ExpKind::Return(_) => todo!("can't handle return in interpreter yet"),
    ExpKind::ArrayLiteral(exps) => Value::Array(
      exps
        .into_iter()
        .map(|exp| eval(exp, env))
        .collect::<Result<_, _>>()?,
    ),
    ExpKind::Reference(exp) => eval(*exp, env)?,
    ExpKind::Uninitialized => Value::Uninitialized,
    ExpKind::ZeroedArray => Value::zeroed(exp.data.kind.unwrap_known(), env)?,
  })
}
