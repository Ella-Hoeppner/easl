use crate::{compiler::ir::structs::UntypedStruct, parse::TyntTree};

use super::{
  environment::Environment,
  expression::{
    Exp::{self, *},
    ExpNode, Number,
  },
  structs::Struct,
};
use core::fmt::Debug;

pub enum CompileError {
  TypeError(TypeError),
  InvalidMetadata,
  ExpectedTypeAnnotatedName,
  InvalidStructField,
  InvalidStructName,
  InvalidTopLevelVar(String),
  InvalidDef(String),
  InvalidFunction(String),
  UnrecognizedTopLevelForm,
}

pub enum TypeError {
  CouldntInfer,
  IncompatibleConstraints,
  UnknownFunction,
  InvalidArity,
  UnrecognizedTypeName,
}

impl From<TypeError> for CompileError {
  fn from(err: TypeError) -> Self {
    Self::TypeError(err)
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyntType {
  None,
  F32,
  I32,
  U32,
  Bool,
  Struct(String),
}
impl TyntType {
  pub fn from_name(
    name: String,
    struct_names: &Vec<String>,
  ) -> Result<Self, CompileError> {
    use TyntType::*;
    Ok(match name.as_str() {
      "F32" => F32,
      "I32" => I32,
      "U32" => U32,
      "Bool" => Bool,
      _ => {
        if struct_names.contains(&name) {
          Struct(name)
        } else {
          return Err(TypeError::UnrecognizedTypeName.into());
        }
      }
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeConstraints {
  Anything,
  OneOf(Vec<TyntType>),
  Definitely(TyntType),
}
use TypeConstraints::*;

impl TypeConstraints {
  fn restrict(&mut self, constraints: Self) -> Result<bool, TypeError> {
    match (self.clone(), constraints) {
      (Anything, Anything) => Ok(false),
      (_, Anything) => Ok(true),
      (Anything, mut constraints) => {
        std::mem::swap(self, &mut constraints);
        Ok(true)
      }
      (Definitely(a), Definitely(b)) => {
        if a == b {
          Ok(false)
        } else {
          Err(TypeError::IncompatibleConstraints)
        }
      }
      (OneOf(a), OneOf(b)) => {
        if a == b {
          return Ok(false);
        }
        let narrowed: Vec<_> =
          a.into_iter().filter(|a| b.contains(&a)).collect();
        if narrowed.is_empty() {
          Err(TypeError::IncompatibleConstraints)
        } else {
          std::mem::swap(
            self,
            &mut if narrowed.len() == 1 {
              Definitely(narrowed.into_iter().next().unwrap())
            } else {
              OneOf(narrowed)
            },
          );
          Ok(true)
        }
      }
      (OneOf(a), Definitely(b)) => {
        if a.contains(&b) {
          std::mem::swap(self, &mut Definitely(b));
          Ok(true)
        } else {
          Err(TypeError::IncompatibleConstraints)
        }
      }
      (Definitely(a), OneOf(b)) => {
        if b.contains(&a) {
          std::mem::swap(self, &mut Definitely(a));
          Ok(false)
        } else {
          Err(TypeError::IncompatibleConstraints)
        }
      }
    }
  }
}

pub enum SharedTypeConstraints {
  Representative(TypeConstraints),
  JoinedTo(usize),
}

pub struct TypeMap {
  types: Vec<SharedTypeConstraints>,
}

impl TypeMap {
  pub fn empty() -> Self {
    Self { types: vec![] }
  }
  pub fn new(types: Vec<SharedTypeConstraints>) -> Self {
    Self { types }
  }
  pub fn get_type_representative_index(&self, i: usize) -> usize {
    match self.types[i] {
      SharedTypeConstraints::Representative(_) => i,
      SharedTypeConstraints::JoinedTo(parent) => {
        self.get_type_representative_index(parent)
      }
    }
  }
  pub fn get_type(&self, i: usize) -> TypeConstraints {
    if let SharedTypeConstraints::Representative(t) =
      &self.types[self.get_type_representative_index(i)]
    {
      t.clone()
    } else {
      unreachable!()
    }
  }
  pub fn join(&mut self, a: usize, b: usize) -> Result<bool, TypeError> {
    let a_representative = self.get_type_representative_index(a);
    let b_representative = self.get_type_representative_index(b);
    if a_representative == b_representative {
      return Ok(false);
    }
    let mut b_constraints = SharedTypeConstraints::JoinedTo(a);
    std::mem::swap(&mut self.types[b_representative], &mut b_constraints);
    Ok(
      if let SharedTypeConstraints::Representative(a_type_ref) =
        &mut self.types[a_representative]
      {
        if let SharedTypeConstraints::Representative(b_type) = b_constraints {
          a_type_ref.restrict(b_type)?
        } else {
          unreachable!()
        }
      } else {
        unreachable!()
      },
    )
  }
  pub fn constrain(
    &mut self,
    index: usize,
    constraints: TypeConstraints,
  ) -> Result<bool, TypeError> {
    let representative_index = self.get_type_representative_index(index);
    if let SharedTypeConstraints::Representative(t) =
      &mut self.types[representative_index]
    {
      t.restrict(constraints)
    } else {
      unreachable!()
    }
  }
}

impl ExpNode<(usize, Environment)> {
  pub fn build_from_tynt_tree(
    tree: TyntTree,
    parent_env: &Environment,
    tree_type_constraints: &mut Vec<SharedTypeConstraints>,
  ) -> Self {
    let mut build_type_constraints =
      |exp, (constraints, env)| -> Result<ExpNode<(usize, Environment)>, !> {
        let constraint_index = tree_type_constraints.len();
        tree_type_constraints
          .push(SharedTypeConstraints::Representative(constraints));
        Ok(ExpNode {
          exp: exp,
          data: (constraint_index, env),
        })
      };
    match tree {
      TyntTree::Leaf(_, leaf) => {
        if leaf == "true" || leaf == "false" {
          ExpNode {
            exp: Box::new(Exp::BooleanLiteral(leaf == "true")),
            data: (Definitely(TyntType::Bool), parent_env.clone()),
          }
        } else if let Ok(i) = leaf.parse::<i64>() {
          ExpNode {
            exp: Box::new(Exp::NumberLiteral(Number::Int(i))),
            data: if i < 0 {
              (
                OneOf(vec![TyntType::I32, TyntType::F32]),
                parent_env.clone(),
              )
            } else {
              (
                OneOf(vec![TyntType::I32, TyntType::U32, TyntType::U32]),
                parent_env.clone(),
              )
            },
          }
        } else if let Ok(f) = leaf.parse::<f64>() {
          ExpNode {
            exp: Box::new(Exp::NumberLiteral(Number::Float(f))),
            data: (Definitely(TyntType::F32), parent_env.clone()),
          }
        } else {
          ExpNode {
            exp: Box::new(Exp::Name(leaf)),
            data: (Anything, parent_env.clone()),
          }
        }
      }
      TyntTree::Inner((_, encloser_or_operator), children) => {
        use crate::parse::Encloser::*;
        use crate::parse::Operator::*;
        use sse::syntax::EncloserOrOperator::*;
        match encloser_or_operator {
          Encloser(e) => match e {
            Parens => todo!(),
            Square => todo!(),
            Curly => todo!("I haven't implemented anonymous structs yet!!"),
          },
          Operator(o) => match o {
            Metadata => todo!(),
            TypeAnnotation => todo!(),
          },
        }
      }
    }
    .walk(&mut |node| Ok(node), &mut build_type_constraints)
    .unwrap()
  }
}
