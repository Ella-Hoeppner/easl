use core::fmt::Debug;
use std::collections::HashMap;

use sse::syntax::EncloserOrOperator;

use crate::parse::{Operator, TyntTree};

use super::{error::CompileError, functions::FunctionSignature};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyntType {
  None,
  F32,
  I32,
  U32,
  Bool,
  Struct(String),
  Function(Box<FunctionSignature>),
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
          return Err(CompileError::UnrecognizedTypeName);
        }
      }
    })
  }
  pub fn from_tynt_tree(
    tree: TyntTree,
    struct_names: &Vec<String>,
  ) -> Result<Self, CompileError> {
    if let TyntTree::Leaf(_, type_name) = tree {
      Ok(TyntType::from_name(type_name, struct_names)?)
    } else {
      Err(CompileError::InvalidType)
    }
  }
}

pub fn extract_type_annotation(
  exp: TyntTree,
  struct_names: &Vec<String>,
) -> Result<(Option<TyntType>, TyntTree), CompileError> {
  Ok(
    if let TyntTree::Inner(
      (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
      mut children,
    ) = exp
    {
      let t = TyntType::from_tynt_tree(children.remove(1), struct_names)?;
      (Some(t), children.remove(0))
    } else {
      (None, exp)
    },
  )
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeState {
  Unknown,
  Known(TyntType),
}

impl TypeState {
  pub fn constrain(
    &mut self,
    mut other: TypeState,
  ) -> Result<bool, CompileError> {
    Ok(match (&self, &other) {
      (TypeState::Unknown, TypeState::Unknown) => false,
      (TypeState::Unknown, TypeState::Known(_)) => {
        std::mem::swap(self, &mut other);
        true
      }
      (TypeState::Known(_), TypeState::Unknown) => false,
      (TypeState::Known(current_type), TypeState::Known(new_type)) => {
        if current_type == new_type {
          return Err(CompileError::IncompatibleTypes);
        }
        false
      }
    })
  }
  pub fn mutually_constrain(
    &mut self,
    other: &mut TypeState,
  ) -> Result<bool, CompileError> {
    let self_changed = self.constrain(other.clone())?;
    let other_changed = other.constrain(self.clone())?;
    Ok(self_changed || other_changed)
  }
}

pub struct Context {
  bindings: HashMap<String, Vec<TypeState>>,
}

impl Context {
  pub fn bind(&mut self, name: &str, t: TypeState) {
    if !self.bindings.contains_key(name) {
      self.bindings.insert(name.to_string(), vec![]);
    }
    self.bindings.get_mut(name).unwrap().push(t);
  }
  pub fn unbind(&mut self, name: &str) -> TypeState {
    let name_bindings = self.bindings.get_mut(name).unwrap();
    let t = name_bindings.pop().unwrap();
    if name_bindings.is_empty() {
      self.bindings.remove(name);
    }
    t
  }
  pub fn is_bound(&self, name: &str) -> bool {
    self.bindings.contains_key(name)
  }
  pub fn get_typestate_mut(&mut self, name: &str) -> &mut TypeState {
    self.bindings.get_mut(name).unwrap().last_mut().unwrap()
  }
}
