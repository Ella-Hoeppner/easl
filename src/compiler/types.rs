use core::fmt::Debug;
use std::collections::HashMap;

use sse::syntax::EncloserOrOperator;

use crate::parse::{Operator, TyntTree};

use super::{
  builtins::built_in_functions, error::CompileError,
  functions::FunctionSignature, structs::Struct, util::compile_word,
};

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
      "F32" | "f32" => F32,
      "I32" | "i32" => I32,
      "U32" | "u32" => U32,
      "Bool" => Bool,
      _ => {
        if struct_names.contains(&name) {
          Struct(name)
        } else {
          return Err(CompileError::UnrecognizedTypeName(name));
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
  pub fn compile(self) -> String {
    match self {
      TyntType::None => panic!("Attempted to compile None type"),
      TyntType::F32 => "f32".to_string(),
      TyntType::I32 => "i32".to_string(),
      TyntType::U32 => "u32".to_string(),
      TyntType::Bool => "bool".to_string(),
      TyntType::Struct(name) => compile_word(name),
      TyntType::Function(_) => panic!("Attempted to compile Function type"),
    }
  }
}

pub fn extract_type_annotation_ast(
  exp: TyntTree,
) -> Result<(Option<TyntTree>, TyntTree), CompileError> {
  Ok(
    if let TyntTree::Inner(
      (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
      mut children,
    ) = exp
    {
      (Some(children.remove(1)), children.remove(0))
    } else {
      (None, exp)
    },
  )
}

pub fn extract_type_annotation(
  exp: TyntTree,
  struct_names: &Vec<String>,
) -> Result<(Option<TyntType>, TyntTree), CompileError> {
  let (t, value) = extract_type_annotation_ast(exp)?;
  Ok((
    t.map(|t| TyntType::from_tynt_tree(t, struct_names))
      .map_or(Ok(None), |v| v.map(Some))?,
    value,
  ))
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
        if current_type != new_type {
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
  pub bindings: HashMap<String, Vec<TypeState>>,
}

impl Context {
  pub fn default_global() -> Self {
    let mut ctx = Context {
      bindings: HashMap::new(),
    };
    for (name, signature) in built_in_functions() {
      ctx.bind(
        name,
        TypeState::Known(TyntType::Function(Box::new(signature))),
      );
    }
    ctx
  }
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
  pub fn get_typestate_mut(
    &mut self,
    name: &str,
  ) -> Result<&mut TypeState, CompileError> {
    Ok(
      self
        .bindings
        .get_mut(name)
        .ok_or_else(|| CompileError::UnboundName(name.to_string()))?
        .last_mut()
        .unwrap(),
    )
  }
  pub fn with_struct_constructors(mut self, structs: &Vec<Struct>) -> Self {
    for s in structs {
      self.bind(
        &s.name,
        TypeState::Known(TyntType::Function(Box::new(FunctionSignature {
          arg_types: s
            .fields
            .iter()
            .map(|field| field.field_type.clone())
            .collect(),
          return_type: TyntType::Struct(s.name.clone()),
        }))),
      )
    }
    self
  }
}
