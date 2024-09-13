use core::fmt::Debug;
use std::collections::HashMap;

use sse::syntax::EncloserOrOperator;

use crate::parse::{Operator, TyntTree};

use super::{
  builtins::{built_in_functions, built_in_multi_signature_functions},
  error::CompileError,
  functions::FunctionSignature,
  structs::Struct,
  util::compile_word,
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
  pub fn compile(&self) -> String {
    match self {
      TyntType::None => panic!("Attempted to compile None type"),
      TyntType::F32 => "f32".to_string(),
      TyntType::I32 => "i32".to_string(),
      TyntType::U32 => "u32".to_string(),
      TyntType::Bool => "bool".to_string(),
      TyntType::Struct(name) => compile_word(name.clone()),
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
  OneOf(Vec<TyntType>),
  Known(TyntType),
}

impl TypeState {
  pub fn constrain(
    &mut self,
    mut other: TypeState,
  ) -> Result<bool, CompileError> {
    let changed = match (&self, &other) {
      (_, TypeState::Unknown) => false,
      (TypeState::Unknown, _) => {
        std::mem::swap(self, &mut other);
        true
      }
      (TypeState::Known(current_type), TypeState::Known(new_type)) => {
        if current_type != new_type {
          return Err(CompileError::IncompatibleTypes);
        }
        false
      }
      (
        TypeState::OneOf(possibilities),
        TypeState::OneOf(other_possibilities),
      ) => {
        let mut new_possibilities = vec![];
        let mut changed = false;
        for possibility in possibilities {
          if other_possibilities.contains(possibility) {
            new_possibilities.push(possibility.clone());
          } else {
            changed = true;
          }
        }
        std::mem::swap(
          self,
          &mut TypeState::OneOf(new_possibilities).simplified()?,
        );
        changed
      }
      (TypeState::OneOf(possibilities), TypeState::Known(t)) => {
        if possibilities.contains(t) {
          std::mem::swap(self, &mut TypeState::Known(t.clone()));
          true
        } else {
          return Err(CompileError::IncompatibleTypes);
        }
      }
      (TypeState::Known(t), TypeState::OneOf(possibilities)) => {
        if !possibilities.contains(t) {
          return Err(CompileError::IncompatibleTypes);
        }
        false
      }
    };
    self.simplify()?;
    Ok(changed)
  }
  pub fn mutually_constrain(
    &mut self,
    other: &mut TypeState,
  ) -> Result<bool, CompileError> {
    let self_changed = self.constrain(other.clone())?;
    let other_changed = other.constrain(self.clone())?;
    Ok(self_changed || other_changed)
  }
  pub fn constrain_fn_by_argument_types(
    &mut self,
    arg_types: Vec<TypeState>,
  ) -> Result<bool, CompileError> {
    match self {
      TypeState::OneOf(possibilities) => {
        let mut new_possibilities = possibilities
          .iter()
          .filter_map(|t| match t {
            TyntType::Function(signature) => {
              if signature.are_args_compatible(&arg_types) {
                Some(t.clone())
              } else {
                None
              }
            }
            _ => panic!("tried to constrain fn on non-fn"),
          })
          .collect::<Vec<_>>();
        if new_possibilities.len() == possibilities.len() {
          Ok(false)
        } else {
          if new_possibilities.is_empty() {
            Err(CompileError::IncompatibleTypes)
          } else {
            std::mem::swap(
              self,
              &mut if new_possibilities.len() == 1 {
                TypeState::Known(new_possibilities.remove(0))
              } else {
                TypeState::OneOf(new_possibilities)
              },
            );
            Ok(true)
          }
        }
      }
      TypeState::Known(t) => match t {
        TyntType::Function(signature) => {
          if signature.are_args_compatible(&arg_types) {
            Ok(false)
          } else {
            Err(CompileError::IncompatibleTypes)
          }
        }
        _ => panic!("tried to constrain fn on non-fn"),
      },
      TypeState::Unknown => Ok(false),
    }
  }
  pub fn is_compatible(&self, t: &TyntType) -> bool {
    match self {
      TypeState::Unknown => true,
      TypeState::OneOf(possibilities) => possibilities.contains(t),
      TypeState::Known(known_t) => known_t == t,
    }
  }
  pub fn simplify(&mut self) -> Result<(), CompileError> {
    Ok(if let TypeState::OneOf(mut possibilities) = self.clone() {
      possibilities.dedup();
      std::mem::swap(
        self,
        &mut match possibilities.len() {
          0 => return Err(CompileError::IncompatibleTypes),
          1 => TypeState::Known(possibilities.remove(0)),
          _ => TypeState::OneOf(possibilities),
        },
      )
    })
  }
  pub fn simplified(self) -> Result<Self, CompileError> {
    Ok(if let TypeState::OneOf(mut possibilities) = self {
      possibilities.dedup();
      match possibilities.len() {
        0 => return Err(CompileError::IncompatibleTypes),
        1 => TypeState::Known(possibilities.remove(0)),
        _ => TypeState::OneOf(possibilities),
      }
    } else {
      self
    })
  }
  pub fn compile(&self) -> String {
    if let TypeState::Known(t) = self {
      t.compile()
    } else {
      panic!("attempted to compile TypeState that wasn't Known")
    }
  }
}

#[derive(Debug, Clone)]
pub struct Context {
  pub structs: Vec<Struct>,
  pub multi_signature_functions: HashMap<String, Vec<FunctionSignature>>,
  pub bindings: HashMap<String, Vec<TypeState>>,
}

impl Context {
  pub fn default_global() -> Self {
    let mut multi_signature_functions = HashMap::new();
    for (name, signatures) in built_in_multi_signature_functions() {
      multi_signature_functions.insert(name.to_string(), signatures);
    }
    let mut ctx = Context {
      structs: vec![],
      multi_signature_functions,
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
      || self.multi_signature_functions.contains_key(name)
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
  pub fn with_structs(mut self, structs: Vec<Struct>) -> Self {
    for s in &structs {
      if s.has_normal_constructor {
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
    }
    self.structs = structs;
    self
  }
  pub fn constrain_name_type(
    &mut self,
    name: &str,
    t: &mut TypeState,
  ) -> Result<bool, CompileError> {
    if let Some(signatures) = self.multi_signature_functions.get(name) {
      t.constrain(TypeState::OneOf(
        signatures
          .iter()
          .cloned()
          .map(|signature| TyntType::Function(Box::new(signature)))
          .collect(),
      ))
    } else {
      t.mutually_constrain(self.get_typestate_mut(name)?)
    }
  }
}
