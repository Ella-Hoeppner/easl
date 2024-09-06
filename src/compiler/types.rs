use core::fmt::Debug;

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
