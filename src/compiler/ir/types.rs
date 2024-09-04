use core::fmt::Debug;

use crate::parse::TyntTree;

pub enum CompileError {
  TypeError(TypeError),
  InvalidMetadata(String),
  ExpectedTypeAnnotatedName,
  InvalidStructField,
  InvalidStructName,
  InvalidTopLevelVar(String),
  InvalidDef(String),
  InvalidFunction(String),
  UnrecognizedTopLevelForm,
  EmptyList,
  InvalidType,
  FunctionArgMissingType,
  InvalidArgumentName,
}

pub enum TypeError {
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
  Function(Vec<TyntType>, Box<TyntType>),
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
