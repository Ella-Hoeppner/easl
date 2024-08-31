use crate::parse::TyntTree;

use super::types::CompileError;

#[derive(Debug, Clone, PartialEq)]
pub enum ExpMetadata {
  Singular(String),
  Map(Vec<(String, String)>),
}

impl ExpMetadata {
  pub fn from_metadata_expression(exp: TyntTree) -> Result<Self, CompileError> {
    todo!()
  }
}
