use crate::parse::TyntTree;

use super::types::CompileError;

#[derive(Debug, Clone, PartialEq)]
pub enum Metadata {
  Singular(String),
  Map(Vec<(String, String)>),
}

impl Metadata {
  pub fn from_metadata_tree(ast: TyntTree) -> Result<Self, CompileError> {
    todo!()
  }
}
