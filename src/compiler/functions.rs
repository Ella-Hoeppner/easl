use super::{
  error::CompileError, expression::TypedExp, metadata::Metadata,
  types::TyntType,
};

pub struct TopLevelFunction {
  pub name: String,
  pub arg_metadata: Vec<Option<Metadata>>,
  pub return_metadata: Option<Metadata>,
  pub metadata: Option<Metadata>,
  pub exp: TypedExp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
  pub arg_types: Vec<TyntType>,
  pub return_type: TyntType,
}

pub struct BuiltInFunction {
  pub name: String,
  pub signature: FunctionSignature,
}

impl TopLevelFunction {
  pub fn compile(self) -> Result<String, CompileError> {
    todo!()
  }
}
