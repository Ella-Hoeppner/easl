use super::{environment::Environment, expression::Expression};

#[derive(Debug, Clone, PartialEq)]
pub enum TyntType {
  None,
  Float,
  Int,
  Bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypePossibilities {
  Any,
  OneOf(Vec<TyntType>),
  Definitely(TyntType),
}

enum TypeInferenceError {}

impl Expression<TypePossibilities> {
  fn infer_types(
    self,
    env: Environment,
  ) -> Result<Expression<TyntType>, TypeInferenceError> {
    todo!()
  }
}
