use crate::compiler::error::CompileError;

use super::{expression::Expression, types::TyntType};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ExpressionContext {
  return_value: bool,
  top_level: bool,
}
impl ExpressionContext {
  fn not_top_level(mut self) -> Self {
    self.top_level = false;
    self
  }
}

impl Expression<TyntType> {
  fn compile(self, context: ExpressionContext) -> Result<String, CompileError> {
    todo!()
  }
}
