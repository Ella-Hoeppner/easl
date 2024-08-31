use crate::compiler::error::CompileError;

use super::{expression::ExpNode, types::TyntType};

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

impl ExpNode<TyntType> {
  fn compile(self, context: ExpressionContext) -> Result<String, CompileError> {
    todo!()
  }
}
