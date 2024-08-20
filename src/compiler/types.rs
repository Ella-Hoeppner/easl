use sse::syntax::EncloserOrOperator;

use crate::parse::{Operator, TyntTree};

use super::{
  error::CompileError, metadata::compile_expression_possibly_with_metadata,
  word::compile_word,
};

pub fn compile_type(type_expression: TyntTree) -> Result<String, CompileError> {
  compile_word(type_expression)
}

pub fn compile_type_annotated_expression(
  expression: TyntTree,
  subexpression_compiler: impl Fn(TyntTree) -> Result<String, CompileError>,
) -> Result<String, CompileError> {
  if let TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut children,
  ) = expression
  {
    let type_expression = children.remove(1);
    let expression = children.remove(0);
    Ok(
      subexpression_compiler(expression)?
        + ": "
        + &compile_type(type_expression)?,
    )
  } else {
    Err(CompileError::ExpectedTypeAnnotatedName)
  }
}

pub fn compile_type_annotated_name(
  expression: TyntTree,
) -> Result<String, CompileError> {
  compile_type_annotated_expression(expression, |subexpression| {
    compile_expression_possibly_with_metadata(subexpression, compile_word)
  })
}
