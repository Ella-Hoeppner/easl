use sse::syntax::EncloserOrOperator;

use crate::parse::{Operator, TyntTree};

use super::error::CompileError;

pub fn read_type_annotated_name(
  exp: TyntTree,
) -> Result<(String, String), CompileError> {
  if let TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut children,
  ) = exp
  {
    if let TyntTree::Leaf(_, type_name) = children.remove(1) {
      if let TyntTree::Leaf(_, name) = children.remove(0) {
        Ok((name, type_name))
      } else {
        Err(CompileError::ExpectedTypeAnnotatedName)
      }
    } else {
      Err(CompileError::ExpectedTypeAnnotatedName)
    }
  } else {
    Err(CompileError::ExpectedTypeAnnotatedName)
  }
}

pub fn tynt_word_to_wgsl_word(word: String) -> String {
  word.replace("-", "_")
}
