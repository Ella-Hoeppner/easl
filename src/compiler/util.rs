use sse::syntax::EncloserOrOperator;

use crate::parse::{Operator, TyntTree};

use super::error::CompileError;

pub fn indent(s: String) -> String {
  s.replace("\n", "\n  ")
}

pub fn read_leaf(ast: TyntTree) -> Result<String, CompileError> {
  if let TyntTree::Leaf(_, word) = ast {
    Ok(word)
  } else {
    Err(CompileError::ExpectedLeaf)
  }
}

pub fn read_type_annotated_name(
  exp: TyntTree,
) -> Result<(String, String), CompileError> {
  if let TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut children,
  ) = exp
  {
    if let TyntTree::Leaf(_, type_name) = children.remove(1) {
      let x = children.remove(0);
      if let TyntTree::Leaf(_, name) = x {
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

pub fn compile_word(word: String) -> String {
  word.replace("-", "_")
}
