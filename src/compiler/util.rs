use sse::syntax::EncloserOrOperator;

use crate::parse::{Operator, TyntTree};

use super::error::{err, CompileErrorKind::*, CompileResult};

pub fn indent(s: String) -> String {
  s.replace("\n", "\n  ")
}

pub fn read_leaf(ast: TyntTree) -> CompileResult<String> {
  if let TyntTree::Leaf(_, word) = ast {
    Ok(word)
  } else {
    err(ExpectedLeaf)
  }
}

pub fn read_type_annotated_name(
  exp: TyntTree,
) -> CompileResult<(String, String)> {
  if let TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut children,
  ) = exp
  {
    if let TyntTree::Leaf(_, type_name) = children.remove(1) {
      if let TyntTree::Leaf(_, name) = children.remove(0) {
        Ok((name, type_name))
      } else {
        err(ExpectedTypeAnnotatedName)
      }
    } else {
      err(ExpectedTypeAnnotatedName)
    }
  } else {
    err(ExpectedTypeAnnotatedName)
  }
}

pub fn compile_word(word: String) -> String {
  word.replace("-", "_")
}
