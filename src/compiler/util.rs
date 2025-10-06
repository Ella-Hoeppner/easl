use std::rc::Rc;

use sse::syntax::EncloserOrOperator;

use crate::parse::{EaslTree, Operator};

use super::error::{CompileErrorKind::*, CompileResult, SourceTrace, err};

pub fn indent(s: String) -> String {
  s.replace("\n", "\n  ")
}

pub fn read_leaf(ast: EaslTree) -> CompileResult<Rc<str>> {
  if let EaslTree::Leaf(_, word) = ast {
    Ok(word.into())
  } else {
    err(ExpectedLeaf, SourceTrace::empty())
  }
}

pub fn read_type_annotated_name(
  ast: EaslTree,
) -> CompileResult<(Rc<str>, EaslTree)> {
  if let EaslTree::Inner(
    (position, EncloserOrOperator::Operator(Operator::TypeAscription)),
    mut children,
  ) = ast
  {
    let type_ast = children.remove(1);
    let name_ast = children.remove(0);
    if let EaslTree::Leaf(_, name) = name_ast {
      Ok((name.into(), type_ast))
    } else {
      err(ExpectedTypeAnnotatedName, position.into())
    }
  } else {
    err(ExpectedTypeAnnotatedName, ast.position().clone().into())
  }
}

pub fn compile_word(word: Rc<str>) -> String {
  word
    .replace("-", "_")
    .replace("+", "PLUS")
    .replace("*", "STAR")
    .replace(">", "ABRACKET_RIGHT")
    .replace("<", "ABRACKET_LEFT")
}
