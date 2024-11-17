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
    err(ExpectedLeaf, vec![])
  }
}

pub fn read_type_annotated_name(
  ast: TyntTree,
) -> CompileResult<(String, TyntTree)> {
  if let TyntTree::Inner(
    (position, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut children,
  ) = ast
  {
    let type_ast = children.remove(1);
    let name_ast = children.remove(0);
    if let TyntTree::Leaf(_, name) = name_ast {
      Ok((name, type_ast))
    } else {
      err(ExpectedTypeAnnotatedName, vec![position.path])
    }
  } else {
    err(ExpectedTypeAnnotatedName, vec![ast.position().path.clone()])
  }
}

pub fn compile_word(word: String) -> String {
  word.replace("-", "_")
}
