use std::rc::Rc;

use crate::parse::EaslTree;

use super::error::{CompileError, CompileErrorKind, ErrorLog, SourceTrace};

pub struct Macro(
  pub Box<dyn Fn(&EaslTree) -> Option<Result<EaslTree, (SourceTrace, Rc<str>)>>>,
);

pub fn macroexpand(
  tree: EaslTree,
  macros: &Vec<Macro>,
  errors: &mut ErrorLog,
) -> EaslTree {
  let mut new_tree = match tree {
    EaslTree::Inner(data, children) => {
      let new_tree = children
        .into_iter()
        .map(|subtree| macroexpand(subtree, macros, errors))
        .collect::<Vec<EaslTree>>();
      EaslTree::Inner(data, new_tree)
    }
    leaf => leaf,
  };
  for Macro(f) in macros {
    if let Some(r) = f(&new_tree) {
      match r {
        Ok(replacement_tree) => new_tree = replacement_tree,
        Err((source_trace, err)) => errors.log(CompileError {
          kind: CompileErrorKind::MacroError(err.to_string()),
          source_trace,
        }),
      }
    }
  }
  new_tree
}
