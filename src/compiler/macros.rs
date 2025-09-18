use std::rc::Rc;

use crate::{compiler::program::NameContext, parse::EaslTree};

use super::error::{CompileError, CompileErrorKind, ErrorLog, SourceTrace};

pub struct Macro {
  pub reserved_names: Vec<Rc<str>>,
  pub rewrite: Box<
    dyn Fn(
      &EaslTree,
      &mut NameContext,
    ) -> Option<Result<EaslTree, (SourceTrace, Rc<str>)>>,
  >,
}

pub fn macroexpand(
  tree: EaslTree,
  macros: &Vec<Macro>,
  names: &mut NameContext,
  errors: &mut ErrorLog,
) -> EaslTree {
  let mut new_tree = match tree {
    EaslTree::Inner(data, children) => {
      let new_tree = children
        .into_iter()
        .map(|subtree| macroexpand(subtree, macros, names, errors))
        .collect::<Vec<EaslTree>>();
      EaslTree::Inner(data, new_tree)
    }
    leaf => leaf,
  };
  for Macro { rewrite, .. } in macros {
    if let Some(r) = rewrite(&new_tree, names) {
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
