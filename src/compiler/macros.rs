use std::rc::Rc;

use crate::parse::EaslTree;

use super::error::SourceTrace;

pub struct Macro(
  pub Box<dyn Fn(&EaslTree) -> Option<Result<EaslTree, (SourceTrace, Rc<str>)>>>,
);

pub fn macroexpand(
  tree: EaslTree,
  macros: &Vec<Macro>,
) -> (EaslTree, Vec<(SourceTrace, Rc<str>)>) {
  let (mut new_tree, mut child_errors) = match tree {
    EaslTree::Inner(data, children) => {
      let (new_tree, child_errors) = children
        .into_iter()
        .map(|subtree| macroexpand(subtree, macros))
        .collect::<(Vec<EaslTree>, Vec<Vec<(SourceTrace, Rc<str>)>>)>();
      (
        EaslTree::Inner(data, new_tree),
        child_errors.into_iter().flatten().collect(),
      )
    }
    leaf => (leaf, vec![]),
  };
  for Macro(f) in macros {
    if let Some(r) = f(&new_tree) {
      match r {
        Ok(replacement_tree) => new_tree = replacement_tree,
        Err(err) => child_errors.push(err),
      }
    }
  }
  (new_tree, child_errors)
}
