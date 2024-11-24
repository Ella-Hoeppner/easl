use crate::parse::EaslTree;

use super::error::SourceTrace;

pub struct Macro(
  pub  Box<
    dyn Fn(
      EaslTree,
    ) -> Result<Result<EaslTree, (SourceTrace, String)>, EaslTree>,
  >,
);

pub fn macroexpand(
  tree: EaslTree,
  macros: &Vec<Macro>,
) -> Result<EaslTree, (SourceTrace, String)> {
  let new_tree = match tree {
    EaslTree::Inner(data, children) => EaslTree::Inner(
      data,
      children
        .into_iter()
        .map(|subtree| macroexpand(subtree, macros))
        .collect::<Result<Vec<EaslTree>, (SourceTrace, String)>>()?,
    ),
    leaf => leaf,
  };
  macros.iter().fold(Ok(new_tree), |tree_result, Macro(f)| {
    tree_result
      .map(|tree| match f(tree) {
        Ok(Ok(new_tree)) => Ok(macroexpand(new_tree, macros)?),
        Ok(Err(macro_failure)) => Err(macro_failure),
        Err(original_tree) => Ok(original_tree),
      })
      .flatten()
  })
}
