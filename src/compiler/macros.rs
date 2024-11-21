use crate::parse::TyntTree;

use super::error::SourceTrace;

pub struct Macro(
  pub  Box<
    dyn Fn(
      TyntTree,
    ) -> Result<Result<TyntTree, (SourceTrace, String)>, TyntTree>,
  >,
);

pub fn macroexpand(
  tree: TyntTree,
  macros: &Vec<Macro>,
) -> Result<TyntTree, (SourceTrace, String)> {
  let new_tree = match tree {
    TyntTree::Inner(data, children) => TyntTree::Inner(
      data,
      children
        .into_iter()
        .map(|subtree| macroexpand(subtree, macros))
        .collect::<Result<Vec<TyntTree>, (SourceTrace, String)>>()?,
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
