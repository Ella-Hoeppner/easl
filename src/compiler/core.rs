use sse::{syntax::EncloserOrOperator, RawSexp};

use crate::parse::{parse_tynt, Encloser, Operator, TyntTree};

use super::{
  error::CompileError, functions::compile_function, metadata::compile_metadata,
  structs::compile_struct, var::compile_top_level_var,
};

pub fn compile_top_level_tynt_expression(
  expression: TyntTree,
) -> Result<String, CompileError> {
  let original_expression = expression.clone();
  match expression {
    TyntTree::Leaf(_, label) => Ok(label.replace("-", "_")),
    TyntTree::Inner((_, encloser_or_operator), mut children) => {
      match encloser_or_operator {
        EncloserOrOperator::Encloser(Encloser::Parens) => {
          let mut children_iter = children.into_iter();
          if let Some(TyntTree::Leaf(_, first_symbol)) = children_iter.next() {
            match first_symbol.as_str() {
              "var" => compile_top_level_var(children_iter.collect()),
              "struct" => compile_struct(children_iter.collect()),
              "fn" => compile_function(children_iter.collect()),
              _ => Ok(RawSexp::from(original_expression).to_string()),
            }
          } else {
            Err(CompileError::UnrecognizedTopLevelExpression)
          }
        }
        EncloserOrOperator::Operator(Operator::Metadata) => {
          let body = compile_top_level_tynt_expression(children.remove(1))?;
          let metadata = compile_metadata(children.remove(0))?;
          Ok(metadata + &body)
        }
        _ => Err(CompileError::UnrecognizedTopLevelExpression),
      }
    }
  }
}

pub fn compile_tynt(tynt_source: &str) -> Result<String, CompileError> {
  let document = parse_tynt(tynt_source)?;
  Ok(
    document
      .syntax_trees
      .into_iter()
      .map(|tree| compile_top_level_tynt_expression(tree))
      .collect::<Result<Vec<String>, CompileError>>()?
      .into_iter()
      .fold(String::new(), |text, tree| text + "\n\n" + &tree)
      + "\n",
  )
}
