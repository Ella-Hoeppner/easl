use sse::syntax::EncloserOrOperator;

use crate::parse::{parse_tynt, Encloser, Operator, TyntTree};

use super::{
  error::CompileError, functions::compile_function, metadata::compile_metadata,
  structs::compile_struct, var::compile_top_level_var,
};

pub fn indent(s: String) -> String {
  "  ".to_string() + &s.replace("\n", "\n  ")
}

pub fn compile_top_level_form(form: TyntTree) -> Result<String, CompileError> {
  match form {
    TyntTree::Leaf(_, label) => Ok(label.replace("-", "_")),
    TyntTree::Inner((_, encloser_or_operator), mut children) => {
      match encloser_or_operator {
        EncloserOrOperator::Encloser(Encloser::Parens) => {
          let mut children_iter = children.into_iter();
          if let Some(TyntTree::Leaf(_, first_symbol)) = children_iter.next() {
            match first_symbol.as_str() {
              "var" => compile_top_level_var(children_iter.collect()),
              "struct" => compile_struct(children_iter.collect()),
              "defn" => compile_function(children_iter.collect()),
              _ => Err(CompileError::UnrecognizedTopLevelForm),
            }
          } else {
            Err(CompileError::UnrecognizedTopLevelForm)
          }
        }
        EncloserOrOperator::Operator(Operator::Metadata) => {
          let body = compile_top_level_form(children.remove(1))?;
          let metadata = compile_metadata(children.remove(0))?;
          Ok(metadata + &body)
        }
        _ => Err(CompileError::UnrecognizedTopLevelForm),
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
      .map(|tree| compile_top_level_form(tree))
      .collect::<Result<Vec<String>, CompileError>>()?
      .into_iter()
      .fold(String::new(), |text, tree| text + "\n\n" + &tree)
      + "\n",
  )
}
