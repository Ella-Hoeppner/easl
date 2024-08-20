use crate::parse::TyntTree;

use super::{
  error::CompileError, types::compile_type_annotated_name,
  word::tynt_word_to_wgsl_word,
};

pub fn compile_struct(
  expressions: Vec<TyntTree>,
) -> Result<String, CompileError> {
  let mut expression_iter = expressions.into_iter();
  if let Some(TyntTree::Leaf(_, name)) = expression_iter.next() {
    let mut struct_string =
      "struct ".to_string() + &tynt_word_to_wgsl_word(name) + " {\n";
    while let Some(field) = expression_iter.next() {
      struct_string += "  ";
      struct_string += &compile_type_annotated_name(field)?;
      struct_string += ",\n";
    }
    struct_string += "}";
    Ok(struct_string)
  } else {
    Err(CompileError::InvalidStructName)
  }
}
