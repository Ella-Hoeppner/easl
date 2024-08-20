use sse::syntax::EncloserOrOperator;

use crate::parse::{Encloser, TyntTree};

use super::{
  error::CompileError, types::compile_type_annotated_name, word::compile_word,
};

pub fn compile_top_level_var(
  mut forms: Vec<TyntTree>,
) -> Result<String, CompileError> {
  match forms.len() {
    1 => compile_type_annotated_name(forms.remove(0))
      .map(|s| "var ".to_string() + &s + ";"),
    2 => {
      let name = compile_type_annotated_name(forms.remove(1))?;
      if let TyntTree::Inner(
        (_, EncloserOrOperator::Encloser(Encloser::Square)),
        attributes,
      ) = forms.remove(0)
      {
        if attributes.is_empty() {
          return Err(CompileError::InvalidTopLevelVarAttributes);
        }
        let attribute_strings: Vec<String> = attributes
          .into_iter()
          .map(|attribute| compile_word(attribute))
          .collect::<Result<Vec<String>, CompileError>>()?;
        Ok(
          "var<".to_string()
            + &attribute_strings
              .into_iter()
              .reduce(|a, b| a + ", " + &b)
              .unwrap()
            + "> "
            + &name
            + ";",
        )
      } else {
        Err(CompileError::InvalidTopLevelVarAttributes)
      }
    }
    _ => Err(CompileError::InvalidTopLevelVar),
  }
}
