use sse::syntax::EncloserOrOperator;

use crate::parse::{Encloser, Operator, TyntTree};

use super::{
  error::CompileError, metadata::compile_metadata,
  types::compile_type_annotated_name, word::compile_word,
};

pub fn compile_function(forms: Vec<TyntTree>) -> Result<String, CompileError> {
  if forms.len() <= 2 {
    return Err(CompileError::InvalidFunctionBody);
  }
  let mut forms_iter = forms.into_iter();
  let mut fn_string = "fn ".to_string();
  fn_string += &compile_word(forms_iter.next().unwrap())?;
  fn_string += "(";
  if let Some(TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut arg_list_and_return_type,
  )) = forms_iter.next()
  {
    let return_type_form = arg_list_and_return_type.remove(1);
    let arg_list_form = arg_list_and_return_type.remove(0);
    if let TyntTree::Inner(
      (_, EncloserOrOperator::Encloser(Encloser::Square)),
      argument_forms,
    ) = arg_list_form
    {
      let arg_strings = argument_forms
        .into_iter()
        .map(compile_type_annotated_name)
        .collect::<Result<Vec<String>, CompileError>>()?;
      fn_string += &arg_strings.join(", ");
      fn_string += ") -> ";
      if let TyntTree::Inner(
        (_, EncloserOrOperator::Operator(Operator::Metadata)),
        mut metadata_and_output_type,
      ) = return_type_form
      {
        let output_type_form = metadata_and_output_type.remove(1);
        let metadata = metadata_and_output_type.remove(0);
        fn_string += &compile_metadata(metadata)?;
        fn_string += &compile_word(output_type_form)?;
      } else {
        fn_string += &compile_word(return_type_form)?;
      }
      fn_string += " {\n";
      fn_string += "}";
    } else {
      return Err(CompileError::InvalidFunctionArgumentList);
    }
  } else {
    return Err(CompileError::InvalidFunctionArgumentList);
  }
  Ok(fn_string)
}
