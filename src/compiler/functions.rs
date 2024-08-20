use sse::syntax::EncloserOrOperator;

use crate::parse::{Encloser, Operator, TyntTree};

use super::{
  error::CompileError, metadata::compile_metadata,
  types::compile_type_annotated_name, word::compile_word,
};

pub fn compile_function(
  expressions: Vec<TyntTree>,
) -> Result<String, CompileError> {
  if expressions.len() <= 2 {
    return Err(CompileError::InvalidFunctionBody);
  }
  let mut expressions_iter = expressions.into_iter();
  let mut fn_string = "fn ".to_string();
  fn_string += &compile_word(expressions_iter.next().unwrap())?;
  fn_string += "(";
  if let Some(TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut arg_list_and_return_type,
  )) = expressions_iter.next()
  {
    let return_type_expression = arg_list_and_return_type.remove(1);
    let arg_list_expression = arg_list_and_return_type.remove(0);
    if let TyntTree::Inner(
      (_, EncloserOrOperator::Encloser(Encloser::Square)),
      argument_expressions,
    ) = arg_list_expression
    {
      let arg_strings = argument_expressions
        .into_iter()
        .map(compile_type_annotated_name)
        .collect::<Result<Vec<String>, CompileError>>()?;
      fn_string += &arg_strings.join(", ");
      fn_string += ") -> ";
      if let TyntTree::Inner(
        (_, EncloserOrOperator::Operator(Operator::Metadata)),
        mut metadata_and_output_type,
      ) = return_type_expression
      {
        let output_type_expression = metadata_and_output_type.remove(1);
        let metadata = metadata_and_output_type.remove(0);
        fn_string += &compile_metadata(metadata)?;
        fn_string += &compile_word(output_type_expression)?;
      } else {
        fn_string += &compile_word(return_type_expression)?;
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
