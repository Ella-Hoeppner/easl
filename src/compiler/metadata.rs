use sse::syntax::EncloserOrOperator;

use crate::parse::{Encloser, Operator, TyntTree};

use super::{
  error::CompileError,
  word::{compile_word, tynt_word_to_wgsl_word},
};

pub fn compile_metadata(form: TyntTree) -> Result<String, CompileError> {
  match form {
    TyntTree::Leaf(_, label) => {
      Ok("@".to_string() + &tynt_word_to_wgsl_word(label) + "\n")
    }
    TyntTree::Inner((_, encloser_or_operator), children) => {
      match encloser_or_operator {
        EncloserOrOperator::Encloser(Encloser::Curly) => {
          if children.len() % 2 == 0 {
            Ok(
              children
                .chunks(2)
                .map(|x| -> Result<String, CompileError> {
                  let property = x[0].clone();
                  let value = x[1].clone();
                  let property_string = compile_word(property)?;
                  let value_string = compile_word(value)?;
                  Ok(
                    "@".to_string()
                      + &property_string
                      + "("
                      + &value_string
                      + ") ",
                  )
                })
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .reduce(|a, b| a + &b)
                .unwrap(),
            )
          } else {
            Err(CompileError::InvalidMetadataForm)
          }
        }
        _ => Err(CompileError::InvalidMetadataForm),
      }
    }
  }
}

pub fn compile_form_possibly_with_metadata(
  form: TyntTree,
  subform_compiler: impl Fn(TyntTree) -> Result<String, CompileError>,
) -> Result<String, CompileError> {
  if let TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::Metadata)),
    mut children,
  ) = form
  {
    let form = children.remove(1);
    let metadata_form = children.remove(0);
    let form_string =
      compile_form_possibly_with_metadata(form, subform_compiler)?;
    Ok(compile_metadata(metadata_form)? + &form_string)
  } else {
    subform_compiler(form)
  }
}
