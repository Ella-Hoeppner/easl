use sse::syntax::EncloserOrOperator;

use crate::parse::{Operator, TyntTree};

use super::{
  error::CompileError, metadata::compile_form_possibly_with_metadata,
  word::compile_word,
};

pub fn compile_type(type_form: TyntTree) -> Result<String, CompileError> {
  compile_word(type_form)
}

pub fn compile_type_annotated_form(
  form: TyntTree,
  subform_compiler: impl Fn(TyntTree) -> Result<String, CompileError>,
) -> Result<String, CompileError> {
  if let TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut children,
  ) = form
  {
    let type_form = children.remove(1);
    let form = children.remove(0);
    Ok(
      subform_compiler(form)?
        + ": "
        + &compile_type(type_form)?,
    )
  } else {
    Err(CompileError::ExpectedTypeAnnotatedName)
  }
}

pub fn compile_type_annotated_name(
  form: TyntTree,
) -> Result<String, CompileError> {
  compile_type_annotated_form(form, |subform| {
    compile_form_possibly_with_metadata(subform, compile_word)
  })
}
