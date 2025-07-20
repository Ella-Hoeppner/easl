use sse::document::Document;

use crate::{
  compiler::{expression::ExpKind, functions::FunctionImplementationKind},
  parse::{parse_easl, Context as ParseContext, Encloser, Operator},
};

use super::{builtins::built_in_macros, error::ErrorLog, program::Program};

pub fn compile_easl_document_to_wgsl(
  document: Document<'_, ParseContext, Encloser, Operator>,
) -> Result<String, ErrorLog> {
  let (mut program, errors) =
    Program::from_easl_document(&document, built_in_macros());
  if !errors.is_empty() {
    return Err(errors);
  }
  let errors = program.validate_raw_program();
  if !errors.is_empty() {
    return Err(errors);
  }

  program.compile_to_wgsl().map_err(ErrorLog::from)
}

pub fn compile_easl_source_to_wgsl(
  easl_source: &str,
) -> Result<String, ErrorLog> {
  let mut doc = parse_easl(easl_source).map_err(ErrorLog::from)?;
  doc.strip_comments();
  compile_easl_document_to_wgsl(doc)
}
