use sse::document::Document;

use crate::{
  compiler::info::ProgramInfo,
  parse::{Context as ParseContext, Encloser, Operator, parse_easl},
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
  let mut document = parse_easl(easl_source).map_err(ErrorLog::from)?;
  document.strip_comments();
  compile_easl_document_to_wgsl(document)
}

pub fn get_easl_program_info(
  easl_source: &str,
) -> Result<ProgramInfo, ErrorLog> {
  let mut document = parse_easl(easl_source)?;
  document.strip_comments();
  let (mut program, _) =
    Program::from_easl_document(&document, built_in_macros());
  let errors = program.validate_raw_program();
  if errors.is_empty() {
    Ok(ProgramInfo::from(&program))
  } else {
    Err(errors)
  }
}
