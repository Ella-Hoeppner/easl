use fsexp::ParseError;

use crate::{
  compiler::{info::ProgramInfo, program::EaslDocument},
  parse::parse_easl,
};

use super::{builtins::built_in_macros, error::ErrorLog, program::Program};

pub fn compile_easl_document_to_wgsl<'t>(
  document: EaslDocument<'t>,
) -> Result<String, (EaslDocument<'t>, ErrorLog)> {
  let (mut program, errors) =
    Program::from_easl_document(&document, built_in_macros());
  if !errors.is_empty() {
    return Err((document, errors));
  }
  let errors = program.validate_raw_program();
  if !errors.is_empty() {
    return Err((document, errors));
  }

  program
    .compile_to_wgsl()
    .map_err(|e| (document, ErrorLog::from(e)))
}

pub fn compile_easl_source_to_wgsl<'t>(
  easl_source: &'t str,
) -> Result<Result<String, (EaslDocument<'t>, ErrorLog)>, EaslDocument<'t>> {
  let mut document = parse_easl(easl_source);
  if !document.parsing_failures.is_empty() {
    return Err(document);
  }
  document.strip_comments();
  Ok(compile_easl_document_to_wgsl(document))
}

pub fn get_easl_program_info(
  easl_source: &str,
) -> Result<Result<ProgramInfo, ErrorLog>, Vec<ParseError>> {
  let mut document = parse_easl(easl_source);
  if !document.parsing_failures.is_empty() {
    return Err(document.parsing_failures);
  }
  document.strip_comments();
  let (mut program, _) =
    Program::from_easl_document(&document, built_in_macros());
  let errors = program.validate_raw_program();
  Ok(if errors.is_empty() {
    Ok(ProgramInfo::from(&program))
  } else {
    Err(errors)
  })
}
