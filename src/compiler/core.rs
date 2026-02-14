use fsexp::ParseError;

use crate::{
  compiler::{
    info::ProgramInfo,
    program::{CompilerTarget, EaslDocument},
  },
  parse::parse_easl_without_comments,
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
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
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
  let document = parse_easl_without_comments(easl_source);
  if !document.parsing_failures.is_empty() {
    return Err(document);
  }
  Ok(compile_easl_document_to_wgsl(document))
}

pub fn get_easl_program_info(
  easl_source: &str,
) -> Result<Result<ProgramInfo, ErrorLog>, Vec<ParseError>> {
  let document = parse_easl_without_comments(easl_source);
  if !document.parsing_failures.is_empty() {
    return Err(document.parsing_failures);
  }
  let (mut program, _) =
    Program::from_easl_document(&document, built_in_macros());
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  Ok(if errors.is_empty() {
    Ok(ProgramInfo::from(&program))
  } else {
    Err(errors)
  })
}
