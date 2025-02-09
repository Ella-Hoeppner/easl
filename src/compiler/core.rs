use sse::document::Document;

use crate::{
  compiler::program::Program,
  parse::{parse_easl, Context, Encloser, Operator},
};

use super::{builtins::built_in_macros, error::CompileError};

pub fn compile_easl_document_to_wgsl(
  document: Document<'_, Context, Encloser, Operator>,
) -> Result<String, Vec<CompileError>> {
  let (mut program, errors) =
    Program::from_easl_document(&document, built_in_macros());
  if !errors.is_empty() {
    return Err(errors);
  }
  program.validate_raw_program().map_err(|e| vec![e])?;
  program.compile_to_wgsl().map_err(|e| vec![e])
}

pub fn compile_easl_source_to_wgsl(
  easl_source: &str,
) -> Result<String, Vec<CompileError>> {
  compile_easl_document_to_wgsl(parse_easl(easl_source).map_err(|e| vec![e])?)
}
