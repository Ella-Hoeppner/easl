use sse::document::Document;

use crate::parse::{parse_easl, Context as ParseContext, Encloser, Operator};

use super::{builtins::built_in_macros, error::CompileError, program::Program};

pub fn compile_easl_document_to_wgsl(
  document: Document<'_, ParseContext, Encloser, Operator>,
) -> Result<String, Vec<CompileError>> {
  let (mut program, errors) =
    Program::from_easl_document(&document, built_in_macros());
  if !errors.is_empty() {
    return Err(errors);
  }
  let errors = program.validate_raw_program();
  if !errors.is_empty() {
    return Err(errors);
  }
  program.compile_to_wgsl().map_err(|e| vec![e])
}

pub fn compile_easl_source_to_wgsl(
  easl_source: &str,
) -> Result<String, Vec<CompileError>> {
  let mut doc = parse_easl(easl_source).map_err(|e| vec![e])?;
  doc.strip_comments();
  compile_easl_document_to_wgsl(doc)
}
