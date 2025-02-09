use sse::document::Document;

use crate::{
  compiler::program::Program,
  parse::{parse_easl, Context, Encloser, Operator},
};

use super::{builtins::built_in_macros, error::CompileResult};

pub fn compile_easl_document_to_wgsl(
  document: Document<'_, Context, Encloser, Operator>,
) -> CompileResult<String> {
  let mut program = Program::from_easl_document(&document, built_in_macros())?;
  program.process_raw_program()?;
  program.compile_to_wgsl()
}

pub fn compile_easl_source_to_wgsl(easl_source: &str) -> CompileResult<String> {
  compile_easl_document_to_wgsl(parse_easl(easl_source)?)
}
