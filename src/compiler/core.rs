use sse::document::Document;

use crate::{
  compiler::program::Program,
  parse::{parse_easl, Context, Encloser, Operator},
};

use super::{builtins::built_in_macros, error::CompileResult};

pub fn compile_easl_document_to_wgsl(
  document: Document<'_, Context, Encloser, Operator>,
) -> CompileResult<String> {
  Program::from_easl_document(&document, built_in_macros())
    .map_err(|e| e.attach_error_source(&document))?
    .fully_infer_types()
    .map_err(|e| e.attach_error_source(&document))?
    .check_assignment_validity()
    .map_err(|e| e.attach_error_source(&document))?
    .monomorphize()
    .map_err(|e| e.attach_error_source(&document))?
    .compile_to_wgsl()
}

pub fn compile_easl_source_to_wgsl(easl_source: &str) -> CompileResult<String> {
  compile_easl_document_to_wgsl(parse_easl(easl_source)?)
}
