use crate::{compiler::program::Program, parse::parse_tynt};

use super::{builtins::built_in_macros, error::CompileResult};

pub fn compile_tynt_to_wgsl(tynt_source: &str) -> CompileResult<String> {
  let document = parse_tynt(tynt_source)?;
  Program::from_tynt_document(&document, built_in_macros())
    .map_err(|e| e.attach_error_source(&document))?
    .fully_infer_types()
    .map_err(|e| e.attach_error_source(&document))?
    .check_assignment_validity()
    .map_err(|e| e.attach_error_source(&document))?
    .monomorphize()
    .map_err(|e| e.attach_error_source(&document))?
    .compile_to_wgsl()
}
