use crate::{compiler::program::Program, parse::parse_tynt};

use super::error::CompileError;

pub fn compile_tynt_to_wgsl(tynt_source: &str) -> Result<String, CompileError> {
  Program::init_from_tynt_trees(parse_tynt(tynt_source)?.syntax_trees)?
    .fully_infer_types()?
    .compile_to_wgsl()
}
