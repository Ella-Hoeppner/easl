use crate::{compiler::program::Program, parse::parse_tynt};

use super::{builtins::built_in_macros, error::CompileResult};

pub fn compile_tynt_to_wgsl(tynt_source: &str) -> CompileResult<String> {
  Program::init_from_tynt_trees(
    parse_tynt(tynt_source)?.syntax_trees,
    built_in_macros(),
  )?
  .fully_infer_types()?
  .check_assignment_validity()?
  .monomorphize()?
  .compile_to_wgsl()
}
