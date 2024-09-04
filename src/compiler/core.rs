use crate::parse::parse_tynt;

use super::error::CompileError;

pub fn compile_tynt(tynt_source: &str) -> Result<String, CompileError> {
  let document = parse_tynt(tynt_source)?;
  todo!()
}
