use crate::parse::parse_tynt;
use sse::ParseError;

fn compile_tynt_to_wgsl(tynt_source: &str) -> Result<String, ParseError> {
  let document = parse_tynt(tynt_source)?;
  todo!()
}
