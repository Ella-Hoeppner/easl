use sse::ParseError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
  Parse(ParseError),
  InvalidMetadataExpression,
  ExpectedWord,
  UnrecognizedTopLevelExpression,
  InvalidStructName,
  InvalidStructBody,
  InvalidStructField,
  InvalidName,
  InvalidTopLevelVar,
  InvalidTopLevelVarAttributes,
  ExpectedTypeAnnotatedName,
  InvalidFunctionBody,
  InvalidFunctionArgumentList,
}

impl From<ParseError> for CompileError {
  fn from(err: ParseError) -> Self {
    CompileError::Parse(err)
  }
}
