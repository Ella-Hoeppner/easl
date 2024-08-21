use sse::ParseError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
  Parse(ParseError),
  InvalidMetadataForm,
  ExpectedWord,
  UnrecognizedTopLevelForm,
  InvalidStructName,
  InvalidStructBody,
  InvalidStructField,
  InvalidName,
  InvalidTopLevelVar,
  InvalidTopLevelVarAttributes,
  ExpectedTypeAnnotatedName,
  InvalidFunctionBody,
  InvalidFunctionArgumentList,
  InvalidExpression,
  InvalidLetBlock,
  InvalidLetBindings,
}

impl From<ParseError> for CompileError {
  fn from(err: ParseError) -> Self {
    CompileError::Parse(err)
  }
}
