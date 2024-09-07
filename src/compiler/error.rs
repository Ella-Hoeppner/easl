use sse::ParseError;

#[derive(Clone, Debug)]
pub enum CompileError {
  Parse(ParseError),
  UnrecognizedTypeName,
  InvalidMetadata(String),
  ExpectedTypeAnnotatedName,
  InvalidStructField,
  InvalidStructName,
  InvalidTopLevelVar(String),
  InvalidDef(String),
  InvalidFunction(String),
  UnrecognizedTopLevelForm,
  EmptyList,
  InvalidType,
  FunctionArgMissingType,
  InvalidArgumentName,
  CouldntInferTypes,
  IncompatibleTypes,
  UnboundName,
  AppliedNonFunction,
  WrongArity,
}

impl From<ParseError> for CompileError {
  fn from(err: ParseError) -> Self {
    Self::Parse(err)
  }
}
