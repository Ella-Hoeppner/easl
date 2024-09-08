use sse::ParseError;

#[derive(Clone, Debug)]
pub enum CompileError {
  Parse(ParseError),
  UnrecognizedTypeName(String),
  InvalidMetadata(String),
  ExpectedTypeAnnotatedName,
  StructFieldMissingType,
  InvalidStructName,
  InvalidTopLevelVar(String),
  InvalidDef(String),
  InvalidFunction,
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
  ExpectedLeaf,
  InvalidFunctionArgumentList,
  InvalidFunctionSignature,
  FunctionSignatureMissingArgumentList,
  FunctionSignatureNotSquareBrackets,
  FunctionSignatureMissingReturnType,
}

impl From<ParseError> for CompileError {
  fn from(err: ParseError) -> Self {
    Self::Parse(err)
  }
}
