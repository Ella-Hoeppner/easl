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
  UnboundName(String),
  AppliedNonFunction,
  WrongArity,
  ExpectedLeaf,
  InvalidFunctionArgumentList,
  InvalidFunctionSignature,
  FunctionSignatureMissingArgumentList,
  FunctionSignatureNotSquareBrackets,
  FunctionSignatureMissingReturnType,
  NoSuchField,
  AccessorHadMultipleArguments,
  NotEnoughLetBlockChildren,
  LetBindingsNotSquareBracketed,
  OddNumberOfChildrenInLetBindings,
  ExpectedBindingName,
}

impl From<ParseError> for CompileError {
  fn from(err: ParseError) -> Self {
    Self::Parse(err)
  }
}
