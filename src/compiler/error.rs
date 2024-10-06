use sse::ParseError;
use std::{backtrace::Backtrace, rc::Rc};

use super::{
  metadata::Metadata,
  types::{Type, TypeState},
};

#[derive(Clone, Debug)]
pub enum CompileErrorKind {
  ParsingFailed(ParseError),
  UnrecognizedTypeName(String),
  InvalidMetadata(String),
  ExpectedTypeAnnotatedName,
  StructFieldMissingType,
  InvalidStructName,
  UnknownStructName,
  InvalidStructDefinition,
  InvalidTopLevelVar(String),
  InvalidDef(String),
  InvalidFunction,
  UnrecognizedTopLevelForm,
  EmptyList,
  InvalidType,
  FunctionArgMissingType,
  InvalidArgumentName,
  CouldntInferTypes,
  IncompatibleTypes(TypeState, TypeState),
  FunctionArgumentTypesIncompatible(TypeState, Vec<TypeState>),
  FunctionExpressionHasNonFunctionType(Type),
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
  EmptyBlock,
  InvalidVariableMetadata(Metadata),
  InvalidAssignmentTarget,
  AssignmentTargetMustBeVariable,
}

#[derive(Clone, Debug)]
pub struct CompileError {
  kind: CompileErrorKind,
  context: Vec<String>,
  backtrace: Rc<Backtrace>,
}
impl CompileError {
  pub fn new(kind: CompileErrorKind) -> Self {
    Self {
      kind,
      context: vec![],
      backtrace: Rc::new(Backtrace::capture()),
    }
  }
}

impl From<CompileErrorKind> for CompileError {
  fn from(kind: CompileErrorKind) -> Self {
    Self::new(kind)
  }
}

pub type CompileResult<T> = Result<T, CompileError>;

pub trait WithContext {
  fn with_context(self, context: String) -> Self;
}

impl WithContext for CompileError {
  fn with_context(mut self, context: String) -> Self {
    self.context.push(context);
    self
  }
}

impl<T> WithContext for CompileResult<T> {
  fn with_context(self, context: String) -> Self {
    self.map_err(|err| err.with_context(context))
  }
}

impl From<ParseError> for CompileError {
  fn from(err: ParseError) -> Self {
    CompileErrorKind::ParsingFailed(err).into()
  }
}

pub fn err<T, E: Into<CompileError>>(e: E) -> CompileResult<T> {
  Err(e.into())
}
