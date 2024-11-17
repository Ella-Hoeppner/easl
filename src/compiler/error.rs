use sse::ParseError;
use std::{backtrace::Backtrace, rc::Rc};

use crate::parse::TyntTree;

use super::{
  expression::TypedExp,
  metadata::Metadata,
  types::{Type, TypeConstraint, TypeState},
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
  UnrecognizedGeneric(String),
  InvalidTopLevelVar(String),
  InvalidDef(String),
  InvalidDefn(String),
  InvalidFunction,
  FunctionMissingBody,
  UnrecognizedTopLevelForm(TyntTree),
  EmptyList,
  MissingType,
  InvalidType(TyntTree),
  InvalidTypeName(String),
  FunctionArgMissingType,
  InvalidArgumentName,
  CouldntInferTypes(Vec<TypedExp>),
  IncompatibleTypes(TypeState, TypeState),
  FunctionArgumentTypesIncompatible(TypeState, Vec<TypeState>),
  FunctionExpressionHasNonFunctionType(Type),
  UnboundName(String),
  AppliedNonFunction,
  WrongArity,
  ExpectedLeaf,
  InvalidFunctionArgumentName,
  InvalidFunctionArgumentList,
  InvalidFunctionSignature,
  FunctionSignatureMissingArgumentList,
  FunctionSignatureNotSquareBrackets,
  FunctionSignatureMissingReturnType,
  NoSuchField,
  AccessorOnNonStruct,
  AccessorHadMultipleArguments,
  NotEnoughLetBlockChildren,
  LetBindingsNotSquareBracketed,
  OddNumberOfChildrenInLetBindings,
  ExpectedBindingName,
  EmptyBlock,
  InvalidVariableMetadata(Metadata),
  InvalidAssignmentTarget,
  AssignmentTargetMustBeVariable,
  MatchMissingScrutinee,
  MatchMissingArms,
  MatchIncompleteArm,
  InvalidStructFieldType,
  InvalidTypeBound,
  UnsatisfiedTypeBound(TypeConstraint),
}

#[derive(Clone, Debug)]
pub struct CompileError {
  pub kind: CompileErrorKind,
  pub context: Vec<String>,
  _backtrace: Rc<Backtrace>,
  pub source_paths: Vec<Vec<usize>>,
}
impl CompileError {
  pub fn new(kind: CompileErrorKind, source_paths: Vec<Vec<usize>>) -> Self {
    Self {
      kind,
      context: vec![],
      _backtrace: Rc::new(Backtrace::capture()),
      source_paths,
    }
  }
}

pub type CompileResult<T> = Result<T, CompileError>;

impl From<ParseError> for CompileError {
  fn from(err: ParseError) -> Self {
    Self::new(CompileErrorKind::ParsingFailed(err), vec![])
  }
}

pub fn err<T>(
  kind: CompileErrorKind,
  source_paths: Vec<Vec<usize>>,
) -> CompileResult<T> {
  Err(CompileError::new(kind, source_paths))
}
