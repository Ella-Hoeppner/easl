use sse::ParseError;
use std::{backtrace::Backtrace, rc::Rc};

use crate::parse::TyntTree;

use super::{
  expression::TypedExp,
  metadata::Metadata,
  types::{Type, TypeConstraint, TypeState},
};

#[derive(Clone, Debug, PartialEq)]
pub enum SourceTraceKind {
  Empty,
  Singular(Vec<usize>),
  Combination(Vec<SourceTrace>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct SourceTrace {
  kind: Rc<SourceTraceKind>,
}

impl SourceTrace {
  pub fn empty() -> Self {
    Self {
      kind: Rc::new(SourceTraceKind::Empty),
    }
  }
  pub fn combine_with(self, other: Self) -> Self {
    Self {
      kind: Rc::new(SourceTraceKind::Combination(vec![self, other])),
    }
  }
}

impl From<Vec<usize>> for SourceTrace {
  fn from(value: Vec<usize>) -> Self {
    Self {
      kind: Rc::new(SourceTraceKind::Singular(value)),
    }
  }
}

impl FromIterator<SourceTrace> for SourceTrace {
  fn from_iter<T: IntoIterator<Item = SourceTrace>>(iter: T) -> Self {
    let mut subtraces: Vec<SourceTrace> = iter.into_iter().collect();
    match subtraces.len() {
      0 => SourceTrace::empty(),
      1 => subtraces.remove(0),
      _ => SourceTrace {
        kind: Rc::new(SourceTraceKind::Combination(subtraces)),
      },
    }
  }
}

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
  UnsatisfiedTypeConstraint(TypeConstraint),
  InvalidForLoopHeader,
  InvalidWhileLoop,
  ReturnOutsideFunction,
  EnclosingFunctionTypeWasntFunction,
  InvalidReturn,
}

#[derive(Clone, Debug)]
pub struct CompileError {
  pub kind: CompileErrorKind,
  pub context: Vec<String>,
  _backtrace: Rc<Backtrace>,
  pub source_trace: SourceTrace,
}
impl CompileError {
  pub fn new(kind: CompileErrorKind, source_trace: SourceTrace) -> Self {
    Self {
      kind,
      context: vec![],
      _backtrace: Rc::new(Backtrace::capture()),
      source_trace,
    }
  }
}

pub type CompileResult<T> = Result<T, CompileError>;

impl From<ParseError> for CompileError {
  fn from(err: ParseError) -> Self {
    Self::new(CompileErrorKind::ParsingFailed(err), SourceTrace::empty())
  }
}

pub fn err<T>(
  kind: CompileErrorKind,
  source_trace: SourceTrace,
) -> CompileResult<T> {
  Err(CompileError::new(kind, source_trace))
}
