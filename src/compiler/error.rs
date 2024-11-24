use sse::{document::DocumentPosition, ParseError};
use std::{backtrace::Backtrace, rc::Rc};

use crate::parse::TyntTree;

use super::{
  expression::TypedExp,
  metadata::Metadata,
  program::TyntDocument,
  types::{Type, TypeConstraint, TypeState},
};

#[derive(Clone, Debug, PartialEq)]
pub enum SourceTraceKind {
  Empty,
  Singular(DocumentPosition),
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
  pub fn all_document_positions(&self) -> Vec<&DocumentPosition> {
    match &*self.kind {
      SourceTraceKind::Empty => vec![],
      SourceTraceKind::Singular(pos) => vec![&pos],
      SourceTraceKind::Combination(subtraces) => subtraces
        .iter()
        .map(|subtrace| subtrace.all_document_positions())
        .flatten()
        .collect(),
    }
  }
}

impl From<DocumentPosition> for SourceTrace {
  fn from(position: DocumentPosition) -> Self {
    Self {
      kind: Rc::new(SourceTraceKind::Singular(position)),
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
  NoStructNamed(String),
  UnknownStructName,
  InvalidArraySignature,
  InvalidStructDefinition,
  UnrecognizedGeneric(String),
  InvalidToken(String),
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
  WrongArity(Option<String>),
  ExpectedLeaf,
  InvalidFunctionArgumentList,
  InvalidFunctionSignature,
  FunctionSignatureMissingArgumentList,
  FunctionSignatureNotSquareBrackets,
  FunctionSignatureMissingReturnType,
  NoSuchField {
    struct_name: String,
    field_name: String,
  },
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
  MacroError(String),
  InvalidArrayAccessSyntax,
}

#[derive(Clone, Debug)]
pub struct CompileError {
  pub kind: CompileErrorKind,
  pub context: Vec<String>,
  _backtrace: Rc<Backtrace>,
  pub source_trace: SourceTrace,
  pub source: Option<Vec<String>>,
}
impl CompileError {
  pub fn new(kind: CompileErrorKind, source_trace: SourceTrace) -> Self {
    Self {
      kind,
      context: vec![],
      _backtrace: Rc::new(Backtrace::capture()),
      source_trace,
      source: None,
    }
  }
  pub fn attach_error_source(mut self, document: &TyntDocument<'_>) -> Self {
    self.source = Some(
      self
        .source_trace
        .all_document_positions()
        .into_iter()
        .map(|position| document.text[position.span.clone()].to_string())
        .collect(),
    );
    self
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
