use sse::{document::DocumentPosition, ParseError};
use std::{fmt::Display, rc::Rc};
use thiserror::Error;

use crate::parse::EaslTree;

use super::{
  expression::TypedExp,
  metadata::Metadata,
  types::{Type, TypeConstraint, TypeState},
};

use std::error::Error;

#[derive(Clone, Debug, PartialEq)]
pub enum SourceTraceKind {
  Empty,
  Singular(DocumentPosition),
  Combination(Vec<SourceTrace>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct SourceTrace {
  pub kind: Rc<SourceTraceKind>,
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

impl From<&DocumentPosition> for SourceTrace {
  fn from(position: &DocumentPosition) -> Self {
    position.clone().into()
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

#[derive(Clone, Debug, Error)]
pub enum CompileErrorKind {
  #[error("Parsing error: `{0}`")]
  ParsingFailed(ParseError),
  #[error("Unrecognized type name: `{0}`")]
  UnrecognizedTypeName(Rc<str>),
  #[error("Invalid metadata: `{0}`")]
  InvalidMetadata(Rc<str>),
  #[error("Expected a name with a type annotation")]
  ExpectedTypeAnnotatedName,
  #[error("Struct fields must be given explicit types")]
  StructFieldMissingType,
  #[error("Invalid struct name")]
  InvalidStructName,
  #[error("Couldn't find a struct named `{0}`")]
  NoStructNamed(Rc<str>),
  #[error("Invalid array signature")]
  InvalidArraySignature,
  #[error("Invalid struct definition")]
  InvalidStructDefinition,
  #[error("No generic called `{0}` in this scope")]
  UnrecognizedGeneric(Rc<str>),
  #[error("Invalid token `{0}`")]
  InvalidToken(Rc<str>),
  #[error("Invalid top-level var `{0}`")]
  InvalidTopLevelVar(Rc<str>),
  #[error("Invalid defn `{0}`")]
  InvalidDefn(Rc<str>),
  #[error("Invalid function")]
  InvalidFunction,
  #[error("Function missing body")]
  FunctionMissingBody,
  #[error("Unrecognized top-level form")]
  UnrecognizedTopLevelForm(EaslTree),
  #[error("Invalid application `()`")]
  EmptyList,
  #[error("Missing type annotation")]
  MissingType,
  #[error("Unrecognized top-level form")]
  InvalidType(EaslTree),
  #[error("Invalid Type Name `{0}`")]
  InvalidTypeName(Rc<str>),
  #[error("Function argument missing type annotation")]
  FunctionArgMissingType,
  #[error("Invalid argument name")]
  InvalidArgumentName,
  #[error("Couldn't infer types")]
  CouldntInferTypes(TypedExp),
  #[error("Incompatible types: expected {0}, found {1}")]
  IncompatibleTypes(TypeState, TypeState),
  #[error("Function argument types incompatible: expected {0:?}, found {1:?}")]
  FunctionArgumentTypesIncompatible(TypeState, Vec<TypeState>),
  #[error("Function expression has non-function type: {0}")]
  FunctionExpressionHasNonFunctionType(Type),
  #[error("Unbound name: `{0}`")]
  UnboundName(Rc<str>),
  #[error("Applied non-function in function position")]
  AppliedNonFunction,
  #[error("Wrong number of arguments for function{}", .0.as_ref().map_or(String::new(), |name| format!(": `{}`", name)))]
  WrongArity(Option<Rc<str>>),
  #[error("Expected leaf node")]
  ExpectedLeaf,
  #[error("Invalid function argument list")]
  InvalidFunctionArgumentList,
  #[error("Invalid function signature")]
  InvalidFunctionSignature,
  #[error("Function signature missing argument list")]
  FunctionSignatureMissingArgumentList,
  #[error("Function signature not enclosed in square brackets")]
  FunctionSignatureNotSquareBrackets,
  #[error("Function signature missing return type")]
  FunctionSignatureMissingReturnType,
  #[error("No field named `{field_name}` in struct `{struct_name}`")]
  NoSuchField {
    struct_name: Rc<str>,
    field_name: Rc<str>,
  },
  #[error("Accessor used on non-struct type")]
  AccessorOnNonStruct,
  #[error("Accessor had multiple arguments")]
  AccessorHadMultipleArguments,
  #[error("Not enough children in let block")]
  NotEnoughLetBlockChildren,
  #[error("Let bindings not enclosed in square brackets")]
  LetBindingsNotSquareBracketed,
  #[error("Odd number of children in let bindings")]
  OddNumberOfChildrenInLetBindings,
  #[error("Expected binding name")]
  ExpectedBindingName,
  #[error("Empty block")]
  EmptyBlock,
  #[error("Top-level constants may not have metadata")]
  ConstantMayNotHaveMetadata,
  #[error("Invalid top-level variable metadata: {0}")]
  InvalidVariableMetadata(Metadata),
  #[error("Invalid top-level function metadata: {0}")]
  InvalidFunctionMetadata(Metadata),
  #[error("Invalid assignment target")]
  InvalidAssignmentTarget,
  #[error("Assignment target must be a variable: `{0}`")]
  AssignmentTargetMustBeVariable(Rc<str>),
  #[error("Match expression missing scrutinee")]
  MatchMissingScrutinee,
  #[error("Match expression missing arms")]
  MatchMissingArms,
  #[error("Incomplete match arm")]
  MatchIncompleteArm,
  #[error("Invalid struct field type")]
  InvalidStructFieldType,
  #[error("Invalid type bound")]
  InvalidTypeBound,
  #[error("Unsatisfied type constraint: {0}")]
  UnsatisfiedTypeConstraint(TypeConstraint),
  #[error("Invalid for loop header")]
  InvalidForLoopHeader,
  #[error("Invalid while loop")]
  InvalidWhileLoop,
  #[error("Return statement outside of function")]
  ReturnOutsideFunction,
  #[error("Enclosing function type wasn't a function type")]
  EnclosingFunctionTypeWasntFunction,
  #[error("Invalid return statement")]
  InvalidReturn,
  #[error("Macro error: {0}")]
  MacroError(Rc<str>),
  #[error("Invalid array access syntax")]
  InvalidArrayAccessSyntax,
  #[error("Array access on non-array type")]
  ArrayAccessOnNonArray,
  #[error("Applications must use names")]
  ApplicationsMustUseNames,
  #[error("Anonymous functions are not yet supported")]
  AnonymousFunctionsNotYetSupported,
  #[error("Anonymous structs are not yet supported")]
  AnonymousStructsNotYetSupported,
  #[error("Encountered comment in source")]
  EncounteredCommentInSource,
  #[error("Encountered metadata in internal expression")]
  EncounteredMetadataInInternalExpression,
  #[error("Invalid function type")]
  InvalidFunctionType,
  #[error("Cannot inline function without abstract ancestor")]
  CantInlineFunctionWithoutAbstractAncestor,
  #[error("No argument names for function")]
  NoArgNamesForFunction,
  #[error("zeroed-array shouldn't have any children")]
  ZeroedArrayShouldntHaveChildren,
  #[error("expected function, found non-function value")]
  ExpectedFunctionFoundNonFunction,
}

#[derive(Clone, Debug)]
pub struct CompileError {
  pub kind: CompileErrorKind,
  //_backtrace: Rc<std::backtrace::Backtrace>,
  pub source_trace: SourceTrace,
}
impl CompileError {
  pub fn new(kind: CompileErrorKind, source_trace: SourceTrace) -> Self {
    Self {
      kind,
      //_backtrace: Rc::new(std::backtrace::Backtrace::capture()),
      source_trace,
    }
  }
}

impl Display for CompileError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.kind.fmt(f)
  }
}

impl Error for CompileError {}

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
