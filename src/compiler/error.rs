use sse::{ParseError, document::DocumentPosition};
use std::{collections::HashSet, fmt::Display, hash::Hash};
use thiserror::Error;

use crate::parse::EaslTree;

use super::{
  metadata::Metadata,
  types::{TypeConstraintDescription, TypeDescription, TypeStateDescription},
};

use std::error::Error;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceTrace {
  primary_position: Option<DocumentPosition>,
  secondary_positions: Vec<DocumentPosition>,
}

impl SourceTrace {
  pub fn empty() -> Self {
    Self {
      primary_position: None,
      secondary_positions: vec![],
    }
  }
  pub fn insert_as_secondary(mut self, other: Self) -> Self {
    for pos in other.into_document_positions_iter() {
      self.secondary_positions.push(pos);
    }
    self
  }
  pub fn document_positions_iter(
    &self,
  ) -> impl Iterator<Item = &DocumentPosition> {
    self
      .primary_position
      .iter()
      .chain(self.secondary_positions.iter())
  }
  pub fn into_document_positions_iter(
    self,
  ) -> impl Iterator<Item = DocumentPosition> {
    self
      .primary_position
      .into_iter()
      .chain(self.secondary_positions.into_iter())
  }
}

impl From<DocumentPosition> for SourceTrace {
  fn from(position: DocumentPosition) -> Self {
    Self {
      primary_position: position.into(),
      secondary_positions: vec![],
    }
  }
}

impl From<&DocumentPosition> for SourceTrace {
  fn from(position: &DocumentPosition) -> Self {
    position.clone().into()
  }
}

/*impl FromIterator<SourceTrace> for SourceTrace {
  fn from_iter<T: IntoIterator<Item = SourceTrace>>(iter: T) -> Self {
    let mut iter = iter.into_iter();
    if let Some(first) = iter.next() {
      iter.fold(first, |trace, subtrace| trace.combine_with(subtrace))
    } else {
      Self::empty()
    }
  }
}*/

#[derive(Clone, Debug, Error, PartialEq, Eq, Hash)]
pub enum CompileErrorKind {
  #[error("Parsing error: `{0}`")]
  ParsingFailed(ParseError),
  #[error("Unrecognized type name: `{0}`")]
  UnrecognizedTypeName(String),
  #[error("Invalid metadata: `{0}`")]
  InvalidMetadata(String),
  #[error("Expected a name with a type annotation")]
  ExpectedTypeAnnotatedName,
  #[error("Struct fields must be given explicit types")]
  StructFieldMissingType,
  #[error("Invalid struct name")]
  InvalidStructName,
  #[error("Couldn't find a struct named `{0}`")]
  NoStructNamed(String),
  #[error("Invalid array signature")]
  InvalidArraySignature,
  #[error("Array lookup requires 1 argument as an index, got {0} arguments")]
  ArrayLookupInvalidArity(usize),
  #[error("Invalid struct definition")]
  InvalidStructDefinition,
  #[error("No generic called `{0}` in this scope")]
  UnrecognizedGeneric(String),
  #[error("Invalid token `{0}`")]
  InvalidToken(String),
  #[error("Invalid top-level var `{0}`")]
  InvalidTopLevelVar(String),
  #[error("Invalid defn `{0}`")]
  InvalidDefn(String),
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
  InvalidTypeName(String),
  #[error("Function argument missing type annotation")]
  FunctionArgMissingType,
  #[error("Invalid argument name")]
  InvalidArgumentName,
  #[error("Couldn't infer types")]
  CouldntInferTypes,
  #[error("Incompatible types: expected {0}, found {1}")]
  IncompatibleTypes(TypeStateDescription, TypeStateDescription),
  #[error("Function argument types incompatible. Function had type {}\n Recieved arguments of types:\n{}", .f, .args.iter().map(|t| format!("{t}")).collect::<Vec<String>>().join("\n"))]
  FunctionArgumentTypesIncompatible {
    f: TypeStateDescription,
    args: Vec<TypeStateDescription>,
  },
  #[error("Duplicate function signature \"`{0}`\"")]
  DuplicateFunctionSignature(String),
  #[error("Function signature \"`{0}`\" conflicts with built-in function")]
  FunctionSignatureConflictsWithBuiltin(String),
  #[error("Function expression has non-function type: {0}")]
  FunctionExpressionHasNonFunctionType(TypeDescription),
  #[error("Unbound name: `{0}`")]
  UnboundName(String),
  #[error("Applied non-function in function position")]
  AppliedNonFunction,
  #[error("Wrong number of arguments for function{}", .0.as_ref().map_or(String::new(), |name| format!(": `{}`", name)))]
  WrongArity(Option<String>),
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
    struct_name: String,
    field_name: String,
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
  #[error("Invalid top-level variable metadata: {0:?}")]
  InvalidVariableMetadata(MetadataDescription),
  #[error("Invalid top-level function metadata: {0:?}")]
  InvalidFunctionMetadata(MetadataDescription),
  #[error("Invalid assignment target")]
  InvalidAssignmentTarget,
  #[error("Assignment target must be a variable: `{0}`")]
  AssignmentTargetMustBeVariable(String),
  #[error("Match expression missing scrutinee")]
  MatchMissingScrutinee,
  #[error("Match expression missing arms")]
  MatchMissingArms,
  #[error("Incomplete match arm")]
  MatchIncompleteArm,
  #[error("Invalid struct field type")]
  InvalidStructFieldType,
  #[error("Invalid type bound")]
  InvalidTypeConstraint,
  #[error("Unsatisfied type constraint: {0}")]
  UnsatisfiedTypeConstraint(TypeConstraintDescription),
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
  MacroError(String),
  #[error("Invalid array access syntax")]
  ArrayLiteralMistyped,
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
  #[error("Can't shadow top-level binding \"{0}\"")]
  CantShadowTopLevelBinding(String),
  #[error(
    "Invalid signature for @associative function, signature must conform to (Fn [T T]: T)"
  )]
  InvalidAssociativeSignature,
  #[error("Can't bind a name to a typeless expression")]
  TypelessBinding,
  #[error("`discard` can only occur in @fragment functions")]
  DiscardOutsideFragment,
  #[error("`continue` can only occur inside a loop")]
  ContinueOutsideLoop,
  #[error("`break` can only occur inside a loop")]
  BreakOutsideLoop,
  #[error("Wildcard encountered outside of pattern")]
  WildcardOutsidePattern,
  #[error(
    "Can't have additional patterns in a match block after a wildcard pattern"
  )]
  PatternAfterWildcard,
  #[error(
    "Invalid pattern; patterns may only be literals, names, or wildcards"
  )]
  InvalidPattern,
  #[error("The same pattern cannot appear twice in a match block")]
  DuplicatePattern,
  #[error("`match` expression doesn't have exhaustive patterns")]
  NonexhaustiveMatch,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CompileError {
  pub kind: CompileErrorKind,
  pub source_trace: SourceTrace,
}
impl CompileError {
  pub fn new(kind: CompileErrorKind, source_trace: SourceTrace) -> Self {
    Self { kind, source_trace }
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

#[derive(Debug, Clone)]
pub struct ErrorLog {
  errors: HashSet<CompileError>,
}

impl ErrorLog {
  pub fn new() -> Self {
    Self {
      errors: HashSet::new(),
    }
  }
  pub fn log(&mut self, err: CompileError) {
    self.errors.insert(err);
  }
  pub fn log_all(&mut self, errs: impl IntoIterator<Item = CompileError>) {
    for err in errs.into_iter() {
      self.log(err);
    }
  }
  pub fn is_empty(&self) -> bool {
    self.errors.is_empty()
  }
  pub fn into_iter(self) -> impl Iterator<Item = CompileError> {
    self.errors.into_iter()
  }
  pub fn iter(&self) -> impl Iterator<Item = CompileError> {
    self.clone().into_iter()
  }
}

impl From<CompileError> for ErrorLog {
  fn from(e: CompileError) -> Self {
    let mut errors = ErrorLog::new();
    errors.log(e);
    errors
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MetadataDescription {
  Singular(String),
  Map(Vec<(String, String)>),
  Multiple(Vec<Self>),
}

impl From<Metadata> for MetadataDescription {
  fn from(metadata: Metadata) -> Self {
    match metadata {
      Metadata::Singular(s) => MetadataDescription::Singular((*s).to_string()),
      Metadata::Map(items) => MetadataDescription::Map(
        items
          .into_iter()
          .map(|(a, b)| ((*a).to_string(), (*b).to_string()))
          .collect(),
      ),
      Metadata::Multiple(metadatas) => MetadataDescription::Multiple(
        metadatas.into_iter().map(|m| m.into()).collect(),
      ),
    }
  }
}
