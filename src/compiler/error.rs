use sse::{ParseError, document::DocumentPosition};
use std::{collections::HashSet, fmt::Display, hash::Hash};
use thiserror::Error;

use crate::{compiler::vars::VariableAddressSpace, parse::EaslTree};

use super::{
  annotation::Annotation,
  types::{TypeConstraintDescription, TypeDescription, TypeStateDescription},
};

use std::error::Error;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceTrace {
  pub primary_position: Option<DocumentPosition>,
  pub secondary_positions: Vec<DocumentPosition>,
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

#[derive(Clone, Debug, Error)]
pub enum CompileErrorKind {
  #[error("Parsing error: `{0}`")]
  ParsingFailed(ParseError),
  #[error("Unrecognized type: `{0}`")]
  UnrecognizedTypeName(String),
  #[error("Invalid annotation: `{0}`")]
  InvalidAnnotation(String),
  #[error("Expected type annotation")]
  ExpectedTypeAnnotatedName,
  #[error("Struct fields must be typed")]
  StructFieldMissingType,
  #[error("Invalid type name")]
  InvalidTypeName,
  #[error("Couldn't find a type named `{0}`")]
  NoTypeNamed(String),
  #[error("Invalid array signature")]
  InvalidArraySignature,
  #[error("Array indexing needs 1 argument, got {0}")]
  ArrayLookupInvalidArity(usize),
  #[error("Invalid type definition")]
  InvalidTypeDefinition,
  #[error("No generic named `{0}`")]
  UnrecognizedGeneric(String),
  #[error("Invalid token `{0}`")]
  InvalidToken(String),
  #[error("Invalid var `{0}`")]
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
  #[error("Function argument missing type")]
  FunctionArgMissingType,
  #[error("Invalid argument name")]
  InvalidArgumentName,
  #[error("Couldn't infer types")]
  CouldntInferTypes,
  #[error("Incompatible types: {0} != {1}")]
  IncompatibleTypes(TypeStateDescription, TypeStateDescription),
  #[error("Function argument types incompatible. Function had type {}\n Recieved arguments of types:\n{}", .f, .args.iter().map(|t| format!("{t}")).collect::<Vec<String>>().join("\n"))]
  FunctionArgumentTypesIncompatible {
    f: TypeStateDescription,
    args: Vec<TypeStateDescription>,
  },
  #[error("Duplicate function signature `{0}`")]
  DuplicateFunctionSignature(String),
  #[error("Function signature `{0}` conflicts with built-in function")]
  FunctionSignatureConflictsWithBuiltin(String),
  #[error("Function expression has non-function type: {0}")]
  FunctionExpressionHasNonFunctionType(TypeDescription),
  #[error("Unbound name: `{0}`")]
  UnboundName(String),
  #[error("Applied non-function")]
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
  #[error("Top-level constants may not have annotation")]
  ConstantMayNotHaveAnnotation,
  #[error("Invalid top-level variable annotation: {0:?}")]
  InvalidVariableAnnotation(AnnotationDescription),
  #[error("Invalid top-level function annotation: {0:?}")]
  InvalidFunctionAnnotation(AnnotationDescription),
  #[error("Invalid workgroup size: {0:?}")]
  InvalidWorkgroupSize(String),
  #[error("Conflicting entry point annotations")]
  ConflictingEntryPointAnnotations,
  #[error(
    "workgroup-size annotations are only allowed on functions marked as `@compute` entry points"
  )]
  InvalidWorkgroupSizeAnnotation,
  #[error("compute entry points must specify a workgroup-size")]
  ComputeEntryMissingWorkgroupSize,
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
  #[error("Invalid array")]
  ArrayLiteralMistyped,
  #[error("Applications must use names")]
  ApplicationsMustUseNames,
  #[error("Anonymous functions are not yet supported")]
  AnonymousFunctionsNotYetSupported,
  #[error("Anonymous structs are not yet supported")]
  AnonymousStructsNotYetSupported,
  #[error("Internal compiler error: Encountered comment in source")]
  EncounteredCommentInSource,
  #[error("Can't use annotation here")]
  AnnotationNotAllowed,
  #[error("Invalid function type")]
  InvalidFunctionType,
  #[error(
    "Internal compiler error: Cannot inline function without abstract ancestor"
  )]
  CantInlineFunctionWithoutAbstractAncestor,
  #[error("No argument names for function")]
  NoArgNamesForFunction,
  #[error("zeroed-array shouldn't have any children")]
  ZeroedArrayShouldntHaveChildren,
  #[error("expected function, found non-function value")]
  ExpectedFunctionFoundNonFunction,
  #[error("Can't shadow top-level binding `{0}`")]
  CantShadowTopLevelBinding(String),
  #[error(
    "Invalid signature for @associative function, signature must conform to (Fn [T T]: T)"
  )]
  InvalidAssociativeSignature,
  #[error("Can't bind a name to a typeless expression")]
  TypelessBinding,
  #[error("`discard` can only occur in @fragment functions")]
  DiscardOutsideFragment,
  #[error("function `{0}` can only occur in @fragment functions")]
  FragmentExclusiveFunctionOutsideFragment(String),
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
  #[error("The same pattern cannot appear twice in a match block")]
  DuplicatePattern,
  #[error("`match` expression doesn't have exhaustive patterns")]
  NonexhaustiveMatch,
  #[error("Invalid enum variant")]
  InvalidEnumVariant,
  #[error("Cannot calculate size of type")]
  CantCalculateSize,
  #[error("Invalid `match` pattern")]
  InvalidMatchPattern,
  #[error("`binding` must be specified whenever `group` is specified")]
  GroupMissingBinding,
  #[error("`binding` must be specified whenever `group` is specified")]
  BindingMissingGroup,
  #[error("Address space must be annotated here, e.g. `address uniform`")]
  NeedAddressAnnotation,
  #[error("Invalid address space, this type is only compatible with `{0}`")]
  InvalidAddressSpace(VariableAddressSpace),
  #[error("`group` or `binding` annotations are required address space `{0}`")]
  NeedGroupAndBinding(VariableAddressSpace),
  #[error("Can't assign `group` or `binding` in address space `{0}`")]
  DisallowedGroupAndBinding(VariableAddressSpace),
  #[error(
    "Type `{0}` needs `group` and `binding` annotations, e.g. `@{{group 0 binding 0}}`"
  )]
  NeedsGroupAndBinding(String),
  #[error("Variables in `{0}` require an initial value")]
  NeedsInitializationValue(VariableAddressSpace),
  #[error("Variables in `{0}` may not be given an initial value")]
  DisallowedInitializationValue(VariableAddressSpace),
  #[error("The name `{0}` is used for more than one top-level variable")]
  VariableNameCollision(String),
  #[error("The name `{0}` is used as both a variable name and a function name")]
  VariableFunctionNameCollision(String),
  #[error("Internal compiler error: Tried to compile Unit type")]
  TriedToCompileUnit,
  #[error("Invalid argument annotation")]
  InvalidArgumentAnnotation,
  #[error("Invalid builtin argument name \"{0}\"")]
  InvalidBuiltinArgumentName(String),
  #[error("Multiple conflicting builtins found on function argument")]
  ConflictingBuiltinNames,
  #[error("Builtin argument occurs more than once")]
  DuplicateBuiltinArgument,
  #[error("Builtin arguments are only supported on entry points")]
  BuiltinArgumentsOnlyAllowedOnEntry,
  #[error("Builtin argument \"{0}\" isn't allowed on \"{1}\" entry point")]
  BuiltinArgumentsOnWrongEntry(String, String),
  #[error("Built-in operator \"{0}\" can't accept arguments")]
  BuiltInOperatorTakesNoArguments(String),
}

impl PartialEq for CompileErrorKind {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::ParsingFailed(l0), Self::ParsingFailed(r0)) => l0 == r0,
      (Self::UnrecognizedTypeName(l0), Self::UnrecognizedTypeName(r0)) => {
        l0 == r0
      }
      (Self::InvalidAnnotation(l0), Self::InvalidAnnotation(r0)) => l0 == r0,
      (Self::NoTypeNamed(l0), Self::NoTypeNamed(r0)) => l0 == r0,
      (
        Self::ArrayLookupInvalidArity(l0),
        Self::ArrayLookupInvalidArity(r0),
      ) => l0 == r0,
      (Self::UnrecognizedGeneric(l0), Self::UnrecognizedGeneric(r0)) => {
        l0 == r0
      }
      (Self::InvalidToken(l0), Self::InvalidToken(r0)) => l0 == r0,
      (Self::InvalidTopLevelVar(l0), Self::InvalidTopLevelVar(r0)) => l0 == r0,
      (Self::InvalidDefn(l0), Self::InvalidDefn(r0)) => l0 == r0,
      (
        Self::UnrecognizedTopLevelForm(l0),
        Self::UnrecognizedTopLevelForm(r0),
      ) => l0 == r0,
      (Self::InvalidType(l0), Self::InvalidType(r0)) => l0 == r0,
      (Self::IncompatibleTypes(l0, l1), Self::IncompatibleTypes(r0, r1)) => {
        (l0 == r0 && l1 == r1) || (l0 == r1 && l1 == r0)
      }
      (
        Self::FunctionArgumentTypesIncompatible {
          f: l_f,
          args: l_args,
        },
        Self::FunctionArgumentTypesIncompatible {
          f: r_f,
          args: r_args,
        },
      ) => l_f == r_f && l_args == r_args,
      (
        Self::DuplicateFunctionSignature(l0),
        Self::DuplicateFunctionSignature(r0),
      ) => l0 == r0,
      (
        Self::FunctionSignatureConflictsWithBuiltin(l0),
        Self::FunctionSignatureConflictsWithBuiltin(r0),
      ) => l0 == r0,
      (
        Self::FunctionExpressionHasNonFunctionType(l0),
        Self::FunctionExpressionHasNonFunctionType(r0),
      ) => l0 == r0,
      (Self::UnboundName(l0), Self::UnboundName(r0)) => l0 == r0,
      (Self::WrongArity(l0), Self::WrongArity(r0)) => l0 == r0,
      (
        Self::NoSuchField {
          struct_name: l_struct_name,
          field_name: l_field_name,
        },
        Self::NoSuchField {
          struct_name: r_struct_name,
          field_name: r_field_name,
        },
      ) => l_struct_name == r_struct_name && l_field_name == r_field_name,
      (
        Self::InvalidVariableAnnotation(l0),
        Self::InvalidVariableAnnotation(r0),
      ) => l0 == r0,
      (
        Self::InvalidFunctionAnnotation(l0),
        Self::InvalidFunctionAnnotation(r0),
      ) => l0 == r0,
      (
        Self::AssignmentTargetMustBeVariable(l0),
        Self::AssignmentTargetMustBeVariable(r0),
      ) => l0 == r0,
      (
        Self::UnsatisfiedTypeConstraint(l0),
        Self::UnsatisfiedTypeConstraint(r0),
      ) => l0 == r0,
      (Self::MacroError(l0), Self::MacroError(r0)) => l0 == r0,
      (
        Self::CantShadowTopLevelBinding(l0),
        Self::CantShadowTopLevelBinding(r0),
      ) => l0 == r0,
      _ => core::mem::discriminant(self) == core::mem::discriminant(other),
    }
  }
}

impl Hash for CompileErrorKind {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    core::mem::discriminant(self).hash(state);
  }
}

impl Eq for CompileErrorKind {}

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
pub enum AnnotationDescription {
  Singular(String),
  Map(Vec<(String, String)>),
  Multiple(Vec<Self>),
}

impl From<Annotation> for AnnotationDescription {
  fn from(annotation: Annotation) -> Self {
    match annotation {
      Annotation::Singular(s) => {
        AnnotationDescription::Singular((*s).to_string())
      }
      Annotation::Map(items) => AnnotationDescription::Map(
        items
          .into_iter()
          .map(|(a, b)| ((*a).to_string(), (*b).to_string()))
          .collect(),
      ),
      Annotation::Multiple(sub_annotations) => AnnotationDescription::Multiple(
        sub_annotations.into_iter().map(|m| m.into()).collect(),
      ),
    }
  }
}
