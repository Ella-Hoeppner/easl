use sse::{document::DocumentPosition, ParseError};
use std::{collections::HashSet, fmt::Display, hash::Hash, rc::Rc, sync::Arc};
use thiserror::Error;

use crate::parse::EaslTree;

use super::{
  metadata::Metadata,
  types::{TypeConstraintDescription, TypeDescription, TypeStateDescription},
};

use std::error::Error;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceTrace(Vec<DocumentPosition>);

impl SourceTrace {
  pub fn empty() -> Self {
    Self(vec![])
  }
  pub fn combine_with(mut self, other: Self) -> Self {
    for pos in other.0 {
      if !self.0.contains(&pos) {
        self.0.push(pos);
      }
    }
    self
  }
  pub fn all_document_positions(&self) -> Vec<&DocumentPosition> {
    self.0.iter().collect()
  }
}

impl From<DocumentPosition> for SourceTrace {
  fn from(position: DocumentPosition) -> Self {
    Self(vec![position])
  }
}

impl From<&DocumentPosition> for SourceTrace {
  fn from(position: &DocumentPosition) -> Self {
    position.clone().into()
  }
}

impl FromIterator<SourceTrace> for SourceTrace {
  fn from_iter<T: IntoIterator<Item = SourceTrace>>(iter: T) -> Self {
    let mut iter = iter.into_iter();
    if let Some(first) = iter.next() {
      iter.fold(first, |trace, subtrace| trace.combine_with(subtrace))
    } else {
      Self::empty()
    }
  }
}

#[derive(Clone, Debug, Error, PartialEq, Eq, Hash)]
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
  CouldntInferTypes,
  #[error("Incompatible types: expected {0}, found {1}")]
  IncompatibleTypes(TypeStateDescription, TypeStateDescription),
  #[error("Function argument types incompatible. Function had type {}\n Recieved arguments of types:\n{}", .f, .args.iter().map(|t| format!("{t}")).collect::<Vec<String>>().join("\n"))]
  FunctionArgumentTypesIncompatible {
    f: TypeStateDescription,
    args: Vec<TypeStateDescription>,
  },
  #[error("Function expression has non-function type: {0}")]
  FunctionExpressionHasNonFunctionType(TypeDescription),
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
  MacroError(Rc<str>),
  #[error("Invalid array access syntax")]
  InvalidArrayAccessSyntax,
  #[error("Array access on non-array type")]
  ArrayAccessOnNonArray,
  #[error("Array literal had non-array type")]
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
  CantShadowTopLevelBinding(Rc<str>),
  #[error("Invalid signature for @associative function, signature must conform to (Fn [T T]: T)")]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum AsyncCompileErrorKind {
  ParsingFailed(ParseError),
  UnrecognizedTypeName(Arc<str>),
  InvalidMetadata(Arc<str>),
  ExpectedTypeAnnotatedName,
  StructFieldMissingType,
  InvalidStructName,
  NoStructNamed(Arc<str>),
  InvalidArraySignature,
  InvalidStructDefinition,
  UnrecognizedGeneric(Arc<str>),
  InvalidToken(Arc<str>),
  InvalidTopLevelVar(Arc<str>),
  InvalidDefn(Arc<str>),
  InvalidFunction,
  FunctionMissingBody,
  UnrecognizedTopLevelForm(EaslTree),
  EmptyList,
  MissingType,
  InvalidType(EaslTree),
  InvalidTypeName(Arc<str>),
  FunctionArgMissingType,
  InvalidArgumentName,
  CouldntInferTypes,
  IncompatibleTypes(TypeStateDescription, TypeStateDescription),
  FunctionArgumentTypesIncompatible {
    f: TypeStateDescription,
    args: Vec<TypeStateDescription>,
  },
  FunctionExpressionHasNonFunctionType(TypeDescription),
  UnboundName(Arc<str>),
  AppliedNonFunction,
  WrongArity(Option<Arc<str>>),
  ExpectedLeaf,
  InvalidFunctionArgumentList,
  InvalidFunctionSignature,
  FunctionSignatureMissingArgumentList,
  FunctionSignatureNotSquareBrackets,
  FunctionSignatureMissingReturnType,
  NoSuchField {
    struct_name: Arc<str>,
    field_name: Arc<str>,
  },
  AccessorOnNonStruct,
  AccessorHadMultipleArguments,
  NotEnoughLetBlockChildren,
  LetBindingsNotSquareBracketed,
  OddNumberOfChildrenInLetBindings,
  ExpectedBindingName,
  EmptyBlock,
  ConstantMayNotHaveMetadata,
  InvalidVariableMetadata(AsyncMetadata),
  InvalidFunctionMetadata(AsyncMetadata),
  InvalidAssignmentTarget,
  AssignmentTargetMustBeVariable(Arc<str>),
  MatchMissingScrutinee,
  MatchMissingArms,
  MatchIncompleteArm,
  InvalidStructFieldType,
  InvalidTypeConstraint,
  UnsatisfiedTypeConstraint(TypeConstraintDescription),
  InvalidForLoopHeader,
  InvalidWhileLoop,
  ReturnOutsideFunction,
  EnclosingFunctionTypeWasntFunction,
  InvalidReturn,
  MacroError(Arc<str>),
  InvalidArrayAccessSyntax,
  ArrayAccessOnNonArray,
  ArrayLiteralMistyped,
  ApplicationsMustUseNames,
  AnonymousFunctionsNotYetSupported,
  AnonymousStructsNotYetSupported,
  EncounteredCommentInSource,
  EncounteredMetadataInInternalExpression,
  InvalidFunctionType,
  CantInlineFunctionWithoutAbstractAncestor,
  NoArgNamesForFunction,
  ZeroedArrayShouldntHaveChildren,
  ExpectedFunctionFoundNonFunction,
  CantShadowTopLevelBinding(Arc<str>),
  InvalidAssociativeSignature,
  TypelessBinding,
  DiscardOutsideFragment,
  ContinueOutsideLoop,
  BreakOutsideLoop,
  PatternAfterWildcard,
  WildcardOutsidePattern,
  InvalidPattern,
  DuplicatePattern,
  NonexhaustiveMatch,
}

impl From<CompileErrorKind> for AsyncCompileErrorKind {
  fn from(value: CompileErrorKind) -> Self {
    use CompileErrorKind::*;
    match value {
      ParsingFailed(a) => AsyncCompileErrorKind::ParsingFailed(a),
      UnrecognizedTypeName(a) => {
        AsyncCompileErrorKind::UnrecognizedTypeName((*a).into())
      }
      InvalidMetadata(a) => AsyncCompileErrorKind::InvalidMetadata((*a).into()),
      ExpectedTypeAnnotatedName => {
        AsyncCompileErrorKind::ExpectedTypeAnnotatedName
      }
      StructFieldMissingType => AsyncCompileErrorKind::StructFieldMissingType,
      InvalidStructName => AsyncCompileErrorKind::InvalidStructName,
      NoStructNamed(a) => AsyncCompileErrorKind::NoStructNamed((*a).into()),
      InvalidArraySignature => AsyncCompileErrorKind::InvalidArraySignature,
      InvalidStructDefinition => AsyncCompileErrorKind::InvalidStructDefinition,
      UnrecognizedGeneric(a) => {
        AsyncCompileErrorKind::UnrecognizedGeneric((*a).into())
      }
      InvalidToken(a) => AsyncCompileErrorKind::InvalidToken((*a).into()),
      InvalidTopLevelVar(a) => {
        AsyncCompileErrorKind::InvalidTopLevelVar((*a).into())
      }
      InvalidDefn(a) => AsyncCompileErrorKind::InvalidDefn((*a).into()),
      InvalidFunction => AsyncCompileErrorKind::InvalidFunction,
      FunctionMissingBody => AsyncCompileErrorKind::FunctionMissingBody,
      UnrecognizedTopLevelForm(ast) => {
        AsyncCompileErrorKind::UnrecognizedTopLevelForm(ast)
      }
      WildcardOutsidePattern => AsyncCompileErrorKind::WildcardOutsidePattern,
      EmptyList => AsyncCompileErrorKind::EmptyList,
      MissingType => AsyncCompileErrorKind::MissingType,
      InvalidType(ast) => AsyncCompileErrorKind::InvalidType(ast),
      InvalidTypeName(a) => AsyncCompileErrorKind::InvalidTypeName((*a).into()),
      FunctionArgMissingType => AsyncCompileErrorKind::FunctionArgMissingType,
      InvalidArgumentName => AsyncCompileErrorKind::InvalidArgumentName,
      CouldntInferTypes => AsyncCompileErrorKind::CouldntInferTypes,
      IncompatibleTypes(a, b) => AsyncCompileErrorKind::IncompatibleTypes(a, b),
      FunctionArgumentTypesIncompatible { f, args } => {
        AsyncCompileErrorKind::FunctionArgumentTypesIncompatible { f, args }
      }
      FunctionExpressionHasNonFunctionType(type_description) => {
        AsyncCompileErrorKind::FunctionExpressionHasNonFunctionType(
          type_description,
        )
      }
      UnboundName(a) => AsyncCompileErrorKind::UnboundName((*a).into()),
      AppliedNonFunction => AsyncCompileErrorKind::AppliedNonFunction,
      WrongArity(a) => {
        AsyncCompileErrorKind::WrongArity(a.map(|a| (*a).into()))
      }
      ExpectedLeaf => AsyncCompileErrorKind::ExpectedLeaf,
      InvalidFunctionArgumentList => {
        AsyncCompileErrorKind::InvalidFunctionArgumentList
      }
      InvalidFunctionSignature => {
        AsyncCompileErrorKind::InvalidFunctionSignature
      }
      FunctionSignatureMissingArgumentList => {
        AsyncCompileErrorKind::FunctionSignatureMissingArgumentList
      }
      FunctionSignatureNotSquareBrackets => {
        AsyncCompileErrorKind::FunctionSignatureNotSquareBrackets
      }
      FunctionSignatureMissingReturnType => {
        AsyncCompileErrorKind::FunctionSignatureMissingReturnType
      }
      NoSuchField {
        struct_name,
        field_name,
      } => AsyncCompileErrorKind::NoSuchField {
        struct_name: (*struct_name).into(),
        field_name: (*field_name).into(),
      },
      AccessorOnNonStruct => AsyncCompileErrorKind::AccessorOnNonStruct,
      AccessorHadMultipleArguments => {
        AsyncCompileErrorKind::AccessorHadMultipleArguments
      }
      NotEnoughLetBlockChildren => {
        AsyncCompileErrorKind::NotEnoughLetBlockChildren
      }
      LetBindingsNotSquareBracketed => {
        AsyncCompileErrorKind::LetBindingsNotSquareBracketed
      }
      OddNumberOfChildrenInLetBindings => {
        AsyncCompileErrorKind::OddNumberOfChildrenInLetBindings
      }
      ExpectedBindingName => AsyncCompileErrorKind::ExpectedBindingName,
      EmptyBlock => AsyncCompileErrorKind::EmptyBlock,
      ConstantMayNotHaveMetadata => {
        AsyncCompileErrorKind::ConstantMayNotHaveMetadata
      }
      InvalidVariableMetadata(a) => {
        AsyncCompileErrorKind::InvalidVariableMetadata(a.into())
      }
      InvalidFunctionMetadata(a) => {
        AsyncCompileErrorKind::InvalidFunctionMetadata(a.into())
      }
      InvalidAssignmentTarget => AsyncCompileErrorKind::InvalidAssignmentTarget,
      AssignmentTargetMustBeVariable(a) => {
        AsyncCompileErrorKind::AssignmentTargetMustBeVariable((*a).into())
      }
      MatchMissingScrutinee => AsyncCompileErrorKind::MatchMissingScrutinee,
      MatchMissingArms => AsyncCompileErrorKind::MatchMissingArms,
      MatchIncompleteArm => AsyncCompileErrorKind::MatchIncompleteArm,
      InvalidStructFieldType => AsyncCompileErrorKind::InvalidStructFieldType,
      InvalidTypeConstraint => AsyncCompileErrorKind::InvalidTypeConstraint,
      UnsatisfiedTypeConstraint(a) => {
        AsyncCompileErrorKind::UnsatisfiedTypeConstraint(a)
      }
      InvalidForLoopHeader => AsyncCompileErrorKind::InvalidForLoopHeader,
      InvalidWhileLoop => AsyncCompileErrorKind::InvalidWhileLoop,
      ReturnOutsideFunction => AsyncCompileErrorKind::ReturnOutsideFunction,
      EnclosingFunctionTypeWasntFunction => {
        AsyncCompileErrorKind::EnclosingFunctionTypeWasntFunction
      }
      InvalidReturn => AsyncCompileErrorKind::InvalidReturn,
      MacroError(a) => AsyncCompileErrorKind::MacroError((*a).into()),
      InvalidArrayAccessSyntax => {
        AsyncCompileErrorKind::InvalidArrayAccessSyntax
      }
      ArrayAccessOnNonArray => AsyncCompileErrorKind::ArrayAccessOnNonArray,
      ArrayLiteralMistyped => AsyncCompileErrorKind::ArrayLiteralMistyped,
      ApplicationsMustUseNames => {
        AsyncCompileErrorKind::ApplicationsMustUseNames
      }
      AnonymousFunctionsNotYetSupported => {
        AsyncCompileErrorKind::AnonymousFunctionsNotYetSupported
      }
      AnonymousStructsNotYetSupported => {
        AsyncCompileErrorKind::AnonymousStructsNotYetSupported
      }
      EncounteredCommentInSource => {
        AsyncCompileErrorKind::EncounteredCommentInSource
      }
      EncounteredMetadataInInternalExpression => {
        AsyncCompileErrorKind::EncounteredMetadataInInternalExpression
      }
      InvalidFunctionType => AsyncCompileErrorKind::InvalidFunctionType,
      CantInlineFunctionWithoutAbstractAncestor => {
        AsyncCompileErrorKind::CantInlineFunctionWithoutAbstractAncestor
      }
      NoArgNamesForFunction => AsyncCompileErrorKind::NoArgNamesForFunction,
      ZeroedArrayShouldntHaveChildren => {
        AsyncCompileErrorKind::ZeroedArrayShouldntHaveChildren
      }
      ExpectedFunctionFoundNonFunction => {
        AsyncCompileErrorKind::ExpectedFunctionFoundNonFunction
      }
      CantShadowTopLevelBinding(a) => {
        AsyncCompileErrorKind::CantShadowTopLevelBinding((*a).into())
      }
      InvalidAssociativeSignature => {
        AsyncCompileErrorKind::InvalidAssociativeSignature
      }
      TypelessBinding => AsyncCompileErrorKind::TypelessBinding,
      DiscardOutsideFragment => AsyncCompileErrorKind::DiscardOutsideFragment,
      ContinueOutsideLoop => AsyncCompileErrorKind::ContinueOutsideLoop,
      BreakOutsideLoop => AsyncCompileErrorKind::BreakOutsideLoop,
      PatternAfterWildcard => AsyncCompileErrorKind::PatternAfterWildcard,
      InvalidPattern => AsyncCompileErrorKind::InvalidPattern,
      DuplicatePattern => AsyncCompileErrorKind::DuplicatePattern,
      NonexhaustiveMatch => AsyncCompileErrorKind::NonexhaustiveMatch,
    }
  }
}

impl From<AsyncCompileErrorKind> for CompileErrorKind {
  fn from(value: AsyncCompileErrorKind) -> Self {
    use AsyncCompileErrorKind::*;
    match value {
      ParsingFailed(a) => CompileErrorKind::ParsingFailed(a),
      UnrecognizedTypeName(a) => {
        CompileErrorKind::UnrecognizedTypeName((*a).into())
      }
      InvalidMetadata(a) => CompileErrorKind::InvalidMetadata((*a).into()),
      ExpectedTypeAnnotatedName => CompileErrorKind::ExpectedTypeAnnotatedName,
      StructFieldMissingType => CompileErrorKind::StructFieldMissingType,
      InvalidStructName => CompileErrorKind::InvalidStructName,
      NoStructNamed(a) => CompileErrorKind::NoStructNamed((*a).into()),
      InvalidArraySignature => CompileErrorKind::InvalidArraySignature,
      InvalidStructDefinition => CompileErrorKind::InvalidStructDefinition,
      UnrecognizedGeneric(a) => {
        CompileErrorKind::UnrecognizedGeneric((*a).into())
      }
      InvalidToken(a) => CompileErrorKind::InvalidToken((*a).into()),
      InvalidTopLevelVar(a) => {
        CompileErrorKind::InvalidTopLevelVar((*a).into())
      }
      InvalidDefn(a) => CompileErrorKind::InvalidDefn((*a).into()),
      InvalidFunction => CompileErrorKind::InvalidFunction,
      FunctionMissingBody => CompileErrorKind::FunctionMissingBody,
      UnrecognizedTopLevelForm(ast) => {
        CompileErrorKind::UnrecognizedTopLevelForm(ast)
      }
      EmptyList => CompileErrorKind::EmptyList,
      MissingType => CompileErrorKind::MissingType,
      InvalidType(ast) => CompileErrorKind::InvalidType(ast),
      InvalidTypeName(a) => CompileErrorKind::InvalidTypeName((*a).into()),
      FunctionArgMissingType => CompileErrorKind::FunctionArgMissingType,
      InvalidArgumentName => CompileErrorKind::InvalidArgumentName,
      CouldntInferTypes => CompileErrorKind::CouldntInferTypes,
      IncompatibleTypes(a, b) => CompileErrorKind::IncompatibleTypes(a, b),
      FunctionArgumentTypesIncompatible { f, args } => {
        CompileErrorKind::FunctionArgumentTypesIncompatible { f, args }
      }
      FunctionExpressionHasNonFunctionType(type_description) => {
        CompileErrorKind::FunctionExpressionHasNonFunctionType(type_description)
      }
      UnboundName(a) => CompileErrorKind::UnboundName((*a).into()),
      AppliedNonFunction => CompileErrorKind::AppliedNonFunction,
      WrongArity(a) => CompileErrorKind::WrongArity(a.map(|a| (*a).into())),
      ExpectedLeaf => CompileErrorKind::ExpectedLeaf,
      InvalidFunctionArgumentList => {
        CompileErrorKind::InvalidFunctionArgumentList
      }
      InvalidFunctionSignature => CompileErrorKind::InvalidFunctionSignature,
      FunctionSignatureMissingArgumentList => {
        CompileErrorKind::FunctionSignatureMissingArgumentList
      }
      FunctionSignatureNotSquareBrackets => {
        CompileErrorKind::FunctionSignatureNotSquareBrackets
      }
      FunctionSignatureMissingReturnType => {
        CompileErrorKind::FunctionSignatureMissingReturnType
      }
      NoSuchField {
        struct_name,
        field_name,
      } => CompileErrorKind::NoSuchField {
        struct_name: (*struct_name).into(),
        field_name: (*field_name).into(),
      },
      AccessorOnNonStruct => CompileErrorKind::AccessorOnNonStruct,
      AccessorHadMultipleArguments => {
        CompileErrorKind::AccessorHadMultipleArguments
      }
      NotEnoughLetBlockChildren => CompileErrorKind::NotEnoughLetBlockChildren,
      LetBindingsNotSquareBracketed => {
        CompileErrorKind::LetBindingsNotSquareBracketed
      }
      OddNumberOfChildrenInLetBindings => {
        CompileErrorKind::OddNumberOfChildrenInLetBindings
      }
      ExpectedBindingName => CompileErrorKind::ExpectedBindingName,
      EmptyBlock => CompileErrorKind::EmptyBlock,
      ConstantMayNotHaveMetadata => {
        CompileErrorKind::ConstantMayNotHaveMetadata
      }
      InvalidVariableMetadata(a) => {
        CompileErrorKind::InvalidVariableMetadata(a.into())
      }
      InvalidFunctionMetadata(a) => {
        CompileErrorKind::InvalidFunctionMetadata(a.into())
      }
      InvalidAssignmentTarget => CompileErrorKind::InvalidAssignmentTarget,
      AssignmentTargetMustBeVariable(a) => {
        CompileErrorKind::AssignmentTargetMustBeVariable((*a).into())
      }
      MatchMissingScrutinee => CompileErrorKind::MatchMissingScrutinee,
      MatchMissingArms => CompileErrorKind::MatchMissingArms,
      MatchIncompleteArm => CompileErrorKind::MatchIncompleteArm,
      InvalidStructFieldType => CompileErrorKind::InvalidStructFieldType,
      InvalidTypeConstraint => CompileErrorKind::InvalidTypeConstraint,
      UnsatisfiedTypeConstraint(a) => {
        CompileErrorKind::UnsatisfiedTypeConstraint(a)
      }
      InvalidForLoopHeader => CompileErrorKind::InvalidForLoopHeader,
      InvalidWhileLoop => CompileErrorKind::InvalidWhileLoop,
      ReturnOutsideFunction => CompileErrorKind::ReturnOutsideFunction,
      EnclosingFunctionTypeWasntFunction => {
        CompileErrorKind::EnclosingFunctionTypeWasntFunction
      }
      InvalidReturn => CompileErrorKind::InvalidReturn,
      MacroError(a) => CompileErrorKind::MacroError((*a).into()),
      InvalidArrayAccessSyntax => CompileErrorKind::InvalidArrayAccessSyntax,
      ArrayAccessOnNonArray => CompileErrorKind::ArrayAccessOnNonArray,
      ArrayLiteralMistyped => CompileErrorKind::ArrayLiteralMistyped,
      ApplicationsMustUseNames => CompileErrorKind::ApplicationsMustUseNames,
      AnonymousFunctionsNotYetSupported => {
        CompileErrorKind::AnonymousFunctionsNotYetSupported
      }
      AnonymousStructsNotYetSupported => {
        CompileErrorKind::AnonymousStructsNotYetSupported
      }
      EncounteredCommentInSource => {
        CompileErrorKind::EncounteredCommentInSource
      }
      EncounteredMetadataInInternalExpression => {
        CompileErrorKind::EncounteredMetadataInInternalExpression
      }
      InvalidFunctionType => CompileErrorKind::InvalidFunctionType,
      CantInlineFunctionWithoutAbstractAncestor => {
        CompileErrorKind::CantInlineFunctionWithoutAbstractAncestor
      }
      NoArgNamesForFunction => CompileErrorKind::NoArgNamesForFunction,
      ZeroedArrayShouldntHaveChildren => {
        CompileErrorKind::ZeroedArrayShouldntHaveChildren
      }
      ExpectedFunctionFoundNonFunction => {
        CompileErrorKind::ExpectedFunctionFoundNonFunction
      }
      CantShadowTopLevelBinding(a) => {
        CompileErrorKind::CantShadowTopLevelBinding((*a).into())
      }
      InvalidAssociativeSignature => {
        CompileErrorKind::InvalidAssociativeSignature
      }
      TypelessBinding => CompileErrorKind::TypelessBinding,
      DiscardOutsideFragment => CompileErrorKind::DiscardOutsideFragment,
      ContinueOutsideLoop => CompileErrorKind::ContinueOutsideLoop,
      BreakOutsideLoop => CompileErrorKind::BreakOutsideLoop,
      WildcardOutsidePattern => CompileErrorKind::WildcardOutsidePattern,
      PatternAfterWildcard => CompileErrorKind::PatternAfterWildcard,
      InvalidPattern => CompileErrorKind::InvalidPattern,
      DuplicatePattern => CompileErrorKind::DuplicatePattern,
      NonexhaustiveMatch => CompileErrorKind::NonexhaustiveMatch,
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AsyncCompileError {
  kind: AsyncCompileErrorKind,
  source_trace: SourceTrace,
}

impl From<CompileError> for AsyncCompileError {
  fn from(err: CompileError) -> Self {
    Self {
      kind: err.kind.into(),
      source_trace: err.source_trace,
    }
  }
}

impl From<AsyncCompileError> for CompileError {
  fn from(err: AsyncCompileError) -> Self {
    Self {
      kind: err.kind.into(),
      source_trace: err.source_trace,
    }
  }
}

#[derive(Debug, Clone)]
pub struct AsyncErrorLog {
  errors: HashSet<AsyncCompileError>,
}

impl From<ErrorLog> for AsyncErrorLog {
  fn from(log: ErrorLog) -> Self {
    Self {
      errors: log.errors.into_iter().map(|e| e.into()).collect(),
    }
  }
}

impl From<AsyncErrorLog> for ErrorLog {
  fn from(log: AsyncErrorLog) -> Self {
    Self {
      errors: log.errors.into_iter().map(|e| e.into()).collect(),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AsyncMetadata {
  Singular(Arc<str>),
  Map(Vec<(Arc<str>, Arc<str>)>),
  Multiple(Vec<Self>),
}

impl From<Metadata> for AsyncMetadata {
  fn from(metadata: Metadata) -> Self {
    match metadata {
      Metadata::Singular(s) => AsyncMetadata::Singular((*s).into()),
      Metadata::Map(items) => AsyncMetadata::Map(
        items
          .into_iter()
          .map(|(a, b)| ((*a).into(), (*b).into()))
          .collect(),
      ),
      Metadata::Multiple(metadatas) => AsyncMetadata::Multiple(
        metadatas.into_iter().map(|m| m.into()).collect(),
      ),
    }
  }
}

impl From<AsyncMetadata> for Metadata {
  fn from(metadata: AsyncMetadata) -> Self {
    match metadata {
      AsyncMetadata::Singular(s) => Metadata::Singular((*s).into()),
      AsyncMetadata::Map(items) => Metadata::Map(
        items
          .into_iter()
          .map(|(a, b)| ((*a).into(), (*b).into()))
          .collect(),
      ),
      AsyncMetadata::Multiple(metadatas) => {
        Metadata::Multiple(metadatas.into_iter().map(|m| m.into()).collect())
      }
    }
  }
}
