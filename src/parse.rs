use std::sync::LazyLock;

use sse::{
  Context as SSEContext, DocumentSyntaxTree, Encloser as SSEEncloser,
  Operator as SSEOperator,
  document::Document,
  standard_whitespace_chars,
  syntax::{ContextId, Syntax},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Context {
  Default,
  StructuredComment,
  UnstructuredComment,
}

impl ContextId for Context {
  fn is_comment(&self) -> bool {
    use Context::*;
    match self {
      StructuredComment | UnstructuredComment => true,
      _ => false,
    }
  }
}

use crate::compiler::{
  error::{CompileError, CompileErrorKind, CompileResult, SourceTrace},
  program::EaslDocument,
};
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Encloser {
  Parens,
  Square,
  Curly,
  LineComment,
  BlockComment,
}
impl SSEEncloser for Encloser {
  fn opening_encloser_str(&self) -> &str {
    use Encloser::*;
    match self {
      Parens => "(",
      Square => "[",
      Curly => "{",
      LineComment => ";",
      BlockComment => ";*",
    }
  }

  fn closing_encloser_str(&self) -> &str {
    use Encloser::*;
    match self {
      Parens => ")",
      Square => "]",
      Curly => "}",
      LineComment => "\n",
      BlockComment => "*;",
    }
  }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
  Annotation,
  TypeAscription,
  ExpressionComment,
  Reference,
}
impl SSEOperator for Operator {
  fn left_args(&self) -> usize {
    match self {
      Operator::Annotation => 0,
      Operator::TypeAscription => 1,
      Operator::ExpressionComment => 0,
      Operator::Reference => 0,
    }
  }

  fn right_args(&self) -> usize {
    match self {
      Operator::Annotation => 2,
      Operator::TypeAscription => 1,
      Operator::ExpressionComment => 1,
      Operator::Reference => 1,
    }
  }

  fn op_str(&self) -> &str {
    match self {
      Operator::Annotation => "@",
      Operator::TypeAscription => ":",
      Operator::ExpressionComment => "#_",
      Operator::Reference => "&",
    }
  }
}

static DEFAULT_CTX: LazyLock<SSEContext<Encloser, Operator>> =
  LazyLock::new(|| {
    SSEContext::new(
      vec![
        Encloser::Parens,
        Encloser::Square,
        Encloser::Curly,
        Encloser::LineComment,
        Encloser::BlockComment,
      ],
      vec![
        Operator::Annotation,
        Operator::TypeAscription,
        Operator::ExpressionComment,
        Operator::Reference,
      ],
      None,
      standard_whitespace_chars(),
    )
  });

static TRIVIAL_CTX: LazyLock<SSEContext<Encloser, Operator>> =
  LazyLock::new(|| SSEContext::trivial());

pub struct EaslSyntax;

impl Syntax for EaslSyntax {
  type C = Context;
  type E = Encloser;
  type O = Operator;

  fn root_context(&self) -> Self::C {
    Context::Default
  }
  fn context<'a>(&'a self, id: &Self::C) -> &'a SSEContext<Self::E, Self::O> {
    match id {
      Context::Default | Context::StructuredComment => &*DEFAULT_CTX,
      Context::UnstructuredComment => &*TRIVIAL_CTX,
    }
  }
  fn encloser_context(&self, encloser: &Self::E) -> Self::C {
    match encloser {
      Encloser::Parens | Encloser::Square | Encloser::Curly => Context::Default,
      Encloser::LineComment | Encloser::BlockComment => {
        Context::UnstructuredComment
      }
    }
  }
  fn operator_context(&self, operator: &Self::O) -> Self::C {
    match operator {
      Operator::Annotation | Operator::TypeAscription | Operator::Reference => {
        Context::Default
      }
      Operator::ExpressionComment => Context::StructuredComment,
    }
  }
  fn reserved_tokens(&self) -> impl Iterator<Item = &str> {
    ["&&"].into_iter()
  }
}

pub fn parse_easl(easl_source: &'_ str) -> CompileResult<EaslDocument<'_>> {
  Document::from_text_with_syntax(EaslSyntax, easl_source).map_err(|e| {
    CompileError::new(CompileErrorKind::ParsingFailed(e), SourceTrace::empty())
  })
}

pub fn parse_easl_without_comments(
  easl_source: &'_ str,
) -> CompileResult<EaslDocument<'_>> {
  let mut doc = parse_easl(easl_source)?;
  doc.strip_comments();
  Ok(doc)
}

pub type EaslTree = DocumentSyntaxTree<Encloser, Operator>;
