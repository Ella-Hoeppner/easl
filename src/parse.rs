use std::sync::LazyLock;

use fsexp::{
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

use crate::compiler::program::EaslDocument;
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
}
impl SSEOperator for Operator {
  fn left_args(&self) -> usize {
    match self {
      Operator::Annotation => 0,
      Operator::TypeAscription => 1,
      Operator::ExpressionComment => 0,
    }
  }

  fn right_args(&self) -> usize {
    match self {
      Operator::Annotation => 2,
      Operator::TypeAscription => 1,
      Operator::ExpressionComment => 1,
    }
  }

  fn op_str(&self) -> &str {
    match self {
      Operator::Annotation => "@",
      Operator::TypeAscription => ":",
      Operator::ExpressionComment => "#_",
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
      ],
      None,
      standard_whitespace_chars(),
    )
  });

static TRIVIAL_CTX: LazyLock<SSEContext<Encloser, Operator>> =
  LazyLock::new(|| SSEContext::trivial());

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
  fn encloser_context(&self, encloser: &Self::E) -> Option<Self::C> {
    match encloser {
      Encloser::LineComment | Encloser::BlockComment => {
        Some(Context::UnstructuredComment)
      }
      _ => None,
    }
  }
  fn operator_context(&self, operator: &Self::O) -> Option<Self::C> {
    match operator {
      Operator::ExpressionComment => Some(Context::StructuredComment),
      _ => None,
    }
  }
  fn reserved_tokens(&self) -> impl Iterator<Item = &str> {
    ["||"].into_iter()
  }
}

pub fn parse_easl(easl_source: &'_ str) -> EaslDocument<'_> {
  Document::from_text_with_syntax(EaslSyntax, easl_source)
}

pub fn parse_easl_without_comments(easl_source: &'_ str) -> EaslDocument<'_> {
  let mut doc = parse_easl(easl_source);
  doc.strip_comments();
  doc
}

pub type EaslTree = DocumentSyntaxTree<Encloser, Operator>;
