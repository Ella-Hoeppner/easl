use std::collections::HashMap;

use sse::{
  document::{Document, DocumentPosition},
  formatting::Formatter,
  standard_whitespace_chars,
  syntax::Context as SSEContext,
  DocumentSyntaxTree, Encloser as SSEEncloser, EncloserOrOperator,
  Operator as SSEOperator, SyntaxContext, SyntaxGraph,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Context {
  Default,
  StructuredComment,
  UnstructuredComment,
}

impl SSEContext for Context {
  fn is_comment(&self) -> bool {
    use Context::*;
    match self {
      StructuredComment | UnstructuredComment => true,
      _ => false,
    }
  }
}

use crate::compiler::error::{
  CompileError, CompileErrorKind, CompileResult, SourceTrace,
};
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Encloser {
  Parens,
  Square,
  ArrayLookup,
  Curly,
  LineComment,
  BlockComment,
}
impl SSEEncloser for Encloser {
  fn id_str(&self) -> &str {
    use Encloser::*;
    match self {
      Parens => "",
      Square => ":square-brackets:",
      ArrayLookup => ":array-lookup:",
      Curly => ":cury-brackets:",
      LineComment => ":line-comment:",
      BlockComment => ":block-comment:",
    }
  }

  fn opening_encloser_str(&self) -> &str {
    use Encloser::*;
    match self {
      Parens => "(",
      Square => "[",
      ArrayLookup => "@[",
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
      ArrayLookup => "]",
      Curly => "}",
      LineComment => "\n",
      BlockComment => "*;",
    }
  }
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Operator {
  MetadataAnnotation,
  TypeAnnotation,
  ExpressionComment,
}
impl SSEOperator for Operator {
  fn id_str(&self) -> &str {
    match self {
      Operator::MetadataAnnotation => ":metadata:",
      Operator::TypeAnnotation => ":type-annotation:",
      Operator::ExpressionComment => ":expression-annotation:",
    }
  }

  fn left_args(&self) -> usize {
    match self {
      Operator::MetadataAnnotation => 0,
      Operator::TypeAnnotation => 1,
      Operator::ExpressionComment => 0,
    }
  }

  fn right_args(&self) -> usize {
    match self {
      Operator::MetadataAnnotation => 2,
      Operator::TypeAnnotation => 1,
      Operator::ExpressionComment => 1,
    }
  }

  fn op_str(&self) -> &str {
    match self {
      Operator::MetadataAnnotation => "@",
      Operator::TypeAnnotation => ":",
      Operator::ExpressionComment => "#_",
    }
  }
}

pub type EaslSynaxGraph = SyntaxGraph<Context, Encloser, Operator>;

pub fn easl_syntax_graph() -> EaslSynaxGraph {
  let default_context = SyntaxContext::new(
    vec![
      Encloser::Parens,
      Encloser::Square,
      Encloser::ArrayLookup,
      Encloser::Curly,
      Encloser::LineComment,
      Encloser::BlockComment,
    ],
    vec![
      Operator::MetadataAnnotation,
      Operator::TypeAnnotation,
      Operator::ExpressionComment,
    ],
    None,
    standard_whitespace_chars(),
  );
  let unstructured_comment_context =
    SyntaxContext::new(vec![], vec![], None, vec![]);
  EaslSynaxGraph::new(
    Context::Default,
    [
      (Context::Default, default_context.clone()),
      (Context::StructuredComment, default_context),
      (Context::UnstructuredComment, unstructured_comment_context),
    ]
    .into(),
    [
      (Encloser::Parens, Context::Default),
      (Encloser::Square, Context::Default),
      (Encloser::ArrayLookup, Context::Default),
      (Encloser::Curly, Context::Default),
      (Encloser::LineComment, Context::UnstructuredComment),
      (Encloser::BlockComment, Context::UnstructuredComment),
    ]
    .into(),
    [
      (Operator::MetadataAnnotation, Context::Default),
      (Operator::TypeAnnotation, Context::Default),
      (Operator::ExpressionComment, Context::StructuredComment),
    ]
    .into(),
  )
}

pub fn parse_easl(
  easl_source: &str,
) -> CompileResult<Document<Context, Encloser, Operator>> {
  Document::from_text_with_syntax(easl_syntax_graph(), easl_source)
    .map_err(|e| {
      CompileError::new(
        CompileErrorKind::ParsingFailed(e),
        SourceTrace::empty(),
      )
    })
    .map(|mut doc| {
      doc.strip_comments();
      doc
    })
}

pub type EaslTree = DocumentSyntaxTree<Encloser, Operator>;
