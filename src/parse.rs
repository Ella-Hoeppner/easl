use sse::{
  document::Document, standard_whitespace_chars, DocumentSyntaxTree,
  Encloser as SSEEncloser, Operator as SSEOperator, ParseError, SyntaxGraph,
};

use crate::compiler::error::{CompileError, CompileErrorKind, CompileResult};
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Encloser {
  Parens,
  Square,
  Curly,
}
impl SSEEncloser for Encloser {
  fn id_str(&self) -> &str {
    match self {
      Encloser::Parens => "",
      Encloser::Square => ":square-brackets:",
      Encloser::Curly => ":cury-brackets:",
    }
  }

  fn opening_encloser_str(&self) -> &str {
    match self {
      Encloser::Parens => "(",
      Encloser::Square => "[",
      Encloser::Curly => "{",
    }
  }

  fn closing_encloser_str(&self) -> &str {
    match self {
      Encloser::Parens => ")",
      Encloser::Square => "]",
      Encloser::Curly => "}",
    }
  }
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Operator {
  MetadataAnnotation,
  TypeAnnotation,
}
impl SSEOperator for Operator {
  fn id_str(&self) -> &str {
    match self {
      Operator::MetadataAnnotation => ":metadata:",
      Operator::TypeAnnotation => ":type-annotation:",
    }
  }

  fn left_args(&self) -> usize {
    match self {
      Operator::MetadataAnnotation => 0,
      Operator::TypeAnnotation => 1,
    }
  }

  fn right_args(&self) -> usize {
    match self {
      Operator::MetadataAnnotation => 2,
      Operator::TypeAnnotation => 1,
    }
  }

  fn op_str(&self) -> &str {
    match self {
      Operator::MetadataAnnotation => "@",
      Operator::TypeAnnotation => ":",
    }
  }
}

pub fn parse_tynt(
  tynt_source: &str,
) -> CompileResult<Document<(), Encloser, Operator>> {
  Document::from_text_with_syntax(
    SyntaxGraph::contextless(
      vec![Encloser::Parens, Encloser::Square, Encloser::Curly],
      vec![Operator::MetadataAnnotation, Operator::TypeAnnotation],
      standard_whitespace_chars(),
    ),
    tynt_source,
  )
  .map_err(|e| CompileErrorKind::ParsingFailed(e).into())
}

pub type TyntTree = DocumentSyntaxTree<Encloser, Operator>;
