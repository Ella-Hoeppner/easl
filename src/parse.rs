use sse::document::Document;
use sse::examples::basic::standard_sexp_whitespace_chars;
use sse::Encloser as SSEEncloser;
use sse::Operator as SSEOperator;
use sse::ParseError;
use sse::SyntaxContext;
use sse::SyntaxGraph;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Context {
  Main,
}
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
  Metadata,
  TypeAnnotation,
}
impl SSEOperator for Operator {
  fn id_str(&self) -> &str {
    match self {
      Operator::Metadata => ":metadata:",
      Operator::TypeAnnotation => ":type-annotation:",
    }
  }

  fn left_args(&self) -> usize {
    match self {
      Operator::Metadata => 0,
      Operator::TypeAnnotation => 1,
    }
  }

  fn right_args(&self) -> usize {
    match self {
      Operator::Metadata => 2,
      Operator::TypeAnnotation => 0,
    }
  }

  fn op_str(&self) -> &str {
    match self {
      Operator::Metadata => "@",
      Operator::TypeAnnotation => ":",
    }
  }
}

pub fn parse_tynt(
  tynt_source: &str,
) -> Result<Document<Context, Encloser, Operator>, ParseError> {
  Document::from_text_with_syntax(
    SyntaxGraph::new(
      Context::Main,
      [(
        Context::Main,
        SyntaxContext::new(
          vec![Encloser::Parens, Encloser::Square, Encloser::Curly],
          vec![Operator::Metadata, Operator::TypeAnnotation],
          None,
          standard_sexp_whitespace_chars(),
        ),
      )]
      .into_iter()
      .collect(),
      [
        (Encloser::Parens, Context::Main),
        (Encloser::Square, Context::Main),
        (Encloser::Curly, Context::Main),
      ]
      .into_iter()
      .collect(),
      [
        (Operator::Metadata, Context::Main),
        (Operator::TypeAnnotation, Context::Main),
      ]
      .into_iter()
      .collect(),
    ),
    tynt_source,
  )
}
