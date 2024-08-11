use sse::{
  document::Document, standard_whitespace_chars, Encloser as SSEEncloser,
  Operator as SSEOperator, ParseError, SyntaxGraph,
};
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
) -> Result<Document<(), Encloser, Operator>, ParseError> {
  Document::from_text_with_syntax(
    SyntaxGraph::contextless(
      vec![Encloser::Parens, Encloser::Square, Encloser::Curly],
      vec![Operator::Metadata, Operator::TypeAnnotation],
      standard_whitespace_chars(),
    ),
    tynt_source,
  )
}
