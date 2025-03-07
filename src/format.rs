use sse::{Encloser as SSEEncloser, EncloserOrOperator};

use crate::{
  compiler::program::EaslDocument,
  parse::{EaslTree, Encloser, Operator},
};

#[derive(Clone, Copy, PartialEq)]
pub enum EaslFormatter {
  TopLevel,
  TopLevelMetadata,
  Inner { indentation: usize },
}
use EaslFormatter::*;

impl EaslFormatter {
  fn indent(self, n: usize) -> Self {
    match self {
      Inner { indentation } => Inner {
        indentation: indentation + n,
      },
      _ => Inner { indentation: n },
    }
  }
  fn indentation(&self) -> usize {
    match self {
      Inner { indentation } => *indentation,
      _ => 0,
    }
  }
  fn indented_newline(&self) -> String {
    "\n".to_string() + &" ".repeat(self.indentation())
  }
}

impl EaslFormatter {
  fn format_two_per_line(&mut self, children: Vec<EaslTree>) -> String {
    let mut s = String::new();
    let mut child_iter = children.into_iter();
    while let Some(next_child) = child_iter.next() {
      let next_child_string = self.format(next_child);
      s += &next_child_string;
      if let Some(next_child) = child_iter.next() {
        let secondary_formatter = self.indent(next_child_string.len() + 1);
        s += " ";
        s += &secondary_formatter.format(next_child);
      }
      if child_iter.len() != 0 {
        s += &self.indented_newline()
      }
    }
    s
  }
  pub fn format(self, ast: EaslTree) -> String {
    match ast {
      EaslTree::Leaf(_, leaf) => leaf,
      EaslTree::Inner((_, encloser_or_operator), mut children) => {
        use Encloser::*;
        use Operator::*;
        match encloser_or_operator {
          EncloserOrOperator::Encloser(encloser) => {
            let opener = encloser.opening_encloser_str();
            let closer = encloser.closing_encloser_str();
            let mut inner_formatter = self.indent(opener.len());
            opener.to_string()
              + &match encloser {
                Parens => {
                  let first_child_string =
                    inner_formatter.format(children.remove(0));
                  match first_child_string.as_str() {
                    "var" | "def" | "override" => {
                      first_child_string
                        + " "
                        + &children
                          .into_iter()
                          .map(|child| inner_formatter.format(child))
                          .collect::<Vec<String>>()
                          .join(" ")
                    }
                    "struct" => {
                      let name_string =
                        inner_formatter.format(children.remove(0));
                      let body_formatter = inner_formatter.indent(1);
                      first_child_string
                        + " "
                        + &name_string
                        + &body_formatter.indented_newline()
                        + &children
                          .into_iter()
                          .map(|child| inner_formatter.format(child))
                          .collect::<Vec<String>>()
                          .join(&body_formatter.indented_newline())
                    }
                    "defn" => {
                      let name_string =
                        inner_formatter.format(children.remove(0));
                      let arg_formatter = inner_formatter.indent(
                        first_child_string.len() + name_string.len() + 2,
                      );
                      let body_formatter = inner_formatter.indent(1);
                      first_child_string
                        + " "
                        + &name_string
                        + " "
                        + &arg_formatter.format(children.remove(0))
                        + &body_formatter.indented_newline()
                        + &children
                          .into_iter()
                          .map(|child| body_formatter.format(child))
                          .collect::<Vec<String>>()
                          .join(&body_formatter.indented_newline())
                    }
                    "let" => {
                      let bindings_string = if let Some(EaslTree::Inner(
                        (_, EncloserOrOperator::Encloser(Encloser::Square)),
                        _,
                      )) = children.get(0)
                      {
                        let bindings = children.remove(0);
                        if let EaslTree::Inner(_, bindings_children) = bindings
                        {
                          let mut bindings_formatter = inner_formatter
                            .indent(first_child_string.len() + 2);
                          " [".to_string()
                            + &bindings_formatter
                              .format_two_per_line(bindings_children)
                            + "]"
                        } else {
                          unreachable!()
                        }
                      } else {
                        String::new()
                      };
                      let body_formatter = inner_formatter.indent(1);
                      first_child_string
                        + &bindings_string
                        + &body_formatter.indented_newline()
                        + &children
                          .into_iter()
                          .map(|child| body_formatter.format(child))
                          .collect::<Vec<String>>()
                          .join(&body_formatter.indented_newline())
                    }
                    _ => {
                      let inner_inner_formatter =
                        inner_formatter.indent(first_child_string.len() + 1);
                      first_child_string
                        + " "
                        + &children
                          .into_iter()
                          .map(|child| inner_inner_formatter.format(child))
                          .collect::<Vec<String>>()
                          .join(&inner_inner_formatter.indented_newline())
                    }
                  }
                }
                Curly => {
                  if self == TopLevelMetadata {
                    children
                      .into_iter()
                      .map(|child| inner_formatter.format(child))
                      .collect::<Vec<String>>()
                      .join(" ")
                  } else {
                    inner_formatter.format_two_per_line(children)
                  }
                }
                _ => children
                  .into_iter()
                  .map(|child| inner_formatter.format(child))
                  .collect::<Vec<String>>()
                  .join(&inner_formatter.indented_newline()),
              }
              + closer
          }
          EncloserOrOperator::Operator(operator) => match operator {
            MetadataAnnotation => {
              "@".to_string()
                + &children
                  .into_iter()
                  .enumerate()
                  .map(|(i, child)| {
                    if i == 0 {
                      if self == TopLevel {
                        TopLevelMetadata.format(child)
                      } else {
                        self.indent(1).format(child)
                      }
                    } else {
                      self.format(child)
                    }
                  })
                  .collect::<Vec<String>>()
                  .join(if self == TopLevel { "\n" } else { " " })
            }
            TypeAnnotation => {
              let mut child_strings = children
                .into_iter()
                .map(|child| self.format(child))
                .collect::<Vec<String>>();
              child_strings.remove(0) + ": " + &child_strings.join(" ")
            }
            ExpressionComment => {
              "#_".to_string() + &self.format(children.remove(0))
            }
            Reference => "&".to_string() + &self.format(children.remove(0)),
          },
        }
      }
    }
  }
}

pub fn format_document(document: EaslDocument) -> String {
  let formatter = EaslFormatter::TopLevel;
  let mut s = String::new();
  for (i, ast) in document.syntax_trees.into_iter().enumerate() {
    if i > 0 {
      s += "\n";
    }
    s += &formatter.format(ast);
    s += "\n";
  }
  s
}

impl Default for EaslFormatter {
  fn default() -> Self {
    TopLevel
  }
}
