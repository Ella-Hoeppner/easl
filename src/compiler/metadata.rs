use std::fmt::Display;
use std::rc::Rc;

use sse::syntax::EncloserOrOperator::{self, *};

use crate::compiler::util::compile_word;
use crate::parse::EaslTree;
use crate::parse::{Encloser::*, Operator};

use super::error::{err, CompileErrorKind::*, CompileResult, SourceTrace};

#[derive(Debug, Clone, PartialEq)]
pub enum Metadata {
  Singular(Rc<str>),
  Map(Vec<(Rc<str>, Rc<str>)>),
}

impl Display for Metadata {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Metadata::Singular(s) => write!(f, "{}", s),
      Metadata::Map(items) => {
        write!(f, "{{\n")?;
        for (key, val) in items {
          write!(f, "  {} {}\n", key, val)?;
        }
        write!(f, "\n}}")
      }
    }
  }
}

impl Metadata {
  pub fn from_metadata_tree(ast: EaslTree) -> CompileResult<Self> {
    match ast {
      EaslTree::Leaf(_, singular) => Ok(Self::Singular(singular.into())),
      EaslTree::Inner((position, Encloser(Curly)), map_fields) => {
        let source_trace: SourceTrace = position.into();
        if map_fields.len() % 2 == 0 {
          Ok(Self::Map(
            map_fields
              .into_iter()
              .map(|field| {
                if let EaslTree::Leaf(_, field_string) = field {
                  Ok(field_string.into())
                } else {
                  err(
                    InvalidMetadata("fields must all be leaves".into()),
                    source_trace.clone(),
                  )
                }
              })
              .collect::<CompileResult<Vec<Rc<str>>>>()?
              .chunks(2)
              .map(|arr| (arr[0].clone(), arr[1].clone()))
              .collect(),
          ))
        } else {
          err(
            InvalidMetadata("fields must all be leaves".into()),
            source_trace,
          )
        }
      }
      _ => err(
        InvalidMetadata("fields must all be leaves".into()),
        ast.position().clone().into(),
      ),
    }
  }
  pub fn compile(self) -> String {
    match self {
      Metadata::Singular(value) => format!("@{value} "),
      Metadata::Map(pairs) => {
        pairs
          .into_iter()
          .map(|(property, value)| {
            if &*property == "entry" {
              format!("@{value}")
            } else {
              format!("@{}({})", compile_word(property), compile_word(value))
            }
          })
          .collect::<Vec<String>>()
          .join(" ")
          + " "
      }
    }
  }
  pub fn compile_optional(maybe_self: Option<Self>) -> String {
    if let Some(metadata) = maybe_self {
      metadata.compile()
    } else {
      String::new()
    }
  }
}

pub fn extract_metadata(
  exp: EaslTree,
) -> CompileResult<(Option<Metadata>, EaslTree)> {
  Ok(
    if let EaslTree::Inner(
      (_, EncloserOrOperator::Operator(Operator::MetadataAnnotation)),
      mut children,
    ) = exp
    {
      let exp = children.remove(1);
      (Some(Metadata::from_metadata_tree(children.remove(0))?), exp)
    } else {
      (None, exp)
    },
  )
}
