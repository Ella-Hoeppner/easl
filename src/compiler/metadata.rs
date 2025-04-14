use std::fmt::Display;
use std::rc::Rc;
use std::str::FromStr;

use sse::syntax::EncloserOrOperator::{self, *};

use crate::compiler::util::compile_word;
use crate::parse::EaslTree;
use crate::parse::{Encloser::*, Operator};

use super::error::{
  err, CompileError, CompileErrorKind::*, CompileResult, SourceTrace,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Metadata {
  Singular(Rc<str>),
  Map(Vec<(Rc<str>, Rc<str>)>),
  Multiple(Vec<Self>),
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
      Metadata::Multiple(sub_metadatas) => {
        for metadata in sub_metadatas.iter() {
          metadata.fmt(f).unwrap();
          write!(f, "\n")?
        }
        Ok(())
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
    self
      .properties()
      .into_iter()
      .fold(String::new(), |s, (name, value)| {
        if let Some(value) = value {
          s + &format!("@{}({})", compile_word(name), compile_word(value))
        } else {
          match &*name {
            "vertex" | "fragment" | "compute" => s + &format!("@{name} "),
            _ => s,
          }
        }
      })
  }
  pub fn compile_optional(maybe_self: Option<Self>) -> String {
    if let Some(metadata) = maybe_self {
      metadata.compile()
    } else {
      String::new()
    }
  }
  pub fn properties(&self) -> Vec<(Rc<str>, Option<Rc<str>>)> {
    match self {
      Metadata::Singular(s) => vec![(s.clone(), None)],
      Metadata::Map(items) => items
        .iter()
        .cloned()
        .map(|(property, value)| (property, Some(value)))
        .collect(),
      Metadata::Multiple(sub_metadatas) => sub_metadatas
        .into_iter()
        .map(Self::properties)
        .reduce(|mut a, mut b| {
          a.append(&mut b);
          a
        })
        .unwrap_or(vec![]),
    }
  }
  pub fn validate_for_top_level_variable(
    &self,
    source_trace: &SourceTrace,
  ) -> CompileResult<()> {
    for (property, value) in self.properties().into_iter() {
      match (&*property, value) {
        ("group" | "binding", Some(value)) => {
          if u8::from_str(&*value).is_err() {
            return Err(CompileError::new(
              InvalidVariableMetadata(self.clone()),
              source_trace.clone(),
            ));
          }
        }
        _ => {
          return Err(CompileError::new(
            InvalidVariableMetadata(self.clone()),
            source_trace.clone(),
          ))
        }
      }
    }
    Ok(())
  }
  pub fn validate_for_top_level_function(
    &self,
    source_trace: &SourceTrace,
  ) -> CompileResult<bool> {
    let mut is_associative = false;
    for (property, value) in self.properties().into_iter() {
      match (&*property, value) {
        ("associative", None) => {
          is_associative = true;
        }
        ("fragment" | "vertex" | "compute", None) => {}
        _ => {
          return Err(CompileError::new(
            InvalidFunctionMetadata(self.clone()),
            source_trace.clone(),
          ))
        }
      }
    }
    Ok(is_associative)
  }
}

pub fn extract_metadata(
  exp: EaslTree,
) -> (EaslTree, Option<(Metadata, SourceTrace)>, Vec<CompileError>) {
  if let EaslTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::MetadataAnnotation)),
    mut children,
  ) = exp
  {
    let metadata_tree = children.remove(0);
    let metadata_source_trace: SourceTrace = metadata_tree.position().into();
    let exp = children.remove(0);
    match Metadata::from_metadata_tree(metadata_tree) {
      Ok(metadata) => {
        let (exp, inner_metadata, errors) = extract_metadata(exp);
        if let Some((inner_metadata, inner_metadata_source_trace)) =
          inner_metadata
        {
          (
            exp,
            Some((
              Metadata::Multiple(vec![metadata, inner_metadata]),
              metadata_source_trace.combine_with(inner_metadata_source_trace),
            )),
            errors,
          )
        } else {
          (exp, Some((metadata, metadata_source_trace)), errors)
        }
      }
      Err(err) => (exp, None, vec![err]),
    }
  } else {
    (exp, None, vec![])
  }
}
