use sse::syntax::EncloserOrOperator::{self, *};

use crate::parse::TyntTree;
use crate::parse::{Encloser::*, Operator};

use super::error::CompileError;

#[derive(Debug, Clone, PartialEq)]
pub enum Metadata {
  Singular(String),
  Map(Vec<(String, String)>),
}

impl Metadata {
  pub fn from_metadata_tree(ast: TyntTree) -> Result<Self, CompileError> {
    match ast {
      TyntTree::Leaf(_, singular) => Ok(Self::Singular(singular)),
      TyntTree::Inner((_, Encloser(Curly)), map_fields) => {
        if map_fields.len() % 2 == 0 {
          Ok(Self::Map(
            map_fields
              .into_iter()
              .map(|field| {
                if let TyntTree::Leaf(_, field_string) = field {
                  Ok(field_string)
                } else {
                  Err(CompileError::InvalidMetadata(
                    "fields must all be leaves".to_string(),
                  ))
                }
              })
              .collect::<Result<Vec<String>, CompileError>>()?
              .chunks(2)
              .map(|arr| (arr[0].clone(), arr[1].clone()))
              .collect(),
          ))
        } else {
          Err(CompileError::InvalidMetadata(
            "fields must all be leaves".to_string(),
          ))
        }
      }
      _ => Err(CompileError::InvalidMetadata(
        "fields must all be leaves".to_string(),
      )),
    }
  }
}

pub fn extract_metadata(
  exp: TyntTree,
) -> Result<(Option<Metadata>, TyntTree), CompileError> {
  Ok(
    if let TyntTree::Inner(
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
