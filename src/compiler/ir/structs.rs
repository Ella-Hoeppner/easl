use sse::syntax::EncloserOrOperator;

use crate::parse::TyntTree;

use super::{
  metadata::ExpMetadata,
  types::{CompileError, TyntType},
};

pub struct UntypedStructField {
  metadata: Option<ExpMetadata>,
  name: String,
  field_type_name: String,
}

impl UntypedStructField {
  fn from_field_expression(exp: TyntTree) -> Result<Self, CompileError> {
    use crate::parse::Operator::*;
    use EncloserOrOperator::*;
    let mut metadata = None;
    let inner_exp = match exp {
      TyntTree::Inner((_, Operator(Metadata)), mut children) => {
        let field_exp = children.remove(1);
        metadata =
          Some(ExpMetadata::from_metadata_expression(children.remove(0))?);
        field_exp
      }
      other => other,
    };
    match inner_exp {
      TyntTree::Inner((_, Operator(TypeAnnotation)), mut children) => {
        if let TyntTree::Leaf(_, field_type_name) = children.remove(1) {
          if let TyntTree::Leaf(_, name) = children.remove(0) {
            Ok(Self {
              metadata,
              name,
              field_type_name,
            })
          } else {
            Err(CompileError::InvalidStructField)
          }
        } else {
          Err(CompileError::InvalidStructField)
        }
      }
      _ => Err(CompileError::InvalidStructField),
    }
  }
  pub fn assign_type(
    self,
    struct_names: &Vec<String>,
  ) -> Result<StructField, CompileError> {
    Ok(StructField {
      metadata: self.metadata,
      name: self.name,
      field_type: TyntType::from_name(self.field_type_name, struct_names)?,
    })
  }
}

pub struct UntypedStruct {
  fields: Vec<UntypedStructField>,
}

impl UntypedStruct {
  pub fn from_field_expressions(
    field_expressions: Vec<TyntTree>,
  ) -> Result<Self, CompileError> {
    Ok(Self {
      fields: field_expressions
        .into_iter()
        .map(UntypedStructField::from_field_expression)
        .collect::<Result<_, CompileError>>()?,
    })
  }
  pub fn assign_types(
    self,
    struct_names: &Vec<String>,
  ) -> Result<Struct, CompileError> {
    Ok(Struct {
      fields: self
        .fields
        .into_iter()
        .map(|field| field.assign_type(struct_names))
        .collect::<Result<_, CompileError>>()?,
    })
  }
}

pub struct StructField {
  metadata: Option<ExpMetadata>,
  name: String,
  field_type: TyntType,
}

pub struct Struct {
  fields: Vec<StructField>,
}
