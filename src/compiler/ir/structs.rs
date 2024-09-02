use sse::syntax::EncloserOrOperator;

use crate::{compiler::ir::program::read_type_annotated_name, parse::TyntTree};

use super::{
  metadata::Metadata,
  types::{CompileError, TyntType},
};

pub struct UntypedStructField {
  metadata: Option<Metadata>,
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
          Some(Metadata::from_metadata_expression(children.remove(0))?);
        field_exp
      }
      other => other,
    };
    let (name, type_name) = read_type_annotated_name(inner_exp)?;
    Ok(Self {
      metadata,
      name,
      field_type_name: type_name,
    })
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
  pub name: String,
  pub fields: Vec<UntypedStructField>,
}

impl UntypedStruct {
  pub fn from_field_expressions(
    name: String,
    field_expressions: Vec<TyntTree>,
  ) -> Result<Self, CompileError> {
    Ok(Self {
      name,
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
      name: self.name,
      fields: self
        .fields
        .into_iter()
        .map(|field| field.assign_type(struct_names))
        .collect::<Result<_, CompileError>>()?,
    })
  }
}

pub struct StructField {
  metadata: Option<Metadata>,
  name: String,
  field_type: TyntType,
}

pub struct Struct {
  name: String,
  fields: Vec<StructField>,
}
