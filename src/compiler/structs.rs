use std::collections::HashMap;

use crate::{
  compiler::{
    metadata::extract_metadata,
    types::extract_type_annotation_ast,
    util::{compile_word, read_leaf},
  },
  parse::TyntTree,
};

use super::{
  error::{CompileError, CompileErrorKind::*, CompileResult},
  metadata::Metadata,
  types::{Context, TyntType, TypeState},
};

pub struct UntypedStructField {
  metadata: Option<Metadata>,
  name: String,
  field_type_name: String,
}

impl UntypedStructField {
  fn from_field_tree(ast: TyntTree) -> CompileResult<Self> {
    let (type_ast, inner_ast) = extract_type_annotation_ast(ast)?;
    let type_ast =
      type_ast.ok_or(CompileError::from(StructFieldMissingType))?;
    let (metadata, name) = extract_metadata(inner_ast)?;
    Ok(Self {
      metadata,
      name: read_leaf(name)?,
      field_type_name: read_leaf(type_ast)?,
    })
  }
  pub fn assign_type(
    self,
    structs: &Vec<AbstractStruct>,
  ) -> CompileResult<AbstractStructField> {
    Ok(AbstractStructField {
      metadata: self.metadata,
      name: self.name,
      field_type: TyntType::from_name(self.field_type_name, structs)?,
    })
  }
}

pub struct UntypedStruct {
  pub name: String,
  pub fields: Vec<UntypedStructField>,
  pub generic_args: Vec<String>,
}

impl UntypedStruct {
  pub fn from_field_trees(
    name: String,
    generic_args: Vec<String>,
    field_asts: Vec<TyntTree>,
  ) -> CompileResult<Self> {
    Ok(Self {
      name,
      generic_args,
      fields: field_asts
        .into_iter()
        .map(UntypedStructField::from_field_tree)
        .collect::<CompileResult<_>>()?,
    })
  }
  pub fn assign_types(
    self,
    structs: &Vec<AbstractStruct>,
  ) -> CompileResult<AbstractStruct> {
    Ok(AbstractStruct {
      generic_args: self.generic_args,
      name: self.name,
      fields: self
        .fields
        .into_iter()
        .map(|field| field.assign_type(structs))
        .collect::<CompileResult<Vec<AbstractStructField>>>()?,
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractStructField {
  pub metadata: Option<Metadata>,
  pub name: String,
  pub field_type: TyntType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConcreteStructField {
  pub metadata: Option<Metadata>,
  pub name: String,
  pub field_type: TypeState,
}

impl ConcreteStructField {
  pub fn compile(self) -> String {
    let metadata = if let Some(metadata) = self.metadata {
      metadata.compile()
    } else {
      String::new()
    };
    let name = compile_word(self.name);
    let field_type = self.field_type.compile();
    format!("  {metadata}{name}: {field_type}")
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractStruct {
  pub name: String,
  pub fields: Vec<AbstractStructField>,
  pub generic_args: Vec<String>,
}

impl AbstractStruct {
  pub fn concretize(
    &self,
    generic_variables: &HashMap<String, TypeState>,
  ) -> ConcreteStruct {
    ConcreteStruct {
      name: self.name.clone(),
      fields: self
        .fields
        .iter()
        .map(|field| ConcreteStructField {
          metadata: field.metadata.clone(),
          name: field.name.clone(),
          field_type: field.field_type.concretize_abstract(generic_variables),
        })
        .collect(),
    }
  }
  pub fn is_concrete_struct_compatible(&self, s: ConcreteStruct) -> bool {
    todo!()
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConcreteStruct {
  pub name: String,
  pub fields: Vec<ConcreteStructField>,
}

impl ConcreteStruct {
  pub fn compile(self) -> String {
    let name = compile_word(self.name);
    let fields = self
      .fields
      .into_iter()
      .map(|field| field.compile())
      .collect::<Vec<String>>()
      .join("\n");
    format!("struct {name} {{\n{fields}\n}}")
  }
}
