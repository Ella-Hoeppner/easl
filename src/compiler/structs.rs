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
  types::{Type, TypeState},
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
    structs: &Vec<Struct>,
  ) -> CompileResult<StructField> {
    Ok(StructField {
      metadata: self.metadata,
      name: self.name,
      field_type: TypeState::Known(Type::from_name(
        self.field_type_name,
        structs,
      )?),
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
  pub fn assign_types(self, structs: &Vec<Struct>) -> CompileResult<Struct> {
    Ok(Struct {
      name: self.name,
      fields: self
        .fields
        .into_iter()
        .map(|field| field.assign_type(structs))
        .collect::<CompileResult<Vec<StructField>>>()?,
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
  pub metadata: Option<Metadata>,
  pub name: String,
  pub field_type: TypeState,
}

impl StructField {
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
pub struct Struct {
  pub name: String,
  pub fields: Vec<StructField>,
}

impl Struct {
  pub fn fill_generics(
    mut self,
    generics: &HashMap<String, TypeState>,
  ) -> Self {
    self.fields = self
      .fields
      .into_iter()
      .map(|mut field| {
        if let TypeState::Known(Type::GenericVariable(var_name)) =
          field.field_type
        {
          field.field_type = generics
            .get(&var_name)
            .expect("unrecognized generic name in struct")
            .clone();
          field
        } else {
          field
        }
      })
      .collect();
    self
  }
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
