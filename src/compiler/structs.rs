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
  types::TyntType,
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
    struct_names: &Vec<String>,
  ) -> CompileResult<StructField> {
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
  pub fn from_field_trees(
    name: String,
    field_asts: Vec<TyntTree>,
  ) -> CompileResult<Self> {
    Ok(Self {
      name,
      fields: field_asts
        .into_iter()
        .map(UntypedStructField::from_field_tree)
        .collect::<CompileResult<_>>()?,
    })
  }
  pub fn assign_types(
    self,
    struct_names: &Vec<String>,
  ) -> CompileResult<Struct> {
    Ok(Struct {
      has_normal_constructor: true,
      name: self.name,
      fields: self
        .fields
        .into_iter()
        .map(|field| field.assign_type(struct_names))
        .collect::<CompileResult<_>>()?,
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
  pub metadata: Option<Metadata>,
  pub name: String,
  pub field_type: TyntType,
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
  pub has_normal_constructor: bool,
  pub name: String,
  pub fields: Vec<StructField>,
}

impl Struct {
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
