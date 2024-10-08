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
  types::{GenericOr, Type, TypeState},
};

#[derive(Debug, Clone, PartialEq)]
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
    generic_args: &Vec<String>,
    structs: &Vec<AbstractStruct>,
  ) -> CompileResult<AbstractStructField> {
    Ok(AbstractStructField {
      metadata: self.metadata,
      name: self.name,
      field_type: if generic_args.contains(&self.field_type_name) {
        GenericOr::Generic(self.field_type_name)
      } else {
        GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::from_name(
          self.field_type_name,
          structs,
        )?))
      },
    })
  }
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
      name: self.name,
      fields: self
        .fields
        .into_iter()
        .map(|field| field.assign_type(&self.generic_args, structs))
        .collect::<CompileResult<Vec<AbstractStructField>>>()?,
      generic_args: self.generic_args,
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedStruct {
  pub name: String,
  pub fields: Vec<UntypedStructField>,
  pub generic_args: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractStructField {
  pub metadata: Option<Metadata>,
  pub name: String,
  pub field_type: GenericOr<TypeOrAbstractStruct>,
}

impl AbstractStructField {
  pub fn fill_generics(
    self,
    generics: &HashMap<String, TypeState>,
  ) -> StructField {
    StructField {
      metadata: self.metadata,
      name: self.name,
      field_type: match self.field_type {
        GenericOr::Generic(var_name) => generics
          .get(&var_name)
          .expect("unrecognized generic name in struct")
          .clone(),
        GenericOr::NonGeneric(type_or_struct) => {
          TypeState::Known(match type_or_struct {
            TypeOrAbstractStruct::Type(t) => t,
            TypeOrAbstractStruct::AbstractStruct(s) => {
              Type::Struct(s.fill_generics(generics))
            }
          })
        }
      },
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractStruct {
  pub name: String,
  pub fields: Vec<AbstractStructField>,
  pub generic_args: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeOrAbstractStruct {
  Type(Type),
  AbstractStruct(AbstractStruct),
}

impl AbstractStruct {
  pub fn fill_generics(self, generics: &HashMap<String, TypeState>) -> Struct {
    let new_fields: Vec<_> = self
      .fields
      .into_iter()
      .map(|field| field.fill_generics(generics))
      .collect();
    Struct {
      name: self.name,
      fields: new_fields,
    }
  }
  pub fn fill_generics_ordered(self, generics: Vec<TypeState>) -> Struct {
    let generics_map = self
      .generic_args
      .iter()
      .cloned()
      .zip(generics.into_iter())
      .collect();
    self.fill_generics(&generics_map)
  }
  pub fn fill_generics_with_unification_variables(self) -> Struct {
    let generic_count = self.generic_args.len();
    self.fill_generics_ordered(
      (0..generic_count)
        .into_iter()
        .map(|_| TypeState::fresh_unification_variable())
        .collect(),
    )
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
  pub fn compatible(&self, other: &Self) -> bool {
    self.fields.iter().zip(other.fields.iter()).fold(
      self.name == other.name,
      |compatible_so_far, (field, other_field)| {
        field.field_type.with_dereferenced(|typestate| {
          compatible_so_far
            && if let TypeState::Known(t) = typestate {
              other_field.field_type.with_dereferenced(|other_typestate| {
                if let TypeState::Known(other_t) = other_typestate {
                  t.compatible(other_t)
                } else {
                  true
                }
              })
            } else {
              true
            }
        })
      },
    )
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
