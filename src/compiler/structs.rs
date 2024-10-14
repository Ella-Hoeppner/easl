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
  types::{AbstractType, GenericOr, Type, TypeState},
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
    skolems: &Vec<String>,
  ) -> CompileResult<AbstractStructField> {
    Ok(AbstractStructField {
      metadata: self.metadata,
      name: self.name,
      field_type: GenericOr::from_name(
        self.field_type_name,
        generic_args,
        structs,
        skolems,
      )?,
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
        .map(|field| field.assign_type(&self.generic_args, structs, &vec![]))
        .collect::<CompileResult<Vec<AbstractStructField>>>()?,
      generic_args: self.generic_args,
      filled_generics: vec![],
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
  pub fn concretize(
    &self,
    structs: &Vec<AbstractStruct>,
    skolems: &Vec<String>,
  ) -> CompileResult<StructField> {
    Ok(StructField {
      metadata: self.metadata.clone(),
      name: self.name.clone(),
      field_type: TypeState::Known(
        self.field_type.concretize(structs, skolems)?,
      ),
    })
  }
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
  fn fill_abstract_generics(self, generics: &Vec<(String, Type)>) -> Self {
    AbstractStructField {
      metadata: self.metadata,
      name: self.name,
      field_type: match self.field_type {
        GenericOr::Generic(var_name) => {
          AbstractType::NonGeneric(TypeOrAbstractStruct::Type(
            generics
              .iter()
              .find_map(|(name, t)| (*name == var_name).then(|| t))
              .expect("unrecognized generic name in struct")
              .clone(),
          ))
        }
        GenericOr::NonGeneric(type_or_struct) => {
          AbstractType::NonGeneric(match type_or_struct {
            TypeOrAbstractStruct::Type(t) => TypeOrAbstractStruct::Type(t),
            TypeOrAbstractStruct::AbstractStruct(s) => {
              TypeOrAbstractStruct::AbstractStruct(
                s.fill_abstract_generics_inner(generics),
              )
            }
          })
        }
      },
    }
  }
  pub fn compile(self) -> String {
    let metadata = if let Some(metadata) = self.metadata {
      metadata.compile()
    } else {
      String::new()
    };
    let name = compile_word(self.name);
    let field_type = match self.field_type {
      GenericOr::Generic(_) => {
        panic!("attempted to compile generic struct field")
      }
      GenericOr::NonGeneric(TypeOrAbstractStruct::Type(t)) => t.compile(),
      GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(t)) => t
        .compile_if_non_generic()
        .expect("failed to compile abstract structs"),
    };
    format!("  {metadata}{name}: {field_type}")
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractStruct {
  pub name: String,
  pub filled_generics: Vec<Type>,
  pub fields: Vec<AbstractStructField>,
  pub generic_args: Vec<String>,
}

impl AbstractStruct {
  pub fn concretize(
    &self,
    structs: &Vec<AbstractStruct>,
    skolems: &Vec<String>,
  ) -> CompileResult<Struct> {
    Ok(Struct {
      name: self.name.clone(),
      fields: self
        .fields
        .iter()
        .map(|f| f.concretize(structs, skolems))
        .collect::<CompileResult<Vec<_>>>()?,
    })
  }
  pub fn monomorphized_name(&self) -> String {
    self
      .filled_generics
      .iter()
      .fold(self.name.clone(), |name_so_far, t| {
        name_so_far + "_" + &t.compile()
      })
  }
  pub fn compile_if_non_generic(self) -> Option<String> {
    self.generic_args.is_empty().then(|| {
      let name = self.monomorphized_name();
      let fields = self
        .fields
        .into_iter()
        .map(|field| field.compile())
        .collect::<Vec<String>>()
        .join("\n");
      format!("struct {name} {{\n{fields}\n}}")
    })
  }
  pub fn generate_monomorphized(
    &self,
    field_types: Vec<Type>,
  ) -> Option<AbstractStruct> {
    if self.generic_args.is_empty() {
      return None;
    }
    let generic_args: Vec<Type> = self
      .generic_args
      .iter()
      .cloned()
      .map(|var| {
        let var = GenericOr::Generic(var);
        let first_usage_index = self
          .fields
          .iter()
          .enumerate()
          .find_map(|(index, field)| (field.field_type == var).then(|| index))
          .expect("unused generic variable");
        field_types[first_usage_index].clone()
      })
      .collect();
    Some(AbstractStruct {
      name: self.name.clone(),
      filled_generics: generic_args.clone(),
      generic_args: vec![],
      fields: self
        .fields
        .iter()
        .map(|field| {
          let mut new_field = field.clone();
          if let GenericOr::Generic(generic_var) = &new_field.field_type {
            new_field.field_type =
              GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
                generic_args[self
                  .generic_args
                  .iter()
                  .enumerate()
                  .find_map(|(index, generic_arg)| {
                    (generic_var == generic_arg).then(|| index)
                  })
                  .expect("unrecognized generic variable")]
                .clone(),
              ))
          }
          new_field
        })
        .collect(),
    })
  }
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

  //

  fn fill_abstract_generics_inner(
    self,
    generics: &Vec<(String, Type)>,
  ) -> AbstractStruct {
    let new_fields: Vec<_> = self
      .fields
      .into_iter()
      .map(|field| field.fill_abstract_generics(generics))
      .collect();
    AbstractStruct {
      name: self.name,
      fields: new_fields,
      filled_generics: generics.iter().map(|(_, t)| t.clone()).collect(),
      generic_args: vec![],
    }
  }
  pub fn fill_abstract_generics(self, generics: Vec<Type>) -> AbstractStruct {
    let generics_map: Vec<(String, Type)> = self
      .generic_args
      .iter()
      .cloned()
      .zip(generics.into_iter())
      .collect();
    self.fill_abstract_generics_inner(&generics_map)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeOrAbstractStruct {
  Type(Type),
  AbstractStruct(AbstractStruct),
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
}
