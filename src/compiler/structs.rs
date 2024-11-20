use std::{collections::HashMap, hash::Hash};

use crate::{
  compiler::{
    metadata::extract_metadata,
    types::extract_type_annotation_ast,
    util::{compile_word, read_leaf},
  },
  parse::TyntTree,
};

use super::{
  error::{CompileError, CompileErrorKind::*, CompileResult, SourceTrace},
  metadata::Metadata,
  types::{AbstractType, GenericOr, Type, TypeState},
};

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedStructField {
  metadata: Option<Metadata>,
  name: String,
  type_ast: TyntTree,
}

impl UntypedStructField {
  fn from_field_tree(ast: TyntTree) -> CompileResult<Self> {
    let path = ast.position().path.clone();
    let (type_ast, inner_ast) = extract_type_annotation_ast(ast)?;
    let type_ast =
      type_ast.ok_or(CompileError::new(StructFieldMissingType, path.into()))?;
    let (metadata, name) = extract_metadata(inner_ast)?;
    Ok(Self {
      metadata,
      name: read_leaf(name)?,
      type_ast,
    })
  }
  pub fn assign_type(
    self,
    structs: &Vec<AbstractStruct>,
    aliases: &Vec<(String, AbstractStruct)>,
    generic_args: &Vec<String>,
    skolems: &Vec<String>,
  ) -> CompileResult<AbstractStructField> {
    Ok(AbstractStructField {
      metadata: self.metadata,
      name: self.name,
      field_type: GenericOr::from_ast(
        self.type_ast,
        structs,
        aliases,
        generic_args,
        skolems,
      )?,
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedStruct {
  pub name: String,
  pub fields: Vec<UntypedStructField>,
  pub generic_args: Vec<String>,
  source_trace: SourceTrace,
}

impl UntypedStruct {
  pub fn from_field_trees(
    name: String,
    generic_args: Vec<String>,
    field_asts: Vec<TyntTree>,
    source_trace: SourceTrace,
  ) -> CompileResult<Self> {
    Ok(Self {
      name,
      generic_args,
      fields: field_asts
        .into_iter()
        .map(UntypedStructField::from_field_tree)
        .collect::<CompileResult<_>>()?,
      source_trace,
    })
  }
  pub fn assign_types(
    self,
    structs: &Vec<AbstractStruct>,
    aliases: &Vec<(String, AbstractStruct)>,
  ) -> CompileResult<AbstractStruct> {
    Ok(AbstractStruct {
      name: self.name,
      fields: self
        .fields
        .into_iter()
        .map(|field| {
          field.assign_type(structs, aliases, &self.generic_args, &vec![])
        })
        .collect::<CompileResult<Vec<AbstractStructField>>>()?,
      generic_args: self.generic_args.clone(),
      filled_generics: HashMap::new(),
      abstract_ancestor: None,
      source_trace: self.source_trace,
    })
  }
}

pub fn compiled_vec_name(
  base_struct_name: &str,
  inner_type: Type,
) -> Option<String> {
  match inner_type {
    Type::F32 => Some("f"),
    Type::I32 => Some("i"),
    Type::U32 => Some("u"),
    _ => None,
  }
  .map(|suffix| format!("{base_struct_name}{}", suffix))
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
    source_trace: SourceTrace,
  ) -> CompileResult<StructField> {
    Ok(StructField {
      metadata: self.metadata.clone(),
      name: self.name.clone(),
      field_type: TypeState::Known(self.field_type.concretize(
        structs,
        skolems,
        source_trace,
      )?),
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
  fn fill_abstract_generics(
    self,
    generics: &HashMap<String, AbstractType>,
  ) -> Self {
    AbstractStructField {
      metadata: self.metadata,
      name: self.name,
      field_type: match self.field_type {
        GenericOr::Generic(var_name) => generics
          .iter()
          .find_map(|(name, t)| (*name == var_name).then(|| t))
          .expect("unrecognized generic name in struct")
          .clone(),
        GenericOr::NonGeneric(type_or_struct) => {
          AbstractType::NonGeneric(match type_or_struct {
            TypeOrAbstractStruct::Type(t) => TypeOrAbstractStruct::Type(t),
            TypeOrAbstractStruct::AbstractStruct(s) => {
              TypeOrAbstractStruct::AbstractStruct(
                s.partially_fill_abstract_generics(generics.clone()),
              )
            }
          })
        }
      },
    }
  }
  pub fn compile(self, structs: &Vec<AbstractStruct>) -> CompileResult<String> {
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
        .compile_if_non_generic(structs)?
        .expect("failed to compile abstract structs"),
    };
    Ok(format!("  {metadata}{name}: {field_type}"))
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractStruct {
  pub name: String,
  pub filled_generics: HashMap<String, AbstractType>,
  pub fields: Vec<AbstractStructField>,
  pub generic_args: Vec<String>,
  pub abstract_ancestor: Option<Box<Self>>,
  pub source_trace: SourceTrace,
}

impl AbstractStruct {
  pub fn original_ancestor(&self) -> &Self {
    &self
      .abstract_ancestor
      .as_ref()
      .map(|ancestor| ancestor.original_ancestor())
      .unwrap_or(&self)
  }
  pub fn concretize(
    &self,
    structs: &Vec<AbstractStruct>,
    skolems: &Vec<String>,
    source_trace: SourceTrace,
  ) -> CompileResult<Struct> {
    Ok(Struct {
      abstract_ancestor: self.clone(),
      name: self.name.clone(),
      fields: self
        .fields
        .iter()
        .map(|f| f.concretize(structs, skolems, source_trace.clone()))
        .collect::<CompileResult<Vec<_>>>()?,
    })
  }
  pub fn compile_if_non_generic(
    self,
    structs: &Vec<AbstractStruct>,
  ) -> CompileResult<Option<String>> {
    self
      .generic_args
      .is_empty()
      .then(|| {
        let field_types: Vec<Type> = self
          .fields
          .iter()
          .map(|f| {
            f.field_type
              .concretize(structs, &vec![], self.source_trace.clone())
          })
          .collect::<CompileResult<Vec<Type>>>()?;
        let monomorphized_name = self.monomorphized_name(&field_types);
        let fields = self
          .fields
          .into_iter()
          .map(|field| field.compile(structs))
          .collect::<CompileResult<Vec<String>>>()?
          .join(",\n");
        Ok(format!("struct {monomorphized_name} {{\n{fields}\n}}"))
      })
      .map_or(Ok(None), |v| v.map(Some))
  }
  pub fn monomorphized_name(&self, field_types: &Vec<Type>) -> String {
    let mut generic_bindings = HashMap::new();
    for (field, field_type) in self
      .original_ancestor()
      .fields
      .iter()
      .zip(field_types.iter())
    {
      field
        .field_type
        .extract_generic_bindings(field_type, &mut generic_bindings);
    }

    let name = &self.name;
    generic_bindings
      .get("T")
      .map(|t| {
        (name == "vec2" || name == "vec3" || name == "vec4")
          .then(|| compiled_vec_name(name, t.clone()))
          .flatten()
      })
      .flatten()
      .unwrap_or_else(|| {
        self.original_ancestor().generic_args.iter().fold(
          self.name.clone(),
          |name_so_far, generic_arg_name| {
            name_so_far
              + "_"
              + &generic_bindings.get(generic_arg_name).unwrap().compile()
          },
        )
      })
  }
  pub fn concretized_name(
    &self,
    structs: &Vec<AbstractStruct>,
    source_trace: SourceTrace,
  ) -> CompileResult<String> {
    let s = self.concretize(structs, &vec![], source_trace)?;
    Ok(
      self.monomorphized_name(
        &s.fields
          .iter()
          .map(|f| f.field_type.unwrap_known())
          .collect(),
      ),
    )
  }
  pub fn generate_monomorphized(
    &self,
    field_types: Vec<Type>,
    source_trace: SourceTrace,
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
    source_trace.combine_with(self.source_trace.clone());
    Some(AbstractStruct {
      name: self.name.clone(),
      filled_generics: generic_args
        .iter()
        .zip(self.generic_args.iter().cloned())
        .map(|(t, name)| {
          (
            name,
            AbstractType::NonGeneric(TypeOrAbstractStruct::Type(t.clone())),
          )
        })
        .collect(),
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
      abstract_ancestor: Some(self.clone().into()),
      source_trace: self.source_trace.clone(),
    })
  }
  pub fn fill_generics(self, generics: &HashMap<String, TypeState>) -> Struct {
    let abstract_ancestor = self.clone();
    let new_fields: Vec<_> = self
      .fields
      .into_iter()
      .map(|field| field.fill_generics(generics))
      .collect();
    Struct {
      abstract_ancestor,
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

  pub fn partially_fill_abstract_generics(
    self,
    generics: HashMap<String, AbstractType>,
  ) -> AbstractStruct {
    let abstract_ancestor = self.clone().into();
    AbstractStruct {
      name: self.name.clone(),
      generic_args: self
        .generic_args
        .into_iter()
        .filter(|name| generics.contains_key(name))
        .collect(),
      fields: self
        .fields
        .iter()
        .map(|field| field.clone().fill_abstract_generics(&generics))
        .collect(),
      filled_generics: self
        .filled_generics
        .into_iter()
        .chain(generics.into_iter())
        .collect(),
      abstract_ancestor: Some(abstract_ancestor),
      source_trace: self.source_trace,
    }
  }
  pub fn fill_abstract_generics(
    self,
    generics: Vec<AbstractType>,
  ) -> AbstractStruct {
    let generics_map: HashMap<String, AbstractType> = self
      .generic_args
      .iter()
      .cloned()
      .zip(generics.into_iter())
      .collect();
    self.partially_fill_abstract_generics(generics_map)
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
  pub abstract_ancestor: AbstractStruct,
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
  pub fn monomorphized_name(&self) -> String {
    self.abstract_ancestor.monomorphized_name(
      &self
        .fields
        .iter()
        .map(|f| f.field_type.unwrap_known())
        .collect(),
    )
  }
}
