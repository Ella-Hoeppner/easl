use std::{
  collections::{HashMap, HashSet},
  rc::Rc,
};

use crate::{
  compiler::{
    expression::{Accessor, ExpKind, Number, TypedExp},
    metadata::extract_metadata,
    program::TypeDefs,
    types::{ArraySize, contains_name_leaf, extract_type_annotation_ast},
    util::{compile_word, read_leaf},
  },
  parse::EaslTree,
};

use super::{
  error::{
    CompileError, CompileErrorKind::*, CompileResult, ErrorLog, SourceTrace,
  },
  metadata::Metadata,
  types::{AbstractType, ExpTypeInfo, Type, TypeState},
};

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedStructField {
  metadata: Option<Metadata>,
  name: Rc<str>,
  type_ast: EaslTree,
}

impl UntypedStructField {
  fn from_field_tree(ast: EaslTree) -> CompileResult<Self> {
    let path = ast.position().clone();
    let (type_ast, inner_ast) = extract_type_annotation_ast(ast);
    let type_ast =
      type_ast.ok_or(CompileError::new(StructFieldMissingType, path.into()))?;
    let mut errors = ErrorLog::new();
    let (name, metadata) = extract_metadata(inner_ast, &mut errors);
    if let Some(e) = errors.into_iter().next() {
      return Err(e.clone());
    }
    Ok(Self {
      metadata: metadata.map(|(a, _)| a),
      name: read_leaf(name)?,
      type_ast,
    })
  }
  pub fn references_type_name(&self, name: &Rc<str>) -> bool {
    contains_name_leaf(&name, &self.type_ast)
  }
  pub fn assign_type(
    self,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
  ) -> CompileResult<AbstractStructField> {
    Ok(AbstractStructField {
      metadata: self.metadata,
      name: self.name,
      field_type: AbstractType::from_easl_tree(
        self.type_ast,
        typedefs,
        skolems,
      )?,
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedStruct {
  pub name: Rc<str>,
  pub fields: Vec<UntypedStructField>,
  pub generic_args: Vec<Rc<str>>,
  source_trace: SourceTrace,
}

impl UntypedStruct {
  pub fn from_field_trees(
    name: Rc<str>,
    generic_args: Vec<Rc<str>>,
    field_asts: Vec<EaslTree>,
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
  pub fn references_type_name(&self, name: &Rc<str>) -> bool {
    self
      .fields
      .iter()
      .fold(false, |acc, field| acc || field.references_type_name(name))
  }
  pub fn assign_types(
    self,
    typedefs: &TypeDefs,
  ) -> CompileResult<AbstractStruct> {
    Ok(AbstractStruct {
      name: self.name,
      fields: self
        .fields
        .into_iter()
        .map(|field| field.assign_type(typedefs, &self.generic_args))
        .collect::<CompileResult<Vec<AbstractStructField>>>()?,
      generic_args: self.generic_args.clone(),
      filled_generics: HashMap::new(),
      abstract_ancestor: None,
      source_trace: self.source_trace,
    })
  }
}

pub fn compiled_vec_or_mat_name(
  base_struct_name: &str,
  inner_type: Type,
) -> Option<Rc<str>> {
  match inner_type {
    Type::F32 => Some("f"),
    Type::I32 => Some("i"),
    Type::U32 => Some("u"),
    _ => None,
  }
  .map(|suffix| format!("{base_struct_name}{}", suffix).into())
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractStructField {
  pub metadata: Option<Metadata>,
  pub name: Rc<str>,
  pub field_type: AbstractType,
}

impl AbstractStructField {
  pub fn concretize(
    &self,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
    source_trace: SourceTrace,
  ) -> CompileResult<StructField> {
    Ok(StructField {
      metadata: self.metadata.clone(),
      name: Rc::clone(&self.name),
      field_type: TypeState::Known(self.field_type.concretize(
        skolems,
        typedefs,
        source_trace,
      )?)
      .into(),
    })
  }
  pub fn fill_generics(
    &self,
    generics: &HashMap<Rc<str>, ExpTypeInfo>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<StructField> {
    Ok(StructField {
      metadata: self.metadata.clone(),
      name: self.name.clone(),
      field_type: self.field_type.fill_generics(
        generics,
        typedefs,
        source_trace,
      )?,
    })
  }
  fn fill_abstract_generics(
    self,
    generics: &HashMap<Rc<str>, AbstractType>,
  ) -> Self {
    AbstractStructField {
      metadata: self.metadata,
      name: self.name,
      field_type: self.field_type.fill_abstract_generics(generics),
    }
  }
  pub fn compile(self, typedefs: &TypeDefs) -> CompileResult<String> {
    let metadata = if let Some(metadata) = self.metadata {
      metadata.compile()
    } else {
      String::new()
    };
    let name = compile_word(self.name);
    let field_type = self.field_type.compile(typedefs)?;
    Ok(format!("  {metadata}{name}: {field_type}"))
  }
}

pub fn vec_and_mat_compile_names() -> HashSet<String> {
  (2..=4)
    .flat_map(|n| {
      std::iter::once(format!("vec{n}"))
        .chain((2..=4).map(move |m| format!("mat{n}x{m}")))
    })
    .collect()
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractStruct {
  pub name: Rc<str>,
  pub filled_generics: HashMap<Rc<str>, AbstractType>,
  pub fields: Vec<AbstractStructField>,
  pub generic_args: Vec<Rc<str>>,
  pub abstract_ancestor: Option<Rc<Self>>,
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
    s: Rc<Self>,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
    source_trace: SourceTrace,
  ) -> CompileResult<Struct> {
    Ok(Struct {
      name: Rc::clone(&s.name),
      fields: s
        .fields
        .iter()
        .map(|f| f.concretize(typedefs, skolems, source_trace.clone()))
        .collect::<CompileResult<Vec<_>>>()?,
      abstract_ancestor: s,
    })
  }
  pub fn compile_if_non_generic(
    self,
    typedefs: &TypeDefs,
  ) -> CompileResult<Option<String>> {
    self
      .generic_args
      .is_empty()
      .then(|| {
        let field_types: Vec<Type> = self
          .fields
          .iter()
          .map(|f| {
            f.field_type.concretize(
              &vec![],
              typedefs,
              self.source_trace.clone(),
            )
          })
          .collect::<CompileResult<Vec<Type>>>()?;
        let monomorphized_name = self.monomorphized_name(&field_types);
        let fields = self
          .fields
          .into_iter()
          .map(|field| field.compile(typedefs))
          .collect::<CompileResult<Vec<String>>>()?
          .join(",\n");
        Ok(format!("struct {monomorphized_name} {{\n{fields}\n}}"))
      })
      .map_or(Ok(None), |v| v.map(Some))
  }
  pub fn monomorphized_name(&self, field_types: &Vec<Type>) -> Rc<str> {
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

    let name = &*self.name;

    vec_and_mat_compile_names()
      .contains(name)
      .then(|| {
        compiled_vec_or_mat_name(
          name,
          generic_bindings.values().next().unwrap().clone(),
        )
      })
      .flatten()
      .unwrap_or_else(|| {
        self
          .original_ancestor()
          .generic_args
          .iter()
          .fold(self.name.to_string(), |name_so_far, generic_arg_name| {
            name_so_far
              + "_"
              + &generic_bindings.get(generic_arg_name).unwrap().compile()
          })
          .into()
      })
      .into()
  }
  pub fn concretized_name(
    s: Rc<Self>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<Rc<str>> {
    let concretized =
      Self::concretize(s.clone(), typedefs, &vec![], source_trace)?;
    Ok(
      s.monomorphized_name(
        &concretized
          .fields
          .iter()
          .map(|f| f.field_type.unwrap_known())
          .collect(),
      ),
    )
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
        let var = AbstractType::Generic(var);
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
      filled_generics: generic_args
        .iter()
        .zip(self.generic_args.iter().cloned())
        .map(|(t, name)| (name, AbstractType::Type(t.clone())))
        .collect(),
      generic_args: vec![],
      fields: self
        .fields
        .iter()
        .map(|field| {
          let mut new_field = field.clone();
          if let AbstractType::Generic(generic_var) = &new_field.field_type {
            new_field.field_type = AbstractType::Type(
              generic_args[self
                .generic_args
                .iter()
                .enumerate()
                .find_map(|(index, generic_arg)| {
                  (generic_var == generic_arg).then(|| index)
                })
                .expect("unrecognized generic variable")]
              .clone(),
            )
          }
          new_field
        })
        .collect(),
      abstract_ancestor: Some(self.clone().into()),
      source_trace: self.source_trace.clone(),
    })
  }
  pub fn fill_generics(
    s: Rc<Self>,
    generics: &HashMap<Rc<str>, ExpTypeInfo>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<Struct> {
    let new_fields = s
      .fields
      .iter()
      .map(|field| {
        field.fill_generics(generics, typedefs, source_trace.clone())
      })
      .collect::<CompileResult<Vec<_>>>()?;
    Ok(Struct {
      name: s.name.clone(),
      abstract_ancestor: s,
      fields: new_fields,
    })
  }
  pub fn fill_generics_ordered(
    s: Rc<Self>,
    generics: Vec<ExpTypeInfo>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<Struct> {
    let generics_map = s
      .generic_args
      .iter()
      .cloned()
      .zip(generics.into_iter())
      .collect();
    Self::fill_generics(s, &generics_map, typedefs, source_trace)
  }
  pub fn fill_generics_with_unification_variables(
    s: Rc<Self>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<Struct> {
    let generic_count = s.generic_args.len();
    Self::fill_generics_ordered(
      s,
      (0..generic_count)
        .into_iter()
        .map(|_| TypeState::fresh_unification_variable().into())
        .collect(),
      typedefs,
      source_trace,
    )
  }
  pub fn partially_fill_abstract_generics(
    self,
    generics: HashMap<Rc<str>, AbstractType>,
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
    let generics_map: HashMap<Rc<str>, AbstractType> = self
      .generic_args
      .iter()
      .cloned()
      .zip(generics.into_iter())
      .collect();
    self.partially_fill_abstract_generics(generics_map)
  }
  pub fn extract_generic_bindings(
    &self,
    concrete_struct: &Struct,
    generic_bindings: &mut HashMap<Rc<str>, Type>,
  ) {
    for i in 0..concrete_struct.fields.len() {
      self.fields[i].field_type.extract_generic_bindings(
        &concrete_struct.fields[i].field_type.unwrap_known(),
        generic_bindings,
      );
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
  pub metadata: Option<Metadata>,
  pub name: Rc<str>,
  pub field_type: ExpTypeInfo,
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
  pub name: Rc<str>,
  pub fields: Vec<StructField>,
  pub abstract_ancestor: Rc<AbstractStruct>,
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
  pub fn monomorphized_name(&self) -> Rc<str> {
    self.abstract_ancestor.monomorphized_name(
      &self
        .fields
        .iter()
        .map(|f| f.field_type.unwrap_known())
        .collect(),
    )
  }
  fn bitcastable_chunk_accessors_inner(
    &self,
    value: TypedExp,
    chunks: &mut Vec<TypedExp>,
  ) {
    for f in self.fields.iter() {
      let access = TypedExp {
        data: f.field_type.clone(),
        kind: ExpKind::Access(
          Accessor::Field(Rc::clone(&f.name)),
          value.clone().into(),
        ),
        source_trace: SourceTrace::empty(),
      };
      match f.field_type.unwrap_known() {
        Type::F32 | Type::I32 | Type::U32 | Type::Bool => chunks.push(access),
        Type::Struct(s) => s.bitcastable_chunk_accessors_inner(access, chunks),
        Type::Array(array_size, inner_type) => match array_size {
          Some(ArraySize::Literal(n)) => {
            for i in 0..n {
              chunks.push(TypedExp {
                data: *inner_type.clone(),
                kind: ExpKind::Application(
                  access.clone().into(),
                  vec![TypedExp {
                    data: TypeState::Known(Type::U32).into(),
                    kind: ExpKind::NumberLiteral(Number::Int(i as i64)),
                    source_trace: SourceTrace::empty(),
                  }],
                ),
                source_trace: SourceTrace::empty(),
              });
            }
          }
          Some(ArraySize::Unsized | ArraySize::Constant(_)) | None => {
            panic!("called bitcastable_chunk_accessors on unsized Array")
          }
        },
        Type::Unit => {}
        Type::Enum(_) => todo!("enum"),
        _ => {
          panic!("called bitcastable_chunk_accessors on invalid type")
        }
      }
    }
  }
  pub fn bitcastable_chunk_accessors(
    &self,
    value_name: Rc<str>,
  ) -> Vec<TypedExp> {
    let mut chunks = vec![];
    self.bitcastable_chunk_accessors_inner(
      TypedExp {
        data: TypeState::Known(Type::Struct(self.clone())).into(),
        kind: ExpKind::Name(value_name.clone()),
        source_trace: SourceTrace::empty(),
      },
      &mut chunks,
    );
    chunks
  }
}
