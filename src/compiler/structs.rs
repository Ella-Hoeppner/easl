use std::{
  collections::{HashMap, HashSet},
  rc::Rc,
};

use crate::{
  compiler::{
    annotation::extract_annotation,
    entry::IOAttributes,
    error::err,
    expression::{Accessor, ExpKind, Number, TypedExp},
    functions::is_vec_name,
    program::{NameContext, TypeDefs},
    types::{ArraySize, contains_name_leaf, extract_type_annotation_ast},
    util::{compile_word, read_leaf},
  },
  parse::EaslTree,
};

use super::{
  error::{
    CompileError, CompileErrorKind::*, CompileResult, ErrorLog, SourceTrace,
  },
  types::{AbstractType, ExpTypeInfo, Type, TypeState},
};

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedStructField {
  attributes: IOAttributes,
  name: Rc<str>,
  type_ast: EaslTree,
  source_trace: SourceTrace,
}

impl UntypedStructField {
  fn from_field_tree(ast: EaslTree, errors: &mut ErrorLog) -> Option<Self> {
    let source_trace = ast.position().clone().into();
    let path = ast.position().clone();
    let (type_ast, inner_ast) = extract_type_annotation_ast(ast);
    let Some(type_ast) = type_ast else {
      errors.log(CompileError::new(StructFieldMissingType, path.into()));
      return None;
    };
    let mut errors = ErrorLog::new();
    let (name_ast, annotation) = extract_annotation(inner_ast, &mut errors);
    let name_source = name_ast.position().into();
    let name = match read_leaf(name_ast) {
      Ok(name) => name,
      Err(e) => {
        errors.log(e);
        return None;
      }
    };
    let (attributes, leftovers) = if let Some(annotation) = annotation {
      IOAttributes::parse_from_annotation(
        annotation,
        Some((name.clone(), name_source)),
        &mut errors,
      )
    } else {
      (IOAttributes::empty(path.into()), vec![])
    };
    if !leftovers.is_empty() {
      let mut source_trace = leftovers[0].1.clone();
      if let Some((_, secondary_source)) = &leftovers[0].2 {
        source_trace =
          source_trace.insert_as_secondary(secondary_source.clone());
      }
      for (_, secondary_source, value) in &leftovers[1..] {
        source_trace =
          source_trace.insert_as_secondary(secondary_source.clone());
        if let Some((_, secondary_source)) = value {
          source_trace =
            source_trace.insert_as_secondary(secondary_source.clone());
        }
      }
      errors.log(CompileError::new(
        InvalidStructFieldAnnotation,
        source_trace,
      ));
    }
    Some(Self {
      attributes,
      name,
      type_ast,
      source_trace,
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
      attributes: self.attributes,
      name: self.name,
      field_type: AbstractType::from_easl_tree(
        self.type_ast,
        typedefs,
        skolems,
      )?,
      source_trace: self.source_trace,
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedStruct {
  pub name: (Rc<str>, SourceTrace),
  pub fields: Vec<UntypedStructField>,
  pub generic_args: Vec<(Rc<str>, SourceTrace)>,
  pub source_trace: SourceTrace,
}

impl UntypedStruct {
  pub fn from_field_trees(
    name: (Rc<str>, SourceTrace),
    generic_args: Vec<(Rc<str>, SourceTrace)>,
    field_asts: Vec<EaslTree>,
    source_trace: SourceTrace,
    errors: &mut ErrorLog,
  ) -> Self {
    Self {
      name,
      generic_args,
      fields: field_asts
        .into_iter()
        .filter_map(|field| UntypedStructField::from_field_tree(field, errors))
        .collect(),
      source_trace,
    }
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
        .map(|field| {
          field.assign_type(
            typedefs,
            &self.generic_args.iter().map(|(n, _)| n.clone()).collect(),
          )
        })
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
    Type::Bool => Some("<bool>"),
    _ => None,
  }
  .map(|suffix| format!("{base_struct_name}{suffix}").into())
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractStructField {
  pub attributes: IOAttributes,
  pub name: Rc<str>,
  pub field_type: AbstractType,
  pub source_trace: SourceTrace,
}

impl AbstractStructField {
  pub fn concretize(
    &self,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
    source_trace: SourceTrace,
  ) -> CompileResult<StructField> {
    Ok(StructField {
      attributes: self.attributes.clone(),
      name: Rc::clone(&self.name),
      field_type: self
        .field_type
        .concretize(skolems, typedefs, source_trace)?
        .known()
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
      attributes: self.attributes.clone(),
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
      attributes: self.attributes,
      name: self.name,
      field_type: self.field_type.fill_abstract_generics(generics),
      source_trace: self.source_trace.clone(),
    }
  }
  pub fn compile(
    self,
    typedefs: &TypeDefs,
    names: &mut NameContext,
  ) -> CompileResult<String> {
    let annotation = self.attributes.compile();
    let name = compile_word(self.name);
    let field_type =
      self
        .field_type
        .compile(typedefs, names, &self.source_trace)?;
    Ok(format!("  {annotation}{name}: {field_type}"))
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
  pub name: (Rc<str>, SourceTrace),
  pub filled_generics: HashMap<Rc<str>, AbstractType>,
  pub fields: Vec<AbstractStructField>,
  pub generic_args: Vec<(Rc<str>, SourceTrace)>,
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
  pub fn is_unitlike(&self, names: &mut NameContext) -> bool {
    !self.fields.iter().any(|f| !f.field_type.is_unitlike(names))
  }
  pub fn concretize(
    s: Rc<Self>,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
    source_trace: SourceTrace,
  ) -> CompileResult<Struct> {
    Ok(Struct {
      name: Rc::clone(&s.name.0),
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
    names: &mut NameContext,
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
        let monomorphized_name =
          compile_word(self.monomorphized_name(&field_types, names));
        let fields = self
          .fields
          .into_iter()
          .map(|field| field.compile(typedefs, names))
          .collect::<CompileResult<Vec<String>>>()?
          .join(",\n");
        Ok(format!("struct {monomorphized_name} {{\n{fields}\n}}"))
      })
      .map_or(Ok(None), |v| v.map(Some))
  }
  pub fn monomorphized_name(
    &self,
    field_types: &Vec<Type>,
    names: &mut NameContext,
  ) -> Rc<str> {
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

    let name = &*self.name.0;

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
          .fold(
            self.name.0.to_string(),
            |name_so_far, (generic_arg_name, _)| {
              name_so_far
                + "_"
                + &generic_bindings
                  .get(generic_arg_name)
                  .unwrap()
                  .monomorphized_name(names)
            },
          )
          .into()
      })
      .into()
  }
  pub fn concretized_name(
    s: Rc<Self>,
    typedefs: &TypeDefs,
    names: &mut NameContext,
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
        names,
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
      .map(|(var, _)| {
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
        .map(|(t, (name, _))| (name, AbstractType::Type(t.clone())))
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
                .find_map(|(index, (generic_arg, _))| {
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
      name: s.name.0.clone(),
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
    if s.generic_args.len() != generics.len() {
      return err(
        WrongNumberOfGenericArguments(s.generic_args.len(), generics.len()),
        source_trace,
      );
    }
    let generics_map = s
      .generic_args
      .iter()
      .map(|(n, _)| n)
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
        .filter(|(name, _)| generics.contains_key(name))
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
      .map(|(n, _)| n)
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
  pub fn is_vec(&self) -> bool {
    is_vec_name(&*self.name.0)
  }
  pub fn is_vec4f(&self) -> bool {
    &*self.name.0 == "vec4"
      && self
        .fields
        .get(0)
        .map(|f| f.field_type == AbstractType::Type(Type::F32))
        .unwrap_or(false)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
  pub attributes: IOAttributes,
  pub name: Rc<str>,
  pub field_type: ExpTypeInfo,
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
  pub fn monomorphized_name(&self, names: &mut NameContext) -> Rc<str> {
    self.abstract_ancestor.monomorphized_name(
      &self
        .fields
        .iter()
        .map(|f| f.field_type.unwrap_known())
        .collect(),
      names,
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
                    data: Type::U32.known().into(),
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
        Type::Enum(e) => {
          let data_array_size = e.inner_data_size_in_u32s().unwrap();
          chunks.append(
            &mut std::iter::once(TypedExp {
              data: Type::U32.known().into(),
              kind: ExpKind::Access(
                Accessor::Field("discriminant".into()),
                access.clone().into(),
              ),
              source_trace: SourceTrace::empty(),
            })
            .chain((0..data_array_size).map(|i| {
              TypedExp {
                data: Type::U32.known().into(),
                kind: ExpKind::Application(
                  TypedExp {
                    data: Type::Array(
                      Some(ArraySize::Literal(data_array_size as u32)),
                      Box::new(Type::U32.known().into()),
                    )
                    .known()
                    .into(),
                    kind: ExpKind::Access(
                      Accessor::Field("data".into()),
                      access.clone().into(),
                    ),
                    source_trace: SourceTrace::empty(),
                  }
                  .into(),
                  vec![TypedExp {
                    data: Type::U32.known().into(),
                    kind: ExpKind::NumberLiteral(Number::Int(i as i64)),
                    source_trace: SourceTrace::empty(),
                  }],
                ),
                source_trace: SourceTrace::empty(),
              }
            }))
            .collect(),
          )
        }
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
        data: Type::Struct(self.clone()).known().into(),
        kind: ExpKind::Name(value_name.clone()),
        source_trace: SourceTrace::empty(),
      },
      &mut chunks,
    );
    chunks
  }
}
