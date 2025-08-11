use std::{
  collections::{HashMap, HashSet},
  rc::Rc,
};

use crate::{
  compiler::{
    metadata::extract_metadata,
    types::extract_type_annotation_ast,
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
    fn contains_name_leaf(name: &Rc<str>, tree: &EaslTree) -> bool {
      match &tree {
        EaslTree::Leaf(_, leaf) => leaf == &**name,
        EaslTree::Inner(_, children) => children
          .iter()
          .fold(false, |acc, child| acc || contains_name_leaf(name, child)),
      }
    }
    contains_name_leaf(&name, &self.type_ast)
  }
  pub fn assign_type(
    self,
    structs: &Vec<Rc<AbstractStruct>>,
    aliases: &Vec<(Rc<str>, Rc<AbstractStruct>)>,
    skolems: &Vec<Rc<str>>,
  ) -> CompileResult<AbstractStructField> {
    Ok(AbstractStructField {
      metadata: self.metadata,
      name: self.name,
      field_type: AbstractType::from_easl_tree(
        self.type_ast,
        structs,
        aliases,
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
    structs: &Vec<Rc<AbstractStruct>>,
    aliases: &Vec<(Rc<str>, Rc<AbstractStruct>)>,
  ) -> CompileResult<AbstractStruct> {
    Ok(AbstractStruct {
      name: self.name,
      fields: self
        .fields
        .into_iter()
        .map(|field| field.assign_type(structs, aliases, &self.generic_args))
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
    structs: &Vec<Rc<AbstractStruct>>,
    skolems: &Vec<Rc<str>>,
    source_trace: SourceTrace,
  ) -> CompileResult<StructField> {
    Ok(StructField {
      metadata: self.metadata.clone(),
      name: Rc::clone(&self.name),
      field_type: TypeState::Known(self.field_type.concretize(
        skolems,
        structs,
        source_trace,
      )?)
      .into(),
    })
  }
  pub fn fill_generics(
    &self,
    generics: &HashMap<Rc<str>, ExpTypeInfo>,
    structs: &Vec<Rc<AbstractStruct>>,
    source_trace: SourceTrace,
  ) -> CompileResult<StructField> {
    Ok(StructField {
      metadata: self.metadata.clone(),
      name: self.name.clone(),
      field_type: self.field_type.fill_generics(
        generics,
        structs,
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
  pub fn compile(
    self,
    structs: &Vec<Rc<AbstractStruct>>,
  ) -> CompileResult<String> {
    let metadata = if let Some(metadata) = self.metadata {
      metadata.compile()
    } else {
      String::new()
    };
    let name = compile_word(self.name);
    let field_type = self.field_type.compile(structs)?;
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
    structs: &Vec<Rc<AbstractStruct>>,
    skolems: &Vec<Rc<str>>,
    source_trace: SourceTrace,
  ) -> CompileResult<Struct> {
    Ok(Struct {
      name: Rc::clone(&s.name),
      fields: s
        .fields
        .iter()
        .map(|f| f.concretize(structs, skolems, source_trace.clone()))
        .collect::<CompileResult<Vec<_>>>()?,
      abstract_ancestor: s,
    })
  }
  pub fn compile_if_non_generic(
    self,
    structs: &Vec<Rc<AbstractStruct>>,
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
              .concretize(&vec![], structs, self.source_trace.clone())
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
    structs: &Vec<Rc<AbstractStruct>>,
    source_trace: SourceTrace,
  ) -> CompileResult<Rc<str>> {
    let concretized =
      Self::concretize(s.clone(), structs, &vec![], source_trace)?;
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
    structs: &Vec<Rc<AbstractStruct>>,
    source_trace: SourceTrace,
  ) -> CompileResult<Struct> {
    let new_fields = s
      .fields
      .iter()
      .map(|field| field.fill_generics(generics, structs, source_trace.clone()))
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
    structs: &Vec<Rc<AbstractStruct>>,
    source_trace: SourceTrace,
  ) -> CompileResult<Struct> {
    let generics_map = s
      .generic_args
      .iter()
      .cloned()
      .zip(generics.into_iter())
      .collect();
    Self::fill_generics(s, &generics_map, structs, source_trace)
  }
  pub fn fill_generics_with_unification_variables(
    s: Rc<Self>,
    structs: &Vec<Rc<AbstractStruct>>,
    source_trace: SourceTrace,
  ) -> CompileResult<Struct> {
    let generic_count = s.generic_args.len();
    Self::fill_generics_ordered(
      s,
      (0..generic_count)
        .into_iter()
        .map(|_| TypeState::fresh_unification_variable().into())
        .collect(),
      structs,
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
}
