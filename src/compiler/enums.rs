use std::{collections::HashMap, rc::Rc};

use sse::{Ast, EncloserOrOperator};

use crate::{
  compiler::{
    error::{CompileError, CompileErrorKind, CompileResult, SourceTrace},
    program::TypeDefs,
    structs::{compiled_vec_or_mat_name, vec_and_mat_compile_names},
    types::{AbstractType, ExpTypeInfo, Type, TypeState, contains_name_leaf},
  },
  parse::{EaslTree, Encloser},
};

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedEnumVariant {
  name: Rc<str>,
  type_ast: Option<EaslTree>,
}

impl UntypedEnumVariant {
  fn from_field_tree(ast: EaslTree) -> CompileResult<Self> {
    match ast {
      Ast::Leaf(_, name) => Ok(Self {
        name: name.into(),
        type_ast: None,
      }),
      Ast::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Parens)),
        mut children,
      ) => {
        if children.len() <= 2 {
          let type_ast = children.pop();
          let name = children.pop().unwrap();
          if let Ast::Leaf(_, name) = name {
            Ok(Self {
              name: name.into(),
              type_ast,
            })
          } else {
            Err(CompileError::new(
              CompileErrorKind::InvalidEnumVariant,
              position.into(),
            ))
          }
        } else {
          Err(CompileError::new(
            CompileErrorKind::InvalidEnumVariant,
            position.into(),
          ))
        }
      }
      Ast::Inner((position, _), _) => Err(CompileError::new(
        CompileErrorKind::InvalidEnumVariant,
        position.into(),
      )),
    }
  }
  pub fn references_type_name(&self, name: &Rc<str>) -> bool {
    if let Some(type_ast) = &self.type_ast {
      contains_name_leaf(&name, &type_ast)
    } else {
      false
    }
  }
  pub fn assign_type(
    self,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
  ) -> CompileResult<AbstractEnumVariant> {
    Ok(AbstractEnumVariant {
      name: self.name,
      inner_type: if let Some(type_ast) = self.type_ast {
        AbstractType::from_easl_tree(type_ast, typedefs, skolems)?
      } else {
        AbstractType::Type(Type::Unit)
      },
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedEnum {
  pub name: Rc<str>,
  pub variants: Vec<UntypedEnumVariant>,
  pub generic_args: Vec<Rc<str>>,
  source_trace: SourceTrace,
}
impl UntypedEnum {
  pub fn references_type_name(&self, name: &Rc<str>) -> bool {
    self
      .variants
      .iter()
      .fold(false, |acc, v| acc || v.references_type_name(name))
  }
  pub fn from_field_trees(
    name: Rc<str>,
    generic_args: Vec<Rc<str>>,
    variant_asts: Vec<EaslTree>,
    source_trace: SourceTrace,
  ) -> CompileResult<Self> {
    Ok(Self {
      name,
      generic_args,
      variants: variant_asts
        .into_iter()
        .map(UntypedEnumVariant::from_field_tree)
        .collect::<CompileResult<_>>()?,
      source_trace,
    })
  }
  pub fn assign_types(
    self,
    typedefs: &TypeDefs,
  ) -> CompileResult<AbstractEnum> {
    Ok(AbstractEnum {
      name: self.name,
      variants: self
        .variants
        .into_iter()
        .map(|variant| variant.assign_type(typedefs, &self.generic_args))
        .collect::<CompileResult<Vec<AbstractEnumVariant>>>()?,
      generic_args: self.generic_args.clone(),
      filled_generics: HashMap::new(),
      abstract_ancestor: None,
      source_trace: self.source_trace,
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractEnumVariant {
  pub name: Rc<str>,
  pub inner_type: AbstractType,
}

impl AbstractEnumVariant {
  pub fn fill_generics(
    &self,
    generics: &HashMap<Rc<str>, ExpTypeInfo>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<EnumVariant> {
    Ok(EnumVariant {
      name: self.name.clone(),
      inner_type: self.inner_type.fill_generics(
        generics,
        typedefs,
        source_trace,
      )?,
    })
  }
  pub fn concretize(
    &self,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
    source_trace: SourceTrace,
  ) -> CompileResult<EnumVariant> {
    Ok(EnumVariant {
      name: Rc::clone(&self.name),
      inner_type: TypeState::Known(self.inner_type.concretize(
        skolems,
        typedefs,
        source_trace,
      )?)
      .into(),
    })
  }
  fn fill_abstract_generics(
    self,
    generics: &HashMap<Rc<str>, AbstractType>,
  ) -> Self {
    AbstractEnumVariant {
      name: self.name,
      inner_type: self.inner_type.fill_abstract_generics(generics),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractEnum {
  pub name: Rc<str>,
  pub filled_generics: HashMap<Rc<str>, AbstractType>,
  pub generic_args: Vec<Rc<str>>,
  pub variants: Vec<AbstractEnumVariant>,
  pub abstract_ancestor: Option<Rc<Self>>,
  pub source_trace: SourceTrace,
}

impl AbstractEnum {
  pub fn original_ancestor(&self) -> &Self {
    &self
      .abstract_ancestor
      .as_ref()
      .map(|ancestor| ancestor.original_ancestor())
      .unwrap_or(&self)
  }
  pub fn fill_generics(
    s: Rc<Self>,
    generics: &HashMap<Rc<str>, ExpTypeInfo>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<Enum> {
    let new_variants = s
      .variants
      .iter()
      .map(|variant| {
        variant.fill_generics(generics, typedefs, source_trace.clone())
      })
      .collect::<CompileResult<Vec<_>>>()?;
    Ok(Enum {
      name: s.name.clone(),
      abstract_ancestor: s,
      variants: new_variants,
    })
  }
  pub fn concretize(
    s: Rc<Self>,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
    source_trace: SourceTrace,
  ) -> CompileResult<Enum> {
    Ok(Enum {
      name: Rc::clone(&s.name),
      variants: s
        .variants
        .iter()
        .map(|v| v.concretize(typedefs, skolems, source_trace.clone()))
        .collect::<CompileResult<Vec<_>>>()?,
      abstract_ancestor: s,
    })
  }
  pub fn partially_fill_abstract_generics(
    self,
    generics: HashMap<Rc<str>, AbstractType>,
  ) -> AbstractEnum {
    let abstract_ancestor = self.clone().into();
    AbstractEnum {
      name: self.name.clone(),
      generic_args: self
        .generic_args
        .into_iter()
        .filter(|name| generics.contains_key(name))
        .collect(),
      variants: self
        .variants
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
  pub fn fill_generics_ordered(
    s: Rc<Self>,
    generics: Vec<ExpTypeInfo>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<Enum> {
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
  ) -> CompileResult<Enum> {
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
  pub fn inner_size_in_u32s(&self) -> CompileResult<usize> {
    Ok(
      self
        .variants
        .iter()
        .map(|x| x.inner_type.size_in_u32s(&self.source_trace))
        .collect::<CompileResult<Vec<usize>>>()?
        .into_iter()
        .max()
        .unwrap_or(0),
    )
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
          .variants
          .iter()
          .map(|v| {
            v.inner_type.concretize(
              &vec![],
              typedefs,
              self.source_trace.clone(),
            )
          })
          .collect::<CompileResult<Vec<Type>>>()?;
        let monomorphized_name = self.monomorphized_name(&field_types);
        let size = self.inner_size_in_u32s()?;
        Ok(format!(
          "struct {monomorphized_name} {{\n  inner: array<u8, {size}>\n}}"
        ))
      })
      .map_or(Ok(None), |v| v.map(Some))
  }
  pub fn monomorphized_name(&self, variant_types: &Vec<Type>) -> Rc<str> {
    let mut generic_bindings = HashMap::new();
    for (variant, variant_type) in self
      .original_ancestor()
      .variants
      .iter()
      .zip(variant_types.iter())
    {
      variant
        .inner_type
        .extract_generic_bindings(variant_type, &mut generic_bindings);
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
  /*pub fn generate_monomorphized(
    &self,
    field_types: Vec<Type>,
  ) -> Option<AbstractEnum> {
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
  }*/
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
  pub name: Rc<str>,
  pub inner_type: ExpTypeInfo,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
  pub name: Rc<str>,
  pub variants: Vec<EnumVariant>,
  pub abstract_ancestor: Rc<AbstractEnum>,
}

impl Enum {
  pub fn monomorphized_name(&self) -> Rc<str> {
    self.abstract_ancestor.monomorphized_name(
      &self
        .variants
        .iter()
        .map(|v| v.inner_type.unwrap_known())
        .collect(),
    )
  }
  pub fn size_in_u32s(&self) -> CompileResult<usize> {
    Ok(
      self
        .variants
        .iter()
        .map(|v| {
          v.inner_type
            .unwrap_known()
            .size_in_u32s(&self.abstract_ancestor.source_trace)
        })
        .collect::<CompileResult<Vec<usize>>>()?
        .into_iter()
        .sum::<usize>(),
    )
  }
}
