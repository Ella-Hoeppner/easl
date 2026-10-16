use std::collections::HashMap;

use std::sync::{Arc, RwLock};

use fsexp::{Ast, EncloserOrOperator};

use crate::{
  Never,
  compiler::{
    error::{
      CompileError, CompileErrorKind::*, CompileResult, ErrorLog, SourceTrace,
      err,
    },
    exp_builder::{
      ExpBuilder, builtin_signature, enum_constructor, generated_function,
    },
    expression::{ExpKind, TypedExp},
    functions::{
      AbstractFunctionSignature, FunctionImplementationKind, Ownership,
    },
    modules::display_name,
    program::{CompilerTarget, NameContext, Program, TypeDefs},
    types::{
      AbstractType, ConstGenericValue, ExpTypeInfo, GenericArgument,
      GenericArgumentValue, Type, TypeConstraint, TypeDescription, TypeState,
      contains_name_leaf,
    },
    util::compile_word,
  },
  parse::{EaslTree, Encloser},
};

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedEnumVariant {
  name: Arc<str>,
  source: SourceTrace,
  type_ast: Option<EaslTree>,
}

impl UntypedEnumVariant {
  fn from_field_tree(ast: EaslTree) -> CompileResult<Self> {
    match ast {
      Ast::Leaf(pos, name) => Ok(Self {
        source: pos.into(),
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
              source: position.into(),
              name: name.into(),
              type_ast,
            })
          } else {
            Err(CompileError::new(InvalidEnumVariant, position.into()))
          }
        } else {
          Err(CompileError::new(InvalidEnumVariant, position.into()))
        }
      }
      Ast::Inner((position, _), _) => {
        Err(CompileError::new(InvalidEnumVariant, position.into()))
      }
    }
  }
  pub fn references_type_name(&self, name: &Arc<str>) -> bool {
    if let Some(type_ast) = &self.type_ast {
      contains_name_leaf(&name, &type_ast)
    } else {
      false
    }
  }
  pub fn assign_type(
    self,
    typedefs: &TypeDefs,
    skolems: &Vec<(Arc<str>, Vec<TypeConstraint>)>,
  ) -> CompileResult<AbstractEnumVariant> {
    Ok(AbstractEnumVariant {
      name: self.name,
      source: self.source,
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
  pub name: (Arc<str>, SourceTrace),
  pub variants: Vec<UntypedEnumVariant>,
  pub generic_args: Vec<(Arc<str>, GenericArgument, SourceTrace)>,
  pub source_trace: SourceTrace,
}
impl UntypedEnum {
  pub fn references_type_name(&self, name: &Arc<str>) -> bool {
    self
      .variants
      .iter()
      .fold(false, |acc, v| acc || v.references_type_name(name))
  }
  pub fn from_field_trees(
    name: (Arc<str>, SourceTrace),
    generic_args: Vec<(Arc<str>, GenericArgument, SourceTrace)>,
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
        .map(|variant| {
          variant.assign_type(
            typedefs,
            &self
              .generic_args
              .iter()
              .map(|(n, arg, _)| (n.clone(), arg.type_constraints()))
              .collect(),
          )
        })
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
  pub name: Arc<str>,
  pub source: SourceTrace,
  pub inner_type: AbstractType,
}

impl AbstractEnumVariant {
  pub fn fill_generics(
    &self,
    generics: &HashMap<Arc<str>, ExpTypeInfo>,
    generic_constants: &HashMap<Arc<str>, ConstGenericValue>,
    typedefs: &TypeDefs,
    source_trace: &SourceTrace,
  ) -> CompileResult<EnumVariant> {
    Ok(EnumVariant {
      name: self.name.clone(),
      inner_type: self.inner_type.fill_generics(
        generics,
        generic_constants,
        typedefs,
        source_trace,
      )?,
    })
  }
  pub fn concretize(
    &self,
    typedefs: &TypeDefs,
    skolems: &Vec<(Arc<str>, Vec<TypeConstraint>)>,
    source_trace: SourceTrace,
  ) -> CompileResult<EnumVariant> {
    Ok(EnumVariant {
      name: Arc::clone(&self.name),
      inner_type: self
        .inner_type
        .concretize(skolems, typedefs, source_trace)?
        .known()
        .into(),
    })
  }
  fn fill_abstract_generics(
    self,
    generics: &HashMap<Arc<str>, AbstractType>,
  ) -> Self {
    AbstractEnumVariant {
      name: self.name,
      source: self.source,
      inner_type: self.inner_type.fill_abstract_generics(generics),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractEnum {
  pub name: (Arc<str>, SourceTrace),
  pub filled_generics: HashMap<Arc<str>, AbstractType>,
  pub generic_args: Vec<(Arc<str>, GenericArgument, SourceTrace)>,
  pub variants: Vec<AbstractEnumVariant>,
  pub abstract_ancestor: Option<Arc<Self>>,
  pub source_trace: SourceTrace,
}

impl AbstractEnum {
  pub fn has_unit_variant_named(&self, name: &str) -> bool {
    self
      .variants
      .iter()
      .find(|variant| {
        variant.inner_type == AbstractType::Type(Type::Unit)
          && &*variant.name == name
      })
      .is_some()
  }
  pub fn original_ancestor(&self) -> &Self {
    &self
      .abstract_ancestor
      .as_ref()
      .map(|ancestor| ancestor.original_ancestor())
      .unwrap_or(&self)
  }
  pub fn fill_generics(
    s: Arc<Self>,
    generics: &HashMap<Arc<str>, ExpTypeInfo>,
    generic_constants: &HashMap<Arc<str>, ConstGenericValue>,
    typedefs: &TypeDefs,
    source_trace: &SourceTrace,
  ) -> CompileResult<Enum> {
    let new_variants = s
      .variants
      .iter()
      .map(|variant| {
        variant.fill_generics(
          generics,
          generic_constants,
          typedefs,
          source_trace,
        )
      })
      .collect::<CompileResult<Vec<_>>>()?;
    Ok(Enum {
      name: s.name.0.clone(),
      abstract_ancestor: s,
      variants: new_variants,
    })
  }
  pub fn concretize(
    s: Arc<Self>,
    typedefs: &TypeDefs,
    skolems: &Vec<(Arc<str>, Vec<TypeConstraint>)>,
    source_trace: SourceTrace,
  ) -> CompileResult<Enum> {
    Ok(Enum {
      name: Arc::clone(&s.name.0),
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
    generics: HashMap<Arc<str>, AbstractType>,
  ) -> AbstractEnum {
    let abstract_ancestor = self.clone().into();
    AbstractEnum {
      name: self.name.clone(),
      generic_args: self
        .generic_args
        .into_iter()
        .filter(|(name, _, _)| !generics.contains_key(name))
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
    s: Arc<Self>,
    generics: Vec<GenericArgumentValue>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<Enum> {
    if s.generic_args.len() != generics.len() {
      return err(
        WrongNumberOfGenericArguments(s.generic_args.len(), generics.len()),
        source_trace,
      );
    }
    let mut generics_map: HashMap<Arc<str>, ExpTypeInfo> = HashMap::new();
    let mut generic_constants_map: HashMap<Arc<str>, ConstGenericValue> =
      HashMap::new();
    for ((name, generic_argument, _), generic_value) in
      s.generic_args.iter().cloned().zip(generics.into_iter())
    {
      match generic_argument {
        GenericArgument::Type(_) => match generic_value {
          GenericArgumentValue::Type(t) => {
            generics_map.insert(name, t);
          }
          GenericArgumentValue::Constant(_) => {
            panic!("const-generic found when type-generic was expected");
          }
        },
        GenericArgument::Constant => match generic_value {
          GenericArgumentValue::Type(_) => {
            panic!("type-generic found when const-generic was expected");
          }
          GenericArgumentValue::Constant(c) => {
            generic_constants_map.insert(name, c);
          }
        },
      }
    }
    Self::fill_generics(
      s,
      &generics_map,
      &generic_constants_map,
      typedefs,
      &source_trace,
    )
  }
  pub fn fill_generics_with_unification_variables(
    s: Arc<Self>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<Enum> {
    let generic_values = s
      .generic_args
      .iter()
      .map(|(_, arg, _)| match arg {
        GenericArgument::Type(_) => GenericArgumentValue::Type(
          TypeState::fresh_unification_variable().into(),
        ),
        GenericArgument::Constant => {
          GenericArgumentValue::Constant(ConstGenericValue::fresh())
        }
      })
      .collect();
    Self::fill_generics_ordered(s, generic_values, typedefs, source_trace)
  }
  pub fn inner_flat_data_size_in_u32s(&self) -> CompileResult<usize> {
    Ok(
      self
        .variants
        .iter()
        .map(|x| x.inner_type.flat_data_size_in_u32s(&self.source_trace))
        .collect::<CompileResult<Vec<usize>>>()?
        .into_iter()
        .max()
        .unwrap_or(0),
    )
  }
  pub fn compile_if_non_generic(
    self,
    typedefs: &TypeDefs,
    names: &mut NameContext,
    target: CompilerTarget,
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
        let monomorphized_name =
          compile_word(self.monomorphized_name(&field_types, names, target));
        let size = self.inner_flat_data_size_in_u32s()?;
        let unit_constructor_constants: Vec<String> = self
          .variants
          .iter()
          .enumerate()
          .map(|(i, variant)| {
            Ok(if variant.inner_type == AbstractType::Type(Type::Unit) {
              let generic_arg_names = self.generic_arg_monomorphized_names(
                &field_types,
                names,
                target,
              );
              let const_name = compile_word(names.get_monomorphized_name(
                variant.name.clone(),
                generic_arg_names,
              ));
              Some(match target {
                CompilerTarget::WGSL => format!(
                  "const {const_name}: {monomorphized_name} = \
                  {monomorphized_name}({i}{});",
                  if size == 0 {
                    String::new()
                  } else {
                    let mut zeroed_array_string = ", array(".to_string();
                    for i in 0..self.inner_flat_data_size_in_u32s()? {
                      zeroed_array_string += if i == 0 { "0" } else { ", 0" }
                    }
                    zeroed_array_string += ")";
                    zeroed_array_string
                  }
                ),
                CompilerTarget::C => {
                  format!("const {monomorphized_name} {const_name} = {{{i}}};")
                }
                CompilerTarget::VM => panic!(),
              })
            } else {
              None
            })
          })
          .collect::<CompileResult<Vec<Option<String>>>>()?
          .into_iter()
          .filter_map(|x| x)
          .collect();
        Ok(unit_constructor_constants.into_iter().fold(
          {
            let data_line = if size == 0 {
              String::new()
            } else {
              match target {
                CompilerTarget::WGSL => {
                  format!("  data: array<u32, {size}>\n")
                }
                CompilerTarget::C => format!("  uint32_t data[{size}];\n"),
                CompilerTarget::VM => panic!(),
              }
            };
            match target {
              CompilerTarget::WGSL => format!(
                "struct {monomorphized_name} {{\n  \
                  discriminant: u32,\n\
                  {data_line}\
                }}"
              ),
              CompilerTarget::C => format!(
                "typedef struct {{\n  \
                  uint32_t discriminant;\n\
                  {data_line}\
                }} {monomorphized_name};"
              ),
              CompilerTarget::VM => panic!(),
            }
          },
          |acc, constant_string| acc + "\n\n" + &constant_string,
        ))
      })
      .map_or(Ok(None), |v| v.map(Some))
  }
  pub fn generic_arg_monomorphized_names(
    &self,
    variant_types: &Vec<Type>,
    names: &mut NameContext,
    target: CompilerTarget,
  ) -> Vec<Arc<str>> {
    let mut generic_type_bindings = HashMap::new();
    let mut generic_constant_bindings = HashMap::new();
    for (variant, variant_type) in self
      .original_ancestor()
      .variants
      .iter()
      .zip(variant_types.iter())
    {
      variant.inner_type.extract_generic_bindings(
        variant_type,
        &mut generic_type_bindings,
        &mut generic_constant_bindings,
      );
    }

    self
      .original_ancestor()
      .generic_args
      .iter()
      .map(|(name, generic_arg, _)| match generic_arg {
        GenericArgument::Type(_) => generic_type_bindings
          .get(name)
          .unwrap()
          .monomorphized_name(names, target)
          .into(),
        GenericArgument::Constant => {
          format!("{}", generic_constant_bindings.get(name).unwrap()).into()
        }
      })
      .collect()
  }
  pub fn monomorphized_name(
    &self,
    variant_types: &Vec<Type>,
    names: &mut NameContext,
    target: CompilerTarget,
  ) -> Arc<str> {
    let generic_arg_names =
      self.generic_arg_monomorphized_names(variant_types, names, target);
    names.get_monomorphized_name(self.name.0.clone(), generic_arg_names)
  }
  pub fn fill_abstract_generics(
    self,
    generics: Vec<AbstractType>,
  ) -> AbstractEnum {
    let generics_map: HashMap<Arc<str>, AbstractType> = self
      .generic_args
      .iter()
      .map(|(n, _, _)| n)
      .cloned()
      .zip(generics.into_iter())
      .collect();
    self.partially_fill_abstract_generics(generics_map)
  }
  pub fn fill_const_generics(
    self,
    bindings: &HashMap<Arc<str>, u32>,
  ) -> AbstractEnum {
    let abstract_ancestor = self.clone().into();
    AbstractEnum {
      name: self.name.clone(),
      generic_args: self
        .generic_args
        .into_iter()
        .filter(|(name, arg, _)| match arg {
          GenericArgument::Constant => !bindings.contains_key(name),
          GenericArgument::Type(_) => true,
        })
        .collect(),
      variants: self
        .variants
        .into_iter()
        .map(|mut variant| {
          variant.inner_type = variant.inner_type.fill_const_generics(bindings);
          variant
        })
        .collect(),
      filled_generics: self.filled_generics,
      abstract_ancestor: Some(abstract_ancestor),
      source_trace: self.source_trace,
    }
  }
  pub fn extract_generic_bindings(
    &self,
    concrete_enum: &Enum,
    type_bindings: &mut HashMap<Arc<str>, Type>,
    constant_bindings: &mut HashMap<Arc<str>, u32>,
  ) {
    for i in 0..concrete_enum.variants.len() {
      self.variants[i].inner_type.extract_generic_bindings(
        &concrete_enum.variants[i].inner_type.unwrap_known(),
        type_bindings,
        constant_bindings,
      );
    }
  }
  pub fn generate_monomorphized(
    &self,
    concrete_enum: Enum,
  ) -> Option<AbstractEnum> {
    if self.generic_args.is_empty() {
      return None;
    }
    let mut generic_arg_type_map = HashMap::new();
    let mut generic_arg_constant_map = HashMap::new();
    self.extract_generic_bindings(
      &concrete_enum,
      &mut generic_arg_type_map,
      &mut generic_arg_constant_map,
    );
    let filled_generics: HashMap<Arc<str>, AbstractType> = self
      .generic_args
      .iter()
      .filter_map(|(name, arg, _)| match arg {
        GenericArgument::Type(_) => generic_arg_type_map
          .get(name)
          .map(|t| (name.clone(), AbstractType::Type(t.clone()))),
        GenericArgument::Constant => None,
      })
      .collect();
    Some(AbstractEnum {
      name: self.name.clone(),
      generic_args: vec![],
      variants: self
        .variants
        .iter()
        .map(|variant| {
          let mut new_variant = variant.clone();
          // Fills the generic wherever it appears: the payload itself, an
          // array's elements, a generic struct's or enum's arguments.
          new_variant.inner_type = new_variant
            .inner_type
            .fill_abstract_generics(&filled_generics);
          // A generic nested inside a concrete type (`(Fn [T] T)`) is a
          // skolem there.
          new_variant
            .inner_type
            .replace_bound_skolems(&generic_arg_type_map);
          if !generic_arg_constant_map.is_empty() {
            new_variant.inner_type = new_variant
              .inner_type
              .fill_const_generics(&generic_arg_constant_map);
          }
          new_variant
        })
        .collect(),
      filled_generics,
      abstract_ancestor: Some(self.clone().into()),
      source_trace: self.source_trace.clone(),
    })
  }
  pub fn concretized_name(
    e: Arc<Self>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
    names: &mut NameContext,
    target: CompilerTarget,
  ) -> CompileResult<Arc<str>> {
    let concretized =
      Self::concretize(e.clone(), typedefs, &vec![], source_trace)?;
    Ok(
      e.monomorphized_name(
        &concretized
          .variants
          .iter()
          .map(|variant| variant.inner_type.unwrap_known())
          .collect(),
        names,
        target,
      ),
    )
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
  pub name: Arc<str>,
  pub inner_type: ExpTypeInfo,
}

#[derive(Debug, Clone)]
pub struct Enum {
  pub name: Arc<str>,
  pub variants: Vec<EnumVariant>,
  pub abstract_ancestor: Arc<AbstractEnum>,
}

impl PartialEq for Enum {
  /// The abstract ancestor is lineage metadata, not identity: the same
  /// concrete enum can arrive from different monomorphization paths
  /// carrying either the original generic ancestor or a monomorphized
  /// wrapper around it. (Mirrors `FunctionSignature`'s `PartialEq`, which
  /// likewise ignores `abstract_ancestor`.)
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name && self.variants == other.variants
  }
}

impl Enum {
  pub fn compatible(&self, other: &Self) -> bool {
    self.variants.iter().zip(other.variants.iter()).fold(
      self.name == other.name,
      |compatible_so_far, (field, other_field)| {
        field.inner_type.with_dereferenced(|typestate| {
          compatible_so_far
            && if let TypeState::Known(t) = typestate {
              other_field.inner_type.with_dereferenced(|other_typestate| {
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
  pub fn monomorphized_name(
    &self,
    names: &mut NameContext,
    target: CompilerTarget,
  ) -> Arc<str> {
    self.abstract_ancestor.monomorphized_name(
      &self
        .variants
        .iter()
        .map(|v| v.inner_type.unwrap_known())
        .collect(),
      names,
      target,
    )
  }
  pub fn inner_flat_data_size_in_u32s(&self) -> CompileResult<usize> {
    // The tagged-union `data` array holds only the active variant's payload, so
    // it's sized to the largest variant, matching the struct-declaration path
    // (`AbstractEnum::inner_flat_data_size_in_u32s`) and `vm_stack_size`.
    Ok(
      self
        .variants
        .iter()
        .map(|v| {
          v.inner_type
            .unwrap_known()
            .flat_data_size_in_u32s(&self.abstract_ancestor.source_trace)
        })
        .collect::<CompileResult<Vec<usize>>>()?
        .into_iter()
        .max()
        .unwrap_or(0),
    )
  }
}

/// Generates the functions `==` and `!=` on enums lower to, one per
/// concrete enum (see `Program::lower_enum_comparisons`).
#[derive(Default)]
struct EnumComparisons {
  /// By enum (its monomorphized name).
  functions: HashMap<Arc<str>, Arc<RwLock<AbstractFunctionSignature>>>,
  new_functions: Vec<Arc<RwLock<AbstractFunctionSignature>>>,
}

impl EnumComparisons {
  /// Lowers the enum comparisons in `exp`, logging an error at each one
  /// whose enum holds a payload without `==`.
  fn lower_in(
    &mut self,
    exp: &mut TypedExp,
    program: &Program,
    target: CompilerTarget,
    errors: &mut ErrorLog,
  ) {
    exp
      .walk_mut(&mut |exp| {
        if let ExpKind::Application(f, args) = &mut exp.kind
          && let ExpKind::Name(op) = &f.kind
          && (&**op == "==" || &**op == "!=")
          && args.len() == 2
          && let Type::Enum(enum_type) = args[0].data.unwrap_known()
          && let Type::Function(signature) = f.data.unwrap_known()
          && signature
            .abstract_ancestor
            .as_ref()
            .is_some_and(|ancestor| {
              matches!(
                ancestor.read().unwrap().implementation,
                FunctionImplementationKind::Builtin { .. }
              )
            })
        {
          let b = ExpBuilder::at(&exp.source_trace);
          let equal = match self.equality_function(&enum_type, program, target)
          {
            Ok(equal) => equal,
            Err((variant, payload)) => {
              errors.log(CompileError::new(
                IncomparableEnum(
                  display_name(
                    &enum_type.abstract_ancestor.original_ancestor().name.0,
                  )
                  .to_string(),
                  display_name(&variant).to_string(),
                  TypeDescription::from(payload).to_string(),
                ),
                exp.source_trace.clone(),
              ));
              return Ok(false);
            }
          };
          let negate = &**op == "!=";
          let operands = std::mem::take(args);
          let comparison = b.call(&equal, operands, &Type::Bool);
          let kind = if negate {
            let not = builtin_signature(
              "!",
              vec![(Type::Bool, Ownership::Owned)],
              Type::Bool,
              None,
            );
            b.call(&not, vec![comparison], &Type::Bool).kind
          } else {
            comparison.kind
          };
          *exp = b.with_data(kind, exp.data.clone());
        }
        Ok::<bool, Never>(true)
      })
      .unwrap();
  }

  /// The generated equality function of `enum_type`, or the variant and
  /// payload type that has no `==`.
  fn equality_function(
    &mut self,
    enum_type: &Enum,
    program: &Program,
    target: CompilerTarget,
  ) -> Result<Arc<RwLock<AbstractFunctionSignature>>, (Arc<str>, Type)> {
    let key =
      enum_type.monomorphized_name(&mut program.names.write().unwrap(), target);
    if let Some(function) = self.functions.get(&key) {
      return Ok(function.clone());
    }
    let source = SourceTrace::empty();
    let b = ExpBuilder::at(&source);
    let enum_t = Type::Enum(enum_type.clone());
    let gensym = |base: &str| program.names.write().unwrap().gensym(base);
    let (a, other) = (gensym("enum_eq_a"), gensym("enum_eq_b"));
    let generic_arg_names = enum_type
      .abstract_ancestor
      .original_ancestor()
      .generic_arg_monomorphized_names(
        &enum_type
          .variants
          .iter()
          .map(|variant| variant.inner_type.unwrap_known())
          .collect(),
        &mut program.names.write().unwrap(),
        target,
      );
    let mut arms = vec![];
    for variant in enum_type.variants.iter() {
      let payload = variant.inner_type.unwrap_known();
      let (pattern, other_pattern, payloads_equal) = if payload == Type::Unit {
        let unit_name = program.names.write().unwrap().get_monomorphized_name(
          variant.name.clone(),
          generic_arg_names.clone(),
        );
        (
          b.name(&unit_name, &enum_t),
          b.name(&unit_name, &enum_t),
          b.bool(true),
        )
      } else {
        let (x, y) = (gensym("enum_eq_x"), gensym("enum_eq_y"));
        let constructor = enum_constructor(
          &variant.name,
          &payload,
          AbstractType::Type(enum_t.clone()),
        );
        let Some(payloads_equal) = self.values_equal(
          b.name(&x, &payload),
          b.name(&y, &payload),
          &payload,
          program,
          target,
        ) else {
          return Err((variant.name.clone(), payload));
        };
        (
          b.call(&constructor, vec![b.name(&x, &payload)], &enum_t),
          b.call(&constructor, vec![b.name(&y, &payload)], &enum_t),
          payloads_equal,
        )
      };
      let same_variant = b.match_on(
        b.name(&other, &enum_t),
        vec![
          (other_pattern, payloads_equal),
          (b.wildcard(&enum_t), b.bool(false)),
        ],
        &Type::Bool,
      );
      arms.push((pattern, same_variant));
    }
    let body = b.match_on(b.name(&a, &enum_t), arms, &Type::Bool);
    let name = gensym(&format!("easl_enum_equal_{key}"));
    let function = generated_function(
      &name,
      &[
        (a, enum_t.clone(), Ownership::Owned),
        (other, enum_t, Ownership::Owned),
      ],
      &Type::Bool,
      body,
      &source,
    );
    self.functions.insert(key, function.clone());
    self.new_functions.push(function.clone());
    Ok(function)
  }

  /// Whether `x` and `y`, of type `t`, are equal by `t`'s own `==` (every
  /// component of a vector; a nested enum's generated function), or `None`
  /// when `t` has no `==`.
  fn values_equal(
    &mut self,
    x: TypedExp,
    y: TypedExp,
    t: &Type,
    program: &Program,
    target: CompilerTarget,
  ) -> Option<TypedExp> {
    let b = ExpBuilder::at(&x.source_trace);
    let builtin = |name: &str, operand: &Type| {
      builtin_signature(
        name,
        vec![
          (operand.clone(), Ownership::Owned),
          (operand.clone(), Ownership::Owned),
        ],
        Type::Bool,
        None,
      )
    };
    match t {
      Type::Bool | Type::F32 | Type::I32 | Type::U32 | Type::String => {
        Some(b.call(&builtin("==", t), vec![x, y], &Type::Bool))
      }
      Type::Enum(inner) => {
        let equal = self.equality_function(inner, program, target).ok()?;
        Some(b.call(&equal, vec![x, y], &Type::Bool))
      }
      Type::Struct(s) if t.is_vector() => {
        let element = s.fields[0].field_type.unwrap_known();
        let components = s.fields.iter().map(|field| {
          b.call(
            &builtin("==", &element),
            vec![
              b.field(x.clone(), &field.name, element.clone()),
              b.field(y.clone(), &field.name, element.clone()),
            ],
            &Type::Bool,
          )
        });
        components.reduce(|all, component| {
          b.call(
            &builtin("&&", &Type::Bool),
            vec![all, component],
            &Type::Bool,
          )
        })
      }
      _ => None,
    }
  }
}

impl Program {
  /// Lowers `==` and `!=` on enum values to calls of a generated function
  /// per enum (`easl_enum_equal_<enum>`): a `match` on both operands that's
  /// true for the same variant with equal payloads, comparing payloads with
  /// their own `==` (every component of a vector, a nested enum's generated
  /// function). `!=` negates it. Runs right after monomorphization, when
  /// every enum is concrete; the generated functions are deexpressionified
  /// here, having missed that pass.
  pub fn lower_enum_comparisons(
    &mut self,
    target: CompilerTarget,
    errors: &mut ErrorLog,
  ) {
    let mut comparisons = EnumComparisons::default();
    // Generic functions stay registered beside their monomorphized copies,
    // and only the copies' enums are concrete.
    let implementations: Vec<_> = self
      .abstract_functions_iter()
      .filter_map(|signature| {
        let signature = signature.read().unwrap();
        match &signature.implementation {
          FunctionImplementationKind::Composite(f)
            if signature.generic_args.is_empty() =>
          {
            Some(f.clone())
          }
          _ => None,
        }
      })
      .collect();
    for f in implementations {
      comparisons.lower_in(
        &mut f.write().unwrap().expression,
        self,
        target,
        errors,
      );
    }
    let mut vars = std::mem::take(&mut self.top_level_vars);
    for var in vars.iter_mut() {
      if let Some(value) = &mut var.value {
        comparisons.lower_in(value, self, target, errors);
      }
    }
    self.top_level_vars = vars;
    for signature in comparisons.new_functions {
      if let FunctionImplementationKind::Composite(f) =
        &signature.read().unwrap().implementation
      {
        let mut f = f.write().unwrap();
        f.expression.throw_away_inner_values_in_blocks(self);
        f.expression.deexpressionify(self, target);
        if let ExpKind::Function(arg_names, _) = &f.expression.kind {
          f.arg_names = arg_names.clone();
        }
      }
      self.add_abstract_function(signature);
    }
  }
}
