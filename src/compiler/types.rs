use core::fmt::Debug;
use std::{
  cell::RefCell,
  collections::{HashMap, HashSet},
  fmt::Display,
  ops::{Deref, DerefMut},
  rc::Rc,
};

use fsexp::{document::DocumentPosition, syntax::EncloserOrOperator};

use crate::{
  compiler::{
    builtins::bitcast,
    enums::{AbstractEnum, Enum, UntypedEnum},
    error::{CompileError, CompileErrorKind},
    expression::{Accessor, ExpKind, Number, TypedExp},
    functions::Ownership,
    program::{NameContext, TypeDefs},
    structs::UntypedStruct,
    vars::VariableAddressSpace,
  },
  parse::{EaslTree, Encloser, Operator},
};

use super::{
  error::{CompileErrorKind::*, CompileResult, ErrorLog, SourceTrace, err},
  functions::FunctionSignature,
  program::Program,
  structs::{AbstractStruct, Struct},
  util::compile_word,
};

pub fn contains_name_leaf(name: &Rc<str>, tree: &EaslTree) -> bool {
  match &tree {
    EaslTree::Leaf(_, leaf) => leaf == &**name,
    EaslTree::Inner(_, children) => children
      .iter()
      .fold(false, |acc, child| acc || contains_name_leaf(name, child)),
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UntypedType {
  Struct(UntypedStruct),
  Enum(UntypedEnum),
}
impl UntypedType {
  pub fn references_type_name(&self, name: &Rc<str>) -> bool {
    match self {
      UntypedType::Struct(untyped_struct) => {
        untyped_struct.references_type_name(name)
      }
      UntypedType::Enum(untyped_enum) => {
        untyped_enum.references_type_name(name)
      }
    }
  }

  pub fn name(&self) -> &Rc<str> {
    match self {
      UntypedType::Struct(untyped_struct) => &untyped_struct.name.0,
      UntypedType::Enum(untyped_enum) => &untyped_enum.name.0,
    }
  }

  pub fn source_trace(&self) -> &SourceTrace {
    match self {
      UntypedType::Struct(untyped_struct) => &untyped_struct.source_trace,
      UntypedType::Enum(untyped_enum) => &untyped_enum.source_trace,
    }
  }

  pub fn sort_by_references(
    unsorted_types: &Vec<Self>,
  ) -> Result<Vec<Self>, Vec<Rc<str>>> {
    let mut sorted = Vec::new();
    let mut sorted_names = HashSet::new();

    while sorted.len() < unsorted_types.len() {
      let start_len = sorted.len();

      for typ in unsorted_types {
        if sorted_names.contains(typ.name()) {
          continue;
        }

        let mut can_add = true;
        for other in unsorted_types {
          if typ.references_type_name(other.name())
            && !sorted_names.contains(other.name())
          {
            can_add = false;
            break;
          }
        }

        if can_add {
          sorted.push(typ.clone());
          sorted_names.insert(typ.name().clone());
        }
      }

      if sorted.len() == start_len {
        for typ in unsorted_types {
          if !sorted_names.contains(typ.name()) {
            if let Some(cycle_path) =
              Self::find_cycle_path(typ, &unsorted_types, &sorted_names)
            {
              return Err(cycle_path);
            }
          }
        }
        return Err(vec![]);
      }
    }

    Ok(sorted)
  }

  fn find_cycle_path(
    start: &Self,
    all_types: &[Self],
    already_added: &HashSet<Rc<str>>,
  ) -> Option<Vec<Rc<str>>> {
    let mut visited = HashSet::new();
    let mut path = Vec::new();

    fn trace_dependencies(
      current_name: &Rc<str>,
      all_types: &[UntypedType],
      already_added: &HashSet<Rc<str>>,
      visited: &mut HashSet<Rc<str>>,
      path: &mut Vec<Rc<str>>,
    ) -> Option<Vec<Rc<str>>> {
      if let Some(cycle_start_idx) = path.iter().position(|n| n == current_name)
      {
        let mut cycle = path[cycle_start_idx..].to_vec();
        cycle.push(current_name.clone());
        return Some(cycle);
      }

      if already_added.contains(current_name) || visited.contains(current_name)
      {
        return None;
      }

      path.push(current_name.clone());
      visited.insert(current_name.clone());

      if let Some(typ) = all_types.iter().find(|t| t.name() == current_name) {
        for other in all_types {
          if typ.references_type_name(other.name()) {
            if let Some(cycle) = trace_dependencies(
              other.name(),
              all_types,
              already_added,
              visited,
              path,
            ) {
              return Some(cycle);
            }
          }
        }
      }

      path.pop();
      None
    }

    trace_dependencies(
      start.name(),
      all_types,
      already_added,
      &mut visited,
      &mut path,
    )
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AbstractType {
  Unit,
  Generic(Rc<str>),
  Type(Type),
  AbstractStruct(Rc<AbstractStruct>),
  AbstractEnum(Rc<AbstractEnum>),
  AbstractArray {
    size: ArraySize,
    inner_type: Box<Self>,
    source_trace: SourceTrace,
  },
}

impl AbstractType {
  pub(crate) fn track_generic_names(&self, names: &mut Vec<Rc<str>>) {
    match self {
      AbstractType::Generic(name) => names.push(name.clone()),
      AbstractType::AbstractArray { inner_type, .. } => {
        inner_type.track_generic_names(names)
      }
      AbstractType::AbstractStruct(abstract_struct) => {
        for f in abstract_struct.fields.iter() {
          f.field_type.track_generic_names(names);
        }
      }
      _ => {}
    }
  }
  pub fn fill_generics(
    &self,
    generics: &HashMap<Rc<str>, ExpTypeInfo>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<ExpTypeInfo> {
    Ok(match self {
      AbstractType::Unit => TypeState::Known(Type::Unit).into(),
      AbstractType::Generic(var_name) => generics
        .get(var_name)
        .expect("unrecognized generic name in struct")
        .clone(),
      AbstractType::Type(t) => t.clone().known().into(),
      AbstractType::AbstractStruct(s) => {
        Type::Struct(AbstractStruct::fill_generics(
          s.clone(),
          generics,
          typedefs,
          source_trace,
        )?)
        .known()
        .into()
      }
      AbstractType::AbstractEnum(e) => Type::Enum(AbstractEnum::fill_generics(
        e.clone(),
        generics,
        typedefs,
        source_trace,
      )?)
      .known()
      .into(),
      AbstractType::AbstractArray {
        size, inner_type, ..
      } => Type::Array(
        Some(size.clone()),
        inner_type
          .fill_generics(generics, typedefs, source_trace)?
          .into(),
      )
      .known()
      .into(),
    })
  }
  pub fn concretize(
    &self,
    skolems: &Vec<Rc<str>>,
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<Type> {
    match self {
      AbstractType::Unit => Ok(Type::Unit),
      AbstractType::Generic(name) => {
        if skolems.contains(name) {
          Ok(Type::Skolem(Rc::clone(name)))
        } else {
          err(UnrecognizedGeneric(name.to_string()), source_trace)
        }
      }
      AbstractType::AbstractStruct(s) => Ok(Type::Struct(
        AbstractStruct::concretize(s.clone(), typedefs, skolems, source_trace)?,
      )),
      AbstractType::AbstractEnum(e) => Ok(Type::Enum(
        AbstractEnum::concretize(e.clone(), typedefs, skolems, source_trace)?,
      )),
      AbstractType::Type(t) => Ok(t.clone()),
      AbstractType::AbstractArray {
        size, inner_type, ..
      } => Ok(Type::Array(
        Some(size.clone()),
        Box::new(
          inner_type
            .concretize(skolems, typedefs, source_trace)?
            .known()
            .into(),
        ),
      )),
    }
  }
  pub fn fill_abstract_generics(
    self,
    generics: &HashMap<Rc<str>, AbstractType>,
  ) -> Self {
    match self {
      AbstractType::Unit => AbstractType::Unit,
      AbstractType::Generic(var_name) => generics
        .iter()
        .find_map(|(name, t)| (*name == var_name).then(|| t))
        .expect("unrecognized generic name in struct")
        .clone(),
      AbstractType::Type(t) => AbstractType::Type(t),
      AbstractType::AbstractStruct(s) => AbstractType::AbstractStruct(Rc::new(
        (*s)
          .clone()
          .partially_fill_abstract_generics(generics.clone()),
      )),
      AbstractType::AbstractEnum(e) => AbstractType::AbstractEnum(Rc::new(
        (*e)
          .clone()
          .partially_fill_abstract_generics(generics.clone()),
      )),
      AbstractType::AbstractArray {
        size,
        inner_type,
        source_trace,
      } => AbstractType::AbstractArray {
        size,
        source_trace,
        inner_type: inner_type.fill_abstract_generics(generics).into(),
      },
    }
  }
  pub fn compile(
    self,
    typedefs: &TypeDefs,
    names: &mut NameContext,
    source_trace: &SourceTrace,
  ) -> CompileResult<String> {
    Ok(match self {
      AbstractType::Unit => {
        return Err(CompileError::new(
          CompileErrorKind::TriedToCompileUnit,
          source_trace.clone(),
        ));
      }
      AbstractType::Generic(_) => {
        panic!("attempted to compile generic struct field")
      }
      AbstractType::Type(t) => t.monomorphized_name(names),
      AbstractType::AbstractStruct(t) => Rc::unwrap_or_clone(t)
        .compile_if_non_generic(typedefs, names)?
        .expect("failed to compile abstract struct"),
      AbstractType::AbstractEnum(e) => Rc::unwrap_or_clone(e)
        .compile_if_non_generic(typedefs, names)?
        .expect("failed to compile abstract enum"),
      AbstractType::AbstractArray {
        size, inner_type, ..
      } => {
        format!(
          "array<{}{}>",
          inner_type.compile(typedefs, names, source_trace)?,
          format!("{}", size.compile_type())
        )
      }
    })
  }
  pub fn from_easl_tree(
    tree: EaslTree,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
  ) -> CompileResult<Self> {
    match tree {
      EaslTree::Leaf(position, leaf) => {
        let leaf_rc: Rc<str> = leaf.into();
        Ok(if skolems.contains(&leaf_rc) {
          AbstractType::Generic(leaf_rc)
        } else {
          AbstractType::Type(Type::from_name(
            leaf_rc,
            position.clone(),
            typedefs,
            skolems,
          )?)
        })
      }
      EaslTree::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Parens)),
        children,
      ) => {
        if children.is_empty() {
          return Ok(Self::Unit);
        }
        let mut children_iter = children.iter();
        let generic_struct_name =
          if let Some(EaslTree::Leaf(_, leaf)) = children_iter.next() {
            leaf
          } else {
            return err(InvalidTypeName, position.into());
          };
        if generic_struct_name.as_str() == "Fn" {
          Ok(AbstractType::Type(Type::from_easl_tree(
            EaslTree::Inner(
              (position, EncloserOrOperator::Encloser(Encloser::Parens)),
              children,
            ),
            typedefs,
            skolems,
          )?))
        } else {
          let mut children_iter = children.into_iter();
          let generic_type_name =
            if let EaslTree::Leaf(_, leaf) = children_iter.next().unwrap() {
              leaf
            } else {
              unreachable!()
            };
          match (
            typedefs
              .structs
              .iter()
              .find(|s| &*s.name.0 == generic_type_name.as_str()),
            typedefs
              .enums
              .iter()
              .find(|e| &*e.name.0 == generic_type_name.as_str()),
          ) {
            (Some(generic_struct), None) => {
              let generic_args = children_iter
                .map(|subtree: EaslTree| {
                  Self::from_easl_tree(subtree, typedefs, skolems)
                })
                .collect::<CompileResult<Vec<_>>>()?;
              Ok(AbstractType::AbstractStruct(Rc::new(
                generic_struct.clone().fill_abstract_generics(generic_args),
              )))
            }
            (None, Some(generic_enum)) => {
              let generic_args = children_iter
                .map(|subtree: EaslTree| {
                  Self::from_easl_tree(subtree, typedefs, skolems)
                })
                .collect::<CompileResult<Vec<_>>>()?;
              Ok(AbstractType::AbstractEnum(Rc::new(
                Rc::unwrap_or_clone(generic_enum.clone())
                  .fill_abstract_generics(generic_args),
              )))
            }
            (None, None) => {
              return Err(CompileError::new(
                NoTypeNamed(generic_type_name.clone().into()),
                position.into(),
              ));
            }
            (Some(_), Some(_)) => panic!("duplicate type name encountered"),
          }
        }
      }
      EaslTree::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Square)),
        array_children,
      ) => {
        let source_trace: SourceTrace = position.clone().into();
        if array_children.len() == 1 {
          match array_children.iter().next().unwrap().clone() {
            EaslTree::Inner(
              (
                position,
                EncloserOrOperator::Operator(Operator::TypeAscription),
              ),
              mut type_annotation_children,
            ) => {
              let source_trace: SourceTrace = position.into();
              if let EaslTree::Leaf(_, num_str) =
                type_annotation_children.remove(0)
              {
                let inner_type = Type::from_easl_tree(
                  type_annotation_children.remove(0),
                  typedefs,
                  skolems,
                )?;
                Ok(AbstractType::Type(Type::Array(
                  Some(if let Ok(array_size) = num_str.parse::<u32>() {
                    ArraySize::Literal(array_size)
                  } else {
                    ArraySize::Constant(num_str.into())
                  }),
                  Box::new(inner_type.known().into()),
                )))
              } else {
                return err(InvalidArraySignature, source_trace);
              }
            }
            other => {
              let inner_type = Type::from_easl_tree(other, typedefs, skolems)?;
              Ok(AbstractType::Type(Type::Array(
                Some(ArraySize::Unsized),
                Box::new(inner_type.known().into()),
              )))
            }
          }
        } else {
          return err(InvalidArraySignature, source_trace);
        }
      }
      _ => err(InvalidStructFieldType, tree.position().clone().into()),
    }
  }
  pub fn from_name(
    name: Rc<str>,
    position: DocumentPosition,
    typedefs: &TypeDefs,
    generic_args: &Vec<Rc<str>>,
    skolems: &Vec<Rc<str>>,
  ) -> CompileResult<Self> {
    Ok(if generic_args.contains(&name) {
      AbstractType::Generic(name.into())
    } else {
      AbstractType::Type(Type::from_name(name, position, typedefs, skolems)?)
    })
  }
  pub fn extract_generic_bindings(
    &self,
    concrete_type: &Type,
    generic_bindings: &mut HashMap<Rc<str>, Type>,
  ) {
    match self {
      AbstractType::Generic(generic) => {
        generic_bindings.insert(generic.clone(), concrete_type.clone());
      }
      AbstractType::AbstractStruct(abstract_struct) => {
        if let Type::Struct(s) = concrete_type {
          abstract_struct.extract_generic_bindings(s, generic_bindings);
        } else {
          panic!("incompatible types in extract_generic_bindings")
        }
      }
      AbstractType::AbstractEnum(abstract_enum) => {
        if let Type::Enum(e) = concrete_type {
          abstract_enum.extract_generic_bindings(e, generic_bindings);
        } else {
          panic!("incompatible types in extract_generic_bindings")
        }
      }
      _ => {}
    }
  }
  pub fn rename_generic(self, old_name: &str, new_name: &str) -> Self {
    match self {
      AbstractType::Generic(name) => {
        AbstractType::Generic(if &*name == old_name {
          new_name.into()
        } else {
          name
        })
      }
      AbstractType::AbstractStruct(s) => {
        let mut s = Rc::unwrap_or_clone(s);
        s.generic_args = s
          .generic_args
          .into_iter()
          .map(|(name, source)| {
            (
              if &*name == old_name {
                new_name.into()
              } else {
                name
              },
              source,
            )
          })
          .collect();
        s.fields = s
          .fields
          .into_iter()
          .map(|mut f| {
            f.field_type = f.field_type.rename_generic(old_name, new_name);
            f
          })
          .collect();
        AbstractType::AbstractStruct(Rc::new(s))
      }
      other => other,
    }
  }
  pub fn data_size_in_u32s(
    &self,
    source_trace: &SourceTrace,
  ) -> CompileResult<usize> {
    Ok(match self {
      AbstractType::Unit => 0,
      AbstractType::Generic(_) => panic!(
        "encountered Generic while calculating data_size_in_u32s, this should \
        never happen"
      ),
      AbstractType::Type(t) => t.data_size_in_u32s(source_trace)?,
      AbstractType::AbstractStruct(s) => s
        .fields
        .iter()
        .map(|f| f.field_type.data_size_in_u32s(source_trace))
        .collect::<CompileResult<Vec<usize>>>()?
        .into_iter()
        .sum::<usize>(),
      AbstractType::AbstractEnum(e) => e.inner_data_size_in_u32s()? + 1,
      AbstractType::AbstractArray {
        size,
        inner_type,
        source_trace,
      } => {
        inner_type.data_size_in_u32s(source_trace)?
          * match size {
            ArraySize::Literal(x) => *x as usize,
            ArraySize::Constant(_) | ArraySize::Unsized => {
              return Err(CompileError::new(
                CompileErrorKind::CantCalculateSize,
                source_trace.clone(),
              ));
            }
          }
      }
    })
  }
  pub fn is_vec4f(&self) -> bool {
    match self {
      AbstractType::Type(t) => t.is_vec4f(),
      AbstractType::AbstractStruct(s) => s.is_vec4f(),
      _ => false,
    }
  }
  pub fn is_unitlike(&self, names: &mut NameContext) -> bool {
    match self {
      AbstractType::Unit => true,
      AbstractType::Generic(_) => false,
      AbstractType::Type(t) => t.is_unitlike(names),
      AbstractType::AbstractStruct(abstract_struct) => !abstract_struct
        .fields
        .iter()
        .any(|f| !f.field_type.is_unitlike(names)),
      AbstractType::AbstractEnum(abstract_enum) => {
        if abstract_enum.variants.len() <= 1 {
          abstract_enum
            .variants
            .get(0)
            .map(|v| v.inner_type.is_unitlike(names))
            .unwrap_or(true)
        } else {
          false
        }
      }
      AbstractType::AbstractArray {
        size, inner_type, ..
      } => {
        inner_type.is_unitlike(names)
          || match size {
            ArraySize::Literal(s) => *s == 0,
            ArraySize::Constant(_) => false,
            ArraySize::Unsized => false,
          }
      }
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArraySize {
  Literal(u32),
  Constant(String),
  Unsized,
}

impl ArraySize {
  pub fn compile_type(&self) -> String {
    match self {
      ArraySize::Literal(size) => format!(", {size}"),
      ArraySize::Constant(name) => {
        format!(", {}", compile_word(format!("{name}").into()))
      }
      ArraySize::Unsized => String::new(),
    }
  }
}

impl Display for ArraySize {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        ArraySize::Literal(size) => format!("{size}"),
        ArraySize::Constant(name) => compile_word((**name).into()),
        ArraySize::Unsized => String::new(),
      }
    )
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
  Unit,
  F32,
  I32,
  U32,
  Bool,
  Struct(Struct),
  Enum(Enum),
  Function(Box<FunctionSignature>),
  Skolem(Rc<str>),
  Array(Option<ArraySize>, Box<ExpTypeInfo>),
}
impl Type {
  pub fn is_unitlike(&self, names: &mut NameContext) -> bool {
    match self {
      Type::Unit => true,
      Type::Struct(s) => !s.fields.iter().any(|f| {
        !f.field_type.kind.with_dereferenced(|f| match f {
          TypeState::Known(t) => t.is_unitlike(names),
          _ => false,
        })
      }),
      Type::Enum(e) => {
        if e.variants.len() <= 1 {
          e.variants
            .get(0)
            .map(|v| {
              v.inner_type.kind.with_dereferenced(|f| match f {
                TypeState::Known(t) => t.is_unitlike(names),
                _ => false,
              })
            })
            .unwrap_or(true)
        } else {
          false
        }
      }
      Type::Function(function_signature) => function_signature
        .abstract_ancestor
        .as_ref()
        .map(|f| f.representative_type(names).is_unitlike(names))
        .unwrap_or(false),
      Type::Array(array_size, inner_type) => {
        inner_type.kind.with_dereferenced(|f| match f {
          TypeState::Known(t) => t.is_unitlike(names),
          _ => false,
        }) || match array_size {
          Some(size) => match size {
            ArraySize::Literal(size) => *size == 0,
            ArraySize::Constant(_) => false,
            ArraySize::Unsized => false,
          },
          None => false,
        }
      }
      _ => false,
    }
  }
  pub fn data_size_in_u32s(
    &self,
    source_trace: &SourceTrace,
  ) -> CompileResult<usize> {
    Ok(match self {
      Type::Unit => 0,
      Type::F32 | Type::I32 | Type::U32 | Type::Bool => 1,
      Type::Struct(s) => s
        .fields
        .iter()
        .map(|f| f.field_type.unwrap_known().data_size_in_u32s(source_trace))
        .collect::<CompileResult<Vec<usize>>>()?
        .into_iter()
        .sum::<usize>(),
      Type::Enum(e) => e.inner_data_size_in_u32s()? + 1,
      Type::Function(_) => {
        return err(UninlinableHigherOrderFunction, source_trace.clone());
      }
      Type::Skolem(_) => panic!("tried to calculate size of skolem"),
      Type::Array(size, inner_type) => {
        inner_type.unwrap_known().data_size_in_u32s(source_trace)?
          * match size {
            Some(ArraySize::Literal(x)) => *x as usize,
            _ => {
              return Err(CompileError::new(
                CompileErrorKind::CantCalculateSize,
                source_trace.clone(),
              ));
            }
          }
      }
    })
  }
  pub fn satisfies_constraints(&self, constraint: &TypeConstraint) -> bool {
    match constraint.kind {
      TypeConstraintKind::Scalar => {
        *self == Type::I32 || *self == Type::F32 || *self == Type::U32
      }
      TypeConstraintKind::ScalarOrBool => {
        *self == Type::I32
          || *self == Type::F32
          || *self == Type::U32
          || *self == Type::Bool
      }
      TypeConstraintKind::Integer => *self == Type::I32 || *self == Type::U32,
    }
  }
  pub fn from_easl_tree(
    tree: EaslTree,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
  ) -> CompileResult<Self> {
    match tree {
      EaslTree::Leaf(position, type_name) => {
        Type::from_name(type_name.into(), position, typedefs, skolems)
      }
      EaslTree::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Parens)),
        type_signature_children,
      ) => {
        let source_trace: SourceTrace = position.into();
        let mut signature_leaves = type_signature_children.into_iter();
        match signature_leaves.next() {
          None => Ok(Self::Unit),
          Some(EaslTree::Leaf(_, struct_name))
            if struct_name.as_str() == "Fn" =>
          {
            match (
              signature_leaves.len(),
              signature_leaves.next(),
              signature_leaves.next(),
            ) {
              (
                2,
                Some(EaslTree::Inner(
                  (_, EncloserOrOperator::Encloser(Encloser::Square)),
                  arg_type_asts,
                )),
                Some(return_type_ast),
              ) => Ok(Self::Function(Box::new(FunctionSignature {
                abstract_ancestor: None,
                args: arg_type_asts
                  .into_iter()
                  .map(|arg_type_ast| {
                    Ok((
                      Variable::immutable(
                        Self::from_easl_tree(arg_type_ast, typedefs, skolems)?
                          .known()
                          .into(),
                      ),
                      vec![],
                    ))
                  })
                  .collect::<CompileResult<Vec<_>>>()?,
                return_type: Self::from_easl_tree(
                  return_type_ast,
                  typedefs,
                  skolems,
                )?
                .known()
                .into(),
              }))),
              _ => err(InvalidFunctionType, source_trace),
            }
          }
          Some(EaslTree::Leaf(_, type_name)) => {
            if signature_leaves.len() == 0 {
              return err(InvalidTypeName, source_trace);
            } else {
              let generic_args: Vec<ExpTypeInfo> = signature_leaves
                .map(|signature_arg| {
                  Ok(
                    AbstractType::from_easl_tree(
                      signature_arg,
                      typedefs,
                      skolems,
                    )?
                    .concretize(skolems, typedefs, source_trace.clone())?
                    .known()
                    .into(),
                  )
                })
                .collect::<CompileResult<Vec<ExpTypeInfo>>>()?;
              if let Some(s) =
                typedefs.structs.iter().find(|s| &*s.name.0 == type_name)
              {
                Ok(Type::Struct(AbstractStruct::fill_generics_ordered(
                  Rc::new(s.clone()),
                  generic_args,
                  typedefs,
                  source_trace.clone(),
                )?))
              } else if let Some(e) =
                typedefs.enums.iter().find(|e| &*e.name.0 == type_name)
              {
                Ok(Type::Enum(AbstractEnum::fill_generics_ordered(
                  e.clone(),
                  generic_args,
                  typedefs,
                  source_trace.clone(),
                )?))
              } else {
                return err(NoTypeNamed(type_name.into()), source_trace);
              }
            }
          }
          _ => return err(InvalidTypeName, source_trace),
        }
      }
      EaslTree::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Square)),
        array_children,
      ) => {
        let source_trace: SourceTrace = position.into();
        if array_children.len() == 1 {
          let child = array_children.into_iter().next().unwrap();
          match child {
            EaslTree::Inner(
              (
                position,
                EncloserOrOperator::Operator(Operator::TypeAscription),
              ),
              mut type_annotation_children,
            ) => {
              let source_trace: SourceTrace = position.into();
              if let EaslTree::Leaf(_, num_str) =
                type_annotation_children.remove(0)
              {
                let inner_type = Type::from_easl_tree(
                  type_annotation_children.remove(0),
                  typedefs,
                  skolems,
                )?;
                Ok(Type::Array(
                  Some(if let Ok(array_size) = num_str.parse::<u32>() {
                    ArraySize::Literal(array_size)
                  } else {
                    ArraySize::Constant(num_str.into())
                  }),
                  Box::new(inner_type.known().into()),
                ))
              } else {
                return err(InvalidArraySignature, source_trace);
              }
            }
            other => {
              let inner_type = Type::from_easl_tree(other, typedefs, skolems)?;
              Ok(Type::Array(
                Some(ArraySize::Unsized),
                Box::new(inner_type.known().into()),
              ))
            }
          }
        } else {
          return err(InvalidArraySignature, source_trace);
        }
      }
      other => {
        let source_trace = other.position().clone().into();
        return err(InvalidType(other), source_trace);
      }
    }
  }
  pub fn compatible(&self, other: &Self) -> bool {
    let b = match (self, other) {
      (Type::Function(a), Type::Function(b)) => a.compatible(b),
      (Type::Struct(a), Type::Struct(b)) => a.compatible(b),
      (Type::Enum(a), Type::Enum(b)) => a.compatible(b),
      (Type::Array(count_a, a), Type::Array(count_b, b)) => {
        (count_a.is_none() || count_b.is_none() || count_a == count_b)
          && TypeState::are_compatible(a, b)
      }
      (a, b) => a == b,
    };
    b
  }
  pub fn compatible_with_any(&self, others: &[Self]) -> bool {
    others.iter().find(|x| self.compatible(x)).is_some()
  }
  pub fn filter_compatibles(&self, others: &[Self]) -> Vec<Self> {
    others
      .iter()
      .filter(|x| self.compatible(x))
      .cloned()
      .collect()
  }
  pub fn from_name(
    name: Rc<str>,
    source_position: DocumentPosition,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
  ) -> CompileResult<Self> {
    use Type::*;
    let source_trace: SourceTrace = source_position.into();
    Ok(match &*name {
      "None" => Unit,
      "F32" | "f32" => F32,
      "I32" | "i32" => I32,
      "U32" | "u32" => U32,
      "Bool" | "bool" => Bool,
      _ => {
        if skolems.contains(&name) {
          Skolem(name)
        } else if let Some(s) =
          typedefs.structs.iter().find(|s| s.name.0 == name)
        {
          Struct(AbstractStruct::fill_generics_with_unification_variables(
            Rc::new(s.clone()),
            &typedefs,
            source_trace.clone(),
          )?)
        } else if let Some(e) = typedefs.enums.iter().find(|e| e.name.0 == name)
        {
          Enum(AbstractEnum::fill_generics_with_unification_variables(
            e.clone(),
            &typedefs,
            source_trace.clone(),
          )?)
        } else if let Some(s) = typedefs
          .type_aliases
          .iter()
          .find_map(|(alias, s)| (*alias == name).then(|| s))
        {
          Struct(AbstractStruct::fill_generics_with_unification_variables(
            s.clone(),
            &typedefs,
            source_trace.clone(),
          )?)
        } else {
          return err(UnrecognizedTypeName(name.to_string()), source_trace);
        }
      }
    })
  }
  pub fn monomorphized_name(&self, names: &mut NameContext) -> String {
    match self {
      Type::Unit => "()".to_string(),
      Type::F32 => "f32".to_string(),
      Type::I32 => "i32".to_string(),
      Type::U32 => "u32".to_string(),
      Type::Bool => "bool".to_string(),
      Type::Struct(s) => match &*s.name {
        "Texture2D" => format!(
          "texture_2d<{}>",
          s.fields[0]
            .field_type
            .unwrap_known()
            .monomorphized_name(names)
        ),
        "Sampler" => "sampler".to_string(),
        _ => compile_word(s.monomorphized_name(names)),
      },
      Type::Enum(e) => compile_word(e.monomorphized_name(names)),
      Type::Array(size, inner_type) => {
        format!(
          "array<{}{}>",
          inner_type.monomorphized_name(names),
          size
            .clone()
            .map(|size| format!("{}", size.compile_type()))
            .unwrap_or(String::new())
        )
      }
      Type::Function(f) => {
        if let Some(f) = &f.abstract_ancestor {
          f.representative_type(names).name.0.to_string()
        } else {
          "ANCESTORLESS".to_string()
          /*panic!(
            "Attempted to compile ConcreteFunction type with no abstract ancestor"
          );*/
        }
      }
      Type::Skolem(name) => {
        panic!("Attempted to compile Skolem \"{name}\"")
      }
    }
  }
  pub fn replace_skolems(&mut self, skolems: &HashMap<Rc<str>, Type>) {
    if let Type::Skolem(s) = &self {
      std::mem::swap(self, &mut skolems.get(s).unwrap().clone())
    } else {
      match self {
        Type::Struct(s) => {
          for field in s.fields.iter_mut() {
            field
              .field_type
              .as_known_mut(|t| t.replace_skolems(skolems));
          }
        }
        Type::Enum(e) => {
          for variant in e.variants.iter_mut() {
            variant
              .inner_type
              .as_known_mut(|t| t.replace_skolems(skolems));
          }
        }
        Type::Function(f) => {
          f.return_type.as_known_mut(|t| t.replace_skolems(skolems));
          for (arg, _) in f.args.iter_mut() {
            arg.var_type.as_known_mut(|t| t.replace_skolems(skolems))
          }
        }
        _ => {}
      }
    }
  }
  pub fn bitcastable_chunk_accessors(
    &self,
    value_name: Rc<str>,
  ) -> Vec<TypedExp> {
    match self {
      Type::Unit => vec![],
      Type::F32 | Type::I32 | Type::U32 | Type::Bool => vec![TypedExp {
        data: self.clone().known().into(),
        kind: ExpKind::Name(value_name),
        source_trace: SourceTrace::empty(),
      }],
      Type::Struct(s) => s.bitcastable_chunk_accessors(value_name),
      Type::Enum(e) => {
        let data_array_length = e.inner_data_size_in_u32s().unwrap();
        std::iter::once(TypedExp {
          data: Type::U32.known().into(),
          kind: ExpKind::Access(
            Accessor::Field("discriminant".into()),
            TypedExp {
              data: self.clone().known().into(),
              kind: ExpKind::Name(value_name.clone()),
              source_trace: SourceTrace::empty(),
            }
            .into(),
          ),
          source_trace: SourceTrace::empty(),
        })
        .chain((0..data_array_length).map(|i| {
          TypedExp {
            data: Type::U32.known().into(),
            kind: ExpKind::Application(
              TypedExp {
                data: Type::Array(
                  Some(ArraySize::Literal(data_array_length as u32)),
                  Box::new(Type::U32.known().into()),
                )
                .known()
                .into(),
                kind: ExpKind::Access(
                  Accessor::Field("data".into()),
                  TypedExp {
                    data: self.clone().known().into(),
                    kind: ExpKind::Name(value_name.clone()),
                    source_trace: SourceTrace::empty(),
                  }
                  .into(),
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
        .collect()
      }
      Type::Array(array_size, inner_type) => match array_size {
        Some(ArraySize::Literal(n)) => (0..*n)
          .map(|i| TypedExp {
            data: *inner_type.clone(),
            kind: ExpKind::Application(
              TypedExp {
                data: self.clone().known().into(),
                kind: ExpKind::Name(value_name.clone()),
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
          })
          .collect(),
        Some(ArraySize::Unsized | ArraySize::Constant(_)) | None => {
          panic!("called bitcastable_chunk_accessors on unsized Array")
        }
      },
      _ => {
        panic!("called bitcastable_chunk_accessors on invalid type")
      }
    }
  }
  fn bitcasted_from_enum_data_inner(
    &self,
    enum_value_name: &Rc<str>,
    enum_type: &Enum,
    current_index: usize,
    names: &mut NameContext,
  ) -> (TypedExp, usize) {
    let data_array_access = |offset: usize| TypedExp {
      data: Type::U32.known().into(),
      kind: ExpKind::Application(
        TypedExp {
          data: Type::Array(
            Some(ArraySize::Literal(
              enum_type.inner_data_size_in_u32s().unwrap() as u32,
            )),
            Box::new(Type::U32.known().into()),
          )
          .known()
          .into(),
          kind: ExpKind::Access(
            Accessor::Field("data".into()),
            TypedExp {
              data: Type::Enum(enum_type.clone()).known().into(),
              kind: ExpKind::Name(enum_value_name.clone()),
              source_trace: SourceTrace::empty(),
            }
            .into(),
          ),
          source_trace: SourceTrace::empty(),
        }
        .into(),
        vec![TypedExp {
          data: Type::U32.known().into(),
          kind: ExpKind::NumberLiteral(Number::Int(
            (current_index + offset) as i64,
          )),
          source_trace: SourceTrace::empty(),
        }],
      ),
      source_trace: SourceTrace::empty(),
    };
    let (kind, consumed_indeces) = match self {
      Type::Unit => (ExpKind::Unit, 0),
      Type::F32 | Type::I32 | Type::U32 | Type::Bool => (
        ExpKind::Application(
          TypedExp {
            data: Type::Function(
              FunctionSignature {
                abstract_ancestor: Some(bitcast().into()),
                args: vec![(
                  Variable::immutable(Type::U32.known().into()),
                  vec![],
                )],
                return_type: self.clone().known().into(),
              }
              .into(),
            )
            .known()
            .into(),
            kind: ExpKind::Name(
              format!("bitcast<{}>", self.monomorphized_name(names)).into(),
            ),
            source_trace: SourceTrace::empty(),
          }
          .into(),
          vec![data_array_access(0)],
        ),
        1,
      ),
      Type::Enum(e) => {
        let name = e.monomorphized_name(names);
        let inner_data_array_type: ExpTypeInfo = Type::Array(
          Some(ArraySize::Literal(
            e.inner_data_size_in_u32s().unwrap() as u32
          )),
          Box::new(Type::U32.known().into()),
        )
        .known()
        .into();
        let inner_data_size = e.inner_data_size_in_u32s().unwrap();
        (
          ExpKind::Application(
            TypedExp {
              data: Type::Function(
                FunctionSignature {
                  abstract_ancestor: None,
                  args: vec![
                    (Variable::immutable(Type::U32.known().into()), vec![]),
                    (
                      Variable::immutable(inner_data_array_type.clone()),
                      vec![],
                    ),
                  ],
                  return_type: Type::Enum(e.clone()).known().into(),
                }
                .into(),
              )
              .known()
              .into(),
              kind: ExpKind::Name(name.into()),
              source_trace: SourceTrace::empty(),
            }
            .into(),
            vec![
              data_array_access(0),
              TypedExp {
                data: inner_data_array_type,
                kind: ExpKind::ArrayLiteral(
                  (0..inner_data_size)
                    .map(|i| data_array_access(i + 1))
                    .collect(),
                ),
                source_trace: SourceTrace::empty(),
              },
            ],
          ),
          inner_data_size + 1,
        )
      }
      Type::Struct(s) => {
        let (constructor_args, consumed_indeces) = s.fields.iter().fold(
          (vec![], 0),
          |(mut constructor_args, consumed_indeces), field| {
            let (arg, arg_consumed_indeces) = field
              .field_type
              .unwrap_known()
              .bitcasted_from_enum_data_inner(
                enum_value_name,
                enum_type,
                current_index + consumed_indeces,
                names,
              );
            constructor_args.push(arg);
            (constructor_args, consumed_indeces + arg_consumed_indeces)
          },
        );
        (
          ExpKind::Application(
            TypedExp {
              data: Type::Function(Box::new(FunctionSignature {
                abstract_ancestor: None,
                args: s
                  .fields
                  .iter()
                  .map(|field| {
                    (Variable::immutable(field.field_type.clone()), vec![])
                  })
                  .collect(),
                return_type: Type::Struct(s.clone()).known().into(),
              }))
              .known()
              .into(),
              kind: ExpKind::Name(s.name.clone()),
              source_trace: SourceTrace::empty(),
            }
            .into(),
            constructor_args,
          ),
          consumed_indeces,
        )
      }
      Type::Array(size, inner_type) => {
        let Some(ArraySize::Literal(size)) = size else {
          panic!("tried to construct unsized array form enum data")
        };
        let size = *size as usize;
        let inner_type = inner_type.unwrap_known();
        let inner_type_size =
          inner_type.data_size_in_u32s(&SourceTrace::empty()).unwrap();
        (
          ExpKind::ArrayLiteral(
            (0..size)
              .map(|i| {
                inner_type
                  .bitcasted_from_enum_data_inner(
                    enum_value_name,
                    enum_type,
                    current_index + i * inner_type_size,
                    names,
                  )
                  .0
              })
              .collect(),
          ),
          size * inner_type_size,
        )
      }
      _ => {
        panic!("called bitcasted_from_enum_data_inner on invalid type")
      }
    };
    (
      TypedExp {
        data: self.clone().known().into(),
        kind,
        source_trace: SourceTrace::empty(),
      },
      consumed_indeces,
    )
  }
  pub fn bitcasted_from_enum_data(
    &self,
    enum_value_name: Rc<str>,
    enum_type: &Enum,
    names: &mut NameContext,
  ) -> TypedExp {
    self
      .bitcasted_from_enum_data_inner(&enum_value_name, enum_type, 0, names)
      .0
  }
  pub fn replace_skolems_with_unification_variables(
    &mut self,
    replacements: &HashMap<Rc<str>, ExpTypeInfo>,
  ) {
    match self {
      Type::Struct(s) => {
        for f in s.fields.iter_mut() {
          f.field_type
            .replace_skolems_with_unification_variables(replacements);
        }
      }
      Type::Enum(e) => {
        for v in e.variants.iter_mut() {
          v.inner_type
            .replace_skolems_with_unification_variables(replacements);
        }
      }
      Type::Function(f) => {
        for (arg, _) in f.args.iter_mut() {
          arg
            .var_type
            .replace_skolems_with_unification_variables(replacements);
        }
        f.return_type
          .replace_skolems_with_unification_variables(replacements);
      }
      Type::Array(_, t) => {
        t.replace_skolems_with_unification_variables(replacements)
      }
      _ => {}
    }
  }
  pub fn known(self) -> TypeState {
    TypeState::Known(self)
  }
  pub fn required_address_space(&self) -> Option<VariableAddressSpace> {
    if let Type::Struct(s) = self
      && (&*s.name == "Texture2D" || &*s.name == "Sampler")
    {
      Some(VariableAddressSpace::Handle)
    } else {
      None
    }
  }
  pub fn check_is_fully_known(&self) -> bool {
    match self {
      Type::Struct(s) => !s
        .fields
        .iter()
        .find(|field| !field.field_type.check_is_fully_known())
        .is_some(),
      Type::Enum(e) => !e
        .variants
        .iter()
        .find(|variant| !variant.inner_type.check_is_fully_known())
        .is_some(),
      Type::Function(function_signature) => {
        function_signature.args.iter().fold(
          function_signature.return_type.check_is_fully_known(),
          |typed_so_far, (arg_var, _)| {
            typed_so_far && arg_var.var_type.check_is_fully_known()
          },
        )
      }
      Type::Array(size, inner_type) => {
        size.is_some() && inner_type.check_is_fully_known()
      }
      _ => true,
    }
  }
  pub fn is_attributable(&self) -> bool {
    match self {
      Type::F32 | Type::I32 | Type::U32 | Type::Bool => true,
      Type::Struct(s) => match &*s.abstract_ancestor.original_ancestor().name.0
      {
        "vec2" | "vec3" | "vec4" => {
          match s.fields.first().unwrap().field_type.unwrap_known() {
            Type::F32 | Type::I32 | Type::U32 => true,
            _ => false,
          }
        }
        _ => false,
      },
      _ => false,
    }
  }
  pub fn is_location_attributable(&self) -> bool {
    *self != Type::Bool && self.is_attributable()
  }
  pub fn is_vector(&self) -> bool {
    if let Type::Struct(s) = self
      && (&*s.name == "vec2" || &*s.name == "vec3" || &*s.name == "vec4")
    {
      true
    } else {
      false
    }
  }
  pub fn is_vec3u(&self) -> bool {
    if let Type::Struct(s) = self
      && &*s.name == "vec3"
      && s
        .fields
        .get(0)
        .map(|f| f.field_type.unwrap_known() == Type::U32)
        .unwrap_or(false)
    {
      true
    } else {
      false
    }
  }
  pub fn is_vec4f(&self) -> bool {
    if let Type::Struct(s) = self
      && &*s.name == "vec4"
      && s
        .fields
        .get(0)
        .map(|f| f.field_type.unwrap_known() == Type::F32)
        .unwrap_or(false)
    {
      true
    } else {
      false
    }
  }
}

pub fn extract_type_annotation_ast(
  exp: EaslTree,
) -> (Option<EaslTree>, EaslTree) {
  if let EaslTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAscription)),
    mut children,
  ) = exp
  {
    (Some(children.remove(1)), children.remove(0))
  } else {
    (None, exp)
  }
}

pub fn extract_type_annotation(
  exp: EaslTree,
  typedefs: &TypeDefs,
  skolems: &Vec<Rc<str>>,
) -> CompileResult<(Option<AbstractType>, EaslTree)> {
  let (t, value) = extract_type_annotation_ast(exp);
  Ok((
    t.map(|t| AbstractType::from_easl_tree(t, typedefs, skolems))
      .map_or(Ok(None), |v| v.map(Some))?,
    value,
  ))
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpTypeInfo {
  pub kind: TypeState,
  pub ownership: Ownership,
  pub subtree_fully_typed: bool,
  pub errored: bool,
  pub fully_known_cached: bool,
}

impl Deref for ExpTypeInfo {
  type Target = TypeState;

  fn deref(&self) -> &Self::Target {
    &self.kind
  }
}

impl DerefMut for ExpTypeInfo {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.kind
  }
}

impl From<TypeState> for ExpTypeInfo {
  fn from(kind: TypeState) -> Self {
    ExpTypeInfo {
      kind,
      ownership: Ownership::Owned,
      subtree_fully_typed: false,
      fully_known_cached: false,
      errored: false,
    }
  }
}

impl ExpTypeInfo {
  pub fn is_fully_known(&mut self) -> bool {
    if self.fully_known_cached {
      return true;
    }
    if self.check_is_fully_known() {
      self.fully_known_cached = true;
    }
    self.fully_known_cached
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeState {
  Unknown,
  OneOf(Vec<Type>),
  Known(Type),
  UnificationVariable(Rc<RefCell<TypeState>>),
}

impl TypeState {
  pub fn as_fn_type_if_known(
    &mut self,
    err_fn: impl Fn() -> CompileError,
  ) -> CompileResult<Option<&mut FunctionSignature>> {
    if let TypeState::Known(t) = self {
      if let Type::Function(signature) = t {
        Ok(Some(signature))
      } else {
        Err(err_fn())
      }
    } else {
      Ok(None)
    }
  }
  pub fn check_is_fully_known(&self) -> bool {
    self.with_dereferenced(|typestate| {
      if let TypeState::Known(t) = typestate {
        t.check_is_fully_known()
      } else {
        false
      }
    })
  }
  pub fn unwrap_known(&self) -> Type {
    self.with_dereferenced(|typestate| {
      if let TypeState::Known(t) = typestate {
        t.clone()
      } else {
        panic!("unwrapped non-Known TypeState \"{typestate:?}\"")
      }
    })
  }
  pub fn as_known_mut<O>(&mut self, f: impl FnOnce(&mut Type) -> O) -> O {
    self.with_dereferenced_mut(|typestate| {
      if let TypeState::Known(t) = typestate {
        f(t)
      } else {
        panic!("as_known_mut on a non-Known TypeState")
      }
    })
  }
  pub fn any_of(possibilities: Vec<TypeState>) -> Self {
    let mut type_possibilities = vec![];
    for possibility in possibilities {
      match possibility {
        TypeState::Unknown => {}
        TypeState::OneOf(mut new_possibilities) => {
          type_possibilities.append(&mut new_possibilities);
        }
        TypeState::Known(t) => type_possibilities.push(t),
        TypeState::UnificationVariable(_) => {
          panic!("can't handle UnificationVariable in any_of :(")
        }
      }
    }
    Self::OneOf(type_possibilities).simplified()
  }
  pub fn fresh_unification_variable() -> Self {
    TypeState::UnificationVariable(Rc::new(RefCell::new(TypeState::Unknown)))
  }
  pub fn with_dereferenced<T>(&self, f: impl FnOnce(&Self) -> T) -> T {
    match self {
      TypeState::UnificationVariable(var) => {
        (&*var.borrow()).with_dereferenced(f)
      }
      other => f(other),
    }
  }
  pub fn with_dereferenced_mut<T>(
    &mut self,
    f: impl FnOnce(&mut Self) -> T,
  ) -> T {
    match self {
      TypeState::UnificationVariable(var) => {
        (&mut *var.borrow_mut()).with_dereferenced_mut(f)
      }
      other => f(other),
    }
  }
  pub fn are_compatible<'a>(a: &'a Self, b: &'a Self) -> bool {
    use TypeState::*;
    a.with_dereferenced(|a| {
      b.with_dereferenced(|b| match (a, b) {
        (Unknown, _) => true,
        (_, Unknown) => true,
        (UnificationVariable(_), _) => unreachable!(),
        (_, UnificationVariable(_)) => unreachable!(),
        (Known(a), Known(b)) => a.compatible(b),
        (OneOf(a), Known(b)) => b.compatible_with_any(a),
        (Known(a), OneOf(b)) => a.compatible_with_any(b),
        (OneOf(a), OneOf(b)) => {
          a.iter().find(|a| a.compatible_with_any(b)).is_some()
        }
      })
    })
  }
  pub fn constrain(
    &mut self,
    mut other: TypeState,
    source_trace: &SourceTrace,
    errors: &mut ErrorLog,
  ) -> bool {
    if *self == other {
      return false;
    }
    self.with_dereferenced_mut(move |mut this| {
      other.with_dereferenced_mut(|mut other| {
        let result = match (&mut this, &mut other) {
          (TypeState::UnificationVariable(_), _) => unreachable!(),
          (_, TypeState::UnificationVariable(_)) => unreachable!(),
          (_, TypeState::Unknown) => false,
          (TypeState::Unknown, _) => {
            std::mem::swap(this, &mut other.clone());
            true
          }
          (TypeState::Known(current_type), TypeState::Known(other_type)) => {
            if !current_type.compatible(&other_type) {
              errors.log(CompileError::new(
                IncompatibleTypes(this.clone().into(), other.clone().into()),
                source_trace.clone(),
              ));
              false
            } else {
              match (current_type, other_type) {
                (
                  Type::Function(signature),
                  Type::Function(other_signature),
                ) => {
                  let mut anything_changed = signature.return_type.constrain(
                    other_signature.return_type.kind.clone(),
                    source_trace,
                    errors,
                  );
                  for ((t, _), (other_t, _)) in signature
                    .args
                    .iter_mut()
                    .zip(other_signature.args.iter_mut())
                  {
                    let changed = t.var_type.constrain(
                      other_t.var_type.kind.clone(),
                      source_trace,
                      errors,
                    );
                    anything_changed |= changed;
                  }
                  anything_changed
                }
                (Type::Struct(s), Type::Struct(other_s)) => {
                  let mut anything_changed = false;
                  for (t, other_t) in
                    s.fields.iter_mut().zip(other_s.fields.iter_mut())
                  {
                    let changed = t.field_type.constrain(
                      other_t.field_type.kind.clone(),
                      source_trace,
                      errors,
                    );
                    anything_changed |= changed;
                  }
                  anything_changed
                }
                (Type::Enum(e), Type::Enum(other_e)) => {
                  let mut anything_changed = false;
                  for (v, other_v) in
                    e.variants.iter_mut().zip(other_e.variants.iter_mut())
                  {
                    let changed = v.inner_type.constrain(
                      other_v.inner_type.kind.clone(),
                      source_trace,
                      errors,
                    );
                    anything_changed |= changed;
                  }
                  anything_changed
                }
                (
                  Type::Array(size, inner_type),
                  Type::Array(other_size, other_inner_type),
                ) => {
                  let changed = inner_type.constrain(
                    other_inner_type.kind.clone(),
                    source_trace,
                    errors,
                  );
                  if let Some(other_size) = other_size {
                    if let Some(size) = &size {
                      if size != other_size {
                        errors.log(CompileError::new(
                          IncompatibleTypes(
                            this.clone().into(),
                            other.clone().into(),
                          ),
                          source_trace.clone(),
                        ));
                      }
                    } else {
                      std::mem::swap(size, &mut Some(other_size.clone()))
                    }
                  }
                  changed
                }
                _ => false,
              }
            }
          }
          (
            TypeState::OneOf(possibilities),
            TypeState::OneOf(other_possibilities),
          ) => {
            let mut new_possibilities = vec![];
            let mut changed = false;
            for possibility in possibilities {
              if possibility.compatible_with_any(other_possibilities) {
                new_possibilities.push(possibility.clone());
              } else {
                changed = true;
              }
            }
            std::mem::swap(
              this,
              &mut TypeState::OneOf(new_possibilities).simplified(),
            );
            changed
          }
          (TypeState::OneOf(possibilities), TypeState::Known(t)) => {
            if t.compatible_with_any(&possibilities) {
              std::mem::swap(this, &mut t.clone().known());
              true
            } else {
              errors.log(CompileError::new(
                IncompatibleTypes(this.clone().into(), other.clone().into()),
                source_trace.clone(),
              ));
              false
            }
          }
          (TypeState::Known(t), TypeState::OneOf(possibilities)) => {
            if !t.compatible_with_any(&possibilities) {
              errors.log(CompileError::new(
                IncompatibleTypes(this.clone().into(), other.clone().into()),
                source_trace.clone(),
              ));
            }
            false
          }
        };
        this.simplify();
        result
      })
    })
  }
  pub fn mutually_constrain(
    &mut self,
    other: &mut TypeState,
    source_trace: &SourceTrace,
    errors: &mut ErrorLog,
  ) -> bool {
    let self_changed = self.constrain(other.clone(), source_trace, errors);
    let other_changed = other.constrain(self.clone(), source_trace, errors);
    self_changed || other_changed
  }
  pub fn constrain_fn_by_argument_types(
    &mut self,
    mut arg_types: Vec<&mut TypeState>,
    source_trace: &SourceTrace,
    errors: &mut ErrorLog,
  ) -> bool {
    self.with_dereferenced_mut(|typestate| match typestate {
      TypeState::OneOf(possibilities) => {
        let mut anything_changed = false;
        let mut new_possibilities: Vec<Type> = vec![];
        for possibility in possibilities {
          match possibility {
            Type::Function(signature) => {
              if signature.are_args_compatible(
                &arg_types.iter().map(|t| (*t).clone()).collect(),
              ) {
                new_possibilities.push(Type::Function(signature.clone()))
              } else {
                anything_changed = true;
              }
            }
            Type::Array(size, inner_type) => {
              if arg_types.len() == 1
                && TypeState::are_compatible(
                  &arg_types[0],
                  &TypeState::OneOf(vec![Type::U32, Type::I32]),
                )
              {
                new_possibilities
                  .push(Type::Array(size.clone(), inner_type.clone()))
              } else {
                anything_changed = true;
              }
            }
            _ => errors.log(CompileError::new(
              ExpectedFunctionFoundNonFunction,
              source_trace.clone(),
            )),
          }
        }
        if new_possibilities.is_empty() {
          errors.log(CompileError::new(
            FunctionArgumentTypesIncompatible {
              f: typestate.clone().into(),
              args: arg_types.into_iter().map(|t| t.clone().into()).collect(),
            },
            source_trace.clone(),
          ));
          false
        } else {
          std::mem::swap(
            typestate,
            &mut TypeState::OneOf(new_possibilities).simplified(),
          );
          anything_changed
        }
      }
      TypeState::Known(t) => match t {
        Type::Function(signature) => {
          let changed = signature.mutually_constrain_arguments(
            arg_types,
            source_trace.clone(),
            errors,
          );
          changed
        }
        Type::Array(_, _) => arg_types[0].constrain(
          TypeState::OneOf(vec![Type::I32, Type::U32]),
          source_trace,
          errors,
        ),
        _ => {
          errors.log(CompileError::new(
            ExpectedFunctionFoundNonFunction,
            source_trace.clone(),
          ));
          false
        }
      },
      TypeState::Unknown => false,
      TypeState::UnificationVariable(_) => unreachable!(),
    })
  }
  pub fn simplify(&mut self) {
    if let TypeState::OneOf(mut possibilities) = self.clone() {
      possibilities.dedup();
      std::mem::swap(
        self,
        &mut match possibilities.len() {
          0 => unreachable!(),
          1 => possibilities.remove(0).known(),
          _ => TypeState::OneOf(possibilities),
        },
      )
    }
  }
  pub fn simplified(mut self) -> Self {
    self.simplify();
    self
  }
  pub fn monomorphized_name(&self, names: &mut NameContext) -> String {
    self.unwrap_known().monomorphized_name(names)
  }
  pub fn replace_skolems_with_unification_variables(
    &mut self,
    replacements: &HashMap<Rc<str>, ExpTypeInfo>,
  ) {
    if let Some(replacement) =
      self.with_dereferenced_mut(|typestate| match typestate {
        TypeState::OneOf(types) => {
          for t in types.iter_mut() {
            t.replace_skolems_with_unification_variables(replacements);
          }
          None
        }
        TypeState::Known(t) => {
          if let Type::Skolem(name) = t
            && let Some(replacement) = replacements.get(name)
          {
            Some(replacement.kind.clone())
          } else {
            t.replace_skolems_with_unification_variables(replacements);
            None
          }
        }
        _ => None,
      })
    {
      *self = replacement;
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VariableKind {
  Let,
  Var,
  Override,
}

impl VariableKind {
  pub fn compile(self) -> &'static str {
    match self {
      VariableKind::Let => "let",
      VariableKind::Var => "var",
      VariableKind::Override => "override",
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
  pub kind: VariableKind,
  pub var_type: ExpTypeInfo,
}

impl Variable {
  pub fn immutable(var_type: ExpTypeInfo) -> Self {
    Self {
      var_type,
      kind: VariableKind::Let,
    }
  }
  pub fn mutable(var_type: ExpTypeInfo) -> Self {
    Self {
      var_type,
      kind: VariableKind::Var,
    }
  }
  pub fn with_kind(mut self, kind: VariableKind) -> Self {
    self.kind = kind;
    self
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeConstraintKind {
  Scalar,
  ScalarOrBool,
  Integer,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeConstraint {
  kind: TypeConstraintKind,
  args: Vec<Vec<TypeConstraint>>,
}

impl TypeConstraint {
  pub fn name(&self) -> String {
    match self.kind {
      TypeConstraintKind::Scalar => "Scalar",
      TypeConstraintKind::ScalarOrBool => "ScalarOrBool",
      TypeConstraintKind::Integer => "Integer",
    }
    .to_string()
  }
  pub fn scalar() -> Self {
    Self {
      kind: TypeConstraintKind::Scalar,
      args: vec![],
    }
  }
  pub fn scalar_or_bool() -> Self {
    Self {
      kind: TypeConstraintKind::ScalarOrBool,
      args: vec![],
    }
  }
  pub fn integer() -> Self {
    Self {
      kind: TypeConstraintKind::Integer,
      args: vec![],
    }
  }
}

pub fn parse_type_constraint(
  ast: EaslTree,
  _typedefs: &TypeDefs,
  _generic_args: &Vec<Rc<str>>,
) -> CompileResult<TypeConstraint> {
  match ast {
    EaslTree::Leaf(position, name) => match name.as_str() {
      "Scalar" => Ok(TypeConstraint::scalar()),
      "ScalarOrBool" => Ok(TypeConstraint::scalar_or_bool()),
      "Integer" => Ok(TypeConstraint::integer()),
      _ => err(TypeConstraintsNotYetSupported, position.into()),
    },
    EaslTree::Inner(
      (position, EncloserOrOperator::Operator(Operator::TypeAscription)),
      _children,
    ) => {
      err(TypeConstraintsNotYetSupported, position.into())
      /*let source_trace: SourceTrace = position.into();
      let mut children_iter = children.into_iter();
      let name = if let EaslTree::Leaf(_, name) =
        children_iter.next().ok_or_else(|| {
          CompileError::new(InvalidTypeConstraint, source_trace.clone())
        })? {
        name.into()
      } else {
        return err(InvalidTypeConstraint, source_trace);
      };
      let args = children_iter
        .map(|child_ast| {
          AbstractType::from_easl_tree(
            child_ast,
            structs,
            aliases,
            generic_args,
          )
        })
        .collect::<CompileResult<Vec<AbstractType>>>()?;
      Ok(TypeConstraint { name, args })*/
    }
    _ => err(InvalidTypeConstraint, ast.position().clone().into()),
  }
}

pub fn parse_generic_argument(
  ast: EaslTree,
  typedefs: &TypeDefs,
  generic_args: &Vec<Rc<str>>,
) -> CompileResult<(Rc<str>, SourceTrace, Vec<TypeConstraint>)> {
  match ast {
    EaslTree::Leaf(pos, generic_name) => {
      Ok((generic_name.into(), pos.into(), vec![]))
    }
    EaslTree::Inner(
      (position, EncloserOrOperator::Operator(Operator::TypeAscription)),
      mut children,
    ) => {
      if children.len() < 2 {
        return err(
          InvalidDefn("Invalid generic name".into()),
          position.into(),
        );
      }
      let bounds_tree = children.remove(1);
      if let EaslTree::Leaf(_, generic_name) = children.remove(0) {
        match bounds_tree {
          EaslTree::Inner(
            (pos, EncloserOrOperator::Encloser(Encloser::Square)),
            bound_children,
          ) => Ok((
            generic_name.into(),
            pos.into(),
            bound_children
              .into_iter()
              .map(|child_ast| {
                parse_type_constraint(child_ast, typedefs, generic_args)
              })
              .collect::<CompileResult<_>>()?,
          )),
          other => Ok((
            generic_name.into(),
            other.position().into(),
            vec![parse_type_constraint(other, typedefs, generic_args)?],
          )),
        }
      } else {
        err(InvalidDefn("Invalid generic name".into()), position.into())
      }
    }
    _ => err(
      InvalidDefn("Invalid generic name".into()),
      ast.position().clone().into(),
    ),
  }
}

#[derive(Debug)]
pub struct LocalContext<P: Deref<Target = Program>> {
  pub variables: HashMap<Rc<str>, (Variable, SourceTrace)>,
  pub enclosing_function_types: Vec<TypeState>,
  pub inside_pattern: bool,
  pub program: P,
}

impl<P: Deref<Target = Program>> LocalContext<P> {
  pub fn empty(program: P) -> Self {
    Self {
      variables: HashMap::new(),
      enclosing_function_types: vec![],
      inside_pattern: false,
      program,
    }
  }
  pub fn push_enclosing_function_type(&mut self, typestate: TypeState) {
    self.enclosing_function_types.push(typestate);
  }
  pub fn pop_enclosing_function_type(&mut self) {
    self.enclosing_function_types.pop();
  }
  pub fn enclosing_function_type(&mut self) -> Option<&mut TypeState> {
    self.enclosing_function_types.last_mut()
  }
  pub fn bind(&mut self, name: &str, v: Variable, s: SourceTrace) {
    self.variables.insert(name.into(), (v, s));
  }
  pub fn unbind(&mut self, name: &str) -> Variable {
    self.variables.remove(name).unwrap().0
  }
  pub fn is_bound(&self, name: &str) -> bool {
    let name_rc: Rc<str> = name.to_string().into();
    self.variables.contains_key(name)
      || self.program.abstract_functions.contains_key(&name_rc)
      || self
        .program
        .top_level_vars
        .iter()
        .find(|top_level_var| &*top_level_var.name == name)
        .is_some()
      || self
        .program
        .typedefs
        .enums
        .iter()
        .find(|e| e.has_unit_variant_named(name))
        .is_some()
  }
  pub fn get_variable_kind(&self, name: &str) -> VariableKind {
    self
      .variables
      .get(name)
      .map(|(var, _)| var.kind.clone())
      .or(
        self
          .program
          .top_level_vars
          .iter()
          .find_map(|v| (&*v.name == name).then(|| v.variable_kind())),
      )
      .unwrap()
  }
  pub fn get_name_definition_source(
    &self,
    name: &str,
  ) -> Option<NameDefinitionSource> {
    self
      .variables
      .get(name)
      .map(|(_, source_trace)| {
        NameDefinitionSource::LocalBinding(source_trace.primary_path())
      })
      .or(self.program.top_level_vars.iter().find_map(|v| {
        (&*v.name == name).then(|| {
          NameDefinitionSource::GlobalBinding(v.source_trace.primary_path())
        })
      }))
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NameDefinitionSource {
  BuiltInFunction(Vec<usize>),
  Defn(Vec<Vec<usize>>),
  Struct(Vec<usize>),
  Enum(Vec<usize>),
  GlobalBinding(Vec<usize>),
  LocalBinding(Vec<usize>),
}

pub type ImmutableProgramLocalContext<'p> = LocalContext<&'p Program>;

pub type MutableProgramLocalContext<'p> = LocalContext<&'p mut Program>;
impl<'p> MutableProgramLocalContext<'p> {
  pub fn get_variable_ownership(&self, name: &str) -> Option<Ownership> {
    if let Some((var, _)) = self.variables.get(name) {
      Some(var.var_type.ownership)
    } else {
      None
    }
  }
  fn get_typestate_mut(
    &mut self,
    name: &str,
    source_trace: SourceTrace,
  ) -> CompileResult<Result<&mut TypeState, TypeState>> {
    if let Some((var, _)) = self.variables.get_mut(name) {
      Ok(Ok(&mut var.var_type))
    } else if let Some(top_level_var) = self
      .program
      .top_level_vars
      .iter_mut()
      .find(|var| &*var.name == name)
    {
      Ok(Err(top_level_var.var_type.clone().known()))
    } else if let Some(e) = self
      .program
      .typedefs
      .enums
      .iter()
      .find(|e| e.has_unit_variant_named(name))
    {
      Ok(Err(
        Type::Enum(AbstractEnum::fill_generics_with_unification_variables(
          e.clone(),
          &self.program.typedefs,
          source_trace,
        )?)
        .known(),
      ))
    } else {
      Err(CompileError::new(UnboundName(name.into()), source_trace))
    }
  }
  pub fn constrain_name_type(
    &mut self,
    name: &Rc<str>,
    source_trace: &SourceTrace,
    t: &mut ExpTypeInfo,
    errors: &mut ErrorLog,
  ) -> bool {
    match self.program.concrete_signatures(name, source_trace.clone()) {
      Err(e) => {
        errors.log(e);
        false
      }
      Ok(Some(signatures)) => {
        t.constrain(TypeState::OneOf(signatures), source_trace, errors)
      }
      Ok(None) => match self.get_typestate_mut(name, source_trace.clone()) {
        Ok(typestate) => match typestate {
          Ok(typestate) => {
            t.mutually_constrain(typestate, source_trace, errors)
          }
          Err(mut typestate) => {
            t.mutually_constrain(&mut typestate, source_trace, errors)
          }
        },
        Err(e) => {
          errors.log(e);
          false
        }
      },
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeDescription {
  Unit,
  F32,
  I32,
  U32,
  Bool,
  Struct(String),
  Enum(String),
  Function {
    arg_types: Vec<(TypeStateDescription, Vec<TypeConstraintDescription>)>,
    return_type: Box<TypeStateDescription>,
  },
  Skolem(String),
  Array(Option<ArraySize>, Box<TypeStateDescription>),
}
impl From<Type> for TypeDescription {
  fn from(t: Type) -> Self {
    match t {
      Type::Unit => Self::Unit,
      Type::F32 => Self::F32,
      Type::I32 => Self::I32,
      Type::U32 => Self::U32,
      Type::Bool => Self::Bool,
      Type::Struct(s) => Self::Struct(match &*s.name {
        "Texture2D" => format!(
          "(Texture2D {})",
          TypeDescription::from(s.fields[0].field_type.unwrap_known())
            .to_string()
        ),
        _ => {
          compile_word(s.name)
          // todo! this should display a name more like the above one for
          // Texture2D, using a kind of type-level function application syntax
        }
      }),
      Type::Function(f) => Self::Function {
        arg_types: f
          .args
          .into_iter()
          .map(|(var, constraints)| {
            (
              TypeStateDescription::from(var.var_type.kind),
              constraints
                .into_iter()
                .map(TypeConstraintDescription::from)
                .collect(),
            )
          })
          .collect(),
        return_type: TypeStateDescription::from(f.return_type.kind).into(),
      },
      Type::Skolem(name) => Self::Skolem(name.to_string()),
      Type::Array(array_size, t) => {
        Self::Array(array_size, TypeStateDescription::from(t.kind).into())
      }
      Type::Enum(e) => Self::Enum(e.name.to_string()),
    }
  }
}
impl Display for TypeDescription {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Unit => "()".to_string(),
        Self::F32 => "f32".to_string(),
        Self::I32 => "i32".to_string(),
        Self::U32 => "u32".to_string(),
        Self::Bool => "bool".to_string(),
        Self::Struct(name) => name.clone(),
        Self::Enum(name) => name.clone(),
        Self::Array(size, inner_type) => {
          if let Some(size) = size {
            format!("[{}: {}]", size, inner_type.to_string())
          } else {
            format!("[{}]", inner_type.to_string())
          }
        }
        Self::Function {
          arg_types,
          return_type,
        } => {
          format!(
            "(Fn [{}]: {})",
            arg_types
              .iter()
              .map(|(t, _)| t.to_string())
              .collect::<Vec<String>>()
              .join(" "),
            return_type.to_string()
          )
        }
        Self::Skolem(name) => name.to_string(),
      }
    )
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeStateDescription {
  Unknown,
  OneOf(Vec<TypeDescription>),
  Known(TypeDescription),
}
impl From<TypeState> for TypeStateDescription {
  fn from(typestate: TypeState) -> Self {
    typestate.with_dereferenced(|typestate| match typestate {
      TypeState::Unknown => Self::Unknown,
      TypeState::OneOf(items) => {
        Self::OneOf(items.iter().cloned().map(TypeDescription::from).collect())
      }
      TypeState::Known(t) => Self::Known(TypeDescription::from(t.clone())),
      TypeState::UnificationVariable(_) => unreachable!(),
    })
  }
}
impl Display for TypeStateDescription {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Unknown => "?".to_string(),
        Self::OneOf(items) => items
          .into_iter()
          .map(|t| t.to_string())
          .collect::<Vec<String>>()
          .join(" or "),
        Self::Known(t) => t.to_string(),
      }
    )
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeConstraintDescription {
  name: String,
  args: Vec<String>,
}
impl From<TypeConstraint> for TypeConstraintDescription {
  fn from(constraint: TypeConstraint) -> Self {
    Self {
      name: constraint.name().to_string(),
      args: (0..constraint.args.len())
        .map(|i| ((65 + (i as u8)) as char).to_string().into())
        .collect(),
    }
  }
}
impl Display for TypeConstraintDescription {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      if self.args.len() == 0 {
        format!(
          "({} {})",
          self.name,
          self
            .args
            .iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(" ")
        )
      } else {
        self.name.clone().to_string()
      }
    )
  }
}
