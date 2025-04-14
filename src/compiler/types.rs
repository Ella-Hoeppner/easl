use core::fmt::Debug;
use std::{
  cell::RefCell,
  collections::{HashMap, HashSet},
  fmt::Display,
  ops::{Deref, DerefMut},
  rc::Rc,
};

use sse::{document::DocumentPosition, syntax::EncloserOrOperator};

use crate::{
  compiler::error::CompileError,
  parse::{EaslTree, Encloser, Operator},
};

use super::{
  builtins::{
    built_in_functions, built_in_structs, built_in_type_aliases,
    ABNORMAL_CONSTRUCTOR_STRUCTS,
  },
  error::{err, CompileErrorKind::*, CompileResult, SourceTrace},
  expression::{ExpKind, TypedExp},
  functions::{
    AbstractFunctionSignature, FunctionImplementationKind, FunctionSignature,
  },
  structs::{AbstractStruct, Struct},
  util::compile_word,
  vars::TopLevelVar,
};

#[derive(Debug, Clone, PartialEq)]
pub enum AbstractType {
  Generic(Rc<str>),
  Type(Type),
  AbstractStruct(Rc<AbstractStruct>),
  AbstractArray {
    size: ArraySize,
    inner_type: Box<Self>,
  },
  Reference(Box<Self>),
}

impl AbstractType {
  pub fn fill_generics(
    &self,
    generics: &HashMap<Rc<str>, ExpTypeInfo>,
    structs: &Vec<Rc<AbstractStruct>>,
    source_trace: SourceTrace,
  ) -> CompileResult<ExpTypeInfo> {
    Ok(match self {
      AbstractType::Generic(var_name) => generics
        .get(var_name)
        .expect("unrecognized generic name in struct")
        .clone(),
      AbstractType::Type(t) => TypeState::Known(t.clone()).into(),
      AbstractType::AbstractStruct(s) => {
        TypeState::Known(Type::Struct(AbstractStruct::fill_generics(
          s.clone(),
          generics,
          structs,
          source_trace,
        )?))
        .into()
      }
      AbstractType::AbstractArray { size, inner_type } => {
        TypeState::Known(Type::Array(
          Some(size.clone()),
          inner_type
            .fill_generics(generics, structs, source_trace)?
            .into(),
        ))
        .into()
      }
      AbstractType::Reference(inner_type) => TypeState::Known(Type::Reference(
        inner_type
          .fill_generics(generics, structs, source_trace)?
          .into(),
      ))
      .into(),
    })
  }
  pub fn concretize(
    &self,
    skolems: &Vec<Rc<str>>,
    structs: &Vec<Rc<AbstractStruct>>,
    source_trace: SourceTrace,
  ) -> CompileResult<Type> {
    match self {
      AbstractType::Generic(name) => {
        if skolems.contains(name) {
          Ok(Type::Skolem(Rc::clone(name)))
        } else {
          err(UnrecognizedGeneric(name.clone().into()), source_trace)
        }
      }
      AbstractType::AbstractStruct(s) => Ok(Type::Struct(
        AbstractStruct::concretize(s.clone(), structs, skolems, source_trace)?,
      )),
      AbstractType::Type(t) => Ok(t.clone()),
      AbstractType::AbstractArray { size, inner_type } => Ok(Type::Array(
        Some(size.clone()),
        Box::new(
          TypeState::Known(inner_type.concretize(
            skolems,
            structs,
            source_trace,
          )?)
          .into(),
        ),
      )),
      AbstractType::Reference(inner_type) => Ok(Type::Reference(Box::new(
        TypeState::Known(inner_type.concretize(
          skolems,
          structs,
          source_trace,
        )?)
        .into(),
      ))),
    }
  }
  pub fn fill_abstract_generics(
    self,
    generics: &HashMap<Rc<str>, AbstractType>,
  ) -> Self {
    match self {
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
      AbstractType::AbstractArray { size, inner_type } => {
        AbstractType::AbstractArray {
          size,
          inner_type: inner_type.fill_abstract_generics(generics).into(),
        }
      }
      AbstractType::Reference(inner_type) => AbstractType::Reference(
        inner_type.fill_abstract_generics(generics).into(),
      ),
    }
  }
  pub fn compile(
    self,
    structs: &Vec<Rc<AbstractStruct>>,
  ) -> CompileResult<String> {
    Ok(match self {
      AbstractType::Generic(_) => {
        panic!("attempted to compile generic struct field")
      }
      AbstractType::Type(t) => t.compile(),
      AbstractType::AbstractStruct(t) => Rc::unwrap_or_clone(t)
        .compile_if_non_generic(structs)?
        .expect("failed to compile abstract structs"),
      AbstractType::AbstractArray { size, inner_type } => {
        format!(
          "array<{}{}>",
          inner_type.compile(structs)?,
          format!("{}", size.compile_type())
        )
      }
      AbstractType::Reference(_) => {
        unreachable!("compiling abstract reference type, this shouldn't happen")
      }
    })
  }
  pub fn from_easl_tree(
    tree: EaslTree,
    structs: &Vec<Rc<AbstractStruct>>,
    aliases: &Vec<(Rc<str>, Rc<AbstractStruct>)>,
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
            structs,
            aliases,
            skolems,
          )?)
        })
      }
      EaslTree::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Parens)),
        children,
      ) => {
        let mut children_iter = children.iter();
        let generic_struct_name =
          if let Some(EaslTree::Leaf(_, leaf)) = children_iter.next() {
            leaf
          } else {
            return err(InvalidStructName, position.into());
          };
        if generic_struct_name.as_str() == "Fn" {
          Ok(AbstractType::Type(Type::from_easl_tree(
            EaslTree::Inner(
              (position, EncloserOrOperator::Encloser(Encloser::Parens)),
              children,
            ),
            structs,
            aliases,
            skolems,
          )?))
        } else {
          let mut children_iter = children.into_iter();
          let generic_struct_name =
            if let EaslTree::Leaf(_, leaf) = children_iter.next().unwrap() {
              leaf
            } else {
              unreachable!()
            };
          let generic_struct = structs
            .iter()
            .find(|s| &*s.name == generic_struct_name.as_str())
            .ok_or_else(|| {
              CompileError::new(
                NoStructNamed(generic_struct_name.clone().into()),
                position.into(),
              )
            })?
            .clone();
          let generic_args = children_iter
            .map(|subtree: EaslTree| {
              Self::from_easl_tree(subtree, structs, aliases, skolems)
            })
            .collect::<CompileResult<Vec<_>>>()?;
          Ok(AbstractType::AbstractStruct(Rc::new(
            Rc::unwrap_or_clone(generic_struct)
              .fill_abstract_generics(generic_args),
          )))
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
                EncloserOrOperator::Operator(Operator::TypeAnnotation),
              ),
              mut type_annotation_children,
            ) => {
              let source_trace: SourceTrace = position.into();
              if let EaslTree::Leaf(_, num_str) =
                type_annotation_children.remove(0)
              {
                let inner_type = Type::from_easl_tree(
                  type_annotation_children.remove(0),
                  structs,
                  aliases,
                  skolems,
                )?;
                Ok(AbstractType::Type(Type::Array(
                  Some(if let Ok(array_size) = num_str.parse::<u32>() {
                    ArraySize::Constant(array_size)
                  } else {
                    ArraySize::Override(num_str.into())
                  }),
                  Box::new(TypeState::Known(inner_type).into()),
                )))
              } else {
                return err(InvalidArraySignature, source_trace);
              }
            }
            other => {
              let inner_type =
                Type::from_easl_tree(other, structs, aliases, skolems)?;
              Ok(AbstractType::Type(Type::Array(
                Some(ArraySize::Unsized),
                Box::new(TypeState::Known(inner_type).into()),
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
    structs: &Vec<Rc<AbstractStruct>>,
    aliases: &Vec<(Rc<str>, Rc<AbstractStruct>)>,
    generic_args: &Vec<Rc<str>>,
    skolems: &Vec<Rc<str>>,
  ) -> CompileResult<Self> {
    Ok(if generic_args.contains(&name) {
      AbstractType::Generic(name.into())
    } else {
      AbstractType::Type(Type::from_name(
        name, position, structs, aliases, skolems,
      )?)
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
          for i in 0..s.fields.len() {
            abstract_struct.fields[i]
              .field_type
              .extract_generic_bindings(
                &s.fields[i].field_type.unwrap_known(),
                generic_bindings,
              );
          }
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
          .map(|name| {
            if &*name == old_name {
              new_name.into()
            } else {
              name
            }
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArraySize {
  Constant(u32),
  Override(Rc<str>),
  Unsized,
}

impl ArraySize {
  pub fn compile_type(&self) -> String {
    match self {
      ArraySize::Constant(size) => format!(", {size}"),
      ArraySize::Override(name) => format!(", {name}"),
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
        ArraySize::Constant(size) => format!("{size}"),
        ArraySize::Override(name) => compile_word(name.clone()),
        ArraySize::Unsized => String::new(),
      }
    )
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
  None,
  F32,
  I32,
  U32,
  Bool,
  Struct(Struct),
  Function(Box<FunctionSignature>),
  Skolem(Rc<str>),
  Array(Option<ArraySize>, Box<ExpTypeInfo>),
  Reference(Box<ExpTypeInfo>),
}
impl Type {
  pub fn satisfies_constraints(&self, constraint: &TypeConstraint) -> bool {
    match &*constraint.name {
      "Scalar" => {
        *self == Type::I32 || *self == Type::F32 || *self == Type::U32
      }
      "Integer" => *self == Type::I32 || *self == Type::U32,
      _ => todo!("custom constraints not yet supported"),
    }
  }
  pub fn from_easl_tree(
    tree: EaslTree,
    structs: &Vec<Rc<AbstractStruct>>,
    aliases: &Vec<(Rc<str>, Rc<AbstractStruct>)>,
    skolems: &Vec<Rc<str>>,
  ) -> CompileResult<Self> {
    match tree {
      EaslTree::Leaf(position, type_name) => {
        Type::from_name(type_name.into(), position, structs, aliases, skolems)
      }
      EaslTree::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Parens)),
        struct_signature_children,
      ) => {
        let source_trace: SourceTrace = position.into();
        let mut signature_leaves = struct_signature_children.into_iter();
        match signature_leaves.next() {
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
                arg_types: arg_type_asts
                  .into_iter()
                  .map(|arg_type_ast| {
                    Ok((
                      TypeState::Known(Self::from_easl_tree(
                        arg_type_ast,
                        structs,
                        aliases,
                        skolems,
                      )?)
                      .into(),
                      vec![],
                    ))
                  })
                  .collect::<CompileResult<Vec<_>>>()?,
                return_type: TypeState::Known(Self::from_easl_tree(
                  return_type_ast,
                  structs,
                  aliases,
                  skolems,
                )?)
                .into(),
              }))),
              _ => err(InvalidFunctionType, source_trace),
            }
          }
          Some(EaslTree::Leaf(_, struct_name)) => {
            if signature_leaves.len() == 0 {
              return err(InvalidStructName, source_trace);
            } else {
              let generic_args: Vec<ExpTypeInfo> = signature_leaves
                .map(|signature_arg| {
                  Ok(
                    TypeState::Known(
                      AbstractType::from_easl_tree(
                        signature_arg,
                        structs,
                        aliases,
                        skolems,
                      )?
                      .concretize(
                        skolems,
                        structs,
                        source_trace.clone(),
                      )?,
                    )
                    .into(),
                  )
                })
                .collect::<CompileResult<Vec<ExpTypeInfo>>>()?;
              if let Some(s) = structs.iter().find(|s| &*s.name == struct_name)
              {
                Ok(Type::Struct(AbstractStruct::fill_generics_ordered(
                  s.clone(),
                  generic_args,
                  structs,
                  source_trace.clone(),
                )?))
              } else {
                return err(NoStructNamed(struct_name.into()), source_trace);
              }
            }
          }
          _ => return err(InvalidStructName, source_trace),
        }
      }
      EaslTree::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Square)),
        array_children,
      ) => {
        let source_trace: SourceTrace = position.into();
        if array_children.len() == 1 {
          if let EaslTree::Inner(
            (position, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
            mut type_annotation_children,
          ) = array_children.into_iter().next().unwrap()
          {
            let source_trace: SourceTrace = position.into();
            if let EaslTree::Leaf(_, num_str) =
              type_annotation_children.remove(0)
            {
              let inner_type = Type::from_easl_tree(
                type_annotation_children.remove(0),
                structs,
                aliases,
                skolems,
              )?;
              Ok(Type::Array(
                Some(if let Ok(array_size) = num_str.parse::<u32>() {
                  ArraySize::Constant(array_size)
                } else {
                  ArraySize::Override(num_str.into())
                }),
                Box::new(TypeState::Known(inner_type).into()),
              ))
            } else {
              return err(InvalidArraySignature, source_trace);
            }
          } else {
            return err(InvalidArraySignature, source_trace);
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
      (Type::Array(count_a, a), Type::Array(count_b, b)) => {
        (count_a.is_none() || count_b.is_none() || count_a == count_b)
          && TypeState::are_compatible(a, b)
      }
      (Type::Reference(a), Type::Reference(b)) => {
        TypeState::are_compatible(a, b)
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
    structs: &Vec<Rc<AbstractStruct>>,
    type_aliases: &Vec<(Rc<str>, Rc<AbstractStruct>)>,
    skolems: &Vec<Rc<str>>,
  ) -> CompileResult<Self> {
    use Type::*;
    let source_trace: SourceTrace = source_position.into();
    Ok(match &*name {
      "None" => None,
      "F32" | "f32" => F32,
      "I32" | "i32" => I32,
      "U32" | "u32" => U32,
      "Bool" | "bool" => Bool,
      _ => {
        if skolems.contains(&name) {
          Skolem(name)
        } else if let Some(s) = structs.iter().find(|s| s.name == name) {
          Struct(AbstractStruct::fill_generics_with_unification_variables(
            s.clone(),
            structs,
            source_trace.clone(),
          )?)
        } else if let Some(s) = type_aliases
          .iter()
          .find_map(|(alias, s)| (*alias == name).then(|| s))
        {
          Struct(AbstractStruct::fill_generics_with_unification_variables(
            s.clone(),
            structs,
            source_trace.clone(),
          )?)
        } else {
          return err(UnrecognizedTypeName(name), source_trace);
        }
      }
    })
  }
  pub fn compile(&self) -> String {
    match self {
      Type::None => panic!("Attempted to compile None type"),
      Type::F32 => "f32".to_string(),
      Type::I32 => "i32".to_string(),
      Type::U32 => "u32".to_string(),
      Type::Bool => "bool".to_string(),
      Type::Struct(s) => match &*s.name {
        "Texture2D" => format!(
          "texture_2d<{}>",
          s.fields[0].field_type.unwrap_known().compile()
        ),
        _ => compile_word(s.monomorphized_name()),
      },
      Type::Array(size, inner_type) => {
        format!(
          "array<{}{}>",
          inner_type.compile(),
          size
            .clone()
            .map(|size| format!("{}", size.compile_type()))
            .unwrap_or(String::new())
        )
      }
      Type::Reference(inner_type) => {
        format!("ptr<storage, {}>", inner_type.compile())
      }
      Type::Function(_) => {
        panic!("Attempted to compile ConcreteFunction type")
      }
      Type::Skolem(name) => {
        panic!("Attempted to compile Skolem \"{name}\"")
      }
    }
  }
  pub fn replace_skolems(&mut self, skolems: &Vec<(Rc<str>, Type)>) {
    if let Type::Skolem(s) = &self {
      std::mem::swap(
        self,
        &mut skolems
          .iter()
          .find_map(|(skolem_name, t)| (skolem_name == s).then(|| t.clone()))
          .unwrap(),
      )
    } else {
      match self {
        Type::Struct(s) => {
          for field in s.fields.iter_mut() {
            field
              .field_type
              .as_known_mut(|t| t.replace_skolems(skolems));
          }
        }
        Type::Function(f) => {
          f.return_type.as_known_mut(|t| t.replace_skolems(skolems));
          for (arg_type, _) in f.arg_types.iter_mut() {
            arg_type.as_known_mut(|t| t.replace_skolems(skolems))
          }
        }
        _ => {}
      }
    }
  }
  pub fn do_patterns_exhaust<'a>(
    &self,
    patterns: impl Iterator<Item = &'a TypedExp> + Clone,
  ) -> bool {
    for pattern in patterns.clone() {
      if pattern.kind == ExpKind::Wildcard {
        return true;
      }
    }
    if *self == Type::Bool {
      let mut found_true = false;
      let mut found_false = false;
      for pattern in patterns {
        if let ExpKind::BooleanLiteral(b) = pattern.kind {
          if b {
            found_true = true;
          } else {
            found_false = true;
          }
        }
      }
      found_true && found_false
    } else {
      false
    }
  }
}

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Type::None => panic!("Attempted to compile None type"),
        Type::F32 => "f32".to_string(),
        Type::I32 => "i32".to_string(),
        Type::U32 => "u32".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Struct(s) => match &*s.name {
          "Texture2D" => format!(
            "(Texture2D {})",
            s.fields[0].field_type.unwrap_known().to_string()
          ),
          _ => {
            compile_word(s.monomorphized_name())
            // todo! this should display a name more like the above one for
            // Texture2D, using a kind of type-level function application syntax
          }
        },
        Type::Array(size, inner_type) => {
          if let Some(size) = size {
            format!("[{}: {}]", size, inner_type.to_string())
          } else {
            format!("[{}]", inner_type.to_string())
          }
        }
        Type::Reference(inner_type) => {
          format!("&{}", inner_type.to_string())
        }
        Type::Function(f) => {
          format!(
            "(Fn [{}]: {})",
            f.arg_types
              .iter()
              .map(|(t, _)| t.kind.to_string())
              .collect::<Vec<String>>()
              .join(" "),
            f.return_type.to_string()
          )
        }
        Type::Skolem(name) => name.to_string(),
      }
    )
  }
}

pub fn extract_type_annotation_ast(
  exp: EaslTree,
) -> (Option<EaslTree>, EaslTree) {
  if let EaslTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
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
  structs: &Vec<Rc<AbstractStruct>>,
  aliases: &Vec<(Rc<str>, Rc<AbstractStruct>)>,
  skolems: &Vec<Rc<str>>,
) -> CompileResult<(Option<AbstractType>, EaslTree)> {
  let (t, value) = extract_type_annotation_ast(exp);
  Ok((
    t.map(|t| AbstractType::from_easl_tree(t, structs, aliases, skolems))
      .map_or(Ok(None), |v| v.map(Some))?,
    value,
  ))
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpTypeInfo {
  pub kind: TypeState,
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
        match t {
          Type::Struct(s) => !s
            .fields
            .iter()
            .find(|field| !field.field_type.check_is_fully_known())
            .is_some(),
          Type::Function(function_signature) => {
            function_signature.arg_types.iter().fold(
              function_signature.return_type.check_is_fully_known(),
              |typed_so_far, (arg_type, _)| {
                typed_so_far && arg_type.check_is_fully_known()
              },
            )
          }
          Type::Array(size, inner_type) => {
            size.is_some() && inner_type.check_is_fully_known()
          }
          Type::Reference(inner_type) => inner_type.check_is_fully_known(),
          _ => true,
        }
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
    source_trace: SourceTrace,
  ) -> (bool, Vec<CompileError>) {
    if *self == other {
      return (false, vec![]);
    }
    self.with_dereferenced_mut(move |mut this| {
      other.with_dereferenced_mut(|mut other| {
        let result = match (&mut this, &mut other) {
          (TypeState::UnificationVariable(_), _) => unreachable!(),
          (_, TypeState::UnificationVariable(_)) => unreachable!(),
          (_, TypeState::Unknown) => (false, vec![]),
          (TypeState::Unknown, _) => {
            std::mem::swap(this, &mut other.clone());
            (true, vec![])
          }
          (TypeState::Known(current_type), TypeState::Known(other_type)) => {
            if !current_type.compatible(&other_type) {
              (
                false,
                vec![CompileError::new(
                  IncompatibleTypes(this.clone(), other.clone()),
                  source_trace,
                )],
              )
            } else {
              match (current_type, other_type) {
                (
                  Type::Function(signature),
                  Type::Function(other_signature),
                ) => {
                  let (mut anything_changed, mut errors) =
                    signature.return_type.constrain(
                      other_signature.return_type.kind.clone(),
                      source_trace.clone(),
                    );
                  for ((t, _), (other_t, _)) in signature
                    .arg_types
                    .iter_mut()
                    .zip(other_signature.arg_types.iter_mut())
                  {
                    let (changed, mut sub_errors) =
                      t.constrain(other_t.kind.clone(), source_trace.clone());
                    anything_changed |= changed;
                    errors.append(&mut sub_errors);
                  }
                  (anything_changed, errors)
                }
                (Type::Struct(s), Type::Struct(other_s)) => {
                  let mut anything_changed = false;
                  let mut errors = vec![];
                  for (t, other_t) in
                    s.fields.iter_mut().zip(other_s.fields.iter_mut())
                  {
                    let (changed, mut sub_errors) = t.field_type.constrain(
                      other_t.field_type.kind.clone(),
                      source_trace.clone(),
                    );
                    anything_changed |= changed;
                    errors.append(&mut sub_errors);
                  }
                  (anything_changed, errors)
                }
                (
                  Type::Array(size, inner_type),
                  Type::Array(other_size, other_inner_type),
                ) => {
                  let mut errors = vec![];
                  let (changed, mut sub_errors) = inner_type.constrain(
                    other_inner_type.kind.clone(),
                    source_trace.clone(),
                  );
                  if let Some(other_size) = other_size {
                    if let Some(size) = &size {
                      if size != other_size {
                        errors.push(CompileError::new(
                          IncompatibleTypes(this.clone(), other.clone()),
                          source_trace,
                        ));
                      }
                    } else {
                      std::mem::swap(size, &mut Some(other_size.clone()))
                    }
                  }
                  errors.append(&mut sub_errors);
                  (changed, errors)
                }
                (
                  Type::Reference(inner_type),
                  Type::Reference(other_inner_type),
                ) => inner_type
                  .constrain(other_inner_type.kind.clone(), source_trace),
                _ => (false, vec![]),
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
            (changed, vec![])
          }
          (TypeState::OneOf(possibilities), TypeState::Known(t)) => {
            if t.compatible_with_any(&possibilities) {
              std::mem::swap(this, &mut TypeState::Known(t.clone()));
              (true, vec![])
            } else {
              (
                false,
                vec![CompileError::new(
                  IncompatibleTypes(this.clone(), other.clone()),
                  source_trace,
                )],
              )
            }
          }
          (TypeState::Known(t), TypeState::OneOf(possibilities)) => {
            if !t.compatible_with_any(&possibilities) {
              (
                false,
                vec![CompileError::new(
                  IncompatibleTypes(this.clone(), other.clone()),
                  source_trace,
                )],
              )
            } else {
              (false, vec![])
            }
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
    source_trace: SourceTrace,
  ) -> (bool, Vec<CompileError>) {
    let (self_changed, mut errors) =
      self.constrain(other.clone(), source_trace.clone());
    let (other_changed, mut other_errors) =
      other.constrain(self.clone(), source_trace);
    errors.append(&mut other_errors);
    (self_changed || other_changed, errors)
  }
  pub fn constrain_fn_by_argument_types(
    &mut self,
    mut arg_types: Vec<TypeState>,
    source_trace: SourceTrace,
  ) -> (bool, Vec<CompileError>) {
    self.with_dereferenced_mut(|typestate| match typestate {
      TypeState::OneOf(possibilities) => {
        let mut errors = vec![];
        let mut anything_changed = false;
        let mut new_possibilities: Vec<Type> = vec![];
        for possibility in possibilities {
          match possibility {
            Type::Function(signature) => {
              if signature.are_args_compatible(&arg_types) {
                new_possibilities.push(Type::Function(signature.clone()))
              } else {
                anything_changed = true;
              }
            }
            _ => errors.push(CompileError::new(
              ExpectedFunctionFoundNonFunction,
              source_trace.clone(),
            )),
          }
        }
        if new_possibilities.is_empty() {
          errors.push(CompileError::new(
            FunctionArgumentTypesIncompatible(typestate.clone(), arg_types),
            source_trace,
          ));
          (false, errors)
        } else {
          std::mem::swap(
            typestate,
            &mut TypeState::OneOf(new_possibilities).simplified(),
          );
          (anything_changed, errors)
        }
      }
      TypeState::Known(t) => match t {
        Type::Function(signature) => {
          signature.mutually_constrain_arguments(&mut arg_types, source_trace)
        }
        _ => (
          false,
          vec![CompileError::new(
            ExpectedFunctionFoundNonFunction,
            source_trace,
          )],
        ),
      },
      TypeState::Unknown => (false, vec![]),
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
          1 => TypeState::Known(possibilities.remove(0)),
          _ => TypeState::OneOf(possibilities),
        },
      )
    }
  }
  pub fn simplified(mut self) -> Self {
    self.simplify();
    self
  }
  pub fn compile(&self) -> String {
    self.unwrap_known().compile()
  }
}

impl Display for TypeState {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      self.with_dereferenced(|t| match t {
        TypeState::Unknown => "?".to_string(),
        TypeState::OneOf(items) => items
          .into_iter()
          .map(|t| t.to_string())
          .collect::<Vec<String>>()
          .join(" or "),
        TypeState::Known(t) => t.to_string(),
        TypeState::UnificationVariable(_) => unreachable!(),
      })
    )
  }
}

#[derive(Debug, Clone, PartialEq)]
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
  pub typestate: ExpTypeInfo,
}

impl Variable {
  pub fn new(typestate: ExpTypeInfo) -> Self {
    Self {
      typestate,
      kind: VariableKind::Let,
    }
  }
  pub fn with_kind(mut self, kind: VariableKind) -> Self {
    self.kind = kind;
    self
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeConstraint {
  name: Rc<str>,
  args: Vec<AbstractType>,
}

impl Display for TypeConstraint {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name)
  }
}

impl TypeConstraint {
  pub fn scalar() -> Self {
    Self {
      name: "Scalar".into(),
      args: vec![],
    }
  }
  pub fn integer() -> Self {
    Self {
      name: "Integer".into(),
      args: vec![],
    }
  }
}

pub fn parse_type_bound(
  ast: EaslTree,
  structs: &Vec<Rc<AbstractStruct>>,
  aliases: &Vec<(Rc<str>, Rc<AbstractStruct>)>,
  generic_args: &Vec<Rc<str>>,
) -> CompileResult<TypeConstraint> {
  match ast {
    EaslTree::Leaf(_, name) => Ok(TypeConstraint {
      name: name.into(),
      args: vec![],
    }),
    EaslTree::Inner(
      (position, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
      children,
    ) => {
      let source_trace: SourceTrace = position.into();
      let mut children_iter = children.into_iter();
      let name = if let EaslTree::Leaf(_, name) =
        children_iter.next().ok_or_else(|| {
          CompileError::new(InvalidTypeBound, source_trace.clone())
        })? {
        name.into()
      } else {
        return err(InvalidTypeBound, source_trace);
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
      Ok(TypeConstraint { name, args })
    }
    _ => err(InvalidTypeBound, ast.position().clone().into()),
  }
}

pub fn parse_generic_argument(
  ast: EaslTree,
  structs: &Vec<Rc<AbstractStruct>>,
  aliases: &Vec<(Rc<str>, Rc<AbstractStruct>)>,
  generic_args: &Vec<Rc<str>>,
) -> CompileResult<(Rc<str>, Vec<TypeConstraint>)> {
  match ast {
    EaslTree::Leaf(_, generic_name) => Ok((generic_name.into(), vec![])),
    EaslTree::Inner(
      (position, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
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
            (_, EncloserOrOperator::Encloser(Encloser::Square)),
            bound_children,
          ) => Ok((
            generic_name.into(),
            bound_children
              .into_iter()
              .map(|child_ast| {
                parse_type_bound(child_ast, structs, aliases, generic_args)
              })
              .collect::<CompileResult<_>>()?,
          )),
          other => Ok((
            generic_name.into(),
            vec![parse_type_bound(other, structs, aliases, generic_args)?],
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

#[derive(Debug, Clone)]
pub struct Context {
  pub structs: Vec<Rc<AbstractStruct>>,
  pub variables: HashMap<Rc<str>, Vec<Variable>>,
  pub abstract_functions:
    HashMap<Rc<str>, Vec<Rc<RefCell<AbstractFunctionSignature>>>>,
  pub type_aliases: Vec<(Rc<str>, Rc<AbstractStruct>)>,
  pub enclosing_function_types: Vec<TypeState>,
  pub top_level_vars: Vec<TopLevelVar>,
}

impl Context {
  fn empty() -> Self {
    Self {
      structs: vec![],
      variables: HashMap::new(),
      abstract_functions: HashMap::new(),
      type_aliases: vec![],
      enclosing_function_types: vec![],
      top_level_vars: vec![],
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
  pub fn default_global() -> Self {
    DEFAULT_GLOBAL_CONTEXT.with_borrow(|ctx| ctx.clone())
  }
  pub fn add_abstract_function(
    &mut self,
    signature: Rc<RefCell<AbstractFunctionSignature>>,
  ) {
    let name = Rc::clone(&signature.borrow().name);
    if let Some(bucket) = self.abstract_functions.get_mut(&name) {
      bucket.push(signature.into());
    } else {
      self.abstract_functions.insert(name, vec![signature.into()]);
    }
  }
  pub fn with_functions(
    mut self,
    functions: Vec<AbstractFunctionSignature>,
  ) -> Self {
    for f in functions {
      self.add_abstract_function(Rc::new(RefCell::new(f)));
    }
    self
  }
  pub fn with_struct(mut self, s: Rc<AbstractStruct>) -> Self {
    if !self.structs.contains(&s) {
      if !ABNORMAL_CONSTRUCTOR_STRUCTS.contains(&&*s.name) {
        self.add_abstract_function(Rc::new(RefCell::new(
          AbstractFunctionSignature {
            name: s.name.clone(),
            generic_args: s
              .generic_args
              .iter()
              .map(|name| (name.clone(), vec![]))
              .collect(),
            arg_types: s
              .fields
              .iter()
              .map(|field| field.field_type.clone())
              .collect(),
            return_type: AbstractType::AbstractStruct(s.clone()),
            implementation: FunctionImplementationKind::Constructor,
            associative: false,
          },
        )));
      }
      self.structs.push(s);
      self.structs.dedup();
    }
    self
  }
  pub fn with_structs(self, structs: Vec<Rc<AbstractStruct>>) -> Self {
    structs.into_iter().fold(self, |ctx, s| ctx.with_struct(s))
  }
  pub fn with_type_aliases(
    mut self,
    mut aliases: Vec<(Rc<str>, Rc<AbstractStruct>)>,
  ) -> Self {
    self.type_aliases.append(&mut aliases);
    self
  }
  pub fn add_monomorphized_struct(&mut self, s: Rc<AbstractStruct>) {
    if self
      .structs
      .iter()
      .find(|existing_struct| {
        existing_struct.name == s.name
          && existing_struct.filled_generics == s.filled_generics
      })
      .is_none()
    {
      self.structs.push(s);
    }
  }
  pub fn concrete_signatures(
    &mut self,
    fn_name: &Rc<str>,
    source_trace: SourceTrace,
  ) -> CompileResult<Option<Vec<Type>>> {
    if let Some(signatures) = self.abstract_functions.get(fn_name) {
      signatures
        .into_iter()
        .map(|signature| {
          Ok(Type::Function(Box::new(
            AbstractFunctionSignature::concretize(
              Rc::new(RefCell::new(signature.borrow().clone())),
              &self.structs,
              source_trace.clone(),
            )?,
          )))
        })
        .collect::<CompileResult<Vec<_>>>()
        .map(|x| Some(x))
    } else {
      Ok(None)
    }
  }
  pub fn constrain_name_type(
    &mut self,
    name: &Rc<str>,
    source_trace: SourceTrace,
    t: &mut TypeState,
  ) -> (bool, Vec<CompileError>) {
    match self.concrete_signatures(name, source_trace.clone()) {
      Err(e) => (false, vec![e]),
      Ok(Some(signatures)) => {
        t.constrain(TypeState::OneOf(signatures), source_trace)
      }
      Ok(None) => match self.get_typestate_mut(name, source_trace.clone()) {
        Ok(typestate) => t.mutually_constrain(typestate, source_trace),
        Err(e) => (false, vec![e]),
      },
    }
  }
  pub fn bind(&mut self, name: &str, v: Variable) {
    if !self.variables.contains_key(name) {
      self.variables.insert(name.into(), vec![]);
    }
    self.variables.get_mut(name).unwrap().push(v);
  }
  pub fn unbind(&mut self, name: &str) -> Variable {
    let name_bindings = self.variables.get_mut(name).unwrap();
    let v = name_bindings.pop().unwrap();
    if name_bindings.is_empty() {
      self.variables.remove(name);
    }
    v
  }
  pub fn is_bound(&self, name: &str) -> bool {
    let name_rc: Rc<str> = name.to_string().into();
    self.variables.contains_key(name)
      || self.abstract_functions.contains_key(&name_rc)
      || self
        .top_level_vars
        .iter()
        .find(|top_level_var| &*top_level_var.name == name)
        .is_some()
  }
  pub fn get_variable_kind(&self, name: &str) -> VariableKind {
    self
      .variables
      .get(name)
      .map(|vars| vars.last().unwrap().kind.clone())
      .or(
        self
          .top_level_vars
          .iter()
          .find_map(|v| (&*v.name == name).then(|| v.var.kind.clone())),
      )
      .unwrap()
  }
  pub fn get_typestate_mut(
    &mut self,
    name: &str,
    source_trace: SourceTrace,
  ) -> CompileResult<&mut TypeState> {
    if let Some(var) = self.variables.get_mut(name) {
      Ok(&mut var.last_mut().unwrap().typestate)
    } else {
      if let Some(top_level_var) = self
        .top_level_vars
        .iter_mut()
        .find(|var| &*var.name == name)
      {
        Ok(&mut top_level_var.var.typestate)
      } else {
        Err(CompileError::new(UnboundName(name.into()), source_trace))
      }
    }
  }
  pub fn abstract_functions_iter(
    &self,
  ) -> impl Iterator<Item = &Rc<RefCell<AbstractFunctionSignature>>> {
    self
      .abstract_functions
      .values()
      .map(|fs| fs.iter())
      .flatten()
  }
  pub fn abstract_functions_iter_mut(
    &mut self,
  ) -> impl Iterator<Item = &mut Rc<RefCell<AbstractFunctionSignature>>> {
    self
      .abstract_functions
      .values_mut()
      .map(|fs| fs.iter_mut())
      .flatten()
  }
}

thread_local! {
  static DEFAULT_GLOBAL_CONTEXT: RefCell<Context> =
    RefCell::new(
      Context::empty()
        .with_functions(built_in_functions())
        .with_structs(
          built_in_structs().into_iter().map(|s| Rc::new(s)).collect(),
        )
        .with_type_aliases(built_in_type_aliases()));
}
