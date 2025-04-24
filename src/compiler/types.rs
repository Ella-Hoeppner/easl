use core::fmt::Debug;
use std::{
  cell::RefCell,
  collections::HashMap,
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
  error::{err, CompileErrorKind::*, CompileResult, ErrorLog, SourceTrace},
  functions::FunctionSignature,
  program::Program,
  structs::{AbstractStruct, Struct},
  util::compile_word,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
  Unit,
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
                mutated_args: vec![],
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
      "None" => Unit,
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
      Type::Unit => panic!("Attempted to compile Unit type"),
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
                source_trace,
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
                    source_trace.clone(),
                    errors,
                  );
                  for ((t, _), (other_t, _)) in signature
                    .arg_types
                    .iter_mut()
                    .zip(other_signature.arg_types.iter_mut())
                  {
                    let changed = t.constrain(
                      other_t.kind.clone(),
                      source_trace.clone(),
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
                      source_trace.clone(),
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
                    source_trace.clone(),
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
                          source_trace,
                        ));
                      }
                    } else {
                      std::mem::swap(size, &mut Some(other_size.clone()))
                    }
                  }
                  changed
                }
                (
                  Type::Reference(inner_type),
                  Type::Reference(other_inner_type),
                ) => inner_type.constrain(
                  other_inner_type.kind.clone(),
                  source_trace,
                  errors,
                ),
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
              std::mem::swap(this, &mut TypeState::Known(t.clone()));
              true
            } else {
              errors.log(CompileError::new(
                IncompatibleTypes(this.clone().into(), other.clone().into()),
                source_trace,
              ));
              false
            }
          }
          (TypeState::Known(t), TypeState::OneOf(possibilities)) => {
            if !t.compatible_with_any(&possibilities) {
              errors.log(CompileError::new(
                IncompatibleTypes(this.clone().into(), other.clone().into()),
                source_trace,
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
    source_trace: SourceTrace,
    errors: &mut ErrorLog,
  ) -> bool {
    let self_changed =
      self.constrain(other.clone(), source_trace.clone(), errors);
    let other_changed = other.constrain(self.clone(), source_trace, errors);
    self_changed || other_changed
  }
  pub fn constrain_fn_by_argument_types(
    &mut self,
    mut arg_types: Vec<TypeState>,
    source_trace: SourceTrace,
    errors: &mut ErrorLog,
  ) -> bool {
    self.with_dereferenced_mut(|typestate| match typestate {
      TypeState::OneOf(possibilities) => {
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
            _ => errors.log(CompileError::new(
              ExpectedFunctionFoundNonFunction,
              source_trace.clone(),
            )),
          }
        }
        if new_possibilities.is_empty() {
          errors.log(CompileError::new(
            FunctionArgumentTypesIncompatible(
              typestate.clone().into(),
              arg_types.into_iter().map(|t| t.into()).collect(),
            ),
            source_trace,
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
        Type::Function(signature) => signature.mutually_constrain_arguments(
          &mut arg_types,
          source_trace,
          errors,
        ),
        _ => {
          errors.log(CompileError::new(
            ExpectedFunctionFoundNonFunction,
            source_trace,
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
  args: Vec<Vec<TypeConstraint>>,
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

pub fn parse_type_constraint(
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
      todo!("can't parse type constraints yet")
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
                parse_type_constraint(child_ast, structs, aliases, generic_args)
              })
              .collect::<CompileResult<_>>()?,
          )),
          other => Ok((
            generic_name.into(),
            vec![parse_type_constraint(
              other,
              structs,
              aliases,
              generic_args,
            )?],
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
  pub variables: HashMap<Rc<str>, Vec<Variable>>,
  pub enclosing_function_types: Vec<TypeState>,
  pub program: P,
}

impl<P: Deref<Target = Program>> LocalContext<P> {
  pub fn empty(program: P) -> Self {
    Self {
      variables: HashMap::new(),
      enclosing_function_types: vec![],
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
      || self.program.abstract_functions.contains_key(&name_rc)
      || self
        .program
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
          .program
          .top_level_vars
          .iter()
          .find_map(|v| (&*v.name == name).then(|| v.var.kind.clone())),
      )
      .unwrap()
  }
}

pub type ImmutableProgramLocalContext<'p> = LocalContext<&'p Program>;

pub type MutableProgramLocalContext<'p> = LocalContext<&'p mut Program>;
impl<'p> MutableProgramLocalContext<'p> {
  fn get_typestate_mut(
    &mut self,
    name: &str,
    source_trace: SourceTrace,
  ) -> CompileResult<&mut TypeState> {
    if let Some(var) = self.variables.get_mut(name) {
      Ok(&mut var.last_mut().unwrap().typestate)
    } else {
      if let Some(top_level_var) = self
        .program
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
  pub fn constrain_name_type(
    &mut self,
    name: &Rc<str>,
    source_trace: SourceTrace,
    t: &mut TypeState,
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
        Ok(typestate) => t.mutually_constrain(typestate, source_trace, errors),
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
  Function {
    arg_types: Vec<(TypeStateDescription, Vec<TypeConstraintDescription>)>,
    return_type: Box<TypeStateDescription>,
  },
  Skolem(Rc<str>),
  Array(Option<ArraySize>, Box<TypeStateDescription>),
  Reference(Box<TypeStateDescription>),
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
          compile_word(s.monomorphized_name())
          // todo! this should display a name more like the above one for
          // Texture2D, using a kind of type-level function application syntax
        }
      }),
      Type::Function(f) => Self::Function {
        arg_types: f
          .arg_types
          .into_iter()
          .map(|(t, constraints)| {
            (
              TypeStateDescription::from(t.kind),
              constraints
                .into_iter()
                .map(TypeConstraintDescription::from)
                .collect(),
            )
          })
          .collect(),
        return_type: TypeStateDescription::from(f.return_type.kind).into(),
      },
      Type::Skolem(name) => Self::Skolem(name),
      Type::Array(array_size, t) => {
        Self::Array(array_size, TypeStateDescription::from(t.kind).into())
      }
      Type::Reference(t) => {
        Self::Reference(TypeStateDescription::from(t.kind).into())
      }
    }
  }
}
impl Display for TypeDescription {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Unit => panic!("Attempted to compile Unit type"),
        Self::F32 => "f32".to_string(),
        Self::I32 => "i32".to_string(),
        Self::U32 => "u32".to_string(),
        Self::Bool => "bool".to_string(),
        Self::Struct(name) => name.clone(),
        Self::Array(size, inner_type) => {
          if let Some(size) = size {
            format!("[{}: {}]", size, inner_type.to_string())
          } else {
            format!("[{}]", inner_type.to_string())
          }
        }
        Self::Reference(inner_type) => {
          format!("&{}", inner_type.to_string())
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
  UnificationVariable(Box<Self>),
}
impl From<TypeState> for TypeStateDescription {
  fn from(typestate: TypeState) -> Self {
    match typestate {
      TypeState::Unknown => Self::Unknown,
      TypeState::OneOf(items) => {
        Self::OneOf(items.into_iter().map(TypeDescription::from).collect())
      }
      TypeState::Known(t) => Self::Known(TypeDescription::from(t)),
      TypeState::UnificationVariable(inner_type) => Self::UnificationVariable(
        Self::from(inner_type.borrow().clone()).into(),
      ),
    }
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
        Self::UnificationVariable(inner) =>
          format!("UnificationVariable({inner})"),
      }
    )
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeConstraintDescription {
  name: Rc<str>,
  args: Vec<Rc<str>>,
}
impl From<TypeConstraint> for TypeConstraintDescription {
  fn from(constraint: TypeConstraint) -> Self {
    Self {
      name: constraint.name,
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
