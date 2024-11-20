use core::fmt::Debug;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use sse::syntax::EncloserOrOperator;

use crate::{
  compiler::error::{CompileError, CompileErrorKind},
  parse::{Encloser, Operator, TyntTree},
};

use super::{
  builtins::{
    built_in_functions, built_in_structs, built_in_type_aliases,
    ABNORMAL_CONSTRUCTOR_STRUCTS,
  },
  error::{err, CompileErrorKind::*, CompileResult, SourceTrace},
  functions::{
    AbstractFunctionSignature, FunctionImplementationKind, FunctionSignature,
  },
  structs::{AbstractStruct, Struct, TypeOrAbstractStruct},
  util::compile_word,
  vars::TopLevelVar,
};

#[derive(Debug, Clone, PartialEq)]
pub enum GenericOr<T> {
  Generic(String),
  NonGeneric(T),
}

impl GenericOr<Type> {
  pub fn to_typestate(
    self,
    generic_variables: &HashMap<String, TypeState>,
  ) -> TypeState {
    match self {
      GenericOr::Generic(name) => generic_variables
        .get(&name)
        .expect("unrecognized generic variable")
        .clone(),
      GenericOr::NonGeneric(t) => TypeState::Known(t),
    }
  }
}

pub type AbstractType = GenericOr<TypeOrAbstractStruct>;

impl AbstractType {
  pub fn from_tynt_tree(
    tree: TyntTree,
    structs: &Vec<AbstractStruct>,
    aliases: &Vec<(String, AbstractStruct)>,
    generic_args: &Vec<String>,
    skolems: &Vec<String>,
  ) -> CompileResult<Self> {
    match &tree {
      TyntTree::Leaf(_, type_name) => Ok(Self::from_name(
        type_name.clone(),
        tree.position().path.clone(),
        structs,
        aliases,
        generic_args,
        skolems,
      )?),
      TyntTree::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Parens)),
        children,
      ) => {
        let mut children_iter = children.into_iter();
        if let Some(TyntTree::Leaf(position, type_name)) = children_iter.next()
        {
          if let Some(s) = structs.iter().find(|s| s.name == *type_name) {
            let struct_generic_args = children_iter
              .map(|t| {
                Ok(Self::from_tynt_tree(
                  t.clone(),
                  structs,
                  aliases,
                  generic_args,
                  skolems,
                )?)
              })
              .collect::<CompileResult<Vec<Self>>>()?;
            Ok(AbstractType::NonGeneric(
              TypeOrAbstractStruct::AbstractStruct(
                s.clone().fill_abstract_generics(struct_generic_args),
              ),
            ))
          } else {
            err(
              InvalidTypeName(type_name.clone()),
              position.path.clone().into(),
            )
          }
        } else {
          err(InvalidType(tree.clone()), position.path.clone().into())
        }
      }
      _ => err(
        InvalidType(tree.clone()),
        tree.position().path.clone().into(),
      ),
    }
  }
  pub fn from_name(
    name: String,
    position: Vec<usize>,
    structs: &Vec<AbstractStruct>,
    aliases: &Vec<(String, AbstractStruct)>,
    generic_args: &Vec<String>,
    skolems: &Vec<String>,
  ) -> CompileResult<Self> {
    Ok(if generic_args.contains(&name) {
      GenericOr::Generic(name)
    } else {
      GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::from_name(
        name, position, structs, aliases, skolems,
      )?))
    })
  }
  pub fn from_ast(
    ast: TyntTree,
    structs: &Vec<AbstractStruct>,
    aliases: &Vec<(String, AbstractStruct)>,
    generic_args: &Vec<String>,
    skolems: &Vec<String>,
  ) -> CompileResult<Self> {
    match ast {
      TyntTree::Leaf(position, leaf) => Ok(if generic_args.contains(&leaf) {
        GenericOr::Generic(leaf)
      } else {
        GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::from_name(
          leaf,
          position.path.clone(),
          structs,
          aliases,
          skolems,
        )?))
      }),
      TyntTree::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Parens)),
        children,
      ) => {
        let mut children_iter = children.into_iter();
        let generic_struct_name =
          if let Some(TyntTree::Leaf(_, leaf)) = children_iter.next() {
            leaf
          } else {
            return err(InvalidStructName, position.path.into());
          };
        let generic_struct = structs
          .iter()
          .find(|s| s.name == generic_struct_name)
          .ok_or_else(|| {
            CompileError::new(InvalidFunctionArgumentName, position.path.into())
          })?
          .clone();
        let generic_args = children_iter
          .map(|subtree: TyntTree| {
            Self::from_ast(subtree, structs, aliases, generic_args, skolems)
          })
          .collect::<CompileResult<Vec<_>>>()?;
        Ok(GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
          generic_struct.fill_abstract_generics(generic_args),
        )))
      }
      _ => err(InvalidStructFieldType, ast.position().path.clone().into()),
    }
  }
  pub fn concretize(
    &self,
    structs: &Vec<AbstractStruct>,
    skolems: &Vec<String>,
    source_trace: SourceTrace,
  ) -> CompileResult<Type> {
    match self {
      GenericOr::Generic(name) => {
        if skolems.contains(name) {
          Ok(Type::Skolem(name.clone()))
        } else {
          err(UnrecognizedGeneric(name.clone()), source_trace)
        }
      }
      GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(s)) => Ok(
        Type::Struct(s.concretize(structs, skolems, source_trace)?),
      ),
      GenericOr::NonGeneric(TypeOrAbstractStruct::Type(t)) => Ok(t.clone()),
    }
  }
  pub fn extract_generic_bindings(
    &self,
    concrete_type: &Type,
    generic_bindings: &mut HashMap<String, Type>,
  ) {
    match self {
      GenericOr::Generic(generic) => {
        generic_bindings.insert(generic.clone(), concrete_type.clone());
      }
      GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
        abstract_struct,
      )) => {
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
      GenericOr::Generic(name) => GenericOr::Generic(if name == old_name {
        new_name.to_string()
      } else {
        name
      }),
      GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(mut s)) => {
        s.generic_args = s
          .generic_args
          .into_iter()
          .map(|name| {
            if name == old_name {
              new_name.to_string()
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
        GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(s))
      }
      other => other,
    }
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
  Skolem(String),
}
impl Type {
  pub fn satisfies_constraints(&self, constraint: &TypeConstraint) -> bool {
    match constraint.name.as_str() {
      "Scalar" => {
        *self == Type::I32 || *self == Type::F32 || *self == Type::U32
      }
      _ => todo!(),
    }
  }
  pub fn from_tynt_tree(
    tree: TyntTree,
    structs: &Vec<AbstractStruct>,
    aliases: &Vec<(String, AbstractStruct)>,
    skolems: &Vec<String>,
  ) -> CompileResult<Self> {
    match tree {
      TyntTree::Leaf(position, type_name) => {
        Type::from_name(type_name, position.path, structs, aliases, skolems)
      }
      TyntTree::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Parens)),
        struct_signature_children,
      ) => {
        let source_trace: SourceTrace = position.path.into();
        let mut signature_leaves = struct_signature_children.into_iter();
        if let Some(TyntTree::Leaf(_, struct_name)) = signature_leaves.next() {
          if signature_leaves.is_empty() {
            return err(InvalidStructName, source_trace);
          } else {
            let generic_args: Vec<TypeState> = signature_leaves
              .map(|signature_arg| {
                Ok(TypeState::Known(
                  AbstractType::from_tynt_tree(
                    signature_arg,
                    structs,
                    aliases,
                    &vec![],
                    skolems,
                  )?
                  .concretize(
                    structs,
                    skolems,
                    source_trace.clone(),
                  )?,
                ))
              })
              .collect::<CompileResult<Vec<TypeState>>>()?;
            if let Some(s) = structs.iter().find(|s| s.name == struct_name) {
              Ok(Type::Struct(s.clone().fill_generics_ordered(generic_args)))
            } else {
              return err(UnknownStructName, source_trace);
            }
          }
        } else {
          return err(InvalidStructName, source_trace);
        }
      }
      other => {
        let source_trace = other.position().path.clone().into();
        return err(InvalidType(other), source_trace);
      }
    }
  }
  pub fn compatible(&self, other: &Self) -> bool {
    let b = match (self, other) {
      (Type::Function(a), Type::Function(b)) => a.compatible(b),
      (Type::Struct(a), Type::Struct(b)) => a.compatible(b),
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
    name: String,
    source_path: Vec<usize>,
    structs: &Vec<AbstractStruct>,
    type_aliases: &Vec<(String, AbstractStruct)>,
    skolems: &Vec<String>,
  ) -> CompileResult<Self> {
    use Type::*;
    Ok(match name.as_str() {
      "F32" | "f32" => F32,
      "I32" | "i32" => I32,
      "U32" | "u32" => U32,
      "Bool" | "bool" => Bool,
      _ => {
        if skolems.contains(&name) {
          Skolem(name)
        } else if let Some(s) = structs.iter().find(|s| s.name == name) {
          Struct(s.clone().fill_generics_with_unification_variables())
        } else if let Some(s) = type_aliases
          .iter()
          .find_map(|(alias, s)| (*alias == name).then(|| s))
        {
          Struct(s.clone().fill_generics_with_unification_variables())
        } else {
          return err(UnrecognizedTypeName(name), source_path.into());
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
      Type::Struct(s) => compile_word(s.monomorphized_name()),
      Type::Function(_) => {
        panic!("Attempted to compile ConcreteFunction type")
      }
      Type::Skolem(name) => {
        panic!("Attempted to compile Skolem \"{name}\"")
      }
    }
  }
  pub fn replace_skolems(&mut self, skolems: &Vec<(String, Type)>) {
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
  exp: TyntTree,
) -> CompileResult<(Option<TyntTree>, TyntTree)> {
  Ok(
    if let TyntTree::Inner(
      (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
      mut children,
    ) = exp
    {
      (Some(children.remove(1)), children.remove(0))
    } else {
      (None, exp)
    },
  )
}

pub fn extract_type_annotation(
  exp: TyntTree,
  structs: &Vec<AbstractStruct>,
  aliases: &Vec<(String, AbstractStruct)>,
  generic_args: &Vec<String>,
  skolems: &Vec<String>,
) -> CompileResult<(Option<AbstractType>, TyntTree)> {
  let (t, value) = extract_type_annotation_ast(exp)?;
  Ok((
    t.map(|t| {
      AbstractType::from_tynt_tree(t, structs, aliases, generic_args, skolems)
    })
    .map_or(Ok(None), |v| v.map(Some))?,
    value,
  ))
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
  pub fn is_fully_known(&self) -> bool {
    self.with_dereferenced(|typestate| {
      if let TypeState::Known(t) = typestate {
        match t {
          Type::Struct(s) => !s
            .fields
            .iter()
            .find(|field| !field.field_type.is_fully_known())
            .is_some(),
          Type::Function(function_signature) => {
            function_signature.arg_types.iter().fold(
              function_signature.return_type.is_fully_known(),
              |typed_so_far, (arg_type, _)| {
                typed_so_far && arg_type.is_fully_known()
              },
            )
          }
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
  pub fn as_known_mut(&mut self, f: impl FnOnce(&mut Type)) {
    self.with_dereferenced_mut(|typestate| {
      if let TypeState::Known(t) = typestate {
        f(t);
      } else {
        panic!("as_known_mut on a non-Known TypeState")
      }
    })
  }
  pub fn any_of(possibilities: Vec<TypeState>) -> CompileResult<Self> {
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
  ) -> CompileResult<bool> {
    if *self == other {
      return Ok(false);
    }
    self.with_dereferenced_mut(move |mut this| {
      other.with_dereferenced_mut(|mut other| {
        let changed = match (&mut this, &mut other) {
          (TypeState::UnificationVariable(_), _) => unreachable!(),
          (_, TypeState::UnificationVariable(_)) => unreachable!(),
          (_, TypeState::Unknown) => false,
          (TypeState::Unknown, _) => {
            std::mem::swap(this, &mut other.clone());
            true
          }
          (TypeState::Known(current_type), TypeState::Known(other_type)) => {
            if !current_type.compatible(&other_type) {
              return err(
                IncompatibleTypes(this.clone(), other.clone()),
                source_trace,
              );
            }
            match (current_type, other_type) {
              (Type::Function(signature), Type::Function(other_signature)) => {
                let mut changed = signature.return_type.constrain(
                  other_signature.return_type.clone(),
                  source_trace.clone(),
                )?;
                for ((t, _), (other_t, _)) in signature
                  .arg_types
                  .iter_mut()
                  .zip(other_signature.arg_types.iter_mut())
                {
                  changed |=
                    t.constrain(other_t.clone(), source_trace.clone())?;
                }
                changed
              }
              (Type::Struct(s), Type::Struct(other_s)) => {
                let mut changed = false;
                for (t, other_t) in
                  s.fields.iter_mut().zip(other_s.fields.iter_mut())
                {
                  changed |= t.field_type.constrain(
                    other_t.field_type.clone(),
                    source_trace.clone(),
                  )?;
                }
                changed
              }
              _ => false,
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
              &mut TypeState::OneOf(new_possibilities).simplified()?,
            );
            changed
          }
          (TypeState::OneOf(possibilities), TypeState::Known(t)) => {
            if t.compatible_with_any(&possibilities) {
              std::mem::swap(this, &mut TypeState::Known(t.clone()));
              true
            } else {
              return err(
                IncompatibleTypes(this.clone(), other.clone()),
                source_trace,
              );
            }
          }
          (TypeState::Known(t), TypeState::OneOf(possibilities)) => {
            if !t.compatible_with_any(&possibilities) {
              return err(
                IncompatibleTypes(this.clone(), other.clone()),
                source_trace,
              );
            }
            false
          }
        };
        this.simplify()?;
        Ok(changed)
      })
    })
  }
  pub fn mutually_constrain(
    &mut self,
    other: &mut TypeState,
    source_trace: SourceTrace,
  ) -> CompileResult<bool> {
    let self_changed = self.constrain(other.clone(), source_trace.clone())?;
    let other_changed = other.constrain(self.clone(), source_trace)?;
    Ok(self_changed || other_changed)
  }
  pub fn constrain_fn_by_argument_types(
    &mut self,
    mut arg_types: Vec<TypeState>,
    source_trace: SourceTrace,
  ) -> CompileResult<bool> {
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
            _ => panic!("tried to constrain fn on non-fn"),
          }
        }
        if new_possibilities.is_empty() {
          err(
            FunctionArgumentTypesIncompatible(typestate.clone(), arg_types),
            source_trace,
          )
        } else {
          std::mem::swap(
            typestate,
            &mut TypeState::OneOf(new_possibilities).simplified()?,
          );
          Ok(anything_changed)
        }
      }
      TypeState::Known(t) => match t {
        Type::Function(signature) => Ok(
          signature
            .mutually_constrain_arguments(&mut arg_types, source_trace)?,
        ),
        other => panic!(
          "tried to constrain fn on non-fn \n\n{arg_types:#?} \n\n{other:#?}"
        ),
      },
      TypeState::Unknown => Ok(false),
      TypeState::UnificationVariable(_) => unreachable!(),
    })
  }
  pub fn simplify(&mut self) -> CompileResult<()> {
    Ok(if let TypeState::OneOf(mut possibilities) = self.clone() {
      possibilities.dedup();
      std::mem::swap(
        self,
        &mut match possibilities.len() {
          0 => unreachable!(),
          1 => TypeState::Known(possibilities.remove(0)),
          _ => TypeState::OneOf(possibilities),
        },
      )
    })
  }
  pub fn simplified(mut self) -> CompileResult<Self> {
    self.simplify()?;
    Ok(self)
  }
  pub fn compile(&self) -> String {
    self.unwrap_known().compile()
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableKind {
  Let,
  Var,
}

impl VariableKind {
  pub fn compile(self) -> &'static str {
    match self {
      VariableKind::Let => "let",
      VariableKind::Var => "var",
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
  pub kind: VariableKind,
  pub typestate: TypeState,
}

impl Variable {
  pub fn new(typestate: TypeState) -> Self {
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
  name: String,
  args: Vec<AbstractType>,
}

impl TypeConstraint {
  pub fn scalar() -> Self {
    Self {
      name: "Scalar".to_string(),
      args: vec![],
    }
  }
}

pub fn parse_type_bound(
  ast: TyntTree,
  structs: &Vec<AbstractStruct>,
  aliases: &Vec<(String, AbstractStruct)>,
  generic_args: &Vec<String>,
) -> CompileResult<TypeConstraint> {
  match ast {
    TyntTree::Leaf(_, name) => Ok(TypeConstraint { name, args: vec![] }),
    TyntTree::Inner(
      (position, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
      children,
    ) => {
      let source_trace: SourceTrace = position.path.into();
      let mut children_iter = children.into_iter();
      let name = if let TyntTree::Leaf(_, name) =
        children_iter.next().ok_or_else(|| {
          CompileError::new(InvalidTypeBound, source_trace.clone())
        })? {
        name
      } else {
        return err(InvalidTypeBound, source_trace);
      };
      let args = children_iter
        .map(|child_ast| {
          AbstractType::from_ast(
            child_ast,
            structs,
            aliases,
            generic_args,
            &vec![],
          )
        })
        .collect::<CompileResult<Vec<AbstractType>>>()?;
      Ok(TypeConstraint { name, args })
    }
    _ => err(InvalidTypeBound, ast.position().path.clone().into()),
  }
}

pub fn parse_generic_argument(
  ast: TyntTree,
  structs: &Vec<AbstractStruct>,
  aliases: &Vec<(String, AbstractStruct)>,
  generic_args: &Vec<String>,
) -> CompileResult<(String, Vec<TypeConstraint>)> {
  match ast {
    TyntTree::Leaf(_, generic_name) => Ok((generic_name, vec![])),
    TyntTree::Inner(
      (position, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
      mut children,
    ) => {
      if children.len() < 2 {
        return err(
          InvalidDefn("Invalid generic name".to_string()),
          position.path.into(),
        );
      }
      let bounds_tree = children.remove(1);
      if let TyntTree::Leaf(_, generic_name) = children.remove(0) {
        match bounds_tree {
          TyntTree::Inner(
            (_, EncloserOrOperator::Encloser(Encloser::Square)),
            bound_children,
          ) => Ok((
            generic_name,
            bound_children
              .into_iter()
              .map(|child_ast| {
                parse_type_bound(child_ast, structs, aliases, generic_args)
              })
              .collect::<CompileResult<_>>()?,
          )),
          other => Ok((
            generic_name,
            vec![parse_type_bound(other, structs, aliases, generic_args)?],
          )),
        }
      } else {
        err(
          InvalidDefn("Invalid generic name".to_string()),
          position.path.into(),
        )
      }
    }
    _ => err(
      InvalidDefn("Invalid generic name".to_string()),
      ast.position().path.clone().into(),
    ),
  }
}

#[derive(Debug, Clone)]
pub struct Context {
  pub structs: Vec<AbstractStruct>,
  pub variables: HashMap<String, Vec<Variable>>,
  pub abstract_functions: Vec<AbstractFunctionSignature>,
  pub type_aliases: Vec<(String, AbstractStruct)>,
  pub enclosing_function_types: Vec<TypeState>,
  pub top_level_vars: Vec<TopLevelVar>,
}

impl Context {
  fn empty() -> Self {
    Self {
      structs: vec![],
      variables: HashMap::new(),
      abstract_functions: vec![],
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
    Self::empty()
      .with_functions(built_in_functions())
      .with_structs(built_in_structs())
      .with_type_aliases(built_in_type_aliases())
  }
  pub fn with_functions(
    mut self,
    mut functions: Vec<AbstractFunctionSignature>,
  ) -> Self {
    self.abstract_functions.append(&mut functions);
    self.structs.dedup();
    self
  }
  pub fn with_structs(mut self, mut structs: Vec<AbstractStruct>) -> Self {
    for s in &structs {
      if !ABNORMAL_CONSTRUCTOR_STRUCTS.contains(&s.name.as_str()) {
        self.add_abstract_function(AbstractFunctionSignature {
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
          return_type: GenericOr::NonGeneric(
            TypeOrAbstractStruct::AbstractStruct(s.clone()),
          ),
          implementation: FunctionImplementationKind::Constructor,
        });
      }
    }
    self.structs.append(&mut structs);
    self.structs.dedup();
    self
  }
  pub fn with_type_aliases(
    mut self,
    mut aliases: Vec<(String, AbstractStruct)>,
  ) -> Self {
    self.type_aliases.append(&mut aliases);
    self
  }
  pub fn add_monomorphized_struct(&mut self, s: AbstractStruct) {
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
  pub fn get_abstract_function_signatures(
    &self,
    name: &str,
  ) -> Vec<AbstractFunctionSignature> {
    self
      .abstract_functions
      .iter()
      .filter(|f| f.name == name)
      .cloned()
      .collect()
  }
  pub fn constrain_name_type(
    &mut self,
    name: &str,
    source_trace: SourceTrace,
    t: &mut TypeState,
  ) -> CompileResult<bool> {
    let abstract_signatures = self.get_abstract_function_signatures(name);
    if abstract_signatures.is_empty() {
      t.mutually_constrain(
        self.get_typestate_mut(name, source_trace.clone())?,
        source_trace,
      )
    } else {
      t.constrain(
        TypeState::OneOf(
          abstract_signatures
            .iter()
            .cloned()
            .map(|signature| Type::Function(Box::new(signature.concretize())))
            .collect(),
        ),
        source_trace,
      )
    }
  }
  pub fn bind(&mut self, name: &str, v: Variable) {
    if !self.variables.contains_key(name) {
      self.variables.insert(name.to_string(), vec![]);
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
  pub fn add_abstract_function(
    &mut self,
    signatures: AbstractFunctionSignature,
  ) {
    self.abstract_functions.push(signatures);
  }
  pub fn is_bound(&self, name: &str) -> bool {
    self.variables.contains_key(name)
      || self
        .abstract_functions
        .iter()
        .find(|f| f.name == name)
        .is_some()
      || self
        .top_level_vars
        .iter()
        .find(|top_level_var| top_level_var.name == name)
        .is_some()
  }
  pub fn get_variable_kind(&self, name: &str) -> &VariableKind {
    &self.variables.get(name).unwrap().last().unwrap().kind
  }
  pub fn get_typestate_mut(
    &mut self,
    name: &str,
    source_trace: SourceTrace,
  ) -> CompileResult<&mut TypeState> {
    /*
    .or_else(|| {
      self
        .top_level_vars
        .iter_mut()
        .find_map(|var| (var.name == name).then(|| &mut var.var_type))
    }) */
    if let Some(var) = self.variables.get_mut(name) {
      Ok(&mut var.last_mut().unwrap().typestate)
    } else {
      if let Some(top_level_var) =
        self.top_level_vars.iter_mut().find(|var| var.name == name)
      {
        Ok(&mut top_level_var.var.typestate)
      } else {
        Err(CompileError::new(
          UnboundName(name.to_string()),
          source_trace,
        ))
      }
    }
  }
  pub fn merge(mut self, mut other: Context) -> Self {
    self.structs.append(&mut other.structs);
    self.structs.dedup();
    self
      .abstract_functions
      .append(&mut other.abstract_functions);
    self.abstract_functions.dedup();
    self.variables.extend(other.variables.into_iter());
    self
  }
}
