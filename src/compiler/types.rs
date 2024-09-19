use core::fmt::Debug;
use std::collections::HashMap;

use sse::syntax::EncloserOrOperator;

use crate::parse::{Operator, TyntTree};

use super::{
  builtins::{built_in_functions, built_in_multi_signature_functions},
  error::CompileError,
  functions::{AbstractFunctionSignature, ConcreteFunctionSignature},
  structs::Struct,
  util::compile_word,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyntType {
  None,
  F32,
  I32,
  U32,
  Bool,
  Struct(String),
  AbstractFunction(Box<AbstractFunctionSignature>),
  ConcreteFunction(Box<ConcreteFunctionSignature>),
  GenericVariable(String),
}
impl TyntType {
  pub fn compatible(&self, other: &Self, ctx: &UnificationContext) -> bool {
    match (self, other) {
      (TyntType::AbstractFunction(a), TyntType::ConcreteFunction(b)) => {
        a.is_concrete_signature_compatible(b.as_ref(), ctx)
      }
      (TyntType::ConcreteFunction(a), TyntType::AbstractFunction(b)) => {
        b.is_concrete_signature_compatible(a.as_ref(), ctx)
      }
      (a, b) => a == b,
    }
  }
  pub fn compatible_with_any(
    &self,
    others: &[Self],
    ctx: &UnificationContext,
  ) -> bool {
    others.iter().find(|x| self.compatible(x, ctx)).is_some()
  }
  pub fn filter_compatibles(
    &self,
    others: &[Self],
    ctx: &UnificationContext,
  ) -> Vec<Self> {
    others
      .iter()
      .filter(|x| self.compatible(x, ctx))
      .cloned()
      .collect()
  }
  pub fn from_name(
    name: String,
    struct_names: &Vec<String>,
  ) -> Result<Self, CompileError> {
    use TyntType::*;
    Ok(match name.as_str() {
      "F32" | "f32" => F32,
      "I32" | "i32" => I32,
      "U32" | "u32" => U32,
      "Bool" | "bool" => Bool,
      _ => {
        if struct_names.contains(&name) {
          Struct(name)
        } else {
          return Err(CompileError::UnrecognizedTypeName(name));
        }
      }
    })
  }
  pub fn from_tynt_tree(
    tree: TyntTree,
    struct_names: &Vec<String>,
  ) -> Result<Self, CompileError> {
    if let TyntTree::Leaf(_, type_name) = tree {
      Ok(TyntType::from_name(type_name, struct_names)?)
    } else {
      Err(CompileError::InvalidType)
    }
  }
  pub fn compile(&self) -> String {
    match self {
      TyntType::None => panic!("Attempted to compile None type"),
      TyntType::F32 => "f32".to_string(),
      TyntType::I32 => "i32".to_string(),
      TyntType::U32 => "u32".to_string(),
      TyntType::Bool => "bool".to_string(),
      TyntType::Struct(name) => compile_word(name.clone()),
      TyntType::AbstractFunction(_) => {
        panic!("Attempted to compile AbstractFunction type")
      }
      TyntType::ConcreteFunction(_) => {
        panic!("Attempted to compile ConcreteFunction type")
      }
      TyntType::GenericVariable(_) => {
        panic!("Attempted to compile GenericVariable type")
      }
    }
  }
}

pub fn extract_type_annotation_ast(
  exp: TyntTree,
) -> Result<(Option<TyntTree>, TyntTree), CompileError> {
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
  struct_names: &Vec<String>,
) -> Result<(Option<TyntType>, TyntTree), CompileError> {
  let (t, value) = extract_type_annotation_ast(exp)?;
  Ok((
    t.map(|t| TyntType::from_tynt_tree(t, struct_names))
      .map_or(Ok(None), |v| v.map(Some))?,
    value,
  ))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeState {
  Unknown,
  OneOf(Vec<TyntType>),
  Known(TyntType),
  UnificationVariable(usize),
}

impl TypeState {
  pub fn are_compatible<'a>(
    a: &'a Self,
    b: &'a Self,
    ctx: &'a UnificationContext,
  ) -> bool {
    use TypeState::*;
    let a = ctx.dereference_variable(a);
    let b = ctx.dereference_variable(b);
    match (a, b) {
      (Unknown, _) => true,
      (_, Unknown) => true,
      (UnificationVariable(_), _) => unreachable!(),
      (_, UnificationVariable(_)) => unreachable!(),
      (Known(a), Known(b)) => a == b,
      (OneOf(a), Known(b)) => a.contains(b),
      (Known(a), OneOf(b)) => b.contains(a),
      (OneOf(a), OneOf(b)) => a.iter().find(|a| b.contains(a)).is_some(),
    }
  }
  pub fn constrain(
    &mut self,
    mut other: TypeState,
    ctx: &mut UnificationContext,
  ) -> Result<bool, CompileError> {
    let changed = match (&self, &other) {
      (_, TypeState::Unknown) => false,
      (TypeState::Unknown, _) => {
        std::mem::swap(self, &mut other);
        true
      }
      (TypeState::Known(current_type), TypeState::Known(new_type)) => {
        if current_type.compatible(new_type, ctx) {
          return Err(CompileError::IncompatibleTypes);
        }
        false
      }
      (
        TypeState::OneOf(possibilities),
        TypeState::OneOf(other_possibilities),
      ) => {
        let mut new_possibilities = vec![];
        let mut changed = false;
        for possibility in possibilities {
          if possibility.compatible_with_any(other_possibilities, ctx) {
            new_possibilities.push(possibility.clone());
          } else {
            changed = true;
          }
        }
        std::mem::swap(
          self,
          &mut TypeState::OneOf(new_possibilities).simplified()?,
        );
        changed
      }
      (TypeState::OneOf(possibilities), TypeState::Known(t)) => {
        if t.compatible_with_any(&possibilities, ctx) {
          std::mem::swap(self, &mut TypeState::Known(t.clone()));
          true
        } else {
          return Err(CompileError::IncompatibleTypes);
        }
      }
      (TypeState::Known(t), TypeState::OneOf(possibilities)) => {
        if !t.compatible_with_any(&possibilities, ctx) {
          return Err(CompileError::IncompatibleTypes);
        }
        false
      }
      (TypeState::OneOf(_), TypeState::UnificationVariable(_)) => {
        todo!("unification")
      }
      (TypeState::Known(_), TypeState::UnificationVariable(_)) => {
        todo!("unification")
      }
      (TypeState::UnificationVariable(_), TypeState::OneOf(_)) => {
        todo!("unification")
      }
      (TypeState::UnificationVariable(_), TypeState::Known(_)) => {
        todo!("unification")
      }
      (
        TypeState::UnificationVariable(_),
        TypeState::UnificationVariable(_),
      ) => todo!("unification"),
    };
    self.simplify()?;
    Ok(changed)
  }
  pub fn mutually_constrain(
    &mut self,
    other: &mut TypeState,
    ctx: &mut UnificationContext,
  ) -> Result<bool, CompileError> {
    let self_changed = self.constrain(other.clone(), ctx)?;
    let other_changed = other.constrain(self.clone(), ctx)?;
    Ok(self_changed || other_changed)
  }
  pub fn constrain_fn_by_argument_types(
    &mut self,
    mut arg_types: Vec<TypeState>,
    ctx: &mut UnificationContext,
  ) -> Result<bool, CompileError> {
    match self {
      TypeState::OneOf(possibilities) => {
        let mut new_possibilities = possibilities
          .iter_mut()
          .map(|t| {
            Ok(match t {
              TyntType::ConcreteFunction(signature) => {
                if signature
                  .mutually_constrain_arguments(&mut arg_types, ctx)?
                {
                  Some(t.clone())
                } else {
                  None
                }
              }
              TyntType::AbstractFunction(signature) => {
                if signature.are_args_compatible(&arg_types) {
                  Some(t.clone())
                } else {
                  None
                }
              }
              _ => panic!("tried to constrain fn on non-fn"),
            })
          })
          .collect::<Result<Vec<_>, CompileError>>()?
          .into_iter()
          .filter_map(|x| x)
          .collect::<Vec<_>>();
        if new_possibilities.len() == possibilities.len() {
          Ok(false)
        } else {
          if new_possibilities.is_empty() {
            Err(CompileError::IncompatibleTypes)
          } else {
            std::mem::swap(
              self,
              &mut if new_possibilities.len() == 1 {
                TypeState::Known(new_possibilities.remove(0))
              } else {
                TypeState::OneOf(new_possibilities)
              },
            );
            Ok(true)
          }
        }
      }
      TypeState::Known(t) => match t {
        TyntType::ConcreteFunction(signature) => {
          if signature.mutually_constrain_arguments(&mut arg_types, ctx)? {
            Ok(false)
          } else {
            Err(CompileError::IncompatibleTypes)
          }
        }
        _ => panic!("tried to constrain fn on non-fn"),
      },
      TypeState::Unknown => Ok(false),
      TypeState::UnificationVariable(_) => todo!("unification"),
    }
  }
  pub fn is_compatible(&self, t: &TyntType) -> bool {
    match self {
      TypeState::Unknown => true,
      TypeState::OneOf(possibilities) => possibilities.contains(t),
      TypeState::Known(known_t) => known_t == t,
      TypeState::UnificationVariable(_) => todo!("unification"),
    }
  }
  pub fn simplify(&mut self) -> Result<(), CompileError> {
    Ok(if let TypeState::OneOf(mut possibilities) = self.clone() {
      possibilities.dedup();
      std::mem::swap(
        self,
        &mut match possibilities.len() {
          0 => return Err(CompileError::IncompatibleTypes),
          1 => TypeState::Known(possibilities.remove(0)),
          _ => TypeState::OneOf(possibilities),
        },
      )
    })
  }
  pub fn simplified(self) -> Result<Self, CompileError> {
    Ok(if let TypeState::OneOf(mut possibilities) = self {
      possibilities.dedup();
      match possibilities.len() {
        0 => return Err(CompileError::IncompatibleTypes),
        1 => TypeState::Known(possibilities.remove(0)),
        _ => TypeState::OneOf(possibilities),
      }
    } else {
      self
    })
  }
  pub fn compile(&self) -> String {
    if let TypeState::Known(t) = self {
      t.compile()
    } else {
      panic!("attempted to compile TypeState that wasn't Known")
    }
  }
}

#[derive(Debug, Clone)]
pub struct UnificationContext {
  pub unification_variables: Vec<TypeState>,
}

impl UnificationContext {
  pub fn new() -> Self {
    Self {
      unification_variables: vec![],
    }
  }
  pub fn constrain(
    &mut self,
    _index: usize,
    _t: &TypeState,
  ) -> Result<bool, CompileError> {
    todo!()
  }
  pub fn dereference_variable<'a>(
    &'a self,
    mut t: &'a TypeState,
  ) -> &'a TypeState {
    while let TypeState::UnificationVariable(var) = t {
      t = &self.unification_variables[*var];
    }
    t
  }
}

#[derive(Debug, Clone)]
pub struct Bindings {
  pub bindings: HashMap<String, Vec<TypeState>>,
  pub multi_signature_functions:
    HashMap<String, Vec<AbstractFunctionSignature>>,
}

impl Bindings {
  pub fn default_global() -> Self {
    let mut multi_signature_functions = HashMap::new();
    for (name, signatures) in built_in_multi_signature_functions() {
      multi_signature_functions.insert(name.to_string(), signatures);
    }
    Self {
      bindings: HashMap::new(),
      multi_signature_functions,
    }
  }
  pub fn bind(&mut self, name: &str, t: TypeState) {
    if !self.bindings.contains_key(name) {
      self.bindings.insert(name.to_string(), vec![]);
    }
    self.bindings.get_mut(name).unwrap().push(t);
  }
  pub fn unbind(&mut self, name: &str) -> TypeState {
    let name_bindings = self.bindings.get_mut(name).unwrap();
    let t = name_bindings.pop().unwrap();
    if name_bindings.is_empty() {
      self.bindings.remove(name);
    }
    t
  }
  pub fn is_bound(&self, name: &str) -> bool {
    self.bindings.contains_key(name)
      || self.multi_signature_functions.contains_key(name)
  }
  pub fn get_typestate_mut(
    &mut self,
    name: &str,
  ) -> Result<&mut TypeState, CompileError> {
    Ok(
      self
        .bindings
        .get_mut(name)
        .ok_or_else(|| CompileError::UnboundName(name.to_string()))?
        .last_mut()
        .unwrap(),
    )
  }
}

#[derive(Debug, Clone)]
pub struct Context {
  pub structs: Vec<Struct>,
  pub bindings: Bindings,
  pub unification_context: UnificationContext,
}

impl Context {
  pub fn default_global() -> Self {
    let mut multi_signature_functions = HashMap::new();
    for (name, signatures) in built_in_multi_signature_functions() {
      multi_signature_functions.insert(name.to_string(), signatures);
    }
    let mut ctx = Context {
      structs: vec![],
      bindings: Bindings::default_global(),
      unification_context: UnificationContext::new(),
    };
    for (name, signature) in built_in_functions() {
      ctx.bindings.bind(
        name,
        TypeState::Known(TyntType::AbstractFunction(Box::new(signature))),
      );
    }
    ctx
  }
  pub fn init_unification_variable(&mut self) -> TypeState {
    self
      .unification_context
      .unification_variables
      .push(TypeState::Unknown);
    TypeState::UnificationVariable(
      self.unification_context.unification_variables.len() - 1,
    )
  }
  pub fn with_structs(mut self, structs: Vec<Struct>) -> Self {
    for s in &structs {
      if s.has_normal_constructor {
        self.bindings.bind(
          &s.name,
          TypeState::Known(TyntType::AbstractFunction(Box::new(
            AbstractFunctionSignature {
              generic_args: vec![],
              arg_types: s
                .fields
                .iter()
                .map(|field| field.field_type.clone())
                .collect(),
              return_type: TyntType::Struct(s.name.clone()),
            },
          ))),
        )
      }
    }
    self.structs = structs;
    self
  }
  pub fn constrain_name_type(
    &mut self,
    name: &str,
    t: &mut TypeState,
  ) -> Result<bool, CompileError> {
    println!(
      "constraining name type: {name}\n{:#?}",
      self.bindings.get_typestate_mut(name)
    );
    let x = if let Some(signatures) =
      self.bindings.multi_signature_functions.get(name)
    {
      t.constrain(
        TypeState::OneOf(
          signatures
            .iter()
            .cloned()
            .map(|signature| TyntType::AbstractFunction(Box::new(signature)))
            .collect(),
        ),
        &mut self.unification_context,
      )
    } else {
      let x = self.bindings.get_typestate_mut(name)?;
      t.mutually_constrain(x, &mut self.unification_context)
    };
    println!("constrain_name_type result: {x:#?}");
    x
  }
}
