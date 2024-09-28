use core::fmt::Debug;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use sse::syntax::EncloserOrOperator;

use crate::parse::{Operator, TyntTree};

use super::{
  builtins::{built_in_functions, built_in_multi_signature_functions},
  error::{err, CompileErrorKind::*, CompileResult},
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
  pub fn compatible(&self, other: &Self) -> bool {
    let b = match (self, other) {
      (TyntType::AbstractFunction(a), TyntType::ConcreteFunction(b)) => {
        a.is_concrete_signature_compatible(b.as_ref())
      }
      (TyntType::ConcreteFunction(a), TyntType::AbstractFunction(b)) => {
        b.is_concrete_signature_compatible(a.as_ref())
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
    name: String,
    struct_names: &Vec<String>,
  ) -> CompileResult<Self> {
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
          return err(UnrecognizedTypeName(name));
        }
      }
    })
  }
  pub fn from_tynt_tree(
    tree: TyntTree,
    struct_names: &Vec<String>,
  ) -> CompileResult<Self> {
    if let TyntTree::Leaf(_, type_name) = tree {
      Ok(TyntType::from_name(type_name, struct_names)?)
    } else {
      err(InvalidType)
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
  struct_names: &Vec<String>,
) -> CompileResult<(Option<TyntType>, TyntTree)> {
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
  UnificationVariable(Rc<RefCell<TypeState>>),
}

impl TypeState {
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
  fn constrain_inner(&mut self, mut other: TypeState) -> CompileResult<bool> {
    self.with_dereferenced_mut(move |this| {
      other.with_dereferenced_mut(|other| {
        let changed = match (&this, &other) {
          (TypeState::UnificationVariable(_), _) => unreachable!(),
          (_, TypeState::UnificationVariable(_)) => unreachable!(),
          (_, TypeState::Unknown) => false,
          (TypeState::Unknown, _) => {
            std::mem::swap(this, other);
            true
          }
          (TypeState::Known(current_type), TypeState::Known(new_type)) => {
            if !current_type.compatible(new_type) {
              return err(IncompatibleTypes(this.clone(), other.clone()));
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
              return err(IncompatibleTypes(this.clone(), other.clone()));
            }
          }
          (TypeState::Known(t), TypeState::OneOf(possibilities)) => {
            if !t.compatible_with_any(&possibilities) {
              return err(IncompatibleTypes(this.clone(), other.clone()));
            }
            false
          }
        };
        this.simplify()?;
        Ok(changed)
      })
    })
  }
  pub fn constrain(&mut self, other: TypeState) -> CompileResult<bool> {
    self.constrain_inner(other)
  }
  pub fn mutually_constrain(
    &mut self,
    other: &mut TypeState,
  ) -> CompileResult<bool> {
    let self_changed = self.constrain(other.clone())?;
    let other_changed = other.constrain(self.clone())?;
    Ok(self_changed || other_changed)
  }
  pub fn constrain_fn_by_argument_types(
    &mut self,
    mut arg_types: Vec<TypeState>,
  ) -> CompileResult<bool> {
    self.with_dereferenced_mut(|x| match x {
      TypeState::OneOf(possibilities) => {
        let mut new_possibilities = possibilities
          .iter_mut()
          .map(|t| {
            Ok(match t {
              TyntType::ConcreteFunction(signature) => {
                if signature.mutually_constrain_arguments(&mut arg_types)? {
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
          .collect::<CompileResult<Vec<_>>>()?
          .into_iter()
          .filter_map(|x| x)
          .collect::<Vec<_>>();
        if new_possibilities.len() == possibilities.len() {
          Ok(false)
        } else {
          if new_possibilities.is_empty() {
            err(FunctionArgumentTypesIncompatible(x.clone(), arg_types))
          } else {
            std::mem::swap(
              x,
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
          Ok(signature.mutually_constrain_arguments(&mut arg_types)?)
        }
        TyntType::AbstractFunction(_) => Ok(false),
        other => panic!(
          "tried to constrain fn on non-fn \n\n{arg_types:#?} \n\n{other:#?}"
        ),
      },
      TypeState::Unknown => Ok(false),
      TypeState::UnificationVariable(_) => unreachable!(),
    })
  }
  pub fn is_compatible(&self, t: &TyntType) -> bool {
    self.with_dereferenced(|x| match x {
      TypeState::Unknown => true,
      TypeState::OneOf(possibilities) => possibilities.contains(t),
      TypeState::Known(known_t) => known_t == t,
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
    if let TypeState::Known(t) = self {
      t.compile()
    } else {
      panic!("attempted to compile TypeState that wasn't Known")
    }
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
pub struct Bindings {
  pub bindings: HashMap<String, Vec<Variable>>,
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
  pub fn bind(&mut self, name: &str, v: Variable) {
    if !self.bindings.contains_key(name) {
      self.bindings.insert(name.to_string(), vec![]);
    }
    self.bindings.get_mut(name).unwrap().push(v);
  }
  pub fn unbind(&mut self, name: &str) -> Variable {
    let name_bindings = self.bindings.get_mut(name).unwrap();
    let v = name_bindings.pop().unwrap();
    if name_bindings.is_empty() {
      self.bindings.remove(name);
    }
    v
  }
  pub fn is_bound(&self, name: &str) -> bool {
    self.bindings.contains_key(name)
      || self.multi_signature_functions.contains_key(name)
  }
  pub fn get_variable_kind(&self, name: &str) -> &VariableKind {
    &self.bindings.get(name).unwrap().last().unwrap().kind
  }
  pub fn get_typestate_mut(
    &mut self,
    name: &str,
  ) -> CompileResult<&mut TypeState> {
    Ok(
      &mut self
        .bindings
        .get_mut(name)
        .ok_or_else(|| UnboundName(name.to_string()))?
        .last_mut()
        .unwrap()
        .typestate,
    )
  }
}

#[derive(Debug, Clone)]
pub struct Context {
  pub structs: Vec<Struct>,
  pub bindings: Bindings,
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
    };
    for (name, signature) in built_in_functions() {
      ctx.bindings.bind(
        name,
        Variable::new(TypeState::Known(TyntType::AbstractFunction(Box::new(
          signature,
        )))),
      );
    }
    ctx
  }
  pub fn with_structs(mut self, structs: Vec<Struct>) -> Self {
    for s in &structs {
      if s.has_normal_constructor {
        self.bindings.bind(
          &s.name,
          Variable::new(TypeState::Known(TyntType::AbstractFunction(
            Box::new(AbstractFunctionSignature {
              generic_args: vec![],
              arg_types: s
                .fields
                .iter()
                .map(|field| field.field_type.clone())
                .collect(),
              return_type: TyntType::Struct(s.name.clone()),
            }),
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
  ) -> CompileResult<bool> {
    if let Some(signatures) = self.bindings.multi_signature_functions.get(name)
    {
      t.constrain(TypeState::OneOf(
        signatures
          .iter()
          .cloned()
          .map(|signature| TyntType::AbstractFunction(Box::new(signature)))
          .collect(),
      ))
    } else {
      t.mutually_constrain(self.bindings.get_typestate_mut(name)?)
    }
  }
}
