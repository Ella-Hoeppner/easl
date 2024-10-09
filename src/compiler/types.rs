use core::fmt::Debug;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use sse::syntax::EncloserOrOperator;

use crate::parse::{Operator, TyntTree};

use super::{
  builtins::{
    built_in_functions, built_in_multi_signature_functions,
    ABNORMAL_CONSTRUCTOR_STRUCTS,
  },
  error::{err, CompileErrorKind::*, CompileResult},
  functions::{AbstractFunctionSignature, FunctionSignature},
  structs::{AbstractStruct, Struct, TypeOrAbstractStruct},
  util::compile_word,
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

impl GenericOr<TypeOrAbstractStruct> {
  pub fn from_name(
    name: String,
    generic_args: &Vec<String>,
    structs: &Vec<AbstractStruct>,
  ) -> CompileResult<Self> {
    Ok(if generic_args.contains(&name) {
      GenericOr::Generic(name)
    } else {
      GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::from_name(
        name, structs,
      )?))
    })
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
}
impl Type {
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
    structs: &Vec<AbstractStruct>,
  ) -> CompileResult<Self> {
    use Type::*;
    Ok(match name.as_str() {
      "F32" | "f32" => F32,
      "I32" | "i32" => I32,
      "U32" | "u32" => U32,
      "Bool" | "bool" => Bool,
      _ => {
        if let Some(s) = structs.iter().find(|s| s.name == name) {
          Struct(s.clone().fill_generics_with_unification_variables())
        } else {
          return err(UnrecognizedTypeName(name));
        }
      }
    })
  }
  pub fn from_tynt_tree(
    tree: TyntTree,
    structs: &Vec<AbstractStruct>,
  ) -> CompileResult<Self> {
    if let TyntTree::Leaf(_, type_name) = tree {
      Ok(Type::from_name(type_name, structs)?)
    } else {
      err(InvalidType)
    }
  }
  pub fn compile(&self) -> String {
    match self {
      Type::None => panic!("Attempted to compile None type"),
      Type::F32 => "f32".to_string(),
      Type::I32 => "i32".to_string(),
      Type::U32 => "u32".to_string(),
      Type::Bool => "bool".to_string(),
      Type::Struct(s) => compile_word(s.name.clone()),
      Type::Function(_) => {
        panic!("Attempted to compile ConcreteFunction type")
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
) -> CompileResult<(Option<Type>, TyntTree)> {
  let (t, value) = extract_type_annotation_ast(exp)?;
  Ok((
    t.map(|t| Type::from_tynt_tree(t, structs))
      .map_or(Ok(None), |v| v.map(Some))?,
    value,
  ))
}

/*pub fn parse_abstract_type(
  tree: TyntTree,
  generic_args: &Vec<String>,
  structs: &Vec<AbstractStruct>,
) -> CompileResult<GenericOr<TypeOrAbstractStruct>> {
  todo!()
}

pub fn extract_abstract_type_annotation(
  exp: TyntTree,
  generic_args: &Vec<String>,
  structs: &Vec<AbstractStruct>,
) -> CompileResult<(Option<GenericOr<TypeOrAbstractStruct>>, TyntTree)> {
  let (t, value) = extract_type_annotation_ast(exp)?;
  Ok((
    t.map(|t| parse_abstract_type(t, generic_args, structs))
      .map_or(Ok(None), |v| v.map(Some))?,
    value,
  ))
}*/

#[derive(Debug, Clone, PartialEq)]
pub enum TypeState {
  Unknown,
  OneOf(Vec<Type>),
  Known(Type),
  UnificationVariable(Rc<RefCell<TypeState>>),
}

impl TypeState {
  pub fn unwrap_known(&self) -> Type {
    self.with_dereferenced(|typestate| {
      if let TypeState::Known(t) = typestate {
        t.clone()
      } else {
        panic!("unwrapped non-Known TypeState")
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
          err(FunctionArgumentTypesIncompatible(
            typestate.clone(),
            arg_types,
          ))
        } else {
          std::mem::swap(
            typestate,
            &mut TypeState::OneOf(new_possibilities).simplified()?,
          );
          Ok(anything_changed)
        }
      }
      TypeState::Known(t) => match t {
        Type::Function(signature) => {
          Ok(signature.mutually_constrain_arguments(&mut arg_types)?)
        }
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
pub struct Bindings {
  pub variables: HashMap<String, Vec<Variable>>,
  pub abstract_functions: HashMap<String, Vec<AbstractFunctionSignature>>,
}

impl Bindings {
  pub fn default_global() -> Self {
    let mut abstract_functions = HashMap::new();
    for (name, signatures) in built_in_multi_signature_functions() {
      abstract_functions.insert(name.to_string(), signatures);
    }
    Self {
      variables: HashMap::new(),
      abstract_functions,
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
    name: String,
    signatures: Vec<AbstractFunctionSignature>,
  ) {
    self.abstract_functions.insert(name, signatures);
  }
  pub fn is_bound(&self, name: &str) -> bool {
    self.variables.contains_key(name)
      || self.abstract_functions.contains_key(name)
  }
  pub fn get_variable_kind(&self, name: &str) -> &VariableKind {
    &self.variables.get(name).unwrap().last().unwrap().kind
  }
  pub fn get_typestate_mut(
    &mut self,
    name: &str,
  ) -> CompileResult<&mut TypeState> {
    Ok(
      &mut self
        .variables
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
  pub structs: Vec<AbstractStruct>,
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
      ctx
        .bindings
        .add_abstract_function(name.to_string(), vec![signature]);
    }
    ctx
  }
  pub fn with_structs(mut self, structs: Vec<AbstractStruct>) -> Self {
    for s in &structs {
      if !ABNORMAL_CONSTRUCTOR_STRUCTS.contains(&s.name.as_str()) {
        self.bindings.add_abstract_function(
          s.name.clone(),
          vec![AbstractFunctionSignature {
            generic_args: s.generic_args.clone(),
            arg_types: s
              .fields
              .iter()
              .map(|field| field.field_type.clone())
              .collect(),
            return_type: GenericOr::NonGeneric(
              TypeOrAbstractStruct::AbstractStruct(s.clone()),
            ),
          }],
        );
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
    if let Some(signatures) = self.bindings.abstract_functions.get(name) {
      t.constrain(TypeState::OneOf(
        signatures
          .iter()
          .cloned()
          .map(|signature| Type::Function(Box::new(signature.concretize())))
          .collect(),
      ))
    } else {
      t.mutually_constrain(self.bindings.get_typestate_mut(name)?)
    }
  }
  pub fn add_monomorphized_struct(&mut self, s: AbstractStruct) {
    if self
      .structs
      .iter()
      .find(|existing_struct| existing_struct.name == s.name)
      .is_none()
    {
      self.structs.push(s);
    }
  }
}
