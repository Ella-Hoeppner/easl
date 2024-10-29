use core::fmt::Debug;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use sse::syntax::EncloserOrOperator;

use crate::parse::{Encloser, Operator, TyntTree};

use super::{
  builtins::{built_in_functions, ABNORMAL_CONSTRUCTOR_STRUCTS},
  error::{err, CompileErrorKind::*, CompileResult},
  functions::{
    AbstractFunctionSignature, FunctionImplementationKind, FunctionSignature,
  },
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

pub type AbstractType = GenericOr<TypeOrAbstractStruct>;

impl AbstractType {
  pub fn from_tynt_tree(
    tree: TyntTree,
    structs: &Vec<AbstractStruct>,
    generic_args: &Vec<String>,
    skolems: &Vec<String>,
  ) -> CompileResult<Self> {
    match &tree {
      TyntTree::Leaf(_, type_name) => Ok(Self::from_name(
        type_name.clone(),
        structs,
        generic_args,
        skolems,
      )?),
      TyntTree::Inner(
        (_, EncloserOrOperator::Encloser(Encloser::Parens)),
        children,
      ) => {
        let mut children_iter = children.into_iter();
        if let Some(TyntTree::Leaf(_, type_name)) = children_iter.next() {
          if let Some(s) = structs.iter().find(|s| s.name == *type_name) {
            let struct_generic_args = children_iter
              .map(|t| {
                Ok(Self::from_tynt_tree(
                  t.clone(),
                  structs,
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
            err(InvalidTypeName(type_name.clone()))
          }
        } else {
          err(InvalidType(tree))
        }
      }
      _ => err(InvalidType(tree)),
    }
  }
  pub fn from_name(
    name: String,
    structs: &Vec<AbstractStruct>,
    generic_args: &Vec<String>,
    skolems: &Vec<String>,
  ) -> CompileResult<Self> {
    Ok(if generic_args.contains(&name) {
      GenericOr::Generic(name)
    } else {
      GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::from_name(
        name, structs, skolems,
      )?))
    })
  }
  pub fn from_ast(
    ast: TyntTree,
    structs: &Vec<AbstractStruct>,
    generic_args: &Vec<String>,
    skolems: &Vec<String>,
  ) -> CompileResult<Self> {
    match ast {
      TyntTree::Leaf(_, leaf) => Ok(if generic_args.contains(&leaf) {
        GenericOr::Generic(leaf)
      } else {
        GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::from_name(
          leaf, structs, skolems,
        )?))
      }),
      TyntTree::Inner(
        (_, EncloserOrOperator::Encloser(Encloser::Parens)),
        children,
      ) => {
        let mut children_iter = children.into_iter();
        let generic_struct_name =
          if let Some(TyntTree::Leaf(_, leaf)) = children_iter.next() {
            Ok(leaf)
          } else {
            err(InvalidStructName)
          }?;
        let generic_struct = structs
          .iter()
          .find(|s| s.name == generic_struct_name)
          .ok_or(InvalidFunctionArgumentName)?
          .clone();
        let generic_args = children_iter
          .map(|subtree: TyntTree| {
            Self::from_ast(subtree, structs, generic_args, skolems)
          })
          .collect::<CompileResult<Vec<_>>>()?;
        Ok(GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
          generic_struct.fill_abstract_generics(generic_args),
        )))
      }
      _ => err(InvalidStructFieldType),
    }
  }
  pub fn concretize(
    &self,
    structs: &Vec<AbstractStruct>,
    skolems: &Vec<String>,
  ) -> CompileResult<Type> {
    match self {
      GenericOr::Generic(name) => {
        if skolems.contains(name) {
          Ok(Type::Skolem(name.clone()))
        } else {
          err(UnrecognizedGeneric(name.clone()))
        }
      }
      GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(s)) => {
        Ok(Type::Struct(s.concretize(structs, skolems)?))
      }
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
  pub fn from_tynt_tree(
    tree: TyntTree,
    structs: &Vec<AbstractStruct>,
    skolems: &Vec<String>,
  ) -> CompileResult<Self> {
    if let AbstractType::NonGeneric(TypeOrAbstractStruct::Type(t)) =
      AbstractType::from_tynt_tree(tree.clone(), structs, &vec![], skolems)?
    {
      Ok(t)
    } else {
      err(InvalidType(tree))
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
    structs: &Vec<AbstractStruct>,
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
        } else {
          return err(UnrecognizedTypeName(name));
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
          for arg_type in f.arg_types.iter_mut() {
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
  generic_args: &Vec<String>,
  skolems: &Vec<String>,
) -> CompileResult<(Option<AbstractType>, TyntTree)> {
  let (t, value) = extract_type_annotation_ast(exp)?;
  Ok((
    t.map(|t| AbstractType::from_tynt_tree(t, structs, generic_args, skolems))
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
  pub fn constrain(&mut self, mut other: TypeState) -> CompileResult<bool> {
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

#[derive(Debug, Clone)]
pub struct Context {
  pub structs: Vec<AbstractStruct>,
  pub variables: HashMap<String, Vec<Variable>>,
  pub abstract_functions: Vec<AbstractFunctionSignature>,
}

impl Context {
  pub fn empty() -> Self {
    Self {
      structs: vec![],
      variables: HashMap::new(),
      abstract_functions: vec![],
    }
  }
  pub fn default_global() -> Self {
    Self {
      structs: vec![],
      variables: HashMap::new(),
      abstract_functions: built_in_functions(),
    }
  }
  pub fn with_structs(mut self, structs: Vec<AbstractStruct>) -> Self {
    for s in &structs {
      if !ABNORMAL_CONSTRUCTOR_STRUCTS.contains(&s.name.as_str()) {
        self.add_abstract_function(AbstractFunctionSignature {
          name: s.name.clone(),
          generic_args: s.generic_args.clone(),
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
    self.structs = structs.into();
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
    t: &mut TypeState,
  ) -> CompileResult<bool> {
    let abstract_signatures = self.get_abstract_function_signatures(name);
    if abstract_signatures.is_empty() {
      t.mutually_constrain(self.get_typestate_mut(name)?)
    } else {
      t.constrain(TypeState::OneOf(
        abstract_signatures
          .iter()
          .cloned()
          .map(|signature| Type::Function(Box::new(signature.concretize())))
          .collect(),
      ))
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
