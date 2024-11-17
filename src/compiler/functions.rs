use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::compiler::util::compile_word;

use super::{
  error::{err, CompileErrorKind::*, CompileResult},
  expression::{ExpKind, ExpressionCompilationPosition, TypedExp},
  metadata::Metadata,
  structs::TypeOrAbstractStruct,
  types::{Context, GenericOr, Type, TypeConstraint, TypeState},
  util::indent,
};

#[derive(Debug, Clone, PartialEq)]
pub struct TopLevelFunction {
  pub arg_metadata: Vec<Option<Metadata>>,
  pub return_metadata: Option<Metadata>,
  pub metadata: Option<Metadata>,
  pub body: TypedExp,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionImplementationKind {
  Builtin,
  Constructor,
  Composite(Rc<RefCell<TopLevelFunction>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractFunctionSignature {
  pub name: String,
  pub generic_args: Vec<(String, Vec<TypeConstraint>)>,
  pub arg_types: Vec<GenericOr<TypeOrAbstractStruct>>,
  pub return_type: GenericOr<TypeOrAbstractStruct>,
  pub implementation: FunctionImplementationKind,
}

impl AbstractFunctionSignature {
  pub fn generate_monomorphized(
    &self,
    arg_types: Vec<Type>,
    return_type: Type,
    base_ctx: &Context,
    new_ctx: &mut Context,
  ) -> CompileResult<Self> {
    let mut monomorphized = self.clone();
    let mut generic_bindings = HashMap::new();
    for i in 0..self.arg_types.len() {
      self.arg_types[i]
        .extract_generic_bindings(&arg_types[i], &mut generic_bindings);
    }
    self
      .return_type
      .extract_generic_bindings(&return_type, &mut generic_bindings);
    monomorphized.name = self.generic_args.iter().fold(
      Ok(monomorphized.name),
      |full_name: CompileResult<String>, (arg, bounds)| {
        let generic_type = generic_bindings.get(arg).unwrap();
        if let Some(unsatisfied_bound) = bounds
          .iter()
          .find(|constraint| !generic_type.satisfies_bounds(constraint))
        {
          err(
            UnsatisfiedTypeBound(unsatisfied_bound.clone()),
            vec![/*todo!("source paths")*/],
          )
        } else {
          full_name.map(|full_name| full_name + "_" + &generic_type.compile())
        }
      },
    )?;
    monomorphized.generic_args = vec![];
    if let FunctionImplementationKind::Composite(monomorphized_fn) =
      &mut monomorphized.implementation
    {
      let mut new_fn = monomorphized_fn.borrow().clone();
      let replacement_pairs: Vec<_> = generic_bindings
        .iter()
        .map(|(x, y)| (x.clone(), y.clone()))
        .collect();
      new_fn.body.replace_skolems(&replacement_pairs);
      new_fn.body.monomorphize(base_ctx, new_ctx)?;
      std::mem::swap(monomorphized_fn, &mut Rc::new(RefCell::new(new_fn)));
    } else {
      panic!("attempted to monomorphize non-composite abstract function")
    }
    Ok(monomorphized)
  }
  pub fn concretize(&self) -> FunctionSignature {
    let generic_variables: HashMap<String, TypeState> = self
      .generic_args
      .iter()
      .map(|(name, _bounds)| {
        (name.clone(), TypeState::fresh_unification_variable())
      })
      .collect();
    FunctionSignature {
      abstract_ancestor: Some(self.clone()),
      arg_types: self
        .arg_types
        .iter()
        .cloned()
        .map(|t| match t {
          GenericOr::Generic(var_name) => generic_variables
            .get(&var_name)
            .expect("unrecognized generic")
            .clone(),
          GenericOr::NonGeneric(TypeOrAbstractStruct::Type(t)) => {
            TypeState::Known(t)
          }
          GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(s)) => {
            TypeState::Known(Type::Struct(s.fill_generics(&generic_variables)))
          }
        })
        .collect(),
      return_type: match &self.return_type {
        GenericOr::Generic(var_name) => generic_variables
          .get(var_name)
          .expect("unrecognized generic")
          .clone(),
        GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(s)) => {
          TypeState::Known(Type::Struct(
            s.clone().fill_generics(&generic_variables),
          ))
        }
        GenericOr::NonGeneric(TypeOrAbstractStruct::Type(t)) => {
          TypeState::Known(t.clone())
        }
      },
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
  pub abstract_ancestor: Option<AbstractFunctionSignature>,
  pub arg_types: Vec<TypeState>,
  pub return_type: TypeState,
}

impl FunctionSignature {
  pub fn compatible(&self, other: &Self) -> bool {
    if self.arg_types.len() != other.arg_types.len() {
      return false;
    }
    !self
      .arg_types
      .iter()
      .zip(other.arg_types.iter())
      .find(|(a, b)| !TypeState::are_compatible(a, b))
      .is_some()
  }
  pub fn are_args_compatible(&self, arg_types: &Vec<TypeState>) -> bool {
    if arg_types.len() != self.arg_types.len() {
      return false;
    }
    for i in 0..arg_types.len() {
      if !TypeState::are_compatible(&self.arg_types[i], &arg_types[i]) {
        return false;
      }
    }
    true
  }
  pub fn mutually_constrain_arguments(
    &mut self,
    args: &mut Vec<TypeState>,
  ) -> CompileResult<bool> {
    if args.len() == self.arg_types.len() {
      let mut any_arg_changed = false;
      for i in 0..args.len() {
        any_arg_changed |=
          args[i].mutually_constrain(&mut self.arg_types[i])?;
      }
      Ok(any_arg_changed)
    } else {
      err(WrongArity, vec![/*todo!("source paths")*/])
    }
  }
}

pub struct BuiltInFunction {
  pub name: String,
  pub signature: AbstractFunctionSignature,
}

impl TopLevelFunction {
  pub fn compile(self, name: &str) -> CompileResult<String> {
    let TypedExp { data, kind, .. } = self.body;
    let (arg_types, return_type) =
      if let Type::Function(signature) = data.unwrap_known() {
        (signature.arg_types, signature.return_type)
      } else {
        panic!("attempted to compile function with invalid type data")
      };
    let (arg_names, body) = if let ExpKind::Function(arg_names, body) = kind {
      (arg_names, *body)
    } else {
      panic!("attempted to compile function with invalid ExpKind")
    };
    let args = arg_names
      .into_iter()
      .zip(arg_types.into_iter())
      .zip(self.arg_metadata.into_iter())
      .map(|((name, arg_type), metadata)| {
        format!(
          "{}{}: {}",
          Metadata::compile_optional(metadata),
          compile_word(name),
          arg_type.compile()
        )
      })
      .collect::<Vec<String>>()
      .join(", ");

    Ok(format!(
      "{}fn {}({args}) -> {}{} {{{}\n}}",
      Metadata::compile_optional(self.metadata),
      compile_word(name.to_string()),
      Metadata::compile_optional(self.return_metadata),
      return_type.compile(),
      indent(body.compile(ExpressionCompilationPosition::Return))
    ))
  }
}
