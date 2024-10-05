use std::collections::HashMap;

use crate::compiler::util::compile_word;

use super::{
  error::{err, CompileErrorKind::*, CompileResult},
  expression::{ExpKind, ExpressionCompilationPosition, TypedExp},
  metadata::Metadata,
  types::{Context, Type, TypeState},
  util::indent,
};

#[derive(Debug)]
pub struct TopLevelFunction {
  pub name: String,
  pub arg_metadata: Vec<Option<Metadata>>,
  pub return_metadata: Option<Metadata>,
  pub metadata: Option<Metadata>,
  pub body: TypedExp,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractFunctionSignature {
  pub generic_args: Vec<String>,
  pub arg_types: Vec<Type>,
  pub return_type: Type,
}

impl AbstractFunctionSignature {
  pub fn are_args_compatible(&self, args: &Vec<TypeState>) -> bool {
    if args.len() != self.arg_types.len() {
      return false;
    }
    for i in 0..args.len() {
      if !args[i].is_compatible(&self.arg_types[i]) {
        return false;
      }
    }
    true
  }
  pub fn concretize(&self) -> ConcreteFunctionSignature {
    let generic_variables: HashMap<String, TypeState> = self
      .generic_args
      .iter()
      .map(|name| (name.clone(), TypeState::fresh_unification_variable()))
      .collect();
    ConcreteFunctionSignature {
      arg_types: self
        .arg_types
        .iter()
        .cloned()
        .map(|t| t.fill_generics(&generic_variables))
        .collect(),
      return_type: self.return_type.clone().fill_generics(&generic_variables),
    }
  }
  pub fn is_concrete_signature_compatible(
    &self,
    concrete_signature: &ConcreteFunctionSignature,
  ) -> bool {
    if self.arg_types.len() != concrete_signature.arg_types.len() {
      return false;
    }
    let mut generic_assignments: HashMap<String, TypeState> = HashMap::new();
    for i in 0..self.arg_types.len() {
      if !self.arg_types[i].is_concrete_compatible(
        &concrete_signature.arg_types[i],
        &mut generic_assignments,
      ) {
        return false;
      }
    }
    self.return_type.is_concrete_compatible(
      &concrete_signature.return_type,
      &mut generic_assignments,
    )
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConcreteFunctionSignature {
  pub arg_types: Vec<TypeState>,
  pub return_type: TypeState,
}

impl ConcreteFunctionSignature {
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
      err(WrongArity)
    }
  }
}

pub struct BuiltInFunction {
  pub name: String,
  pub signature: AbstractFunctionSignature,
}

impl TopLevelFunction {
  pub fn compile(self) -> CompileResult<String> {
    let TypedExp { data, kind } = self.body;
    let (arg_types, return_type) =
      if let TypeState::Known(Type::AbstractFunction(signature)) = data {
        (signature.arg_types, signature.return_type)
      } else {
        panic!("attempted to compile function with invalid type data")
      };
    let (arg_names, body) = if let ExpKind::Function(arg_names, body) = kind {
      (arg_names, *body)
    } else {
      panic!("attempted to compile function with invalid ExpKind")
    };
    let name = compile_word(self.name);
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
      "{}fn {name}({args}) -> {}{} {{{}\n}}",
      Metadata::compile_optional(self.metadata),
      Metadata::compile_optional(self.return_metadata),
      return_type.compile(),
      indent(body.compile(ExpressionCompilationPosition::Return))
    ))
  }
}
