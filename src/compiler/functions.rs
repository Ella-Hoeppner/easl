use std::collections::HashMap;

use crate::compiler::util::compile_word;

use super::{
  error::{err, CompileErrorKind::*, CompileResult},
  expression::{ExpKind, ExpressionCompilationContext, TypedExp},
  metadata::Metadata,
  types::{Context, TyntType, TypeState, UnificationContext},
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AbstractFunctionSignature {
  pub generic_args: Vec<String>,
  pub arg_types: Vec<TyntType>,
  pub return_type: TyntType,
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
  pub fn concretize(&self, ctx: &mut Context) -> ConcreteFunctionSignature {
    println!("concretizing AbstractFunctionSignature");
    //println!("concretizing! {:#?}", self);
    let generic_variables: HashMap<String, TypeState> = self
      .generic_args
      .iter()
      .map(|name| (name.clone(), ctx.init_unification_variable()))
      .collect();
    let convert_type = |t: &TyntType| {
      if let TyntType::GenericVariable(name) = t {
        generic_variables
          .get(name)
          .expect("found unrecognized generic name while concretizing ")
          .clone()
      } else {
        TypeState::Known(t.clone())
      }
    };
    /*println!(
      "concretized to: {:#?}",
      ConcreteFunctionSignature {
        arg_types: self.arg_types.iter().map(convert_type).collect(),
        return_type: convert_type(&self.return_type),
      }
    );*/
    ConcreteFunctionSignature {
      arg_types: self.arg_types.iter().map(convert_type).collect(),
      return_type: convert_type(&self.return_type),
    }
  }
  pub fn is_concrete_signature_compatible(
    &self,
    concrete_signature: &ConcreteFunctionSignature,
    ctx: &UnificationContext,
  ) -> bool {
    if self.arg_types.len() != concrete_signature.arg_types.len() {
      return false;
    }
    let mut assignments: HashMap<String, TypeState> = HashMap::new();
    for i in 0..self.arg_types.len() {
      let t = &concrete_signature.arg_types[i];
      if let TyntType::GenericVariable(generic_name) = &self.arg_types[i] {
        if let Some(assignment) = assignments.get(generic_name) {
          if !TypeState::are_compatible(t, assignment, ctx) {
            return false;
          }
        } else {
          assignments.insert(generic_name.clone(), t.clone());
        }
      } else {
        if !t.is_compatible(&self.arg_types[i]) {
          return false;
        }
      }
    }
    todo!()
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConcreteFunctionSignature {
  pub arg_types: Vec<TypeState>,
  pub return_type: TypeState,
}

impl ConcreteFunctionSignature {
  pub fn mutually_constrain_arguments(
    &mut self,
    args: &mut Vec<TypeState>,
    ctx: &mut UnificationContext,
  ) -> CompileResult<bool> {
    if args.len() == self.arg_types.len() {
      let mut any_arg_changed = false;
      for i in 0..args.len() {
        any_arg_changed |=
          args[i].mutually_constrain(&mut self.arg_types[i], ctx)?;
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
      if let TypeState::Known(TyntType::AbstractFunction(signature)) = data {
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
      indent(body.compile(ExpressionCompilationContext::Return))
    ))
  }
}
