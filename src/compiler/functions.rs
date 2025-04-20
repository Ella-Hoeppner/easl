use std::{
  cell::RefCell,
  collections::{HashMap, HashSet},
  rc::Rc,
};

use take_mut::take;

use crate::compiler::util::compile_word;

use super::{
  effects::Effect,
  error::{err, CompileError, CompileErrorKind::*, CompileResult, SourceTrace},
  expression::{ExpKind, ExpressionCompilationPosition, TypedExp},
  metadata::Metadata,
  program::Program,
  structs::AbstractStruct,
  types::{AbstractType, ExpTypeInfo, Type, TypeConstraint, TypeState},
  util::indent,
};

#[derive(Debug, Clone, PartialEq)]
pub struct TopLevelFunction {
  pub arg_names: Vec<Rc<str>>,
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
  pub name: Rc<str>,
  pub generic_args: Vec<(Rc<str>, Vec<TypeConstraint>)>,
  pub arg_types: Vec<AbstractType>,
  pub mutated_args: Vec<usize>,
  pub return_type: AbstractType,
  pub implementation: FunctionImplementationKind,
  pub associative: bool,
}

impl AbstractFunctionSignature {
  pub fn arg_names(
    &self,
    source_trace: SourceTrace,
  ) -> CompileResult<Vec<Rc<str>>> {
    if let FunctionImplementationKind::Composite(f) = &self.implementation {
      Ok(f.borrow().arg_names.clone())
    } else {
      err(NoArgNamesForFunction, source_trace)
    }
  }
  pub fn arg_metadata(
    &self,
    source_trace: SourceTrace,
  ) -> CompileResult<Vec<Option<Metadata>>> {
    if let FunctionImplementationKind::Composite(f) = &self.implementation {
      Ok(f.borrow().arg_metadata.clone())
    } else {
      err(NoArgNamesForFunction, source_trace)
    }
  }
  pub fn implementation(
    &self,
    source_trace: SourceTrace,
  ) -> CompileResult<TopLevelFunction> {
    if let FunctionImplementationKind::Composite(f) = &self.implementation {
      Ok(f.borrow().clone())
    } else {
      err(NoArgNamesForFunction, source_trace)
    }
  }
  pub fn generate_monomorphized(
    &self,
    arg_types: Vec<Type>,
    return_type: Type,
    base_program: &Program,
    new_program: &mut Program,
    source_trace: SourceTrace,
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
    monomorphized.name = self
      .generic_args
      .iter()
      .fold(
        Ok(monomorphized.name.to_string()),
        |full_name: CompileResult<String>, (arg, bounds)| {
          let generic_type = generic_bindings.get(arg).unwrap();
          if let Some(unsatisfied_bound) = bounds
            .iter()
            .find(|constraint| !generic_type.satisfies_constraints(constraint))
          {
            err(
              UnsatisfiedTypeConstraint(unsatisfied_bound.clone()),
              source_trace.clone(),
            )
          } else {
            full_name.map(|full_name| full_name + "_" + &generic_type.compile())
          }
        },
      )?
      .into();
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
      new_fn.body.monomorphize(base_program, new_program)?;
      std::mem::swap(monomorphized_fn, &mut Rc::new(RefCell::new(new_fn)));
    } else {
      panic!("attempted to monomorphize non-composite abstract function")
    }
    Ok(monomorphized)
  }
  pub fn generate_higher_order_functions_inlined_version(
    &self,
    fn_args: Vec<TypedExp>,
    source_trace: SourceTrace,
  ) -> CompileResult<Self> {
    let mut implementation = self.implementation(source_trace.clone())?;
    let (fn_parameter_names, remaining_parameters) = self
      .arg_types
      .iter()
      .cloned()
      .zip(implementation.arg_names.into_iter())
      .zip(implementation.arg_metadata.into_iter())
      .map(|((t, name), metadata)| {
        if let AbstractType::Type(Type::Function(_)) = t {
          (Some(name.clone()), None)
        } else {
          (None, Some((name.clone(), (metadata.clone(), t))))
        }
      })
      .collect::<(
        Vec<Option<Rc<str>>>,
        Vec<Option<(Rc<str>, (Option<Metadata>, AbstractType))>>,
      )>();
    let (new_parameter_names, (new_parameter_metadata, new_parameter_types)): (
      Vec<_>,
      (Vec<_>, Vec<_>),
    ) = remaining_parameters.into_iter().filter_map(|x| x).collect();
    let (retained_argument_indeces, removed_param_names): (
      Vec<usize>,
      Vec<Rc<str>>,
    ) = fn_parameter_names
      .into_iter()
      .enumerate()
      .filter_map(|(i, x)| x.map(|x| (i, x)))
      .collect();
    take(&mut implementation.body, |mut body| {
      body.kind = if let ExpKind::Function(_, body_exp) = body.kind {
        ExpKind::Function(new_parameter_names.clone(), body_exp)
      } else {
        unreachable!()
      };
      if let TypeState::Known(Type::Function(signature)) = &mut body.data.kind {
        for i in retained_argument_indeces.into_iter().rev() {
          signature.arg_types.remove(i);
        }
      } else {
        unreachable!()
      }
      body
    });
    implementation
      .body
      .inline_args(&removed_param_names, &fn_args);
    implementation.arg_names = new_parameter_names;
    implementation.arg_metadata = new_parameter_metadata;
    let f = AbstractFunctionSignature {
      name: (self.name.to_string()
        + "__"
        + &fn_args
          .iter()
          .map(|arg| {
            if let ExpKind::Name(name) = &arg.kind {
              &**name
            } else {
              panic!("element of fn_args wasn't a Name")
            }
          })
          .collect::<Vec<&str>>()
          .join("_"))
        .into(),
      generic_args: self.generic_args.clone(),
      arg_types: new_parameter_types,
      mutated_args: vec![],
      return_type: self.return_type.clone(),
      implementation: FunctionImplementationKind::Composite(Rc::new(
        RefCell::new(implementation),
      )),
      associative: self.associative,
    };
    Ok(f)
  }
  pub fn concretize(
    f: Rc<RefCell<Self>>,
    structs: &Vec<Rc<AbstractStruct>>,
    source_trace: SourceTrace,
  ) -> CompileResult<FunctionSignature> {
    let (generic_variables, generic_constraints): (
      HashMap<Rc<str>, ExpTypeInfo>,
      HashMap<Rc<str>, Vec<TypeConstraint>>,
    ) = f
      .borrow()
      .generic_args
      .iter()
      .map(|(name, bounds)| {
        (
          (name.clone(), TypeState::fresh_unification_variable().into()),
          (name.clone(), bounds.clone()),
        )
      })
      .collect();
    Ok(FunctionSignature {
      arg_types: f
        .borrow()
        .arg_types
        .iter()
        .map(|t| {
          Ok(match t {
            AbstractType::Generic(var_name) => (
              generic_variables
                .get(var_name)
                .expect("unrecognized generic")
                .clone(),
              generic_constraints
                .get(var_name)
                .expect("unrecognized generic")
                .clone(),
            ),
            AbstractType::Type(t) => {
              (TypeState::Known(t.clone()).into(), vec![])
            }
            AbstractType::AbstractStruct(s) => (
              TypeState::Known(Type::Struct(AbstractStruct::fill_generics(
                s.clone(),
                &generic_variables,
                structs,
                source_trace.clone(),
              )?))
              .into(),
              vec![],
            ),
            AbstractType::AbstractArray { size, inner_type } => (
              TypeState::Known(Type::Array(
                Some(size.clone()),
                inner_type
                  .fill_generics(
                    &generic_variables,
                    structs,
                    source_trace.clone(),
                  )?
                  .into(),
              ))
              .into(),
              vec![],
            ),
            AbstractType::Reference(abstract_type) => (
              TypeState::Known(
                Type::Reference(
                  abstract_type
                    .fill_generics(
                      &generic_variables,
                      structs,
                      source_trace.clone(),
                    )?
                    .into(),
                )
                .into(),
              )
              .into(),
              vec![],
            ),
          })
        })
        .collect::<CompileResult<_>>()?,
      return_type: match &f.borrow().return_type {
        AbstractType::Generic(var_name) => generic_variables
          .get(var_name)
          .expect("unrecognized generic")
          .clone(),
        AbstractType::AbstractStruct(s) => {
          TypeState::Known(Type::Struct(AbstractStruct::fill_generics(
            s.clone(),
            &generic_variables,
            structs,
            source_trace,
          )?))
          .into()
        }
        AbstractType::Type(t) => TypeState::Known(t.clone()).into(),
        AbstractType::AbstractArray { size, inner_type } => {
          TypeState::Known(Type::Array(
            Some(size.clone()),
            inner_type
              .fill_generics(&generic_variables, structs, source_trace)?
              .into(),
          ))
          .into()
        }
        AbstractType::Reference(inner_type) => {
          TypeState::Known(Type::Reference(
            inner_type
              .fill_generics(&generic_variables, structs, source_trace)?
              .into(),
          ))
          .into()
        }
      },
      abstract_ancestor: Some(Rc::new(f.borrow().clone())),
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
  pub abstract_ancestor: Option<Rc<AbstractFunctionSignature>>,
  pub arg_types: Vec<(ExpTypeInfo, Vec<TypeConstraint>)>,
  pub return_type: ExpTypeInfo,
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
      .find(
        |((a_typestate, a_constraints), (b_typestate, b_constraints))| {
          !TypeState::are_compatible(a_typestate, b_typestate)
            || a_constraints != b_constraints
        },
      )
      .is_some()
  }
  pub fn are_args_compatible(&self, arg_types: &Vec<TypeState>) -> bool {
    if arg_types.len() != self.arg_types.len() {
      if let Some(ancestor) = &self.abstract_ancestor {
        if ancestor.associative {
          if arg_types.len() == 0 {
            return false;
          }
          let (arg_type, arg_constraints) = &self.arg_types[0];
          for arg_typestate in arg_types {
            if !TypeState::are_compatible(arg_typestate, &arg_type.kind) {
              return false;
            }
            if let TypeState::Known(t) = arg_typestate {
              for constraint in arg_constraints.iter() {
                if !t.satisfies_constraints(constraint) {
                  return false;
                }
              }
            }
          }
          return true;
        }
      }
      return false;
    }
    for i in 0..arg_types.len() {
      let (arg_typestate, arg_constraints) = &self.arg_types[i];
      if !TypeState::are_compatible(arg_typestate, &arg_types[i]) {
        return false;
      }
      if let TypeState::Known(t) = &arg_types[i] {
        for constraint in arg_constraints {
          if !t.satisfies_constraints(constraint) {
            return false;
          }
        }
      }
    }
    true
  }
  pub fn mutually_constrain_arguments(
    &mut self,
    args: &mut Vec<TypeState>,
    source_trace: SourceTrace,
  ) -> (bool, Vec<CompileError>) {
    if args.len() == self.arg_types.len() {
      let mut any_arg_changed = false;
      let mut all_errors = vec![];
      for i in 0..args.len() {
        let (changed, mut errors) = args[i]
          .mutually_constrain(&mut self.arg_types[i].0, source_trace.clone());
        any_arg_changed |= changed;
        all_errors.append(&mut errors);
      }
      (any_arg_changed, all_errors)
    } else {
      if let Some(ancestor) = &self.abstract_ancestor {
        if ancestor.associative {
          if args.len() != 0 {
            let arg_type = &mut self.arg_types.get_mut(0).unwrap().0;
            let mut any_arg_changed = false;
            let mut all_errors = vec![];
            for i in 0..args.len() {
              let (changed, mut errors) =
                args[i].mutually_constrain(arg_type, source_trace.clone());
              any_arg_changed |= changed;
              all_errors.append(&mut errors);
            }
            return (any_arg_changed, all_errors);
          }
        }
      }
      (
        false,
        vec![CompileError::new(WrongArity(self.name()), source_trace)],
      )
    }
  }
  pub fn name(&self) -> Option<Rc<str>> {
    self
      .abstract_ancestor
      .as_ref()
      .map(|abstract_ancestor| abstract_ancestor.name.clone())
  }
  pub fn effects(&self) -> HashSet<Effect> {
    if let Some(abstract_ancestor) = &self.abstract_ancestor {
      match &abstract_ancestor.implementation {
        FunctionImplementationKind::Composite(f) => f.borrow().body.effects(),
        _ => HashSet::new(),
      }
    } else {
      HashSet::new()
    }
  }
}

pub struct BuiltInFunction {
  pub name: Rc<str>,
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
          arg_type.0.compile()
        )
      })
      .collect::<Vec<String>>()
      .join(", ");

    Ok(format!(
      "{}fn {}({args}){} {{{}\n}}",
      Metadata::compile_optional(self.metadata),
      compile_word(name.into()),
      if return_type.kind.unwrap_known() == Type::Unit {
        "".to_string()
      } else {
        format!(
          " -> {}{}",
          Metadata::compile_optional(self.return_metadata),
          return_type.compile()
        )
      },
      indent(
        body.compile(if return_type.kind.unwrap_known() == Type::Unit {
          ExpressionCompilationPosition::InnerLine
        } else {
          ExpressionCompilationPosition::Return
        })
      )
    ))
  }
}
