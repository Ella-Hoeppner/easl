use std::{
  cell::RefCell,
  collections::{HashMap, HashSet},
  rc::Rc,
};

use take_mut::take;

use crate::compiler::{
  effects::{Effect, EffectType},
  enums::AbstractEnum,
  program::{NameContext, TypeDefs},
  types::Variable,
  util::compile_word,
};

use super::{
  annotation::Annotation,
  error::{
    CompileError, CompileErrorKind::*, CompileResult, ErrorLog, SourceTrace,
    err,
  },
  expression::{ExpKind, ExpressionCompilationPosition, TypedExp},
  program::Program,
  structs::AbstractStruct,
  types::{AbstractType, ExpTypeInfo, Type, TypeConstraint, TypeState},
  util::indent,
};

#[derive(Debug, Clone, PartialEq)]
pub struct TopLevelFunction {
  pub arg_names: Vec<Rc<str>>,
  pub arg_annotations: Vec<Option<Annotation>>,
  pub return_annotation: Option<Annotation>,
  pub annotation: Option<Annotation>,
  pub body: TypedExp,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionImplementationKind {
  Builtin,
  StructConstructor,
  EnumConstructor(Rc<str>),
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
  pub(crate) fn normalized_signature(
    &self,
  ) -> (Vec<Vec<TypeConstraint>>, Vec<AbstractType>, AbstractType) {
    let mut used_generic_names = vec![];
    for t in self.arg_types.iter() {
      t.track_generic_names(&mut used_generic_names);
    }
    self
      .return_type
      .track_generic_names(&mut used_generic_names);
    let generic_name_order: Vec<Rc<str>> = {
      let mut duplicate_generic_names = HashSet::new();
      used_generic_names
        .into_iter()
        .filter_map(|name| {
          if duplicate_generic_names.contains(&name) {
            None
          } else {
            duplicate_generic_names.insert(name.clone());
            Some(name)
          }
        })
        .collect()
    };
    let ordered_type_constraints: Vec<Vec<TypeConstraint>> = generic_name_order
      .iter()
      .map(|name| {
        self
          .generic_args
          .iter()
          .find_map(|(generic_name, constraints)| {
            (generic_name == name).then(|| constraints.clone())
          })
          .unwrap()
      })
      .collect();
    let rename_generics = |mut t: AbstractType| {
      for (i, name) in generic_name_order.iter().enumerate() {
        t = t.rename_generic(name, &format!("{i}"));
      }
      t
    };
    (
      ordered_type_constraints,
      self
        .arg_types
        .iter()
        .map(|t| rename_generics(t.clone()))
        .collect(),
      rename_generics(self.return_type.clone()),
    )
  }
  pub fn effects(&self, program: &Program) -> EffectType {
    if let FunctionImplementationKind::Composite(f) = &self.implementation {
      let f = f.borrow();
      if let ExpKind::Function(arg_names, body) = &f.body.kind {
        let mut effects = body.effects(&program);
        for name in arg_names {
          effects.remove(&Effect::ReadsVar(name.clone()))
        }
        return effects;
      }
    }
    EffectType::empty()
  }
  pub fn arg_names(
    &self,
    source_trace: &SourceTrace,
  ) -> CompileResult<Vec<Rc<str>>> {
    if let FunctionImplementationKind::Composite(f) = &self.implementation {
      Ok(f.borrow().arg_names.clone())
    } else {
      err(NoArgNamesForFunction, source_trace.clone())
    }
  }
  pub fn arg_annotations(
    &self,
    source_trace: SourceTrace,
  ) -> CompileResult<Vec<Option<Annotation>>> {
    if let FunctionImplementationKind::Composite(f) = &self.implementation {
      Ok(f.borrow().arg_annotations.clone())
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
    let generic_arg_names = self
      .generic_args
      .iter()
      .map(|(arg, bounds)| {
        let generic_type = generic_bindings.get(arg).unwrap();
        if let Some(unsatisfied_bound) = bounds
          .iter()
          .find(|constraint| !generic_type.satisfies_constraints(constraint))
        {
          err(
            UnsatisfiedTypeConstraint(unsatisfied_bound.clone().into()),
            source_trace.clone(),
          )
        } else {
          Ok(
            generic_type
              .monomorphized_name(&mut new_program.names.borrow_mut())
              .into(),
          )
        }
      })
      .collect::<CompileResult<Vec<Rc<str>>>>()?;
    monomorphized.name = new_program
      .names
      .borrow_mut()
      .get_monomorphized_name(self.name.clone(), generic_arg_names);
    monomorphized.generic_args = vec![];
    for t in monomorphized
      .arg_types
      .iter_mut()
      .chain(std::iter::once(&mut monomorphized.return_type))
    {
      take(t, |t| {
        t.fill_abstract_generics(
          &generic_bindings
            .iter()
            .map(|(a, b)| (a.clone(), AbstractType::Type(b.clone())))
            .collect::<HashMap<_, _>>(),
        )
      })
    }
    if let FunctionImplementationKind::Composite(monomorphized_fn) =
      &mut monomorphized.implementation
    {
      let mut new_fn = monomorphized_fn.borrow().clone();
      let replacement_pairs: HashMap<Rc<str>, Type> = generic_bindings
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
    name: &Rc<str>,
    fn_args: Vec<TypedExp>,
    names: &mut NameContext,
    source_trace: SourceTrace,
  ) -> CompileResult<Self> {
    let mut implementation = self.implementation(source_trace.clone())?;
    let (fn_parameter_names, remaining_parameters) = self
      .arg_types
      .iter()
      .cloned()
      .zip(implementation.arg_names.into_iter())
      .zip(implementation.arg_annotations.into_iter())
      .map(|((t, name), annotation)| {
        if let AbstractType::Type(Type::Function(_)) = t {
          (Some(name.clone()), None)
        } else {
          (None, Some((name.clone(), (annotation.clone(), t))))
        }
      })
      .collect::<(
        Vec<Option<Rc<str>>>,
        Vec<Option<(Rc<str>, (Option<Annotation>, AbstractType))>>,
      )>();
    let (new_parameter_names, (new_parameter_annotation, new_parameter_types)): (
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
          signature.args.remove(i);
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
    implementation.arg_annotations = new_parameter_annotation;
    let f = AbstractFunctionSignature {
      name: names.get_monomorphized_name(
        name.clone(),
        fn_args
          .iter()
          .map(|arg| {
            if let ExpKind::Name(higher_order_arg_name) = &arg.kind {
              higher_order_arg_name.clone()
            } else {
              panic!("element of fn_args wasn't a Name")
            }
          })
          .collect::<Vec<Rc<str>>>(),
      ),
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
    typedefs: &TypeDefs,
    source_trace: SourceTrace,
  ) -> CompileResult<FunctionSignature> {
    let f = f.borrow();
    let (generic_variables, generic_constraints): (
      HashMap<Rc<str>, ExpTypeInfo>,
      HashMap<Rc<str>, Vec<TypeConstraint>>,
    ) = f
      .generic_args
      .iter()
      .map(|(name, bounds)| {
        (
          (name.clone(), TypeState::fresh_unification_variable().into()),
          (name.clone(), bounds.clone()),
        )
      })
      .collect();
    let mut args: Vec<_> = f
      .arg_types
      .iter()
      .map(|t| {
        let (var_type, constraints) = match t {
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
          AbstractType::Type(t) => (t.clone().known().into(), vec![]),
          AbstractType::AbstractStruct(s) => (
            Type::Struct(AbstractStruct::fill_generics(
              s.clone(),
              &generic_variables,
              typedefs,
              source_trace.clone(),
            )?)
            .known()
            .into(),
            vec![],
          ),
          AbstractType::AbstractEnum(e) => (
            Type::Enum(AbstractEnum::fill_generics(
              e.clone(),
              &generic_variables,
              typedefs,
              source_trace.clone(),
            )?)
            .known()
            .into(),
            vec![],
          ),
          AbstractType::AbstractArray {
            size, inner_type, ..
          } => (
            Type::Array(
              Some(size.clone()),
              inner_type
                .fill_generics(
                  &generic_variables,
                  typedefs,
                  source_trace.clone(),
                )?
                .into(),
            )
            .known()
            .into(),
            vec![],
          ),
          AbstractType::Reference(abstract_type) => (
            Type::Reference(
              abstract_type
                .fill_generics(
                  &generic_variables,
                  typedefs,
                  source_trace.clone(),
                )?
                .into(),
            )
            .known()
            .into(),
            vec![],
          ),
        };
        Ok((Variable::immutable(var_type), constraints))
      })
      .collect::<CompileResult<_>>()?;
    let mut return_type = match &f.return_type {
      AbstractType::Generic(var_name) => generic_variables
        .get(var_name)
        .expect("unrecognized generic")
        .clone(),
      AbstractType::AbstractStruct(s) => {
        Type::Struct(AbstractStruct::fill_generics(
          s.clone(),
          &generic_variables,
          typedefs,
          source_trace,
        )?)
        .known()
        .into()
      }
      AbstractType::AbstractEnum(e) => Type::Enum(AbstractEnum::fill_generics(
        e.clone(),
        &generic_variables,
        typedefs,
        source_trace,
      )?)
      .known()
      .into(),
      AbstractType::Type(t) => t.clone().known().into(),
      AbstractType::AbstractArray {
        size, inner_type, ..
      } => Type::Array(
        Some(size.clone()),
        inner_type
          .fill_generics(&generic_variables, typedefs, source_trace)?
          .into(),
      )
      .known()
      .into(),
      AbstractType::Reference(inner_type) => Type::Reference(
        inner_type
          .fill_generics(&generic_variables, typedefs, source_trace)?
          .into(),
      )
      .known()
      .into(),
    };
    for (v, _) in args.iter_mut() {
      v.var_type
        .kind
        .replace_skolems_with_unification_variables(&generic_variables);
    }
    return_type
      .kind
      .replace_skolems_with_unification_variables(&generic_variables);
    Ok(FunctionSignature {
      args,
      return_type,
      abstract_ancestor: Some(Rc::new(f.clone())),
      mutated_args: f.mutated_args.clone(),
    })
  }
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
  pub abstract_ancestor: Option<Rc<AbstractFunctionSignature>>,
  pub args: Vec<(Variable, Vec<TypeConstraint>)>,
  pub mutated_args: Vec<usize>,
  pub return_type: ExpTypeInfo,
}

impl PartialEq for FunctionSignature {
  fn eq(&self, other: &Self) -> bool {
    self.args == other.args
      && self.mutated_args == other.mutated_args
      && self.return_type == other.return_type
  }
}

impl FunctionSignature {
  pub fn compatible(&self, other: &Self) -> bool {
    if self.args.len() != other.args.len() {
      return false;
    }
    !self
      .args
      .iter()
      .zip(other.args.iter())
      .find(|((a_var, a_constraints), (b_var, b_constraints))| {
        !TypeState::are_compatible(&a_var.var_type, &b_var.var_type)
          || a_constraints != b_constraints
      })
      .is_some()
  }
  pub fn are_args_compatible(&self, arg_types: &Vec<TypeState>) -> bool {
    if arg_types.len() != self.args.len() {
      if let Some(ancestor) = &self.abstract_ancestor {
        if ancestor.associative {
          if arg_types.len() == 0 {
            return false;
          }
          let (arg, arg_constraints) = &self.args[0];
          for arg_typestate in arg_types {
            if !TypeState::are_compatible(arg_typestate, &arg.var_type.kind) {
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
      let (arg, arg_constraints) = &self.args[i];
      if !TypeState::are_compatible(&arg.var_type, &arg_types[i]) {
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
    mut args: Vec<&mut TypeState>,
    source_trace: SourceTrace,
    errors: &mut ErrorLog,
  ) -> bool {
    if args.len() == self.args.len() {
      let mut any_arg_changed = false;
      for i in 0..args.len() {
        let changed = args[i].mutually_constrain(
          &mut self.args[i].0.var_type,
          &source_trace,
          errors,
        );
        any_arg_changed |= changed;
      }
      any_arg_changed
    } else {
      if let Some(ancestor) = &self.abstract_ancestor {
        if ancestor.associative {
          if args.len() != 0 {
            let arg_type = &mut self.args.get_mut(0).unwrap().0.var_type;
            let mut any_arg_changed = false;
            for i in 0..args.len() {
              let changed =
                args[i].mutually_constrain(arg_type, &source_trace, errors);
              any_arg_changed |= changed;
            }
            return any_arg_changed;
          }
        }
      }
      errors.log(CompileError::new(
        WrongArity(self.name().map(|n| n.to_string())),
        source_trace,
      ));
      false
    }
  }
  pub fn name(&self) -> Option<Rc<str>> {
    self
      .abstract_ancestor
      .as_ref()
      .map(|abstract_ancestor| abstract_ancestor.name.clone())
  }
  pub fn effects(&self, program: &Program) -> EffectType {
    if let Some(abstract_ancestor) = &self.abstract_ancestor {
      match &abstract_ancestor.implementation {
        FunctionImplementationKind::Composite(f) => {
          if let ExpKind::Function(arg_names, body) = &f.borrow().body.kind {
            let mut effects = body.effects(&program);
            for name in arg_names {
              effects.remove(&Effect::ReadsVar(name.clone()))
            }
            return effects;
          };
        }
        _ => {}
      }
    }
    EffectType::empty()
  }
  pub fn unwrap_type_signature(&self) -> Vec<Type> {
    self
      .args
      .iter()
      .map(|a| &a.0.var_type)
      .chain(std::iter::once(&self.return_type))
      .map(|t| t.unwrap_known())
      .collect::<Vec<Type>>()
  }
}

pub struct BuiltInFunction {
  pub name: Rc<str>,
  pub signature: AbstractFunctionSignature,
}

impl TopLevelFunction {
  pub fn compile(
    self,
    name: &str,
    names: &mut NameContext,
  ) -> CompileResult<String> {
    let TypedExp { data, kind, .. } = self.body;
    let (args, return_type) =
      if let Type::Function(signature) = data.unwrap_known() {
        (signature.args, signature.return_type)
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
      .zip(args.into_iter())
      .zip(self.arg_annotations.into_iter())
      .map(|((name, (arg, _)), annotation)| {
        format!(
          "{}{}: {}",
          Annotation::compile_optional(annotation),
          compile_word(name),
          arg.var_type.monomorphized_name(names)
        )
      })
      .collect::<Vec<String>>()
      .join(", ");

    Ok(format!(
      "{}fn {}({args}){} {{{}\n}}",
      Annotation::compile_optional(self.annotation),
      compile_word(name.into()),
      if return_type.kind.unwrap_known() == Type::Unit {
        "".to_string()
      } else {
        format!(
          " -> {}{}",
          Annotation::compile_optional(self.return_annotation),
          return_type.monomorphized_name(names)
        )
      },
      indent(body.compile(
        if return_type.kind.unwrap_known() == Type::Unit {
          ExpressionCompilationPosition::InnerLine
        } else {
          ExpressionCompilationPosition::Return
        },
        names
      ))
    ))
  }
}
