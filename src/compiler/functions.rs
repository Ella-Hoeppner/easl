use std::{
  cell::RefCell,
  collections::{HashMap, HashSet},
  rc::Rc,
};

use take_mut::take;

use crate::{
  compiler::{
    annotation::FunctionAnnotation,
    builtins::vec3,
    effects::{Effect, EffectType},
    enums::AbstractEnum,
    expression::arg_list_and_return_type_from_easl_tree,
    program::{NameContext, TypeDefs},
    types::{Variable, VariableKind, parse_generic_argument},
    util::compile_word,
  },
  parse::EaslTree,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryPoint {
  Vertex,
  Fragment,
  Compute(usize),
}
impl EntryPoint {
  fn compile(&self) -> String {
    match self {
      EntryPoint::Vertex => "@vertex\n".to_string(),
      EntryPoint::Fragment => "@fragment\n".to_string(),
      EntryPoint::Compute(size) => {
        format!("@compute\n@workgroup_size({size})\n")
      }
    }
  }
  pub fn name(&self) -> &'static str {
    match self {
      EntryPoint::Vertex => "vertex",
      EntryPoint::Fragment => "fragment",
      EntryPoint::Compute(_) => "compute",
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinArgumentAnnotation {
  VertexIndex,
  InstanceIndex,
  FrontFacing,
  SampleIndex,
  LocalInvocationId,
  LocalInvocationIndex,
  GlobalInvocationId,
}

impl BuiltinArgumentAnnotation {
  pub fn arg_type(&self) -> AbstractType {
    use BuiltinArgumentAnnotation::*;
    match self {
      VertexIndex | InstanceIndex | LocalInvocationIndex | SampleIndex => {
        AbstractType::Type(Type::U32)
      }
      FrontFacing => AbstractType::Type(Type::Bool),
      LocalInvocationId | GlobalInvocationId => AbstractType::AbstractStruct(
        vec3()
          .fill_abstract_generics(vec![AbstractType::Type(Type::U32)])
          .into(),
      ),
    }
  }
  pub fn from_name(name: &str) -> Option<Self> {
    use BuiltinArgumentAnnotation::*;
    Some(match name {
      "vertex-index" => VertexIndex,
      "instance-index" => InstanceIndex,
      "front-facing" => FrontFacing,
      "sample-index" => SampleIndex,
      "local-invocation-id" => LocalInvocationId,
      "local-invocation-index" => LocalInvocationIndex,
      "global-invocation-index" => GlobalInvocationId,
      _ => return None,
    })
  }
  pub fn name(&self) -> &'static str {
    use BuiltinArgumentAnnotation::*;
    match self {
      VertexIndex => "vertex-index",
      InstanceIndex => "instance-index",
      FrontFacing => "front-facing",
      SampleIndex => "sample-index",
      LocalInvocationId => "local-invocation-id",
      LocalInvocationIndex => "local-invocation-index",
      GlobalInvocationId => "global-invocation-index",
    }
  }
  pub fn allowed_for_entry(&self, entry: EntryPoint) -> bool {
    use BuiltinArgumentAnnotation::*;
    match (self, entry) {
      (VertexIndex | InstanceIndex, EntryPoint::Vertex) => true,
      (FrontFacing | SampleIndex, EntryPoint::Fragment) => true,
      (
        LocalInvocationId | LocalInvocationIndex | GlobalInvocationId,
        EntryPoint::Compute(_),
      ) => true,
      _ => false,
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArgumentAnnotation {
  pub var: bool,
  pub builtin: Option<BuiltinArgumentAnnotation>,
}

impl FunctionArgumentAnnotation {
  fn compile(&self) -> String {
    use BuiltinArgumentAnnotation::*;
    if let Some(builtin) = &self.builtin {
      format!(
        "@builtin({}) ",
        match builtin {
          VertexIndex => "vertex_index",
          InstanceIndex => "instance_index",
          FrontFacing => "front_facing",
          SampleIndex => "sample_index",
          LocalInvocationId => "local_invocation_id",
          LocalInvocationIndex => "local_invocation_index",
          GlobalInvocationId => "global_invocation_id",
        }
      )
    } else {
      String::new()
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopLevelFunction {
  pub arg_names: Vec<Rc<str>>,
  pub arg_annotations: Vec<Option<FunctionArgumentAnnotation>>,
  pub return_location: Option<usize>,
  pub entry_point: Option<EntryPoint>,
  pub body: TypedExp,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionImplementationKind {
  Builtin(EffectType),
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
  pub(crate) fn from_defn_ast(
    mut children_iter: impl Iterator<Item = EaslTree>,
    first_child_source_trace: SourceTrace,
    parens_source_trace: SourceTrace,
    annotation: Option<(Annotation, SourceTrace)>,
    program: &Program,
    errors: &mut ErrorLog,
  ) -> Option<Self> {
    use crate::parse::Encloser::*;
    use sse::syntax::EncloserOrOperator::*;
    let Some(name_ast) = children_iter.next() else {
      errors.log(CompileError::new(
        InvalidDefn("Missing Name".into()),
        parens_source_trace.clone(),
      ));
      return None;
    };
    let fn_and_generic_names: Option<(Rc<str>, Vec<_>)> = match name_ast {
      EaslTree::Leaf(_, name) => Some((name.into(), vec![])),
      EaslTree::Inner((_, Encloser(Parens)), subtrees) => {
        let mut subtrees_iter = subtrees.into_iter();
        if let Some(EaslTree::Leaf(_, name)) = subtrees_iter.next() {
          match subtrees_iter
            .map(|subtree| {
              parse_generic_argument(subtree, &program.typedefs, &vec![])
            })
            .collect::<CompileResult<Vec<_>>>()
          {
            Ok(generic_args) => Some((name.into(), generic_args)),
            Err(e) => {
              errors.log(e);
              None
            }
          }
        } else {
          errors.log(CompileError::new(
            InvalidDefn("Invalid name".into()),
            first_child_source_trace,
          ));
          None
        }
      }
      _ => {
        errors.log(CompileError::new(
          InvalidDefn(
            "Expected name or parens with name and generic arguments".into(),
          ),
          first_child_source_trace,
        ));
        None
      }
    };
    let Some((fn_name, generic_args)) = fn_and_generic_names else {
      return None;
    };
    let Some(arg_list_ast) = children_iter.next() else {
      errors.log(CompileError::new(
        InvalidDefn("Missing Argument List".into()),
        parens_source_trace.clone(),
      ));
      return None;
    };
    let generic_arg_names: Vec<Rc<str>> =
      generic_args.iter().map(|(name, _)| name.clone()).collect();
    match arg_list_and_return_type_from_easl_tree(
      arg_list_ast,
      &program.typedefs,
      &mut program.names.borrow_mut(),
      &generic_arg_names,
    ) {
      Ok((
        source_path,
        arg_names,
        arg_types,
        arg_annotations,
        return_type,
        return_annotation,
      )) => {
        match return_type.concretize(
          &generic_arg_names,
          &program.typedefs,
          source_path.clone().into(),
        ) {
          Ok(concrete_return_type) => {
            match arg_types
              .iter()
              .zip(arg_annotations.iter())
              .map(|(t, annotation)| {
                Ok((
                  Variable {
                    var_type: t
                      .concretize(
                        &generic_arg_names,
                        &program.typedefs,
                        source_path.clone().into(),
                      )?
                      .known()
                      .into(),
                    kind: if let Some(annotation) = annotation
                      && annotation.var
                    {
                      VariableKind::Var
                    } else {
                      VariableKind::Let
                    },
                  },
                  if let AbstractType::Generic(generic_name) = t {
                    generic_args
                      .iter()
                      .find_map(|(name, constraints)| {
                        (generic_name == name).then(|| constraints.clone())
                      })
                      .unwrap_or(vec![])
                  } else {
                    vec![]
                  },
                ))
              })
              .collect::<CompileResult<Vec<(Variable, Vec<TypeConstraint>)>>>()
            {
              Ok(concrete_args) => {
                match TypedExp::function_from_body_tree(
                  source_path.clone(),
                  children_iter.collect(),
                  concrete_return_type.known().into(),
                  arg_names.clone(),
                  concrete_args,
                  &program.typedefs,
                  &generic_arg_names,
                ) {
                  Ok(body) => {
                    let parsed_annotation =
                      if let Some((annotation, annotation_source_trace)) =
                        &annotation
                      {
                        match annotation.validate_as_function_annotation(
                          annotation_source_trace,
                        ) {
                          Ok(is_associative) => is_associative,
                          Err(e) => {
                            errors.log(e);
                            FunctionAnnotation::default()
                          }
                        }
                      } else {
                        FunctionAnnotation::default()
                      };
                    let all_arg_annotations: Vec<FunctionArgumentAnnotation> =
                      arg_annotations
                        .iter()
                        .cloned()
                        .filter_map(|x| x)
                        .collect();
                    for annotation in all_arg_annotations.iter() {
                      if let Some(builtin) = &annotation.builtin {
                        if let Some(entry) = parsed_annotation.entry {
                          if !builtin.allowed_for_entry(entry) {
                            errors.log(CompileError {
                              kind: BuiltinArgumentsOnWrongEntry(
                                builtin.name().to_string(),
                                entry.name().to_string(),
                              ),
                              source_trace: source_path.clone(),
                            });
                          }
                        } else {
                          errors.log(CompileError {
                            kind: BuiltinArgumentsOnlyAllowedOnEntry,
                            source_trace: source_path.clone(),
                          });
                        }
                      }
                    }
                    let all_builtins: Vec<BuiltinArgumentAnnotation> =
                      all_arg_annotations
                        .iter()
                        .filter_map(|a| a.builtin.clone())
                        .collect();
                    if all_builtins.iter().collect::<HashSet<_>>().len()
                      < all_builtins.len()
                    {
                      errors.log(CompileError {
                        kind: DuplicateBuiltinArgument,
                        source_trace: source_path.clone(),
                      });
                    }
                    let mut return_location = None;
                    if let Some(return_annotation) = return_annotation {
                      for (name, value) in return_annotation.properties() {
                        match (&*name, value) {
                          ("location", Some(value)) => {
                            match value.parse::<usize>() {
                              Ok(location) => {
                                return_location = Some(location);
                                if parsed_annotation.entry
                                  != Some(EntryPoint::Fragment)
                                {
                                  errors.log(CompileError {
                                    kind: InvalidReturnTypeAnnotation,
                                    source_trace: source_path.clone(),
                                  });
                                }
                              }
                              Err(_) => errors.log(CompileError {
                                kind: InvalidReturnTypeAnnotation,
                                source_trace: source_path.clone(),
                              }),
                            }
                          }
                          _ => errors.log(CompileError {
                            kind: InvalidReturnTypeAnnotation,
                            source_trace: source_path.clone(),
                          }),
                        }
                      }
                    }
                    if return_location.is_none()
                      && parsed_annotation.entry == Some(EntryPoint::Fragment)
                    {
                      return_location = Some(0);
                    }
                    let implementation = FunctionImplementationKind::Composite(
                      Rc::new(RefCell::new(TopLevelFunction {
                        arg_names,
                        arg_annotations,
                        return_location,
                        entry_point: parsed_annotation.entry,
                        body,
                      })),
                    );
                    return Some(AbstractFunctionSignature {
                      name: fn_name,
                      generic_args,
                      arg_types,
                      mutated_args: vec![],
                      return_type,
                      implementation,
                      associative: parsed_annotation.associative,
                    });
                  }
                  Err(e) => errors.log(e),
                }
              }
              Err(e) => {
                errors.log(e);
              }
            }
          }
          Err(e) => {
            errors.log(e);
          }
        }
      }
      Err(e) => errors.log(e),
    }
    None
  }
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
  ) -> CompileResult<Vec<Option<FunctionArgumentAnnotation>>> {
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
        Vec<
          Option<(Rc<str>, (Option<FunctionArgumentAnnotation>, AbstractType))>,
        >,
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
          AbstractType::Unit => (TypeState::Known(Type::Unit).into(), vec![]),
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
      AbstractType::Unit => TypeState::Known(Type::Unit).into(),
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
        FunctionImplementationKind::Builtin(effects) => return effects.clone(),
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
          annotation
            .map(|annotation| annotation.compile())
            .unwrap_or_else(|| String::new()),
          compile_word(name),
          arg.var_type.monomorphized_name(names)
        )
      })
      .collect::<Vec<String>>()
      .join(", ");

    Ok(format!(
      "{}fn {}({args}){} {{{}\n}}",
      self
        .entry_point
        .as_ref()
        .map(|e| e.compile())
        .unwrap_or(String::new()),
      compile_word(name.into()),
      if return_type.kind.unwrap_known() == Type::Unit {
        "".to_string()
      } else {
        format!(
          " -> {}{}",
          self
            .return_location
            .map(|location| format!("@location({location}) "))
            .unwrap_or_default(),
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
