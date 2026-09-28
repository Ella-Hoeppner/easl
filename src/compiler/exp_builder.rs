//! Constructing fully-typed expressions for compiler-generated code.
//!
//! Passes that synthesize code after type inference (dispatchers, payload
//! helpers, copy-backs, zero values) build `TypedExp` trees directly, with
//! every node's type spelled out. `ExpBuilder` carries the source trace every
//! generated node shares and derives what it can — a call's callee type from
//! the called signature, a `let`'s type from its body — so construction reads
//! like the code being generated.

use std::sync::{Arc, RwLock};

use crate::compiler::{
  effects::{Effect, EffectType},
  entry::IOAttributes,
  error::SourceTrace,
  expression::{Accessor, Exp, ExpKind, Number, TypedExp},
  functions::{
    AbstractFunctionSignature, FunctionArgumentAnnotation,
    FunctionImplementationKind, FunctionSignature, FunctionTargetConfiguration,
    Ownership, TopLevelFunction,
  },
  types::{AbstractType, ExpTypeInfo, Type, TypeState, Variable, VariableKind},
};

/// Builds typed expressions that all share one source trace.
pub(crate) struct ExpBuilder {
  source: SourceTrace,
}

impl ExpBuilder {
  pub(crate) fn at(source: &SourceTrace) -> Self {
    Self {
      source: source.clone(),
    }
  }
  /// A node of `kind` with known type `t`.
  pub(crate) fn typed(&self, kind: ExpKind<ExpTypeInfo>, t: Type) -> TypedExp {
    let mut data: ExpTypeInfo = TypeState::Known(t).into();
    data.subtree_fully_typed = true;
    Exp {
      data,
      kind,
      source_trace: self.source.clone(),
    }
  }
  /// A node of `kind` keeping existing type information `data` (ownership
  /// and binding flags included) — for a node standing in for, or wrapping,
  /// an existing expression.
  pub(crate) fn with_data(
    &self,
    kind: ExpKind<ExpTypeInfo>,
    data: ExpTypeInfo,
  ) -> TypedExp {
    Exp {
      data,
      kind,
      source_trace: self.source.clone(),
    }
  }
  pub(crate) fn unit(&self) -> TypedExp {
    self.typed(ExpKind::Unit, Type::Unit)
  }
  pub(crate) fn bool(&self, value: bool) -> TypedExp {
    self.typed(ExpKind::BooleanLiteral(value), Type::Bool)
  }
  pub(crate) fn u32(&self, value: u32) -> TypedExp {
    self.typed(ExpKind::NumberLiteral(Number::Int(value as i64)), Type::U32)
  }
  /// `[elements...]`, of array type `t`.
  pub(crate) fn array(&self, elements: Vec<TypedExp>, t: Type) -> TypedExp {
    self.typed(ExpKind::ArrayLiteral(elements), t)
  }
  /// `base.field`, of type `t`.
  pub(crate) fn field(&self, base: TypedExp, field: &str, t: Type) -> TypedExp {
    self.typed(
      ExpKind::Access(Accessor::Field(field.into()), Box::new(base)),
      t,
    )
  }
  /// `(array index)`: an element of type `t`.
  pub(crate) fn index(
    &self,
    array: TypedExp,
    index: TypedExp,
    t: Type,
  ) -> TypedExp {
    self.apply(array, vec![index], &t)
  }
  /// `(match condition true () _ (break))`: leaves the enclosing loop unless
  /// `condition` holds.
  pub(crate) fn break_unless(&self, condition: TypedExp) -> TypedExp {
    self.match_on(
      condition,
      vec![
        (self.bool(true), self.unit()),
        (
          self.wildcard(&Type::Bool),
          self.typed(ExpKind::Break, Type::Unit),
        ),
      ],
      &Type::Unit,
    )
  }
  /// A `_` pattern matching values of type `t`.
  pub(crate) fn wildcard(&self, t: &Type) -> TypedExp {
    self.typed(ExpKind::Wildcard, t.clone())
  }
  /// A reference to the variable `name`, of type `t`.
  pub(crate) fn name(&self, name: &Arc<str>, t: &Type) -> TypedExp {
    self.typed(ExpKind::Name(name.clone()), t.clone())
  }
  /// A reference to the variable `name` with the given ownership (a
  /// reference parameter, say).
  pub(crate) fn name_with(
    &self,
    name: &Arc<str>,
    t: &Type,
    ownership: Ownership,
  ) -> TypedExp {
    let mut exp = self.name(name, t);
    exp.data.ownership = ownership;
    exp
  }
  /// `(callee args...)`, of type `ret`.
  pub(crate) fn apply(
    &self,
    callee: TypedExp,
    args: Vec<TypedExp>,
    ret: &Type,
  ) -> TypedExp {
    self.typed(ExpKind::Application(Box::new(callee), args), ret.clone())
  }
  /// A reference to `signature`'s function, typed for parameters `params`
  /// and return `ret`.
  pub(crate) fn callee(
    &self,
    signature: &Arc<RwLock<AbstractFunctionSignature>>,
    params: &[Type],
    ret: &Type,
  ) -> TypedExp {
    let name = signature.read().unwrap().name.clone();
    self.typed(ExpKind::Name(name), function_type(signature, params, ret))
  }
  /// A call of `signature`'s function, its parameter types taken from the
  /// arguments'.
  pub(crate) fn call(
    &self,
    signature: &Arc<RwLock<AbstractFunctionSignature>>,
    args: Vec<TypedExp>,
    ret: &Type,
  ) -> TypedExp {
    let params: Vec<Type> =
      args.iter().map(|a| a.data.kind.unwrap_known()).collect();
    self.apply(self.callee(signature, &params, ret), args, ret)
  }
  /// `(= target value)`: `target` is passed by mutable reference.
  pub(crate) fn assign(&self, target: TypedExp, value: TypedExp) -> TypedExp {
    let mut target = target;
    target.data.ownership = Ownership::MutableReference;
    let t = target.data.kind.unwrap_known();
    let assignment = TypedExp::assignment_function(TypeState::Known(t).into());
    self.apply(assignment, vec![target, value], &Type::Unit)
  }
  /// `(let [bindings...] body)`, of `body`'s type.
  pub(crate) fn let_in(
    &self,
    bindings: Vec<(Arc<str>, VariableKind, TypedExp)>,
    body: TypedExp,
  ) -> TypedExp {
    let t = body.data.kind.unwrap_known();
    let bindings = bindings
      .into_iter()
      .map(|(name, kind, value)| (name, self.source.clone(), kind, value))
      .collect();
    self.typed(ExpKind::Let(bindings, Box::new(body)), t)
  }
  /// `(do exps...)`, of the last expression's type.
  pub(crate) fn block(&self, exps: Vec<TypedExp>) -> TypedExp {
    let t = exps
      .last()
      .map(|e| e.data.kind.unwrap_known())
      .unwrap_or(Type::Unit);
    self.typed(ExpKind::Block(exps), t)
  }
  /// `(match scrutinee arms...)`, of type `t`.
  pub(crate) fn match_on(
    &self,
    scrutinee: TypedExp,
    arms: Vec<(TypedExp, TypedExp)>,
    t: &Type,
  ) -> TypedExp {
    self.typed(ExpKind::Match(Box::new(scrutinee), arms), t.clone())
  }
  /// Evaluates `value`, then `after`, yielding `value`'s result — held in
  /// `result_name` unless it's unit.
  pub(crate) fn then_run(
    &self,
    value: TypedExp,
    result_name: Arc<str>,
    after: Vec<TypedExp>,
  ) -> TypedExp {
    let t = value.data.kind.unwrap_known();
    if t == Type::Unit {
      let mut exps = vec![value];
      exps.extend(after);
      return self.block(exps);
    }
    let mut exps = after;
    exps.push(self.name(&result_name, &t));
    self.let_in(
      vec![(result_name, VariableKind::Let, value)],
      self.block(exps),
    )
  }
}

/// `(let [bindings...] body)` standing in for `body`: keeps `body`'s type
/// information and source trace. Each binding carries its own trace.
pub(crate) fn let_around(
  bindings: Vec<(Arc<str>, SourceTrace, VariableKind, TypedExp)>,
  body: TypedExp,
) -> TypedExp {
  Exp {
    data: body.data.clone(),
    source_trace: body.source_trace.clone(),
    kind: ExpKind::Let(bindings, Box::new(body)),
  }
}

/// A parameter or signature variable of type `t`.
fn variable(t: Type, ownership: Ownership, kind: VariableKind) -> Variable {
  let mut var_type: ExpTypeInfo = TypeState::Known(t).into();
  var_type.ownership = ownership;
  Variable { kind, var_type }
}

/// The function type of a reference to `signature`'s function, for
/// parameters `params` (ownerships from the signature) and return `ret`.
pub(crate) fn function_type(
  signature: &Arc<RwLock<AbstractFunctionSignature>>,
  params: &[Type],
  ret: &Type,
) -> Type {
  let s = signature.read().unwrap();
  Type::Function(Box::new(FunctionSignature {
    abstract_ancestor: Some(signature.clone()),
    args: params
      .iter()
      .enumerate()
      .map(|(i, t)| {
        let ownership = s
          .arg_types
          .get(i)
          .map(|(_, o)| *o)
          .unwrap_or(Ownership::Owned);
        let kind = if ownership == Ownership::MutableReference {
          VariableKind::Var
        } else {
          VariableKind::Let
        };
        (variable(t.clone(), ownership, kind), vec![])
      })
      .collect(),
    return_type: TypeState::Known(ret.clone()).into(),
  }))
}

/// The signature of a builtin taking `arg_types` and returning `return_type`.
pub(crate) fn builtin_signature(
  name: &str,
  arg_types: Vec<(Type, Ownership)>,
  return_type: Type,
  effect: Option<Effect>,
) -> Arc<RwLock<AbstractFunctionSignature>> {
  Arc::new(RwLock::new(AbstractFunctionSignature {
    name: name.into(),
    arg_types: arg_types
      .into_iter()
      .map(|(t, o)| (AbstractType::Type(t), o))
      .collect(),
    return_type: AbstractType::Type(return_type),
    implementation: FunctionImplementationKind::Builtin {
      effect_type: effect.map(|e| e.into()).unwrap_or_else(EffectType::empty),
      target_configuration: FunctionTargetConfiguration::Default,
      target_specific_emulations: Default::default(),
    },
    ..Default::default()
  }))
}

/// The constructor of struct `name`, whose fields have types `fields`.
pub(crate) fn struct_constructor(
  name: &Arc<str>,
  fields: &[Type],
  struct_type: &Type,
) -> Arc<RwLock<AbstractFunctionSignature>> {
  Arc::new(RwLock::new(AbstractFunctionSignature {
    name: name.clone(),
    arg_types: fields
      .iter()
      .map(|t| (AbstractType::Type(t.clone()), Ownership::Owned))
      .collect(),
    return_type: AbstractType::Type(struct_type.clone()),
    implementation: FunctionImplementationKind::StructConstructor,
    ..Default::default()
  }))
}

/// The constructor of enum variant `variant`, which carries a `payload`.
pub(crate) fn enum_constructor(
  variant: &Arc<str>,
  payload: &Type,
  enum_type: AbstractType,
) -> Arc<RwLock<AbstractFunctionSignature>> {
  Arc::new(RwLock::new(AbstractFunctionSignature {
    name: variant.clone(),
    arg_types: vec![(AbstractType::Type(payload.clone()), Ownership::Owned)],
    return_type: enum_type,
    implementation: FunctionImplementationKind::EnumConstructor(
      variant.clone(),
    ),
    ..Default::default()
  }))
}

/// A compiler-generated composite function `name` taking `params` (name,
/// type, ownership) and returning `ret`, with body `body`.
pub(crate) fn generated_function(
  name: &Arc<str>,
  params: &[(Arc<str>, Type, Ownership)],
  ret: &Type,
  body: TypedExp,
  source: &SourceTrace,
) -> Arc<RwLock<AbstractFunctionSignature>> {
  let arg_names: Vec<(Arc<str>, SourceTrace)> = params
    .iter()
    .map(|(n, _, _)| (n.clone(), source.clone()))
    .collect();
  let implementation = Arc::new(RwLock::new(TopLevelFunction {
    name_source_trace: source.clone(),
    arg_names: arg_names.clone(),
    arg_annotations: params
      .iter()
      .map(|(_, _, ownership)| {
        let mut a = FunctionArgumentAnnotation::empty(source.clone());
        if *ownership == Ownership::MutableReference {
          a.var = true;
          a.ownership = Ownership::MutableReference;
        }
        a
      })
      .collect(),
    return_attributes: IOAttributes::empty(source.clone()),
    entry_point: None,
    directly_user_written: false,
    expression: ExpBuilder::at(source).unit(),
  }));
  let signature = Arc::new(RwLock::new(AbstractFunctionSignature {
    name: name.clone(),
    arg_types: params
      .iter()
      .map(|(_, t, o)| (AbstractType::Type(t.clone()), *o))
      .collect(),
    return_type: AbstractType::Type(ret.clone()),
    implementation: FunctionImplementationKind::Composite(
      implementation.clone(),
    ),
    ..Default::default()
  }));
  let param_types: Vec<Type> =
    params.iter().map(|(_, t, _)| t.clone()).collect();
  implementation.write().unwrap().expression = ExpBuilder::at(source).typed(
    ExpKind::Function(arg_names, Box::new(body)),
    function_type(&signature, &param_types, ret),
  );
  signature
}
