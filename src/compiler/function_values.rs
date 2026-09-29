//! First-class function values: functions and closures stored in arrays,
//! struct fields, enum payloads, and variables, or chosen at runtime by an
//! `if`/`match`.
//!
//! Easl's higher-order functions are otherwise fully static: every
//! function-typed value carries exactly one statically-known ancestor, and
//! higher-order arguments are inlined away. A function value whose identity is
//! only known at runtime breaks that invariant, so it is handled in two passes
//! around the static machinery:
//!
//! 1. [`Program::box_function_values`] runs right after type inference (and
//!    `rewrite_aliased_builtin_calls`). It
//!    retypes every function value in a *dynamic* position — anything nested
//!    in an array / struct / enum / global, the result of an `if`/`match` that
//!    yields a function, anything read back out of those, and every `@var`
//!    holding a function — as [`Type::BoxedFunction`], an opaque value type.
//!    Static function values flowing into a dynamic position are wrapped in
//!    `$fnbox-make`, and applications of boxed values become `$fnbox-apply`
//!    calls (passing the boxed value by mutable reference, so a stateful
//!    closure advances *in place*, exactly like a directly-called closure).
//!    A function that itself takes or returns functions is stored through
//!    a clone with those positions boxed (a top-level function) or retyped
//!    in place (a lambda written where it's stored); a static closure passed
//!    to a boxed parameter is lent as `$fnbox-borrow`.
//!    From then on the deexpressionification / monomorphization / closure
//!    extraction / higher-order-inlining passes see boxed values as plain
//!    data, and their one-static-ancestor invariant holds for every remaining
//!    `Type::Function`.
//!
//! 2. [`Program::defunctionalize_boxed_functions`] runs after the
//!    extraction / inlining loop, when every member (top-level function or
//!    extracted closure) has an identity and a scope struct. A flow analysis
//!    computes, for every boxed position, the set of functions that can
//!    inhabit it, and each position is then lowered to its per-usage
//!    representation: nothing for a single scopeless function, the closure's
//!    own scope struct for a single closure, and a tagged-union enum (with a
//!    generated dispatcher) for two or more.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::sync::{Arc, RwLock};

use take_mut::take;

use crate::Never;
use crate::compiler::{
  builtins::{ASSIGNMENT_OPS, ATOMIC_MUTATION_OPS},
  effects::Effect,
  enums::{AbstractEnum, AbstractEnumVariant, Enum, EnumVariant},
  error::{CompileError, CompileErrorKind, ErrorLog, SourceTrace},
  exp_builder::{
    ExpBuilder, builtin_signature, enum_constructor, function_type,
    generated_function, let_around, struct_constructor,
  },
  expression::{Accessor, Exp, ExpKind, Number, TypedExp},
  functions::{
    AbstractFunctionSignature, FunctionImplementationKind, FunctionSignature,
    Ownership, TopLevelFunction,
  },
  program::{
    CompilerTarget, NameContext, Program, ref_arg_lvalue_path,
    ref_paths_provably_disjoint,
  },
  structs::{AbstractStruct, AbstractStructField, Struct, StructField},
  types::{
    AbstractType, ConcreteArraySize, ExpTypeInfo, GenericArgument, Type,
    TypeConstraint, TypeState, VariableKind,
  },
};

const FNBOX_MAKE: &str = "$fnbox-make";
const FNBOX_APPLY: &str = "$fnbox-apply";
const FNBOX_BORROW: &str = "$fnbox-borrow";

fn ptr_key<T: ?Sized>(arc: &Arc<T>) -> usize {
  Arc::as_ptr(arc) as *const () as usize
}

/// The implementation Arc's address: the stable identity of a composite
/// function (signature Arcs get copied by inference; implementation Arcs
/// are shared by every copy).
fn implementation_key(
  signature: &Arc<RwLock<AbstractFunctionSignature>>,
) -> Option<usize> {
  match &signature.read().unwrap().implementation {
    FunctionImplementationKind::Composite(implementation) => {
      Some(ptr_key(implementation))
    }
    _ => None,
  }
}

fn known_type(data: &ExpTypeInfo) -> Option<Type> {
  data.kind.try_unwrap_known()
}

// ===========================================================================
// Type predicates: each "does this type hold X" question is a small match on
// what X is, delegating the structure walk to `any_child` /
// `any_signature_type` / `abstract_type_holds`.
// ===========================================================================

/// Whether `f` holds for any type directly inside `t`: an array's element, a
/// struct's fields, or an enum's payloads. Types not yet known hold nothing.
fn any_child(t: &Type, f: &dyn Fn(&Type) -> bool) -> bool {
  let known = |ts: &TypeState| ts.try_unwrap_known().is_some_and(|t| f(&t));
  match t {
    Type::Array(_, inner) => known(&inner.kind),
    Type::Struct(s) => {
      s.fields.iter().any(|field| known(&field.field_type.kind))
    }
    Type::Enum(e) => e.variants.iter().any(|v| known(&v.inner_type.kind)),
    _ => false,
  }
}

/// Whether `f` holds for any of a function signature's parameter types or its
/// return type.
fn any_signature_type(
  sig: &FunctionSignature,
  f: &dyn Fn(&Type) -> bool,
) -> bool {
  sig
    .args
    .iter()
    .map(|(a, _)| &a.var_type.kind)
    .chain(std::iter::once(&sig.return_type.kind))
    .any(|ts| ts.try_unwrap_known().is_some_and(|t| f(&t)))
}

/// Whether `f` holds for any concrete type in an abstract type, looking
/// through abstract struct fields, enum payloads, and array elements.
fn abstract_type_holds(at: &AbstractType, f: &dyn Fn(&Type) -> bool) -> bool {
  match at {
    AbstractType::Type(t) => f(t),
    AbstractType::AbstractStruct(s) => s
      .fields
      .iter()
      .any(|field| abstract_type_holds(&field.field_type, f)),
    AbstractType::AbstractEnum(e) => e
      .variants
      .iter()
      .any(|v| abstract_type_holds(&v.inner_type, f)),
    AbstractType::AbstractArray { inner_type, .. } => {
      abstract_type_holds(inner_type, f)
    }
    AbstractType::Generic(_) | AbstractType::Unit => false,
  }
}

/// Whether values of type `t` hold a function value, static or boxed: a
/// function type or an aggregate containing one.
pub(crate) fn holds_function(t: &Type) -> bool {
  matches!(t, Type::Function(_) | Type::BoxedFunction(_))
    || any_child(t, &holds_function)
}

/// Whether `t` holds a function *inside* an aggregate (array element, struct
/// field, enum payload) — a position that is always boxed — at top level or
/// in a function type's parameters or return.
fn holds_nested_function(t: &Type) -> bool {
  match t {
    Type::Function(sig) | Type::BoxedFunction(sig) => {
      any_signature_type(sig, &holds_nested_function)
    }
    _ => any_child(t, &holds_function),
  }
}

/// Whether values of type `t` hold a (still boxed) function value, including
/// in a static function type's parameters or return.
pub(crate) fn holds_boxed_function(t: &Type) -> bool {
  match t {
    Type::BoxedFunction(_) => true,
    Type::Function(sig) => any_signature_type(sig, &holds_boxed_function),
    _ => any_child(t, &holds_boxed_function),
  }
}

// ===========================================================================
// Type boxing
// ===========================================================================

/// Rewrites types so that every function in a storage position is boxed.
/// "Nested" rewriting boxes functions inside aggregates but leaves a
/// top-level function type alone (a static higher-order parameter, a
/// statically-known function value); "top" rewriting also boxes a top-level
/// function type. A boxed signature is boxed all the way down.
struct TypeBoxer {
  structs: HashMap<Arc<str>, Arc<AbstractStruct>>,
  enums: HashMap<Arc<str>, Arc<AbstractEnum>>,
}

impl TypeBoxer {
  fn type_nested(&self, t: &mut Type) {
    match t {
      Type::Array(_, inner) => self.ts_top(&mut inner.kind),
      Type::Struct(s) => {
        for f in s.fields.iter_mut() {
          self.ts_top(&mut f.field_type.kind);
        }
        if let Some(boxed) = self.structs.get(&s.abstract_ancestor.name.0) {
          s.abstract_ancestor = boxed.clone();
        }
      }
      Type::Enum(e) => {
        for v in e.variants.iter_mut() {
          self.ts_top(&mut v.inner_type.kind);
        }
        if let Some(boxed) = self.enums.get(&e.abstract_ancestor.name.0) {
          e.abstract_ancestor = boxed.clone();
        }
      }
      Type::Function(sig) => {
        for (a, _) in sig.args.iter_mut() {
          self.ts_nested(&mut a.var_type.kind);
        }
        self.ts_nested(&mut sig.return_type.kind);
      }
      Type::BoxedFunction(sig) => {
        for (a, _) in sig.args.iter_mut() {
          self.ts_top(&mut a.var_type.kind);
        }
        self.ts_top(&mut sig.return_type.kind);
      }
      _ => {}
    }
  }
  fn type_top(&self, t: &mut Type) {
    if let Type::Function(sig) = t {
      let mut sig = (**sig).clone();
      sig.abstract_ancestor = None;
      *t = Type::BoxedFunction(Box::new(sig));
    }
    self.type_nested(t);
  }
  fn ts_nested(&self, ts: &mut TypeState) {
    ts.with_dereferenced_mut(|ts| {
      if let TypeState::Known(t) = ts {
        self.type_nested(t)
      }
    })
  }
  fn ts_top(&self, ts: &mut TypeState) {
    ts.with_dereferenced_mut(|ts| {
      if let TypeState::Known(t) = ts {
        self.type_top(t)
      }
    })
  }
  fn abstract_nested(&self, at: &mut AbstractType) {
    match at {
      AbstractType::Type(t) => self.type_nested(t),
      AbstractType::AbstractStruct(s) => {
        if let Some(boxed) = self.structs.get(&s.name.0) {
          *s = boxed.clone();
        }
      }
      AbstractType::AbstractEnum(e) => {
        if let Some(boxed) = self.enums.get(&e.name.0) {
          *e = boxed.clone();
        }
      }
      AbstractType::AbstractArray { inner_type, .. } => {
        self.abstract_top(inner_type)
      }
      AbstractType::Generic(_) | AbstractType::Unit => {}
    }
  }
  fn abstract_top(&self, at: &mut AbstractType) {
    if let AbstractType::Type(t) = at {
      self.type_top(t);
    } else {
      self.abstract_nested(at);
    }
  }
  fn boxed_signature(&self, sig: &FunctionSignature) -> FunctionSignature {
    let mut t = Type::Function(Box::new(sig.clone()));
    self.type_top(&mut t);
    let Type::BoxedFunction(boxed) = t else {
      unreachable!()
    };
    *boxed
  }
}

/// Builds the boxed versions of every struct / enum definition that holds a
/// function anywhere in its fields / payloads, recursing through the
/// definitions they reference (definitions are acyclic).
fn build_type_boxer(program: &Program) -> TypeBoxer {
  let mut boxer = TypeBoxer {
    structs: HashMap::new(),
    enums: HashMap::new(),
  };
  // Definitions can reference each other, so iterate until every boxed
  // definition refers only to boxed definitions (bounded by nesting depth).
  loop {
    let mut changed = false;
    for s in program.typedefs.structs.iter() {
      if !s
        .fields
        .iter()
        .any(|f| abstract_type_holds(&f.field_type, &holds_function))
      {
        continue;
      }
      let mut boxed = s.clone();
      for f in boxed.fields.iter_mut() {
        boxer.abstract_top(&mut f.field_type);
      }
      let is_new = boxer
        .structs
        .get(&s.name.0)
        .is_none_or(|existing| **existing != boxed);
      if is_new {
        boxer.structs.insert(s.name.0.clone(), Arc::new(boxed));
        changed = true;
      }
    }
    for e in program.typedefs.enums.iter() {
      if !e
        .variants
        .iter()
        .any(|v| abstract_type_holds(&v.inner_type, &holds_function))
      {
        continue;
      }
      let mut boxed = e.clone();
      for v in boxed.variants.iter_mut() {
        boxer.abstract_top(&mut v.inner_type);
      }
      let is_new = boxer
        .enums
        .get(&e.name.0)
        .is_none_or(|existing| **existing != boxed);
      if is_new {
        boxer.enums.insert(e.name.0.clone(), Arc::new(boxed));
        changed = true;
      }
    }
    if !changed {
      break;
    }
  }
  boxer
}

// ===========================================================================
// Shapes: which top-level function positions of a (static) function type are
// boxed. Used to carry a function's return classification to its call sites
// without disturbing the call site's own (possibly generic-instantiated)
// types.
// ===========================================================================

#[derive(Clone, Debug, PartialEq)]
enum Shape {
  Keep,
  Boxed,
  StaticFn { args: Vec<Shape>, ret: Box<Shape> },
}

fn shape_of(t: &Type) -> Shape {
  match t {
    Type::BoxedFunction(_) => Shape::Boxed,
    Type::Function(sig) => {
      let args: Vec<Shape> = sig
        .args
        .iter()
        .map(|(a, _)| {
          a.var_type
            .kind
            .try_unwrap_known()
            .map(|t| shape_of(&t))
            .unwrap_or(Shape::Keep)
        })
        .collect();
      let ret = sig
        .return_type
        .kind
        .try_unwrap_known()
        .map(|t| shape_of(&t))
        .unwrap_or(Shape::Keep);
      if args.iter().all(|a| *a == Shape::Keep) && ret == Shape::Keep {
        Shape::Keep
      } else {
        Shape::StaticFn {
          args,
          ret: Box::new(ret),
        }
      }
    }
    _ => Shape::Keep,
  }
}

fn apply_shape(ts: &mut TypeState, shape: &Shape, boxer: &TypeBoxer) {
  match shape {
    Shape::Keep => {}
    Shape::Boxed => boxer.ts_top(ts),
    Shape::StaticFn { args, ret } => ts.with_dereferenced_mut(|ts| {
      if let TypeState::Known(Type::Function(sig)) = ts {
        for ((a, _), s) in sig.args.iter_mut().zip(args.iter()) {
          apply_shape(&mut a.var_type.kind, s, boxer);
        }
        apply_shape(&mut sig.return_type.kind, ret, boxer);
      }
    }),
  }
}

// ===========================================================================
// The boxing walk
// ===========================================================================

#[derive(Clone, Copy, Debug, PartialEq)]
enum Kind {
  NotFn,
  Static,
  Boxed,
}

fn kind_of_type(t: &Type) -> Kind {
  match t {
    Type::Function(_) => Kind::Static,
    Type::BoxedFunction(_) => Kind::Boxed,
    _ => Kind::NotFn,
  }
}

/// `($fnbox-make value)`: wraps a statically-known function value into the
/// boxed representation.
fn wrap_in_box(exp: &mut TypedExp, boxed: FunctionSignature) {
  box_through(exp, boxed, FNBOX_MAKE, Ownership::Owned);
}

/// `($fnbox-borrow place)`: lends the static closure held in a local place
/// to a boxed function parameter, which advances it in place.
fn borrow_into_box(exp: &mut TypedExp, boxed: FunctionSignature) {
  box_through(exp, boxed, FNBOX_BORROW, Ownership::MutableReference);
}

/// `(builtin value)`, for the boxing builtin `builtin` taking the static
/// function value with `ownership`.
fn box_through(
  exp: &mut TypedExp,
  boxed: FunctionSignature,
  builtin: &str,
  ownership: Ownership,
) {
  take(exp, |value| {
    let b = ExpBuilder::at(&value.source_trace);
    let static_type = known_type(&value.data).unwrap();
    let boxed_type = Type::BoxedFunction(Box::new(boxed));
    let signature = builtin_signature(
      builtin,
      vec![(static_type, ownership)],
      boxed_type.clone(),
      None,
    );
    b.call(&signature, vec![value], &boxed_type)
  });
}

/// The callee of `($fnbox-apply boxed args...)`: boxed function arguments
/// are passed by mutable reference.
fn apply_callee(boxed: &FunctionSignature, source: &SourceTrace) -> TypedExp {
  let boxed_type = Type::BoxedFunction(Box::new(boxed.clone()));
  let params: Vec<Type> = std::iter::once(boxed_type)
    .chain(boxed.args.iter().map(|(a, _)| a.var_type.unwrap_known()))
    .collect();
  let return_type = boxed.return_type.unwrap_known();
  let signature = builtin_signature(
    FNBOX_APPLY,
    params
      .iter()
      .enumerate()
      .map(|(i, t)| {
        let ownership = if i == 0 || matches!(t, Type::BoxedFunction(_)) {
          Ownership::MutableReference
        } else {
          Ownership::Owned
        };
        (t.clone(), ownership)
      })
      .collect(),
    return_type.clone(),
    Some(Effect::InvokesUnknownFunction),
  );
  ExpBuilder::at(source).callee(&signature, &params, &return_type)
}

fn is_lvalue(exp: &TypedExp) -> bool {
  match &exp.kind {
    ExpKind::Name(_) => true,
    ExpKind::Access(_, inner) => is_lvalue(inner),
    ExpKind::Application(f, _) => {
      matches!(known_type(&f.data), Some(Type::Array(_, _))) && is_lvalue(f)
    }
    _ => false,
  }
}

/// Whether builtin parameter `i` takes a *host-invoked* function (the
/// function argument of `spawn-window` / `dispatch-*` / `start-audio`), which
/// must stay statically known.
fn is_host_function_param(
  signature: &AbstractFunctionSignature,
  i: usize,
) -> bool {
  match signature.arg_types.get(i).map(|(t, _)| t) {
    Some(AbstractType::Type(Type::Function(_))) => true,
    Some(AbstractType::Generic(name)) => {
      signature.generic_args.iter().any(|(generic_name, arg, _)| {
        generic_name == name
          && matches!(arg, GenericArgument::Type(constraints)
            if constraints.contains(&TypeConstraint::function()))
      })
    }
    _ => false,
  }
}

/// Visits every position of a call site's view of a callee's signature
/// where a type generic of the callee appears outside any aggregate — a whole
/// parameter or return, or (recursively) a function type's parameter or
/// return — passing the generic's name and the view's type there. Positions
/// inside arrays, structs, and enums are skipped: those are always boxed.
fn visit_generic_positions(
  abstract_type: &AbstractType,
  view: &mut TypeState,
  visit: &mut dyn FnMut(&Arc<str>, &mut TypeState),
) {
  match abstract_type {
    AbstractType::Generic(name) => visit(name, view),
    AbstractType::Type(t) => visit_skolem_positions(t, view, visit),
    _ => {}
  }
}

fn visit_skolem_positions(
  t: &Type,
  view: &mut TypeState,
  visit: &mut dyn FnMut(&Arc<str>, &mut TypeState),
) {
  match t {
    Type::Skolem(name, _) => visit(name, view),
    Type::Function(sig) => view.with_dereferenced_mut(|ts| {
      if let TypeState::Known(Type::Function(view_sig)) = ts {
        for ((arg, _), (view_arg, _)) in
          sig.args.iter().zip(view_sig.args.iter_mut())
        {
          if let Some(arg_type) = known_type(&arg.var_type) {
            visit_skolem_positions(
              &arg_type,
              &mut view_arg.var_type.kind,
              visit,
            );
          }
        }
        if let Some(return_type) = known_type(&sig.return_type) {
          visit_skolem_positions(
            &return_type,
            &mut view_sig.return_type.kind,
            visit,
          );
        }
      }
    }),
    _ => {}
  }
}

/// Visits every generic position (see [`visit_generic_positions`]) of a
/// call site's view `view` of `callee`'s parameters and return.
fn visit_signature_generic_positions(
  callee: &AbstractFunctionSignature,
  view: &mut FunctionSignature,
  visit: &mut dyn FnMut(&Arc<str>, &mut TypeState),
) {
  for ((abstract_type, _), (arg, _)) in
    callee.arg_types.iter().zip(view.args.iter_mut())
  {
    visit_generic_positions(abstract_type, &mut arg.var_type.kind, visit);
  }
  visit_generic_positions(
    &callee.return_type,
    &mut view.return_type.kind,
    visit,
  );
}

/// The type generics of a composite `callee` that the call site `view`
/// instantiates at a function type. User generics can't be constrained to
/// functions, so a generic body can only store, move, or return such a
/// value as opaque data: every position of the generic is a storage
/// position, boxed like a generic builtin's.
fn function_instantiated_generics(
  callee: &AbstractFunctionSignature,
  view: &FunctionSignature,
) -> HashSet<Arc<str>> {
  let mut generics = HashSet::new();
  if callee.generic_args.is_empty() {
    return generics;
  }
  visit_signature_generic_positions(
    callee,
    &mut view.clone(),
    &mut |name, ts| {
      if matches!(
        ts.try_unwrap_known(),
        Some(Type::Function(_) | Type::BoxedFunction(_))
      ) {
        generics.insert(name.clone());
      }
    },
  );
  generics
}

/// Whether `callee` is a composite function called with one of its type
/// generics instantiated at a function type.
fn instantiates_generic_at_function(callee: &TypedExp) -> bool {
  let Some(Type::Function(sig)) = known_type(&callee.data) else {
    return false;
  };
  let Some(ancestor) = &sig.abstract_ancestor else {
    return false;
  };
  let ancestor = ancestor.read().unwrap();
  matches!(
    ancestor.implementation,
    FunctionImplementationKind::Composite(_)
  ) && !function_instantiated_generics(&ancestor, &sig).is_empty()
}

/// Per-function boxing state shared by the dry-run classification walks and
/// the final rewriting walk.
struct Boxer<'a> {
  types: &'a TypeBoxer,
  /// Return shape of each composite function, keyed by implementation.
  return_shapes: &'a HashMap<usize, Shape>,
  /// Existing boxed-parameter clones, by [`CloneKey`].
  clones: &'a HashMap<CloneKey, Arc<RwLock<AbstractFunctionSignature>>>,
  global_vars: &'a HashSet<Arc<str>>,
  names: &'a RwLock<NameContext>,
  /// The registry signature of each composite function, by implementation.
  /// Inference attaches *copies* of signatures to references; later passes
  /// (monomorphization in particular) read a reference's ancestor, so every
  /// reference is repointed at the one (boxed) registry signature.
  canonical: &'a HashMap<usize, Arc<RwLock<AbstractFunctionSignature>>>,
  /// Requested-but-missing boxed-parameter clones.
  requests: Vec<CloneRequest>,
  /// Let-bound lambdas' parameters boxed so far, and the function being
  /// walked (keying its lambdas).
  local_boxed_params: &'a HashMap<LocalLambdaKey, BTreeSet<usize>>,
  function_key: usize,
  /// Requested-but-missing let-bound lambda parameter boxings.
  local_requests: Vec<(LocalLambdaKey, usize)>,
  /// Let-bound lambda names in the function being walked.
  local_lambdas: HashSet<Arc<str>>,
  /// Local bindings whose closures are lent to boxed calls, which advance
  /// them in place (they become `@var`s).
  lent_places: HashSet<Arc<str>>,
  env: HashMap<Arc<str>, (Kind, TypeState)>,
  /// The kind of every value the current function can return (its tail and
  /// each `return`), and the types of its `return`ed values.
  returns: Vec<Kind>,
  return_types: Vec<TypeState>,
  errors: Vec<CompileError>,
}

impl<'a> Boxer<'a> {
  fn error(&mut self, kind: CompileErrorKind, source: &SourceTrace) {
    self.errors.push(CompileError::new(kind, source.clone()));
  }
  fn set_top(&self, exp: &mut TypedExp, kind: Kind) {
    self.types.ts_nested(&mut exp.data.kind);
    if kind == Kind::Boxed {
      self.types.ts_top(&mut exp.data.kind);
    }
  }
  /// Coerces a value flowing into a boxed position: a statically-known
  /// function value is wrapped in `$fnbox-make`, pushing the wrap down into
  /// block / let tails and `if`/`match` arms so the wrapped operand is always
  /// a simple function-valued expression.
  fn coerce_to_box(&mut self, exp: &mut TypedExp) {
    match known_type(&exp.data) {
      Some(Type::Function(sig)) => match &mut exp.kind {
        ExpKind::Block(exps) if !exps.is_empty() => {
          self.coerce_to_box(exps.last_mut().unwrap());
          exp.data.kind = exps.last().unwrap().data.kind.clone();
        }
        ExpKind::Let(_, body) => {
          self.coerce_to_box(body);
          exp.data.kind = body.data.kind.clone();
        }
        ExpKind::Match(_, arms) => {
          for (_, value) in arms.iter_mut() {
            self.coerce_to_box(value);
          }
          self.types.ts_top(&mut exp.data.kind);
        }
        _ => {
          let (params, box_return) = static_function_positions(&sig);
          if !params.is_empty() || box_return {
            if !self.box_static_positions(exp, &params, box_return) {
              self.error(
                CompileErrorKind::UnsupportedFunctionValueSignature,
                &exp.source_trace,
              );
              return;
            }
          }
          let Some(Type::Function(sig)) = known_type(&exp.data) else {
            unreachable!()
          };
          let boxed = self.types.boxed_signature(&sig);
          wrap_in_box(exp, boxed);
        }
      },
      _ => {}
    }
  }
  /// Makes a static function value that takes or returns functions storable
  /// by boxing its top-level function-typed parameters (`params`) and, when
  /// `box_return`, its return: a reference to a top-level function is
  /// retargeted at a boxed-parameter clone, and a lambda literal is retyped
  /// in place and re-walked. Returns false for any other value (a local
  /// binding holding such a function, whose definition can't be retyped
  /// per use).
  fn box_static_positions(
    &mut self,
    exp: &mut TypedExp,
    params: &[usize],
    box_return: bool,
  ) -> bool {
    let retype = |types: &TypeBoxer, exp: &mut TypedExp| {
      exp.data.kind.with_dereferenced_mut(|ts| {
        if let TypeState::Known(Type::Function(s)) = ts {
          for i in params {
            types.ts_top(&mut s.args[*i].0.var_type.kind);
          }
          if box_return {
            types.ts_top(&mut s.return_type.kind);
          }
        }
      });
    };
    match &mut exp.kind {
      ExpKind::Function(_, _) => {
        retype(self.types, exp);
        self.walk(exp);
        true
      }
      ExpKind::Name(name) if !self.env.contains_key(name) => {
        let Some(Type::Function(sig)) = known_type(&exp.data) else {
          return false;
        };
        // Inference can leave a reference unified from a sibling array
        // element without its ancestor; resolve it by name then.
        let Some(ancestor) = sig.abstract_ancestor.clone().or_else(|| {
          let mut candidates = self.canonical.values().filter(|c| {
            let c = c.read().unwrap();
            c.name == *name && c.arg_types.len() == sig.args.len()
          });
          let found = candidates.next().cloned();
          if candidates.next().is_some() {
            None
          } else {
            found
          }
        }) else {
          return false;
        };
        let Some(key) = implementation_key(&ancestor) else {
          return false;
        };
        let clone_key = (key, params.to_vec(), box_return);
        let Some(clone) = self.clones.get(&clone_key).cloned() else {
          self.requests.push((ancestor, params.to_vec(), box_return));
          return false;
        };
        *name = clone.read().unwrap().name.clone();
        exp.data.kind.with_dereferenced_mut(|ts| {
          if let TypeState::Known(Type::Function(s)) = ts {
            s.abstract_ancestor = Some(clone.clone());
          }
        });
        retype(self.types, exp);
        true
      }
      _ => false,
    }
  }
  /// Boxes every position of the generics in `generics` (instantiated at a
  /// function type, see [`function_instantiated_generics`]) in the call
  /// site's view `callee` of `ancestor`, so monomorphization instantiates
  /// them at the boxed type. A static function argument whose parameter now
  /// takes or returns boxed values (`f: (Fn [T] U)` in a generic `map`) is
  /// made to box them too; top-level boxed parameters are coerced by the
  /// caller like any other.
  fn box_generic_instantiations(
    &mut self,
    callee: &mut TypedExp,
    args: &mut [TypedExp],
    arg_kinds: &[Kind],
    ancestor: &Arc<RwLock<AbstractFunctionSignature>>,
    generics: &HashSet<Arc<str>>,
  ) {
    let types = self.types;
    callee.data.kind.with_dereferenced_mut(|ts| {
      if let TypeState::Known(Type::Function(view)) = ts {
        visit_signature_generic_positions(
          &ancestor.read().unwrap(),
          view,
          &mut |name, ts| {
            if generics.contains(name) {
              types.ts_top(ts);
            }
          },
        );
      }
    });
    let Some(Type::Function(view)) = known_type(&callee.data) else {
      return;
    };
    for (i, arg) in args.iter_mut().enumerate() {
      if arg_kinds.get(i) != Some(&Kind::Static) {
        continue;
      }
      let (Some(Some(Type::Function(param))), Some(Type::Function(arg_sig))) = (
        view.args.get(i).map(|(a, _)| known_type(&a.var_type)),
        known_type(&arg.data),
      ) else {
        continue;
      };
      let newly_boxed = |param_type: &TypeState, arg_type: &TypeState| {
        matches!(param_type.try_unwrap_known(), Some(Type::BoxedFunction(_)))
          && matches!(arg_type.try_unwrap_known(), Some(Type::Function(_)))
      };
      let params: Vec<usize> = param
        .args
        .iter()
        .zip(arg_sig.args.iter())
        .enumerate()
        .filter(|(_, ((p, _), (a, _)))| {
          newly_boxed(&p.var_type.kind, &a.var_type.kind)
        })
        .map(|(j, _)| j)
        .collect();
      let box_return =
        newly_boxed(&param.return_type.kind, &arg_sig.return_type.kind);
      if (!params.is_empty() || box_return)
        && !self.box_static_positions(arg, &params, box_return)
      {
        self.error(
          CompileErrorKind::UnsupportedFunctionValueSignature,
          &arg.source_trace,
        );
      }
    }
  }
  /// Prepares an argument for a boxed function parameter (passed by mutable
  /// reference): a static closure held in a local place is lent as
  /// `$fnbox-borrow`, which lowers to the place itself (or, when the
  /// parameter's set is a union, to a copy whose state is written back after
  /// the call); any other static function is boxed; and the result is bound
  /// to a temporary unless it's already a place. Returns that temporary.
  fn pass_to_boxed_param(
    &mut self,
    a: &mut TypedExp,
    param: FunctionSignature,
    kind: Kind,
  ) -> Option<(Arc<str>, TypedExp)> {
    if kind == Kind::Static {
      if let ExpKind::Name(n) = &a.kind
        && self.env.contains_key(n)
      {
        self.lent_places.insert(n.clone());
        borrow_into_box(a, param);
        return None;
      }
      self.coerce_to_box(a);
    }
    self.ensure_place(a)
  }
  /// Wraps `exp` in a `let` binding `temps` as `@var`s.
  fn bind_temps(
    exp: &mut TypedExp,
    temps: Vec<(Arc<str>, TypedExp)>,
    source: &SourceTrace,
  ) {
    if temps.is_empty() {
      return;
    }
    take(exp, |application| {
      let_around(
        temps
          .into_iter()
          .map(|(n, v)| (n, source.clone(), VariableKind::Var, v))
          .collect(),
        application,
      )
    });
  }
  /// Binds `exp` to a fresh `@var` local if it isn't already a place, so it
  /// can be passed by mutable reference.
  fn ensure_place(
    &mut self,
    exp: &mut TypedExp,
  ) -> Option<(Arc<str>, TypedExp)> {
    if is_lvalue(exp) {
      return None;
    }
    let name = self.names.write().unwrap().gensym("fnval_temp");
    let value = std::mem::replace(
      exp,
      Exp {
        data: exp.data.clone(),
        kind: ExpKind::Name(name.clone()),
        source_trace: exp.source_trace.clone(),
      },
    );
    Some((name, value))
  }
  fn bind_pattern_names(&mut self, pattern: &mut TypedExp) {
    self.set_top(pattern, Kind::NotFn);
    match &mut pattern.kind {
      ExpKind::Application(f, args) => {
        self.set_top(f, Kind::NotFn);
        for a in args.iter_mut() {
          if let ExpKind::Name(bound) = &a.kind {
            let bound = bound.clone();
            let kind = known_type(&a.data)
              .map(|t| match t {
                Type::Function(_) => Kind::Boxed,
                t => kind_of_type(&t),
              })
              .unwrap_or(Kind::NotFn);
            self.set_top(a, kind);
            self.env.insert(bound, (kind, a.data.kind.clone()));
          } else {
            self.bind_pattern_names(a);
          }
        }
      }
      _ => {}
    }
  }
  fn walk(&mut self, exp: &mut TypedExp) -> Kind {
    let kind = self.walk_inner(exp);
    self.set_top(exp, kind);
    kind
  }
  fn walk_inner(&mut self, exp: &mut TypedExp) -> Kind {
    let own_type = known_type(&exp.data);
    let own_is_fn = matches!(own_type, Some(Type::Function(_)));
    match &mut exp.kind {
      ExpKind::Name(name) => {
        if let Some((kind, t)) = self.env.get(name) {
          let kind = *kind;
          exp.data.kind = t.clone();
          kind
        } else if self.global_vars.contains(name) {
          if own_is_fn { Kind::Boxed } else { Kind::NotFn }
        } else if let Some(Type::Function(sig)) = &own_type {
          // A top-level function referenced as a value: carry its return
          // classification onto this reference.
          if let Some(key) =
            sig.abstract_ancestor.as_ref().and_then(implementation_key)
            && let Some(canonical) = self.canonical.get(&key)
          {
            let canonical = canonical.clone();
            exp.data.kind.with_dereferenced_mut(|ts| {
              if let TypeState::Known(Type::Function(sig)) = ts {
                sig.abstract_ancestor = Some(canonical);
              }
            });
          }
          if let Some(key) =
            sig.abstract_ancestor.as_ref().and_then(implementation_key)
            && let Some(shape) = self.return_shapes.get(&key)
          {
            let shape = shape.clone();
            exp.data.kind.with_dereferenced_mut(|ts| {
              if let TypeState::Known(Type::Function(sig)) = ts {
                apply_shape(&mut sig.return_type.kind, &shape, self.types);
              }
            });
          }
          Kind::Static
        } else {
          Kind::NotFn
        }
      }
      ExpKind::Function(arg_names, body) => {
        // A lambda: its function-typed parameters are static (it is either
        // called directly or higher-order-inlined like any other function).
        let Some(Type::Function(sig)) = &own_type else {
          return Kind::NotFn;
        };
        for ((name, _), (arg, _)) in arg_names.iter().zip(sig.args.iter()) {
          let mut t = arg.var_type.kind.clone();
          self.types.ts_nested(&mut t);
          let kind = known_type(&arg.var_type)
            .map(|t| kind_of_type(&t))
            .unwrap_or(Kind::NotFn);
          self.env.insert(name.clone(), (kind, t));
        }
        let outer_returns = std::mem::take(&mut self.returns);
        let outer_return_types = std::mem::take(&mut self.return_types);
        let tail = self.walk(body);
        self.returns.push(tail);
        let returned_kind = if matches!(
          sig.return_type.kind.try_unwrap_known(),
          Some(Type::BoxedFunction(_))
        ) {
          Kind::Boxed
        } else {
          self.merged_return_kind()
        };
        if returned_kind == Kind::Boxed {
          self.coerce_returns(body);
        }
        self.returns = outer_returns;
        self.return_types = outer_return_types;
        let body_type = body.data.kind.clone();
        exp.data.kind.with_dereferenced_mut(|ts| {
          if let TypeState::Known(Type::Function(sig)) = ts {
            for (a, _) in sig.args.iter_mut() {
              self.types.ts_nested(&mut a.var_type.kind);
            }
            if returned_kind == Kind::Boxed {
              self.types.ts_top(&mut sig.return_type.kind);
            } else if let Some(t) = body_type.try_unwrap_known() {
              sig.return_type.kind = TypeState::Known(t);
            }
          }
        });
        Kind::Static
      }
      ExpKind::Let(bindings, body) => {
        for (name, _, var_kind, value) in bindings.iter_mut() {
          if let ExpKind::Function(_, _) = &value.kind {
            self.local_lambdas.insert(name.clone());
            if let Some(params) = self
              .local_boxed_params
              .get(&(self.function_key, name.clone()))
            {
              let types = self.types;
              value.data.kind.with_dereferenced_mut(|ts| {
                if let TypeState::Known(Type::Function(s)) = ts {
                  for i in params {
                    types.ts_top(&mut s.args[*i].0.var_type.kind);
                  }
                }
              });
            }
          }
          let mut kind = self.walk(value);
          if kind == Kind::Static && *var_kind == VariableKind::Var {
            self.coerce_to_box(value);
            kind = Kind::Boxed;
          }
          self
            .env
            .insert(name.clone(), (kind, value.data.kind.clone()));
        }
        let kind = self.walk(body);
        exp.data.kind = body.data.kind.clone();
        kind
      }
      ExpKind::Block(exps) => {
        let mut kind = Kind::NotFn;
        for e in exps.iter_mut() {
          kind = self.walk(e);
        }
        if let Some(last) = exps.last() {
          exp.data.kind = last.data.kind.clone();
        }
        kind
      }
      ExpKind::Match(scrutinee, arms) => {
        self.walk(scrutinee);
        let mut arm_kinds = vec![];
        for (pattern, value) in arms.iter_mut() {
          self.bind_pattern_names(pattern);
          arm_kinds.push(self.walk(value));
        }
        if own_is_fn || arm_kinds.contains(&Kind::Boxed) {
          for (_, value) in arms.iter_mut() {
            self.coerce_to_box(value);
          }
          Kind::Boxed
        } else {
          Kind::NotFn
        }
      }
      ExpKind::Access(accessor, inner) => {
        if let Accessor::ArrayIndex(index) = accessor {
          self.walk(index);
        }
        self.walk(inner);
        if own_is_fn { Kind::Boxed } else { Kind::NotFn }
      }
      ExpKind::ArrayLiteral(elements) => {
        for e in elements.iter_mut() {
          self.walk(e);
          self.coerce_to_box(e);
        }
        Kind::NotFn
      }
      ExpKind::Return(value) => {
        let kind = self.walk(value);
        self.returns.push(kind);
        self.return_types.push(value.data.kind.clone());
        Kind::NotFn
      }
      ExpKind::ForLoop {
        increment_variable_initial_value_expression,
        continue_condition_expression,
        update_expression,
        body_expression,
        ..
      } => {
        self.walk(increment_variable_initial_value_expression);
        self.walk(continue_condition_expression);
        if let Some(u) = update_expression {
          self.walk(u);
        }
        self.walk(body_expression);
        Kind::NotFn
      }
      ExpKind::WhileLoop {
        condition_expression,
        body_expression,
      } => {
        self.walk(condition_expression);
        self.walk(body_expression);
        Kind::NotFn
      }
      ExpKind::Uninitialized => {
        if own_is_fn {
          Kind::Boxed
        } else {
          Kind::NotFn
        }
      }
      ExpKind::Application(_, _) => self.walk_application(exp),
      _ => {
        if own_is_fn {
          Kind::Static
        } else {
          Kind::NotFn
        }
      }
    }
  }
  fn merged_return_kind(&self) -> Kind {
    let function_returns =
      self.returns.iter().filter(|k| **k != Kind::NotFn).count();
    if self.returns.contains(&Kind::Boxed) || function_returns > 1 {
      Kind::Boxed
    } else if function_returns == 1 {
      Kind::Static
    } else {
      Kind::NotFn
    }
  }
  /// Coerces the tail value and every `return`ed value of a body whose
  /// return is boxed.
  fn coerce_returns(&mut self, body: &mut TypedExp) {
    self.coerce_to_box(body);
    let _ = body.walk_mut::<()>(&mut |e| {
      if let ExpKind::Return(value) = &mut e.kind {
        self.coerce_to_box(value);
      }
      Ok(!matches!(e.kind, ExpKind::Function(_, _)))
    });
    self.types.ts_top(&mut body.data.kind);
  }
  fn walk_application(&mut self, exp: &mut TypedExp) -> Kind {
    let source = exp.source_trace.clone();
    let ExpKind::Application(f, args) = &mut exp.kind else {
      unreachable!()
    };
    self.walk(f);
    let arg_kinds: Vec<Kind> = args.iter_mut().map(|a| self.walk(a)).collect();
    let callee_type = known_type(&f.data);
    match callee_type {
      Some(Type::BoxedFunction(boxed)) => {
        // Applying a runtime-selected function value.
        // Function arguments of a boxed function are passed by mutable
        // reference, so a stateful closure argument advances in place. A
        // boxed argument is passed as (or bound to) a place; a static
        // closure held in a local place is lent as `$fnbox-borrow`, which
        // lowers to the place itself (or, when the parameter's set is a
        // union, to a copy whose state is written back after the call).
        let mut temps = vec![];
        for (i, a) in args.iter_mut().enumerate() {
          if let Some(Type::BoxedFunction(param)) =
            boxed.args.get(i).and_then(|(v, _)| known_type(&v.var_type))
          {
            temps.extend(self.pass_to_boxed_param(a, *param, arg_kinds[i]));
          }
        }
        let return_kind = kind_of_type(&boxed.return_type.unwrap_known());
        let mut new_callee = apply_callee(&boxed, &source);
        std::mem::swap(f.as_mut(), &mut new_callee);
        let mut value = new_callee;
        if let Some(temp) = self.ensure_place(&mut value) {
          temps.insert(0, temp);
        }
        args.insert(0, value);
        exp.data.kind = boxed.return_type.kind.clone();
        Self::bind_temps(exp, temps, &source);
        return_kind
      }
      Some(Type::Function(sig)) => {
        let ancestor = sig.abstract_ancestor.clone();
        let implementation_kind = ancestor
          .as_ref()
          .map(|a| a.read().unwrap().implementation.clone());
        match implementation_kind {
          Some(FunctionImplementationKind::Composite(_)) => {
            let ancestor = ancestor.unwrap();
            let generics =
              function_instantiated_generics(&ancestor.read().unwrap(), &sig);
            if !generics.is_empty() {
              self.box_generic_instantiations(
                f, args, &arg_kinds, &ancestor, &generics,
              );
            }
            let unboxed_sig = sig;
            let sig = match known_type(&f.data) {
              Some(Type::Function(boxed_sig)) => boxed_sig,
              _ => unboxed_sig.clone(),
            };
            // Parameters boxed by the generic instantiation, which (like a
            // clone's boxed parameters) lower to references.
            let generic_boxed_params: Vec<usize> = sig
              .args
              .iter()
              .zip(unboxed_sig.args.iter())
              .enumerate()
              .filter(|(_, ((boxed, _), (unboxed, _)))| {
                matches!(
                  known_type(&boxed.var_type),
                  Some(Type::BoxedFunction(_))
                ) && !matches!(
                  known_type(&unboxed.var_type),
                  Some(Type::BoxedFunction(_))
                )
              })
              .map(|(i, _)| i)
              .collect();
            // Static higher-order parameters receiving a boxed value need a
            // clone of the callee with those parameters boxed.
            let boxed_params: Vec<usize> = sig
              .args
              .iter()
              .enumerate()
              .filter(|(i, (a, _))| {
                matches!(known_type(&a.var_type), Some(Type::Function(_)))
                  && arg_kinds.get(*i) == Some(&Kind::Boxed)
              })
              .map(|(i, _)| i)
              .collect();
            let target = if boxed_params.is_empty() {
              Some(ancestor.clone())
            } else {
              let key = implementation_key(&ancestor).unwrap();
              match self.clones.get(&(key, boxed_params.clone(), false)) {
                Some(clone) => Some(clone.clone()),
                None => {
                  self.requests.push((
                    ancestor.clone(),
                    boxed_params.clone(),
                    false,
                  ));
                  None
                }
              }
            };
            if let Some(target) = &target
              && !boxed_params.is_empty()
            {
              let target_name = target.read().unwrap().name.clone();
              if let ExpKind::Name(n) = &mut f.kind {
                *n = target_name;
              }
              f.data.kind.with_dereferenced_mut(|ts| {
                if let TypeState::Known(Type::Function(s)) = ts {
                  s.abstract_ancestor = Some(target.clone());
                  for i in boxed_params.iter() {
                    self.types.ts_top(&mut s.args[*i].0.var_type.kind);
                  }
                }
              });
            }
            // The (possibly cloned) callee's return classification.
            if let Some(target) = &target
              && let Some(key) = implementation_key(target)
              && let Some(shape) = self.return_shapes.get(&key)
            {
              let shape = shape.clone();
              f.data.kind.with_dereferenced_mut(|ts| {
                if let TypeState::Known(Type::Function(s)) = ts {
                  apply_shape(&mut s.return_type.kind, &shape, self.types);
                }
              });
            }
            // Coerce arguments flowing into boxed parameters.
            let param_types: Vec<Option<Type>> = match known_type(&f.data) {
              Some(Type::Function(s)) => s
                .args
                .iter()
                .map(|(a, _)| known_type(&a.var_type))
                .collect(),
              _ => vec![],
            };
            let mut temps = vec![];
            for (i, a) in args.iter_mut().enumerate() {
              if matches!(
                param_types.get(i),
                Some(Some(Type::BoxedFunction(_)))
              ) {
                if arg_kinds[i] == Kind::Static {
                  self.coerce_to_box(a);
                }
                if boxed_params.contains(&i)
                  || generic_boxed_params.contains(&i)
                {
                  if let Some(temp) = self.ensure_place(a) {
                    temps.push(temp);
                  }
                }
              }
            }
            let return_type = match known_type(&f.data) {
              Some(Type::Function(s)) => s.return_type.kind.clone(),
              _ => exp.data.kind.clone(),
            };
            exp.data.kind = return_type;
            let kind = known_type(&exp.data)
              .map(|t| kind_of_type(&t))
              .unwrap_or(Kind::NotFn);
            Self::bind_temps(exp, temps, &source);
            kind
          }
          Some(
            FunctionImplementationKind::StructConstructor
            | FunctionImplementationKind::EnumConstructor(_),
          ) => {
            // Storage positions: every function-typed field / payload is boxed.
            if let Some(ancestor) = &ancestor {
              let mut a = ancestor.write().unwrap();
              for (t, _) in a.arg_types.iter_mut() {
                self.types.abstract_top(t);
              }
              self.types.abstract_nested(&mut a.return_type);
            }
            f.data.kind.with_dereferenced_mut(|ts| {
              if let TypeState::Known(Type::Function(s)) = ts {
                for (a, _) in s.args.iter_mut() {
                  self.types.ts_top(&mut a.var_type.kind);
                }
              }
            });
            for a in args.iter_mut() {
              self.coerce_to_box(a);
            }
            Kind::NotFn
          }
          Some(FunctionImplementationKind::Builtin { .. }) => {
            let ancestor = ancestor.unwrap();
            let name = ancestor.read().unwrap().name.clone();
            if &*name == FNBOX_MAKE || &*name == FNBOX_BORROW {
              return Kind::Boxed;
            }
            if &*name == FNBOX_APPLY {
              return known_type(&exp.data)
                .map(|t| kind_of_type(&t))
                .unwrap_or(Kind::NotFn);
            }
            let host_params: Vec<bool> = (0..args.len())
              .map(|i| is_host_function_param(&ancestor.read().unwrap(), i))
              .collect();
            for (i, a) in args.iter().enumerate() {
              if host_params[i] && arg_kinds[i] == Kind::Boxed {
                self.error(
                  CompileErrorKind::DynamicFunctionValueNotAllowedHere(
                    name.to_string(),
                  ),
                  &a.source_trace,
                );
              }
            }
            // A generic builtin instantiated at a function type (`push`,
            // `into-dynamic-array`, assignment, ...) stores or moves the
            // value: those positions are boxed.
            let mut boxed_positions = vec![];
            f.data.kind.with_dereferenced_mut(|ts| {
              if let TypeState::Known(Type::Function(s)) = ts {
                for (i, (a, _)) in s.args.iter_mut().enumerate() {
                  if !host_params[i]
                    && matches!(
                      a.var_type.kind.try_unwrap_known(),
                      Some(Type::Function(_))
                    )
                  {
                    self.types.ts_top(&mut a.var_type.kind);
                    boxed_positions.push(i);
                  }
                }
                self.types.ts_top(&mut s.return_type.kind);
              }
            });
            for i in boxed_positions {
              self.coerce_to_box(&mut args[i]);
            }
            if own_type_is_fn(exp) {
              Kind::Boxed
            } else {
              Kind::NotFn
            }
          }
          None => {
            // Applying a local statically-known function value: a
            // let-bound lambda, whose function parameters receiving a boxed
            // value get boxed (for every call, via the classification
            // fixpoint), or a higher-order parameter, whose function
            // parameters are static.
            let local_lambda = match &f.kind {
              ExpKind::Name(n) if self.local_lambdas.contains(n) => {
                Some(n.clone())
              }
              _ => None,
            };
            let mut temps = vec![];
            for (i, a) in args.iter_mut().enumerate() {
              match sig.args.get(i).and_then(|(v, _)| known_type(&v.var_type)) {
                Some(Type::Function(_)) if arg_kinds[i] == Kind::Boxed => {
                  match &local_lambda {
                    Some(lambda) => self
                      .local_requests
                      .push(((self.function_key, lambda.clone()), i)),
                    None => self.error(
                      CompileErrorKind::StoredFunctionToHigherOrderParameter,
                      &a.source_trace,
                    ),
                  }
                }
                Some(Type::BoxedFunction(param)) => {
                  temps.extend(self.pass_to_boxed_param(
                    a,
                    *param,
                    arg_kinds[i],
                  ));
                }
                _ => {}
              }
            }
            exp.data.kind = sig.return_type.kind.clone();
            let kind = known_type(&exp.data)
              .map(|t| kind_of_type(&t))
              .unwrap_or(Kind::NotFn);
            Self::bind_temps(exp, temps, &source);
            kind
          }
        }
      }
      Some(Type::Array(_, _)) => {
        // `(arr i)`: indexing out a stored function yields a boxed value.
        if own_type_is_fn(exp) {
          Kind::Boxed
        } else {
          Kind::NotFn
        }
      }
      _ => Kind::NotFn,
    }
  }
}

/// The top-level function-typed parameters of a static function signature,
/// and whether it returns a function: the positions boxed when the function
/// is itself stored as a value.
fn static_function_positions(sig: &FunctionSignature) -> (Vec<usize>, bool) {
  let params = sig
    .args
    .iter()
    .enumerate()
    .filter(|(_, (a, _))| {
      matches!(known_type(&a.var_type), Some(Type::Function(_)))
    })
    .map(|(i, _)| i)
    .collect();
  let box_return = matches!(
    sig.return_type.kind.try_unwrap_known(),
    Some(Type::Function(_))
  );
  (params, box_return)
}

fn own_type_is_fn(exp: &TypedExp) -> bool {
  matches!(
    known_type(&exp.data),
    Some(Type::Function(_) | Type::BoxedFunction(_))
  )
}

// ===========================================================================
// Boxing driver
// ===========================================================================

/// Whether any function value in the program needs boxing — if not, the
/// pass is a strict no-op.
fn program_has_function_values(program: &Program) -> bool {
  if program.typedefs.structs.iter().any(|s| {
    s.fields
      .iter()
      .any(|f| abstract_type_holds(&f.field_type, &holds_function))
  }) || program.typedefs.enums.iter().any(|e| {
    e.variants
      .iter()
      .any(|v| abstract_type_holds(&v.inner_type, &holds_function))
  }) || program
    .top_level_vars
    .iter()
    .any(|v| holds_function(&v.var_type))
  {
    return true;
  }
  for f in program.abstract_functions_iter() {
    let FunctionImplementationKind::Composite(implementation) =
      &f.read().unwrap().implementation
    else {
      continue;
    };
    let mut found = false;
    implementation
      .read()
      .unwrap()
      .expression
      .walk(&mut |exp| {
        let t = known_type(&exp.data);
        let is_fn = matches!(t, Some(Type::Function(_)));
        found |= t.as_ref().is_some_and(holds_nested_function)
          || (is_fn
            && matches!(
              exp.kind,
              ExpKind::Match(_, _)
                | ExpKind::Access(_, _)
                | ExpKind::Uninitialized
            ))
          || matches!(&exp.kind, ExpKind::Let(bindings, _)
          if bindings.iter().any(|(_, _, kind, value)| {
            *kind == VariableKind::Var
              && matches!(known_type(&value.data), Some(Type::Function(_)))
          }))
          || matches!(&exp.kind, ExpKind::Application(f, _)
            if instantiates_generic_at_function(f));
        Ok::<bool, Never>(!found)
      })
      .unwrap();
    if found {
      return true;
    }
  }
  false
}

/// A boxed-parameter clone's identity: (implementation, boxed parameters,
/// whether the return is boxed).
type CloneKey = (usize, Vec<usize>, bool);

/// A requested boxed-parameter clone: (callee, boxed parameters, whether the
/// return is boxed).
type CloneRequest = (Arc<RwLock<AbstractFunctionSignature>>, Vec<usize>, bool);

/// A let-bound lambda: (enclosing function's implementation key, binding
/// name). Deshadowing makes binding names unique within a function.
type LocalLambdaKey = (usize, Arc<str>);

/// What every boxing walk reads: the type boxer and the classification
/// fixpoint's current state.
struct BoxingContext<'a> {
  types: &'a TypeBoxer,
  return_shapes: &'a HashMap<usize, Shape>,
  clones: &'a HashMap<CloneKey, Arc<RwLock<AbstractFunctionSignature>>>,
  local_boxed_params: &'a HashMap<LocalLambdaKey, BTreeSet<usize>>,
  global_vars: &'a HashSet<Arc<str>>,
  names: &'a RwLock<NameContext>,
  canonical: &'a HashMap<usize, Arc<RwLock<AbstractFunctionSignature>>>,
}

impl<'a> BoxingContext<'a> {
  fn boxer(&self, function_key: usize) -> Boxer<'a> {
    Boxer {
      types: self.types,
      return_shapes: self.return_shapes,
      clones: self.clones,
      local_boxed_params: self.local_boxed_params,
      global_vars: self.global_vars,
      names: self.names,
      canonical: self.canonical,
      function_key,
      requests: vec![],
      local_requests: vec![],
      local_lambdas: HashSet::new(),
      lent_places: HashSet::new(),
      env: HashMap::new(),
      returns: vec![],
      return_types: vec![],
      errors: vec![],
    }
  }
}

struct BoxingOutcome {
  shape: Shape,
  requests: Vec<CloneRequest>,
  /// Let-bound lambdas' static function parameters receiving a boxed value.
  local_requests: Vec<(LocalLambdaKey, usize)>,
  errors: Vec<CompileError>,
}

fn box_function_body(
  implementation: &mut TopLevelFunction,
  cx: &BoxingContext,
  function_key: usize,
) -> BoxingOutcome {
  let types = cx.types;
  let mut boxer = cx.boxer(function_key);
  types.ts_nested(&mut implementation.expression.data.kind);
  let Some(Type::Function(sig)) = known_type(&implementation.expression.data)
  else {
    return BoxingOutcome {
      shape: Shape::Keep,
      requests: vec![],
      local_requests: vec![],
      errors: vec![],
    };
  };
  let ExpKind::Function(arg_names, body) = &mut implementation.expression.kind
  else {
    unreachable!()
  };
  for ((name, _), (arg, _)) in arg_names.iter().zip(sig.args.iter()) {
    let kind = known_type(&arg.var_type)
      .map(|t| kind_of_type(&t))
      .unwrap_or(Kind::NotFn);
    boxer
      .env
      .insert(name.clone(), (kind, arg.var_type.kind.clone()));
  }
  let tail = boxer.walk(body);
  boxer.returns.push(tail);
  let declared_return = sig.return_type.kind.clone();
  let returned = if matches!(
    declared_return.try_unwrap_known(),
    Some(Type::BoxedFunction(_))
  ) {
    Kind::Boxed
  } else {
    boxer.merged_return_kind()
  };
  let return_type = match returned {
    Kind::Boxed => {
      boxer.coerce_returns(body);
      let mut t = declared_return.clone();
      types.ts_top(&mut t);
      t
    }
    Kind::Static => {
      // The single returned static value's own (possibly refined) type.
      let returned_type = boxer
        .return_types
        .first()
        .cloned()
        .unwrap_or_else(|| body.data.kind.clone());
      if matches!(returned_type.try_unwrap_known(), Some(Type::Function(_))) {
        returned_type
      } else {
        declared_return.clone()
      }
    }
    Kind::NotFn => declared_return.clone(),
  };
  let shape = return_type
    .try_unwrap_known()
    .map(|t| shape_of(&t))
    .unwrap_or(Shape::Keep);
  implementation
    .expression
    .data
    .kind
    .with_dereferenced_mut(|ts| {
      if let TypeState::Known(Type::Function(sig)) = ts {
        apply_shape(&mut sig.return_type.kind, &shape, types);
      }
    });
  if !boxer.lent_places.is_empty() {
    let targets = &boxer.lent_places;
    let _ = implementation.expression.walk_mut::<()>(&mut |e| {
      if let ExpKind::Let(bindings, _) = &mut e.kind {
        for (name, _, kind, _) in bindings.iter_mut() {
          if targets.contains(name) {
            *kind = VariableKind::Var;
          }
        }
      }
      Ok(true)
    });
  }
  BoxingOutcome {
    shape,
    requests: boxer.requests,
    local_requests: boxer.local_requests,
    errors: boxer.errors,
  }
}

/// Boxes a top-level var's initializer (`var_type` is the var's already-boxed
/// type).
fn box_global_initializer(
  value: &mut TypedExp,
  var_type: &Type,
  cx: &BoxingContext,
) -> BoxingOutcome {
  let mut boxer = cx.boxer(0);
  let kind = boxer.walk(value);
  if kind == Kind::Static && matches!(var_type, Type::BoxedFunction(_)) {
    boxer.coerce_to_box(value);
  }
  BoxingOutcome {
    shape: Shape::Keep,
    requests: boxer.requests,
    local_requests: boxer.local_requests,
    errors: boxer.errors,
  }
}

impl Program {
  /// See the module docs: retypes every function value in a dynamic
  /// position as a `BoxedFunction`, inserting `$fnbox-make` wraps and
  /// `$fnbox-apply` calls. A strict no-op for programs without function
  /// values.
  pub fn box_function_values(&mut self, errors: &mut ErrorLog) {
    if !program_has_function_values(self) {
      return;
    }
    let types = build_type_boxer(self);
    // Snapshot every body's types so a top-level retype never leaks through
    // a unification variable shared with another node.
    let composites: Vec<(
      Arc<RwLock<AbstractFunctionSignature>>,
      Arc<RwLock<TopLevelFunction>>,
    )> = self
      .abstract_functions_iter()
      .filter_map(|f| match &f.read().unwrap().implementation {
        FunctionImplementationKind::Composite(implementation) => {
          Some((f.clone(), implementation.clone()))
        }
        _ => None,
      })
      .collect();
    let mut seen_implementations = HashSet::new();
    for (_, implementation) in composites.iter() {
      if seen_implementations.insert(ptr_key(implementation)) {
        implementation.write().unwrap().expression.snapshot_types();
      }
    }
    // Definitions and signatures.
    for s in self.typedefs.structs.iter_mut() {
      if let Some(boxed) = types.structs.get(&s.name.0) {
        *s = (**boxed).clone();
      }
    }
    for e in self.typedefs.enums.iter_mut() {
      if let Some(boxed) = types.enums.get(&e.name.0) {
        *e = (**boxed).clone();
      }
    }
    for f in self.abstract_functions_iter() {
      let mut f = f.write().unwrap();
      let storage = matches!(
        f.implementation,
        FunctionImplementationKind::StructConstructor
          | FunctionImplementationKind::EnumConstructor(_)
      );
      match &f.implementation {
        FunctionImplementationKind::Builtin { .. } => continue,
        _ => {}
      }
      for (t, _) in f.arg_types.iter_mut() {
        if storage {
          types.abstract_top(t);
        } else {
          types.abstract_nested(t);
        }
      }
      types.abstract_nested(&mut f.return_type);
    }
    for v in self.top_level_vars.iter_mut() {
      types.type_top(&mut v.var_type);
    }
    let global_vars: HashSet<Arc<str>> =
      self.top_level_vars.iter().map(|v| v.name.clone()).collect();
    // Pristine (pre-walk) bodies, keyed by implementation.
    let mut pristine: HashMap<usize, TopLevelFunction> = HashMap::new();
    let mut functions: Vec<(usize, Arc<RwLock<AbstractFunctionSignature>>)> =
      vec![];
    for (sig, implementation) in composites.iter() {
      let key = ptr_key(implementation);
      if !pristine.contains_key(&key) {
        pristine.insert(key, implementation.read().unwrap().clone());
        functions.push((key, sig.clone()));
      }
    }
    let mut canonical: HashMap<usize, Arc<RwLock<AbstractFunctionSignature>>> =
      HashMap::new();
    for (sig, implementation) in composites.iter() {
      canonical
        .entry(ptr_key(implementation))
        .or_insert_with(|| sig.clone());
    }
    for v in self.top_level_vars.iter_mut() {
      if let Some(value) = &mut v.value {
        value.snapshot_types();
      }
    }
    // Classification fixpoint: return shapes + boxed-parameter clones.
    let mut return_shapes: HashMap<usize, Shape> = HashMap::new();
    let mut clones: HashMap<CloneKey, Arc<RwLock<AbstractFunctionSignature>>> =
      HashMap::new();
    let mut local_boxed_params: HashMap<LocalLambdaKey, BTreeSet<usize>> =
      HashMap::new();
    loop {
      let mut changed = false;
      let mut requests = vec![];
      let mut local_requests = vec![];
      for (key, _) in functions.iter() {
        let mut body = pristine[key].clone();
        let outcome = box_function_body(
          &mut body,
          &BoxingContext {
            types: &types,
            return_shapes: &return_shapes,
            clones: &clones,
            local_boxed_params: &local_boxed_params,
            global_vars: &global_vars,
            names: &self.names,
            canonical: &canonical,
          },
          *key,
        );
        if return_shapes.get(key) != Some(&outcome.shape) {
          return_shapes.insert(*key, outcome.shape);
          changed = true;
        }
        requests.extend(outcome.requests);
        local_requests.extend(outcome.local_requests);
      }
      for v in self.top_level_vars.iter() {
        let Some(value) = &v.value else { continue };
        let outcome = box_global_initializer(
          &mut value.clone(),
          &v.var_type,
          &BoxingContext {
            types: &types,
            return_shapes: &return_shapes,
            clones: &clones,
            local_boxed_params: &local_boxed_params,
            global_vars: &global_vars,
            names: &self.names,
            canonical: &canonical,
          },
        );
        requests.extend(outcome.requests);
        local_requests.extend(outcome.local_requests);
      }
      for (callee, params, box_return) in requests {
        let Some(callee_key) = implementation_key(&callee) else {
          continue;
        };
        if clones.contains_key(&(callee_key, params.clone(), box_return)) {
          continue;
        }
        let Some(template) = pristine.get(&callee_key) else {
          continue;
        };
        let (clone_signature, clone_implementation) = self
          .boxed_parameter_clone(
            &callee, template, &params, box_return, &types,
          );
        let clone_key = ptr_key(&clone_implementation);
        canonical.insert(clone_key, clone_signature.clone());
        pristine
          .insert(clone_key, clone_implementation.read().unwrap().clone());
        functions.push((clone_key, clone_signature.clone()));
        clones.insert((callee_key, params, box_return), clone_signature);
        changed = true;
      }
      for (lambda, param) in local_requests {
        changed |= local_boxed_params.entry(lambda).or_default().insert(param);
      }
      if !changed {
        break;
      }
    }
    // Final rewrite.
    let mut implementations: HashMap<usize, Arc<RwLock<TopLevelFunction>>> =
      HashMap::new();
    for f in self.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(implementation) =
        &f.read().unwrap().implementation
      {
        implementations.insert(ptr_key(implementation), implementation.clone());
      }
    }
    for (key, signature) in functions.iter() {
      let mut body = pristine[key].clone();
      let outcome = box_function_body(
        &mut body,
        &BoxingContext {
          types: &types,
          return_shapes: &return_shapes,
          clones: &clones,
          local_boxed_params: &local_boxed_params,
          global_vars: &global_vars,
          names: &self.names,
          canonical: &canonical,
        },
        *key,
      );
      for e in outcome.errors {
        errors.log(e);
      }
      if let Some(implementation) = implementations.get(key) {
        *implementation.write().unwrap() = body;
      }
      let shape = &return_shapes[key];
      let mut signature = signature.write().unwrap();
      if let AbstractType::Type(t) = &mut signature.return_type {
        let mut ts = TypeState::Known(t.clone());
        apply_shape(&mut ts, shape, &types);
        *t = ts.unwrap_known();
      }
    }
    // Top-level var initializers.
    let mut top_level_vars = std::mem::take(&mut self.top_level_vars);
    for v in top_level_vars.iter_mut() {
      let Some(value) = &mut v.value else { continue };
      let outcome = box_global_initializer(
        value,
        &v.var_type,
        &BoxingContext {
          types: &types,
          return_shapes: &return_shapes,
          clones: &clones,
          local_boxed_params: &local_boxed_params,
          global_vars: &global_vars,
          names: &self.names,
          canonical: &canonical,
        },
      );
      for e in outcome.errors {
        errors.log(e);
      }
    }
    self.top_level_vars = top_level_vars;
  }

  /// A copy of `callee` whose parameters at `params` (static higher-order
  /// parameters receiving a boxed value at some call site, or every
  /// function-typed parameter of a function stored as a value) are boxed,
  /// along with its return when `box_return` (a stored function returning a
  /// function). Like a
  /// closure passed to a higher-order function, such a parameter is passed
  /// by mutable reference (so the caller's value advances when the callee
  /// calls it) — but, again like higher-order inlining, it only becomes a
  /// reference after the reference validations, in
  /// `defunctionalize_boxed_functions`: until then it's an owned value, so a
  /// lambda capturing it captures a copy.
  fn boxed_parameter_clone(
    &mut self,
    callee: &Arc<RwLock<AbstractFunctionSignature>>,
    template: &TopLevelFunction,
    params: &[usize],
    box_return: bool,
    types: &TypeBoxer,
  ) -> (
    Arc<RwLock<AbstractFunctionSignature>>,
    Arc<RwLock<TopLevelFunction>>,
  ) {
    let original = callee.read().unwrap().clone();
    let mut implementation = template.derived_from();
    implementation
      .expression
      .data
      .kind
      .with_dereferenced_mut(|ts| {
        if let TypeState::Known(Type::Function(sig)) = ts {
          for i in params {
            types.ts_top(&mut sig.args[*i].0.var_type.kind);
          }
          if box_return {
            types.ts_top(&mut sig.return_type.kind);
          }
        }
      });
    let mut arg_types = original.arg_types.clone();
    for i in params {
      types.abstract_top(&mut arg_types[*i].0);
    }
    let mut return_type = original.return_type.clone();
    if box_return {
      types.abstract_top(&mut return_type);
    }
    let name = self
      .names
      .write()
      .unwrap()
      .gensym(&format!("{}_fnvalarg", original.name));
    let implementation = Arc::new(RwLock::new(implementation));
    let signature = self.add_abstract_function(Arc::new(RwLock::new(
      AbstractFunctionSignature {
        name,
        generic_args: original.generic_args.clone(),
        arg_types,
        return_type,
        implementation: FunctionImplementationKind::Composite(
          implementation.clone(),
        ),
        associative: original.associative,
        captured_scope: None,
        entry_point: None,
      },
    )));
    (signature, implementation)
  }
}

// ===========================================================================
// Defunctionalization, part 1: flow analysis
//
// Every boxed position in every function *instance* gets a union-find node
// holding the set of functions that can inhabit it (with, for each closure
// member, a skeleton of its scope). Flows (bindings, assignments, argument /
// parameter and return / result pairs, array elements, struct fields, enum
// payloads, `if`/`match` arms) unify skeletons, so a position's final set is
// every function that can reach it. Context sensitivity comes from
// instantiation: a function whose signature involves boxed values is
// re-analyzed as a fresh *instance* at every call site (the call graph is a
// DAG, so this terminates), making its sets per-usage; functions whose
// signatures don't involve boxed values have a single shared instance.
// ===========================================================================

type NodeId = usize;

/// The boxed positions of a value, mirroring its type's structure. A static
/// closure value's skeleton is its scope struct's.
#[derive(Clone, Debug)]
enum Skel {
  Leaf,
  Box(NodeId),
  Array(Box<Skel>),
  Struct(Vec<Skel>),
  Enum(Vec<Skel>),
}

#[derive(Clone)]
struct Member {
  function: Arc<RwLock<AbstractFunctionSignature>>,
  /// The member's static function type, as it was wrapped.
  static_type: Type,
  /// The closure scope skeleton, for a closure member.
  payload: Option<Skel>,
}

#[derive(Default)]
struct NodeData {
  members: BTreeMap<String, Member>,
  apply_sites: Vec<usize>,
}

enum InstanceBody {
  Function(TopLevelFunction),
  /// A top-level var's initializer, with the var's name.
  GlobalInit(Arc<str>, TypedExp),
  Taken,
}

struct Instance {
  function: Option<Arc<RwLock<AbstractFunctionSignature>>>,
  body: InstanceBody,
  slots: Vec<Skel>,
  params: Vec<Skel>,
  ret: Skel,
  /// slot of a composite call -> callee instance
  calls: HashMap<u32, usize>,
  /// slot of a `$fnbox-apply` call -> apply site
  applies: HashMap<u32, usize>,
  /// slot of a host-invoked closure argument -> closure instance
  hosts: HashMap<u32, usize>,
}

struct ApplySite {
  node: NodeId,
  args: Vec<Skel>,
  result: Skel,
  /// member key -> member instance (`None` for a builtin member)
  linked: BTreeMap<String, Option<usize>>,
}

/// A composite callee's registered signature. Every reference to a composite
/// function shares its implementation Arc with exactly one registered
/// signature (inference and monomorphization attach signature copies, which
/// share the implementation; monomorphization repoints each reference at the
/// registered specialization), so the implementation is the lookup key. Names
/// can't be: overload separation renames registered signatures, not copies.
fn resolve_in_registry(
  function: &Arc<RwLock<AbstractFunctionSignature>>,
  registry_by_implementation: &HashMap<
    usize,
    Arc<RwLock<AbstractFunctionSignature>>,
  >,
) -> Arc<RwLock<AbstractFunctionSignature>> {
  let Some(key) = implementation_key(function) else {
    return function.clone();
  };
  registry_by_implementation
    .get(&key)
    .unwrap_or_else(|| {
      panic!(
        "function reference `{}` shares no implementation with a registered function",
        function.read().unwrap().name
      )
    })
    .clone()
}

fn member_key(
  function: &AbstractFunctionSignature,
  static_type: &Type,
) -> String {
  match &function.implementation {
    FunctionImplementationKind::Composite(_) => format!("fn:{}", function.name),
    _ => format!("builtin:{}:{}", function.name, type_key(static_type)),
  }
}

/// Whether values of type `t` carry boxed positions: a boxed function, an
/// aggregate containing one, or a static closure whose scope does.
fn type_involves_box(t: &Type, program: &Program) -> bool {
  match t {
    Type::BoxedFunction(_) => true,
    Type::Function(sig) => sig
      .closure_scope()
      .and_then(|scope| scope.concrete_type())
      .is_some_and(|scope| type_involves_box(&scope, program)),
    _ => any_child(t, &|child| type_involves_box(child, program)),
  }
}

fn function_involves_box(
  function: &Arc<RwLock<AbstractFunctionSignature>>,
  program: &Program,
) -> bool {
  let FunctionImplementationKind::Composite(implementation) =
    &function.read().unwrap().implementation
  else {
    return false;
  };
  let Some(Type::Function(sig)) =
    known_type(&implementation.read().unwrap().expression.data)
  else {
    return false;
  };
  sig.args.iter().any(|(a, _)| {
    known_type(&a.var_type).is_some_and(|t| type_involves_box(&t, program))
  }) || known_type(&sig.return_type)
    .is_some_and(|t| type_involves_box(&t, program))
}

struct Analysis<'p> {
  program: &'p Program,
  parent: Vec<NodeId>,
  nodes: Vec<NodeData>,
  instances: Vec<Instance>,
  sites: Vec<ApplySite>,
  pending: Vec<(usize, String)>,
  roots: HashMap<usize, usize>,
  /// Registered composite signatures by implementation key.
  registry_by_implementation:
    HashMap<usize, Arc<RwLock<AbstractFunctionSignature>>>,
  globals: HashMap<Arc<str>, Skel>,
  global_vars: HashMap<Arc<str>, Type>,
  /// Sets found to (transitively) contain a closure capturing a value of
  /// the same set.
  recursive_nodes: HashSet<NodeId>,
}

impl<'p> Analysis<'p> {
  fn resolve(
    &self,
    function: &Arc<RwLock<AbstractFunctionSignature>>,
  ) -> Arc<RwLock<AbstractFunctionSignature>> {
    resolve_in_registry(function, &self.registry_by_implementation)
  }
  fn new_node(&mut self) -> NodeId {
    self.parent.push(self.parent.len());
    self.nodes.push(NodeData::default());
    self.parent.len() - 1
  }
  fn find(&mut self, mut n: NodeId) -> NodeId {
    while self.parent[n] != n {
      self.parent[n] = self.parent[self.parent[n]];
      n = self.parent[n];
    }
    n
  }
  fn fresh(&mut self, t: &Type) -> Skel {
    if !type_involves_box(t, self.program) {
      return Skel::Leaf;
    }
    match t {
      Type::BoxedFunction(_) => Skel::Box(self.new_node()),
      Type::Array(_, inner) => {
        Skel::Array(Box::new(self.fresh(&inner.unwrap_known())))
      }
      Type::Struct(s) => Skel::Struct(
        s.fields
          .iter()
          .map(|f| self.fresh(&f.field_type.unwrap_known()))
          .collect(),
      ),
      Type::Enum(e) => Skel::Enum(
        e.variants
          .iter()
          .map(|v| self.fresh(&v.inner_type.unwrap_known()))
          .collect(),
      ),
      Type::Function(sig) => {
        match sig.closure_scope().and_then(|scope| scope.concrete_type()) {
          Some(scope) => self.fresh(&scope),
          None => Skel::Leaf,
        }
      }
      _ => Skel::Leaf,
    }
  }
  fn fresh_ts(&mut self, ts: &TypeState) -> Skel {
    match ts.try_unwrap_known() {
      Some(t) => self.fresh(&t),
      None => Skel::Leaf,
    }
  }
  fn unify(&mut self, a: &Skel, b: &Skel) {
    let mut stack = vec![(a.clone(), b.clone())];
    while let Some((a, b)) = stack.pop() {
      match (a, b) {
        (Skel::Box(x), Skel::Box(y)) => {
          let (x, y) = (self.find(x), self.find(y));
          if x == y {
            continue;
          }
          self.parent[y] = x;
          let absorbed = std::mem::take(&mut self.nodes[y]);
          for (key, member) in absorbed.members {
            match self.nodes[x].members.get(&key) {
              Some(existing) => {
                if let (Some(p), Some(q)) =
                  (existing.payload.clone(), member.payload.clone())
                {
                  stack.push((p, q));
                }
              }
              None => {
                self.nodes[x].members.insert(key, member);
              }
            }
          }
          self.nodes[x].apply_sites.extend(absorbed.apply_sites);
          self.schedule_links(x);
        }
        (Skel::Array(p), Skel::Array(q)) => stack.push((*p, *q)),
        (Skel::Struct(ps), Skel::Struct(qs))
        | (Skel::Enum(ps), Skel::Enum(qs)) => {
          for (p, q) in ps.into_iter().zip(qs.into_iter()) {
            stack.push((p, q));
          }
        }
        _ => {}
      }
    }
  }
  fn schedule_links(&mut self, node: NodeId) {
    let node = self.find(node);
    let keys: Vec<String> = self.nodes[node].members.keys().cloned().collect();
    for site in self.nodes[node].apply_sites.clone() {
      for key in keys.iter() {
        if !self.sites[site].linked.contains_key(key) {
          self.pending.push((site, key.clone()));
        }
      }
    }
  }
  fn add_member(&mut self, node: NodeId, key: String, member: Member) {
    let node = self.find(node);
    match self.nodes[node].members.get(&key) {
      Some(existing) => {
        if let (Some(p), Some(q)) =
          (existing.payload.clone(), member.payload.clone())
        {
          self.unify(&p, &q);
        }
      }
      None => {
        self.nodes[node].members.insert(key, member);
        self.schedule_links(node);
      }
    }
  }
  fn global_skel(&mut self, name: &Arc<str>) -> Skel {
    if let Some(s) = self.globals.get(name) {
      return s.clone();
    }
    let t = self.global_vars[name].clone();
    let s = self.fresh(&t);
    self.globals.insert(name.clone(), s.clone());
    s
  }
  fn root(
    &mut self,
    function: &Arc<RwLock<AbstractFunctionSignature>>,
  ) -> usize {
    let key = implementation_key(function).unwrap();
    if let Some(i) = self.roots.get(&key) {
      return *i;
    }
    self.instantiate(function, true)
  }
  /// Creates and analyzes a fresh instance of `function`.
  fn instantiate(
    &mut self,
    function: &Arc<RwLock<AbstractFunctionSignature>>,
    as_root: bool,
  ) -> usize {
    let FunctionImplementationKind::Composite(implementation) =
      function.read().unwrap().implementation.clone()
    else {
      unreachable!()
    };
    let body = implementation.read().unwrap().clone();
    let Some(Type::Function(sig)) = known_type(&body.expression.data) else {
      unreachable!()
    };
    let params: Vec<Skel> = sig
      .args
      .iter()
      .map(|(a, _)| self.fresh_ts(&a.var_type.kind))
      .collect();
    let ret = self.fresh_ts(&sig.return_type.kind);
    let index = self.instances.len();
    self.instances.push(Instance {
      function: Some(function.clone()),
      body: InstanceBody::Taken,
      slots: vec![],
      params: params.clone(),
      ret: ret.clone(),
      calls: HashMap::new(),
      applies: HashMap::new(),
      hosts: HashMap::new(),
    });
    if as_root {
      self
        .roots
        .insert(implementation_key(function).unwrap(), index);
    }
    let mut body = body;
    let mut env: HashMap<Arc<str>, Skel> = HashMap::new();
    let mut slots = vec![];
    if let ExpKind::Function(arg_names, fn_body) = &mut body.expression.kind {
      for ((name, _), p) in arg_names.iter().zip(params.iter()) {
        env.insert(name.clone(), p.clone());
      }
      let tail = self.analyze(index, fn_body, &mut env, &mut slots);
      self.unify(&tail, &ret);
    }
    self.instances[index].slots = slots;
    self.instances[index].body = InstanceBody::Function(body);
    index
  }
  fn analyze_global_init(&mut self, name: &Arc<str>, mut value: TypedExp) {
    let index = self.instances.len();
    self.instances.push(Instance {
      function: None,
      body: InstanceBody::Taken,
      slots: vec![],
      params: vec![],
      ret: Skel::Leaf,
      calls: HashMap::new(),
      applies: HashMap::new(),
      hosts: HashMap::new(),
    });
    let mut env = HashMap::new();
    let mut slots = vec![];
    let s = self.analyze(index, &mut value, &mut env, &mut slots);
    let g = self.global_skel(name);
    self.unify(&s, &g);
    self.instances[index].slots = slots;
    self.instances[index].body = InstanceBody::GlobalInit(name.clone(), value);
  }
  fn record(exp: &mut TypedExp, slots: &mut Vec<Skel>, s: Skel) -> u32 {
    let slot = slots.len() as u32;
    slots.push(s);
    exp.data.defun_slot = Some(slot);
    slot
  }
  fn analyze(
    &mut self,
    instance: usize,
    exp: &mut TypedExp,
    env: &mut HashMap<Arc<str>, Skel>,
    slots: &mut Vec<Skel>,
  ) -> Skel {
    // Reserve this node's slot first so a parent's slot precedes its
    // children's (the lowering walk only ever looks slots up by index).
    let slot = Self::record(exp, slots, Skel::Leaf);
    let t = known_type(&exp.data);
    let s = match &mut exp.kind {
      ExpKind::Name(name) => {
        if let Some(s) = env.get(name) {
          s.clone()
        } else if self.global_vars.contains_key(name) {
          self.global_skel(name)
        } else {
          t.map(|t| self.fresh(&t)).unwrap_or(Skel::Leaf)
        }
      }
      ExpKind::Let(bindings, body) => {
        for (name, _, _, value) in bindings.iter_mut() {
          let s = self.analyze(instance, value, env, slots);
          env.insert(name.clone(), s);
        }
        self.analyze(instance, body, env, slots)
      }
      ExpKind::Block(exps) => {
        let mut s = Skel::Leaf;
        for e in exps.iter_mut() {
          s = self.analyze(instance, e, env, slots);
        }
        s
      }
      ExpKind::Match(scrutinee, arms) => {
        let scrutinee_skel = self.analyze(instance, scrutinee, env, slots);
        let scrutinee_type = known_type(&scrutinee.data);
        let result = t.map(|t| self.fresh(&t)).unwrap_or(Skel::Leaf);
        for (pattern, value) in arms.iter_mut() {
          // Bind enum-payload pattern names to the payload's skeleton.
          let is_payload_pattern = matches!(&pattern.kind,
            ExpKind::Application(_, pattern_args) if pattern_args.len() == 1)
            && matches!(scrutinee_type, Some(Type::Enum(_)));
          if is_payload_pattern {
            Self::record(pattern, slots, scrutinee_skel.clone());
          }
          if is_payload_pattern
            && let ExpKind::Application(ctor, pattern_args) = &mut pattern.kind
            && let Some(Type::Enum(e)) = &scrutinee_type
          {
            let variant = enum_variant_index_of_ctor(ctor, e);
            Self::record(ctor, slots, Skel::Leaf);
            let payload = match (&scrutinee_skel, variant) {
              (Skel::Enum(vs), Some(i)) => vs[i].clone(),
              _ => self.fresh_ts(&pattern_args[0].data.kind),
            };
            if let ExpKind::Name(bound) = &pattern_args[0].kind {
              env.insert(bound.clone(), payload.clone());
            }
            Self::record(&mut pattern_args[0], slots, payload);
          } else {
            let p = self.analyze(instance, pattern, env, slots);
            self.unify(&p, &scrutinee_skel);
          }
          let v = self.analyze(instance, value, env, slots);
          self.unify(&v, &result);
        }
        result
      }
      ExpKind::Access(accessor, inner) => {
        if let Accessor::ArrayIndex(index) = accessor {
          self.analyze(instance, index, env, slots);
        }
        let inner_skel = self.analyze(instance, inner, env, slots);
        match (accessor, inner_skel, known_type(&inner.data)) {
          (
            Accessor::Field(name),
            Skel::Struct(fields),
            Some(Type::Struct(s)),
          ) => match s.fields.iter().position(|f| f.name == *name) {
            Some(i) => fields[i].clone(),
            None => Skel::Leaf,
          },
          (Accessor::ArrayIndex(_), Skel::Array(element), _) => *element,
          _ => t.map(|t| self.fresh(&t)).unwrap_or(Skel::Leaf),
        }
      }
      ExpKind::ArrayLiteral(elements) => {
        let element = match &t {
          Some(Type::Array(_, inner)) => self.fresh_ts(&inner.kind),
          _ => Skel::Leaf,
        };
        for e in elements.iter_mut() {
          let s = self.analyze(instance, e, env, slots);
          self.unify(&s, &element);
        }
        match t {
          Some(t) if type_involves_box(&t, self.program) => {
            Skel::Array(Box::new(element))
          }
          _ => Skel::Leaf,
        }
      }
      ExpKind::Return(value) => {
        let s = self.analyze(instance, value, env, slots);
        let ret = self.instances[instance].ret.clone();
        self.unify(&s, &ret);
        Skel::Leaf
      }
      ExpKind::ForLoop {
        increment_variable_initial_value_expression,
        continue_condition_expression,
        update_expression,
        body_expression,
        ..
      } => {
        self.analyze(
          instance,
          increment_variable_initial_value_expression,
          env,
          slots,
        );
        self.analyze(instance, continue_condition_expression, env, slots);
        if let Some(u) = update_expression {
          self.analyze(instance, u, env, slots);
        }
        self.analyze(instance, body_expression, env, slots);
        Skel::Leaf
      }
      ExpKind::WhileLoop {
        condition_expression,
        body_expression,
      } => {
        self.analyze(instance, condition_expression, env, slots);
        self.analyze(instance, body_expression, env, slots);
        Skel::Leaf
      }
      ExpKind::Function(_, body) => {
        // Only reachable for a stray nested lambda (all are extracted by
        // now); analyze its body for completeness.
        self.analyze(instance, body, env, slots);
        Skel::Leaf
      }
      ExpKind::Application(_, _) => {
        self.analyze_application(instance, exp, slot, env, slots)
      }
      _ => t.map(|t| self.fresh(&t)).unwrap_or(Skel::Leaf),
    };
    slots[slot as usize] = s.clone();
    s
  }
  fn analyze_application(
    &mut self,
    instance: usize,
    exp: &mut TypedExp,
    slot: u32,
    env: &mut HashMap<Arc<str>, Skel>,
    slots: &mut Vec<Skel>,
  ) -> Skel {
    let t = known_type(&exp.data);
    let ExpKind::Application(f, args) = &mut exp.kind else {
      unreachable!()
    };
    let callee_type = known_type(&f.data);
    match callee_type {
      Some(Type::Array(_, _)) => {
        let array = self.analyze(instance, f, env, slots);
        for a in args.iter_mut() {
          self.analyze(instance, a, env, slots);
        }
        match array {
          Skel::Array(element) => *element,
          _ => t.map(|t| self.fresh(&t)).unwrap_or(Skel::Leaf),
        }
      }
      Some(Type::Function(sig)) => {
        Self::record(f, slots, Skel::Leaf);
        let arg_skels: Vec<Skel> = args
          .iter_mut()
          .map(|a| self.analyze(instance, a, env, slots))
          .collect();
        let Some(ancestor) = sig.abstract_ancestor.clone() else {
          return t.map(|t| self.fresh(&t)).unwrap_or(Skel::Leaf);
        };
        let (name, implementation) = {
          let a = ancestor.read().unwrap();
          (a.name.clone(), a.implementation.clone())
        };
        if &*name == FNBOX_MAKE || &*name == FNBOX_BORROW {
          let node = self.new_node();
          stamp_member_ancestor(&mut args[0], self.program);
          let static_type = known_type(&args[0].data).unwrap();
          let Type::Function(member_sig) = &static_type else {
            unreachable!()
          };
          if let Some(member_function) = member_sig.abstract_ancestor.clone() {
            let member_function = self.resolve(&member_function);
            let has_scope =
              member_function.read().unwrap().captured_scope.is_some();
            let key =
              member_key(&member_function.read().unwrap(), &static_type);
            let payload = if has_scope {
              Some(match &arg_skels[0] {
                Skel::Leaf => {
                  let scope = member_sig
                    .closure_scope()
                    .and_then(|scope| scope.concrete_type())
                    .unwrap();
                  self.fresh(&scope)
                }
                s => s.clone(),
              })
            } else {
              None
            };
            self.add_member(
              node,
              key,
              Member {
                function: member_function,
                static_type: static_type.clone(),
                payload,
              },
            );
          }
          return Skel::Box(node);
        }
        if &*name == FNBOX_APPLY {
          let Skel::Box(node) = arg_skels[0].clone() else {
            return t.map(|t| self.fresh(&t)).unwrap_or(Skel::Leaf);
          };
          let result = t.map(|t| self.fresh(&t)).unwrap_or(Skel::Leaf);
          let site = self.sites.len();
          self.sites.push(ApplySite {
            node,
            args: arg_skels[1..].to_vec(),
            result: result.clone(),
            linked: Default::default(),
          });
          let rep = self.find(node);
          self.nodes[rep].apply_sites.push(site);
          self.schedule_links(rep);
          self.instances[instance].applies.insert(slot, site);
          return result;
        }
        match implementation {
          FunctionImplementationKind::StructConstructor => {
            // A struct construction — or a closure's scope construction,
            // whose value (the closure) is represented by its scope.
            if t
              .as_ref()
              .is_some_and(|t| type_involves_box(t, self.program))
            {
              Skel::Struct(arg_skels)
            } else {
              Skel::Leaf
            }
          }
          FunctionImplementationKind::EnumConstructor(variant) => {
            let result =
              t.clone().map(|t| self.fresh(&t)).unwrap_or(Skel::Leaf);
            if let (Skel::Enum(payloads), Some(Type::Enum(e))) = (&result, &t)
              && let Some(i) = e.variants.iter().position(|v| v.name == variant)
            {
              let payload = payloads[i].clone();
              self.unify(&payload, &arg_skels[0]);
            }
            result
          }
          FunctionImplementationKind::Composite(_) => {
            let ancestor = self.resolve(&ancestor);
            let callee = if function_involves_box(&ancestor, self.program) {
              self.instantiate(&ancestor, false)
            } else {
              self.root(&ancestor)
            };
            let params = self.instances[callee].params.clone();
            for (a, p) in arg_skels.iter().zip(params.iter()) {
              self.unify(a, p);
            }
            self.instances[instance].calls.insert(slot, callee);
            self.instances[callee].ret.clone()
          }
          FunctionImplementationKind::Builtin { .. } => {
            // Host-invoked closures (`spawn-window`, `dispatch-*`,
            // `start-audio`) are instantiated at the handoff site.
            for (a, s) in args.iter().zip(arg_skels.iter()) {
              if let Some(Type::Function(arg_sig)) = known_type(&a.data)
                && let Some(host) = arg_sig.abstract_ancestor.clone()
                && matches!(
                  host.read().unwrap().implementation,
                  FunctionImplementationKind::Composite(_)
                )
              {
                let host = self.resolve(&host);
                let host_instance =
                  if function_involves_box(&host, self.program) {
                    self.instantiate(&host, false)
                  } else {
                    self.root(&host)
                  };
                if let Some(scope_param) =
                  self.instances[host_instance].params.last().cloned()
                  && host.read().unwrap().captured_scope.is_some()
                {
                  self.unify(s, &scope_param);
                }
                if let Some(arg_slot) = a.data.defun_slot {
                  self.instances[instance]
                    .hosts
                    .insert(arg_slot, host_instance);
                }
              }
            }
            let result =
              t.as_ref().map(|t| self.fresh(t)).unwrap_or(Skel::Leaf);
            // A builtin can only move a function value into its result or
            // through a mutable-reference parameter. One with neither
            // (`array-length`, a comparison) only reads its operands, so it
            // relates no boxed positions.
            let moves_values = t
              .as_ref()
              .is_some_and(|t| type_involves_box(t, self.program))
              || ancestor.read().unwrap().arg_types.iter().any(
                |(_, ownership)| *ownership == Ownership::MutableReference,
              );
            if !moves_values {
              return result;
            }
            // Any other builtin moving boxed values (assignment, `push`,
            // `into-dynamic-array`, ...): conservatively unify every boxed
            // position of matching signature among its operands and result.
            let mut boxes: Vec<(FunctionSignature, NodeId)> = vec![];
            let arg_types: Vec<Option<Type>> =
              args.iter().map(|a| known_type(&a.data)).collect();
            for (s, at) in arg_skels.iter().zip(arg_types.iter()) {
              if let Some(at) = at {
                collect_boxes(s, at, &mut boxes);
              }
            }
            if let Some(t) = known_type(&exp.data) {
              collect_boxes(&result, &t, &mut boxes);
            }
            let mut by_signature: Vec<(FunctionSignature, NodeId)> = vec![];
            for (signature, node) in boxes {
              match by_signature.iter().find(|(existing, _)| {
                same_boxed_signature(existing, &signature)
              }) {
                Some((_, existing)) => {
                  let (a, b) = (Skel::Box(*existing), Skel::Box(node));
                  self.unify(&a, &b);
                }
                None => by_signature.push((signature, node)),
              }
            }
            result
          }
        }
      }
      _ => {
        self.analyze(instance, f, env, slots);
        for a in args.iter_mut() {
          self.analyze(instance, a, env, slots);
        }
        t.map(|t| self.fresh(&t)).unwrap_or(Skel::Leaf)
      }
    }
  }
  /// Links member `key` of the apply site's node: instantiates the member
  /// with the site's arguments / result (and, for a closure, its scope).
  fn link(&mut self, site: usize, key: String) {
    if self.sites[site].linked.contains_key(&key) {
      return;
    }
    let node = self.find(self.sites[site].node);
    let Some(member) = self.nodes[node].members.get(&key).cloned() else {
      return;
    };
    let composite = matches!(
      member.function.read().unwrap().implementation,
      FunctionImplementationKind::Composite(_)
    );
    if !composite {
      self.sites[site].linked.insert(key, None);
      return;
    }
    // A closure whose scope (transitively) holds a value of the very set
    // it's a member of would need an infinitely nested representation.
    if let Some(payload) = &member.payload
      && self.skel_reaches(payload, node, &mut HashSet::new())
    {
      self.recursive_nodes.insert(node);
      self.sites[site].linked.insert(key, None);
      return;
    }
    // Mark linked before instantiating: instantiation can trigger further
    // links of this same site through unification.
    self.sites[site].linked.insert(key.clone(), None);
    let member_instance =
      if function_involves_box(&member.function, self.program) {
        self.instantiate(&member.function, false)
      } else {
        self.root(&member.function)
      };
    self.sites[site].linked.insert(key, Some(member_instance));
    let params = self.instances[member_instance].params.clone();
    let args = self.sites[site].args.clone();
    for (a, p) in args.iter().zip(params.iter()) {
      self.unify(a, p);
    }
    if let (Some(payload), Some(scope_param)) = (&member.payload, params.last())
    {
      self.unify(payload, scope_param);
    }
    let result = self.sites[site].result.clone();
    let ret = self.instances[member_instance].ret.clone();
    self.unify(&result, &ret);
  }
  /// Whether `target`'s set is reachable from `s` through boxed positions
  /// and closure scopes.
  fn skel_reaches(
    &mut self,
    s: &Skel,
    target: NodeId,
    visited: &mut HashSet<NodeId>,
  ) -> bool {
    match s {
      Skel::Leaf => false,
      Skel::Box(n) => {
        let n = self.find(*n);
        let target = self.find(target);
        if n == target {
          return true;
        }
        if !visited.insert(n) {
          return false;
        }
        let payloads: Vec<Skel> = self.nodes[n]
          .members
          .values()
          .filter_map(|m| m.payload.clone())
          .collect();
        payloads
          .iter()
          .any(|p| self.skel_reaches(p, target, visited))
      }
      Skel::Array(e) => self.skel_reaches(e, target, visited),
      Skel::Struct(fs) | Skel::Enum(fs) => {
        fs.iter().any(|f| self.skel_reaches(f, target, visited))
      }
    }
  }
  fn run(&mut self) {
    while let Some((site, key)) = self.pending.pop() {
      self.link(site, key);
    }
  }
}

/// Every boxed node in a skeleton, with its boxed signature.
fn collect_boxes(
  s: &Skel,
  t: &Type,
  out: &mut Vec<(FunctionSignature, NodeId)>,
) {
  match (s, t) {
    (Skel::Box(n), Type::BoxedFunction(sig)) => {
      out.push(((**sig).clone(), *n));
    }
    (Skel::Array(e), Type::Array(_, inner)) => {
      collect_boxes(e, &inner.unwrap_known(), out)
    }
    (Skel::Struct(fs), Type::Struct(st)) => {
      for (f, sf) in fs.iter().zip(st.fields.iter()) {
        collect_boxes(f, &sf.field_type.unwrap_known(), out);
      }
    }
    (Skel::Enum(vs), Type::Enum(e)) => {
      for (v, ev) in vs.iter().zip(e.variants.iter()) {
        collect_boxes(v, &ev.inner_type.unwrap_known(), out);
      }
    }
    _ => {}
  }
}

/// Whether two boxed function values' signatures match: the same parameter
/// ownerships and types, and the same return type. Only positions with
/// matching signatures can hold the same functions. (`FunctionSignature`'s
/// own equality also compares how each parameter was declared, which a
/// caller can't observe.)
fn same_boxed_signature(a: &FunctionSignature, b: &FunctionSignature) -> bool {
  a.args.len() == b.args.len()
    && a.args.iter().zip(&b.args).all(|((a, _), (b, _))| {
      a.var_type.ownership == b.var_type.ownership
        && a.var_type.kind == b.var_type.kind
    })
    && a.return_type.kind == b.return_type.kind
}

/// Makes sure a wrapped function value's static type names its function.
/// Inference can leave a top-level function reference without its ancestor
/// (e.g. when its type was unified from a sibling array element, or in a
/// top-level var initializer), so a bare name is resolved by lookup.
fn stamp_member_ancestor(value: &mut TypedExp, program: &Program) {
  let ExpKind::Name(name) = &value.kind else {
    return;
  };
  let missing = matches!(known_type(&value.data),
    Some(Type::Function(sig)) if sig.abstract_ancestor.is_none());
  if !missing {
    return;
  }
  let Some(candidates) = program.abstract_functions.get(name) else {
    return;
  };
  let Some(Type::Function(value_sig)) = known_type(&value.data) else {
    return;
  };
  let value_args: Vec<Option<Type>> = value_sig
    .args
    .iter()
    .map(|(a, _)| known_type(&a.var_type))
    .collect();
  let chosen = candidates
    .iter()
    .find(|c| {
      let c = c.read().unwrap();
      c.arg_types.len() == value_args.len()
        && c.arg_types.iter().zip(value_args.iter()).all(
          |((at, _), vt)| match (at, vt) {
            (AbstractType::Type(t), Some(v)) => t == v,
            _ => true,
          },
        )
    })
    .or_else(|| candidates.first())
    .cloned();
  value.data.kind.with_dereferenced_mut(|ts| {
    if let TypeState::Known(Type::Function(sig)) = ts {
      sig.abstract_ancestor = chosen;
    }
  });
}

fn enum_variant_index_of_ctor(ctor: &TypedExp, e: &Enum) -> Option<usize> {
  let Some(Type::Function(sig)) = known_type(&ctor.data) else {
    return None;
  };
  let ancestor = sig.abstract_ancestor.as_ref()?;
  let FunctionImplementationKind::EnumConstructor(variant) =
    &ancestor.read().unwrap().implementation
  else {
    return None;
  };
  e.variants.iter().position(|v| v.name == *variant)
}

// ===========================================================================
// Defunctionalization, part 2: representations, naming, and lowering
// ===========================================================================

/// A generated tagged union for a boxed position holding two or more
/// functions. Variant `i` carries member `i`'s (specialized) closure scope,
/// or nothing for a scopeless member.
struct UnionEnum {
  enum_type: Type,
  definition: AbstractEnum,
  /// (member key, variant name, payload type, constructor)
  variants: Vec<(
    String,
    Arc<str>,
    Type,
    Option<Arc<RwLock<AbstractFunctionSignature>>>,
  )>,
}

/// A struct / enum definition specialized for one assignment of
/// representations to its boxed fields / payloads.
struct SpecializedEnum {
  enum_type: Type,
  /// new variant names, by variant index
  variant_names: Vec<Arc<str>>,
  constructors: Vec<Option<Arc<RwLock<AbstractFunctionSignature>>>>,
}

/// A match arm's payload binding aliased to the payload stored in a place
/// scrutinee (see `Lowering::alias_payload_binding`).
struct PayloadAlias {
  scrutinee: TypedExp,
  /// The pattern's variant constructor (callee expression).
  constructor: TypedExp,
  bound: Arc<str>,
  payload_type: Type,
  enum_type: Type,
}

impl PayloadAlias {
  /// How a helper that only reads the scrutinee takes it: by reference when
  /// the scrutinee is rooted at a reference parameter (which can't be passed
  /// as an owned value), by value otherwise.
  fn read_ownership(&self) -> Ownership {
    if self.root().data.ownership == Ownership::Owned {
      Ownership::Owned
    } else {
      Ownership::Reference
    }
  }
  /// The variable the scrutinee place is rooted at.
  fn root(&self) -> &TypedExp {
    let mut root = &self.scrutinee;
    loop {
      match &root.kind {
        ExpKind::Access(_, inner) => root = inner,
        ExpKind::Application(f, _)
          if matches!(known_type(&f.data), Some(Type::Array(_, _))) =>
        {
          root = f
        }
        _ => break,
      }
    }
    root
  }
}

/// The lowered form of one distinct function instance.
struct LoweredFunction {
  name: Arc<str>,
  signature: Arc<RwLock<AbstractFunctionSignature>>,
  params: Vec<Type>,
  ret: Type,
  /// The instance whose body is lowered to produce this function.
  representative: usize,
}

fn ownership_key(o: Ownership) -> &'static str {
  match o {
    Ownership::MutableReference => "&mut",
    Ownership::Owned => "",
    _ => "&",
  }
}

fn type_key(t: &Type) -> String {
  match t {
    Type::Unit => "()".into(),
    Type::F32 => "f32".into(),
    Type::I32 => "i32".into(),
    Type::U32 => "u32".into(),
    Type::Bool => "bool".into(),
    Type::String => "String".into(),
    Type::Skolem(n, _) => format!("'{n}"),
    Type::Array(size, inner) => format!(
      "[{}:{}]",
      size.as_ref().map(|s| s.compile_type()).unwrap_or_default(),
      type_key(&inner.unwrap_known())
    ),
    Type::Struct(s) => {
      let fields: Vec<String> = s
        .fields
        .iter()
        .map(|f| type_key(&f.field_type.unwrap_known()))
        .collect();
      format!("{}{{{}}}", s.name, fields.join(","))
    }
    Type::Enum(e) => {
      let variants: Vec<String> = e
        .variants
        .iter()
        .map(|v| type_key(&v.inner_type.unwrap_known()))
        .collect();
      format!("{}<{}>", e.name, variants.join(","))
    }
    Type::Function(sig) => {
      let ancestor = sig
        .abstract_ancestor
        .as_ref()
        .map(|a| {
          let a = a.read().unwrap();
          format!(
            "{}#{}",
            a.name,
            a.captured_scope
              .as_ref()
              .map(|s| s.name.0.to_string())
              .unwrap_or_default()
          )
        })
        .unwrap_or_default();
      format!("fn:{ancestor}")
    }
    Type::BoxedFunction(_) => "box".into(),
  }
}

/// Boxed positions with no flow information (never assigned) have no
/// function to represent: they become unit.
fn erase_boxes(t: &Type) -> Type {
  let mut t = t.clone();
  t.walk_mut(&|t| {
    if let Type::BoxedFunction(_) = t {
      *t = Type::Unit;
    }
  });
  t
}

struct Lowering<'p> {
  analysis: Analysis<'p>,
  errors: Vec<CompileError>,
  resolving: Vec<NodeId>,
  union_enums: HashMap<String, UnionEnum>,
  struct_specs: HashMap<String, Type>,
  used_group_names: HashSet<String>,
  new_structs: Vec<AbstractStruct>,
  enum_specs: HashMap<String, SpecializedEnum>,
  new_enums: Vec<AbstractEnum>,
  new_functions: Vec<Arc<RwLock<AbstractFunctionSignature>>>,
  /// (closure fn name, scope representation key) -> the signature closure
  /// values of that representation are typed with
  closure_signatures:
    HashMap<(Arc<str>, String), Arc<RwLock<AbstractFunctionSignature>>>,
  adopted_closure_signatures: HashSet<(Arc<str>, String)>,
  instance_keys: HashMap<usize, String>,
  lowered: HashMap<String, LoweredFunction>,
  used_function_names: HashSet<Arc<str>>,
  dispatchers: HashMap<String, Arc<RwLock<AbstractFunctionSignature>>>,
  /// Whether each generated dispatcher takes its union by reference (some
  /// member mutates its scope), by dispatcher name.
  dispatcher_by_reference: HashMap<Arc<str>, bool>,
  /// By-value twins of lowered closures that don't mutate their scope, by
  /// the lowered closure's name (see `by_value_twin`).
  by_value_twins: HashMap<Arc<str>, Arc<RwLock<AbstractFunctionSignature>>>,
  /// The twins' signatures and bodies, added to the program with the
  /// lowered clones.
  by_value_twin_outputs:
    Vec<(Arc<RwLock<AbstractFunctionSignature>>, TopLevelFunction)>,
  /// The instance each lowered function name was produced from.
  instance_by_name: HashMap<Arc<str>, usize>,
  /// Payload getters, by (enum name, variant name, whether the scrutinee is
  /// taken by reference).
  payload_getters:
    HashMap<(Arc<str>, Arc<str>, bool), Arc<RwLock<AbstractFunctionSignature>>>,
  /// Lowered bodies, by instance (memoized: dispatcher generation needs a
  /// member's lowered body to tell whether it mutates its scope).
  lowered_bodies: HashMap<usize, TopLevelFunction>,
  root_instances: HashSet<usize>,
}

impl<'p> Lowering<'p> {
  fn program(&self) -> &'p Program {
    self.analysis.program
  }
  fn gensym(&self, base: &str) -> Arc<str> {
    self.program().names.write().unwrap().gensym(base)
  }
  fn members_of(&mut self, n: NodeId) -> Vec<(String, Member)> {
    let n = self.analysis.find(n);
    self.analysis.nodes[n]
      .members
      .iter()
      .map(|(k, m)| (k.clone(), m.clone()))
      .collect()
  }
  /// The representation of a boxed position.
  fn box_rep(&mut self, n: NodeId, source: &SourceTrace) -> Type {
    let n = self.analysis.find(n);
    if self.resolving.contains(&n) {
      self.errors.push(CompileError::new(
        CompileErrorKind::RecursiveFunctionValue,
        source.clone(),
      ));
      return Type::Unit;
    }
    let members = self.members_of(n);
    match members.len() {
      0 => Type::Unit,
      1 if members[0].1.payload.is_none() => Type::Unit,
      1 => {
        self.resolving.push(n);
        let t = self.payload_rep(&members[0].1, source);
        self.resolving.pop();
        t
      }
      _ => {
        self.resolving.push(n);
        let key = self.union_key(&members, source);
        self.resolving.pop();
        self.union_enums[&key].enum_type.clone()
      }
    }
  }
  fn payload_rep(&mut self, member: &Member, source: &SourceTrace) -> Type {
    let scope = member
      .function
      .read()
      .unwrap()
      .captured_scope
      .clone()
      .unwrap();
    let scope_type = scope.concrete_type().unwrap();
    self.rep(&scope_type, member.payload.as_ref().unwrap(), source)
  }
  fn union_key(
    &mut self,
    members: &[(String, Member)],
    source: &SourceTrace,
  ) -> String {
    // A closure whose scope carries nothing (every capture lowered to
    // unit) gets a unit variant: unit-like scope structs are dropped from
    // the program, so no payload can name one.
    let payloads: Vec<Type> = members
      .iter()
      .map(|(_, m)| {
        if m.payload.is_some() {
          let payload = self.payload_rep(m, source);
          if payload.is_unitlike(&mut self.program().names.write().unwrap()) {
            Type::Unit
          } else {
            payload
          }
        } else {
          Type::Unit
        }
      })
      .collect();
    let key: String = members
      .iter()
      .zip(payloads.iter())
      .map(|((k, _), p)| format!("{k}={}", type_key(p)))
      .collect::<Vec<_>>()
      .join("|");
    if self.union_enums.contains_key(&key) {
      return key;
    }
    let enum_name = self.gensym("FnUnion");
    let mut variants = vec![];
    for ((member_key, member), payload) in members.iter().zip(payloads.iter()) {
      let short = member.function.read().unwrap().name.clone();
      let variant_name = self.gensym(&format!("{enum_name}_{short}"));
      variants.push((member_key.clone(), variant_name, payload.clone(), None));
    }
    let definition = AbstractEnum {
      name: (enum_name.clone(), SourceTrace::empty()),
      filled_generics: HashMap::new(),
      generic_args: vec![],
      variants: variants
        .iter()
        .map(|(_, name, payload, _)| AbstractEnumVariant {
          name: name.clone(),
          source: SourceTrace::empty(),
          inner_type: AbstractType::Type(payload.clone()),
        })
        .collect(),
      abstract_ancestor: None,
      source_trace: SourceTrace::empty(),
    };
    let definition_arc = Arc::new(definition.clone());
    let enum_type = Type::Enum(Enum {
      name: enum_name.clone(),
      variants: variants
        .iter()
        .map(|(_, name, payload, _)| EnumVariant {
          name: name.clone(),
          inner_type: TypeState::Known(payload.clone()).into(),
        })
        .collect(),
      abstract_ancestor: definition_arc.clone(),
    });
    for (_, name, payload, ctor) in variants.iter_mut() {
      if *payload != Type::Unit {
        let signature = enum_constructor(
          name,
          payload,
          AbstractType::AbstractEnum(definition_arc.clone()),
        );
        self.new_functions.push(signature.clone());
        *ctor = Some(signature);
      }
    }
    self.union_enums.insert(
      key.clone(),
      UnionEnum {
        enum_type,
        definition,
        variants,
      },
    );
    key
  }
  /// The representation of a value of type `t` with skeleton `s`.
  fn rep(&mut self, t: &Type, s: &Skel, source: &SourceTrace) -> Type {
    if !type_involves_box(t, self.program()) {
      return t.clone();
    }
    match (t, s) {
      (Type::BoxedFunction(_), Skel::Box(n)) => self.box_rep(*n, source),
      (Type::Array(size, inner), Skel::Array(element)) => {
        let element = self.rep(&inner.unwrap_known(), element, source);
        Type::Array(size.clone(), Box::new(TypeState::Known(element).into()))
      }
      (Type::Struct(st), Skel::Struct(fields)) => {
        self.struct_rep(st, fields, source)
      }
      (Type::Enum(e), Skel::Enum(payloads)) => {
        self.enum_rep(e, payloads, source).enum_type.clone()
      }
      (Type::Function(sig), Skel::Struct(fields)) => {
        self.closure_rep(sig, fields, source)
      }
      _ => erase_boxes(t),
    }
  }
  fn struct_rep(
    &mut self,
    st: &Struct,
    fields: &[Skel],
    source: &SourceTrace,
  ) -> Type {
    let field_reps: Vec<Type> = st
      .fields
      .iter()
      .zip(fields.iter())
      .map(|(f, s)| self.rep(&f.field_type.unwrap_known(), s, source))
      .collect();
    let group = st
      .monomorphized_name(
        &mut self.program().names.write().unwrap(),
        CompilerTarget::WGSL,
      )
      .to_string();
    let key = format!(
      "{group}{{{}}}",
      field_reps
        .iter()
        .map(type_key)
        .collect::<Vec<_>>()
        .join(",")
    );
    if let Some(t) = self.struct_specs.get(&key) {
      return t.clone();
    }
    let name: Arc<str> = if self.used_group_names.insert(group.clone()) {
      group.clone().into()
    } else {
      self.gensym(&group)
    };
    let ancestor_fields = &st.abstract_ancestor.fields;
    let definition = AbstractStruct {
      name: (name.clone(), st.abstract_ancestor.source_trace.clone()),
      filled_generics: HashMap::new(),
      fields: st
        .fields
        .iter()
        .zip(field_reps.iter())
        .enumerate()
        .map(|(i, (f, rep))| AbstractStructField {
          attributes: f.attributes.clone(),
          name: f.name.clone(),
          field_type: AbstractType::Type(rep.clone()),
          source_trace: ancestor_fields
            .get(i)
            .map(|af| af.source_trace.clone())
            .unwrap_or_else(SourceTrace::empty),
        })
        .collect(),
      generic_args: vec![],
      abstract_ancestor: None,
      source_trace: st.abstract_ancestor.source_trace.clone(),
      opaque: false,
    };
    let t = Type::Struct(Struct {
      name: name.clone(),
      fields: st
        .fields
        .iter()
        .zip(field_reps.iter())
        .map(|(f, rep)| StructField {
          attributes: f.attributes.clone(),
          name: f.name.clone(),
          field_type: TypeState::Known(rep.clone()).into(),
        })
        .collect(),
      abstract_ancestor: Arc::new(definition.clone()),
    });
    self.new_structs.push(definition);
    self.struct_specs.insert(key, t.clone());
    t
  }
  fn enum_rep(
    &mut self,
    e: &Enum,
    payloads: &[Skel],
    source: &SourceTrace,
  ) -> &SpecializedEnum {
    let payload_reps: Vec<Type> = e
      .variants
      .iter()
      .zip(payloads.iter())
      .map(|(v, s)| self.rep(&v.inner_type.unwrap_known(), s, source))
      .collect();
    let group = e
      .monomorphized_name(
        &mut self.program().names.write().unwrap(),
        CompilerTarget::WGSL,
      )
      .to_string();
    let key = format!(
      "{group}<{}>",
      payload_reps
        .iter()
        .map(type_key)
        .collect::<Vec<_>>()
        .join(",")
    );
    if !self.enum_specs.contains_key(&key) {
      let was_generic = !e
        .abstract_ancestor
        .original_ancestor()
        .generic_args
        .is_empty();
      let first_in_group = self.used_group_names.insert(group.clone());
      let name: Arc<str> = if first_in_group {
        group.clone().into()
      } else {
        self.gensym(&group)
      };
      let variant_names: Vec<Arc<str>> = e
        .variants
        .iter()
        .map(|v| {
          if first_in_group && !was_generic {
            v.name.clone()
          } else {
            self.gensym(&format!("{}_{}", v.name, name))
          }
        })
        .collect();
      let definition = AbstractEnum {
        name: (name.clone(), e.abstract_ancestor.source_trace.clone()),
        filled_generics: HashMap::new(),
        generic_args: vec![],
        variants: variant_names
          .iter()
          .zip(payload_reps.iter())
          .map(|(n, p)| AbstractEnumVariant {
            name: n.clone(),
            source: SourceTrace::empty(),
            inner_type: AbstractType::Type(p.clone()),
          })
          .collect(),
        abstract_ancestor: None,
        source_trace: e.abstract_ancestor.source_trace.clone(),
      };
      let definition_arc = Arc::new(definition.clone());
      let enum_type = Type::Enum(Enum {
        name: name.clone(),
        variants: variant_names
          .iter()
          .zip(payload_reps.iter())
          .map(|(n, p)| EnumVariant {
            name: n.clone(),
            inner_type: TypeState::Known(p.clone()).into(),
          })
          .collect(),
        abstract_ancestor: definition_arc.clone(),
      });
      let constructors = variant_names
        .iter()
        .zip(payload_reps.iter())
        .map(|(n, p)| {
          (*p != Type::Unit).then(|| {
            let signature = enum_constructor(
              n,
              p,
              AbstractType::AbstractEnum(definition_arc.clone()),
            );
            self.new_functions.push(signature.clone());
            signature
          })
        })
        .collect();
      self.new_enums.push(definition);
      self.enum_specs.insert(
        key.clone(),
        SpecializedEnum {
          enum_type,
          variant_names,
          constructors,
        },
      );
    }
    &self.enum_specs[&key]
  }
  fn closure_rep(
    &mut self,
    sig: &FunctionSignature,
    fields: &[Skel],
    source: &SourceTrace,
  ) -> Type {
    let ancestor = sig.abstract_ancestor.clone().unwrap();
    let scope_type = sig
      .closure_scope()
      .and_then(|scope| scope.concrete_type())
      .unwrap();
    let scope_rep =
      self.rep(&scope_type, &Skel::Struct(fields.to_vec()), source);
    let signature = self.closure_signature(&ancestor, &scope_rep);
    let mut sig = sig.clone();
    sig.abstract_ancestor = Some(signature);
    for (a, _) in sig.args.iter_mut() {
      if let Some(t) = a.var_type.kind.try_unwrap_known() {
        a.var_type.kind = TypeState::Known(erase_boxes(&t));
      }
    }
    if let Some(t) = sig.return_type.kind.try_unwrap_known() {
      sig.return_type.kind = TypeState::Known(erase_boxes(&t));
    }
    Type::Function(Box::new(sig))
  }
  /// The signature closure values of `closure` with scope representation
  /// `scope_rep` are typed with. Starts as a placeholder that the first
  /// lowered instance of that closure with that scope adopts.
  fn closure_signature(
    &mut self,
    closure: &Arc<RwLock<AbstractFunctionSignature>>,
    scope_rep: &Type,
  ) -> Arc<RwLock<AbstractFunctionSignature>> {
    let name = closure.read().unwrap().name.clone();
    let key = (name, type_key(scope_rep));
    if let Some(s) = self.closure_signatures.get(&key) {
      return s.clone();
    }
    let mut placeholder = closure.read().unwrap().clone();
    if let Type::Struct(s) = scope_rep {
      placeholder.captured_scope = Some((*s.abstract_ancestor).clone());
      if let Some((scope_arg, _)) = placeholder.arg_types.last_mut() {
        *scope_arg = AbstractType::AbstractStruct(s.abstract_ancestor.clone());
      }
    }
    let signature = Arc::new(RwLock::new(placeholder));
    self.closure_signatures.insert(key, signature.clone());
    signature
  }
  // ---- instance keys & naming ----------------------------------------------
  fn slot_skel(&self, instance: usize, slot: Option<u32>) -> Skel {
    slot
      .and_then(|s| {
        self.analysis.instances[instance]
          .slots
          .get(s as usize)
          .cloned()
      })
      .unwrap_or(Skel::Leaf)
  }
  fn instance_param_types(&mut self, instance: usize) -> (Vec<Type>, Type) {
    let function = self.analysis.instances[instance].function.clone().unwrap();
    let FunctionImplementationKind::Composite(implementation) =
      function.read().unwrap().implementation.clone()
    else {
      unreachable!()
    };
    let Some(Type::Function(sig)) =
      known_type(&implementation.read().unwrap().expression.data)
    else {
      unreachable!()
    };
    let params = self.analysis.instances[instance].params.clone();
    let ret = self.analysis.instances[instance].ret.clone();
    let source = SourceTrace::empty();
    let param_types = sig
      .args
      .iter()
      .zip(params.iter())
      .map(|((a, _), s)| self.rep(&a.var_type.unwrap_known(), s, &source))
      .collect();
    let ret = self.rep(&sig.return_type.unwrap_known(), &ret, &source);
    (param_types, ret)
  }
  /// Parameters of an instance's function that hold a boxed function at top
  /// level — the boxed-parameter clones' parameters, passed by reference
  /// unless their representation is unit (a scopeless function has no state
  /// to advance).
  fn boxed_reference_params(&mut self, instance: usize) -> Vec<usize> {
    let Some(function) = self.analysis.instances[instance].function.clone()
    else {
      return vec![];
    };
    let FunctionImplementationKind::Composite(implementation) =
      function.read().unwrap().implementation.clone()
    else {
      return vec![];
    };
    let Some(Type::Function(sig)) =
      known_type(&implementation.read().unwrap().expression.data)
    else {
      return vec![];
    };
    let boxed: Vec<usize> = sig
      .args
      .iter()
      .enumerate()
      .filter(|(_, (a, _))| {
        matches!(known_type(&a.var_type), Some(Type::BoxedFunction(_)))
      })
      .map(|(i, _)| i)
      .collect();
    if boxed.is_empty() {
      return boxed;
    }
    let (param_types, _) = self.instance_param_types(instance);
    boxed
      .into_iter()
      .filter(|i| param_types.get(*i) != Some(&Type::Unit))
      .collect()
  }
  fn instance_key(&mut self, instance: usize) -> String {
    if let Some(k) = self.instance_keys.get(&instance) {
      return k.clone();
    }
    // Guard against re-entrance while computing (the call graph is a DAG,
    // so this only matters for pathological self-reference).
    self.instance_keys.insert(instance, format!("#{instance}"));
    let function_name = self.analysis.instances[instance]
      .function
      .as_ref()
      .map(|f| f.read().unwrap().name.clone())
      .unwrap_or_default();
    let (params, ret) = self.instance_param_types(instance);
    let mut key = format!(
      "{function_name}({})->{}",
      params.iter().map(type_key).collect::<Vec<_>>().join(","),
      type_key(&ret)
    );
    let slots = self.analysis.instances[instance].slots.clone();
    for s in slots.iter() {
      key += &self.skel_key(s);
      key.push(';');
    }
    let mut calls: Vec<(u32, usize)> = self.analysis.instances[instance]
      .calls
      .iter()
      .map(|(a, b)| (*a, *b))
      .collect();
    calls.sort();
    for (slot, callee) in calls {
      let callee_key = self.lowered_name_of(callee).to_string();
      key += &format!("|call{slot}:{callee_key}");
    }
    let mut applies: Vec<(u32, usize)> = self.analysis.instances[instance]
      .applies
      .iter()
      .map(|(a, b)| (*a, *b))
      .collect();
    applies.sort();
    for (slot, site) in applies {
      let linked: Vec<(String, Option<usize>)> = self.analysis.sites[site]
        .linked
        .iter()
        .map(|(k, v)| (k.clone(), *v))
        .collect();
      key += &format!("|apply{slot}:");
      for (k, member_instance) in linked {
        let name = member_instance
          .map(|m| self.lowered_name_of(m).to_string())
          .unwrap_or_default();
        key += &format!("{k}->{name},");
      }
    }
    let mut hosts: Vec<(u32, usize)> = self.analysis.instances[instance]
      .hosts
      .iter()
      .map(|(a, b)| (*a, *b))
      .collect();
    hosts.sort();
    for (slot, host) in hosts {
      let name = self.lowered_name_of(host).to_string();
      key += &format!("|host{slot}:{name}");
    }
    self.instance_keys.insert(instance, key.clone());
    key
  }
  fn skel_key(&mut self, s: &Skel) -> String {
    match s {
      Skel::Leaf => String::new(),
      Skel::Box(n) => {
        let t = self.box_rep(*n, &SourceTrace::empty());
        type_key(&t)
      }
      Skel::Array(e) => format!("[{}]", self.skel_key(e)),
      Skel::Struct(fs) | Skel::Enum(fs) => {
        let parts: Vec<String> = fs.iter().map(|f| self.skel_key(f)).collect();
        format!("{{{}}}", parts.join(","))
      }
    }
  }
  fn is_root(&self, instance: usize) -> bool {
    self.root_instances.contains(&instance)
  }
  /// The name an instance lowers to (roots keep their function's name).
  fn lowered_name_of(&mut self, instance: usize) -> Arc<str> {
    let function = self.analysis.instances[instance].function.clone().unwrap();
    if self.is_root(instance) {
      return function.read().unwrap().name.clone();
    }
    let key = self.instance_key(instance);
    if let Some(l) = self.lowered.get(&key) {
      return l.name.clone();
    }
    let original_name = function.read().unwrap().name.clone();
    let name = if self.used_function_names.insert(original_name.clone()) {
      original_name.clone()
    } else {
      self.gensym(&original_name)
    };
    let (params, ret) = self.instance_param_types(instance);
    let original = function.read().unwrap().clone();
    // A closure instance adopts the placeholder signature its values are
    // typed with, so value types and calls agree on one signature Arc.
    let scope_rep = original
      .captured_scope
      .as_ref()
      .and_then(|_| params.last().cloned());
    let adopted = scope_rep.as_ref().and_then(|scope_rep| {
      let key = (original_name.clone(), type_key(scope_rep));
      if !self.adopted_closure_signatures.contains(&key) {
        let signature = self.closure_signature(&function, scope_rep);
        self.adopted_closure_signatures.insert(key);
        Some(signature)
      } else {
        None
      }
    });
    let signature =
      adopted.unwrap_or_else(|| Arc::new(RwLock::new(original.clone())));
    let reference_params = self.boxed_reference_params(instance);
    {
      let mut s = signature.write().unwrap();
      s.name = name.clone();
      s.generic_args = vec![];
      let scope_index = original
        .captured_scope
        .as_ref()
        .map(|_| params.len().saturating_sub(1));
      s.arg_types = params
        .iter()
        .zip(original.arg_types.iter())
        .enumerate()
        .map(|(i, (t, (_, ownership)))| {
          let ownership = if reference_params.contains(&i) {
            Ownership::MutableReference
          } else {
            *ownership
          };
          // A closure's scope parameter is declared by its struct
          // definition, as extraction declares it (the dispatched / audio
          // closure lifts recognize it that way).
          let abstract_type = match t {
            Type::Struct(st) if Some(i) == scope_index => {
              AbstractType::AbstractStruct(st.abstract_ancestor.clone())
            }
            t => AbstractType::Type(t.clone()),
          };
          (abstract_type, ownership)
        })
        .collect();
      s.return_type = AbstractType::Type(ret.clone());
      s.entry_point = None;
      if let (Some(_), Some(Type::Struct(scope))) =
        (&original.captured_scope, &scope_rep)
      {
        s.captured_scope = Some((*scope.abstract_ancestor).clone());
      }
    }
    self.instance_by_name.insert(name.clone(), instance);
    self.lowered.insert(
      key,
      LoweredFunction {
        name: name.clone(),
        signature,
        params,
        ret,
        representative: instance,
      },
    );
    name
  }
  fn lowered_function(
    &mut self,
    instance: usize,
  ) -> (Arc<RwLock<AbstractFunctionSignature>>, Vec<Type>, Type) {
    if self.is_root(instance) {
      let function =
        self.analysis.instances[instance].function.clone().unwrap();
      let (params, ret) = self.instance_param_types(instance);
      return (function, params, ret);
    }
    self.lowered_name_of(instance);
    let key = self.instance_key(instance);
    let l = &self.lowered[&key];
    (l.signature.clone(), l.params.clone(), l.ret.clone())
  }
}

/// A constant expression of type `t`, for code that provably never runs.
fn zero_value(t: &Type, source: &SourceTrace) -> Option<TypedExp> {
  let b = ExpBuilder::at(source);
  Some(match t {
    Type::Unit => b.unit(),
    Type::F32 => b.typed(ExpKind::NumberLiteral(Number::Float(0.)), Type::F32),
    Type::I32 | Type::U32 => {
      b.typed(ExpKind::NumberLiteral(Number::Int(0)), t.clone())
    }
    Type::Bool => b.typed(ExpKind::BooleanLiteral(false), Type::Bool),
    Type::String => b.typed(ExpKind::StringLiteral("".into()), Type::String),
    Type::Array(Some(ConcreteArraySize::Literal(n)), inner) => {
      let inner = inner.unwrap_known();
      let elements = (0..*n)
        .map(|_| zero_value(&inner, source))
        .collect::<Option<Vec<_>>>()?;
      b.typed(ExpKind::ArrayLiteral(elements), t.clone())
    }
    // An empty runtime-sized array: `(into-dynamic-array [])`.
    Type::Array(Some(ConcreteArraySize::Unsized), inner) => {
      let empty = Type::Array(
        Some(ConcreteArraySize::Literal(0)),
        Box::new(TypeState::Known(inner.unwrap_known()).into()),
      );
      let signature = builtin_signature(
        "into-dynamic-array",
        vec![(empty.clone(), Ownership::Owned)],
        t.clone(),
        None,
      );
      b.call(
        &signature,
        vec![b.typed(ExpKind::ArrayLiteral(vec![]), empty)],
        t,
      )
    }
    Type::Struct(st) => {
      let fields: Vec<Type> = st
        .fields
        .iter()
        .map(|f| f.field_type.unwrap_known())
        .collect();
      let args = fields
        .iter()
        .map(|f| zero_value(f, source))
        .collect::<Option<Vec<_>>>()?;
      b.call(&struct_constructor(&st.name, &fields, t), args, t)
    }
    Type::Enum(e) => {
      // A unit variant if there is one, else the first variant around a
      // zero payload.
      if let Some(v) = e
        .variants
        .iter()
        .find(|v| v.inner_type.unwrap_known() == Type::Unit)
      {
        b.name(&v.name, t)
      } else {
        let v = e.variants.first()?;
        let payload_type = v.inner_type.unwrap_known();
        let payload = zero_value(&payload_type, source)?;
        let constructor = enum_constructor(
          &v.name,
          &payload_type,
          AbstractType::Type(t.clone()),
        );
        b.call(&constructor, vec![payload], t)
      }
    }
    _ => return None,
  })
}

/// Whether `exp` is a `($fnbox-borrow place)` lending a static closure to a
/// boxed function parameter.
pub(crate) fn is_function_value_borrow(exp: &TypedExp) -> bool {
  let ExpKind::Application(callee, _) = &exp.kind else {
    return false;
  };
  matches!(known_type(&callee.data), Some(Type::Function(sig))
    if sig.abstract_ancestor.as_ref().is_some_and(
      |a| &*a.read().unwrap().name == FNBOX_BORROW))
}

/// Whether `exp` is a place whose re-evaluation has no effects: a variable
/// with field / array-index accessors whose indices are variables or
/// literals. A `match` on such a place holding function values keeps it as
/// its scrutinee through deexpressionification, so payload bindings can
/// alias it (see `Lowering::alias_payload_binding`).
pub(crate) fn is_pure_place(exp: &TypedExp) -> bool {
  let pure_index = |i: &TypedExp| {
    matches!(i.kind, ExpKind::Name(_) | ExpKind::NumberLiteral(_))
  };
  match &exp.kind {
    ExpKind::Name(_) => true,
    ExpKind::Access(Accessor::Field(_), inner) => is_pure_place(inner),
    ExpKind::Access(Accessor::ArrayIndex(i), inner) => {
      pure_index(i) && is_pure_place(inner)
    }
    ExpKind::Application(f, args) => {
      matches!(known_type(&f.data), Some(Type::Array(_, _)))
        && args.len() == 1
        && pure_index(&args[0])
        && is_pure_place(f)
    }
    _ => false,
  }
}

/// Whether a (lowered) value of type `t` holds a function value that a
/// call could mutate: a closure scope, a function union, or an aggregate of
/// either.
fn type_carries_closure_value(t: &Type, program: &Program) -> bool {
  let is_scope_struct = |s: &Struct| {
    program.abstract_functions_iter().any(|f| {
      f.read()
        .unwrap()
        .captured_scope
        .as_ref()
        .is_some_and(|scope| scope.name.0 == s.name)
    })
  };
  matches!(t, Type::Struct(s) if is_scope_struct(s))
    || any_child(t, &|child| type_carries_closure_value(child, program))
}

fn body_involves_box(exp: &TypedExp, program: &Program) -> bool {
  let mut found = false;
  exp
    .walk(&mut |e| {
      if let Some(t) = known_type(&e.data)
        && type_involves_box(&t, program)
      {
        found = true;
      }
      if let ExpKind::Application(f, _) = &e.kind
        && let Some(Type::Function(sig)) = known_type(&f.data)
        && sig.args.iter().any(|(a, _)| {
          known_type(&a.var_type)
            .is_some_and(|t| type_involves_box(&t, program))
        })
      {
        found = true;
      }
      Ok::<bool, Never>(!found)
    })
    .unwrap();
  found
}

impl<'p> Lowering<'p> {
  fn lower(&mut self, instance: usize, exp: &mut TypedExp) {
    let skel = self.slot_skel(instance, exp.data.defun_slot);
    let source = exp.source_trace.clone();
    let original_type = known_type(&exp.data);
    match &mut exp.kind {
      ExpKind::Application(_, _) => {
        self.lower_application(instance, exp, &skel);
        return;
      }
      ExpKind::Let(bindings, body) => {
        for (_, _, kind, value) in bindings.iter_mut() {
          let involves = known_type(&value.data)
            .is_some_and(|t| type_involves_box(&t, self.program()));
          self.lower(instance, value);
          if involves {
            *kind = VariableKind::Var;
          }
        }
        self.lower(instance, body);
      }
      ExpKind::Block(exps) | ExpKind::ArrayLiteral(exps) => {
        for e in exps.iter_mut() {
          self.lower(instance, e);
        }
      }
      ExpKind::Match(scrutinee, arms) => {
        let scrutinee_type = known_type(&scrutinee.data);
        let scrutinee_skel =
          self.slot_skel(instance, scrutinee.data.defun_slot);
        self.lower(instance, scrutinee);
        let scrutinee_place =
          is_lvalue(scrutinee).then(|| (**scrutinee).clone());
        for (pattern, value) in arms.iter_mut() {
          self.lower_pattern(
            instance,
            pattern,
            &scrutinee_type,
            &scrutinee_skel,
          );
          self.lower(instance, value);
          match &scrutinee_place {
            Some(place) => self.alias_payload_binding(place, pattern, value),
            None => self.rebind_payload_mutably(pattern, value),
          }
        }
        // A place scrutinee kept by deexpressionification (for the aliasing
        // above) is bound to a temporary now, as it would have been.
        if scrutinee_place.is_some()
          && !matches!(scrutinee.kind, ExpKind::Name(_))
        {
          if let Some(t) = &original_type
            && type_involves_box(t, self.program())
          {
            exp.data.kind = TypeState::Known(self.rep(t, &skel, &source));
          }
          let ExpKind::Match(scrutinee, _) = &mut exp.kind else {
            unreachable!()
          };
          let temp = self.gensym("scrutinee");
          let temp_name = Exp {
            data: scrutinee.data.clone(),
            kind: ExpKind::Name(temp.clone()),
            source_trace: scrutinee.source_trace.clone(),
          };
          let place = std::mem::replace(scrutinee.as_mut(), temp_name);
          let source = exp.source_trace.clone();
          take(exp, |exp| {
            let_around(vec![(temp, source, VariableKind::Let, place)], exp)
          });
          return;
        }
      }
      ExpKind::Access(accessor, inner) => {
        if let Accessor::ArrayIndex(index) = accessor {
          self.lower(instance, index);
        }
        self.lower(instance, inner);
      }
      ExpKind::Return(value) => self.lower(instance, value),
      ExpKind::Function(_, body) => self.lower(instance, body),
      ExpKind::ForLoop {
        increment_variable_initial_value_expression,
        continue_condition_expression,
        update_expression,
        body_expression,
        ..
      } => {
        self.lower(instance, increment_variable_initial_value_expression);
        self.lower(instance, continue_condition_expression);
        if let Some(u) = update_expression {
          self.lower(instance, u);
        }
        self.lower(instance, body_expression);
      }
      ExpKind::WhileLoop {
        condition_expression,
        body_expression,
      } => {
        self.lower(instance, condition_expression);
        self.lower(instance, body_expression);
      }
      ExpKind::Name(name) => {
        if let Some(Type::Enum(e)) = &original_type
          && type_involves_box(original_type.as_ref().unwrap(), self.program())
          && let Some(i) = self.unit_variant_index(e, name)
          && let Skel::Enum(payloads) = &skel
        {
          let spec = self.enum_rep(e, payloads, &source);
          *name = spec.variant_names[i].clone();
        }
      }
      _ => {}
    }
    if let Some(t) = original_type
      && type_involves_box(&t, self.program())
    {
      exp.data.kind = TypeState::Known(self.rep(&t, &skel, &source));
    }
  }
  /// A match arm on a temporary (not a place) binding a payload that holds
  /// function values rebinds it as a `@var` copy: calling a closure passes it
  /// by mutable reference, and pattern bindings are immutable. There's no
  /// enum to write back into, so the copy is the value.
  fn rebind_payload_mutably(
    &mut self,
    pattern: &mut TypedExp,
    value: &mut TypedExp,
  ) {
    let ExpKind::Application(_, pattern_args) = &mut pattern.kind else {
      return;
    };
    let [bound] = pattern_args.as_mut_slice() else {
      return;
    };
    let ExpKind::Name(name) = &bound.kind else {
      return;
    };
    let Some(t) = known_type(&bound.data) else {
      return;
    };
    if !type_carries_closure_value(&t, self.program()) {
      return;
    }
    let name = name.clone();
    let payload_name = self.gensym(&format!("{name}_payload"));
    bound.kind = ExpKind::Name(payload_name.clone());
    let source = value.source_trace.clone();
    take(value, |value| {
      let_around(
        vec![(
          name,
          source.clone(),
          VariableKind::Var,
          ExpBuilder::at(&source).name(&payload_name, &t),
        )],
        value,
      )
    });
  }
  /// Makes a match arm's payload binding `f` (on a place scrutinee `s`) an
  /// alias of the payload stored in `s`, when the payload holds function
  /// values: calling a stored closure through `f` advances the closure in
  /// the enum, just as calling through a struct field does. There's no way
  /// to reference an enum's payload in place (on the GPU it's a packed word
  /// array), so each call passing `f` (or a place inside it) by mutable
  /// reference becomes a generated helper that decodes the payload from `s`,
  /// makes the call, and — when the callee can actually mutate it — writes
  /// it back, all at the moment of the call (so early exits and reads of `s`
  /// inside the arm stay consistent). If `s` no longer holds that variant
  /// when the call happens (the arm reassigned it), the call does nothing
  /// and yields a zero value. Every other read of `f` reads the current
  /// payload; binding it (`(let [g f] ...)`) copies, as ever.
  fn alias_payload_binding(
    &mut self,
    scrutinee: &TypedExp,
    pattern: &TypedExp,
    value: &mut TypedExp,
  ) {
    let ExpKind::Application(ctor, pattern_args) = &pattern.kind else {
      return;
    };
    let [bound] = pattern_args.as_slice() else {
      return;
    };
    let ExpKind::Name(bound_name) = &bound.kind else {
      return;
    };
    let (Some(payload_type), Some(enum_type)) =
      (known_type(&bound.data), known_type(&pattern.data))
    else {
      return;
    };
    if !type_carries_closure_value(&payload_type, self.program()) {
      return;
    }
    let payload = PayloadAlias {
      scrutinee: scrutinee.clone(),
      constructor: (**ctor).clone(),
      bound: bound_name.clone(),
      payload_type,
      enum_type,
    };
    let _ = value.walk_mut::<()>(&mut |e| {
      if let ExpKind::Application(callee, args) = &e.kind
        && let Some(Type::Function(sig)) = known_type(&callee.data)
      {
        let aliased: Vec<usize> = args
          .iter()
          .enumerate()
          .filter(|(i, a)| {
            sig.args.get(*i).is_some_and(|(v, _)| {
              v.var_type.ownership == Ownership::MutableReference
            }) && a.name_or_inner_accessed_name() == Some(&payload.bound)
          })
          .map(|(i, _)| i)
          .collect();
        if !aliased.is_empty() {
          let call =
            std::mem::replace(e, ExpBuilder::at(&SourceTrace::empty()).unit());
          *e = self.call_through_payload(&payload, call, &aliased);
          return Ok(true);
        }
      }
      if let ExpKind::Name(n) = &e.kind
        && *n == payload.bound
      {
        *e = self.read_payload(&payload, &e.source_trace);
        return Ok(false);
      }
      Ok(true)
    });
  }
  /// Whether a call's callee can mutate its reference parameter `i`.
  fn callee_mutates_param(
    &mut self,
    sig: &FunctionSignature,
    i: usize,
  ) -> bool {
    let Some(ancestor) = &sig.abstract_ancestor else {
      return true;
    };
    let (name, composite) = {
      let a = ancestor.read().unwrap();
      (
        a.name.clone(),
        matches!(a.implementation, FunctionImplementationKind::Composite(_)),
      )
    };
    if let Some(by_reference) = self.dispatcher_by_reference.get(&name) {
      return i == 0 && *by_reference;
    }
    if !composite {
      return true;
    }
    let Some(instance) = self.instance_by_name.get(&name).cloned() else {
      return true;
    };
    let body = self.lower_instance(instance);
    let ExpKind::Function(arg_names, fn_body) = &body.expression.kind else {
      return true;
    };
    let Some((param, _)) = arg_names.get(i) else {
      return true;
    };
    fn_body
      .effects()
      .0
      .contains(&Effect::ModifiesLocalVar(param.clone()))
  }
  /// `(helper s other-args... free-names...)` for a call that passes the
  /// aliased payload binding (or places inside it) by reference.
  fn call_through_payload(
    &mut self,
    payload: &PayloadAlias,
    call: TypedExp,
    aliased: &[usize],
  ) -> TypedExp {
    let source = call.source_trace.clone();
    let ret = known_type(&call.data).unwrap_or(Type::Unit);
    let ExpKind::Application(callee, args) = call.kind else {
      unreachable!()
    };
    let Some(Type::Function(sig)) = known_type(&callee.data) else {
      unreachable!()
    };
    let writes_back =
      aliased.iter().any(|i| self.callee_mutates_param(&sig, *i));
    let root = payload.root();
    if writes_back
      && root.data.ownership == Ownership::Reference
      && let ExpKind::Name(root_name) = &root.kind
    {
      self.errors.push(CompileError::new(
        CompileErrorKind::ClosureMutationThroughImmutableRef(
          root_name.to_string(),
        ),
        source.clone(),
      ));
    }
    // Generated names share the helper's parameter list with user names.
    let s_name = self.gensym("fnv_s");
    let payload_name = self.gensym("fnv_p");
    let b = ExpBuilder::at(&source);
    // Helper parameters: the scrutinee, then every non-aliased argument
    // (keeping its reference-ness), then the free names the aliased places
    // read (index variables, say).
    let mut params: Vec<(Arc<str>, Type, Ownership)> = vec![(
      s_name.clone(),
      payload.enum_type.clone(),
      if writes_back {
        Ownership::MutableReference
      } else {
        payload.read_ownership()
      },
    )];
    let mut caller_args = vec![payload.scrutinee.clone()];
    let mut inner_args = vec![];
    let mut free_names: Vec<(Arc<str>, Type)> = vec![];
    for (i, a) in args.into_iter().enumerate() {
      if aliased.contains(&i) {
        let _ = a.walk(&mut |e| {
          // Function-typed names are callees (index arithmetic, say), not
          // values the place reads.
          if let ExpKind::Name(n) = &e.kind
            && *n != payload.bound
            && !e.data.is_globally_bound
            && !free_names.iter().any(|(existing, _)| existing == n)
            && let Some(t) = known_type(&e.data)
            && !matches!(t, Type::Function(_))
          {
            free_names.push((n.clone(), t));
          }
          Ok::<bool, Never>(true)
        });
        inner_args.push(a);
      } else {
        let ownership = sig
          .args
          .get(i)
          .map(|(v, _)| v.var_type.ownership)
          .unwrap_or(Ownership::Owned);
        let t = known_type(&a.data).unwrap_or(Type::Unit);
        let name = self.gensym(&format!("fnv_a{i}"));
        let inner_ownership = if ownership == Ownership::MutableReference {
          Ownership::MutableReference
        } else {
          Ownership::Owned
        };
        inner_args.push(b.name_with(&name, &t, inner_ownership));
        params.push((name, t, ownership));
        caller_args.push(a);
      }
    }
    for (n, t) in free_names.iter() {
      params.push((n.clone(), t.clone(), Ownership::Owned));
      caller_args.push(b.name(n, t));
    }
    let inner_call = b.apply(*callee, inner_args, &ret);
    let arm_value = if writes_back {
      let rebuilt = b.apply(
        payload.constructor.clone(),
        vec![b.name(&payload.bound, &payload.payload_type)],
        &payload.enum_type,
      );
      let write_back = b.assign(b.name(&s_name, &payload.enum_type), rebuilt);
      b.then_run(inner_call, self.gensym("fnv_r"), vec![write_back])
    } else {
      inner_call
    };
    let arm_value = b.let_in(
      vec![(
        payload.bound.clone(),
        VariableKind::Var,
        b.name(&payload_name, &payload.payload_type),
      )],
      arm_value,
    );
    let name = self.gensym("call_through_payload");
    let signature = self.payload_match_function(
      &name,
      payload,
      &params,
      &payload_name,
      arm_value,
      &ret,
      &source,
    );
    let param_types: Vec<Type> =
      params.iter().map(|(_, t, _)| t.clone()).collect();
    b.apply(b.callee(&signature, &param_types, &ret), caller_args, &ret)
  }
  /// `(getter s)`: the payload currently stored in the scrutinee.
  fn read_payload(
    &mut self,
    payload: &PayloadAlias,
    source: &SourceTrace,
  ) -> TypedExp {
    let (Type::Enum(e), ExpKind::Name(variant)) =
      (&payload.enum_type, &payload.constructor.kind)
    else {
      unreachable!()
    };
    let b = ExpBuilder::at(source);
    let ownership = payload.read_ownership();
    let key = (
      e.name.clone(),
      variant.clone(),
      ownership == Ownership::Reference,
    );
    let signature = match self.payload_getters.get(&key) {
      Some(s) => s.clone(),
      None => {
        let name = self.gensym(&format!("payload_of_{variant}"));
        let s_name: Arc<str> = "fnv_s".into();
        let payload_name: Arc<str> = "fnv_p".into();
        let signature = self.payload_match_function(
          &name,
          payload,
          &[(s_name, payload.enum_type.clone(), ownership)],
          &payload_name,
          b.name(&payload_name, &payload.payload_type),
          &payload.payload_type,
          source,
        );
        self.payload_getters.insert(key, signature.clone());
        signature
      }
    };
    b.apply(
      b.callee(
        &signature,
        &[payload.enum_type.clone()],
        &payload.payload_type,
      ),
      vec![payload.scrutinee.clone()],
      &payload.payload_type,
    )
  }
  /// A generated function `(fn [s rest...] (match s (V p) arm-value _ zero))`,
  /// registered for the drain.
  fn payload_match_function(
    &mut self,
    name: &Arc<str>,
    payload: &PayloadAlias,
    params: &[(Arc<str>, Type, Ownership)],
    payload_name: &Arc<str>,
    arm_value: TypedExp,
    ret: &Type,
    source: &SourceTrace,
  ) -> Arc<RwLock<AbstractFunctionSignature>> {
    let b = ExpBuilder::at(source);
    let (s_name, _, s_ownership) = &params[0];
    let pattern = b.apply(
      payload.constructor.clone(),
      vec![b.name(payload_name, &payload.payload_type)],
      &payload.enum_type,
    );
    let fallback = zero_value(ret, source).unwrap_or_else(|| {
      self.errors.push(CompileError::new(
        CompileErrorKind::UnsupportedFunctionValueSignature,
        source.clone(),
      ));
      b.unit()
    });
    let body = b.match_on(
      b.name_with(s_name, &payload.enum_type, *s_ownership),
      vec![
        (pattern, arm_value),
        (b.wildcard(&payload.enum_type), fallback),
      ],
      ret,
    );
    let signature = generated_function(name, params, ret, body, source);
    self.new_functions.push(signature.clone());
    signature
  }
  fn unit_variant_index(&self, e: &Enum, name: &Arc<str>) -> Option<usize> {
    if let Some(i) = e.variants.iter().position(|v| v.name == *name) {
      return Some(i);
    }
    let base = self
      .program()
      .names
      .read()
      .unwrap()
      .monomorphized_to_base_names()
      .get(name)
      .cloned()?;
    e.variants.iter().position(|v| v.name == base)
  }
  fn lower_pattern(
    &mut self,
    instance: usize,
    pattern: &mut TypedExp,
    scrutinee_type: &Option<Type>,
    scrutinee_skel: &Skel,
  ) {
    let source = pattern.source_trace.clone();
    let Some(Type::Enum(e)) = scrutinee_type else {
      self.lower(instance, pattern);
      return;
    };
    if !type_involves_box(scrutinee_type.as_ref().unwrap(), self.program()) {
      return;
    }
    let payloads = match scrutinee_skel {
      Skel::Enum(p) => p.clone(),
      _ => return,
    };
    match &mut pattern.kind {
      ExpKind::Application(ctor, args) if args.len() == 1 => {
        let Some(i) = enum_variant_index_of_ctor(ctor, e) else {
          return;
        };
        let (enum_type, name, constructor) = {
          let spec = self.enum_rep(e, &payloads, &source);
          (
            spec.enum_type.clone(),
            spec.variant_names[i].clone(),
            spec.constructors[i].clone(),
          )
        };
        let Type::Enum(spec_enum) = &enum_type else {
          unreachable!()
        };
        let payload = spec_enum.variants[i].inner_type.unwrap_known();
        if payload == Type::Unit {
          // The payload lowered to nothing (a single scopeless function):
          // this is now a unit variant. The bound name, typed unit, is
          // erased by `remove_unitlike_values`.
          pattern.kind = ExpKind::Name(name);
          pattern.data.kind = TypeState::Known(enum_type);
          return;
        }
        ctor.kind = ExpKind::Name(name);
        if let Some(constructor) = constructor {
          ctor.data.kind = TypeState::Known(function_type(
            &constructor,
            &[payload.clone()],
            &enum_type,
          ));
        }
        args[0].data.kind = TypeState::Known(payload);
        pattern.data.kind = TypeState::Known(enum_type);
      }
      ExpKind::Name(name) => {
        if let Some(i) = self.unit_variant_index(e, name) {
          let spec = self.enum_rep(e, &payloads, &source);
          *name = spec.variant_names[i].clone();
          pattern.data.kind = TypeState::Known(spec.enum_type.clone());
        }
      }
      _ => {}
    }
  }
  fn lower_application(
    &mut self,
    instance: usize,
    exp: &mut TypedExp,
    skel: &Skel,
  ) {
    let source = exp.source_trace.clone();
    let slot = exp.data.defun_slot;
    let original_type = known_type(&exp.data);
    let new_type = original_type.as_ref().map(|t| self.rep(t, skel, &source));
    let ExpKind::Application(f, args) = &mut exp.kind else {
      unreachable!()
    };
    let Some(Type::Function(callee_sig)) = known_type(&f.data) else {
      self.lower(instance, f);
      for a in args.iter_mut() {
        self.lower(instance, a);
      }
      if let Some(t) = new_type {
        exp.data.kind = TypeState::Known(t);
      }
      return;
    };
    let Some(ancestor) = callee_sig.abstract_ancestor.clone() else {
      for a in args.iter_mut() {
        self.lower(instance, a);
      }
      return;
    };
    let (name, implementation) = {
      let a = ancestor.read().unwrap();
      (a.name.clone(), a.implementation.clone())
    };
    // Member identity of a wrapped function, read before its type is
    // retargeted by lowering.
    let make_key =
      (&*name == FNBOX_MAKE || &*name == FNBOX_BORROW).then(|| {
        let static_type = known_type(&args[0].data).unwrap();
        let Type::Function(s) = &static_type else {
          unreachable!()
        };
        member_key(
          &s.abstract_ancestor.as_ref().unwrap().read().unwrap(),
          &static_type,
        )
      });
    // Closures lent to a boxed call's parameters, read before lowering
    // rewrites the arguments: (argument index, lent place, member key, box
    // skeleton).
    let borrows: Vec<(usize, TypedExp, String, Skel)> = {
      args
        .iter()
        .enumerate()
        .filter_map(|(i, a)| {
          let ExpKind::Application(callee, inner) = &a.kind else {
            return None;
          };
          let Some(Type::Function(callee_sig)) = known_type(&callee.data)
          else {
            return None;
          };
          let is_borrow = callee_sig
            .abstract_ancestor
            .as_ref()
            .is_some_and(|c| &*c.read().unwrap().name == FNBOX_BORROW);
          if !is_borrow {
            return None;
          }
          let static_type = known_type(&inner[0].data).unwrap();
          let Type::Function(s) = &static_type else {
            unreachable!()
          };
          let key = member_key(
            &s.abstract_ancestor.as_ref().unwrap().read().unwrap(),
            &static_type,
          );
          Some((
            i,
            inner[0].clone(),
            key,
            self.slot_skel(instance, a.data.defun_slot),
          ))
        })
        .collect()
    };
    let original_arg_types: Vec<Option<Type>> =
      args.iter().map(|a| known_type(&a.data)).collect();
    for a in args.iter_mut() {
      self.lower(instance, a);
    }
    let b = ExpBuilder::at(&source);
    if &*name == FNBOX_MAKE || &*name == FNBOX_BORROW {
      let Skel::Box(n) = skel else { unreachable!() };
      let members = self.members_of(*n);
      let make_key = make_key.unwrap();
      let mut arg = args.remove(0);
      if members.len() == 1 {
        if members[0].1.payload.is_none() {
          *exp = b.unit();
        } else {
          let payload = self.payload_rep(&members[0].1, &source);
          arg.data.kind = TypeState::Known(payload);
          *exp = arg;
        }
        return;
      }
      let union_key = self.union_key(&members, &source);
      let union = &self.union_enums[&union_key];
      let enum_type = union.enum_type.clone();
      let Some((_, variant_name, payload, ctor)) = union
        .variants
        .iter()
        .find(|(k, _, _, _)| *k == make_key)
        .cloned()
      else {
        unreachable!("wrapped function missing from its own union")
      };
      *exp = match ctor {
        Some(ctor) => {
          arg.data.kind = TypeState::Known(payload);
          b.call(&ctor, vec![arg], &enum_type)
        }
        None => b.name(&variant_name, &enum_type),
      };
      return;
    }
    // A lent closure whose box is a union (not the closure's own scope)
    // can't be passed as the place itself: it's copied into a union
    // temporary for the call and its state copied back afterwards.
    let mut copy_backs = vec![];
    let mut lent_temps = vec![];
    let mut copied_targets: Vec<Arc<str>> = vec![];
    for (i, target, key, box_skel) in borrows {
      if is_lvalue(&args[i])
        || known_type(&args[i].data).is_none_or(|t| t == Type::Unit)
      {
        continue;
      }
      // Two copies of one mutating closure would each advance and write
      // back separately; like passing it to two reference parameters,
      // that's rejected.
      if let ExpKind::Name(n) = &target.kind {
        if copied_targets.contains(n) && self.lent_closure_mutates(&target) {
          self.errors.push(CompileError::new(
            CompileErrorKind::AliasedRefArgs(n.to_string()),
            source.clone(),
          ));
        }
        copied_targets.push(n.clone());
      }
      let temp = self.gensym("fnval_lent");
      let temp_type = known_type(&args[i].data).unwrap();
      let value = std::mem::replace(&mut args[i], b.name(&temp, &temp_type));
      let mut target = target;
      self.lower(instance, &mut target);
      copy_backs.push(self.lower_copy_back(
        target,
        b.name(&temp, &temp_type),
        &box_skel,
        &key,
        source.clone(),
      ));
      lent_temps.push((temp, value));
    }
    if &*name == FNBOX_APPLY {
      self.lower_apply(instance, exp, slot, new_type, source.clone());
    } else {
      self.lower_builtin_or_constructor(
        instance,
        exp,
        skel,
        &name,
        implementation,
        original_type,
        original_arg_types,
        new_type,
        slot,
        source.clone(),
      );
    }
    if !lent_temps.is_empty() {
      let call = std::mem::replace(exp, b.unit());
      let body = b.then_run(call, self.gensym("fnval_result"), copy_backs);
      *exp = b.let_in(
        lent_temps
          .into_iter()
          .map(|(n, v)| (n, VariableKind::Var, v))
          .collect(),
        body,
      );
    }
  }
  /// Lowers `($fnbox-apply boxed args...)` (arguments already lowered) to a
  /// direct call of the site's single member, a dispatcher call for a
  /// union, or a zero value for a provably-empty set.
  fn lower_apply(
    &mut self,
    instance: usize,
    exp: &mut TypedExp,
    slot: Option<u32>,
    new_type: Option<Type>,
    source: SourceTrace,
  ) {
    let ExpKind::Application(f, args) = &mut exp.kind else {
      unreachable!()
    };
    let Some(Type::Function(callee_sig)) = known_type(&f.data) else {
      unreachable!()
    };
    let site = self.analysis.instances[instance].applies[&slot.unwrap()];
    let node = self.analysis.sites[site].node;
    let members = self.members_of(node);
    let mut value = args.remove(0);
    let rest = std::mem::take(args);
    let ret = new_type.clone().unwrap_or(Type::Unit);
    let b = ExpBuilder::at(&source);
    match members.len() {
      0 => {
        // No function ever reaches this position in this instance, so the
        // call can't execute (e.g. the payload arm of a `match` in an
        // instance only ever given another variant). Any well-typed value
        // will do.
        match zero_value(&ret, &source) {
          Some(zero) => *exp = zero,
          None => {
            self.errors.push(CompileError::new(
              CompileErrorKind::CallOfEmptyFunctionValue,
              source.clone(),
            ));
            *exp = b.unit();
          }
        }
      }
      1 => {
        let (key, member) = members[0].clone();
        let linked = self.analysis.sites[site]
          .linked
          .get(&key)
          .cloned()
          .flatten();
        let mut call_args = rest;
        let callee = match linked {
          None => {
            b.name(&member.function.read().unwrap().name, &member.static_type)
          }
          Some(member_instance) => {
            let (signature, params, member_ret) =
              self.lowered_function(member_instance);
            if member.payload.is_some() {
              value.data.kind =
                TypeState::Known(params.last().cloned().unwrap());
              // A value that's itself a reference (a boxed reference
              // parameter) can only be passed on by reference.
              let value_is_reference = value.data.ownership != Ownership::Owned
                || self.is_reference_param(instance, &value);
              if !value_is_reference
                && self.may_be_gpu_storage(instance, &value)
                && !self.instance_mutates_scope(member_instance)
              {
                // Read the stored closure by value, like a dispatcher whose
                // members don't mutate: no reference into the value's
                // place, which may be read-only GPU storage.
                let twin = self.by_value_twin(member_instance);
                call_args.push(value);
                b.callee(&twin, &params, &member_ret)
              } else {
                call_args.push(value);
                b.callee(&signature, &params, &member_ret)
              }
            } else {
              b.callee(&signature, &params, &member_ret)
            }
          }
        };
        *exp = b.apply(callee, call_args, &ret);
      }
      _ => {
        let union_key = self.union_key(&members, &source);
        let arg_types: Vec<Type> =
          rest.iter().map(|a| known_type(&a.data).unwrap()).collect();
        let arg_ownerships: Vec<Ownership> = callee_sig
          .args
          .iter()
          .skip(1)
          .zip(arg_types.iter())
          .map(|((a, _), t)| match t {
            Type::Unit => Ownership::Owned,
            _ => a.var_type.ownership,
          })
          .collect();
        // A value that's itself a reference (a boxed reference
        // parameter) can only be passed on by reference.
        let value_is_reference = value.data.ownership != Ownership::Owned
          || self.is_reference_param(instance, &value);
        let dispatcher = self.dispatcher(
          site,
          &union_key,
          &arg_types,
          &arg_ownerships,
          &ret,
          value_is_reference,
        );
        let enum_type = self.union_enums[&union_key].enum_type.clone();
        let mut params = vec![enum_type];
        params.extend(arg_types);
        let mut call_args = vec![value];
        call_args.extend(rest);
        *exp = b.apply(b.callee(&dispatcher, &params, &ret), call_args, &ret);
      }
    }
  }
  /// Lowers an application of a constructor, a composite function, or an
  /// ordinary builtin (arguments already lowered).
  fn lower_builtin_or_constructor(
    &mut self,
    instance: usize,
    exp: &mut TypedExp,
    skel: &Skel,
    name: &str,
    implementation: FunctionImplementationKind,
    original_type: Option<Type>,
    original_arg_types: Vec<Option<Type>>,
    new_type: Option<Type>,
    slot: Option<u32>,
    source: SourceTrace,
  ) {
    let ExpKind::Application(f, args) = &mut exp.kind else {
      unreachable!()
    };
    let Some(Type::Function(callee_sig)) = known_type(&f.data) else {
      unreachable!()
    };
    let arg_types: Vec<Type> = args
      .iter()
      .map(|a| known_type(&a.data).unwrap_or(Type::Unit))
      .collect();
    match implementation {
      FunctionImplementationKind::StructConstructor => {
        let Some(new_type) = new_type else { return };
        if !original_type
          .as_ref()
          .is_some_and(|t| type_involves_box(t, self.program()))
        {
          return;
        }
        let struct_type = match &new_type {
          Type::Function(sig) => {
            sig.closure_scope().and_then(|scope| scope.concrete_type())
          }
          t @ Type::Struct(_) => Some(t.clone()),
          _ => None,
        };
        if let Some(Type::Struct(s)) = &struct_type {
          let constructor = struct_constructor(
            &s.name,
            &arg_types,
            struct_type.as_ref().unwrap(),
          );
          f.kind = ExpKind::Name(s.name.clone());
          f.data.kind = TypeState::Known(function_type(
            &constructor,
            &arg_types,
            &new_type,
          ));
        }
        exp.data.kind = TypeState::Known(new_type);
      }
      FunctionImplementationKind::EnumConstructor(_) => {
        let (
          Some(new_type),
          Some(Type::Enum(original_enum)),
          Skel::Enum(payloads),
        ) = (new_type, original_type.clone(), skel)
        else {
          return;
        };
        if !type_involves_box(original_type.as_ref().unwrap(), self.program()) {
          return;
        }
        if let Some(i) = enum_variant_index_of_ctor(f, &original_enum) {
          let (variant_name, constructor) = {
            let spec = self.enum_rep(&original_enum, payloads, &source);
            (spec.variant_names[i].clone(), spec.constructors[i].clone())
          };
          if constructor.is_none() {
            // The payload lowered to nothing: a unit variant.
            exp.kind = ExpKind::Name(variant_name);
            exp.data.kind = TypeState::Known(new_type);
            return;
          }
          if let Some(constructor) = constructor {
            f.kind = ExpKind::Name(variant_name);
            f.data.kind = TypeState::Known(function_type(
              &constructor,
              &arg_types,
              &new_type,
            ));
          }
        }
        exp.data.kind = TypeState::Known(new_type);
      }
      FunctionImplementationKind::Composite(_) => {
        let Some(callee) = slot.and_then(|s| {
          self.analysis.instances[instance].calls.get(&s).cloned()
        }) else {
          return;
        };
        let (signature, params, ret) = self.lowered_function(callee);
        let callee_name = signature.read().unwrap().name.clone();
        f.kind = ExpKind::Name(callee_name);
        let lowered_ownerships: Vec<Ownership> = signature
          .read()
          .unwrap()
          .arg_types
          .iter()
          .map(|(_, o)| *o)
          .collect();
        let ownerships: Vec<(Ownership, VariableKind)> = callee_sig
          .args
          .iter()
          .enumerate()
          .map(|(i, (a, _))| match lowered_ownerships.get(i) {
            Some(Ownership::MutableReference)
              if a.var_type.ownership == Ownership::Owned =>
            {
              (Ownership::MutableReference, VariableKind::Var)
            }
            _ => (a.var_type.ownership, a.kind.clone()),
          })
          .collect();
        let mut new_callee = callee_sig.clone();
        new_callee.abstract_ancestor = Some(signature);
        for (i, (a, _)) in new_callee.args.iter_mut().enumerate() {
          if let Some(t) = params.get(i) {
            a.var_type.kind = TypeState::Known(t.clone());
          }
          if let Some((o, k)) = ownerships.get(i) {
            a.var_type.ownership = *o;
            a.kind = k.clone();
          }
        }
        new_callee.return_type.kind = TypeState::Known(ret.clone());
        f.data.kind = TypeState::Known(Type::Function(new_callee));
        exp.data.kind = TypeState::Known(ret);
      }
      FunctionImplementationKind::Builtin { .. } => {
        // Assigning a value that lowered to nothing (a single scopeless
        // function) does nothing; keep only the right-hand side's effects.
        if ASSIGNMENT_OPS.contains(&*name)
          && args.len() == 2
          && known_type(&args[0].data) == Some(Type::Unit)
        {
          *exp = args.remove(1);
          return;
        }
        // Host-invoked closures point at the instance the host runs.
        let hosts = self.analysis.instances[instance].hosts.clone();
        for a in args.iter_mut() {
          if let Some(arg_slot) = a.data.defun_slot
            && let Some(host) = hosts.get(&arg_slot)
          {
            let (signature, _, _) = self.lowered_function(*host);
            a.data.kind.with_dereferenced_mut(|ts| {
              if let TypeState::Known(Type::Function(s)) = ts {
                s.abstract_ancestor = Some(signature.clone());
              }
            });
          }
        }
        let lowered_arg_types: Vec<Type> = args
          .iter()
          .map(|a| known_type(&a.data).unwrap_or(Type::Unit))
          .collect();
        let program = self.program();
        f.data.kind.with_dereferenced_mut(|ts| {
          if let TypeState::Known(Type::Function(s)) = ts {
            for (i, (a, _)) in s.args.iter_mut().enumerate() {
              if original_arg_types
                .get(i)
                .cloned()
                .flatten()
                .is_some_and(|t| type_involves_box(&t, program))
                && let Some(t) = lowered_arg_types.get(i)
              {
                a.var_type.kind = TypeState::Known(t.clone());
              }
            }
            if let Some(t) = &new_type {
              s.return_type.kind = TypeState::Known(t.clone());
            }
          }
        });
        if let Some(t) = new_type {
          exp.data.kind = TypeState::Known(t);
        }
      }
    }
  }
  /// Whether `exp` names one of the instance's by-reference boxed
  /// parameters.
  /// Whether `place` may be GPU storage, which can't be referenced: a place
  /// rooted at a global, or at a closure's captured scope (a dispatched
  /// closure's captures become storage bindings). Locals and parameters are
  /// function-space everywhere.
  fn may_be_gpu_storage(&self, instance: usize, place: &TypedExp) -> bool {
    let Some(root) = place.name_or_inner_accessed_name() else {
      return false;
    };
    if self
      .program()
      .top_level_vars
      .iter()
      .any(|v| v.name == *root)
    {
      return true;
    }
    let (InstanceBody::Function(body), Some(function)) = (
      &self.analysis.instances[instance].body,
      &self.analysis.instances[instance].function,
    ) else {
      return false;
    };
    function.read().unwrap().captured_scope.is_some()
      && body.arg_names.last().is_some_and(|(n, _)| n == root)
  }
  fn is_reference_param(&mut self, instance: usize, exp: &TypedExp) -> bool {
    let ExpKind::Name(name) = &exp.kind else {
      return false;
    };
    let InstanceBody::Function(body) = &self.analysis.instances[instance].body
    else {
      return false;
    };
    let Some(i) = body.arg_names.iter().position(|(n, _)| n == name) else {
      return false;
    };
    self.boxed_reference_params(instance).contains(&i)
  }
  /// Whether the static closure held in `place` mutates its own state when
  /// called (conservatively true when its function has no instance).
  fn lent_closure_mutates(&mut self, place: &TypedExp) -> bool {
    let Some(Type::Function(sig)) = known_type(&place.data) else {
      return true;
    };
    let Some(function) = &sig.abstract_ancestor else {
      return true;
    };
    let name = function.read().unwrap().name.clone();
    let instance = self.analysis.instances.iter().position(|i| {
      i.function
        .as_ref()
        .is_some_and(|f| f.read().unwrap().name == name)
        && matches!(i.body, InstanceBody::Function(_))
    });
    match instance {
      Some(instance) => self.instance_mutates_scope(instance),
      None => true,
    }
  }
  /// Writes a lent closure's advanced state back from the union temporary
  /// it was copied into for a boxed call: `(match boxed (V s) (= target s)
  /// _ ())`, or a direct assignment when the box is the closure's own scope,
  /// or nothing for a scopeless function.
  fn lower_copy_back(
    &mut self,
    target: TypedExp,
    boxed: TypedExp,
    boxed_skel: &Skel,
    target_key: &str,
    source: SourceTrace,
  ) -> TypedExp {
    let b = ExpBuilder::at(&source);
    let Skel::Box(n) = boxed_skel else {
      return b.unit();
    };
    let members = self.members_of(*n);
    let Some((_, member)) = members.iter().find(|(k, _)| k == target_key)
    else {
      return b.unit();
    };
    if member.payload.is_none() {
      return b.unit();
    }
    // The target is the closure's scope, retyped as its representation.
    let assign = |mut target: TypedExp, value: TypedExp, payload: &Type| {
      target.data.kind = TypeState::Known(payload.clone());
      b.assign(target, value)
    };
    if members.len() == 1 {
      let payload = self.payload_rep(member, &source);
      let mut value = boxed;
      value.data.kind = TypeState::Known(payload.clone());
      return assign(target, value, &payload);
    }
    let union_key = self.union_key(&members, &source);
    let union = &self.union_enums[&union_key];
    let enum_type = union.enum_type.clone();
    let Some((_, _, payload, Some(ctor))) = union
      .variants
      .iter()
      .find(|(k, _, _, _)| k == target_key)
      .cloned()
    else {
      return b.unit();
    };
    let scope_name = self.gensym("fnval_state");
    let pattern =
      b.call(&ctor, vec![b.name(&scope_name, &payload)], &enum_type);
    let write_back = assign(target, b.name(&scope_name, &payload), &payload);
    b.match_on(
      boxed,
      vec![(pattern, write_back), (b.wildcard(&enum_type), b.unit())],
      &Type::Unit,
    )
  }
  /// The lowered body of an instance (memoized).
  fn lower_instance(&mut self, instance: usize) -> TopLevelFunction {
    if let Some(body) = self.lowered_bodies.get(&instance) {
      return body.clone();
    }
    let InstanceBody::Function(template) =
      &self.analysis.instances[instance].body
    else {
      unreachable!()
    };
    let mut body = template.clone();
    if !self.is_root(instance)
      || body_involves_box(&template.expression, self.program())
    {
      self.lower(instance, &mut body.expression);
    }
    self.lowered_bodies.insert(instance, body.clone());
    body
  }
  /// Whether a closure instance's lowered body mutates its scope parameter.
  fn instance_mutates_scope(&mut self, instance: usize) -> bool {
    let body = self.lower_instance(instance);
    let ExpKind::Function(arg_names, fn_body) = &body.expression.kind else {
      return false;
    };
    let Some((scope_name, _)) = arg_names.last() else {
      return false;
    };
    fn_body
      .effects()
      .0
      .contains(&Effect::ModifiesLocalVar(scope_name.clone()))
  }
  /// The dispatcher applying union `union_key` at apply site `site`:
  /// `apply(u, args...) = (match u (V_i scope) (m_i args... scope) ...)`.
  /// When any member mutates its scope, `u` is taken by mutable reference
  /// and each mutating arm writes its (copied, mutated) scope back into `u`,
  /// so a stateful closure stored in an array / field advances in place.
  /// `force_by_ref` takes `u` by reference regardless (for a value that is
  /// itself a reference parameter).
  fn dispatcher(
    &mut self,
    site: usize,
    union_key: &str,
    arg_types: &[Type],
    arg_ownerships: &[Ownership],
    ret: &Type,
    force_by_ref: bool,
  ) -> Arc<RwLock<AbstractFunctionSignature>> {
    let variants = self.union_enums[union_key].variants.clone();
    let enum_type = self.union_enums[union_key].enum_type.clone();
    let enum_name = match &enum_type {
      Type::Enum(e) => e.name.clone(),
      _ => unreachable!(),
    };
    // Resolve each variant's callee.
    struct Arm {
      variant: Arc<str>,
      payload: Type,
      ctor: Option<Arc<RwLock<AbstractFunctionSignature>>>,
      callee_name: Arc<str>,
      callee_type: Type,
      mutates: bool,
      /// A unit-variant closure's (unit-like) scope, passed as a zero value.
      empty_scope: Option<Type>,
    }
    let mut arms = vec![];
    let mut key = format!("{enum_name}(");
    let node = self.analysis.sites[site].node;
    let node = self.analysis.find(node);
    for (member_key, variant, payload, ctor) in variants.iter() {
      let member = self.analysis.nodes[node].members[member_key].clone();
      let linked = self.analysis.sites[site]
        .linked
        .get(member_key)
        .cloned()
        .flatten();
      let (callee_name, callee_type, mutates) = match linked {
        None => (
          member.function.read().unwrap().name.clone(),
          member.static_type.clone(),
          false,
        ),
        Some(member_instance) => {
          let (signature, params, member_ret) =
            self.lowered_function(member_instance);
          let mutates = member.payload.is_some()
            && self.instance_mutates_scope(member_instance);
          (
            signature.read().unwrap().name.clone(),
            function_type(&signature, &params, &member_ret),
            mutates,
          )
        }
      };
      key += &format!("{callee_name},");
      let empty_scope = (ctor.is_none() && member.payload.is_some())
        .then(|| self.payload_rep(&member, &SourceTrace::empty()));
      arms.push(Arm {
        variant: variant.clone(),
        payload: payload.clone(),
        ctor: ctor.clone(),
        callee_name,
        callee_type,
        mutates,
        empty_scope,
      });
    }
    key += &format!(
      "){}[{}]->{}",
      if force_by_ref { "&mut" } else { "" },
      arg_types
        .iter()
        .zip(arg_ownerships.iter())
        .map(|(t, o)| format!("{}{}", type_key(t), ownership_key(*o)))
        .collect::<Vec<_>>()
        .join(","),
      type_key(ret)
    );
    if let Some(d) = self.dispatchers.get(&key) {
      return d.clone();
    }
    let by_ref = force_by_ref || arms.iter().any(|a| a.mutates);
    let source = SourceTrace::empty();
    let b = ExpBuilder::at(&source);
    let u_name: Arc<str> = "fnv_u".into();
    let u_ownership = if by_ref {
      Ownership::MutableReference
    } else {
      Ownership::Owned
    };
    // Parameters: the union, then the call's arguments.
    let params: Vec<(Arc<str>, Type, Ownership)> =
      std::iter::once((u_name.clone(), enum_type.clone(), u_ownership))
        .chain(
          arg_types
            .iter()
            .zip(arg_ownerships.iter())
            .enumerate()
            .map(|(i, (t, o))| (format!("fnv_a{i}").into(), t.clone(), *o)),
        )
        .collect();
    let arg_refs = || -> Vec<TypedExp> {
      params[1..]
        .iter()
        .map(|(n, t, o)| b.name_with(n, t, *o))
        .collect()
    };
    let match_arms: Vec<(TypedExp, TypedExp)> = arms
      .iter()
      .map(|arm| {
        let callee = b.name(&arm.callee_name, &arm.callee_type);
        let Some(ctor) = &arm.ctor else {
          // A payload-less variant: call the member directly (with an empty
          // scope for a closure whose captures lowered to nothing).
          let mut call_args = arg_refs();
          if let Some(scope) = &arm.empty_scope {
            call_args.extend(zero_value(scope, &source));
          }
          return (
            b.name(&arm.variant, &enum_type),
            b.apply(callee, call_args, ret),
          );
        };
        // (V_i fnv_s): the scope is copied into `@var fnv_s2` for the call.
        let scope_name: Arc<str> = "fnv_s".into();
        let copy_name: Arc<str> = "fnv_s2".into();
        let pattern =
          b.call(ctor, vec![b.name(&scope_name, &arm.payload)], &enum_type);
        let mut call_args = arg_refs();
        call_args.push(b.name(&copy_name, &arm.payload));
        let call = b.apply(callee, call_args, ret);
        let body = if arm.mutates {
          // (= u (V_i fnv_s2)): write the advanced scope back.
          let rebuilt =
            b.call(ctor, vec![b.name(&copy_name, &arm.payload)], &enum_type);
          let write_back =
            b.assign(b.name_with(&u_name, &enum_type, u_ownership), rebuilt);
          b.then_run(call, "fnv_r".into(), vec![write_back])
        } else {
          call
        };
        let value = b.let_in(
          vec![(
            copy_name,
            VariableKind::Var,
            b.name(&scope_name, &arm.payload),
          )],
          body,
        );
        (pattern, value)
      })
      .collect();
    let body = b.match_on(
      b.name_with(&u_name, &enum_type, u_ownership),
      match_arms,
      ret,
    );
    let name = self.gensym(&format!("apply_{enum_name}"));
    let signature = generated_function(&name, &params, ret, body, &source);
    self.new_functions.push(signature.clone());
    self.dispatchers.insert(key, signature.clone());
    self.dispatcher_by_reference.insert(name, by_ref);
    signature
  }
  /// A twin of a lowered closure instance that doesn't mutate its scope,
  /// taking the scope by value instead of by reference: calls through it
  /// read a stored closure without referencing its place.
  fn by_value_twin(
    &mut self,
    instance: usize,
  ) -> Arc<RwLock<AbstractFunctionSignature>> {
    let (signature, params, ret) = self.lowered_function(instance);
    let name = signature.read().unwrap().name.clone();
    if let Some(twin) = self.by_value_twins.get(&name) {
      return twin.clone();
    }
    let scope_index = params.len() - 1;
    let twin_name = self.gensym(&format!("{name}_by_value"));
    let twin = {
      let mut twin = signature.read().unwrap().clone();
      twin.name = twin_name;
      twin.arg_types[scope_index].1 = Ownership::Owned;
      Arc::new(RwLock::new(twin))
    };
    let mut body = self.lower_instance(instance).derived_from();
    body.entry_point = None;
    let reference_params = self.boxed_reference_params(instance);
    mark_reference_param_uses(&mut body, &reference_params);
    let scope_name = body.arg_names[scope_index].0.clone();
    let _ = body.expression.walk_mut::<()>(&mut |e| {
      if let ExpKind::Name(n) = &e.kind
        && *n == scope_name
      {
        e.data.ownership = Ownership::Owned;
      }
      Ok(true)
    });
    body.expression.data.kind =
      TypeState::Known(function_type(&twin, &params, &ret));
    self.by_value_twin_outputs.push((twin.clone(), body));
    self.by_value_twins.insert(name, twin.clone());
    twin
  }
}

/// Makes owned parameters that a lowered body passes by mutable reference
/// (a stateful function value read out of an array parameter, say)
/// addressable, by shadowing each with a `@var` local copy — function
/// parameters aren't addressable in WGSL.
fn make_mutably_referenced_params_addressable(
  implementation: &mut TopLevelFunction,
  names: &RwLock<NameContext>,
) {
  let Some(Type::Function(sig)) = known_type(&implementation.expression.data)
  else {
    return;
  };
  let ExpKind::Function(arg_names, body) = &mut implementation.expression.kind
  else {
    return;
  };
  let owned_params: HashMap<Arc<str>, usize> = arg_names
    .iter()
    .enumerate()
    .filter(|(i, _)| {
      sig.args.get(*i).is_some_and(|(a, _)| {
        a.var_type.ownership == Ownership::Owned && a.kind != VariableKind::Var
      })
    })
    .map(|(i, (n, _))| (n.clone(), i))
    .collect();
  let mut referenced: BTreeSet<usize> = BTreeSet::new();
  let _ = body.walk(&mut |e| {
    if let ExpKind::Application(f, args) = &e.kind
      && let Some(Type::Function(callee)) = known_type(&f.data)
    {
      for (a, (param, _)) in args.iter().zip(callee.args.iter()) {
        if param.var_type.ownership == Ownership::MutableReference
          && let Some(root) = a.name_or_inner_accessed_name()
          && let Some(i) = owned_params.get(root)
        {
          referenced.insert(*i);
        }
      }
    }
    Ok::<bool, Never>(true)
  });
  if referenced.is_empty() {
    return;
  }
  let mut bindings = vec![];
  for i in referenced {
    let (name, source) = arg_names[i].clone();
    let incoming = names.write().unwrap().gensym(&format!("{name}_in"));
    arg_names[i].0 = incoming.clone();
    let t = sig.args[i].0.var_type.unwrap_known();
    bindings.push((
      name,
      source.clone(),
      VariableKind::Var,
      ExpBuilder::at(&source).name(&incoming, &t),
    ));
  }
  take(body.as_mut(), |body| let_around(bindings, body));
  let ExpKind::Function(expression_names, _) = &implementation.expression.kind
  else {
    unreachable!()
  };
  implementation.arg_names = expression_names.clone();
}

/// Marks every use of the given (now by-reference) parameters as a
/// reference, as higher-order inlining does for closure parameters.
fn mark_reference_param_uses(body: &mut TopLevelFunction, params: &[usize]) {
  if params.is_empty() {
    return;
  }
  let names: HashSet<Arc<str>> = params
    .iter()
    .filter_map(|i| body.arg_names.get(*i).map(|(n, _)| n.clone()))
    .collect();
  let _ = body.expression.walk_mut::<()>(&mut |e| {
    if let ExpKind::Name(n) = &e.kind
      && names.contains(n)
    {
      e.data.ownership = Ownership::MutableReference;
    }
    Ok(true)
  });
}

fn program_has_boxed_functions(program: &Program) -> bool {
  program.abstract_functions_iter().any(|f| {
    match &f.read().unwrap().implementation {
      FunctionImplementationKind::Composite(implementation) => {
        body_involves_box(&implementation.read().unwrap().expression, program)
          || function_involves_box(f, program)
      }
      _ => false,
    }
  }) || program
    .top_level_vars
    .iter()
    .any(|v| type_involves_box(&v.var_type, program))
    || program.typedefs.structs.iter().any(|s| {
      s.fields
        .iter()
        .any(|f| abstract_type_holds(&f.field_type, &holds_boxed_function))
    })
    || program.typedefs.enums.iter().any(|e| {
      e.variants
        .iter()
        .any(|v| abstract_type_holds(&v.inner_type, &holds_boxed_function))
    })
}

impl Program {
  /// See the module docs: lowers every boxed function value to its
  /// per-usage representation. Must run after the extraction / inlining loop
  /// (every member needs an identity and scope struct) and before any pass
  /// that sizes or emits types.
  pub fn defunctionalize_boxed_functions(&mut self, errors: &mut ErrorLog) {
    if !program_has_boxed_functions(self) {
      self.validate_copied_stateful_closures(None, errors);
      return;
    }
    // Generic templates and functions still awaiting higher-order inlining
    // are never called by lowered code (monomorphization / inlining made
    // their concrete copies), so they aren't analyzed.
    let composites: Vec<Arc<RwLock<AbstractFunctionSignature>>> = self
      .abstract_functions_iter()
      .filter(|f| {
        let f = f.read().unwrap();
        matches!(f.implementation, FunctionImplementationKind::Composite(_))
          && f.generic_args.is_empty()
          && !f.has_uninlined_higher_order_arguments()
      })
      .cloned()
      .collect();
    let boxed_functions: HashSet<usize> = composites
      .iter()
      .filter(|f| function_involves_box(f, self))
      .filter_map(implementation_key)
      .collect();
    let mut analysis = Analysis {
      program: self,
      parent: vec![],
      nodes: vec![],
      instances: vec![],
      sites: vec![],
      pending: vec![],
      roots: HashMap::new(),
      registry_by_implementation: self
        .abstract_functions_iter()
        .filter_map(|f| implementation_key(f).map(|key| (key, f.clone())))
        .collect(),
      globals: HashMap::new(),
      global_vars: self
        .top_level_vars
        .iter()
        .map(|v| (v.name.clone(), v.var_type.clone()))
        .collect(),
      recursive_nodes: HashSet::new(),
    };
    for f in composites.iter() {
      if !boxed_functions.contains(&implementation_key(f).unwrap()) {
        analysis.root(f);
      }
    }
    for v in self.top_level_vars.iter() {
      if let Some(value) = &v.value {
        analysis.analyze_global_init(&v.name, value.clone());
      }
    }
    analysis.run();
    if !analysis.recursive_nodes.is_empty() {
      // Report once, at the first apply site on a recursive set.
      let mut reported = false;
      for instance in analysis.instances.iter() {
        let InstanceBody::Function(body) = &instance.body else {
          continue;
        };
        let applies = &instance.applies;
        let sites = &analysis.sites;
        let parent = &analysis.parent;
        let find = |mut n: NodeId| {
          while parent[n] != n {
            n = parent[n];
          }
          n
        };
        let recursive: HashSet<NodeId> =
          analysis.recursive_nodes.iter().map(|n| find(*n)).collect();
        let _ = body.expression.walk(&mut |e| {
          if let Some(slot) = e.data.defun_slot
            && let Some(site) = applies.get(&slot)
            && recursive.contains(&find(sites[*site].node))
            && !reported
          {
            errors.log(CompileError::new(
              CompileErrorKind::RecursiveFunctionValue,
              e.source_trace.clone(),
            ));
            reported = true;
          }
          Ok::<bool, Never>(!reported)
        });
      }
      if !reported {
        errors.log(CompileError::new(
          CompileErrorKind::RecursiveFunctionValue,
          SourceTrace::empty(),
        ));
      }
      return;
    }
    self.validate_copied_stateful_closures(Some(&analysis), errors);
    let mut lowering = Lowering {
      analysis,
      errors: vec![],
      resolving: vec![],
      union_enums: HashMap::new(),
      struct_specs: HashMap::new(),
      used_group_names: HashSet::new(),
      new_structs: vec![],
      enum_specs: HashMap::new(),
      new_enums: vec![],
      new_functions: vec![],
      closure_signatures: HashMap::new(),
      adopted_closure_signatures: HashSet::new(),
      instance_keys: HashMap::new(),
      lowered: HashMap::new(),
      used_function_names: HashSet::new(),
      dispatchers: HashMap::new(),
      dispatcher_by_reference: HashMap::new(),
      by_value_twins: HashMap::new(),
      by_value_twin_outputs: vec![],
      instance_by_name: HashMap::new(),
      payload_getters: HashMap::new(),
      lowered_bodies: HashMap::new(),
      root_instances: HashSet::new(),
    };
    lowering.root_instances =
      lowering.analysis.roots.values().cloned().collect();
    for r in lowering.root_instances.clone() {
      if let Some(f) = &lowering.analysis.instances[r].function {
        let name = f.read().unwrap().name.clone();
        lowering.instance_by_name.insert(name, r);
      }
    }
    // Name every instance (dedupes identical ones).
    for i in 0..lowering.analysis.instances.len() {
      if lowering.analysis.instances[i].function.is_some()
        && !lowering.is_root(i)
      {
        lowering.lowered_name_of(i);
      }
    }
    // Lower root bodies (in place) that involve boxes.
    let mut root_updates: Vec<(
      Arc<RwLock<AbstractFunctionSignature>>,
      TopLevelFunction,
    )> = vec![];
    let roots: Vec<usize> = lowering.analysis.roots.values().cloned().collect();
    for r in roots {
      let function = lowering.analysis.instances[r].function.clone().unwrap();
      let InstanceBody::Function(template) =
        &lowering.analysis.instances[r].body
      else {
        continue;
      };
      if !body_involves_box(&template.expression, self) {
        continue;
      }
      let body = lowering.lower_instance(r);
      root_updates.push((function, body));
    }
    // Lower each distinct clone.
    let mut lowered_keys: HashSet<String> = HashSet::new();
    let mut clone_outputs: Vec<(
      Arc<RwLock<AbstractFunctionSignature>>,
      TopLevelFunction,
    )> = vec![];
    loop {
      let pending: Vec<String> = lowering
        .lowered
        .keys()
        .filter(|k| !lowered_keys.contains(*k))
        .cloned()
        .collect();
      if pending.is_empty() {
        break;
      }
      for key in pending {
        lowered_keys.insert(key.clone());
        let representative = lowering.lowered[&key].representative;
        let signature = lowering.lowered[&key].signature.clone();
        let mut body = lowering.lower_instance(representative).derived_from();
        body.entry_point = None;
        let reference_params = lowering.boxed_reference_params(representative);
        mark_reference_param_uses(&mut body, &reference_params);
        let params = lowering.lowered[&key].params.clone();
        let ret = lowering.lowered[&key].ret.clone();
        body.expression.data.kind =
          TypeState::Known(function_type(&signature, &params, &ret));
        clone_outputs.push((signature, body));
      }
    }
    // Global initializers and types.
    let mut global_inits: HashMap<Arc<str>, TypedExp> = HashMap::new();
    for i in 0..lowering.analysis.instances.len() {
      if let InstanceBody::GlobalInit(name, value) =
        &lowering.analysis.instances[i].body
      {
        let name = name.clone();
        let mut value = value.clone();
        lowering.lower(i, &mut value);
        global_inits.insert(name, value);
      }
    }
    let mut global_types: Vec<(Arc<str>, Type)> = vec![];
    for v in self.top_level_vars.iter() {
      if type_involves_box(&v.var_type, self) {
        let skel = lowering.analysis.global_skel(&v.name);
        let t = lowering.rep(&v.var_type, &skel, &v.source_trace);
        global_types.push((v.name.clone(), t));
      }
    }
    let Lowering {
      errors: lowering_errors,
      new_structs,
      new_enums,
      new_functions,
      union_enums,
      by_value_twin_outputs,
      ..
    } = lowering;
    for e in lowering_errors {
      errors.log(e);
    }
    // --- apply ---
    // A root's signature involves no boxed values, so only its body changes.
    for (function, mut body) in root_updates {
      make_mutably_referenced_params_addressable(&mut body, &self.names);
      if let FunctionImplementationKind::Composite(implementation) =
        &function.read().unwrap().implementation
      {
        *implementation.write().unwrap() = body;
      }
    }
    // Drop the box-involving originals; add their lowered clones.
    for sigs in self.abstract_functions.values_mut() {
      sigs.retain(|s| {
        let composite_kept =
          implementation_key(s).is_none_or(|k| !boxed_functions.contains(&k));
        // Struct / enum constructors of definitions holding boxed values are
        // replaced by their specializations' constructors.
        let constructor_kept = {
          let s = s.read().unwrap();
          !matches!(
            s.implementation,
            FunctionImplementationKind::StructConstructor
              | FunctionImplementationKind::EnumConstructor(_)
          ) || !(s
            .arg_types
            .iter()
            .any(|(t, _)| abstract_type_holds(t, &holds_boxed_function))
            || abstract_type_holds(&s.return_type, &holds_boxed_function))
        };
        composite_kept && constructor_kept
      });
    }
    self.abstract_functions.retain(|_, sigs| !sigs.is_empty());
    for (signature, mut body) in
      clone_outputs.into_iter().chain(by_value_twin_outputs)
    {
      make_mutably_referenced_params_addressable(&mut body, &self.names);
      signature.write().unwrap().implementation =
        FunctionImplementationKind::Composite(Arc::new(RwLock::new(body)));
      self.add_abstract_function(signature);
    }
    for f in new_functions {
      self.add_abstract_function(f);
    }
    // Definitions holding boxed values are replaced by their specializations.
    self.typedefs.structs.retain(|s| {
      !s.fields
        .iter()
        .any(|f| abstract_type_holds(&f.field_type, &holds_boxed_function))
    });
    self.typedefs.enums.retain(|e| {
      !e.variants
        .iter()
        .any(|v| abstract_type_holds(&v.inner_type, &holds_boxed_function))
    });
    for s in new_structs {
      self.add_monomorphized_struct(s);
    }
    for e in new_enums {
      self.add_monomorphized_enum(e);
    }
    for (_, u) in union_enums {
      self.add_monomorphized_enum(u.definition);
    }
    // Globals.
    for v in self.top_level_vars.iter_mut() {
      if let Some(value) = global_inits.remove(&v.name) {
        v.value = Some(value);
      }
      if let Some((_, t)) = global_types.iter().find(|(n, _)| *n == v.name) {
        v.var_type = t.clone();
      }
    }
    // A global whose function value lowered to nothing holds no data.
    let lowered_globals: HashSet<Arc<str>> =
      global_types.iter().map(|(n, _)| n.clone()).collect();
    self.top_level_vars.retain(|v| {
      !(lowered_globals.contains(&v.name) && v.var_type == Type::Unit)
    });
  }
}

// ===========================================================================
// Aliased closure state
// ===========================================================================

impl Program {
  /// Rejects a call passing the same stateful closure (or overlapping
  /// places holding one) to two by-reference parameters. Closure arguments
  /// only become references late — through higher-order-argument inlining,
  /// or a boxed parameter's defunctionalization — after
  /// `validate_ref_arg_aliasing` has run, so the aliasing rule it enforces
  /// for `@var @ref` parameters is enforced for closure state here: two
  /// references to one closure's mutable scope would make the result depend
  /// on the runtime's calling convention. Closures that never mutate their
  /// scope may be passed twice (`(compose f f)`).
  pub fn validate_closure_state_aliasing(&self, errors: &mut ErrorLog) {
    // Which closure scope structs belong to closures that mutate them.
    let mut scope_mutates: HashMap<Arc<str>, bool> = HashMap::new();
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      let (Some(scope), FunctionImplementationKind::Composite(implementation)) =
        (&f.captured_scope, &f.implementation)
      else {
        continue;
      };
      let implementation = implementation.read().unwrap();
      let ExpKind::Function(arg_names, body) = &implementation.expression.kind
      else {
        continue;
      };
      let mutates = arg_names.last().is_some_and(|(scope_name, _)| {
        body
          .effects()
          .0
          .contains(&Effect::ModifiesLocalVar(scope_name.clone()))
      });
      *scope_mutates.entry(scope.name.0.clone()).or_insert(false) |= mutates;
    }
    fn carries_mutable_closure_state(
      t: &Type,
      scope_mutates: &HashMap<Arc<str>, bool>,
    ) -> bool {
      match t {
        Type::Struct(s) => scope_mutates.get(&s.name).copied().unwrap_or(false),
        Type::Function(sig) => {
          sig.abstract_ancestor.as_ref().is_some_and(|a| {
            a.read().unwrap().captured_scope.as_ref().is_some_and(|s| {
              scope_mutates.get(&s.name.0).copied().unwrap_or(false)
            })
          })
        }
        Type::Enum(e) => e.variants.iter().any(|v| {
          v.inner_type
            .kind
            .try_unwrap_known()
            .is_some_and(|t| carries_mutable_closure_state(&t, scope_mutates))
        }),
        Type::Array(_, inner) => inner
          .kind
          .try_unwrap_known()
          .is_some_and(|t| carries_mutable_closure_state(&t, scope_mutates)),
        _ => false,
      }
    }
    for f in self.abstract_functions_iter() {
      let FunctionImplementationKind::Composite(implementation) =
        &f.read().unwrap().implementation
      else {
        continue;
      };
      let _ = implementation.read().unwrap().expression.walk(&mut |exp| {
        if let ExpKind::Application(callee, args) = &exp.kind
          && let Some(Type::Function(signature)) = known_type(&callee.data)
        {
          // Higher-order inlining records a closure parameter's reference
          // ownership on the specialization's signature, not on the call
          // site's view of it.
          let declared: Vec<Ownership> = signature
            .abstract_ancestor
            .as_ref()
            .map(|a| {
              a.read()
                .unwrap()
                .arg_types
                .iter()
                .map(|(_, o)| *o)
                .collect()
            })
            .unwrap_or_default();
          let mut places = vec![];
          for (i, ((param, _), arg)) in
            signature.args.iter().zip(args.iter()).enumerate()
          {
            let ownership = match declared.get(i) {
              Some(Ownership::MutableReference) => Ownership::MutableReference,
              _ => param.var_type.ownership,
            };
            if ownership == Ownership::MutableReference
              && known_type(&arg.data).is_some_and(|t| {
                carries_mutable_closure_state(&t, &scope_mutates)
              })
              && let Some(place) = ref_arg_lvalue_path(arg)
            {
              places.push(place);
            }
          }
          for i in 0..places.len() {
            for j in (i + 1)..places.len() {
              if places[i].0 == places[j].0
                && !ref_paths_provably_disjoint(&places[i].1, &places[j].1)
              {
                errors.log(CompileError::new(
                  CompileErrorKind::AliasedRefArgs(places[i].0.to_string()),
                  exp.source_trace.clone(),
                ));
              }
            }
          }
        }
        Ok::<bool, Never>(true)
      });
    }
  }
}

// ===========================================================================
// Copied stateful closures
// ===========================================================================

/// The builtins that take a function to run later (keeping a copy of it).
const HOST_BUILTINS: [&str; 4] = [
  "start-audio",
  "spawn-window",
  "dispatch-compute-shader",
  "dispatch-render-shaders",
];

/// A body the copy check walks: its function's name, the function (for its
/// declared parameter ownerships), the body, and — when function values are
/// being lowered — the analysis instance whose slots describe its boxed
/// positions.
struct CheckedBody<'a> {
  name: Arc<str>,
  function: Arc<RwLock<AbstractFunctionSignature>>,
  body: &'a TopLevelFunction,
  instance: Option<usize>,
}

/// Rejects copying a value holding a closure that mutates its captured
/// variables while the original stays usable (see
/// `Program::validate_copied_stateful_closures`).
struct CopyCheck<'a, 'p> {
  program: &'p Program,
  analysis: Option<&'a Analysis<'p>>,
  /// Closure scope struct name -> whether its closure's own body writes one
  /// of its captured variables.
  scope_writes: HashMap<Arc<str>, bool>,
  /// Function name -> its by-reference parameters whose value it copies out
  /// (captures, stores, or returns).
  escapes: HashMap<Arc<str>, BTreeSet<usize>>,
  report: bool,
  reported: HashSet<(String, Arc<str>)>,
  errors: Vec<CompileError>,
}

/// Per-body state of the copy check's walk.
struct CopyWalk<'b> {
  function_name: Arc<str>,
  /// Parameter index of each by-reference parameter (lent to this function).
  reference_params: HashMap<Arc<str>, usize>,
  /// Match payload bindings on place scrutinees -> the scrutinee's root.
  aliases: HashMap<Arc<str>, Arc<str>>,
  /// The body's local variables (parameters and bindings).
  variables: HashSet<Arc<str>>,
  /// Names declared inside each enclosing loop, innermost last.
  loops: Vec<HashSet<Arc<str>>>,
  instance: Option<usize>,
  slots: Option<&'b [Skel]>,
  changed: bool,
}

/// Every variable name read in `exp` (the target of a whole-variable
/// assignment is written, not read).
fn names_read(exp: &TypedExp, out: &mut HashSet<Arc<str>>) {
  match &exp.kind {
    ExpKind::Name(n) => {
      out.insert(n.clone());
    }
    ExpKind::Application(f, args)
      if is_assignment(f)
        && args.len() == 2
        && matches!(args[0].kind, ExpKind::Name(_)) =>
    {
      names_read(&args[1], out);
    }
    _ => for_each_child(exp, &mut |child| names_read(child, out)),
  }
}

/// Calls `f` on each direct child of `exp`.
fn for_each_child(exp: &TypedExp, f: &mut impl FnMut(&TypedExp)) {
  let mut is_root = true;
  let _ = exp.walk(&mut |e| {
    if is_root {
      is_root = false;
      return Ok::<bool, Never>(true);
    }
    f(e);
    Ok(false)
  });
}

fn reads_of(exps: &[&TypedExp]) -> HashSet<Arc<str>> {
  let mut out = HashSet::new();
  for e in exps {
    names_read(e, &mut out);
  }
  out
}

fn is_assignment(f: &TypedExp) -> bool {
  matches!(&f.kind, ExpKind::Name(n) if ASSIGNMENT_OPS.contains(&**n))
}

/// Whether `body` writes through a place rooted at `name`: assigns to it,
/// applies an atomic operation to it, or lends plain data rooted at it to a
/// mutable reference parameter. (Lending a function value doesn't count: a
/// closure's state is judged by its own definition.)
fn writes_through(body: &TypedExp, name: &Arc<str>) -> bool {
  let mut found = false;
  let _ = body.walk(&mut |e| {
    if let ExpKind::Application(f, args) = &e.kind {
      let rooted = |a: &TypedExp| place_root(a).as_ref() == Some(name);
      let callee_writes = matches!(&f.kind, ExpKind::Name(c)
        if ASSIGNMENT_OPS.contains(&**c) || ATOMIC_MUTATION_OPS.contains(&**c));
      if callee_writes && args.first().is_some_and(rooted) {
        found = true;
      }
      if let Some(Type::Function(sig)) = known_type(&f.data) {
        for ((param, _), a) in sig.args.iter().zip(args.iter()) {
          if param.var_type.ownership == Ownership::MutableReference
            && rooted(a)
            && !known_type(&a.data).is_some_and(|t| holds_function(&t))
          {
            found = true;
          }
        }
      }
    }
    Ok::<bool, Never>(!found)
  });
  found
}

/// The variable a place expression (a name, or field / element accesses of
/// one) is rooted at.
fn place_root(exp: &TypedExp) -> Option<Arc<str>> {
  match &exp.kind {
    ExpKind::Name(n) => Some(n.clone()),
    ExpKind::Access(Accessor::Field(_) | Accessor::ArrayIndex(_), inner) => {
      place_root(inner)
    }
    ExpKind::Application(f, _)
      if matches!(known_type(&f.data), Some(Type::Array(_, _))) =>
    {
      place_root(f)
    }
    _ => None,
  }
}

impl<'a, 'p> CopyCheck<'a, 'p> {
  /// Whether a value of type `t` (with skeleton `skel`, when boxed positions
  /// have one) holds a stateful closure: one whose body writes a captured
  /// variable, or that captures a stateful value.
  fn holds_stateful(&self, t: &Type, skel: Option<&Skel>) -> bool {
    let child_skel = |i: usize| match skel {
      Some(Skel::Struct(fs) | Skel::Enum(fs)) => fs.get(i),
      _ => None,
    };
    let writes =
      |scope: &Arc<str>| self.scope_writes.get(scope).copied().unwrap_or(false);
    match t {
      Type::Function(sig) => {
        let Some(scope) = sig.closure_scope() else {
          return false;
        };
        writes(&scope.name.0)
          || scope
            .concrete_type()
            .is_some_and(|t| self.holds_stateful(&t, skel))
      }
      Type::BoxedFunction(_) => {
        let (Some(analysis), Some(Skel::Box(n))) = (self.analysis, skel) else {
          return false;
        };
        let mut n = *n;
        while analysis.parent[n] != n {
          n = analysis.parent[n];
        }
        analysis.nodes[n].members.values().any(|m| {
          let Some(payload) = &m.payload else {
            return false;
          };
          let Some(scope) = m.function.read().unwrap().captured_scope.clone()
          else {
            return false;
          };
          writes(&scope.name.0)
            || scope
              .concrete_type()
              .is_some_and(|t| self.holds_stateful(&t, Some(payload)))
        })
      }
      Type::Struct(s) => {
        writes(&s.name)
          || s.fields.iter().enumerate().any(|(i, f)| {
            f.field_type
              .kind
              .try_unwrap_known()
              .is_some_and(|t| self.holds_stateful(&t, child_skel(i)))
          })
      }
      Type::Enum(e) => e.variants.iter().enumerate().any(|(i, v)| {
        v.inner_type
          .kind
          .try_unwrap_known()
          .is_some_and(|t| self.holds_stateful(&t, child_skel(i)))
      }),
      Type::Array(_, inner) => {
        let element = match skel {
          Some(Skel::Array(e)) => Some(&**e),
          _ => None,
        };
        inner
          .kind
          .try_unwrap_known()
          .is_some_and(|t| self.holds_stateful(&t, element))
      }
      _ => false,
    }
  }
  fn exp_holds_stateful(&self, exp: &TypedExp, walk: &CopyWalk) -> bool {
    let skel = walk
      .slots
      .zip(exp.data.defun_slot)
      .and_then(|(slots, slot)| slots.get(slot as usize));
    known_type(&exp.data).is_some_and(|t| self.holds_stateful(&t, skel))
  }
  /// `exp` (a place holding a stateful closure) is copied: an error when its
  /// source stays usable afterwards, and an escape of a lent parameter.
  fn copied(
    &mut self,
    exp: &TypedExp,
    later: &HashSet<Arc<str>>,
    overwritten: Option<&Arc<str>>,
    walk: &mut CopyWalk,
  ) {
    // A closure lent to a stored function's call is the place it borrows.
    let exp = match &exp.kind {
      ExpKind::Application(_, args) if is_function_value_borrow(exp) => {
        &args[0]
      }
      _ => exp,
    };
    let Some(name) = place_root(exp) else {
      return;
    };
    if !self.exp_holds_stateful(exp, walk) {
      return;
    }
    let root = walk.aliases.get(&name).cloned().unwrap_or(name.clone());
    if overwritten == Some(&root) {
      return;
    }
    let is_global = self.program.top_level_vars.iter().any(|v| v.name == root);
    if !is_global && !walk.variables.contains(&root) {
      // Not a variable (an enum's unit variant, a function name).
      return;
    }
    let lent = walk.reference_params.get(&root).copied();
    if let Some(i) = lent {
      let escapes = self.escapes.entry(walk.function_name.clone()).or_default();
      walk.changed |= escapes.insert(i);
    }
    let repeated = walk
      .loops
      .last()
      .is_some_and(|declared| !declared.contains(&root));
    let still_used =
      later.contains(&root) || later.contains(&name) || repeated || is_global;
    if still_used {
      self.error(&name, exp);
    }
  }
  fn error(&mut self, name: &Arc<str>, exp: &TypedExp) {
    if !self.report {
      return;
    }
    let key = (format!("{:?}", exp.source_trace), name.clone());
    if self.reported.insert(key) {
      self.errors.push(CompileError::new(
        CompileErrorKind::CopiedStatefulClosure(name.to_string()),
        exp.source_trace.clone(),
      ));
    }
  }
  /// Whether argument `i` of a call to `callee` is lent to a parameter the
  /// callee copies out.
  fn escapes_through(
    &self,
    exp: &TypedExp,
    callee_name: &Arc<str>,
    i: usize,
    walk: &CopyWalk,
  ) -> bool {
    if &**callee_name == FNBOX_APPLY {
      // A stored function's call: any member copying the parameter.
      let (Some(analysis), Some(instance), Some(slot)) =
        (self.analysis, walk.instance, exp.data.defun_slot)
      else {
        return false;
      };
      let Some(site) = analysis.instances[instance].applies.get(&slot) else {
        return false;
      };
      let mut n = analysis.sites[*site].node;
      while analysis.parent[n] != n {
        n = analysis.parent[n];
      }
      return i > 0
        && analysis.nodes[n].members.values().any(|m| {
          let member = m.function.read().unwrap().name.clone();
          self
            .escapes
            .get(&member)
            .is_some_and(|e| e.contains(&(i - 1)))
        });
    }
    self
      .escapes
      .get(callee_name)
      .is_some_and(|e| e.contains(&i))
  }
  /// Walks `exp`, where `later` holds the names read after it.
  fn visit(
    &mut self,
    exp: &TypedExp,
    later: &HashSet<Arc<str>>,
    overwritten: Option<&Arc<str>>,
    walk: &mut CopyWalk,
  ) {
    match &exp.kind {
      ExpKind::Let(bindings, body) => {
        for (i, (name, _, _, value)) in bindings.iter().enumerate() {
          let mut after: Vec<&TypedExp> =
            bindings[i + 1..].iter().map(|(_, _, _, v)| v).collect();
          after.push(body);
          let mut value_later = reads_of(&after);
          value_later.extend(later.iter().cloned());
          self.copied(value, &value_later, overwritten, walk);
          self.visit(value, &value_later, overwritten, walk);
          walk.variables.insert(name.clone());
          if let Some(declared) = walk.loops.last_mut() {
            declared.insert(name.clone());
          }
        }
        self.visit(body, later, overwritten, walk);
      }
      ExpKind::Block(exps) => {
        for (i, e) in exps.iter().enumerate() {
          let after: Vec<&TypedExp> = exps[i + 1..].iter().collect();
          let mut e_later = reads_of(&after);
          e_later.extend(later.iter().cloned());
          self.visit(e, &e_later, overwritten, walk);
        }
      }
      ExpKind::ArrayLiteral(elements) => {
        for (i, e) in elements.iter().enumerate() {
          let after: Vec<&TypedExp> = elements[i + 1..].iter().collect();
          let mut e_later = reads_of(&after);
          e_later.extend(later.iter().cloned());
          self.copied(e, &e_later, overwritten, walk);
          self.visit(e, &e_later, overwritten, walk);
        }
      }
      ExpKind::Application(f, args) if is_assignment(f) && args.len() == 2 => {
        // The value is computed before the target is written; a target
        // that's a whole variable is overwritten, which ends its old value.
        let target_root = match &args[0].kind {
          ExpKind::Name(n) => Some(n.clone()),
          _ => None,
        };
        let mut value_later = reads_of(&[&args[0]]);
        if let Some(root) = &target_root {
          value_later.remove(root);
        }
        value_later.extend(later.iter().cloned());
        let overwrite = target_root.as_ref().or(overwritten);
        self.copied(&args[1], &value_later, overwrite, walk);
        self.visit(&args[1], &value_later, overwrite, walk);
        self.visit(&args[0], later, overwritten, walk);
      }
      ExpKind::Application(f, args) => {
        self.visit(f, later, overwritten, walk);
        let (callee_name, ownerships) = match known_type(&f.data) {
          Some(Type::Function(sig)) => {
            // A boxed parameter of a (boxed-parameter clone) function is
            // lent, though it's declared owned until lowering; a
            // constructor's boxed field is stored.
            let composite = sig.abstract_ancestor.as_ref().is_some_and(|a| {
              matches!(
                a.read().unwrap().implementation,
                FunctionImplementationKind::Composite(_)
              )
            });
            let declared: Vec<Ownership> = sig
              .abstract_ancestor
              .as_ref()
              .map(|a| {
                a.read()
                  .unwrap()
                  .arg_types
                  .iter()
                  .map(|(_, o)| *o)
                  .collect()
              })
              .unwrap_or_default();
            let ownerships: Vec<Ownership> = sig
              .args
              .iter()
              .enumerate()
              .map(|(i, (param, _))| {
                let boxed_param = composite
                  && matches!(
                    known_type(&param.var_type),
                    Some(Type::BoxedFunction(_))
                  );
                match declared.get(i) {
                  _ if boxed_param => Ownership::MutableReference,
                  Some(Ownership::MutableReference) => {
                    Ownership::MutableReference
                  }
                  Some(Ownership::Reference) => Ownership::Reference,
                  _ => param.var_type.ownership,
                }
              })
              .collect();
            let name = sig
              .abstract_ancestor
              .as_ref()
              .map(|a| a.read().unwrap().name.clone());
            (name, ownerships)
          }
          _ => (None, vec![]),
        };
        // A builtin keeps an owned argument only when it can return it (an
        // array utility, boxing) or is a host taking a function to run.
        let is_builtin = matches!(known_type(&f.data), Some(Type::Function(sig))
          if sig.abstract_ancestor.as_ref().is_some_and(|a| matches!(
            a.read().unwrap().implementation,
            FunctionImplementationKind::Builtin { .. })));
        let builtin_keeps_arguments = !is_builtin
          || callee_name
            .as_ref()
            .is_some_and(|n| HOST_BUILTINS.contains(&&**n))
          || self.exp_holds_stateful(exp, walk);
        for (i, a) in args.iter().enumerate() {
          let after: Vec<&TypedExp> = args[i + 1..].iter().collect();
          let mut a_later = reads_of(&after);
          a_later.extend(later.iter().cloned());
          let lent = matches!(
            ownerships.get(i),
            Some(Ownership::MutableReference | Ownership::Reference)
          );
          let copied_out = callee_name
            .as_ref()
            .is_some_and(|n| self.escapes_through(exp, n, i, walk));
          if (!lent && builtin_keeps_arguments) || copied_out {
            self.copied(a, &a_later, overwritten, walk);
          }
          self.visit(a, &a_later, overwritten, walk);
        }
      }
      ExpKind::Match(scrutinee, arms) => {
        let arm_values: Vec<&TypedExp> = arms.iter().map(|(_, v)| v).collect();
        let mut scrutinee_later = reads_of(&arm_values);
        scrutinee_later.extend(later.iter().cloned());
        self.visit(scrutinee, &scrutinee_later, overwritten, walk);
        let scrutinee_root = place_root(scrutinee)
          .map(|r| walk.aliases.get(&r).cloned().unwrap_or(r));
        for (pattern, value) in arms.iter() {
          if let ExpKind::Application(_, bound) = &pattern.kind {
            for b in bound {
              if let ExpKind::Name(n) = &b.kind {
                walk.variables.insert(n.clone());
                match &scrutinee_root {
                  Some(root) => {
                    walk.aliases.insert(n.clone(), root.clone());
                  }
                  None => {
                    if let Some(declared) = walk.loops.last_mut() {
                      declared.insert(n.clone());
                    }
                  }
                }
              }
            }
          }
          self.visit(value, later, overwritten, walk);
        }
      }
      ExpKind::ForLoop {
        increment_variable_name,
        increment_variable_initial_value_expression,
        continue_condition_expression,
        update_expression,
        body_expression,
        ..
      } => {
        self.visit(
          increment_variable_initial_value_expression,
          later,
          overwritten,
          walk,
        );
        let mut parts: Vec<&TypedExp> =
          vec![continue_condition_expression, body_expression];
        if let Some(u) = update_expression {
          parts.push(u);
        }
        let mut loop_later = reads_of(&parts);
        loop_later.extend(later.iter().cloned());
        walk.variables.insert(increment_variable_name.0.clone());
        walk
          .loops
          .push(std::iter::once(increment_variable_name.0.clone()).collect());
        for part in parts {
          self.visit(part, &loop_later, overwritten, walk);
        }
        walk.loops.pop();
      }
      ExpKind::WhileLoop {
        condition_expression,
        body_expression,
      } => {
        let mut loop_later = reads_of(&[condition_expression, body_expression]);
        loop_later.extend(later.iter().cloned());
        walk.loops.push(HashSet::new());
        self.visit(condition_expression, &loop_later, overwritten, walk);
        self.visit(body_expression, &loop_later, overwritten, walk);
        walk.loops.pop();
      }
      ExpKind::Return(value) => {
        // Returning a lent parameter's value copies it out to the caller.
        self.copied(value, &HashSet::new(), overwritten, walk);
        self.visit(value, &HashSet::new(), overwritten, walk);
      }
      _ => for_each_child(exp, &mut |child| {
        self.visit(child, later, overwritten, walk)
      }),
    }
  }
}

/// Every name bound in `exp` (let bindings, match pattern bindings, loop
/// variables), with repeats.
#[cfg(debug_assertions)]
fn bound_names(exp: &TypedExp, out: &mut Vec<Arc<str>>) {
  let _ = exp.walk(&mut |e| {
    match &e.kind {
      ExpKind::Let(bindings, _) => {
        out.extend(bindings.iter().map(|(n, _, _, _)| n.clone()))
      }
      ExpKind::Match(_, arms) => {
        for (pattern, _) in arms {
          if let ExpKind::Application(_, bound) = &pattern.kind {
            for b in bound {
              if let ExpKind::Name(n) = &b.kind {
                out.push(n.clone());
              }
            }
          }
        }
      }
      ExpKind::ForLoop {
        increment_variable_name,
        ..
      } => out.push(increment_variable_name.0.clone()),
      _ => {}
    }
    Ok::<bool, Never>(true)
  });
}

impl<'a, 'p> CopyCheck<'a, 'p> {
  /// Checks one body; returns whether its escape summary grew.
  fn check(&mut self, checked: &CheckedBody) -> bool {
    let ExpKind::Function(arg_names, body) = &checked.body.expression.kind
    else {
      return false;
    };
    // The walk tracks variables by name, relying on deshadowing's invariant
    // (every local name bound once per function) surviving the passes since.
    #[cfg(debug_assertions)]
    {
      let mut names: Vec<Arc<str>> =
        arg_names.iter().map(|(n, _)| n.clone()).collect();
      bound_names(body, &mut names);
      let mut seen = HashSet::new();
      for n in names {
        debug_assert!(
          seen.insert(n.clone()),
          "`{n}` is bound twice in `{}`; deshadowing's one-binding-per-name \
           invariant must hold for the copied-stateful-closure check",
          checked.name
        );
      }
    }
    let Some(Type::Function(sig)) = known_type(&checked.body.expression.data)
    else {
      return false;
    };
    let declared: Vec<Ownership> = checked
      .function
      .read()
      .unwrap()
      .arg_types
      .iter()
      .map(|(_, o)| *o)
      .collect();
    let reference_params = arg_names
      .iter()
      .enumerate()
      .filter(|(i, _)| {
        let boxed = sig.args.get(*i).is_some_and(|(a, _)| {
          matches!(known_type(&a.var_type), Some(Type::BoxedFunction(_)))
        });
        boxed
          || matches!(
            declared.get(*i),
            Some(Ownership::MutableReference | Ownership::Reference)
          )
      })
      .map(|(i, (n, _))| (n.clone(), i))
      .collect();
    let slots = checked
      .instance
      .and_then(|i| self.analysis.map(|a| a.instances[i].slots.as_slice()));
    let mut walk = CopyWalk {
      function_name: checked.name.clone(),
      reference_params,
      aliases: HashMap::new(),
      variables: arg_names.iter().map(|(n, _)| n.clone()).collect(),
      loops: vec![],
      instance: checked.instance,
      slots,
      changed: false,
    };
    self.visit(body, &HashSet::new(), None, &mut walk);
    // The body's value is returned.
    let mut tail = &**body;
    loop {
      match &tail.kind {
        ExpKind::Let(_, b) => tail = b,
        ExpKind::Block(es) if !es.is_empty() => tail = es.last().unwrap(),
        _ => break,
      }
    }
    self.copied(tail, &HashSet::new(), None, &mut walk);
    walk.changed
  }
}

impl Program {
  /// Rejects copying a value that holds a closure mutating its captured
  /// variables while the original stays usable — binding it to a second
  /// name, storing it in an array / struct / global, capturing it in another
  /// closure, or passing it by value, when the original (or, for a function
  /// parameter it was lent through, the caller's value) is used again. Such
  /// copies would advance separately, and a closure's captured variables
  /// are meant to be shared by every reference to the closure (not yet
  /// implemented), so the copy is an error rather than a silent divergence.
  /// Programs this accepts behave the same under both readings.
  ///
  /// Runs on the post-inlining bodies (where each closure has one static
  /// identity and a function's closure parameters are its callers' specific
  /// closures), with `analysis` supplying boxed positions' member sets when
  /// function values are being lowered. Passing a closure to a function
  /// parameter lends it; a parameter the callee copies out (captures,
  /// stores, or returns) is summarized per function and treated as a copy at
  /// its call sites.
  fn validate_copied_stateful_closures(
    &self,
    analysis: Option<&Analysis>,
    errors: &mut ErrorLog,
  ) {
    let mut scope_writes: HashMap<Arc<str>, bool> = HashMap::new();
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      let (Some(scope), FunctionImplementationKind::Composite(implementation)) =
        (&f.captured_scope, &f.implementation)
      else {
        continue;
      };
      let implementation = implementation.read().unwrap();
      let ExpKind::Function(arg_names, body) = &implementation.expression.kind
      else {
        continue;
      };
      let writes = arg_names
        .last()
        .is_some_and(|(scope_name, _)| writes_through(body, scope_name));
      *scope_writes.entry(scope.name.0.clone()).or_insert(false) |= writes;
    }
    if !scope_writes.values().any(|w| *w) {
      return;
    }
    let registry: Vec<(
      Arc<RwLock<AbstractFunctionSignature>>,
      Arc<RwLock<TopLevelFunction>>,
    )> = self
      .abstract_functions_iter()
      .filter_map(|f| {
        let r = f.read().unwrap();
        match &r.implementation {
          FunctionImplementationKind::Composite(i)
            if r.generic_args.is_empty()
              && !r.has_uninlined_higher_order_arguments() =>
          {
            Some((f.clone(), i.clone()))
          }
          _ => None,
        }
      })
      .collect();
    let registry_guards: Vec<_> =
      registry.iter().map(|(_, i)| i.read().unwrap()).collect();
    let bodies: Vec<CheckedBody> = match analysis {
      Some(analysis) => analysis
        .instances
        .iter()
        .enumerate()
        .filter_map(|(i, instance)| match &instance.body {
          InstanceBody::Function(body) => {
            let function = instance.function.clone()?;
            let name = function.read().unwrap().name.clone();
            Some(CheckedBody {
              name,
              function,
              body,
              instance: Some(i),
            })
          }
          _ => None,
        })
        .collect(),
      None => registry
        .iter()
        .zip(registry_guards.iter())
        .map(|((f, _), body)| CheckedBody {
          name: f.read().unwrap().name.clone(),
          function: f.clone(),
          body,
          instance: None,
        })
        .collect(),
    };
    let mut check = CopyCheck {
      program: self,
      analysis,
      scope_writes,
      escapes: HashMap::new(),
      report: false,
      reported: HashSet::new(),
      errors: vec![],
    };
    loop {
      let mut changed = false;
      for body in bodies.iter() {
        changed |= check.check(body);
      }
      if !changed {
        break;
      }
    }
    check.report = true;
    for body in bodies.iter() {
      check.check(body);
    }
    for e in check.errors {
      errors.log(e);
    }
  }
}
