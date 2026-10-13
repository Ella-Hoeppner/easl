//! Closures the host runs outside the main thread's code — dispatched to
//! the GPU, or handed to the audio thread by `start-audio` — and the passes
//! their lifting relies on.
//!
//! Such a closure can't receive its captured scope as an argument (a GPU
//! entry point takes no arguments, and the audio thread runs the closure
//! with no caller), so `Program::lift_closure_captures` moves each capture
//! into a global of its own and rewrites the closure's body to read those.
//! Everything else stays as it is: a captured closure's global holds its
//! scope, and calls pass that global by reference like any other global
//! (storage-ref inlining substitutes it, or a field path into it, for the
//! parameter), so a captured closure can be called, passed on, or captured
//! again exactly as on the CPU. On the GPU a binding's struct can't hold a
//! runtime-sized array, so those are split out into globals of their own
//! (`SplitGlobal`).

use std::collections::{HashMap, HashSet};
use std::iter;
use std::sync::{Arc, RwLock};

use crate::Never;
use crate::compiler::{
  entry::IOAttributes,
  error::{CompileError, CompileErrorKind::*, ErrorLog, SourceTrace},
  expression::{Accessor, ExpKind, TypedExp},
  functions::{
    AbstractFunctionSignature, FunctionImplementationKind, Ownership,
    RefMutability, TopLevelFunction,
  },
  program::{NameContext, Program},
  structs::{AbstractStruct, AbstractStructField},
  types::{AbstractType, ConcreteArraySize, Type, TypeState},
  vars::{
    BindingSpec, TopLevelVar, TopLevelVariableKind, VariableAddressSpace,
  },
};

/// The variable an argument place is rooted at: `p` for `p`, `p.field`, or
/// an element of `p`.
fn place_root(exp: &TypedExp) -> Option<&Arc<str>> {
  match &exp.kind {
    ExpKind::Name(name) => Some(name),
    ExpKind::Access(Accessor::Field(_) | Accessor::ArrayIndex(_), inner) => {
      place_root(inner)
    }
    ExpKind::Application(f, _)
      if f.data.is_known_and(|t| matches!(t, Type::Array(_, _))) =>
    {
      place_root(f)
    }
    _ => None,
  }
}

fn is_mutable(ownership: Ownership) -> bool {
  matches!(
    ownership,
    Ownership::MutableReference | Ownership::Pointer(_, RefMutability::Mutable)
  )
}

fn implementation_key(implementation: &Arc<RwLock<TopLevelFunction>>) -> usize {
  Arc::as_ptr(implementation) as usize
}

/// A function's declared-mutable reference parameters to closures, by
/// position in its own (value-level) function type.
struct ClosureReferenceParams {
  implementation: Arc<RwLock<TopLevelFunction>>,
  params: Vec<(usize, Arc<str>)>,
}

impl Program {
  /// Closure-reference parameters — a closure's scope parameter, and a
  /// higher-order specialization's closure-valued parameter — are declared
  /// mutable references: a stateful closure mutates its captured variables
  /// through its scope, and a specialization lends its closure argument for
  /// that. This makes each one a plain reference unless its function
  /// actually writes through it (directly, or by passing it to a builtin's
  /// mutable parameter or to a function that writes through its own), so
  /// effects — CPU write marks, GPU readbacks, shared-dirty marks — follow
  /// actual writes, and a closure that never mutates its scope can be
  /// called on scope data in read-only storage.
  pub fn refine_closure_reference_mutability(&mut self) {
    let scope_structs: HashSet<Arc<str>> = self
      .abstract_functions_iter()
      .filter_map(|f| {
        f.read()
          .unwrap()
          .captured_scope
          .as_ref()
          .map(|scope| scope.name.0.clone())
      })
      .collect();
    if scope_structs.is_empty() {
      return;
    }
    let is_closure = |t: &Type| match t {
      Type::Function(signature) => signature
        .abstract_ancestor
        .as_ref()
        .is_some_and(|ancestor| {
          ancestor.read().unwrap().captured_scope.is_some()
        }),
      Type::Struct(s) => scope_structs.contains(&s.name),
      _ => false,
    };
    let mut candidates: HashMap<usize, ClosureReferenceParams> = HashMap::new();
    for f in self.abstract_functions_iter() {
      let FunctionImplementationKind::Composite(implementation) =
        f.read().unwrap().implementation.clone()
      else {
        continue;
      };
      let key = implementation_key(&implementation);
      if candidates.contains_key(&key) {
        continue;
      }
      let params = {
        let function = implementation.read().unwrap();
        let Some(Type::Function(signature)) =
          function.expression.data.kind.try_unwrap_known()
        else {
          continue;
        };
        signature
          .args
          .iter()
          .enumerate()
          .filter(|(_, (param, _))| {
            param.var_type.ownership == Ownership::MutableReference
              && param.var_type.is_known_and(&is_closure)
          })
          .map(|(i, _)| (i, function.arg_names[i].0.clone()))
          .collect::<Vec<_>>()
      };
      if !params.is_empty() {
        candidates.insert(
          key,
          ClosureReferenceParams {
            implementation,
            params,
          },
        );
      }
    }
    // Which candidate parameters each function writes through, by position:
    // a least fixpoint over the call graph.
    let mut written: HashMap<usize, HashSet<usize>> = HashMap::new();
    loop {
      let mut changed = false;
      for (&key, candidate) in candidates.iter() {
        let positions: HashMap<&Arc<str>, usize> = candidate
          .params
          .iter()
          .map(|(i, name)| (name, *i))
          .collect();
        let function = candidate.implementation.read().unwrap();
        let mut writes: Vec<usize> = vec![];
        function
          .expression
          .walk(&mut |exp| {
            if let ExpKind::Application(f, args) = &exp.kind
              && let Some(Type::Function(view)) = f.data.kind.try_unwrap_known()
            {
              let callee =
                view.abstract_ancestor.as_ref().and_then(|ancestor| {
                  match &ancestor.read().unwrap().implementation {
                    FunctionImplementationKind::Composite(callee) => {
                      Some(callee.clone())
                    }
                    _ => None,
                  }
                });
              // Which of the callee's parameters are mutable references:
              // a composite callee's own declaration (a call's view of it
              // can be less precise), a builtin's view.
              let mutable_params: Vec<bool> = match &callee {
                Some(callee) => {
                  match callee
                    .read()
                    .unwrap()
                    .expression
                    .data
                    .kind
                    .try_unwrap_known()
                  {
                    Some(Type::Function(declared)) => declared
                      .args
                      .iter()
                      .map(|(param, _)| is_mutable(param.var_type.ownership))
                      .collect(),
                    _ => vec![],
                  }
                }
                None => view
                  .args
                  .iter()
                  .map(|(param, _)| is_mutable(param.var_type.ownership))
                  .collect(),
              };
              for (j, arg) in args.iter().enumerate() {
                if !mutable_params.get(j).copied().unwrap_or(false) {
                  continue;
                }
                let Some(&i) = place_root(arg).and_then(|r| positions.get(r))
                else {
                  continue;
                };
                // A callee writes through a mutable parameter unless it's a
                // candidate found not to.
                let callee_writes = match &callee {
                  Some(callee) => {
                    let callee_key = implementation_key(callee);
                    match candidates.get(&callee_key) {
                      Some(callee_candidate)
                        if callee_candidate
                          .params
                          .iter()
                          .any(|(p, _)| *p == j) =>
                      {
                        written.get(&callee_key).is_some_and(|w| w.contains(&j))
                      }
                      _ => true,
                    }
                  }
                  None => true,
                };
                if callee_writes {
                  writes.push(i);
                }
              }
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
        let entry = written.entry(key).or_default();
        for i in writes {
          changed |= entry.insert(i);
        }
      }
      if !changed {
        break;
      }
    }
    let unwritten: HashMap<usize, Vec<(usize, Arc<str>)>> = candidates
      .iter()
      .filter_map(|(key, candidate)| {
        let params: Vec<_> = candidate
          .params
          .iter()
          .filter(|(i, _)| !written.get(key).is_some_and(|w| w.contains(i)))
          .cloned()
          .collect();
        (!params.is_empty()).then_some((*key, params))
      })
      .collect();
    if unwritten.is_empty() {
      return;
    }
    // The declarations: each function's own type, its body's uses of the
    // parameter, and every registered signature sharing its implementation.
    for (key, params) in unwritten.iter() {
      let candidate = &candidates[key];
      let mut function = candidate.implementation.write().unwrap();
      let Some(Type::Function(mut signature)) =
        function.expression.data.kind.try_unwrap_known()
      else {
        unreachable!()
      };
      for (i, _) in params {
        signature.args[*i].0.var_type.ownership = Ownership::Reference;
      }
      function.expression.data.kind =
        TypeState::Known(Type::Function(signature));
      let names: HashSet<&Arc<str>> = params.iter().map(|(_, n)| n).collect();
      function
        .expression
        .walk_mut(&mut |exp| {
          if let ExpKind::Name(name) = &exp.kind
            && names.contains(name)
            && exp.data.ownership == Ownership::MutableReference
          {
            exp.data.ownership = Ownership::Reference;
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
    for f in self.abstract_functions_iter() {
      let mut signature = f.write().unwrap();
      let FunctionImplementationKind::Composite(implementation) =
        &signature.implementation
      else {
        continue;
      };
      let Some(params) = unwritten.get(&implementation_key(implementation))
      else {
        continue;
      };
      let positions: HashSet<usize> = params.iter().map(|(i, _)| *i).collect();
      Self::refine_declared_arg_ownerships(
        &self.names,
        &mut signature,
        &positions,
      );
    }
    // Every call's view of a refined function.
    let refine_views = |exp: &mut TypedExp| {
      exp
        .walk_mut(&mut |exp| {
          if let ExpKind::Application(f, _) = &mut exp.kind
            && let Some(Type::Function(mut view)) =
              f.data.kind.try_unwrap_known()
            && let Some(params) =
              view.abstract_ancestor.as_ref().and_then(|ancestor| {
                match &ancestor.read().unwrap().implementation {
                  FunctionImplementationKind::Composite(callee) => {
                    unwritten.get(&implementation_key(callee))
                  }
                  _ => None,
                }
              })
          {
            for (i, _) in params {
              if let Some((param, _)) = view.args.get_mut(*i)
                && param.var_type.ownership == Ownership::MutableReference
              {
                param.var_type.ownership = Ownership::Reference;
              }
            }
            // The view's ancestor can be a signature copy outside the
            // registry; ownership validation reads its declarations.
            let positions: HashSet<usize> =
              params.iter().map(|(i, _)| *i).collect();
            if let Some(ancestor) = &view.abstract_ancestor {
              Self::refine_declared_arg_ownerships(
                &self.names,
                &mut ancestor.write().unwrap(),
                &positions,
              );
            }
            f.data.kind = TypeState::Known(Type::Function(view));
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    };
    let mut seen: HashSet<usize> = HashSet::new();
    for f in self.abstract_functions_iter() {
      let FunctionImplementationKind::Composite(implementation) =
        f.read().unwrap().implementation.clone()
      else {
        continue;
      };
      if seen.insert(implementation_key(&implementation)) {
        refine_views(&mut implementation.write().unwrap().expression);
      }
    }
    for var in self.top_level_vars.iter_mut() {
      if let Some(value) = &mut var.value {
        refine_views(value);
      }
    }
  }
  /// Marks the declared parameters at value-level `positions` of
  /// `signature` as plain references. `arg_types` can keep phantom entries
  /// for unit-like parameters the value-level lists no longer carry, so
  /// positions are mapped past those.
  fn refine_declared_arg_ownerships(
    names: &RwLock<NameContext>,
    signature: &mut AbstractFunctionSignature,
    positions: &HashSet<usize>,
  ) {
    let mut names = names.write().unwrap();
    let mut value_position = 0;
    for (t, ownership) in signature.arg_types.iter_mut() {
      if t.is_unitlike(&mut names) {
        continue;
      }
      if positions.contains(&value_position)
        && *ownership == Ownership::MutableReference
      {
        *ownership = Ownership::Reference;
      }
      value_position += 1;
    }
  }
}

/// A global whose runtime-sized arrays — anywhere inside it, through struct
/// fields and closure scopes — live in globals of their own (`parts`). A
/// GPU binding's struct can't hold one (WGSL allows an unsized array only
/// as the last member of a top-level storage struct, so two of them, or a
/// nested one, can't be expressed), so the global itself holds just the
/// rest. Accesses through a part's path are redirected to the part's global
/// (`TypedExp::resolve_split_globals`), also inside functions the global is
/// passed to by reference, once storage-ref inlining has substituted it for
/// the parameter. Nothing about this is specific to closures: it applies to
/// any struct-typed global.
#[derive(Debug, Clone, PartialEq)]
pub struct SplitGlobal {
  /// The type of what the global itself holds, if anything's left.
  pub rest_type: Option<Type>,
  pub parts: Vec<SplitPart>,
}

/// One runtime-sized array split out of a `SplitGlobal`.
#[derive(Debug, Clone, PartialEq)]
pub struct SplitPart {
  /// The field names leading to it from the global's value.
  pub path: Vec<Arc<str>>,
  pub global: Arc<str>,
  pub part_type: Type,
}

/// Where one capture of a lifted closure lives: the global holding its
/// value (a captured closure's value is its scope), split when the GPU
/// requires it.
#[derive(Debug, Clone, PartialEq)]
pub struct LiftedCapture {
  pub global: Arc<str>,
  /// The captured value's type, a closure's as its scope struct.
  pub value_type: Type,
  /// The runtime-sized arrays split out (GPU captures only); when there are
  /// none, the global holds the whole value.
  pub split: Option<SplitGlobal>,
}

impl LiftedCapture {
  /// Whether the capture's own global exists (a split can leave it empty).
  pub fn has_global(&self) -> bool {
    self
      .split
      .as_ref()
      .is_none_or(|split| split.rest_type.is_some())
  }
}

/// The record of one closure entry's lifted captures, stored in
/// `Program::lifted_audio_captures` / `lifted_gpu_captures` by the lift and
/// read by the runtimes, which write each capture's value into its globals
/// at every dispatch (GPU) or `start-audio` handoff (audio). Positionally
/// aligned with the entry's scope struct fields.
#[derive(Debug, Clone, PartialEq)]
pub struct LiftedCaptures {
  pub fields: Vec<LiftedCapture>,
}

impl LiftedCaptures {
  /// Every global the captures live in.
  pub fn global_names(&self, out: &mut Vec<Arc<str>>) {
    for capture in &self.fields {
      if capture.has_global() {
        out.push(capture.global.clone());
      }
      if let Some(split) = &capture.split {
        out.extend(split.parts.iter().map(|part| part.global.clone()));
      }
    }
  }
}

/// The two host-invoked closure-entry systems differ only in what a capture
/// becomes:
///
/// - **GpuDispatch**: captures are per-dispatch *inputs* — implicit
///   read-only storage bindings (elided numbers), written and uploaded at
///   every dispatch; mutating one is a compile error
///   (`catch_dispatched_closure_scope_mutations`). Read-only storage rather
///   than uniform because storage has relaxed layout rules and permits
///   runtime-sized arrays, which are split out (`SplitGlobal`).
/// - **Audio**: captures are owned mutable *state* — ordinary storage-write
///   globals, shared between main and audio through the standard
///   usage-derived analysis (main's seed write at each `start-audio`
///   handoff is visible as a `SeedsGlobalVar` effect on the call site),
///   thereafter mutated per sample by the audio thread and synced at batch
///   boundaries. No GPU entry references them, so they never become runtime
///   GPU bindings.
#[derive(Clone, Copy, PartialEq)]
pub(crate) enum ClosureLiftTarget {
  GpuDispatch,
  Audio,
}

impl ClosureLiftTarget {
  /// A readable base for a capture global's name; `gensym` makes it unique,
  /// and the result is recorded in the `LiftedCaptures`, never re-derived.
  fn capture_global_name(&self, entry_scope_name: &str, field: &str) -> String {
    match self {
      ClosureLiftTarget::GpuDispatch => {
        format!("{entry_scope_name}_data_{field}")
      }
      ClosureLiftTarget::Audio => {
        format!("{entry_scope_name}_audio_data_{field}")
      }
    }
  }
  fn capture_address_space(&self) -> VariableAddressSpace {
    match self {
      ClosureLiftTarget::GpuDispatch => VariableAddressSpace::StorageRead,
      ClosureLiftTarget::Audio => VariableAddressSpace::StorageReadWrite,
    }
  }
}

/// What a closure-entry lift pass creates, installed when it ends.
#[derive(Default)]
pub(crate) struct CaptureLift {
  pub(crate) new_vars: Vec<TopLevelVar>,
  pub(crate) new_structs: Vec<AbstractStruct>,
  pub(crate) split_globals: Vec<(Arc<str>, SplitGlobal)>,
}

impl CaptureLift {
  pub(crate) fn install(self, program: &mut Program) {
    program.top_level_vars.extend(self.new_vars);
    for s in self.new_structs {
      program.add_monomorphized_struct(s);
    }
    program.split_globals.extend(self.split_globals);
  }
}

/// Whether a value of type `t` holds a runtime-sized array, looking through
/// struct fields, enum payloads, fixed-size arrays, and closure scopes.
fn holds_runtime_sized(t: &Type) -> bool {
  match t {
    Type::Array(Some(ConcreteArraySize::Unsized), _) => true,
    Type::Array(_, inner) => inner.kind.is_known_and(holds_runtime_sized),
    Type::Struct(s) => s
      .fields
      .iter()
      .any(|field| field.field_type.is_known_and(holds_runtime_sized)),
    Type::Enum(e) => e
      .variants
      .iter()
      .any(|variant| variant.inner_type.is_known_and(holds_runtime_sized)),
    Type::Function(_) => t
      .closure_data_type()
      .is_some_and(|data| holds_runtime_sized(&data)),
    _ => false,
  }
}

impl Program {
  /// Splits the runtime-sized arrays out of a global named `global` of type
  /// `t` (see `SplitGlobal`): each part gets a global named after `global`
  /// and its path, and the rest a new struct type (pushed to
  /// `new_structs`) for each struct it removes fields from. `None` when an
  /// array sits somewhere a split can't reach — an enum payload or a
  /// fixed-size array's elements.
  pub(crate) fn split_runtime_sized(
    &self,
    global: &Arc<str>,
    t: &Type,
    source_trace: &SourceTrace,
    new_structs: &mut Vec<AbstractStruct>,
  ) -> Option<SplitGlobal> {
    let mut parts = vec![];
    let rest_type =
      self.split_type(t, &mut vec![], &mut parts, source_trace, new_structs)?;
    let parts = parts
      .into_iter()
      .map(|(path, part_type): (Vec<Arc<str>>, Type)| {
        let base = iter::once(global.to_string())
          .chain(path.iter().map(|field| field.to_string()))
          .collect::<Vec<_>>()
          .join("_");
        SplitPart {
          global: self.names.write().unwrap().gensym(&base),
          path,
          part_type,
        }
      })
      .collect();
    Some(SplitGlobal { rest_type, parts })
  }
  /// The type of what's left of a `t` once its runtime-sized arrays are
  /// split out (`None` when nothing is), collecting each array's path.
  fn split_type(
    &self,
    t: &Type,
    path: &mut Vec<Arc<str>>,
    parts: &mut Vec<(Vec<Arc<str>>, Type)>,
    source_trace: &SourceTrace,
    new_structs: &mut Vec<AbstractStruct>,
  ) -> Option<Option<Type>> {
    if let Type::Array(Some(ConcreteArraySize::Unsized), _) = t {
      parts.push((path.clone(), t.clone()));
      return Some(None);
    }
    if !holds_runtime_sized(t) {
      return Some(Some(t.clone()));
    }
    let Type::Struct(s) = t.closure_data_type().unwrap_or_else(|| t.clone())
    else {
      return None;
    };
    let mut kept_fields = vec![];
    for field in s.fields.iter() {
      path.push(field.name.clone());
      let rest = self.split_type(
        &field.field_type.unwrap_known(),
        path,
        parts,
        source_trace,
        new_structs,
      )?;
      path.pop();
      if let Some(rest) = rest {
        kept_fields.push(AbstractStructField {
          attributes: IOAttributes::empty(source_trace.clone()),
          name: field.name.clone(),
          field_type: AbstractType::Type(rest),
          source_trace: source_trace.clone(),
        });
      }
    }
    if kept_fields.is_empty() {
      return Some(None);
    }
    let rest_struct = AbstractStruct {
      name: (
        self
          .names
          .write()
          .unwrap()
          .gensym(&format!("{}_rest", s.name)),
        source_trace.clone(),
      ),
      filled_generics: HashMap::new(),
      fields: kept_fields,
      generic_args: vec![],
      abstract_ancestor: None,
      source_trace: source_trace.clone(),
      opaque: false,
    };
    let rest_type = AbstractType::AbstractStruct(Arc::new(rest_struct.clone()))
      .concretize(&vec![], &self.typedefs, source_trace.clone())
      .expect("a split struct's remaining fields are concrete");
    new_structs.push(rest_struct);
    Some(Some(rest_type))
  }
  /// Lifts a closure entry's captures out of its scope parameter: each
  /// field of `scope_struct` becomes a global holding the captured value
  /// (see the module docs), and `body`'s accesses of the scope (named
  /// `scope_param`) read those globals. On the GPU, runtime-sized arrays
  /// are split out of each capture (`SplitGlobal`).
  pub(crate) fn lift_closure_captures(
    &self,
    scope_struct: &AbstractStruct,
    scope_param: &Arc<str>,
    body: &mut TypedExp,
    target: ClosureLiftTarget,
    source_trace: &SourceTrace,
    lift: &mut CaptureLift,
    errors: &mut ErrorLog,
  ) -> LiftedCaptures {
    let mut fields = Vec::with_capacity(scope_struct.fields.len());
    let mut globals: HashMap<Arc<str>, Arc<str>> = HashMap::new();
    let mut split_globals: HashMap<Arc<str>, SplitGlobal> = HashMap::new();
    for field in scope_struct.fields.iter() {
      let AbstractType::Type(field_type) = &field.field_type else {
        panic!("captured scope field with non-concrete type")
      };
      let value_type = field_type
        .closure_data_type()
        .unwrap_or_else(|| field_type.clone());
      let global: Arc<str> =
        self.names.write().unwrap().gensym(
          &target.capture_global_name(&scope_struct.name.0, &field.name),
        );
      // A runtime-sized array is a binding of its own; only arrays inside
      // the value need splitting out.
      let embeds_runtime_sized = holds_runtime_sized(&value_type)
        && !matches!(
          value_type,
          Type::Array(Some(ConcreteArraySize::Unsized), _)
        );
      let split = match target {
        ClosureLiftTarget::GpuDispatch if embeds_runtime_sized => {
          match self.split_runtime_sized(
            &global,
            &value_type,
            source_trace,
            &mut lift.new_structs,
          ) {
            Some(split) => Some(split),
            None => {
              errors.log(CompileError::new(
                RuntimeSizedFieldInBinding,
                source_trace.clone(),
              ));
              None
            }
          }
        }
        _ => None,
      };
      if target == ClosureLiftTarget::Audio && value_type.involves_string() {
        // Audio-mode compilation filters String-using functions, so an
        // audio body could never read one.
        errors.log(CompileError::new(
          UnshareableAudioCapture("String".to_string()),
          source_trace.clone(),
        ));
      }
      let capture_var = |name: &Arc<str>, var_type: Type| TopLevelVar {
        name: name.clone(),
        kind: TopLevelVariableKind::Var {
          address_space: target.capture_address_space(),
          // Binding numbers are assigned centrally at the end of
          // validation.
          group_and_binding: Some(BindingSpec::Elided),
        },
        var_type,
        value: None,
        source_trace: source_trace.clone(),
        external: false,
        directly_user_written: false,
      };
      match &split {
        Some(split) => {
          if let Some(rest_type) = &split.rest_type {
            lift.new_vars.push(capture_var(&global, rest_type.clone()));
          }
          for part in split.parts.iter() {
            lift
              .new_vars
              .push(capture_var(&part.global, part.part_type.clone()));
          }
          split_globals.insert(global.clone(), split.clone());
          lift.split_globals.push((global.clone(), split.clone()));
        }
        None => lift.new_vars.push(capture_var(&global, value_type.clone())),
      }
      globals.insert(field.name.clone(), global.clone());
      fields.push(LiftedCapture {
        global,
        value_type,
        split,
      });
    }
    body
      .walk_mut(&mut |e| {
        // Accessor/lookup nodes over a lifted global are globally bound,
        // like those inference produces for an ordinary global (the
        // mutable-ref effect derivation reads the *outer* node's flag, so
        // a stale local flag would hide a write from GPU sync and
        // shared-dirty marking).
        let scope_rooted = {
          let mut root = &*e;
          loop {
            match &root.kind {
              ExpKind::Access(_, inner) => root = inner,
              // An array-lookup application is rooted at its callee.
              ExpKind::Application(callee, _) => root = callee,
              ExpKind::Name(name) => break name == scope_param,
              _ => break false,
            }
          }
        };
        if let ExpKind::Access(Accessor::Field(field_name), inner) = &e.kind
          && matches!(&inner.kind, ExpKind::Name(name) if name == scope_param)
        {
          let global = globals.get(field_name).unwrap_or_else(|| {
            panic!("lifted scope field `{field_name}` has no global")
          });
          e.kind = ExpKind::Name(global.clone());
          e.data.is_globally_bound = true;
          e.data.ownership = Ownership::Owned;
        } else if matches!(&e.kind, ExpKind::Name(name) if name == scope_param)
        {
          panic!(
            "a lifted closure's body uses its whole captured scope as a \
             value; its captures are lifted to per-field globals, which \
             only supports field accesses"
          );
        } else if scope_rooted
          && matches!(
            e.kind,
            ExpKind::Access(_, _) | ExpKind::Application(_, _)
          )
        {
          e.data.is_globally_bound = true;
          if matches!(
            e.data.ownership,
            Ownership::Reference | Ownership::MutableReference
          ) {
            e.data.ownership = Ownership::Owned;
          }
        }
        Ok::<bool, Never>(true)
      })
      .unwrap();
    body.resolve_split_globals(&split_globals);
    LiftedCaptures { fields }
  }
}

impl TypedExp {
  /// The global or variable at the root of a static field path (`g`,
  /// `g.a`, `g.a.b` — field accesses only, so it denotes one fixed place),
  /// with the path's field names.
  pub fn static_field_path(&self) -> Option<(&Arc<str>, Vec<Arc<str>>)> {
    match &self.kind {
      ExpKind::Name(name) => Some((name, vec![])),
      ExpKind::Access(Accessor::Field(field), inner) => {
        let (root, mut path) = inner.static_field_path()?;
        path.push(field.clone());
        Some((root, path))
      }
      _ => None,
    }
  }
  /// Redirects every access through a split global's part path to the
  /// part's own global (see `SplitGlobal`).
  pub fn resolve_split_globals(
    &mut self,
    split_globals: &HashMap<Arc<str>, SplitGlobal>,
  ) {
    if split_globals.is_empty() {
      return;
    }
    self
      .walk_mut(&mut |e| {
        if let Some((root, path)) = e.static_field_path()
          && !path.is_empty()
          && let Some(part) = split_globals
            .get(root)
            .and_then(|split| split.parts.iter().find(|part| part.path == path))
        {
          e.kind = ExpKind::Name(part.global.clone());
          e.data.is_globally_bound = true;
          e.data.ownership = Ownership::Owned;
          return Ok(false);
        }
        Ok::<bool, Never>(true)
      })
      .unwrap();
  }
}
