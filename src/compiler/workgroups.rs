//! Compute workgroup sizes: dispatches that choose one, and the
//! `workgroup-size` builtin that reads it.

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use crate::{
  Never,
  compiler::{
    entry::EntryPoint,
    error::{CompileError, CompileErrorKind, ErrorLog, SourceTrace},
    exp_builder::ExpBuilder,
    expression::{ExpKind, Number, TypedExp},
    functions::{AbstractFunctionSignature, FunctionImplementationKind},
    program::Program,
    types::{Type, TypeState},
    vars::{TopLevelVar, TopLevelVariableKind, VariableAddressSpace},
  },
};

/// The implicit `@local` var `(workgroup-size)` reads.
const WORKGROUP_SIZE_VAR: &str = "easl_workgroup_size";

/// Whether `size` fits WebGPU's default compute limits: every dimension at
/// least 1, x and y at most 256, z at most 64, and at most 256 invocations
/// per workgroup.
pub(crate) fn check_workgroup_size(
  size: [u32; 3],
) -> Result<(), CompileErrorKind> {
  let [x, y, z] = size;
  let fits = size.iter().all(|d| *d >= 1)
    && x <= 256
    && y <= 256
    && z <= 64
    && (x as u64) * (y as u64) * (z as u64) <= 256;
  if fits {
    Ok(())
  } else {
    Err(CompileErrorKind::WorkgroupSizeOutOfRange(format!(
      "{x}x{y}x{z}"
    )))
  }
}

impl Program {
  /// The value of a constant `u32`: a literal, or a `def` holding one.
  fn constant_u32(&self, exp: &TypedExp, depth: usize) -> Option<u32> {
    match &exp.kind {
      ExpKind::NumberLiteral(Number::Int(n))
        if exp.data.with_known(|t| *t == Type::U32) =>
      {
        u32::try_from(*n).ok()
      }
      ExpKind::Name(name) => {
        let value = self.constant_def(name, depth)?;
        self.constant_u32(value, depth + 1)
      }
      _ => None,
    }
  }
  /// The value of a constant `vec3u`: a `vec3u` construction from constant
  /// `u32`s (three, or one for every component), or a `def` holding one.
  fn constant_vec3u(&self, exp: &TypedExp, depth: usize) -> Option<[u32; 3]> {
    match &exp.kind {
      ExpKind::Name(name) => {
        let value = self.constant_def(name, depth)?;
        self.constant_vec3u(value, depth + 1)
      }
      ExpKind::Application(f, args)
        if let Type::Function(signature) = f.data.unwrap_known()
          && signature
            .abstract_ancestor
            .as_ref()
            .is_some_and(|ancestor| {
              !matches!(
                ancestor.read().unwrap().implementation,
                FunctionImplementationKind::Composite(_)
              )
            }) =>
      {
        let components = args
          .iter()
          .map(|arg| self.constant_u32(arg, depth))
          .collect::<Option<Vec<u32>>>()?;
        match components[..] {
          [c] => Some([c, c, c]),
          [x, y, z] => Some([x, y, z]),
          _ => None,
        }
      }
      _ => None,
    }
  }
  /// The initializer of the `def` named `name` (bounded by `depth`, so a
  /// definition cycle, reported elsewhere, can't recurse forever).
  fn constant_def(&self, name: &Arc<str>, depth: usize) -> Option<&TypedExp> {
    if depth > 64 {
      return None;
    }
    let var = self.top_level_vars.iter().find(|v| v.name == *name)?;
    matches!(var.kind, TopLevelVariableKind::Const)
      .then_some(var.value.as_ref())
      .flatten()
  }
  /// A copy of the function `ancestor` names, registered under a fresh
  /// name as a compute entry point with workgroup size `size`.
  fn workgroup_sized_function(
    &self,
    ancestor: &Arc<RwLock<AbstractFunctionSignature>>,
    size: [u32; 3],
  ) -> Option<Arc<RwLock<AbstractFunctionSignature>>> {
    let original = ancestor.read().unwrap().clone();
    let FunctionImplementationKind::Composite(implementation) =
      &original.implementation
    else {
      return None;
    };
    let [x, y, z] = size;
    let name = self
      .names
      .write()
      .unwrap()
      .gensym(&format!("{}_workgroup_{x}_{y}_{z}", original.name));
    let mut function = implementation.read().unwrap().derived_from();
    function.entry_point = Some(EntryPoint::Compute(size));
    let function = Arc::new(RwLock::new(function));
    let signature = Arc::new(RwLock::new(AbstractFunctionSignature {
      name,
      implementation: FunctionImplementationKind::Composite(function.clone()),
      entry_point: Some(EntryPoint::Compute(size)),
      ..original
    }));
    // The copy's own type names its own signature.
    let mut function = function.write().unwrap();
    if let Type::Function(mut function_type) =
      function.expression.data.unwrap_known()
    {
      function_type.abstract_ancestor = Some(signature.clone());
      function.expression.data.kind =
        TypeState::Known(Type::Function(function_type));
    }
    Some(signature)
  }
  /// Rewrites every `(dispatch-compute-shader f count size)` into the
  /// two-argument dispatch of a copy of `f` that's a compute entry point
  /// with workgroup size `size` (one copy per function and size), so a
  /// function or closure can be dispatched at any size, including from
  /// inside a higher-order helper. `size` must be a constant `vec3u`
  /// within WebGPU's default limits. Runs before the dispatched-closure
  /// lift and implicit entry marking, which then treat each copy like any
  /// dispatched function.
  pub fn specialize_dispatch_workgroup_sizes(&mut self, errors: &mut ErrorLog) {
    let two_argument_dispatch = self
      .abstract_functions
      .get("dispatch-compute-shader")
      .and_then(|signatures| {
        signatures
          .iter()
          .find(|s| s.read().unwrap().arg_types.len() == 2)
          .cloned()
      });
    let implementations: Vec<_> = self
      .abstract_functions_iter()
      .filter_map(|signature| {
        let signature = signature.read().unwrap();
        match &signature.implementation {
          FunctionImplementationKind::Composite(f)
            if signature.generic_args.is_empty() =>
          {
            Some(f.clone())
          }
          _ => None,
        }
      })
      .collect();
    // By (dispatched function's implementation, size).
    let mut copies: HashMap<
      (usize, [u32; 3]),
      Arc<RwLock<AbstractFunctionSignature>>,
    > = HashMap::new();
    for f in implementations {
      f.write()
        .unwrap()
        .expression
        .walk_mut(&mut |exp| {
          let ExpKind::Application(callee, args) = &mut exp.kind else {
            return Ok::<bool, Never>(true);
          };
          if !matches!(&callee.kind, ExpKind::Name(name)
            if &**name == "dispatch-compute-shader")
            || args.len() != 3
          {
            return Ok(true);
          }
          let size_source = args[2].source_trace.clone();
          let Some(size) = self.constant_vec3u(&args[2], 0) else {
            errors.log(CompileError::new(
              CompileErrorKind::NonConstantWorkgroupSize,
              size_source,
            ));
            return Ok(true);
          };
          if let Err(kind) = check_workgroup_size(size) {
            errors.log(CompileError::new(kind, size_source));
            return Ok(true);
          }
          if let Type::Function(mut dispatched) = args[0].data.unwrap_known()
            && let Some(ancestor) = dispatched.abstract_ancestor.clone()
            && let FunctionImplementationKind::Composite(implementation) =
              &ancestor.read().unwrap().implementation
          {
            let key = (Arc::as_ptr(implementation) as usize, size);
            let copy = match copies.get(&key) {
              Some(copy) => copy.clone(),
              None => {
                let Some(copy) = self.workgroup_sized_function(&ancestor, size)
                else {
                  return Ok(true);
                };
                copies.insert(key, copy.clone());
                copy
              }
            };
            // A reference to the function by name now names the copy (a
            // closure or a local holding one keeps its own name: only its
            // type names the function).
            if let ExpKind::Name(name) = &mut args[0].kind
              && *name == ancestor.read().unwrap().name
            {
              *name = copy.read().unwrap().name.clone();
            }
            dispatched.abstract_ancestor = Some(copy);
            args[0].data.kind = TypeState::Known(Type::Function(dispatched));
          }
          args.pop();
          if let Type::Function(mut callee_type) = callee.data.unwrap_known() {
            callee_type.args.truncate(2);
            if let Some(two_argument_dispatch) = &two_argument_dispatch {
              callee_type.abstract_ancestor =
                Some(two_argument_dispatch.clone());
            }
            callee.data.kind = TypeState::Known(Type::Function(callee_type));
          }
          Ok(true)
        })
        .unwrap();
    }
    for copy in copies.into_values() {
      self.add_abstract_function(copy);
    }
  }
  /// Rewrites every `(workgroup-size)` call into a read of the implicit
  /// `@local` var `easl_workgroup_size`, which every compute entry point
  /// sets to its own workgroup size on entry, so helpers shared by shaders
  /// of different sizes read the running one's. Runs after implicit entry
  /// marking (every compute entry's size is known) and context
  /// exclusivity (which rejects the calls outside compute shaders).
  pub fn extract_workgroup_size(&mut self) {
    let var_name: Arc<str> = WORKGROUP_SIZE_VAR.into();
    let mut size_type: Option<Type> = None;
    let implementations: Vec<_> = self
      .abstract_functions_iter()
      .filter_map(
        |signature| match &signature.read().unwrap().implementation {
          FunctionImplementationKind::Composite(f) => Some(f.clone()),
          _ => None,
        },
      )
      .collect();
    for f in implementations.iter() {
      f.write()
        .unwrap()
        .expression
        .walk_mut(&mut |exp| {
          if let ExpKind::Application(callee, args) = &exp.kind
            && let ExpKind::Name(name) = &callee.kind
            && &**name == "workgroup-size"
            && args.is_empty()
          {
            size_type = Some(exp.data.unwrap_known());
            exp.kind = ExpKind::Name(var_name.clone());
            exp.data.is_globally_bound = true;
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
    let Some(size_type) = size_type else {
      return;
    };
    self.top_level_vars.push(TopLevelVar {
      name: var_name.clone(),
      kind: TopLevelVariableKind::Var {
        address_space: VariableAddressSpace::Local,
        group_and_binding: None,
      },
      var_type: size_type.clone(),
      value: None,
      source_trace: SourceTrace::empty(),
      external: false,
      directly_user_written: false,
    });
    for f in implementations {
      let mut f = f.write().unwrap();
      let Some(EntryPoint::Compute(size)) = f.entry_point else {
        continue;
      };
      let ExpKind::Function(_, body) = &mut f.expression.kind else {
        continue;
      };
      let b = ExpBuilder::at(&body.source_trace);
      let size_var = || {
        let mut var = b.name(&var_name, &size_type);
        var.data.is_globally_bound = true;
        var
      };
      // A field assignment, as swizzle assignments desugar to: the target
      // is a place, not a reference, so emission doesn't dereference it.
      let assignments =
        ["x", "y", "z"].into_iter().zip(size).map(|(field, n)| {
          let mut component = b.field(size_var(), field, Type::U32);
          component.data.is_globally_bound = true;
          b.apply(
            TypedExp::assignment_function(Type::U32.known().into()),
            vec![component, b.u32(n)],
            &Type::Unit,
          )
        });
      let statements = match &mut body.kind {
        ExpKind::Block(statements) => statements,
        _ => {
          let old_body = std::mem::replace(&mut **body, b.unit());
          **body = b.with_data(ExpKind::Block(vec![]), old_body.data.clone());
          let ExpKind::Block(statements) = &mut body.kind else {
            unreachable!()
          };
          statements.push(old_body);
          statements
        }
      };
      statements.splice(0..0, assignments);
    }
  }
}
