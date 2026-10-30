//! Read-only views of storage-write vars, for render shaders.
//!
//! WebGPU (without extensions) doesn't let a vertex shader use `read_write`
//! storage at all, even to read it, while a storage-write var is
//! `read_write` everywhere. So each storage-write var a vertex shader reads
//! gets a second WGSL binding, declared `var<storage, read>`: render
//! shaders read the var through it, and the runtime binds the same buffer
//! there (an alias, `GpuBindingInfo::alias_of`), so nothing is copied and
//! every write (CPU or compute) is visible through it. Compute shaders keep
//! using the var itself.
//!
//! The view exists only in the WGSL output and the GPU interface: the
//! compiler's IR, effects, CPU↔GPU sync, and thread sharing see the one
//! var. Emission gives the render entries that read through views (and the
//! helpers they call that read the var) copies with the var's name swapped
//! for the view's.
//!
//! Vertex shaders writing GPU memory are rejected by context exclusivity
//! (`StorageWriteInVertexShader`). A fragment shader reads through the view
//! only when it doesn't write the var: one draw can't bind a buffer both as
//! `read_write` storage and as read-only storage, so the dispatch validation
//! rejects a draw whose vertex shader reads a var its fragment shader writes
//! (`StorageReadInVertexWrittenInFragment`).

use std::collections::{BTreeSet, HashMap, HashSet};
use std::sync::{Arc, RwLock};

use crate::{
  Never,
  compiler::{
    effects::EffectType,
    entry::EntryPoint,
    expression::ExpKind,
    functions::{EffectsMemo, FunctionImplementationKind, TopLevelFunction},
    program::{EmittedFunctions, NameContext, Program},
    vars::{
      BindingSpec, GroupAndBinding, TopLevelVar, TopLevelVariableKind,
      VariableAddressSpace,
    },
  },
};

/// A read-only view of a storage-write var.
#[derive(Debug, Clone, PartialEq)]
pub struct StorageReadView {
  /// The storage-write var.
  pub var: Arc<str>,
  /// The view's name in the WGSL output.
  pub view: Arc<str>,
  /// The view's binding.
  pub binding: GroupAndBinding,
}

/// Every view a program's render shaders read through
/// (`Program::plan_storage_read_views`).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct StorageReadViews {
  pub views: Vec<StorageReadView>,
  /// Each render entry point that reads through views, with the vars it
  /// reads through them.
  pub entry_vars: HashMap<Arc<str>, Vec<Arc<str>>>,
}

impl StorageReadViews {
  /// The view of `var`, if render shaders read it through one.
  pub fn view_of(&self, var: &str) -> Option<&StorageReadView> {
    self.views.iter().find(|view| &*view.var == var)
  }

  /// Whether the entry point `entry` reads `var` through its view.
  pub fn reads_through_view(&self, entry: &str, var: &str) -> bool {
    self
      .entry_vars
      .get(entry)
      .is_some_and(|vars| vars.iter().any(|v| &**v == var))
  }
}

/// What one render entry point does with storage-write vars.
struct RenderEntry {
  vertex: bool,
  reads: HashSet<Arc<str>>,
  writes: HashSet<Arc<str>>,
}

/// The render-entry function bodies and helper copies that read through
/// views, for WGSL emission (`Program::storage_read_view_functions`).
#[derive(Default)]
pub(crate) struct ViewFunctions {
  /// Render entry points, by name, with their reads through views renamed.
  pub entries: HashMap<Arc<str>, TopLevelFunction>,
  /// Copies of helpers that read through views: each copy's name, the
  /// function it copies, and its body.
  pub helpers: Vec<(Arc<str>, Arc<str>, TopLevelFunction)>,
}

/// The reads (including length-only reads) and writes in `effects` of
/// vars in `vars`.
fn storage_uses(
  effects: &EffectType,
  vars: &HashSet<Arc<str>>,
) -> (HashSet<Arc<str>>, HashSet<Arc<str>>) {
  let (reads, writes) = effects.gpu_read_and_written_globals();
  let keep = |names: Vec<Arc<str>>| -> HashSet<Arc<str>> {
    names.into_iter().filter(|n| vars.contains(n)).collect()
  };
  (keep(reads), keep(writes))
}

impl Program {
  /// The storage-write vars' names.
  pub(crate) fn storage_write_vars(&self) -> HashSet<Arc<str>> {
    self
      .top_level_vars
      .iter()
      .filter(|v| {
        matches!(
          v.kind,
          TopLevelVariableKind::Var {
            address_space: VariableAddressSpace::StorageReadWrite,
            ..
          }
        )
      })
      .map(|v| v.name.clone())
      .collect()
  }

  /// The storage-write vars the vertex shader `vertex` reads and the
  /// fragment shader `fragment` writes, sorted: one draw of the two can't
  /// bind them, since the vertex shader reads them through read-only views.
  pub(crate) fn vertex_reads_of_fragment_writes(
    &self,
    vertex: &TopLevelFunction,
    fragment: &TopLevelFunction,
  ) -> Vec<Arc<str>> {
    let vars = self.storage_write_vars();
    let (reads, _) = storage_uses(&vertex.effects(), &vars);
    let (_, writes) = storage_uses(&fragment.effects(), &vars);
    let mut both: Vec<Arc<str>> =
      reads.intersection(&writes).cloned().collect();
    both.sort();
    both
  }

  /// The vertex and fragment entry points' uses of storage-write vars, by
  /// entry name.
  fn render_entry_storage_uses(
    &self,
    vars: &HashSet<Arc<str>>,
  ) -> HashMap<Arc<str>, RenderEntry> {
    let mut memo = EffectsMemo::default();
    let mut entries = HashMap::new();
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      let vertex = match f.entry_point {
        Some(EntryPoint::Vertex) => true,
        Some(EntryPoint::Fragment) => false,
        _ => continue,
      };
      let FunctionImplementationKind::Composite(implementation) =
        &f.implementation
      else {
        continue;
      };
      let implementation = implementation.read().unwrap();
      let (reads, writes) =
        storage_uses(&implementation.effects_with(&mut memo), vars);
      entries.insert(
        f.name.clone(),
        RenderEntry {
          vertex,
          reads,
          writes,
        },
      );
    }
    entries
  }

  /// Gives each storage-write var a vertex shader reads a read-only view
  /// (see the module docs). Runs after `assign_elided_bindings`, so every
  /// binding is known and views take free ones.
  pub fn plan_storage_read_views(&mut self) {
    let entries = self.render_entry_storage_uses(&self.storage_write_vars());
    let mut entry_names: Vec<&Arc<str>> = entries.keys().collect();
    entry_names.sort();
    let viewed: BTreeSet<Arc<str>> = entries
      .values()
      .filter(|entry| entry.vertex)
      .flat_map(|entry| entry.reads.iter().cloned())
      .collect();
    if viewed.is_empty() {
      return;
    }
    let mut used: HashSet<GroupAndBinding> = self
      .top_level_vars
      .iter()
      .filter_map(|v| match v.kind {
        TopLevelVariableKind::Var {
          group_and_binding: Some(BindingSpec::Specified(binding)),
          ..
        } => Some(binding),
        _ => None,
      })
      .collect();
    let mut views = StorageReadViews::default();
    for var in viewed.iter() {
      let binding = GroupAndBinding::first_free(&used);
      used.insert(binding);
      let view = self
        .names
        .write()
        .unwrap()
        .gensym(&format!("{var}_read_view"));
      views.views.push(StorageReadView {
        var: var.clone(),
        view,
        binding,
      });
    }
    for name in entry_names.iter() {
      let entry = &entries[*name];
      let mut through_view: Vec<Arc<str>> = entry
        .reads
        .iter()
        .filter(|var| {
          viewed.contains(*var)
            && (entry.vertex || !entry.writes.contains(*var))
        })
        .cloned()
        .collect();
      through_view.sort();
      if !through_view.is_empty() {
        views.entry_vars.insert((*name).clone(), through_view);
      }
    }
    self.storage_read_views = views;
  }

  /// The WGSL declaration of `var`'s view, if render shaders read it
  /// through one: the var's type, read-only storage, at the view's binding.
  pub(crate) fn storage_read_view_declaration(
    &self,
    var: &TopLevelVar,
  ) -> Option<TopLevelVar> {
    let view = self.storage_read_views.view_of(&var.name)?;
    Some(TopLevelVar {
      name: view.view.clone(),
      kind: TopLevelVariableKind::Var {
        address_space: VariableAddressSpace::StorageRead,
        group_and_binding: Some(BindingSpec::Specified(view.binding)),
      },
      ..var.clone()
    })
  }

  /// The render entry bodies and helper copies that read through views
  /// (see the module docs), for WGSL emission of `functions`.
  pub(crate) fn storage_read_view_functions(
    &self,
    functions: &EmittedFunctions,
    names: &mut NameContext,
  ) -> ViewFunctions {
    let views = &self.storage_read_views;
    if views.entry_vars.is_empty() {
      return ViewFunctions::default();
    }
    let mut builder = ViewFunctionBuilder {
      views,
      implementations: functions.ordered.iter().cloned().collect(),
      memo: EffectsMemo::default(),
      copies: HashMap::new(),
      names,
      out: ViewFunctions::default(),
    };
    let mut entries: Vec<(&Arc<str>, &Vec<Arc<str>>)> =
      views.entry_vars.iter().collect();
    entries.sort_by(|(a, _), (b, _)| a.cmp(b));
    for (entry, vars) in entries {
      if functions.emitted.contains(entry)
        && builder.implementations.contains_key(entry)
      {
        let body = builder.renamed(entry, vars);
        builder.out.entries.insert(entry.clone(), body);
      }
    }
    builder.out
  }
}

/// Builds the copies of functions that read storage-write vars through
/// their views.
struct ViewFunctionBuilder<'a> {
  views: &'a StorageReadViews,
  implementations: HashMap<Arc<str>, Arc<RwLock<TopLevelFunction>>>,
  memo: EffectsMemo,
  /// Each copy's name, by the function it copies and the vars it reads
  /// through views.
  copies: HashMap<(Arc<str>, Vec<Arc<str>>), Arc<str>>,
  names: &'a mut NameContext,
  out: ViewFunctions,
}

impl ViewFunctionBuilder<'_> {
  /// A copy of `function` reading `vars` through their views (and calling
  /// copies of its callees that read them).
  fn renamed(
    &mut self,
    function: &Arc<str>,
    vars: &[Arc<str>],
  ) -> TopLevelFunction {
    let mut body = self.implementations[function].read().unwrap().clone();
    let mut callees: Vec<Arc<str>> = vec![];
    body
      .expression
      .walk(&mut |exp| {
        if let ExpKind::Name(name) = &exp.kind
          && name != function
          && self.implementations.contains_key(name)
          && !callees.contains(name)
        {
          callees.push(name.clone());
        }
        Ok::<bool, Never>(true)
      })
      .unwrap();
    let mut renames: HashMap<Arc<str>, Arc<str>> = HashMap::new();
    for callee in callees {
      if let Some(copy) = self.copy(&callee, vars) {
        renames.insert(callee, copy);
      }
    }
    for var in vars {
      if let Some(view) = self.views.view_of(var) {
        renames.insert(var.clone(), view.view.clone());
      }
    }
    body
      .expression
      .walk_mut(&mut |exp| {
        if let ExpKind::Name(name) = &mut exp.kind
          && let Some(renamed) = renames.get(name)
        {
          *name = renamed.clone();
        }
        Ok::<bool, Never>(true)
      })
      .unwrap();
    body
  }

  /// The name of the copy of `function` that reads those of `vars` it reads
  /// through their views, or `None` when it reads none of them.
  fn copy(
    &mut self,
    function: &Arc<str>,
    vars: &[Arc<str>],
  ) -> Option<Arc<str>> {
    let effects = self.implementations[function]
      .read()
      .unwrap()
      .effects_with(&mut self.memo);
    let (reads, _) = effects.gpu_read_and_written_globals();
    let mut read_vars: Vec<Arc<str>> =
      vars.iter().filter(|v| reads.contains(v)).cloned().collect();
    if read_vars.is_empty() {
      return None;
    }
    read_vars.sort();
    let key = (function.clone(), read_vars.clone());
    if let Some(copy) = self.copies.get(&key) {
      return Some(copy.clone());
    }
    let copy = self.names.gensym(&format!("{function}_read_view"));
    self.copies.insert(key, copy.clone());
    let body = self.renamed(function, &read_vars);
    self
      .out
      .helpers
      .push((copy.clone(), function.clone(), body));
    Some(copy)
  }
}
