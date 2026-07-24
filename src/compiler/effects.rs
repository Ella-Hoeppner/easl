use std::collections::HashSet;

use std::sync::Arc;

use crate::compiler::{
  entry::BuiltinIOAttribute,
  program::Program,
  vars::{TopLevelVariableKind, VariableAddressSpace},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Effect {
  ReadsVar(Arc<str>),
  /// A read of only an array variable's *length* (via `array-length`), not
  /// its elements. Tracked separately from `ReadsVar` because the GPU can
  /// never resize a buffer — lengths are CPU-authoritative — so a
  /// length-only read of a GPU-dirty array must not trigger a blocking
  /// GPU→CPU readback. It still counts as a read for the CPU→GPU direction:
  /// a dispatched shader calling `arrayLength()` needs the (possibly
  /// resized) buffer uploaded first, since WGSL derives the length from the
  /// buffer's size.
  ReadsArrayLength(Arc<str>),
  ModifiesLocalVar(Arc<str>),
  ModifiesGlobalVar(Arc<str>),
  Break,
  Return,
  Continue,
  Discard,
  FragmentExclusiveFunction(Arc<str>),
  CPUExclusiveFunction(Arc<str>),
  CPUExclusiveType(Arc<str>),
  Print,
  Window,
  LookupBuiltinAttribute(BuiltinIOAttribute),
  InvokesUnknownFunction,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectType(pub HashSet<Effect>);

impl EffectType {
  pub fn empty() -> Self {
    Self(HashSet::new())
  }
  pub fn is_pure(&self) -> bool {
    self.0.is_empty()
  }
  pub fn merge(&mut self, other: impl Into<Self>) {
    self.0.extend(other.into().0);
  }
  pub fn remove(&mut self, e: &Effect) {
    self.0.remove(e);
  }
  pub fn contains(&self, e: &Effect) -> bool {
    self.0.contains(e)
  }
  pub fn is_side_effect_free(&self) -> bool {
    for e in self.0.iter() {
      match e {
        Effect::ReadsVar(_)
        | Effect::ReadsArrayLength(_)
        | Effect::FragmentExclusiveFunction(_)
        | Effect::CPUExclusiveFunction(_)
        | Effect::CPUExclusiveType(_) => {}
        _ => return false,
      }
    }
    true
  }
  pub fn cpu_exclusive_functions(&self) -> Vec<Arc<str>> {
    self
      .0
      .iter()
      .filter_map(|e| {
        if let Effect::CPUExclusiveFunction(name) = e {
          Some(name.clone())
        } else {
          None
        }
      })
      .collect()
  }
  pub fn cpu_exclusive_types(&self) -> Vec<Arc<str>> {
    self
      .0
      .iter()
      .filter_map(|e| {
        if let Effect::CPUExclusiveType(name) = e {
          Some(name.clone())
        } else {
          None
        }
      })
      .collect()
  }
  pub fn gpu_illegal_address_space_writes(
    &self,
    program: &Program,
  ) -> Vec<(Arc<str>, VariableAddressSpace)> {
    self
      .0
      .iter()
      .filter_map(|e| {
        if let Effect::ModifiesGlobalVar(name) = e
          && let Some(top_level_var) =
            program.top_level_vars.iter().find(|v| v.name == *name)
          && let TopLevelVariableKind::Var { address_space, .. } =
            top_level_var.kind
          && !address_space.may_write_from_gpu()
        {
          Some((name.clone(), address_space))
        } else {
          None
        }
      })
      .collect()
  }
  /// The globals whose *values* are read and written. Length-only reads
  /// (`ReadsArrayLength`) are excluded from the read set: this is the set
  /// used to decide GPU→CPU readbacks, and array lengths never need one.
  pub fn read_and_written_globals(&self) -> (Vec<Arc<str>>, Vec<Arc<str>>) {
    (
      self
        .0
        .iter()
        .filter_map(|effect| match effect {
          Effect::ReadsVar(name) => Some(name.clone()),
          _ => None,
        })
        .collect(),
      self
        .0
        .iter()
        .filter_map(|effect| match effect {
          Effect::ModifiesGlobalVar(name) => Some(name.clone()),
          _ => None,
        })
        .collect(),
    )
  }
  /// Like `read_and_written_globals`, but the read set also includes
  /// length-only reads. This is the set used when dispatching GPU work to
  /// decide which CPU-dirty buffers to upload first: a shader calling
  /// `arrayLength()` needs the buffer uploaded even if it never reads the
  /// elements, since WGSL derives the length from the buffer's size.
  pub fn gpu_read_and_written_globals(&self) -> (Vec<Arc<str>>, Vec<Arc<str>>) {
    let (mut reads, writes) = self.read_and_written_globals();
    reads.extend(self.0.iter().filter_map(|effect| match effect {
      Effect::ReadsArrayLength(name) => Some(name.clone()),
      _ => None,
    }));
    (reads, writes)
  }
  pub fn looked_up_builtin_attributes(&self) -> Vec<BuiltinIOAttribute> {
    self
      .0
      .iter()
      .filter_map(|effect| match effect {
        Effect::LookupBuiltinAttribute(a) => Some(*a),
        _ => None,
      })
      .collect()
  }
}

impl From<HashSet<Effect>> for EffectType {
  fn from(e: HashSet<Effect>) -> Self {
    Self(e)
  }
}

impl From<Effect> for EffectType {
  fn from(e: Effect) -> Self {
    Self([e].into_iter().collect())
  }
}

impl From<Vec<Effect>> for EffectType {
  fn from(effects: Vec<Effect>) -> Self {
    Self(effects.into_iter().collect())
  }
}
