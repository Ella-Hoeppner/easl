use std::collections::HashSet;

use std::sync::Arc;

use crate::compiler::{
  entry::BuiltinIOAttribute,
  program::Program,
  vars::{TopLevelVariableKind, VariableAddressSpace},
};

/// A piece of ambient window/input state that the runtime tracks and that
/// easl code can query (e.g. `window-time`, `mouse-coords`). On the CPU
/// these are direct queries against the IO manager; in GPU code the
/// `extract_gpu_window_info` pass rewrites each use into a read of an
/// implicit uniform binding that the runtime refreshes at the start of
/// every frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WindowInfoKind {
  Resolution,
  Time,
  DeltaTime,
  FrameIndex,
  MouseCoords,
  MousePresent,
  MouseDown,
  MouseJustDown,
  /// `key-down?` — takes a compile-time string literal; each distinct key
  /// gets its own binding (see `WindowInfoBindingSource::KeyDown`).
  KeyDown,
  /// `key-just-down?` — as `KeyDown`.
  KeyJustDown,
}

/// What an implicit window-info binding is populated from: either one of
/// the zero-arg queries, or a key query for one specific compile-time key
/// string.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WindowInfoBindingSource {
  Simple(WindowInfoKind),
  KeyDown(Arc<str>),
  KeyJustDown(Arc<str>),
}

impl WindowInfoKind {
  pub const ALL: [WindowInfoKind; 10] = [
    WindowInfoKind::Resolution,
    WindowInfoKind::Time,
    WindowInfoKind::DeltaTime,
    WindowInfoKind::FrameIndex,
    WindowInfoKind::MouseCoords,
    WindowInfoKind::MousePresent,
    WindowInfoKind::MouseDown,
    WindowInfoKind::MouseJustDown,
    WindowInfoKind::KeyDown,
    WindowInfoKind::KeyJustDown,
  ];
  /// The easl builtin function name that queries this info.
  pub fn fn_name(&self) -> &'static str {
    match self {
      WindowInfoKind::Resolution => "window-resolution",
      WindowInfoKind::Time => "window-time",
      WindowInfoKind::DeltaTime => "window-delta-time",
      WindowInfoKind::FrameIndex => "window-frame-index",
      WindowInfoKind::MouseCoords => "mouse-coords",
      WindowInfoKind::MousePresent => "mouse-present?",
      WindowInfoKind::MouseDown => "mouse-down?",
      WindowInfoKind::MouseJustDown => "mouse-just-down?",
      WindowInfoKind::KeyDown => "key-down?",
      WindowInfoKind::KeyJustDown => "key-just-down?",
    }
  }
  pub fn from_fn_name(name: &str) -> Option<Self> {
    Self::ALL.iter().copied().find(|kind| kind.fn_name() == name)
  }
  /// The base name for the implicit uniform binding generated for GPU uses.
  pub fn binding_base_name(&self) -> &'static str {
    match self {
      WindowInfoKind::Resolution => "window_resolution_info",
      WindowInfoKind::Time => "window_time_info",
      WindowInfoKind::DeltaTime => "window_delta_time_info",
      WindowInfoKind::FrameIndex => "window_frame_index_info",
      WindowInfoKind::MouseCoords => "mouse_coords_info",
      WindowInfoKind::MousePresent => "mouse_present_info",
      WindowInfoKind::MouseDown => "mouse_down_info",
      WindowInfoKind::MouseJustDown => "mouse_just_down_info",
      WindowInfoKind::KeyDown => "key_down_info",
      WindowInfoKind::KeyJustDown => "key_just_down_info",
    }
  }
  /// Whether the easl-level query returns a bool. Bools aren't
  /// host-shareable in WGSL uniforms, so these bindings are stored as `u32`
  /// and GPU-side uses are rewritten to `(!= binding 0u)`.
  pub fn is_boolean(&self) -> bool {
    matches!(
      self,
      WindowInfoKind::MousePresent
        | WindowInfoKind::MouseDown
        | WindowInfoKind::MouseJustDown
        | WindowInfoKind::KeyDown
        | WindowInfoKind::KeyJustDown
    )
  }
}

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
  /// A query of ambient window/input state (see `WindowInfoKind`). Legal in
  /// both CPU and GPU code; GPU uses are rewritten into implicit uniform
  /// binding reads by `extract_gpu_window_info`, so by WGSL emission time no
  /// GPU-emitted function carries this effect. Excluded from the C and
  /// audio targets like CPU-exclusive functions.
  WindowInfo(WindowInfoKind),
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
        | Effect::CPUExclusiveType(_)
        | Effect::WindowInfo(_) => {}
        _ => return false,
      }
    }
    true
  }
  pub fn window_info_kinds(&self) -> Vec<WindowInfoKind> {
    self
      .0
      .iter()
      .filter_map(|e| {
        if let Effect::WindowInfo(kind) = e {
          Some(*kind)
        } else {
          None
        }
      })
      .collect()
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
