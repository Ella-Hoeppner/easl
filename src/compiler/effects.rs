use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

use std::sync::Arc;

use crate::compiler::{
  entry::BuiltinIOAttribute,
  error::SourceTrace,
  program::Program,
  vars::{TopLevelVariableKind, VariableAddressSpace},
};
use crate::midi_out::MidiOutKind;

/// A piece of ambient window/input state that the runtime tracks and that
/// easl code can query (e.g. `window-time`, `mouse-coords`). On the CPU
/// these are direct queries against the IO manager; in GPU code the
/// `extract_gpu_window_info` pass rewrites each use into a read of an
/// implicit uniform binding that the runtime refreshes at the start of
/// every frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WindowInfoKind {
  Resolution,
  Time,
  DeltaTime,
  FrameIndex,
  MouseCoords,
  MousePresent,
  MouseDown,
  MouseJustDown,
  /// `mouse-right-down?` / `mouse-right-just-down?`: the secondary button.
  MouseRightDown,
  MouseRightJustDown,
  /// `mouse-delta` — raw mouse motion accumulated since the previous frame.
  MouseDelta,
  /// `mouse-scroll-delta` — mouse wheel / trackpad scrolling accumulated
  /// since the previous frame.
  MouseScrollDelta,
  /// `mouse-captured?` — whether `capture-mouse` currently holds the cursor.
  MouseCaptured,
  /// `key-down?` — takes a compile-time string literal; each distinct key
  /// gets its own binding (see `WindowInfoBindingSource::KeyDown`).
  KeyDown,
  /// `key-just-down?` — as `KeyDown`.
  KeyJustDown,
}

/// What an implicit window-info binding is populated from: either one of
/// the zero-arg queries, or a key query for one specific compile-time key
/// string.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WindowInfoBindingSource {
  Simple(WindowInfoKind),
  KeyDown(Arc<str>),
  KeyJustDown(Arc<str>),
}

impl WindowInfoKind {
  pub const ALL: [WindowInfoKind; 15] = [
    WindowInfoKind::Resolution,
    WindowInfoKind::Time,
    WindowInfoKind::DeltaTime,
    WindowInfoKind::FrameIndex,
    WindowInfoKind::MouseCoords,
    WindowInfoKind::MousePresent,
    WindowInfoKind::MouseDown,
    WindowInfoKind::MouseJustDown,
    WindowInfoKind::MouseRightDown,
    WindowInfoKind::MouseRightJustDown,
    WindowInfoKind::MouseDelta,
    WindowInfoKind::MouseScrollDelta,
    WindowInfoKind::MouseCaptured,
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
      WindowInfoKind::MouseRightDown => "mouse-right-down?",
      WindowInfoKind::MouseRightJustDown => "mouse-right-just-down?",
      WindowInfoKind::MouseDelta => "mouse-delta",
      WindowInfoKind::MouseScrollDelta => "mouse-scroll-delta",
      WindowInfoKind::MouseCaptured => "mouse-captured?",
      WindowInfoKind::KeyDown => "key-down?",
      WindowInfoKind::KeyJustDown => "key-just-down?",
    }
  }
  pub fn from_fn_name(name: &str) -> Option<Self> {
    Self::ALL
      .iter()
      .copied()
      .find(|kind| kind.fn_name() == name)
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
      WindowInfoKind::MouseRightDown => "mouse_right_down_info",
      WindowInfoKind::MouseRightJustDown => "mouse_right_just_down_info",
      WindowInfoKind::MouseDelta => "mouse_delta_info",
      WindowInfoKind::MouseScrollDelta => "mouse_scroll_delta_info",
      WindowInfoKind::MouseCaptured => "mouse_captured_info",
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
        | WindowInfoKind::MouseRightDown
        | WindowInfoKind::MouseRightJustDown
        | WindowInfoKind::MouseCaptured
        | WindowInfoKind::KeyDown
        | WindowInfoKind::KeyJustDown
    )
  }
}

/// A call that WGSL requires to be in uniform control flow: every invocation
/// of its group (a compute shader's workgroup for a barrier, a fragment's
/// 2x2 quad for a derivative) reaches it together, or none does.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UniformControlRequirement {
  /// The builtin called: a barrier, or one taking derivatives.
  pub builtin: Arc<str>,
  pub call: SourceTrace,
}

/// What a value's uniformity depends on, in a uniformity requirement (see
/// `Effect::RequiresUniformValue`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniformitySource {
  /// A variable: a global, or a local or parameter not yet resolved to
  /// what it holds.
  Var(Arc<str>),
  /// The function's argument at this index: its caller's concern.
  Argument(usize),
}

impl UniformitySource {
  /// What `source` holds, when it's one of the variables `held` gives the
  /// sources of: those sources, with any of `held`'s variables among them
  /// resolved in turn. A variable named in its own sources stands for its
  /// value on entry (a parameter's), and stays.
  pub fn resolve_held(
    source: &UniformitySource,
    held: &HashMap<Arc<str>, HashSet<UniformitySource>>,
  ) -> Option<HashSet<UniformitySource>> {
    let UniformitySource::Var(name) = source else {
      return None;
    };
    if !held.contains_key(name) {
      return None;
    }
    let mut resolved = HashSet::new();
    let mut visited = HashSet::from([name.clone()]);
    let mut pending = vec![name.clone()];
    while let Some(name) = pending.pop() {
      for source in held[&name].iter() {
        match source {
          UniformitySource::Var(var)
            if var != &name && held.contains_key(var) =>
          {
            if visited.insert(var.clone()) {
              pending.push(var.clone());
            }
          }
          _ => {
            resolved.insert(source.clone());
          }
        }
      }
    }
    Some(resolved)
  }
}

/// A uniform-control requirement that holds only if `source` is uniform:
/// whether the call runs depends on it, through `condition`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UniformValueRequirement {
  pub source: UniformitySource,
  pub requirement: Arc<UniformControlRequirement>,
  pub condition: SourceTrace,
}

/// The exit an `Effect::ExitDependsOn` is about.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExitKind {
  Return,
  Break,
  Continue,
}

impl ExitKind {
  pub const ALL: [ExitKind; 3] =
    [ExitKind::Return, ExitKind::Break, ExitKind::Continue];
  pub fn effect(self) -> Effect {
    match self {
      ExitKind::Return => Effect::Return,
      ExitKind::Break => Effect::Break,
      ExitKind::Continue => Effect::Continue,
    }
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
  /// A write of a lifted audio-closure capture global performed *inside*
  /// the `start-audio` builtin, on the call that actually starts the audio
  /// thread (the handoff that seeds the audio replica's initial state).
  /// Counts as a write for the thread-sharing audience analysis — it's how
  /// the analysis sees the main thread touching the lifted globals — but
  /// is deliberately excluded from `read_and_written_globals` and thus
  /// from the generic post-application dirty marking: the seed happens
  /// conditionally at runtime (first call only), so the builtin marks the
  /// vars dirty itself exactly when it seeds. Generic every-call marking
  /// would republish main's stale replica each frame, clobbering the audio
  /// thread's state.
  SeedsGlobalVar(Arc<str>),
  Break,
  Return,
  Continue,
  Discard,
  FragmentExclusiveFunction(Arc<str>),
  /// Synchronizes a compute shader's workgroup (`workgroup-barrier`,
  /// `storage-barrier`, `workgroup-uniform-load`): compute-only, and never
  /// pruned, since a barrier's whole effect is on other invocations.
  Barrier(Arc<str>),
  CPUExclusiveFunction(Arc<str>),
  CPUExclusiveType(Arc<str>),
  /// A query of ambient window/input state (see `WindowInfoKind`). Legal in
  /// both CPU and GPU code; GPU uses are rewritten into implicit uniform
  /// binding reads by `extract_gpu_window_info`, so by WGSL emission time no
  /// GPU-emitted function carries this effect. Excluded from the C and
  /// audio targets like CPU-exclusive functions.
  WindowInfo(WindowInfoKind),
  Print,
  /// Writes a file on the host filesystem (`save-png`). Externally
  /// observable, so statements with this effect are never pruned as dead
  /// code (see `is_side_effect_free`).
  FileWrite,
  /// Sends a MIDI message out of the program (the `send-midi-*` builtins,
  /// and `select-midi-output`, which picks where they go). Externally
  /// observable, so never pruned.
  MidiOut,
  Window,
  LookupBuiltinAttribute(BuiltinIOAttribute),
  InvokesUnknownFunction,
  /// A call that must be in uniform control flow (a barrier, or a builtin
  /// taking derivatives). Branches, loops, and early exits the call depends
  /// on turn it into `RequiresUniformValue`s on what they depend on.
  RequiresUniformControl(Arc<UniformControlRequirement>),
  /// A uniform-control requirement that depends on a value. Bindings
  /// resolve a local's into what it holds, and calls an argument's into the
  /// argument's sources; one left on a value that varies between
  /// invocations is an error (`Program::validate_uniformity`).
  RequiresUniformValue(Arc<UniformValueRequirement>),
  /// For uniformity: a write of a local variable (alongside its
  /// `ModifiesLocalVar`) whose value, or whether it happens, depends on a
  /// source. The variable's binding resolves requirements on it into these.
  LocalWriteDependsOn(Arc<str>, UniformitySource),
  /// For uniformity: an exit (alongside its `Return`, `Break`, or
  /// `Continue`) whose happening depends on a source. What follows it, and
  /// the loop it leaves, run only in the invocations it didn't take.
  ExitDependsOn(ExitKind, UniformitySource),
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
    // The larger set absorbs the smaller one.
    let mut other = other.into();
    if other.0.len() > self.0.len() {
      std::mem::swap(&mut self.0, &mut other.0);
    }
    self.0.extend(other.0);
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
        | Effect::WindowInfo(_)
        | Effect::RequiresUniformControl(_)
        | Effect::RequiresUniformValue(_)
        | Effect::LocalWriteDependsOn(..)
        | Effect::ExitDependsOn(..) => {}
        _ => return false,
      }
    }
    true
  }
  /// The uniform-control requirements among these effects.
  pub fn uniform_control_requirements(
    &self,
  ) -> Vec<Arc<UniformControlRequirement>> {
    self
      .0
      .iter()
      .filter_map(|e| match e {
        Effect::RequiresUniformControl(requirement) => {
          Some(requirement.clone())
        }
        _ => None,
      })
      .collect()
  }
  /// The source an effect carrying one for uniformity depends on.
  fn uniformity_source(effect: &Effect) -> Option<&UniformitySource> {
    match effect {
      Effect::RequiresUniformValue(requirement) => Some(&requirement.source),
      Effect::LocalWriteDependsOn(_, source)
      | Effect::ExitDependsOn(_, source) => Some(source),
      _ => None,
    }
  }
  /// Whether one of these effects depends on one of `names` for
  /// uniformity.
  pub fn has_uniformity_source_in(&self, names: &HashSet<Arc<str>>) -> bool {
    self.0.iter().any(|e| {
      matches!(Self::uniformity_source(e), Some(UniformitySource::Var(name))
        if names.contains(name))
    })
  }
  /// Whether these effects include a uniformity requirement on an argument.
  pub fn has_uniformity_requirement_on_arguments(&self) -> bool {
    self.0.iter().any(|e| {
      matches!(
        e,
        Effect::RequiresUniformValue(requirement)
          if matches!(requirement.source, UniformitySource::Argument(_))
      )
    })
  }
  /// Whether these effects include an exit from the enclosing function or
  /// loop: a `return`, `break`, or `continue`.
  pub fn exits(&self) -> bool {
    ExitKind::ALL
      .into_iter()
      .any(|kind| self.0.contains(&kind.effect()))
  }
  /// What decides whether these effects' exits happen.
  pub fn exit_sources(&self) -> HashSet<UniformitySource> {
    self
      .0
      .iter()
      .filter_map(|e| match e {
        Effect::ExitDependsOn(_, source) => Some(source.clone()),
        _ => None,
      })
      .collect()
  }
  /// Whether these effects include something whose uniformity a condition
  /// they run under decides (see `Self::condition_on`).
  pub fn depend_on_conditions(&self) -> bool {
    self.0.iter().any(|e| {
      matches!(
        e,
        Effect::RequiresUniformControl(_)
          | Effect::ModifiesLocalVar(_)
          | Effect::Return
          | Effect::Break
          | Effect::Continue
      )
    })
  }
  /// Records that these effects happen only where `condition` lets them,
  /// whose uniformity depends on `sources`: each uniform-control
  /// requirement then also requires `sources` to be uniform, and each local
  /// write and exit depends on them.
  pub fn condition_on(
    &mut self,
    sources: &HashSet<UniformitySource>,
    condition: &SourceTrace,
  ) {
    if sources.is_empty() {
      return;
    }
    let mut conditioned = vec![];
    for e in self.0.iter() {
      match e {
        Effect::RequiresUniformControl(requirement) => {
          conditioned.extend(sources.iter().map(|source| {
            Effect::RequiresUniformValue(Arc::new(UniformValueRequirement {
              source: source.clone(),
              requirement: requirement.clone(),
              condition: condition.clone(),
            }))
          }))
        }
        Effect::ModifiesLocalVar(var) => {
          conditioned.extend(sources.iter().map(|source| {
            Effect::LocalWriteDependsOn(var.clone(), source.clone())
          }))
        }
        _ => {}
      }
    }
    for kind in ExitKind::ALL {
      if self.0.contains(&kind.effect()) {
        conditioned.extend(
          sources
            .iter()
            .map(|source| Effect::ExitDependsOn(kind, source.clone())),
        );
      }
    }
    self.0.extend(conditioned);
  }
  /// Replaces each dependence on a source `resolve` knows with dependences
  /// on what that source holds.
  pub fn resolve_uniformity_sources(
    &mut self,
    mut resolve: impl FnMut(&UniformitySource) -> Option<HashSet<UniformitySource>>,
  ) {
    let resolved: Vec<(Effect, HashSet<UniformitySource>)> = self
      .0
      .iter()
      .filter_map(|e| Some((e.clone(), resolve(Self::uniformity_source(e)?)?)))
      .collect();
    for (effect, sources) in resolved {
      self.0.remove(&effect);
      self
        .0
        .extend(sources.into_iter().map(|source| match &effect {
          Effect::RequiresUniformValue(requirement) => {
            Effect::RequiresUniformValue(Arc::new(UniformValueRequirement {
              source,
              requirement: requirement.requirement.clone(),
              condition: requirement.condition.clone(),
            }))
          }
          Effect::LocalWriteDependsOn(var, _) => {
            Effect::LocalWriteDependsOn(var.clone(), source)
          }
          Effect::ExitDependsOn(kind, _) => {
            Effect::ExitDependsOn(*kind, source)
          }
          _ => unreachable!(),
        }));
    }
  }
  /// Resolves dependences on each variable `bindings` binds into what it
  /// holds: the sources of its value on entry, given, and of every write
  /// to it among these effects.
  pub fn bind_uniformity_sources(
    &mut self,
    bindings: impl IntoIterator<Item = (Arc<str>, HashSet<UniformitySource>)>,
  ) {
    let mut held: HashMap<Arc<str>, HashSet<UniformitySource>> =
      bindings.into_iter().collect();
    for e in self.0.iter() {
      if let Effect::LocalWriteDependsOn(var, source) = e
        && let Some(sources) = held.get_mut(var)
        && source != &UniformitySource::Var(var.clone())
      {
        sources.insert(source.clone());
      }
    }
    self.resolve_uniformity_sources(|source| {
      UniformitySource::resolve_held(source, &held)
    });
  }
  /// Leaves the scope of the local variables `names`: what concerns only
  /// them (reading or writing them) doesn't concern anything outside.
  pub fn leave_scope(&mut self, names: &HashSet<Arc<str>>) {
    self.0.retain(|e| match e {
      Effect::ReadsVar(name)
      | Effect::ReadsArrayLength(name)
      | Effect::ModifiesLocalVar(name)
      | Effect::LocalWriteDependsOn(name, _) => !names.contains(name),
      _ => true,
    });
  }
  /// Leaves a function with the parameters `params`: its own parameters
  /// are out of scope, and its `return`s end there.
  pub fn leave_function(&mut self, params: &HashSet<Arc<str>>) {
    self.leave_scope(params);
    self.0.retain(|e| {
      !matches!(
        e,
        Effect::Return | Effect::ExitDependsOn(ExitKind::Return, _)
      )
    });
  }
  /// Leaves a loop: its `break`s and `continue`s end there.
  pub fn leave_loop(&mut self) {
    self.0.retain(|e| {
      !matches!(
        e,
        Effect::Break
          | Effect::Continue
          | Effect::ExitDependsOn(ExitKind::Break | ExitKind::Continue, _)
      )
    });
  }
  /// Whether these effects include a builtin only shaders have, which no
  /// CPU or audio code can run.
  pub fn uses_shader_only_builtin(&self) -> bool {
    self.0.iter().any(|e| {
      matches!(
        e,
        Effect::FragmentExclusiveFunction(_)
          | Effect::LookupBuiltinAttribute(_)
          | Effect::Barrier(_)
      )
    })
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
  /// CPU-exclusive builtins that AUDIO code may nonetheless call: the
  /// dynamic-array constructors are VM-native heap operations, so the
  /// audio thread executes them directly — allocation on the audio hot
  /// path is a tradeoff the user is allowed to make. Everything else in
  /// the CPU-exclusive family stays main-thread-only (host ops — IO,
  /// windowing, GPU dispatch — with no audio-side implementation).
  /// Shader contexts reject all of them regardless.
  pub fn cpu_exclusive_function_allowed_in_audio(name: &str) -> bool {
    matches!(
      name,
      "into-dynamic-array"
        | "zeroed-array"
        | "push"
        | "insert"
        | "remove"
        | "concat"
        | "reverse"
        | "subarray"
        | "audio-time"
        | "reset-audio-time"
    ) || MidiOutKind::BUILTIN_NAMES.contains(&name)
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
  /// The globals seeded inside a `start-audio` call (see
  /// `Effect::SeedsGlobalVar`). Only the thread-sharing audience analysis
  /// consumes these.
  pub fn seeded_globals(&self) -> Vec<Arc<str>> {
    self
      .0
      .iter()
      .filter_map(|effect| match effect {
        Effect::SeedsGlobalVar(name) => Some(name.clone()),
        _ => None,
      })
      .collect()
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
