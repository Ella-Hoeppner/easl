//! Programs compiled ahead of time for the bytecode VM's CPU runtime: the
//! compiled `@cpu` and audio bytecode, and everything else running them
//! needs (the WGSL, the GPU interface, the runtime's bindings), so a
//! program can run without the compiler. The web runtime's default build
//! runs programs `easl compile --web` precompiled this way.

use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::{
  compiler::{
    entry::EntryPoint, functions::FunctionImplementationKind, program::Program,
    wgsl::MIDI_VARS,
  },
  interpreter::{EnvironmentSpec, EvalError, UserspaceEvalError},
  vm::bytecode::Code,
};

/// A validated program, compiled for the bytecode VM's CPU runtime
/// (`VmCpuRuntime::from_precompiled`). It serializes without its source
/// map, so errors from a deserialized program have no source locations.
#[derive(Serialize, Deserialize)]
pub struct PrecompiledProgram {
  /// The program's `@cpu` entry points, in the order they're written: the
  /// runtime starts whichever one it's given (see [`Self::entry`]).
  pub entry_points: Vec<Arc<str>>,
  pub cpu: Code,
  pub cpu_function_names: Vec<Arc<str>>,
  /// What `start-audio` runs, for a program with audio entry points.
  pub audio: Option<(Code, Vec<Arc<str>>)>,
  pub environment: EnvironmentSpec,
  /// Whether the program reads MIDI input.
  pub reads_midi: bool,
  /// Whether the program sends MIDI.
  pub sends_midi: bool,
  /// The thread-shared globals holding atomics, which the main and audio
  /// threads operate on directly.
  pub shared_atomic_globals: Vec<Arc<str>>,
}

impl PrecompiledProgram {
  /// Compiles a validated program, with every `@cpu` entry point it has.
  pub fn new(program: Program) -> Result<Self, EvalError> {
    let mut entries: Vec<_> = program
      .cpu_entry_points()
      .into_iter()
      .map(|signature| {
        let signature = signature.read().unwrap();
        let position = match &signature.implementation {
          FunctionImplementationKind::Composite(f) => f
            .read()
            .unwrap()
            .name_source_trace
            .primary_position
            .clone()
            .map(|position| (position.path, position.span.start)),
          _ => None,
        };
        (position, signature.name.clone())
      })
      .collect();
    entries.sort();
    let entry_points: Vec<Arc<str>> =
      entries.into_iter().map(|(_, name)| name).collect();
    if entry_points.is_empty() {
      return Err(UserspaceEvalError::NoCpuEntryPoint.into());
    }
    let audio = (!program
      .find_fn_names_by_entry_point(|e| e == EntryPoint::Audio)
      .is_empty())
    .then(|| {
      let (audio, function_names) =
        program.clone().compile_to_bytecode_program();
      (audio.code, function_names)
    });
    let reads_midi = program
      .top_level_vars
      .iter()
      .any(|var| MIDI_VARS.contains(&&*var.name));
    let sends_midi = program.sends_midi();
    let shared_atomic_globals = program
      .thread_shared_globals()
      .into_iter()
      .map(|(name, _)| name)
      .filter(|name| {
        program
          .top_level_vars
          .iter()
          .any(|var| &var.name == name && var.var_type.involves_atomic())
      })
      .collect();
    let environment = EnvironmentSpec::of(program.clone())?;
    let (cpu, cpu_function_names) = program.compile_to_bytecode_program_cpu();
    Ok(Self {
      entry_points,
      cpu: cpu.code,
      cpu_function_names,
      audio,
      environment,
      reads_midi,
      sends_midi,
      shared_atomic_globals,
    })
  }

  /// The entry point to start: `requested`, which must be one of the
  /// program's, or the program's only one when none is requested. The
  /// error names the entry points to choose from.
  pub fn entry(&self, requested: Option<&str>) -> Result<Arc<str>, String> {
    let names = || {
      self
        .entry_points
        .iter()
        .map(|name| format!("`{name}`"))
        .collect::<Vec<_>>()
        .join(", ")
    };
    match requested {
      Some(requested) => self
        .entry_points
        .iter()
        .find(|name| &***name == requested)
        .cloned()
        .ok_or_else(|| {
          format!(
            "the program has no `@cpu` entry point named `{requested}`; its \
             entry points are {}",
            names()
          )
        }),
      None => match self.entry_points.as_slice() {
        [entry] => Ok(entry.clone()),
        _ => Err(format!(
          "the program has several `@cpu` entry points ({}): name the one \
           to run",
          names()
        )),
      },
    }
  }

  /// This program's encoding, for [`Self::from_bytes`].
  pub fn to_bytes(&self) -> Vec<u8> {
    postcard::to_allocvec(self)
      .expect("serializing a precompiled program failed")
  }

  pub fn from_bytes(bytes: &[u8]) -> Result<Self, String> {
    postcard::from_bytes(bytes)
      .map_err(|e| format!("invalid precompiled program: {e}"))
  }
}
