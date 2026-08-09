//! The VM side of cross-thread global sharing: publishing a program's dirty
//! shared variables into the [`ThreadSharedTable`] and adopting newer
//! published snapshots into the program's replica. Called at iteration
//! boundaries — the frame loop (via the `FrameDriver` implementations), the
//! audio callback batch (via `VmAudioDriver`), and `start-audio`'s
//! bootstrap publish. The tree-walking interpreter has its own `Value`-based
//! equivalents in `interpreter.rs`; both go through the same
//! [`ThreadSharedTable`] slots, index-aligned by the sorted shared-variable
//! list every compiled artifact carries.

use crate::thread_sync::ThreadSharedTable;
use crate::vm::bytecode::{
  BytecodeProgram, DynMemory, SharedStateParts, SharedVarInfo,
  SharedVarStorage,
};

/// Publishes this replica's shared variables into the table, acting as the
/// participant identified by `self_bit`: the dirty ones normally, plus any
/// var whose audience intersects `force_mask` regardless of dirtiness (the
/// `start-audio` bootstrap passes `participant::AUDIO`, so the brand-new
/// replica can adopt the current state of everything it can see). A var is
/// published only when some *other* live participant is in its audience —
/// programs with no second participant pay one atomic load per boundary
/// and nothing else. Calls `on_publish` with each published variable's
/// index (test tracing; production passes `|_| {}`, which monomorphizes
/// away).
pub fn publish_shared(
  stack: &[u32],
  dyn_memory: &[DynMemory],
  shared: &mut SharedStateParts<'_>,
  shared_vars: &[SharedVarInfo],
  table: &ThreadSharedTable,
  self_bit: u32,
  force_mask: u32,
  mut on_publish: impl FnMut(u16),
) {
  let live_others = table.live_others(self_bit);
  if live_others == 0 {
    return;
  }
  for (index, info) in shared_vars.iter().enumerate() {
    if info.audience & live_others == 0 {
      continue;
    }
    // A forced publish is bootstrap gap-filling for a newly-joined
    // participant: it applies only to vars this participant's own code can
    // touch (its replica of anything else is never authoritative) and only
    // when no snapshot exists yet — the newcomer's first adopt takes an
    // existing snapshot anyway, and overwriting one would clobber state
    // published by others (an embedder's pre-run slider seed, say — pinned
    // by the `external_seed_survives_start_audio` golden).
    let forced = info.audience & force_mask != 0
      && info.audience & self_bit != 0
      && !table.slots[index].has_published();
    if !(forced || shared.dirty[index]) {
      continue;
    }
    let mut buffer =
      shared.scratch[index].take().unwrap_or_else(Vec::new);
    buffer.clear();
    match info.storage {
      SharedVarStorage::Slots { position, size } => {
        buffer.extend_from_slice(
          &stack[position as usize..(position + size) as usize],
        );
      }
      SharedVarStorage::DynMemory { region, stride } => {
        match &dyn_memory[region as usize] {
          DynMemory::Words(words) => buffer.extend_from_slice(words),
          DynMemory::Zeroed { elements } => {
            buffer.resize(*elements as usize * stride as usize, 0);
          }
          DynMemory::Cells(_) => panic!(
            "thread-shared variables with heap-backed element types \
             (nested runtime-sized arrays) are not supported: their \
             elements are references into one runtime's heap and can't \
             cross to another"
          ),
        }
      }
    }
    let (version, returned) = table.slots[index].publish(buffer);
    // adopting our own publication would be a wasted copy — we already
    // hold exactly this state
    shared.adopted[index] = version;
    shared.dirty[index] = false;
    shared.scratch[index] = returned;
    on_publish(index as u16);
  }
}

/// Adopts any shared variable whose published version is newer than this
/// replica's last-adopted version, copying the snapshot into the local
/// slots / dynamic memory. Only variables in this participant's own
/// audience are considered — a var shared between the audio thread and an
/// embedder handle flows between them directly, without the main thread
/// ever copying it. Calls `on_adopt` with each adopted variable's index.
pub fn adopt_shared(
  stack: &mut [u32],
  dyn_memory: &mut [DynMemory],
  shared: &mut SharedStateParts<'_>,
  shared_vars: &[SharedVarInfo],
  table: &ThreadSharedTable,
  self_bit: u32,
  mut on_adopt: impl FnMut(u16),
) {
  if table.live_others(self_bit) == 0 {
    // No other participant exists, so nothing can have been published.
    return;
  }
  for (index, info) in shared_vars.iter().enumerate() {
    if info.audience & self_bit == 0 {
      continue;
    }
    let Some(snapshot) =
      table.slots[index].adopt_if_newer(shared.adopted[index])
    else {
      continue;
    };
    match info.storage {
      SharedVarStorage::Slots { position, size } => {
        let n = (size as usize).min(snapshot.words.len());
        stack[position as usize..position as usize + n]
          .copy_from_slice(&snapshot.words[..n]);
      }
      SharedVarStorage::DynMemory { region, stride } => {
        let region = &mut dyn_memory[region as usize];
        let words = region.words_mut(stride as usize);
        words.clear();
        words.extend_from_slice(&snapshot.words);
      }
    }
    shared.adopted[index] = snapshot.version;
    on_adopt(index as u16);
  }
}

impl BytecodeProgram {
  /// [`publish_shared`] over this program's replica.
  pub fn publish_shared(
    &mut self,
    table: &ThreadSharedTable,
    self_bit: u32,
    force_mask: u32,
    on_publish: impl FnMut(u16),
  ) {
    let Self {
      code,
      stack,
      dyn_memory,
      shared_dirty,
      shared_adopted,
      shared_scratch,
      ..
    } = self;
    publish_shared(
      stack,
      dyn_memory,
      &mut SharedStateParts {
        dirty: shared_dirty,
        adopted: shared_adopted,
        scratch: shared_scratch,
      },
      &code.shared_vars,
      table,
      self_bit,
      force_mask,
      on_publish,
    );
  }
  /// [`adopt_shared`] over this program's replica.
  pub fn adopt_shared(
    &mut self,
    table: &ThreadSharedTable,
    self_bit: u32,
    on_adopt: impl FnMut(u16),
  ) {
    let Self {
      code,
      stack,
      dyn_memory,
      shared_dirty,
      shared_adopted,
      shared_scratch,
      ..
    } = self;
    adopt_shared(
      stack,
      dyn_memory,
      &mut SharedStateParts {
        dirty: shared_dirty,
        adopted: shared_adopted,
        scratch: shared_scratch,
      },
      &code.shared_vars,
      table,
      self_bit,
      on_adopt,
    );
  }
}
