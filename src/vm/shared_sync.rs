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

/// Publishes this replica's shared variables into the table: the dirty ones
/// normally, or all of them when `force_all` (the `start-audio` bootstrap,
/// so a brand-new replica can adopt the current state of everything).
/// No-ops while the table is inactive — programs that never start another
/// thread pay nothing at their boundaries. Calls `on_publish` with each
/// published variable's index (test tracing; production passes `|_| {}`,
/// which monomorphizes away).
pub fn publish_shared(
  stack: &[u32],
  dyn_memory: &[DynMemory],
  shared: &mut SharedStateParts<'_>,
  shared_vars: &[SharedVarInfo],
  table: &ThreadSharedTable,
  force_all: bool,
  mut on_publish: impl FnMut(u16),
) {
  if !table.is_active() {
    return;
  }
  for (index, info) in shared_vars.iter().enumerate() {
    if !(force_all || shared.dirty[index]) {
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
/// slots / dynamic memory. Calls `on_adopt` with each adopted variable's
/// index.
pub fn adopt_shared(
  stack: &mut [u32],
  dyn_memory: &mut [DynMemory],
  shared: &mut SharedStateParts<'_>,
  shared_vars: &[SharedVarInfo],
  table: &ThreadSharedTable,
  mut on_adopt: impl FnMut(u16),
) {
  if !table.is_active() {
    // Nothing can have been published yet: publication no-ops while
    // inactive, and activation precedes the bootstrap publish.
    return;
  }
  for (index, info) in shared_vars.iter().enumerate() {
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
    force_all: bool,
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
      force_all,
      on_publish,
    );
  }
  /// [`adopt_shared`] over this program's replica.
  pub fn adopt_shared(
    &mut self,
    table: &ThreadSharedTable,
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
      on_adopt,
    );
  }
}
