//! Cross-thread sharing of global variables.
//!
//! Easl's sharing model is boundary-batched replicated coherence: every
//! thread (the main `@cpu` thread, the `start-audio` thread, and — through
//! its own pre-existing upload/readback machinery — the GPU) holds its own
//! replica of each *shared* global, where "shared" is determined
//! statically (see `Program::thread_shared_globals`). Writes touch only
//! the local replica; at each thread's iteration boundary (a window frame,
//! an audio callback batch) the thread *publishes* snapshots of the shared
//! variables it wrote and *adopts* newer snapshots published by others.
//! Within an iteration, every read is stable.
//!
//! Consequences, deliberately accepted: values cross threads only at
//! boundaries (each thread's per-iteration writes become visible as one
//! atomic batch); concurrent writers resolve by whole-variable
//! last-writer-wins, so interleaved element writes to one array from two
//! threads is a documented footgun rather than an error.
//!
//! This module holds the runtime coordination primitive. It is deliberately
//! free of `unsafe`: snapshots are immutable once published and handed
//! around as `Arc`s via lock-free `arc_swap`, so adopters can never observe
//! a torn write, and publishers never block adopters (nor vice versa) —
//! which is what makes adoption safe inside the real-time audio callback.

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

use arc_swap::ArcSwapOption;

/// One published state of one shared variable: its value as flat words in
/// the VM layout, stamped with a version. Immutable once published.
pub struct SharedSnapshot {
  pub version: u64,
  pub words: Vec<u32>,
}

/// The coordination slot for one shared variable.
pub struct SharedVarSlot {
  published: ArcSwapOption<SharedSnapshot>,
  /// Monotone version source. `fetch_add` gives every publisher a unique,
  /// increasing version even under concurrent publication (in which case
  /// swap order may invert version order: one of the writes is lost —
  /// whole-variable last-writer-wins — but versions never repeat and the
  /// slot can never get stuck behind a stale maximum).
  version_counter: AtomicU64,
}

impl SharedVarSlot {
  pub fn new() -> Self {
    Self {
      published: ArcSwapOption::const_empty(),
      version_counter: AtomicU64::new(0),
    }
  }

  /// Publishes `words` as the newest version of this variable. Returns the
  /// assigned version, plus the previous snapshot's buffer for reuse when
  /// no adopter still holds it — in the steady state (adopters
  /// copy-and-drop) publication allocates nothing.
  pub fn publish(&self, words: Vec<u32>) -> (u64, Option<Vec<u32>>) {
    let version = self.version_counter.fetch_add(1, Ordering::Relaxed) + 1;
    let old = self
      .published
      .swap(Some(Arc::new(SharedSnapshot { version, words })));
    (
      version,
      old
        .and_then(|arc| Arc::try_unwrap(arc).ok())
        .map(|snapshot| snapshot.words),
    )
  }

  /// Returns the current published snapshot if its version is newer than
  /// `last_adopted`. Lock-free; the caller copies the words into its
  /// replica and drops the `Arc` promptly (holding it only delays buffer
  /// reuse, never correctness).
  pub fn adopt_if_newer(
    &self,
    last_adopted: u64,
  ) -> Option<Arc<SharedSnapshot>> {
    let current = self.published.load();
    match &*current {
      Some(snapshot) if snapshot.version > last_adopted => {
        Some(Arc::clone(snapshot))
      }
      _ => None,
    }
  }
}

/// The shared-variable coordination table for one running program: one slot
/// per statically-shared global, index-aligned with the `shared_vars` list
/// every compiled artifact of the program carries (the list is derived once
/// and sorted by name, so indices agree across the main program, the audio
/// program, and the tree-walking environment).
pub struct ThreadSharedTable {
  pub slots: Vec<SharedVarSlot>,
  /// False until a second replica exists (i.e. until `start-audio` fires).
  /// Publishers skip all work while inactive, so programs that never start
  /// another thread pay nothing at their boundaries.
  active: AtomicBool,
}

impl ThreadSharedTable {
  pub fn new(var_count: usize) -> Self {
    Self {
      slots: (0..var_count).map(|_| SharedVarSlot::new()).collect(),
      active: AtomicBool::new(false),
    }
  }
  pub fn activate(&self) {
    self.active.store(true, Ordering::Release);
  }
  pub fn is_active(&self) -> bool {
    self.active.load(Ordering::Acquire)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn publish_and_adopt_roundtrip() {
    let slot = SharedVarSlot::new();
    assert!(slot.adopt_if_newer(0).is_none());
    slot.publish(vec![1, 2, 3]);
    let snapshot = slot.adopt_if_newer(0).unwrap();
    assert_eq!(snapshot.version, 1);
    assert_eq!(snapshot.words, vec![1, 2, 3]);
    // same version isn't re-adopted
    assert!(slot.adopt_if_newer(snapshot.version).is_none());
    // a newer publish is
    slot.publish(vec![4]);
    let newer = slot.adopt_if_newer(snapshot.version).unwrap();
    assert_eq!(newer.version, 2);
    assert_eq!(newer.words, vec![4]);
  }

  #[test]
  fn publish_reuses_unheld_buffers() {
    let slot = SharedVarSlot::new();
    assert!(slot.publish(vec![0; 64]).1.is_none());
    // nobody holds the old snapshot: its buffer comes back for reuse
    let (version, reused) = slot.publish(vec![1; 64]);
    assert_eq!(version, 2);
    assert_eq!(reused.unwrap().len(), 64);
    // an adopter holding the snapshot blocks reuse of that buffer
    let held = slot.adopt_if_newer(0).unwrap();
    assert!(slot.publish(vec![2; 64]).1.is_none());
    drop(held);
  }

  /// Invariant-based stress test with real threads: a writer continuously
  /// publishing buffers filled with a single stamp value while a reader
  /// adopts as fast as it can. Every adopted snapshot must be internally
  /// consistent (all elements equal — no tearing is structurally possible,
  /// this pins it) and versions must be strictly increasing.
  #[test]
  fn concurrent_publish_adopt_never_tears() {
    let slot = Arc::new(SharedVarSlot::new());
    let writer_slot = Arc::clone(&slot);
    let writer = std::thread::spawn(move || {
      for stamp in 1u32..5000 {
        writer_slot.publish(vec![stamp; 512]);
      }
    });
    let mut last_version = 0;
    let mut adopted_count = 0;
    while adopted_count < 100 || !writer.is_finished() {
      if let Some(snapshot) = slot.adopt_if_newer(last_version) {
        assert!(
          snapshot.version > last_version,
          "versions must be strictly increasing"
        );
        let first = snapshot.words[0];
        assert!(
          snapshot.words.iter().all(|w| *w == first),
          "adopted a torn snapshot"
        );
        last_version = snapshot.version;
        adopted_count += 1;
      }
    }
    writer.join().unwrap();
  }

  /// Concurrent publishers from two threads: versions stay unique and the
  /// slot always converges to one of the two writers' final values.
  #[test]
  fn concurrent_publishers_converge() {
    let slot = Arc::new(SharedVarSlot::new());
    let handles: Vec<_> = [10_000u32, 20_000u32]
      .into_iter()
      .map(|base| {
        let slot = Arc::clone(&slot);
        std::thread::spawn(move || {
          for i in 0..1000 {
            slot.publish(vec![base + i; 16]);
          }
        })
      })
      .collect();
    for handle in handles {
      handle.join().unwrap();
    }
    let final_snapshot = slot.adopt_if_newer(0).unwrap();
    let value = final_snapshot.words[0];
    assert!(value == 10_999 || value == 20_999);
    assert_eq!(final_snapshot.version, 2000);
  }
}
