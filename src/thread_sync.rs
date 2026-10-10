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

use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::sync::{Arc, OnceLock};

use arc_swap::ArcSwapOption;

/// Participant identity bits for the sharing system. Every shared variable
/// carries a static *audience* mask — which participants' code can touch
/// it, computed at compile time — and the [`ThreadSharedTable`] tracks
/// which participants are currently *live*. A participant publishes or
/// adopts a variable only when someone **else** who cares about it is
/// live.
///
/// These are participant *classes*, not instances, and classes are always
/// derivable from program structure — which is why a static mask works.
/// The planned multi-threaded `spawn-window` fits by giving each
/// spawn-window *call site* its own class bit (call sites are statically
/// known since closures are compile-time inlinable); u32 leaves 29 bits
/// for them, and overflow sites can share a bit at worst (a coarser
/// audience only ever syncs more conservatively, never less). What that
/// future does require is instance-count-aware liveness — several windows
/// can share one class, so `live_others` masking out the whole class must
/// become "another live *instance* exists" (per-class counters on the
/// table). That change is confined to this module plus the two gating
/// expressions in the publish/adopt paths.
pub mod participant {
  /// The main `@cpu` thread. Always live; also acts as the GPU's proxy
  /// (the GPU has no boundary loop of its own).
  pub const MAIN: u32 = 1;
  /// The `start-audio` thread. Goes live when `start-audio` first fires.
  pub const AUDIO: u32 = 2;
  /// An embedder holding an `ExternalVars` handle. Goes live when the
  /// handle is created (which happens before the run starts).
  pub const EXTERNAL: u32 = 4;
}

/// One published state of one shared variable: its value as flat words in
/// the VM layout, stamped with a version. Immutable once published.
pub struct SharedSnapshot {
  pub version: u64,
  pub words: Vec<u32>,
}

/// The coordination slot for one shared variable.
pub struct SharedVarSlot {
  published: ArcSwapOption<SharedSnapshot>,
  /// Monotone version source: `fetch_add` gives every publisher a unique,
  /// increasing version, even under concurrent publication.
  version_counter: AtomicU64,
  /// For a var holding atomics: its one copy, which every participant
  /// reads and writes directly (no snapshots), so atomic ops from
  /// different threads never lose each other's updates.
  atomic_words: OnceLock<Arc<dyn AtomicWords>>,
}

/// The words of a shared var holding atomics, which every participant
/// operates on directly. Natively they're `AtomicU32`s
/// ([`NativeAtomicWords`]); the web keeps them in a `SharedArrayBuffer`, the
/// only memory its page and audio worklet share.
pub trait AtomicWords: Send + Sync {
  fn len(&self) -> usize;
  fn load(&self, index: usize) -> u32;
  fn store(&self, index: usize, value: u32);
  /// Each read-modify-write returns the word's previous value.
  fn swap(&self, index: usize, value: u32) -> u32;
  fn fetch_add(&self, index: usize, value: u32) -> u32;
  fn fetch_sub(&self, index: usize, value: u32) -> u32;
  fn fetch_and(&self, index: usize, value: u32) -> u32;
  fn fetch_or(&self, index: usize, value: u32) -> u32;
  fn fetch_xor(&self, index: usize, value: u32) -> u32;
  /// Replaces the word with `new` if it holds `current`.
  fn compare_exchange(&self, index: usize, current: u32, new: u32) -> u32;
}

/// An atomic builtin, as applied to a shared word.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AtomicOp {
  Load,
  Store,
  Exchange,
  Add,
  Sub,
  MaxU32,
  MaxI32,
  MinU32,
  MinI32,
  And,
  Or,
  Xor,
}

impl AtomicOp {
  /// The op an atomic builtin performs; `signed` says whether its atomic
  /// holds an `i32` (only asked for `atomic-max`/`atomic-min`).
  pub fn of_builtin(name: &str, signed: impl FnOnce() -> bool) -> Option<Self> {
    let signed = matches!(name, "atomic-max" | "atomic-min") && signed();
    Some(match name {
      "atomic-load" => Self::Load,
      "atomic-store" => Self::Store,
      "atomic-exchange" => Self::Exchange,
      "atomic-add" => Self::Add,
      "atomic-sub" => Self::Sub,
      "atomic-max" if signed => Self::MaxI32,
      "atomic-max" => Self::MaxU32,
      "atomic-min" if signed => Self::MinI32,
      "atomic-min" => Self::MinU32,
      "atomic-and" => Self::And,
      "atomic-or" => Self::Or,
      "atomic-xor" => Self::Xor,
      _ => return None,
    })
  }
}

impl dyn AtomicWords {
  /// Applies `op` to word `index` (wrapped to the words' length) with
  /// `operand`, returning the word's previous value (`Store` returns 0).
  pub fn apply(&self, op: AtomicOp, index: usize, operand: u32) -> u32 {
    let index = index % self.len();
    match op {
      AtomicOp::Load => self.load(index),
      AtomicOp::Store => {
        self.store(index, operand);
        0
      }
      AtomicOp::Exchange => self.swap(index, operand),
      AtomicOp::Add => self.fetch_add(index, operand),
      AtomicOp::Sub => self.fetch_sub(index, operand),
      AtomicOp::And => self.fetch_and(index, operand),
      AtomicOp::Or => self.fetch_or(index, operand),
      AtomicOp::Xor => self.fetch_xor(index, operand),
      AtomicOp::MaxU32 => self.fetch_update(index, |word| word.max(operand)),
      AtomicOp::MinU32 => self.fetch_update(index, |word| word.min(operand)),
      AtomicOp::MaxI32 => self
        .fetch_update(index, |word| (word as i32).max(operand as i32) as u32),
      AtomicOp::MinI32 => self
        .fetch_update(index, |word| (word as i32).min(operand as i32) as u32),
    }
  }

  /// Replaces word `index` with `f` of its value, atomically, returning
  /// its previous value.
  fn fetch_update(&self, index: usize, f: impl Fn(u32) -> u32) -> u32 {
    let mut current = self.load(index);
    loop {
      let previous = self.compare_exchange(index, current, f(current));
      if previous == current {
        return previous;
      }
      current = previous;
    }
  }
}

/// Atomic words in this process's memory.
pub struct NativeAtomicWords(Box<[AtomicU32]>);

impl NativeAtomicWords {
  pub fn zeroed(words: usize) -> Arc<dyn AtomicWords> {
    Arc::new(Self((0..words).map(|_| AtomicU32::new(0)).collect()))
  }
}

impl AtomicWords for NativeAtomicWords {
  fn len(&self) -> usize {
    self.0.len()
  }
  fn load(&self, index: usize) -> u32 {
    self.0[index].load(Ordering::Acquire)
  }
  fn store(&self, index: usize, value: u32) {
    self.0[index].store(value, Ordering::Release)
  }
  fn swap(&self, index: usize, value: u32) -> u32 {
    self.0[index].swap(value, Ordering::AcqRel)
  }
  fn fetch_add(&self, index: usize, value: u32) -> u32 {
    self.0[index].fetch_add(value, Ordering::AcqRel)
  }
  fn fetch_sub(&self, index: usize, value: u32) -> u32 {
    self.0[index].fetch_sub(value, Ordering::AcqRel)
  }
  fn fetch_and(&self, index: usize, value: u32) -> u32 {
    self.0[index].fetch_and(value, Ordering::AcqRel)
  }
  fn fetch_or(&self, index: usize, value: u32) -> u32 {
    self.0[index].fetch_or(value, Ordering::AcqRel)
  }
  fn fetch_xor(&self, index: usize, value: u32) -> u32 {
    self.0[index].fetch_xor(value, Ordering::AcqRel)
  }
  fn compare_exchange(&self, index: usize, current: u32, new: u32) -> u32 {
    match self.0[index].compare_exchange(
      current,
      new,
      Ordering::AcqRel,
      Ordering::Acquire,
    ) {
      Ok(previous) | Err(previous) => previous,
    }
  }
}

impl SharedVarSlot {
  pub fn new() -> Self {
    Self {
      published: ArcSwapOption::const_empty(),
      version_counter: AtomicU64::new(0),
      atomic_words: OnceLock::new(),
    }
  }

  /// The shared words of a var holding atomics, `words` long. The first
  /// participant to ask makes them with `allocate` (zeroed); every
  /// participant asks with the same length — it comes from the var's type
  /// — and gets the same words.
  pub fn atomic_words(
    &self,
    words: usize,
    allocate: impl FnOnce() -> Arc<dyn AtomicWords>,
  ) -> Arc<dyn AtomicWords> {
    let shared = self.atomic_words.get_or_init(allocate);
    assert_eq!(shared.len(), words, "participants disagree on a var's size");
    Arc::clone(shared)
  }

  /// Publishes `words` as a new version of this variable, unless a newer
  /// version is already installed: concurrent publishers take versions in
  /// one order but can finish installing in the other, and the older one
  /// is then dropped rather than installed over the newer one (last writer
  /// wins, by version). Either way, returns the version this publish took —
  /// the publisher records it as adopted, so after a dropped publish it
  /// adopts the newer version at its next boundary like everyone else —
  /// plus a buffer for reuse when nothing else holds it: the replaced
  /// snapshot's, or ours if it wasn't installed. In the steady state
  /// (adopters copy-and-drop) publication allocates nothing.
  pub fn publish(&self, words: Vec<u32>) -> (u64, Option<Vec<u32>>) {
    let version = self.version_counter.fetch_add(1, Ordering::Relaxed) + 1;
    let snapshot = Arc::new(SharedSnapshot { version, words });
    let mut installed = false;
    // Lock-free conditional install: retries only when another publish
    // lands between reading the slot and swapping it.
    let previous = self.published.rcu(|current| match current {
      Some(current) if current.version > version => {
        installed = false;
        Some(Arc::clone(current))
      }
      _ => {
        installed = true;
        Some(Arc::clone(&snapshot))
      }
    });
    let reusable = if installed {
      previous.and_then(|arc| Arc::try_unwrap(arc).ok())
    } else {
      Arc::try_unwrap(snapshot).ok()
    };
    (version, reusable.map(|snapshot| snapshot.words))
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

  /// Whether any snapshot has ever been published to this slot. Used by
  /// the entry-start bootstrap to publish a variable's program-computed
  /// initial value only when an embedder hasn't already seeded it.
  pub fn has_published(&self) -> bool {
    self.published.load().is_some()
  }
}

/// The shared-variable coordination table for one running program: one slot
/// per statically-shared global, index-aligned with the `shared_vars` list
/// every compiled artifact of the program carries (the list is derived once
/// and sorted by name, so indices agree across the main program, the audio
/// program, and the tree-walking environment).
pub struct ThreadSharedTable {
  pub slots: Vec<SharedVarSlot>,
  /// Bitmask of [`participant`] classes that are currently live. Starts as
  /// just `MAIN`; `start-audio` joins `AUDIO`, creating an `ExternalVars`
  /// handle joins `EXTERNAL`. A participant publishes/adopts a variable
  /// only when `audience & live_others(self) != 0` — someone *else* who
  /// cares about it actually exists — so programs that never start another
  /// participant pay one atomic load per boundary and nothing else.
  live: AtomicU32,
}

impl ThreadSharedTable {
  pub fn new(var_count: usize) -> Self {
    Self {
      slots: (0..var_count).map(|_| SharedVarSlot::new()).collect(),
      live: AtomicU32::new(participant::MAIN),
    }
  }
  /// Marks a participant class as live. Monotonic — participants never
  /// leave (an audio stream or embedder handle lives for the run).
  pub fn join(&self, participant: u32) {
    self.live.fetch_or(participant, Ordering::Release);
  }
  /// The live participants other than `self_bit`. Zero means no one else
  /// exists and every publish/adopt can be skipped outright.
  pub fn live_others(&self, self_bit: u32) -> u32 {
    self.live.load(Ordering::Acquire) & !self_bit
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

  /// A publish that took an older version than the one installed (it lost
  /// a race to a concurrent publisher) leaves the newer one in place, and
  /// hands its own buffer back for reuse.
  #[test]
  fn older_publish_is_dropped() {
    let slot = SharedVarSlot::new();
    // The interleaving a race produces: a publisher takes version 1, then
    // another takes version 2 and installs it first.
    slot.version_counter.store(1, Ordering::Relaxed);
    assert_eq!(slot.publish(vec![2; 8]).0, 2);
    slot.version_counter.store(0, Ordering::Relaxed);
    let (version, reusable) = slot.publish(vec![1; 8]);
    assert_eq!(version, 1);
    assert_eq!(reusable, Some(vec![1; 8]));
    let installed = slot.adopt_if_newer(0).unwrap();
    assert_eq!((installed.version, installed.words[0]), (2, 2));
    // the dropped publisher, having recorded version 1, adopts version 2
    assert!(slot.adopt_if_newer(version).is_some());
  }

  /// Two threads publish to one slot at the same moment, round after
  /// round. After each round, the slot must hold the newest version
  /// published: an older snapshot installed over a newer one is skipped by
  /// every participant that already adopted the newer one, so participants
  /// would disagree about the variable until its next publish.
  #[test]
  fn older_version_never_replaces_newer() {
    const ROUNDS: u64 = 200_000;
    let slot = Arc::new(SharedVarSlot::new());
    let barrier = Arc::new(std::sync::Barrier::new(3));
    let writers: Vec<_> = [1u32, 2u32]
      .into_iter()
      .map(|writer| {
        let slot = Arc::clone(&slot);
        let barrier = Arc::clone(&barrier);
        std::thread::spawn(move || {
          for _ in 0..ROUNDS {
            barrier.wait();
            slot.publish(vec![writer]);
            barrier.wait();
          }
        })
      })
      .collect();
    let mut stale_rounds = vec![];
    for round in 1..=ROUNDS {
      barrier.wait();
      barrier.wait();
      let newest_issued = round * 2;
      let installed = slot.adopt_if_newer(0).unwrap().version;
      if installed != newest_issued {
        stale_rounds.push((round, installed, newest_issued));
      }
    }
    for writer in writers {
      writer.join().unwrap();
    }
    assert!(
      stale_rounds.is_empty(),
      "{} of {ROUNDS} rounds left an older version installed over a newer \
       one, e.g. (round, installed version, newest version): {:?}",
      stale_rounds.len(),
      &stale_rounds[..stale_rounds.len().min(5)]
    );
  }
}
