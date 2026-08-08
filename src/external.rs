//! The embedder-facing side of cross-thread variable sharing.
//!
//! A global var marked `@external` can be read and written by the host
//! Rust program through an [`ExternalVars`] handle, with easl's normal
//! sharing semantics doing all the propagation: the handle is one more
//! participant in the boundary-batched system (see `src/thread_sync.rs`),
//! holding its own replica of each shared variable. Every read adopts the
//! newest published snapshot first; every write publishes a new snapshot,
//! which the main thread adopts at its next frame boundary (uploading to
//! the GPU if the var is GPU-bound) and the audio thread at its next
//! callback batch. A var touched only by audio code and the embedder flows
//! between them directly — the main thread never copies it.
//!
//! Usage: validate the program, create the handle with
//! [`ExternalVars::new`], hand a clone to one of the `*_with_external_*`
//! run functions, and call the read/write methods from any thread. The
//! handle must be created from the *same* validated `Program` the runner
//! receives — the shared-variable list is derived from the program, and
//! the two derivations must agree.
//!
//! Semantics to be aware of:
//! - The handle has no iteration boundary: each call is its own boundary,
//!   so two consecutive reads may see different values.
//! - Index writes are read-modify-write on the **whole variable** (adopt
//!   newest, overwrite one element, publish everything): if another thread
//!   writes other elements of the same array between boundaries, one
//!   side's elements win wholesale — same last-writer-wins footgun as any
//!   multi-writer shared var. Giving each variable a single writing thread
//!   avoids it entirely.
//! - Reads before the program has published (i.e. before the entry starts
//!   running, if the embedder hasn't written first) see zeros / an empty
//!   array, not the var's initializer — the initializer value arrives with
//!   the entry-start bootstrap publish.
//!
//! The `_raw` methods speak the VM word layout directly and are the real
//! implementation; the `Value` methods are thin conversion wrappers. The
//! word layout is a public contract: scalars are one word (`f32`/`i32` as
//! raw bits, `bool` as 0/1), vectors and matrices are their scalars in
//! order (matrices column-major), structs are their fields in declaration
//! order, and arrays are their elements consecutively with no padding.

use std::collections::HashMap;
use std::fmt::Display;
use std::sync::{Arc, Mutex};

use crate::compiler::program::Program;
use crate::compiler::types::{ConcreteArraySize, Type};
use crate::interpreter::{
  Value, shared_words_to_value, value_to_shared_words, vm_words_of,
};
use crate::thread_sync::{ThreadSharedTable, participant};

#[derive(Debug, Clone, PartialEq)]
pub enum ExternalVarError {
  /// The name isn't a shared global at all.
  UnknownVar(String),
  /// The name is a shared global, but isn't marked `@external`.
  NotExternal(String),
  /// An `_index` method was called on a non-array variable.
  NotAnArray(String),
  IndexOutOfBounds {
    var: String,
    index: u32,
    length: usize,
  },
  /// A raw write's word count doesn't match the variable's layout: fixed
  /// size vars need exactly their word length, unsized arrays a multiple
  /// of their element stride.
  WrongWordCount { expected: usize, got: usize },
  /// A `Value` write's type doesn't produce the right number of words.
  WrongValueShape { expected: usize, got: usize },
}

impl Display for ExternalVarError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ExternalVarError::UnknownVar(name) => {
        write!(f, "`{name}` is not a shared global variable")
      }
      ExternalVarError::NotExternal(name) => write!(
        f,
        "the global variable `{name}` is not marked `@external`"
      ),
      ExternalVarError::NotAnArray(name) => write!(
        f,
        "`{name}` is not an array; index-based access isn't available"
      ),
      ExternalVarError::IndexOutOfBounds { var, index, length } => write!(
        f,
        "index {index} is out of bounds for `{var}` (length {length})"
      ),
      ExternalVarError::WrongWordCount { expected, got } => write!(
        f,
        "wrong number of words: expected {expected}, got {got}"
      ),
      ExternalVarError::WrongValueShape { expected, got } => write!(
        f,
        "value has the wrong shape: expected {expected} words, got {got}"
      ),
    }
  }
}

impl std::error::Error for ExternalVarError {}

struct ExternalVarLayout {
  name: Arc<str>,
  ty: Type,
  audience: u32,
  /// Words per element for array vars (`None` for non-arrays).
  element_stride: Option<usize>,
  /// Total word length for fixed-size vars (`None` for unsized arrays).
  word_len: Option<usize>,
}

/// The embedder's replica of one shared variable, plus the version it last
/// adopted from the table.
struct ReplicaVar {
  words: Vec<u32>,
  adopted: u64,
}

/// An embedder's handle to a running program's `@external` global
/// variables. Create with [`ExternalVars::new`] from the validated
/// `Program`, pass a clone of the `Arc` into the runner, and read/write
/// from any thread — the handle is `Send + Sync`, reads are lock-free at
/// the table level (one internal mutex serializes the handle's own
/// replica), and neither side ever blocks the audio callback.
pub struct ExternalVars {
  table: Arc<ThreadSharedTable>,
  vars: Vec<ExternalVarLayout>,
  by_name: HashMap<Arc<str>, usize>,
  replica: Mutex<Vec<ReplicaVar>>,
}

impl ExternalVars {
  /// Builds a handle for `program`, which must already be validated (the
  /// shared-variable analysis reads effect data that only exists after
  /// `validate_raw_program`). Creating the handle marks the external
  /// participant live, so the program's boundaries will start publishing
  /// external-audience variables as soon as it runs.
  pub fn new(program: &Program) -> Arc<Self> {
    let vars: Vec<ExternalVarLayout> = program
      .thread_shared_globals()
      .into_iter()
      .map(|(name, audience)| {
        let ty = program
          .top_level_vars
          .iter()
          .find(|v| v.name == name)
          .expect("thread-shared global missing from top-level vars")
          .var_type
          .clone();
        let (element_stride, word_len) = match &ty {
          Type::Array(size, element_type) => {
            let stride =
              vm_words_of(&element_type.kind.unwrap_known()).max(1);
            let len = match size {
              Some(ConcreteArraySize::Literal(n)) => {
                Some(*n as usize * stride)
              }
              _ => None,
            };
            (Some(stride), len)
          }
          other => (None, Some(vm_words_of(other))),
        };
        ExternalVarLayout {
          name,
          ty,
          audience,
          element_stride,
          word_len,
        }
      })
      .collect();
    let by_name = vars
      .iter()
      .enumerate()
      .map(|(index, var)| (var.name.clone(), index))
      .collect();
    let table = Arc::new(ThreadSharedTable::new(vars.len()));
    table.join(participant::EXTERNAL);
    let replica = Mutex::new(
      vars
        .iter()
        .map(|var| ReplicaVar {
          words: vec![0; var.word_len.unwrap_or(0)],
          adopted: 0,
        })
        .collect(),
    );
    Arc::new(Self {
      table,
      vars,
      by_name,
      replica,
    })
  }

  /// The underlying [`ThreadSharedTable`]. Embedders that run their own
  /// replica engines (e.g. a custom audio voice driving
  /// `BytecodeProgram::adopt_shared`/`publish_shared` directly) can wire
  /// them to this table; most embedders never need it.
  pub fn table(&self) -> Arc<ThreadSharedTable> {
    self.table.clone()
  }

  /// The shared table for the runtime that executes the program this
  /// handle was built from. Panics if the runtime's shared-variable count
  /// disagrees — that means the handle was built from a different program.
  pub(crate) fn table_for_env(
    &self,
    expected_len: usize,
  ) -> Arc<ThreadSharedTable> {
    assert_eq!(
      self.vars.len(),
      expected_len,
      "ExternalVars handle was built from a different program than the one \
       being run"
    );
    self.table.clone()
  }

  fn lookup(&self, name: &str) -> Result<usize, ExternalVarError> {
    let index = *self
      .by_name
      .get(name)
      .ok_or_else(|| ExternalVarError::UnknownVar(name.to_string()))?;
    if self.vars[index].audience & participant::EXTERNAL == 0 {
      return Err(ExternalVarError::NotExternal(name.to_string()));
    }
    Ok(index)
  }

  /// Adopts the newest published snapshot of var `index` into the locked
  /// replica, if there is one.
  fn refresh(&self, index: usize, replica: &mut Vec<ReplicaVar>) {
    if let Some(snapshot) =
      self.table.slots[index].adopt_if_newer(replica[index].adopted)
    {
      replica[index].words.clear();
      replica[index].words.extend_from_slice(&snapshot.words);
      replica[index].adopted = snapshot.version;
    }
  }

  /// Publishes the locked replica's current words for var `index` and
  /// records the new version as adopted (re-adopting our own publication
  /// would be a wasted copy).
  fn publish(&self, index: usize, replica: &mut Vec<ReplicaVar>) {
    let (version, _reusable_buffer) =
      self.table.slots[index].publish(replica[index].words.clone());
    replica[index].adopted = version;
  }

  /// The current value of `name` as flat VM-layout words: adopts the
  /// newest published snapshot, then returns a copy of the replica.
  pub fn read_external_var_raw(
    &self,
    name: &str,
  ) -> Result<Vec<u32>, ExternalVarError> {
    let index = self.lookup(name)?;
    let mut replica = self.replica.lock().unwrap();
    self.refresh(index, &mut replica);
    Ok(replica[index].words.clone())
  }

  /// Overwrites `name` with the given VM-layout words and publishes the
  /// new snapshot. Fixed-size vars require exactly their word length;
  /// unsized arrays any multiple of their element stride (which sets the
  /// array's new length).
  pub fn write_external_var_raw(
    &self,
    name: &str,
    words: &[u32],
  ) -> Result<(), ExternalVarError> {
    let index = self.lookup(name)?;
    let var = &self.vars[index];
    if let Some(expected) = var.word_len {
      if words.len() != expected {
        return Err(ExternalVarError::WrongWordCount {
          expected,
          got: words.len(),
        });
      }
    } else if let Some(stride) = var.element_stride
      && words.len() % stride != 0
    {
      return Err(ExternalVarError::WrongWordCount {
        expected: stride,
        got: words.len() % stride,
      });
    }
    let mut replica = self.replica.lock().unwrap();
    replica[index].words.clear();
    replica[index].words.extend_from_slice(words);
    self.publish(index, &mut replica);
    Ok(())
  }

  /// The current words of element `element_index` of the array var
  /// `name` (adopting the newest snapshot first).
  pub fn read_external_var_index_raw(
    &self,
    name: &str,
    element_index: u32,
  ) -> Result<Vec<u32>, ExternalVarError> {
    let index = self.lookup(name)?;
    let stride = self.vars[index]
      .element_stride
      .ok_or_else(|| ExternalVarError::NotAnArray(name.to_string()))?;
    let mut replica = self.replica.lock().unwrap();
    self.refresh(index, &mut replica);
    let words = &replica[index].words;
    let start = element_index as usize * stride;
    if start + stride > words.len() {
      return Err(ExternalVarError::IndexOutOfBounds {
        var: name.to_string(),
        index: element_index,
        length: words.len() / stride,
      });
    }
    Ok(words[start..start + stride].to_vec())
  }

  /// Overwrites element `element_index` of the array var `name`. This is
  /// read-modify-write on the whole variable: the newest snapshot is
  /// adopted, the one element replaced, and the whole array republished.
  pub fn write_external_var_index_raw(
    &self,
    name: &str,
    element_index: u32,
    element_words: &[u32],
  ) -> Result<(), ExternalVarError> {
    let index = self.lookup(name)?;
    let stride = self.vars[index]
      .element_stride
      .ok_or_else(|| ExternalVarError::NotAnArray(name.to_string()))?;
    if element_words.len() != stride {
      return Err(ExternalVarError::WrongWordCount {
        expected: stride,
        got: element_words.len(),
      });
    }
    let mut replica = self.replica.lock().unwrap();
    self.refresh(index, &mut replica);
    let words = &mut replica[index].words;
    let start = element_index as usize * stride;
    if start + stride > words.len() {
      return Err(ExternalVarError::IndexOutOfBounds {
        var: name.to_string(),
        index: element_index,
        length: words.len() / stride,
      });
    }
    words[start..start + stride].copy_from_slice(element_words);
    self.publish(index, &mut replica);
    Ok(())
  }

  /// [`Self::read_external_var_raw`], decoded to a [`Value`].
  pub fn read_external_var(
    &self,
    name: &str,
  ) -> Result<Value, ExternalVarError> {
    let words = self.read_external_var_raw(name)?;
    let index = self.lookup(name)?;
    Ok(shared_words_to_value(&words, &self.vars[index].ty))
  }

  /// [`Self::write_external_var_raw`], encoding a [`Value`].
  pub fn write_external_var(
    &self,
    name: &str,
    value: &Value,
  ) -> Result<(), ExternalVarError> {
    let index = self.lookup(name)?;
    let words = value_to_shared_words(value, &self.vars[index].ty);
    self.write_external_var_raw(name, &words)
  }

  /// [`Self::read_external_var_index_raw`], decoded to a [`Value`] of the
  /// array's element type.
  pub fn read_external_var_index(
    &self,
    name: &str,
    element_index: u32,
  ) -> Result<Value, ExternalVarError> {
    let words = self.read_external_var_index_raw(name, element_index)?;
    let index = self.lookup(name)?;
    let Type::Array(_, element_type) = &self.vars[index].ty else {
      return Err(ExternalVarError::NotAnArray(name.to_string()));
    };
    Ok(Value::from_vm_words(
      &element_type.kind.unwrap_known(),
      &words,
    ))
  }

  /// [`Self::write_external_var_index_raw`], encoding a [`Value`] of the
  /// array's element type.
  pub fn write_external_var_index(
    &self,
    name: &str,
    element_index: u32,
    value: &Value,
  ) -> Result<(), ExternalVarError> {
    let index = self.lookup(name)?;
    let Type::Array(_, element_type) = &self.vars[index].ty else {
      return Err(ExternalVarError::NotAnArray(name.to_string()));
    };
    let element_type = element_type.kind.unwrap_known();
    let words = value.to_vm_words(&element_type);
    let stride = self.vars[index].element_stride.unwrap_or(1);
    if words.len() != stride {
      return Err(ExternalVarError::WrongValueShape {
        expected: stride,
        got: words.len(),
      });
    }
    self.write_external_var_index_raw(name, element_index, &words)
  }
}
