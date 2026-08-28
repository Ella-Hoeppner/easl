//! The VM side of cross-thread global sharing: publishing a program's dirty
//! shared variables into the [`ThreadSharedTable`] and adopting newer
//! published snapshots into the program's replica. Called at iteration
//! boundaries — the frame loop (via the `FrameDriver` implementations), the
//! audio callback batch (via `VmAudioDriver`), and `start-audio`'s
//! bootstrap publish. The tree-walking interpreter has its own `Value`-based
//! equivalents in `interpreter.rs`; both go through the same
//! [`ThreadSharedTable`] slots, index-aligned by the sorted shared-variable
//! list every compiled artifact carries.

use std::sync::Arc;

use crate::compiler::types::{ConcreteArraySize, Type};
use crate::thread_sync::ThreadSharedTable;
use crate::vm::bytecode::{
  BytecodeProgram, DynMemory, HeapCell, SharedStateParts, SharedVarInfo,
  SharedVarStorage, alloc_heap_cell, heap_index, release_heap_id,
};
use crate::vm::compile::vm_stack_size;

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
  heap: &[Option<Arc<HeapCell>>],
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
    let mut buffer = shared.scratch[index].take().unwrap_or_else(Vec::new);
    buffer.clear();
    match info.storage {
      SharedVarStorage::Slots { position, size } => {
        buffer.extend_from_slice(
          &stack[position as usize..(position + size) as usize],
        );
      }
      SharedVarStorage::DynMemory { region, stride } => {
        if needs_wire_encoding(&info.ty) {
          // heap-involving elements: the serialized wire format (see the
          // module comment below) — ids are dereferenced and contents
          // inlined, so nothing heap-private crosses
          let Type::Array(_, element_type) = &info.ty else {
            unreachable!()
          };
          serialize_dyn_memory(
            &element_type.unwrap_known(),
            &dyn_memory[region as usize],
            heap,
            &mut buffer,
          );
        } else {
          match &dyn_memory[region as usize] {
            DynMemory::Words(words) => buffer.extend_from_slice(words),
            DynMemory::Zeroed { elements } => {
              buffer.resize(*elements as usize * stride as usize, 0);
            }
            DynMemory::Cells(_) => {
              unreachable!("Cells storage on a flat-element shared region")
            }
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
/// slots / dynamic memory. Only variables in this participant's own
/// audience are considered — a var shared between the audio thread and an
/// embedder handle flows between them directly, without the main thread
/// ever copying it. Calls `on_adopt` with each adopted variable's index.
pub fn adopt_shared(
  stack: &mut [u32],
  dyn_memory: &mut [DynMemory],
  heap: &mut Vec<Option<Arc<HeapCell>>>,
  heap_free: &mut Vec<u32>,
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
        if needs_wire_encoding(&info.ty) {
          let Type::Array(_, element_type) = &info.ty else {
            unreachable!()
          };
          let element_type = element_type.unwrap_known();
          // the region owns its embedded ids — release them before the
          // wholesale replacement (`Cells` children release via `Drop`)
          release_embedded_region_ids(&element_type, region, heap, heap_free);
          let mut reader = WireReader {
            words: &snapshot.words,
            pos: 0,
          };
          let count = reader.read() as usize;
          *region = deserialize_elements(
            &element_type,
            count,
            &mut reader,
            heap,
            heap_free,
          );
        } else {
          let words = region.words_mut(stride as usize);
          words.clear();
          words.extend_from_slice(&snapshot.words);
        }
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
      heap,
      shared_dirty,
      shared_adopted,
      shared_scratch,
      ..
    } = self;
    publish_shared(
      stack,
      dyn_memory,
      heap,
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
      heap,
      heap_free,
      shared_dirty,
      shared_adopted,
      shared_scratch,
      ..
    } = self;
    adopt_shared(
      stack,
      dyn_memory,
      heap,
      heap_free,
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

// --- The wire encoding for heap-involving shared values ---
//
// A shared variable whose type involves heap values (nested runtime-sized
// arrays, Strings, or elements embedding them) can't publish its raw
// replica words: heap-table ids are private to one runtime's heap, and
// `Cells` children aren't words at all. Such variables publish a
// SERIALIZED snapshot instead — a self-delimiting, type-directed word
// encoding both runtimes (and both directions) speak:
//
//   flat value        -> its flat VM words, verbatim (enum padding incl.)
//   String            -> [char_count, chars...]
//   dyn array [E]     -> [element_count, encode(E) x count]
//   struct w/ heap    -> fields in declaration order, each encoded
//   enum w/ heap      -> [discriminant, encode(that variant's payload)]
//                        (no padding: self-delimiting)
//   fixed array [N:E] -> N x encode(E) (count is static, no prefix)
//
// Serialization dereferences embedded heap ids through the publishing
// heap and inlines their contents; adoption materializes fresh cells in
// the adopting heap and mints its own ids — no id ever crosses. The cost
// is O(total size) per publish/adopt, paid only when the variable was
// actually written that iteration (dirty flags gate publishes, version
// checks gate adopts). Flat-element variables keep the raw-words format
// (and the allocation-free steady state) unchanged.

/// Whether a shared variable of this type uses the serialized wire
/// encoding rather than raw replica words.
pub fn needs_wire_encoding(ty: &Type) -> bool {
  match ty {
    Type::Array(Some(ConcreteArraySize::Unsized), element_type) => {
      let element_type = element_type.unwrap_known();
      element_type.involves_runtime_sized_array()
        || element_type.involves_string()
    }
    _ => false,
  }
}

fn type_is_heap_value(t: &Type) -> bool {
  matches!(
    t,
    Type::Array(Some(ConcreteArraySize::Unsized), _) | Type::String
  )
}

fn type_involves_heap(t: &Type) -> bool {
  t.involves_runtime_sized_array() || t.involves_string()
}

fn serialize_cell(
  t: &Type,
  cell: Option<&Arc<HeapCell>>,
  heap: &[Option<Arc<HeapCell>>],
  out: &mut Vec<u32>,
) {
  match t {
    Type::String => match cell {
      Some(c) => {
        let words = match &c.memory {
          DynMemory::Words(words) => &words[..],
          _ => &[],
        };
        out.push(words.len() as u32);
        out.extend_from_slice(words);
      }
      None => out.push(0),
    },
    Type::Array(Some(ConcreteArraySize::Unsized), element_type) => {
      let element_type = element_type.unwrap_known();
      match cell {
        None => out.push(0),
        Some(c) => serialize_dyn_memory(&element_type, &c.memory, heap, out),
      }
    }
    _ => panic!("serialize_cell on a non-heap-value type"),
  }
}

fn serialize_dyn_memory(
  element_type: &Type,
  memory: &DynMemory,
  heap: &[Option<Arc<HeapCell>>],
  out: &mut Vec<u32>,
) {
  if !type_involves_heap(element_type) {
    let stride = vm_stack_size(element_type).max(1) as usize;
    match memory {
      DynMemory::Zeroed { elements } => {
        out.push(*elements);
        out.extend(std::iter::repeat(0).take(*elements as usize * stride));
      }
      DynMemory::Words(words) => {
        out.push((words.len() / stride) as u32);
        out.extend_from_slice(words);
      }
      DynMemory::Cells(_) => {
        unreachable!("Cells storage with a flat element type")
      }
    }
  } else if type_is_heap_value(element_type) {
    match memory {
      DynMemory::Cells(children) => {
        out.push(children.len() as u32);
        for child in children {
          serialize_cell(element_type, child.as_ref(), heap, out);
        }
      }
      DynMemory::Zeroed { elements } => {
        out.push(*elements);
        for _ in 0..*elements {
          serialize_cell(element_type, None, heap, out);
        }
      }
      DynMemory::Words(_) => {
        unreachable!("Words storage with a heap-value element type")
      }
    }
  } else {
    // elements EMBED heap ids in flat words
    let stride = vm_stack_size(element_type).max(1) as usize;
    match memory {
      DynMemory::Words(words) => {
        out.push((words.len() / stride) as u32);
        for chunk in words.chunks(stride) {
          serialize_flat_value(element_type, chunk, heap, out);
        }
      }
      DynMemory::Zeroed { elements } => {
        out.push(*elements);
        let zeros = vec![0u32; stride];
        for _ in 0..*elements {
          serialize_flat_value(element_type, &zeros, heap, out);
        }
      }
      DynMemory::Cells(_) => {
        unreachable!("Cells storage with an embedding element type")
      }
    }
  }
}

fn serialize_flat_value(
  t: &Type,
  words: &[u32],
  heap: &[Option<Arc<HeapCell>>],
  out: &mut Vec<u32>,
) {
  if !type_involves_heap(t) {
    out.extend_from_slice(words);
    return;
  }
  match t {
    Type::String | Type::Array(Some(ConcreteArraySize::Unsized), _) => {
      serialize_cell(
        t,
        heap_index(words[0]).and_then(|i| heap[i].as_ref()),
        heap,
        out,
      );
    }
    Type::Struct(s) => {
      let mut offset = 0usize;
      for field in s.fields.iter() {
        let field_type = field.field_type.unwrap_known();
        let size = vm_stack_size(&field_type) as usize;
        serialize_flat_value(
          &field_type,
          &words[offset..offset + size],
          heap,
          out,
        );
        offset += size;
      }
    }
    Type::Enum(e) => {
      let discriminant = words[0];
      out.push(discriminant);
      let variant = &e.variants[discriminant as usize];
      let inner_type = variant.inner_type.unwrap_known();
      if inner_type != Type::Unit {
        let size = vm_stack_size(&inner_type) as usize;
        serialize_flat_value(&inner_type, &words[1..1 + size], heap, out);
      }
    }
    Type::Array(Some(size), element_type) => {
      let count = size
        .as_literal()
        .expect("non-literal fixed-array size in shared value")
        as usize;
      let element_type = element_type.unwrap_known();
      let stride = vm_stack_size(&element_type) as usize;
      for i in 0..count {
        serialize_flat_value(
          &element_type,
          &words[i * stride..(i + 1) * stride],
          heap,
          out,
        );
      }
    }
    _ => panic!("unsupported type in shared wire encoding: {t:?}"),
  }
}

struct WireReader<'a> {
  words: &'a [u32],
  pos: usize,
}

impl<'a> WireReader<'a> {
  fn read(&mut self) -> u32 {
    let w = self.words[self.pos];
    self.pos += 1;
    w
  }
  fn read_slice(&mut self, n: usize) -> &'a [u32] {
    let s = &self.words[self.pos..self.pos + n];
    self.pos += n;
    s
  }
}

fn deserialize_cell(
  t: &Type,
  reader: &mut WireReader,
  heap: &mut Vec<Option<Arc<HeapCell>>>,
  heap_free: &mut Vec<u32>,
) -> Option<Arc<HeapCell>> {
  match t {
    Type::String => {
      let count = reader.read() as usize;
      (count > 0).then(|| {
        Arc::new(HeapCell {
          memory: DynMemory::Words(reader.read_slice(count).to_vec()),
          stride: 1,
        })
      })
    }
    Type::Array(Some(ConcreteArraySize::Unsized), element_type) => {
      let element_type = element_type.unwrap_known();
      let count = reader.read() as usize;
      if count == 0 {
        return None;
      }
      let stride = vm_stack_size(&element_type).max(1);
      let memory =
        deserialize_elements(&element_type, count, reader, heap, heap_free);
      Some(Arc::new(HeapCell { memory, stride }))
    }
    _ => panic!("deserialize_cell on a non-heap-value type"),
  }
}

fn deserialize_elements(
  element_type: &Type,
  count: usize,
  reader: &mut WireReader,
  heap: &mut Vec<Option<Arc<HeapCell>>>,
  heap_free: &mut Vec<u32>,
) -> DynMemory {
  if !type_involves_heap(element_type) {
    let stride = vm_stack_size(element_type).max(1) as usize;
    DynMemory::Words(reader.read_slice(count * stride).to_vec())
  } else if type_is_heap_value(element_type) {
    DynMemory::Cells(
      (0..count)
        .map(|_| deserialize_cell(element_type, reader, heap, heap_free))
        .collect(),
    )
  } else {
    let stride = vm_stack_size(element_type) as usize;
    let mut words = Vec::with_capacity(count * stride);
    for _ in 0..count {
      deserialize_flat_value(element_type, reader, &mut words, heap, heap_free);
    }
    DynMemory::Words(words)
  }
}

fn deserialize_flat_value(
  t: &Type,
  reader: &mut WireReader,
  out_words: &mut Vec<u32>,
  heap: &mut Vec<Option<Arc<HeapCell>>>,
  heap_free: &mut Vec<u32>,
) {
  if !type_involves_heap(t) {
    let size = vm_stack_size(t) as usize;
    out_words.extend_from_slice(reader.read_slice(size));
    return;
  }
  match t {
    Type::String | Type::Array(Some(ConcreteArraySize::Unsized), _) => {
      let id = match deserialize_cell(t, reader, heap, heap_free) {
        Some(cell) => alloc_heap_cell(heap, heap_free, cell),
        None => 0,
      };
      out_words.push(id);
    }
    Type::Struct(s) => {
      for field in s.fields.iter() {
        let field_type = field.field_type.unwrap_known();
        deserialize_flat_value(&field_type, reader, out_words, heap, heap_free);
      }
    }
    Type::Enum(e) => {
      let total = vm_stack_size(t) as usize;
      let start = out_words.len();
      let discriminant = reader.read();
      out_words.push(discriminant);
      let variant = &e.variants[discriminant as usize];
      let inner_type = variant.inner_type.unwrap_known();
      if inner_type != Type::Unit {
        deserialize_flat_value(&inner_type, reader, out_words, heap, heap_free);
      }
      // pad to the enum's full flat slot layout
      out_words.resize(start + total, 0);
    }
    Type::Array(Some(size), element_type) => {
      let count = size
        .as_literal()
        .expect("non-literal fixed-array size in shared value")
        as usize;
      let element_type = element_type.unwrap_known();
      for _ in 0..count {
        deserialize_flat_value(
          &element_type,
          reader,
          out_words,
          heap,
          heap_free,
        );
      }
    }
    _ => panic!("unsupported type in shared wire encoding: {t:?}"),
  }
}

/// Releases every heap id embedded in an embedding-element region's flat
/// words — adoption replaces the region wholesale, and the region owns
/// its ids (`Cells` regions release children via `Drop` instead).
fn release_embedded_region_ids(
  element_type: &Type,
  memory: &DynMemory,
  heap: &mut Vec<Option<Arc<HeapCell>>>,
  heap_free: &mut Vec<u32>,
) {
  let DynMemory::Words(words) = memory else {
    return;
  };
  let stride = vm_stack_size(element_type).max(1) as usize;
  for chunk in words.chunks(stride) {
    release_flat_value_ids(element_type, chunk, heap, heap_free);
  }
}

fn release_flat_value_ids(
  t: &Type,
  words: &[u32],
  heap: &mut Vec<Option<Arc<HeapCell>>>,
  heap_free: &mut Vec<u32>,
) {
  if !type_involves_heap(t) {
    return;
  }
  match t {
    Type::String | Type::Array(Some(ConcreteArraySize::Unsized), _) => {
      release_heap_id(heap, heap_free, words[0]);
    }
    Type::Struct(s) => {
      let mut offset = 0usize;
      for field in s.fields.iter() {
        let field_type = field.field_type.unwrap_known();
        let size = vm_stack_size(&field_type) as usize;
        release_flat_value_ids(
          &field_type,
          &words[offset..offset + size],
          heap,
          heap_free,
        );
        offset += size;
      }
    }
    Type::Enum(e) => {
      let discriminant = words[0] as usize;
      if let Some(variant) = e.variants.get(discriminant) {
        let inner_type = variant.inner_type.unwrap_known();
        if inner_type != Type::Unit {
          let size = vm_stack_size(&inner_type) as usize;
          release_flat_value_ids(
            &inner_type,
            &words[1..1 + size],
            heap,
            heap_free,
          );
        }
      }
    }
    Type::Array(Some(size), element_type) => {
      if let Some(count) = size.as_literal() {
        let element_type = element_type.unwrap_known();
        let stride = vm_stack_size(&element_type) as usize;
        for i in 0..count as usize {
          release_flat_value_ids(
            &element_type,
            &words[i * stride..(i + 1) * stride],
            heap,
            heap_free,
          );
        }
      }
    }
    _ => {}
  }
}
