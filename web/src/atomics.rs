//! Shared atomics on the web. The page and its audio worklet each run their
//! own instance of this runtime, sharing no wasm memory, so the words of an
//! atomic shared between them live in a `SharedArrayBuffer`, which the page
//! makes (`WebIO::shared_atomic_words`) and hands the worklet, and both
//! sides operate on it through JS `Atomics`. Browsers only allow
//! `SharedArrayBuffer` on cross-origin isolated pages.

use std::sync::Arc;

use easl::thread_sync::AtomicWords;
use js_sys::{Atomics, Int32Array, Reflect, SharedArrayBuffer};
use wasm_bindgen::JsValue;

/// The headers that make a page cross-origin isolated, as the error for a
/// page without them names them.
pub const ISOLATION_HEADERS: &str = "Cross-Origin-Opener-Policy: same-origin \
  and Cross-Origin-Embedder-Policy: require-corp";

/// Whether this page (or worklet) may use `SharedArrayBuffer`.
pub fn cross_origin_isolated() -> bool {
  Reflect::get(&js_sys::global(), &"crossOriginIsolated".into())
    .is_ok_and(|isolated| isolated.is_truthy())
}

/// Atomic words in a `SharedArrayBuffer`.
pub struct SharedArrayWords {
  buffer: SharedArrayBuffer,
  array: Int32Array,
  len: usize,
}

// A wasm instance has one thread, and the words are reached only through
// `Atomics`, which other threads holding the same buffer synchronize with.
unsafe impl Send for SharedArrayWords {}
unsafe impl Sync for SharedArrayWords {}

impl SharedArrayWords {
  /// `words` zeroed words in a new buffer.
  pub fn zeroed(words: usize) -> Self {
    Self::over(SharedArrayBuffer::new((words * 4) as u32))
  }

  /// The words of `buffer`, which another instance made.
  pub fn over(buffer: SharedArrayBuffer) -> Self {
    let array = Int32Array::new(&buffer);
    let len = array.length() as usize;
    Self { buffer, array, len }
  }

  pub fn buffer(&self) -> &SharedArrayBuffer {
    &self.buffer
  }

  pub fn into_words(self) -> Arc<dyn AtomicWords> {
    Arc::new(self)
  }
}

/// The result of an `Atomics` call on an in-bounds index of an `Int32Array`,
/// which can't fail.
fn word(result: Result<i32, JsValue>) -> u32 {
  result.expect("Atomics on a shared Int32Array") as u32
}

impl AtomicWords for SharedArrayWords {
  fn len(&self) -> usize {
    self.len
  }
  fn load(&self, index: usize) -> u32 {
    word(Atomics::load(&self.array, index as u32))
  }
  fn store(&self, index: usize, value: u32) {
    word(Atomics::store(&self.array, index as u32, value as i32));
  }
  fn swap(&self, index: usize, value: u32) -> u32 {
    word(Atomics::exchange(&self.array, index as u32, value as i32))
  }
  fn fetch_add(&self, index: usize, value: u32) -> u32 {
    word(Atomics::add(&self.array, index as u32, value as i32))
  }
  fn fetch_sub(&self, index: usize, value: u32) -> u32 {
    word(Atomics::sub(&self.array, index as u32, value as i32))
  }
  fn fetch_and(&self, index: usize, value: u32) -> u32 {
    word(Atomics::and(&self.array, index as u32, value as i32))
  }
  fn fetch_or(&self, index: usize, value: u32) -> u32 {
    word(Atomics::or(&self.array, index as u32, value as i32))
  }
  fn fetch_xor(&self, index: usize, value: u32) -> u32 {
    word(Atomics::xor(&self.array, index as u32, value as i32))
  }
  fn compare_exchange(&self, index: usize, current: u32, new: u32) -> u32 {
    word(Atomics::compare_exchange(
      &self.array,
      index as u32,
      current as i32,
      new as i32,
    ))
  }
}
