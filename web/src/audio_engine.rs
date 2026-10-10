//! Audio output on the web, worklet side: the runtime instance inside the
//! AudioWorklet (`templates/easl-audio-worklet.js`), running a program's
//! audio function on the browser's real-time audio thread. See `audio` for
//! the page side and the protocol between them.

use std::sync::Arc;

use easl::{
  audio::VmAudioDriver,
  thread_sync::{ThreadSharedTable, participant},
  vm::bytecode::{BytecodeProgram, Code},
};
use js_sys::SharedArrayBuffer;
use wasm_bindgen::prelude::*;

use crate::atomics::SharedArrayWords;

/// One program's audio thread: its bytecode replica, running the entry the
/// page started, and its side of the shared-variable table.
#[wasm_bindgen]
pub struct AudioEngine {
  /// The audio program, until `start` hands it to the driver.
  program: Option<(BytecodeProgram, Vec<Arc<str>>)>,
  driver: Option<VmAudioDriver>,
  table: Arc<ThreadSharedTable>,
  /// Shared variables the last `render` published.
  published: Vec<u32>,
}

#[wasm_bindgen]
impl AudioEngine {
  /// Loads the page's audio program: its serialized code
  /// (`Code::to_bytes`), its functions' names, and the buffers holding its
  /// shared atomics' words (`atomic_buffers[i]` for the shared variable at
  /// `atomic_indices[i]`).
  #[wasm_bindgen(constructor)]
  pub fn new(
    program: &[u8],
    function_names: Vec<String>,
    atomic_indices: Vec<u32>,
    atomic_buffers: Vec<SharedArrayBuffer>,
  ) -> Result<AudioEngine, JsError> {
    console_error_panic_hook::set_once();
    let program = BytecodeProgram::from_code(
      Code::from_bytes(program).map_err(|e| JsError::new(&e))?,
    );
    let table =
      Arc::new(ThreadSharedTable::new(program.code.shared_vars.len()));
    table.join(participant::AUDIO);
    for (index, buffer) in atomic_indices.into_iter().zip(atomic_buffers) {
      let index = index as usize;
      let words = program.code.shared_vars[index].layout.words() as usize;
      table.slots[index]
        .atomic_words(words, || SharedArrayWords::over(buffer).into_words());
    }
    let function_names = function_names.into_iter().map(Arc::from).collect();
    Ok(AudioEngine {
      program: Some((program, function_names)),
      driver: None,
      table,
      published: vec![],
    })
  }

  /// Starts playing `entry`.
  pub fn start(&mut self, entry: &str) -> Result<(), JsError> {
    let (program, names) = self
      .program
      .take()
      .ok_or_else(|| JsError::new("audio already started"))?;
    self.driver = Some(
      VmAudioDriver::new(entry, program, &names, Some(self.table.clone()))
        .map_err(|e| JsError::new(&e))?,
    );
    Ok(())
  }

  /// Switches the running audio function, keeping all program state.
  #[wasm_bindgen(js_name = switchEntry)]
  pub fn switch_entry(&mut self, entry: &str) -> Result<(), JsError> {
    match &mut self.driver {
      Some(driver) => driver.switch_entry(entry).map_err(|e| JsError::new(&e)),
      None => Err(JsError::new("audio hasn't started")),
    }
  }

  /// Installs a snapshot the page published, for the next render to adopt.
  #[wasm_bindgen(js_name = installSnapshot)]
  pub fn install_snapshot(&self, index: u32, words: Vec<u32>) {
    if let Some(slot) = self.table.slots.get(index as usize) {
      slot.publish(words);
    }
  }

  /// Applies a MIDI message to this instance's MIDI state.
  #[wasm_bindgen(js_name = midiMessage)]
  pub fn midi_message(&self, bytes: &[u8]) {
    easl::midi::handle_message(bytes);
  }

  /// Renders one batch of samples into `output`, returning the indices of
  /// the shared variables the batch published (to post to the page).
  pub fn render(&mut self, output: &mut [f32], sample_rate: f32) -> Vec<u32> {
    self.published.clear();
    let Some(driver) = &mut self.driver else {
      output.fill(0.);
      return vec![];
    };
    let published = &mut self.published;
    let mut samples = output.iter_mut();
    driver.run_batch(
      samples.len(),
      sample_rate,
      |sample| {
        if let Some(slot) = samples.next() {
          *slot = sample;
        }
      },
      |_| {},
      |index| published.push(index as u32),
    );
    self.published.clone()
  }

  /// The words of shared variable `index`'s current snapshot.
  pub fn snapshot(&self, index: u32) -> Vec<u32> {
    self
      .table
      .slots
      .get(index as usize)
      .and_then(|slot| slot.adopt_if_newer(0))
      .map(|snapshot| snapshot.words.clone())
      .unwrap_or_default()
  }
}
