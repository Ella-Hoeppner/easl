//! MIDI input on the web. The page's Web MIDI listener (or its own code,
//! through the runtime's `sendMidiMessage`) feeds raw messages to
//! `handle_message`; the per-frame and per-batch refreshes read the merged
//! state through `current_midi_state`, as they read the native listener's.
//! Each wasm instance (the page's, and the audio worklet's) keeps its own
//! state, so the page forwards every message to the worklet too.
//!
//! MIDI output on the web goes through the page's IO manager and the audio
//! worklet's engine, which send to Web MIDI themselves; these functions are
//! the native output's counterparts, and send nothing.

use std::sync::{Arc, LazyLock, Mutex};

use crate::interpreter::MidiState;

static STATE: LazyLock<Mutex<Arc<MidiState>>> =
  LazyLock::new(|| Mutex::new(Arc::new(MidiState::default())));

/// The current merged MIDI input state.
pub fn current_midi_state() -> Arc<MidiState> {
  STATE.lock().unwrap().clone()
}

/// Applies one raw MIDI message (status byte first) to the merged state.
pub fn handle_message(message: &[u8]) {
  let mut state = STATE.lock().unwrap();
  let mut next = (**state).clone();
  if next.apply_message(message) {
    *state = Arc::new(next);
  }
}

/// MIDI output's counterpart of the native `start_output`.
pub fn start_output() {}

/// MIDI output's counterpart of the native `send_midi`: on the web, the page
/// and the worklet send messages themselves.
pub fn send_midi(_message: crate::midi_out::MidiMessage) {}

/// MIDI output's counterpart of the native `send_midi_bytes`.
pub fn send_midi_bytes(_bytes: &[u8]) {}

/// The native output's port list; empty on the web.
pub fn output_port_names() -> Vec<String> {
  vec![]
}

/// The native output's port selection; a no-op on the web.
pub fn select_output(_name: &str) -> Result<(), String> {
  Ok(())
}
