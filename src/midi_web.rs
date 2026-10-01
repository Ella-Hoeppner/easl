//! MIDI input on the web. The page's Web MIDI listener (or its own code,
//! through the runtime's `sendMidiMessage`) feeds raw messages to
//! `handle_message`; the per-frame and per-batch refreshes read the merged
//! state through `current_midi_state`, as they read the native listener's.
//! Each wasm instance (the page's, and the audio worklet's) keeps its own
//! state, so the page forwards every message to the worklet too.

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
