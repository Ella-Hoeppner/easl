//! MIDI input on the web: not yet supported, so every query reads silence.

use std::sync::{Arc, LazyLock};

use crate::interpreter::MidiState;

static SILENCE: LazyLock<Arc<MidiState>> =
  LazyLock::new(|| Arc::new(MidiState::default()));

/// The current MIDI input state: always silence on the web.
pub fn current_midi_state() -> Arc<MidiState> {
  SILENCE.clone()
}
