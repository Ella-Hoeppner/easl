//! MIDI input runtime — backs the `down-midi-notes` / `midi-cc` /
//! `midi-aftertouch` / `midi-pitch-bend` builtins.
//!
//! A single process-global listener connects to every available MIDI input
//! port on first use (a program that never queries MIDI never starts it)
//! and maintains an immutable published snapshot of the merged input
//! state. Consumers — the per-frame refresh on the main thread and the
//! per-batch refresh on the audio thread — read the snapshot lock-free
//! via `arc_swap`, so the real-time audio callback never blocks on the
//! MIDI callback thread. Snapshots carry a generation counter, letting
//! refresh sites skip all work (and all allocation) while no MIDI events
//! have arrived.
//!
//! All ports and channels are merged into one state. Ports are
//! enumerated once at listener startup; devices plugged in later aren't
//! picked up until the next run.

use std::sync::{Arc, Mutex, OnceLock};

use arc_swap::ArcSwap;
use midir::{Ignore, MidiInput, MidiInputConnection};

use crate::interpreter::{MidiNoteState, MidiState};

static LISTENER: OnceLock<Arc<MidiListener>> = OnceLock::new();

struct MidiListener {
  /// The authoritative state, mutated only by MIDI callbacks (rarely, and
  /// never on a real-time thread).
  master: Mutex<MidiState>,
  /// The published snapshot readers load lock-free.
  published: ArcSwap<MidiState>,
}

/// The current merged MIDI input state. Starts the process-global
/// listener on first call; returns silence when no device or port is
/// available. The returned `Arc` is a lock-free load — allocation-free
/// for the caller, safe from the audio callback.
pub fn current_midi_state() -> Arc<MidiState> {
  listener().published.load_full()
}

fn listener() -> &'static Arc<MidiListener> {
  LISTENER.get_or_init(|| {
    let listener = Arc::new(MidiListener {
      master: Mutex::new(MidiState::default()),
      published: ArcSwap::from_pointee(MidiState::default()),
    });
    let for_thread = listener.clone();
    // The connections are owned by a dedicated parked thread: midir
    // callbacks run on the OS MIDI thread, and keeping the connection
    // handles alive somewhere `'static` without a global teardown story
    // is exactly what a parked thread is for.
    std::thread::spawn(move || own_connections(for_thread));
    listener
  })
}

fn own_connections(listener: Arc<MidiListener>) {
  let mut enumerator = match MidiInput::new("easl") {
    Ok(input) => input,
    Err(e) => {
      eprintln!("easl: MIDI unavailable: {e}");
      return;
    }
  };
  enumerator.ignore(Ignore::None);
  let mut connections: Vec<MidiInputConnection<()>> = vec![];
  for port in enumerator.ports() {
    let Ok(mut input) = MidiInput::new("easl") else {
      continue;
    };
    input.ignore(Ignore::None);
    let port_name = input
      .port_name(&port)
      .unwrap_or_else(|_| "unknown".to_string());
    let for_callback = listener.clone();
    match input.connect(
      &port,
      "easl-midi-in",
      move |_, message, _| for_callback.handle_message(message),
      (),
    ) {
      Ok(connection) => {
        eprintln!("easl: listening for MIDI input on \"{port_name}\"");
        connections.push(connection);
      }
      Err(e) => {
        eprintln!("easl: couldn't open MIDI port \"{port_name}\": {e}");
      }
    }
  }
  if connections.is_empty() {
    return;
  }
  loop {
    std::thread::park();
  }
}

impl MidiListener {
  fn handle_message(&self, message: &[u8]) {
    if message.is_empty() {
      return;
    }
    let mut master = self.master.lock().unwrap();
    // Channels are merged: only the message kind nibble matters.
    match message[0] & 0xF0 {
      0x90 | 0x80 if message.len() >= 3 => {
        let note = (message[1] & 0x7F) as u32;
        let velocity = (message[2] & 0x7F) as f32 / 127.;
        // Note-on with velocity 0 is a note-off by convention.
        let is_on = message[0] & 0xF0 == 0x90 && velocity > 0.;
        if is_on {
          if let Some(existing) =
            master.down_notes.iter_mut().find(|n| n.note == note)
          {
            // A re-strike restarts the note: fresh velocity, and the
            // pressure envelope starts over.
            existing.velocity = velocity;
            existing.aftertouch = 0.;
          } else {
            master.down_notes.push(MidiNoteState {
              note,
              velocity,
              aftertouch: 0.,
            });
          }
        } else {
          master.down_notes.retain(|n| n.note != note);
        }
      }
      0xB0 if message.len() >= 3 => {
        master.cc[(message[1] & 0x7F) as usize] =
          (message[2] & 0x7F) as f32 / 127.;
      }
      // Polyphonic key pressure (per-note aftertouch); pressure on a
      // note that isn't held is meaningless, so it's dropped.
      0xA0 if message.len() >= 3 => {
        let note = (message[1] & 0x7F) as u32;
        let Some(held) = master.down_notes.iter_mut().find(|n| n.note == note)
        else {
          return;
        };
        held.aftertouch = (message[2] & 0x7F) as f32 / 127.;
      }
      // Channel pressure (whole-keyboard aftertouch); one data byte.
      0xD0 if message.len() >= 2 => {
        master.channel_aftertouch = (message[1] & 0x7F) as f32 / 127.;
      }
      0xE0 if message.len() >= 3 => {
        let raw =
          ((message[2] & 0x7F) as i32) << 7 | (message[1] & 0x7F) as i32;
        master.pitch_bend = (raw - 8192) as f32 / 8192.;
      }
      _ => return,
    }
    master.generation = master.generation.wrapping_add(1);
    self.published.store(Arc::new(master.clone()));
  }
}
