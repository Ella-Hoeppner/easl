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
//!
//! MIDI output goes through one process-global sender, started on first
//! use: every thread hands messages to it through a bounded channel whose
//! `try_send` never blocks or allocates (the audio thread sends at the end
//! of each callback batch), and the sender thread polls the channel and
//! writes each message to the selected output ports — every port, until
//! `select-midi-output` picks some.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{Receiver, SyncSender, TrySendError, sync_channel};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::Duration;

use arc_swap::ArcSwap;
use midir::{
  Ignore, MidiInput, MidiInputConnection, MidiOutput, MidiOutputConnection,
};

use crate::interpreter::MidiState;
use crate::midi_out::MidiMessage;

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
    let mut master = self.master.lock().unwrap();
    if master.apply_message(message) {
      self.published.store(Arc::new(master.clone()));
    }
  }
}

/// Messages waiting for the sender thread; past this many, sends are
/// dropped (only possible if the sender thread stalls).
const OUTPUT_QUEUE_CAPACITY: usize = 4096;

/// How long the sender thread sleeps when the queue is empty: the most a
/// message waits before it's sent.
const OUTPUT_POLL_INTERVAL: Duration = Duration::from_micros(500);

static OUTPUT: OnceLock<MidiOutputs> = OnceLock::new();

/// A message waiting for the sender thread.
enum Outgoing {
  /// A short message, as the audio thread sends them (no allocation).
  Message(MidiMessage),
  /// Any message, as the main thread sends them (a system-exclusive
  /// message can be any length).
  Bytes(Box<[u8]>),
}

impl Outgoing {
  fn as_bytes(&self) -> &[u8] {
    match self {
      Outgoing::Message(message) => message.as_bytes(),
      Outgoing::Bytes(bytes) => bytes,
    }
  }
}

struct MidiOutputs {
  queue: SyncSender<Outgoing>,
  /// The open connections, with their port names.
  connections: Arc<Mutex<Vec<(String, MidiOutputConnection)>>>,
  /// Whether a full queue has been reported (once per process).
  reported_full: AtomicBool,
}

fn outputs() -> &'static MidiOutputs {
  OUTPUT.get_or_init(|| {
    let (queue, messages) = sync_channel(OUTPUT_QUEUE_CAPACITY);
    let connections = Arc::new(Mutex::new(connect_outputs(|_| true)));
    let for_thread = connections.clone();
    std::thread::spawn(move || send_queued_messages(messages, for_thread));
    MidiOutputs {
      queue,
      connections,
      reported_full: AtomicBool::new(false),
    }
  })
}

/// Opens a connection to every output port whose name `select` accepts.
fn connect_outputs(
  select: impl Fn(&str) -> bool,
) -> Vec<(String, MidiOutputConnection)> {
  let Ok(enumerator) = MidiOutput::new("easl") else {
    eprintln!("easl: MIDI output unavailable");
    return vec![];
  };
  let mut connections = vec![];
  for port in enumerator.ports() {
    let Ok(name) = enumerator.port_name(&port) else {
      continue;
    };
    if !select(&name) {
      continue;
    }
    let Ok(output) = MidiOutput::new("easl") else {
      continue;
    };
    match output.connect(&port, "easl-midi-out") {
      Ok(connection) => connections.push((name, connection)),
      Err(e) => eprintln!("easl: couldn't open MIDI output \"{name}\": {e}"),
    }
  }
  connections
}

fn send_queued_messages(
  messages: Receiver<Outgoing>,
  connections: Arc<Mutex<Vec<(String, MidiOutputConnection)>>>,
) {
  loop {
    // `try_recv`, not a blocking receive: a receiver blocked on the channel
    // makes every send wake it, which takes a lock on the sending thread.
    while let Ok(message) = messages.try_recv() {
      for (name, connection) in connections.lock().unwrap().iter_mut() {
        if let Err(e) = connection.send(message.as_bytes()) {
          eprintln!("easl: couldn't send MIDI to \"{name}\": {e}");
        }
      }
    }
    std::thread::sleep(OUTPUT_POLL_INTERVAL);
  }
}

/// Starts the MIDI sender (connecting to every output port), if it hasn't
/// started yet.
pub fn start_output() {
  outputs();
}

/// Sends a MIDI message to the selected output ports. Never blocks or
/// allocates, so the audio thread can call it.
pub fn send_midi(message: MidiMessage) {
  enqueue(Outgoing::Message(message));
}

/// Sends a MIDI message of any length (status first) to the selected
/// output ports, after every message sent before it.
pub fn send_midi_bytes(bytes: &[u8]) {
  enqueue(Outgoing::Bytes(bytes.into()));
}

fn enqueue(message: Outgoing) {
  let outputs = outputs();
  if let Err(TrySendError::Full(_)) = outputs.queue.try_send(message)
    && !outputs.reported_full.swap(true, Ordering::Relaxed)
  {
    eprintln!("easl: MIDI output queue full, dropping messages");
  }
}

/// The names of the available MIDI output ports, sorted.
pub fn output_port_names() -> Vec<String> {
  let Ok(enumerator) = MidiOutput::new("easl") else {
    return vec![];
  };
  let mut names: Vec<String> = enumerator
    .ports()
    .iter()
    .filter_map(|port| enumerator.port_name(port).ok())
    .collect();
  names.sort();
  names
}

/// Sends later messages only to the output ports whose names contain
/// `name`, failing (and keeping the current ports) when none does.
pub fn select_output(name: &str) -> Result<(), String> {
  let selected = connect_outputs(|port| port.contains(name));
  if selected.is_empty() {
    let available = output_port_names();
    return Err(format!(
      "no MIDI output port's name contains \"{name}\" (available: {})",
      if available.is_empty() {
        "none".to_string()
      } else {
        available.join(", ")
      }
    ));
  }
  *outputs().connections.lock().unwrap() = selected;
  Ok(())
}
