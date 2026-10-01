//! Audio output on the web, page side. `start-audio` runs its audio
//! function on the browser's real-time audio thread, in an AudioWorklet
//! (`templates/easl-audio-worklet.js`) holding its own instance of this
//! runtime (see `audio_engine`). The two instances share no memory, so the
//! page sends the worklet its compiled audio program, serialized
//! (`Code::to_bytes`): both sides then hold the same program, with the same
//! shared-variable indices, exactly as a native audio thread does.
//!
//! Thread-shared globals follow easl's boundary-batched model, over
//! messages instead of shared memory: each side keeps its own
//! `ThreadSharedTable`, and every snapshot a side publishes at its boundary
//! (a frame here, a render quantum there) is posted to the other side and
//! installed in its table, to be adopted at that side's next boundary.

use std::{cell::RefCell, sync::Arc};

use easl::{audio::AudioSource, thread_sync::ThreadSharedTable};
use js_sys::{Array, Object, Reflect, Uint8Array, Uint32Array};
use wasm_bindgen::{JsCast, prelude::*};
use wasm_bindgen_futures::{JsFuture, spawn_local};
use web_sys::{
  AudioContext, AudioContextState, AudioWorkletNode, AudioWorkletNodeOptions,
  MessageEvent, MessagePort,
};

/// Name the worklet script registers its processor under.
const PROCESSOR_NAME: &str = "easl-audio";

/// What the page provides for audio: the compiled wasm module (the worklet
/// instantiates its own copy) and the worklet script's URL.
pub struct AudioHost {
  pub module: JsValue,
  pub worklet_url: String,
}

/// The running audio output.
struct Output {
  context: AudioContext,
  /// The worklet's message port, once the worklet has loaded.
  port: Option<MessagePort>,
  /// Messages posted before the worklet loaded, sent when it does.
  queued: Vec<JsValue>,
  /// This side's shared-variable table and the variables' names, in table
  /// order; set by the first `start-audio`.
  table: Option<Arc<ThreadSharedTable>>,
  shared_names: Vec<Arc<str>>,
  entry: Option<String>,
}

thread_local! {
  static HOST: RefCell<Option<AudioHost>> = const { RefCell::new(None) };
  static OUTPUT: RefCell<Option<Output>> = const { RefCell::new(None) };
}

/// Prepares audio for a program with audio entry points: creates the audio
/// context up front, so `(sample-rate)` reports the rate audio will run at
/// even before `start-audio` (closure constructors size buffers with it).
pub fn configure(host: AudioHost) -> Result<(), JsValue> {
  let context = AudioContext::new()?;
  HOST.with(|h| *h.borrow_mut() = Some(host));
  OUTPUT.with(|o| {
    *o.borrow_mut() = Some(Output {
      context,
      port: None,
      queued: vec![],
      table: None,
      shared_names: vec![],
      entry: None,
    })
  });
  Ok(())
}

/// The audio context's sample rate, once `configure` has made one.
pub fn sample_rate() -> Option<f32> {
  OUTPUT.with(|o| o.borrow().as_ref().map(|o| o.context.sample_rate()))
}

/// Handles a `start-audio` call. The first carries the audio program (its
/// table already bootstrapped by the call): the worklet is created and told
/// to start. Later calls switch the running entry when it differs.
pub fn start(entry: &str, source: Option<AudioSource>) -> Result<(), String> {
  let first = OUTPUT.with(|o| {
    let mut o = o.borrow_mut();
    let Some(output) = o.as_mut() else {
      return Err("start-audio called without audio configured".to_string());
    };
    Ok(output.entry.is_none())
  })?;
  if !first {
    let changed = OUTPUT.with(|o| {
      let mut o = o.borrow_mut();
      let output = o.as_mut().unwrap();
      let changed = output.entry.as_deref() != Some(entry);
      output.entry = Some(entry.to_string());
      changed
    });
    if changed {
      post(message(&[
        ("type", "switch".into()),
        ("entry", entry.into()),
      ]));
    }
    return Ok(());
  }
  let Some(AudioSource::Bytecode {
    program,
    function_names,
    shared_table,
  }) = source
  else {
    return Err("the web runtime only supports bytecode audio".to_string());
  };
  let table =
    shared_table.expect("start-audio attaches the shared table to its source");
  let shared_names: Vec<Arc<str>> = program
    .code
    .shared_vars
    .iter()
    .map(|info| info.name.clone())
    .collect();
  OUTPUT.with(|o| {
    let mut o = o.borrow_mut();
    let output = o.as_mut().unwrap();
    output.table = Some(table.clone());
    output.shared_names = shared_names;
    output.entry = Some(entry.to_string());
  });
  // Everything published so far (the start-audio bootstrap included),
  // before the start message, so the first render adopts it.
  for index in 0..table.slots.len() {
    if table.slots[index].has_published() {
      post(snapshot_message(
        index as u32,
        &snapshot_words(&table, index),
      ));
    }
  }
  post(message(&[
    ("type", "start".into()),
    ("entry", entry.into()),
  ]));
  let program = Uint8Array::from(&program.code.to_bytes()[..]);
  let function_names = names_array(&function_names);
  spawn_local(async move {
    if let Err(error) = create_worklet(program, function_names).await {
      web_sys::console::error_2(&"easl: couldn't start audio:".into(), &error);
    }
  });
  resume_on_gesture();
  Ok(())
}

/// Forwards a shared variable this side just published to the worklet.
pub fn forward_publish(name: &Arc<str>) {
  let snapshot = OUTPUT.with(|o| {
    let o = o.borrow();
    let output = o.as_ref()?;
    let table = output.table.as_ref()?;
    let index = output.shared_names.iter().position(|n| n == name)?;
    Some((index, snapshot_words(table, index)))
  });
  if let Some((index, words)) = snapshot {
    post(snapshot_message(index as u32, &words));
  }
}

/// Forwards a MIDI message to the worklet, whose instance keeps its own
/// MIDI state for the audio thread's per-batch refresh.
pub fn forward_midi(bytes: &[u8]) {
  let running =
    OUTPUT.with(|o| o.borrow().as_ref().is_some_and(|o| o.entry.is_some()));
  if running {
    post(message(&[
      ("type", "midi".into()),
      ("bytes", Uint8Array::from(bytes).into()),
    ]));
  }
}

fn snapshot_words(table: &ThreadSharedTable, index: usize) -> Vec<u32> {
  table.slots[index]
    .adopt_if_newer(0)
    .map(|snapshot| snapshot.words.clone())
    .unwrap_or_default()
}

fn snapshot_message(index: u32, words: &[u32]) -> JsValue {
  message(&[
    ("type", "snapshot".into()),
    ("index", index.into()),
    ("words", Uint32Array::from(words).into()),
  ])
}

fn names_array(names: &[Arc<str>]) -> Array {
  names.iter().map(|name| JsValue::from_str(name)).collect()
}

fn message(fields: &[(&str, JsValue)]) -> JsValue {
  let object = Object::new();
  for (key, value) in fields {
    Reflect::set(&object, &(*key).into(), value).unwrap();
  }
  object.into()
}

/// Posts to the worklet, or queues until it has loaded.
fn post(message: JsValue) {
  OUTPUT.with(|o| {
    let mut o = o.borrow_mut();
    let Some(output) = o.as_mut() else { return };
    match &output.port {
      Some(port) => {
        if let Err(error) = port.post_message(&message) {
          web_sys::console::error_2(
            &"easl: audio message failed:".into(),
            &error,
          );
        }
      }
      None => output.queued.push(message),
    }
  });
}

/// Loads the worklet script, creates the processor node, and connects it.
/// Creates the worklet, running the serialized audio `program`.
async fn create_worklet(
  program: Uint8Array,
  function_names: Array,
) -> Result<(), JsValue> {
  let context = OUTPUT
    .with(|o| o.borrow().as_ref().map(|o| o.context.clone()))
    .ok_or("audio isn't configured")?;
  let (worklet_url, processor_options) = HOST.with(|h| {
    let h = h.borrow();
    let host = h.as_ref().expect("audio host configured");
    (
      host.worklet_url.clone(),
      message(&[
        ("module", host.module.clone()),
        ("program", program.into()),
        ("functionNames", function_names.into()),
      ]),
    )
  });
  JsFuture::from(context.audio_worklet()?.add_module(&worklet_url)?).await?;
  let options = AudioWorkletNodeOptions::new();
  options.set_processor_options(Some(&processor_options.unchecked_into()));
  let node =
    AudioWorkletNode::new_with_options(&context, PROCESSOR_NAME, &options)?;
  node.connect_with_audio_node(&context.destination())?;
  let port = node.port()?;
  let on_message = Closure::<dyn FnMut(MessageEvent)>::new(receive);
  port.set_onmessage(Some(on_message.as_ref().unchecked_ref()));
  on_message.forget();
  let queued = OUTPUT.with(|o| {
    let mut o = o.borrow_mut();
    let output = o.as_mut().unwrap();
    output.port = Some(port.clone());
    std::mem::take(&mut output.queued)
  });
  for message in queued {
    port.post_message(&message)?;
  }
  // The node must stay alive for as long as the page plays audio.
  std::mem::forget(node);
  Ok(())
}

/// Handles a message from the worklet: a snapshot it published (installed
/// in this side's table, for the next frame to adopt), or an error.
fn receive(event: MessageEvent) {
  let data = event.data();
  let kind = Reflect::get(&data, &"type".into())
    .ok()
    .and_then(|kind| kind.as_string());
  match kind.as_deref() {
    Some("snapshot") => {
      let index = Reflect::get(&data, &"index".into())
        .ok()
        .and_then(|index| index.as_f64())
        .unwrap_or(-1.) as usize;
      let words = Reflect::get(&data, &"words".into())
        .map(|words| Uint32Array::new(&words).to_vec())
        .unwrap_or_default();
      OUTPUT.with(|o| {
        if let Some(table) = o.borrow().as_ref().and_then(|o| o.table.clone())
          && index < table.slots.len()
        {
          table.slots[index].publish(words);
        }
      });
    }
    Some("error") => {
      let text = Reflect::get(&data, &"message".into()).unwrap_or_default();
      web_sys::console::error_2(&"easl audio:".into(), &text);
    }
    _ => {}
  }
}

/// Browsers start audio contexts suspended until the page gets a user
/// gesture; resume on the first one.
fn resume_on_gesture() {
  let Some(context) =
    OUTPUT.with(|o| o.borrow().as_ref().map(|o| o.context.clone()))
  else {
    return;
  };
  if context.state() != AudioContextState::Suspended {
    return;
  }
  web_sys::console::info_1(
    &"easl: audio starts after the first click or key press on the page".into(),
  );
  let window = web_sys::window().unwrap();
  let resume = Closure::<dyn FnMut()>::new(move || {
    let _ = context.resume();
  });
  for event in ["pointerdown", "keydown"] {
    let _ = window
      .add_event_listener_with_callback(event, resume.as_ref().unchecked_ref());
  }
  resume.forget();
}
