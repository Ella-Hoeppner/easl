//! The easl web runtime, compiled to WebAssembly: it runs an easl
//! program's `@cpu` entry on the bytecode VM, rendering into a canvas
//! through WebGPU. Frames come from `requestAnimationFrame` rather than a
//! native window loop.
//!
//! The default build runs programs `easl compile --web` precompiled (see
//! `easl::precompiled`), so it holds no compiler. The `compiler` feature
//! builds a runtime that can also compile programs from their sources in
//! the browser, for pages that change them as they run (a playground); its
//! errors describe where in the sources they happened.

mod atomics;
mod audio;
mod audio_engine;
mod io;

use std::{
  cell::{Cell, RefCell},
  collections::HashMap,
  fmt::Display,
  future::Future,
  rc::Rc,
  sync::{Arc, RwLock},
};

#[cfg(feature = "compiler")]
use std::path::{Path, PathBuf};

use audio::AudioHost;
#[cfg(feature = "compiler")]
use easl::{CompilerTarget, load_easl_program_from_sources};
use easl::{
  interpreter::{EvalError, IOManager, VmCpuRuntime, VmRunState},
  precompiled::PrecompiledProgram,
  window::{
    BufferRead, GpuCore, SCROLL_LINE_HEIGHT, ScreenTarget,
    install_gpu_error_handler,
  },
};
use io::WebIO;
use js_sys::{Function, Promise, Reflect};
use wasm_bindgen::{JsCast, prelude::*};
use wasm_bindgen_futures::{JsFuture, spawn_local};
use web_sys::{HtmlCanvasElement, KeyboardEvent, PointerEvent, WheelEvent};

/// Starts running a precompiled easl program (the bytes `easl compile
/// --web` writes beside the page), rendering into `canvas`, and returns it
/// at once, for the page to pause, resume, and wait on. `host` carries
/// what the page provides: `module` (the compiled wasm module of this
/// runtime, which the audio worklet instantiates too, or a function
/// returning a promise of it) and `workletUrl` (the audio worklet script)
/// for programs that play audio, `startMidi`, called if the program reads
/// MIDI, and optionally `onPrint`, called with each line the program
/// prints in place of the browser console, and `onAudioError`, called in
/// its place with each error from the audio thread (which stop the audio
/// but not the program).
#[wasm_bindgen(js_name = startProgram)]
pub fn start_program(
  canvas: HtmlCanvasElement,
  program: Vec<u8>,
  host: JsValue,
) -> EaslProgram {
  start(canvas, host, async move {
    PrecompiledProgram::from_bytes(&program)
  })
}

/// Like `startProgram`, for a program given as its sources: parallel lists
/// of file paths and sources, with `main_path` naming the file holding the
/// `@cpu` entry point. The program is compiled first, and a compile error
/// rejects `finished`.
#[cfg(feature = "compiler")]
#[wasm_bindgen(js_name = startProgramFromSources)]
pub fn start_program_from_sources(
  canvas: HtmlCanvasElement,
  main_path: String,
  paths: Vec<String>,
  sources: Vec<String>,
  host: JsValue,
) -> EaslProgram {
  start(canvas, host, async move {
    compile_program(&main_path, paths, sources)
  })
}

/// Starts running the program `load` produces.
fn start(
  canvas: HtmlCanvasElement,
  host: JsValue,
  load: impl Future<Output = Result<PrecompiledProgram, String>> + 'static,
) -> EaslProgram {
  let control = Rc::new(ProgramControl::default());
  let run_control = Rc::clone(&control);
  let load = RefCell::new(Some(load));
  let finished = Promise::new(&mut |resolve, reject| {
    let control = Rc::clone(&run_control);
    let (canvas, host) = (canvas.clone(), host.clone());
    let load = load.borrow_mut().take().unwrap();
    spawn_local(async move {
      let result = match load.await {
        Ok(program) => run_program(canvas, program, host, &control).await,
        Err(message) => Err(message),
      };
      match result {
        Ok(()) => resolve.call0(&JsValue::NULL),
        Err(message) => reject.call1(
          &JsValue::NULL,
          &JsError::new(&format!("easl: {message}")).into(),
        ),
      }
      .unwrap();
    });
  });
  EaslProgram { control, finished }
}

/// A running easl program, as its page sees it (see `startProgram`).
#[wasm_bindgen]
pub struct EaslProgram {
  control: Rc<ProgramControl>,
  finished: Promise,
}

#[wasm_bindgen]
impl EaslProgram {
  /// Pauses the program: it runs no more frames until resumed, its canvas
  /// keeps showing the last one, and its audio is suspended. Its clock is
  /// paused too, so `window-time` and `audio-time` continue where they
  /// left off when it's resumed.
  pub fn pause(&self) {
    self.control.set_paused(true);
  }

  /// Resumes a paused program.
  pub fn resume(&self) {
    self.control.set_paused(false);
  }

  /// Whether the program is paused.
  #[wasm_bindgen(getter)]
  pub fn paused(&self) -> bool {
    self.control.paused.get()
  }

  /// Settles when the program ends: resolves when it finishes (a program
  /// with a window runs until it calls `close-window`), and rejects with
  /// the description of a compile or runtime error, prefixed `easl: ` so
  /// it's clear where it comes from.
  #[wasm_bindgen(getter)]
  pub fn finished(&self) -> Promise {
    self.finished.clone()
  }
}

/// What the page controls about a running program.
#[derive(Default)]
struct ProgramControl {
  paused: Cell<bool>,
  /// Whether the program plays audio, which pausing suspends.
  plays_audio: Cell<bool>,
  /// Resolves what a paused run is waiting on (see `unpaused`).
  resume: RefCell<Option<Function>>,
}

impl ProgramControl {
  fn set_paused(&self, paused: bool) {
    self.paused.set(paused);
    if self.plays_audio.get() {
      audio::set_paused(paused);
    }
    if paused {
      return;
    }
    let resume = self.resume.borrow_mut().take();
    if let Some(resume) = resume {
      resume.call0(&JsValue::NULL).unwrap();
    }
  }

  /// Waits until the program isn't paused.
  async fn unpaused(&self) {
    while self.paused.get() {
      let resumed = Promise::new(&mut |resolve, _| {
        *self.resume.borrow_mut() = Some(resolve);
      });
      JsFuture::from(resumed).await.unwrap();
    }
  }
}

async fn run_program(
  canvas: HtmlCanvasElement,
  program: PrecompiledProgram,
  host: JsValue,
  control: &ProgramControl,
) -> Result<(), String> {
  console_error_panic_hook::set_once();
  let entry_name = program.entry_name.clone();
  if program.audio.is_some() {
    control.plays_audio.set(true);
    audio::configure(AudioHost {
      module: Reflect::get(&host, &"module".into()).unwrap_or_default(),
      worklet_url: Reflect::get(&host, &"workletUrl".into())
        .ok()
        .and_then(|url| url.as_string())
        .unwrap_or_default(),
      on_error: Reflect::get(&host, &"onAudioError".into())
        .ok()
        .and_then(|on_error| on_error.dyn_into::<Function>().ok()),
    })
    .map_err(describe_js_error)?;
    if control.paused.get() {
      audio::set_paused(true);
    }
  }
  if program.reads_midi
    && let Ok(start_midi) = Reflect::get(&host, &"startMidi".into())
    && let Some(start_midi) = start_midi.dyn_ref::<Function>()
  {
    let _ = start_midi.call0(&JsValue::NULL);
  }

  check_shared_atomics_supported(&program)?;
  if program.sends_midi {
    return Err(
      "MIDI output isn't supported on the web yet: the program sends MIDI"
        .to_string(),
    );
  }
  let (surface, device, queue, surface_config) =
    create_surface_and_device(&canvas).await?;

  let on_print = Reflect::get(&host, &"onPrint".into())
    .ok()
    .and_then(|on_print| on_print.dyn_into::<Function>().ok());
  let mut runtime =
    VmCpuRuntime::from_precompiled(program, WebIO::new(on_print), None)
      .map_err(describe_error)?;
  install_gpu_error_handler(&device);
  let gpu = GpuCore::new_from_parts(
    device,
    queue,
    runtime.env.wgsl(),
    &runtime.env.binding_infos(),
    runtime.env.gpu_entries(),
  );
  gpu.write().unwrap().attach_surface(
    surface,
    surface_config,
    ScreenTarget::Texture,
  );
  runtime.env.io.set_gpu(Arc::clone(&gpu));
  install_input_listeners(&gpu, &canvas);

  let mut app = WebApp {
    runtime,
    gpu,
    canvas,
    window_start_time: None,
    last_frame_time: None,
    last_frame_delta: 0.,
    resuming: false,
  };
  let started = app.runtime.start(&entry_name);
  let mut state = app.complete_readbacks(started).await?;
  while state == VmRunState::WindowOpen {
    let timestamp = next_animation_frame().await;
    if control.paused.get() {
      control.unpaused().await;
      app.resume();
      continue;
    }
    let begun = app.begin_frame(timestamp);
    state = app.complete_readbacks(begun).await?;
    app.end_frame();
  }
  app.render_queued_work();
  Ok(())
}

/// Applies a raw MIDI message (status byte first) to the program's MIDI
/// input, as if it came from a MIDI device: the page's Web MIDI listener
/// calls this, and so can the page's own code (an on-screen keyboard, say).
#[wasm_bindgen(js_name = sendMidiMessage)]
pub fn send_midi_message(bytes: &[u8]) {
  easl::midi::handle_message(bytes);
  audio::forward_midi(bytes);
}

/// Parses, validates, and compiles the program.
#[cfg(feature = "compiler")]
fn compile_program(
  main_path: &str,
  paths: Vec<String>,
  sources: Vec<String>,
) -> Result<PrecompiledProgram, String> {
  let sources: HashMap<PathBuf, String> =
    paths.into_iter().map(PathBuf::from).zip(sources).collect();
  match load_easl_program_from_sources(Path::new(main_path), &sources)
    .map_err(|e| e.to_string())?
  {
    Ok((documents, Ok(mut program))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      if errors.is_empty() {
        PrecompiledProgram::new(program).map_err(describe_error)
      } else {
        Err(errors.describe(&documents))
      }
    }
    Ok((documents, Err(errors))) => Err(errors.describe(&documents)),
    Err(failed_documents) => Err(failed_documents.describe_parse_failures()),
  }
}

/// Rejects programs with atomics shared between the main and audio threads
/// on pages that aren't cross-origin isolated: those atomics live in
/// `SharedArrayBuffer`s (see `atomics`), which browsers only allow on
/// isolated pages.
fn check_shared_atomics_supported(
  program: &PrecompiledProgram,
) -> Result<(), String> {
  if program.shared_atomic_globals.is_empty()
    || atomics::cross_origin_isolated()
  {
    return Ok(());
  }
  Err(format!(
    "the atomic variable(s) {} are used by both the main thread and the \
     audio thread, which needs a cross-origin isolated page: serve it with \
     the headers {}",
    program.shared_atomic_globals.join(", "),
    atomics::ISOLATION_HEADERS
  ))
}

fn describe_error(error: impl Display) -> String {
  error.to_string()
}

fn describe_js_error(error: JsValue) -> String {
  format!("{error:?}")
}

/// Creates a WebGPU device and a surface for `canvas`, sized to the
/// canvas's displayed size in physical pixels.
async fn create_surface_and_device(
  canvas: &HtmlCanvasElement,
) -> Result<
  (
    wgpu::Surface<'static>,
    wgpu::Device,
    wgpu::Queue,
    wgpu::SurfaceConfiguration,
  ),
  String,
> {
  let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
    backends: wgpu::Backends::BROWSER_WEBGPU,
    ..wgpu::InstanceDescriptor::new_without_display_handle()
  });
  let surface = instance
    .create_surface(wgpu::SurfaceTarget::Canvas(canvas.clone()))
    .map_err(describe_error)?;
  let adapter = instance
    .request_adapter(&wgpu::RequestAdapterOptions {
      power_preference: wgpu::PowerPreference::default(),
      compatible_surface: Some(&surface),
      force_fallback_adapter: false,
    })
    .await
    .map_err(|_| "this browser has no WebGPU support".to_string())?;
  let (device, queue) = adapter
    .request_device(&wgpu::DeviceDescriptor {
      label: None,
      required_features: wgpu::Features::empty(),
      required_limits: adapter.limits(),
      memory_hints: wgpu::MemoryHints::default(),
      ..Default::default()
    })
    .await
    .map_err(describe_error)?;
  let surface_caps = surface.get_capabilities(&adapter);
  let (width, height) = displayed_size(canvas);
  canvas.set_width(width);
  canvas.set_height(height);
  // The canvas is filled by copying the screen texture into it
  // (`ScreenTarget::Texture`), which is this format's sRGB variant, so the
  // canvas holds sRGB-encoded colors like a native window.
  let surface_config = wgpu::SurfaceConfiguration {
    usage: wgpu::TextureUsages::RENDER_ATTACHMENT
      | wgpu::TextureUsages::COPY_DST,
    format: surface_caps.formats[0],
    width,
    height,
    present_mode: wgpu::PresentMode::Fifo,
    alpha_mode: surface_caps.alpha_modes[0],
    view_formats: vec![],
    desired_maximum_frame_latency: 2,
  };
  surface.configure(&device, &surface_config);
  Ok((surface, device, queue, surface_config))
}

/// The canvas's displayed size in physical pixels.
fn displayed_size(canvas: &HtmlCanvasElement) -> (u32, u32) {
  let scale = device_pixel_ratio();
  (
    ((canvas.client_width() as f64 * scale) as u32).max(1),
    ((canvas.client_height() as f64 * scale) as u32).max(1),
  )
}

fn device_pixel_ratio() -> f64 {
  web_sys::window().map_or(1., |window| window.device_pixel_ratio())
}

/// A running program and the canvas it draws to.
struct WebApp {
  runtime: VmCpuRuntime<WebIO>,
  gpu: Arc<RwLock<GpuCore>>,
  canvas: HtmlCanvasElement,
  /// `requestAnimationFrame` timestamps, in milliseconds, with the window's
  /// start moved later by the time the program has spent paused.
  window_start_time: Option<f64>,
  last_frame_time: Option<f64>,
  /// The time between the last two frames, in milliseconds.
  last_frame_delta: f64,
  /// Whether the next frame is the first since the program was resumed.
  resuming: bool,
}

impl WebApp {
  /// Prepares for the first frame after a pause: input that only lasts a
  /// frame and arrived while paused is dropped, and the clock skips the
  /// paused time (see `begin_frame`).
  fn resume(&mut self) {
    self.resuming = true;
    clear_frame_input(&mut self.gpu.write().unwrap());
  }

  /// Starts a frame at `timestamp` (milliseconds), mirroring the native
  /// window loop's per-frame bookkeeping.
  fn begin_frame(&mut self, timestamp: f64) -> Result<VmRunState, EvalError> {
    self.fit_surface_to_canvas();
    if std::mem::take(&mut self.resuming)
      && let (Some(start), Some(last)) = (
        self.window_start_time.as_mut(),
        self.last_frame_time.as_mut(),
      )
    {
      // The paused time passes for neither: the first frame back comes one
      // frame after the last one before the pause, at the pace before it.
      let paused_time = timestamp - *last - self.last_frame_delta;
      *start += paused_time;
      *last += paused_time;
    }
    let start = *self.window_start_time.get_or_insert(timestamp);
    let delta = self.last_frame_time.map_or(0., |last| timestamp - last);
    self.last_frame_time = Some(timestamp);
    self.last_frame_delta = delta;
    let mut gpu = self.gpu.write().unwrap();
    gpu.window_time = ((timestamp - start) / 1000.) as f32;
    gpu.window_delta_time = (delta / 1000.) as f32;
    drop(gpu);
    self.runtime.run_frame()
  }

  /// Finishes a frame: advances the frame counter, clears the inputs that
  /// only last a frame, and renders.
  fn end_frame(&mut self) {
    {
      let mut gpu = self.gpu.write().unwrap();
      gpu.window_frame_index += 1;
      clear_frame_input(&mut gpu);
      match gpu.mouse_capture_request.take() {
        // Browsers only grant pointer lock shortly after a user gesture,
        // which is how programs normally request it (on a click).
        Some(true) => self.canvas.request_pointer_lock(),
        Some(false) => {
          if let Some(document) =
            web_sys::window().and_then(|window| window.document())
          {
            document.exit_pointer_lock();
          }
        }
        None => {}
      }
    }
    self.render_queued_work();
  }

  /// Serves the run's GPU readbacks until it no longer awaits one: each
  /// executes the queued GPU work, reads the binding back, and resumes the
  /// run.
  async fn complete_readbacks(
    &mut self,
    mut state: Result<VmRunState, EvalError>,
  ) -> Result<VmRunState, String> {
    loop {
      match state.map_err(describe_error)? {
        VmRunState::AwaitingReadback => {
          let bytes = self.begin_readback().await;
          state = self.runtime.complete_readback(&bytes);
        }
        other => return Ok(other),
      }
    }
  }

  /// Flushes the queued GPU work and starts reading back the binding the
  /// run is waiting on.
  fn begin_readback(&mut self) -> BufferRead {
    let readback = self
      .runtime
      .pending_readback()
      .expect("the run is awaiting a readback");
    self.runtime.env.io.flush_queued_gpu_work();
    self.gpu.read().unwrap().read_buffer_async(
      readback.group,
      readback.binding,
      readback.size,
    )
  }

  /// Executes the GPU work the program has queued, presenting any draws to
  /// the canvas.
  fn render_queued_work(&mut self) {
    let draw_calls = self.runtime.env.io.take_frame_draw_calls();
    self.gpu.write().unwrap().render_frame(&draw_calls);
  }

  /// Resizes the surface when the canvas's displayed size has changed.
  fn fit_surface_to_canvas(&mut self) {
    let (width, height) = displayed_size(&self.canvas);
    let mut gpu = self.gpu.write().unwrap();
    if gpu.window_size == (width, height) {
      return;
    }
    self.canvas.set_width(width);
    self.canvas.set_height(height);
    gpu.resize_surface(width, height);
  }
}

/// Clears the input that only lasts a frame: presses and movement since the
/// frame before.
fn clear_frame_input(gpu: &mut GpuCore) {
  gpu.keys_just_down.clear();
  gpu.mouse_just_down = false;
  gpu.mouse_right_just_down = false;
  gpu.mouse_delta = (0., 0.);
  gpu.mouse_scroll_delta = (0., 0.);
}

/// Waits for the browser's next animation frame, returning its timestamp in
/// milliseconds.
async fn next_animation_frame() -> f64 {
  let frame = Promise::new(&mut |resolve, _| {
    web_sys::window()
      .unwrap()
      .request_animation_frame(&resolve)
      .unwrap();
  });
  JsFuture::from(frame).await.unwrap().as_f64().unwrap()
}

/// Feeds keyboard and pointer input into the GPU core's input state, which
/// the window/input query builtins read.
fn install_input_listeners(
  gpu: &Arc<RwLock<GpuCore>>,
  canvas: &HtmlCanvasElement,
) {
  let window = web_sys::window().unwrap();

  // Keys are named like the native window names them (`key_names`). A
  // release clears the names its press recorded, by physical key: with
  // modifiers changing in between (shift released before "1"), the release
  // can carry a different `key` than the press did. Events without a
  // physical key (some on-screen keyboards) are named by their own `key`.
  let pressed: Rc<RefCell<HashMap<String, Vec<String>>>> = Rc::default();
  let key_gpu = Arc::clone(gpu);
  let key_pressed = Rc::clone(&pressed);
  add_listener(&window, "keydown", move |event: KeyboardEvent| {
    if event.repeat() {
      return;
    }
    let names = key_names(&event);
    if !event.code().is_empty() {
      key_pressed.borrow_mut().insert(event.code(), names.clone());
    }
    let mut gpu = key_gpu.write().unwrap();
    for key in names {
      gpu.keys_down.insert(key.clone());
      gpu.keys_just_down.insert(key);
    }
  });
  let key_gpu = Arc::clone(gpu);
  let key_pressed = Rc::clone(&pressed);
  add_listener(&window, "keyup", move |event: KeyboardEvent| {
    let names = key_pressed
      .borrow_mut()
      .remove(&event.code())
      .unwrap_or_else(|| key_names(&event));
    let mut gpu = key_gpu.write().unwrap();
    for key in names {
      gpu.keys_down.remove(&key);
    }
  });
  let key_gpu = Arc::clone(gpu);
  add_listener(&window, "blur", move |_: web_sys::Event| {
    pressed.borrow_mut().clear();
    let mut gpu = key_gpu.write().unwrap();
    gpu.keys_down.clear();
    gpu.mouse_down = false;
    gpu.mouse_right_down = false;
  });

  // Pointer lock backs `capture-mouse`: `end_frame` requests it, and the
  // browser reports when it's granted or lost (Escape always exits it).
  let lock_gpu = Arc::clone(gpu);
  let lock_canvas = canvas.clone();
  let document = window.document().unwrap();
  add_listener(&document, "pointerlockchange", move |_: web_sys::Event| {
    let locked = web_sys::window()
      .and_then(|window| window.document())
      .and_then(|document| document.pointer_lock_element())
      .is_some_and(|element| element == *lock_canvas.as_ref());
    lock_gpu.write().unwrap().mouse_captured = locked;
  });

  let pointer_gpu = Arc::clone(gpu);
  add_listener(canvas, "pointermove", move |event: PointerEvent| {
    let scale = device_pixel_ratio();
    let mut gpu = pointer_gpu.write().unwrap();
    gpu.mouse_coords = (
      (event.offset_x() as f64 * scale).max(0.) as u32,
      (event.offset_y() as f64 * scale).max(0.) as u32,
    );
    gpu.mouse_delta.0 += event.movement_x() as f32;
    gpu.mouse_delta.1 += event.movement_y() as f32;
  });
  let pointer_gpu = Arc::clone(gpu);
  add_listener(canvas, "pointerenter", move |_: PointerEvent| {
    pointer_gpu.write().unwrap().mouse_present = true;
  });
  let pointer_gpu = Arc::clone(gpu);
  add_listener(canvas, "pointerleave", move |_: PointerEvent| {
    pointer_gpu.write().unwrap().mouse_present = false;
  });
  let pointer_gpu = Arc::clone(gpu);
  add_listener(canvas, "pointerdown", move |event: PointerEvent| {
    let mut gpu = pointer_gpu.write().unwrap();
    match event.button() {
      0 => {
        gpu.mouse_down = true;
        gpu.mouse_just_down = true;
      }
      2 => {
        gpu.mouse_right_down = true;
        gpu.mouse_right_just_down = true;
      }
      _ => {}
    }
  });
  let pointer_gpu = Arc::clone(gpu);
  add_listener(&window, "pointerup", move |event: PointerEvent| {
    let mut gpu = pointer_gpu.write().unwrap();
    match event.button() {
      0 => gpu.mouse_down = false,
      2 => gpu.mouse_right_down = false,
      _ => {}
    }
  });
  // Scrolling over the canvas is input to the program, not to the page.
  // `WheelEvent`'s deltas already point the way `mouse-scroll-delta`'s do;
  // they're converted from the event's unit (CSS pixels, lines, or pages)
  // to physical pixels.
  let wheel_gpu = Arc::clone(gpu);
  let wheel_canvas = canvas.clone();
  add_listener(canvas, "wheel", move |event: WheelEvent| {
    event.prevent_default();
    let scale = device_pixel_ratio();
    let unit = match event.delta_mode() {
      WheelEvent::DOM_DELTA_LINE => SCROLL_LINE_HEIGHT as f64 * scale,
      WheelEvent::DOM_DELTA_PAGE => wheel_canvas.height() as f64,
      _ => scale,
    };
    let mut gpu = wheel_gpu.write().unwrap();
    gpu.mouse_scroll_delta.0 += (event.delta_x() * unit) as f32;
    gpu.mouse_scroll_delta.1 += (event.delta_y() * unit) as f32;
  });
  // Right clicks are input, not a request for the context menu.
  add_listener(canvas, "contextmenu", move |event: web_sys::Event| {
    event.prevent_default();
  });
}

/// The `key-down?` names a key event affects, mirroring the native
/// window's: fixed names for the supported named keys (`named_key_names`
/// in `src/window.rs`), and for character keys the lowercase character
/// without modifiers, so shift+1 is still `"1"`.
fn key_names(event: &KeyboardEvent) -> Vec<String> {
  let key = event.key();
  let names: &[&str] = match key.as_str() {
    " " => &[" ", "space"],
    "Shift" => &["shift"],
    "Control" => &["ctrl"],
    "Alt" => &["alt"],
    "Meta" | "Super" => &["super"],
    "Escape" => &["escape"],
    "Enter" => &["enter"],
    "Tab" => &["tab"],
    "Backspace" => &["backspace"],
    "Delete" => &["delete"],
    "ArrowUp" => &["up"],
    "ArrowDown" => &["down"],
    "ArrowLeft" => &["left"],
    "ArrowRight" => &["right"],
    _ if key.chars().count() == 1 => {
      return vec![unmodified_character(event, &key)];
    }
    _ => &[],
  };
  names.iter().map(|name| name.to_string()).collect()
}

/// The character a key produces without modifiers, lowercased. Browsers
/// only report the modified character, so: without shift or alt that's
/// the character itself (in the user's layout), shift leaves a letter a
/// letter, and otherwise it's read off the physical key, assuming a US
/// layout.
fn unmodified_character(event: &KeyboardEvent, key: &str) -> String {
  if !event.alt_key()
    && (!event.shift_key() || key.chars().all(char::is_alphabetic))
  {
    return key.to_lowercase();
  }
  us_layout_character(&event.code()).unwrap_or_else(|| key.to_lowercase())
}

/// The unshifted character of a physical key (a `KeyboardEvent.code`) on a
/// US layout.
fn us_layout_character(code: &str) -> Option<String> {
  if let Some(letter) = code.strip_prefix("Key") {
    return Some(letter.to_lowercase());
  }
  if let Some(digit) = code
    .strip_prefix("Digit")
    .or_else(|| code.strip_prefix("Numpad"))
    .filter(|digit| digit.len() == 1)
  {
    return Some(digit.to_string());
  }
  let character = match code {
    "Minus" => "-",
    "Equal" => "=",
    "BracketLeft" => "[",
    "BracketRight" => "]",
    "Backslash" => "\\",
    "Semicolon" => ";",
    "Quote" => "'",
    "Comma" => ",",
    "Period" => ".",
    "Slash" => "/",
    "Backquote" => "`",
    _ => return None,
  };
  Some(character.to_string())
}

/// Adds an event listener that lives as long as the page.
fn add_listener<E: JsCast + 'static>(
  target: &web_sys::EventTarget,
  event_name: &str,
  mut handler: impl FnMut(E) + 'static,
) {
  let closure =
    Closure::<dyn FnMut(web_sys::Event)>::new(move |event: web_sys::Event| {
      handler(event.unchecked_into())
    });
  target
    .add_event_listener_with_callback(
      event_name,
      closure.as_ref().unchecked_ref(),
    )
    .unwrap();
  closure.forget();
}
