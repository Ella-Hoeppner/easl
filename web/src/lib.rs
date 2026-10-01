//! The easl web runtime. Compiled once to WebAssembly, it compiles an easl
//! program in the browser and runs its `@cpu` entry on the bytecode VM,
//! rendering into a canvas through WebGPU. Frames come from
//! `requestAnimationFrame` rather than a native window loop.

mod audio;
mod audio_engine;
mod io;

use std::{
  collections::HashMap,
  path::{Path, PathBuf},
  sync::{Arc, RwLock},
};

use audio::AudioHost;
use easl::{
  CompilerTarget,
  audio::AudioSource,
  compiler::{entry::EntryPoint, program::Program},
  interpreter::{
    EvalError, GpuBindingInfo, GpuBufferKind, IOManager, VmCpuRuntime,
    VmRunState, pick_entry_point_name,
  },
  load_easl_program_from_sources,
  window::{BufferRead, GpuCore, install_gpu_error_handler},
};
use io::WebIO;
use js_sys::{Function, Promise, Reflect};
use wasm_bindgen::{JsCast, prelude::*};
use wasm_bindgen_futures::JsFuture;
use web_sys::{HtmlCanvasElement, KeyboardEvent, PointerEvent};

/// Compiles and runs an easl program, rendering into `canvas`. The program
/// is given as parallel lists of file paths and sources, with `main_path`
/// naming the file holding the `@cpu` entry point. `host` carries what the
/// page provides: `module` (this runtime's compiled wasm module) and
/// `workletUrl` (the audio worklet script) for programs that play audio,
/// and `startMidi`, called if the program reads MIDI. Resolves once the
/// program finishes; compile and runtime errors reject with their
/// description.
#[wasm_bindgen(js_name = runEaslProgram)]
pub async fn run_easl_program(
  canvas: HtmlCanvasElement,
  main_path: String,
  paths: Vec<String>,
  sources: Vec<String>,
  host: JsValue,
) -> Result<(), JsError> {
  console_error_panic_hook::set_once();
  let source_map: HashMap<PathBuf, String> =
    paths.into_iter().map(PathBuf::from).zip(sources).collect();
  let program = compile_program(Path::new(&main_path), &source_map)
    .map_err(|e| JsError::new(&e))?;
  let audio_source = (!program
    .find_fn_names_by_entry_point(|e| e == EntryPoint::Audio)
    .is_empty())
  .then(|| {
    let (program, function_names) =
      program.clone().compile_to_bytecode_program();
    AudioSource::Bytecode {
      program,
      function_names,
      shared_table: None,
    }
  });
  let entry_name =
    pick_entry_point_name(&program, None).map_err(describe_error)?;
  if audio_source.is_some() {
    audio::configure(AudioHost {
      module: Reflect::get(&host, &"module".into()).unwrap_or_default(),
      worklet_url: Reflect::get(&host, &"workletUrl".into())
        .ok()
        .and_then(|url| url.as_string())
        .unwrap_or_default(),
    })
    .map_err(describe_error)?;
  }
  if program
    .top_level_vars
    .iter()
    .any(|var| var.name.starts_with("easl_midi_"))
    && let Ok(start_midi) = Reflect::get(&host, &"startMidi".into())
    && let Some(start_midi) = start_midi.dyn_ref::<Function>()
  {
    let _ = start_midi.call0(&JsValue::NULL);
  }

  let (surface, device, queue, surface_config) =
    create_surface_and_device(&canvas).await?;

  let mut runtime =
    VmCpuRuntime::new(program, WebIO::new(), None, audio_source)
      .map_err(describe_error)?;
  check_browser_support(&runtime.env.binding_infos())?;
  install_gpu_error_handler(&device);
  let gpu = GpuCore::new_from_parts(
    device,
    queue,
    runtime.env.wgsl(),
    &runtime.env.binding_infos(),
    runtime.env.gpu_entries(),
  );
  {
    let mut gpu = gpu.write().unwrap();
    gpu.window_size = (surface_config.width, surface_config.height);
    gpu.surface = Some(surface);
    gpu.surface_config = Some(surface_config);
  }
  runtime.env.io.set_gpu(Arc::clone(&gpu));
  install_input_listeners(&gpu, &canvas);

  let mut app = WebApp {
    runtime,
    gpu,
    canvas,
    window_start_time: None,
    last_frame_time: None,
  };
  let started = app.runtime.start(&entry_name);
  let mut state = app.complete_readbacks(started).await?;
  while state == VmRunState::WindowOpen {
    let timestamp = next_animation_frame().await;
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

/// Parses and validates the program.
fn compile_program(
  main_path: &Path,
  sources: &HashMap<PathBuf, String>,
) -> Result<Program, String> {
  match load_easl_program_from_sources(main_path, sources)
    .map_err(|e| e.to_string())?
  {
    Ok((documents, Ok(mut program))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      if errors.is_empty() {
        Ok(program)
      } else {
        Err(errors.describe(&documents))
      }
    }
    Ok((documents, Err(errors))) => Err(errors.describe(&documents)),
    Err(failed_documents) => Err(failed_documents.describe_parse_failures()),
  }
}

/// Rejects programs whose vertex shaders use storage-write variables: native
/// easl requests the `VERTEX_WRITABLE_STORAGE` feature for them, which
/// browsers don't offer.
fn check_browser_support(bindings: &[GpuBindingInfo]) -> Result<(), JsError> {
  let names: Vec<&str> = bindings
    .iter()
    .filter(|binding| {
      binding.kind == GpuBufferKind::StorageReadWrite && binding.stages.vertex
    })
    .map(|binding| &*binding.name)
    .collect();
  if names.is_empty() {
    Ok(())
  } else {
    Err(JsError::new(&format!(
      "vertex shaders use the storage-write variable(s) {}, which browsers \
       don't support; make them `@storage` (read-only) or only use them \
       outside vertex shaders",
      names.join(", ")
    )))
  }
}

fn describe_error(error: impl std::fmt::Debug) -> JsError {
  JsError::new(&format!("{error:?}"))
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
  JsError,
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
    .map_err(|_| JsError::new("easl needs a browser with WebGPU support"))?;
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
  let surface_config = wgpu::SurfaceConfiguration {
    usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
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
  /// `requestAnimationFrame` timestamps, in milliseconds.
  window_start_time: Option<f64>,
  last_frame_time: Option<f64>,
}

impl WebApp {
  /// Starts a frame at `timestamp` (milliseconds), mirroring the native
  /// window loop's per-frame bookkeeping.
  fn begin_frame(&mut self, timestamp: f64) -> Result<VmRunState, EvalError> {
    self.fit_surface_to_canvas();
    let start = *self.window_start_time.get_or_insert(timestamp);
    let delta = self.last_frame_time.map_or(0., |last| timestamp - last);
    self.last_frame_time = Some(timestamp);
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
      gpu.keys_just_down.clear();
      gpu.mouse_just_down = false;
    }
    self.render_queued_work();
  }

  /// Serves the run's GPU readbacks until it no longer awaits one: each
  /// executes the queued GPU work, reads the binding back, and resumes the
  /// run.
  async fn complete_readbacks(
    &mut self,
    mut state: Result<VmRunState, EvalError>,
  ) -> Result<VmRunState, JsError> {
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
    gpu.pending_present = None;
    gpu.window_size = (width, height);
    let gpu = &mut *gpu;
    if let (Some(surface), Some(config)) =
      (&gpu.surface, &mut gpu.surface_config)
    {
      config.width = width;
      config.height = height;
      surface.configure(&gpu.device, config);
    }
  }
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

  // Single-character keys, lowercased, matching the native window's
  // `Key::Character` handling.
  let key_gpu = Arc::clone(gpu);
  add_listener(&window, "keydown", move |event: KeyboardEvent| {
    let key = event.key();
    if event.repeat() || key.chars().count() != 1 {
      return;
    }
    let key = key.to_lowercase();
    let mut gpu = key_gpu.write().unwrap();
    gpu.keys_down.insert(key.clone());
    gpu.keys_just_down.insert(key);
  });
  let key_gpu = Arc::clone(gpu);
  add_listener(&window, "keyup", move |event: KeyboardEvent| {
    key_gpu
      .write()
      .unwrap()
      .keys_down
      .remove(&event.key().to_lowercase());
  });

  let pointer_gpu = Arc::clone(gpu);
  add_listener(canvas, "pointermove", move |event: PointerEvent| {
    let scale = device_pixel_ratio();
    pointer_gpu.write().unwrap().mouse_coords = (
      (event.offset_x() as f64 * scale).max(0.) as u32,
      (event.offset_y() as f64 * scale).max(0.) as u32,
    );
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
    if event.button() == 0 {
      let mut gpu = pointer_gpu.write().unwrap();
      gpu.mouse_down = true;
      gpu.mouse_just_down = true;
    }
  });
  let pointer_gpu = Arc::clone(gpu);
  add_listener(&window, "pointerup", move |event: PointerEvent| {
    if event.button() == 0 {
      pointer_gpu.write().unwrap().mouse_down = false;
    }
  });
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
