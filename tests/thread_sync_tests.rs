//! Cross-thread shared-variable semantics tests.
//!
//! Each test runs `data/thread_sync/<name>.easl` under a scripted schedule
//! that single-steps the main thread's frame loop and the audio thread's
//! batch loop deterministically on one thread — `Frame` runs one frame
//! through the production `FrameDriver` path (including its boundary
//! adopt/publish), `AudioBatch(n)` runs `n` samples through the production
//! `VmAudioDriver::run_batch` (including its boundary adopt/publish). The
//! sharing semantics are boundary-batched, so iteration-granularity
//! scheduling is a complete model of every possible real-thread
//! interleaving.
//!
//! The ordered trace of publishes, adopts, prints, and audio samples is
//! golden-matched against `data/thread_sync/<name>.txt`, and every test
//! runs on both main-thread runtimes (tree-walking and bytecode VM) against
//! the same golden. The goldens pin *exactly* when cross-thread syncs
//! happen: a spurious publish/adopt is a silent performance bug, a missing
//! one is a correctness bug, and the bootstrap publish-all at `start-audio`
//! enumerates precisely which variables the static analysis classified as
//! shared.

use std::sync::Arc;

use easl::audio::{AudioSource, VmAudioDriver};
use easl::compiler::core::load_easl_program_from_file;
use easl::compiler::program::CompilerTarget;
use easl::external::ExternalVars;
use easl::interpreter::{
  BufferUpload, CpuRuntime, EvalError, EvalException, FrameDriver,
  GpuBindingInfo, GpuEntryInfo, IOManager, StdoutIO, WindowEvent,
  run_program_entry_with_io_runtime_and_external_from_path,
};
use std::fs;
use std::path::Path;

/// One step of the scripted cross-thread schedule.
#[derive(Clone)]
enum Step {
  /// Run one main-thread frame (through the production frame-driver path).
  Frame,
  /// Run one audio callback batch of the given sample count (through the
  /// production `VmAudioDriver::run_batch`).
  AudioBatch(usize),
  /// Overwrite a whole `@external` var through the embedder handle (f32
  /// words). External steps appearing before the first `Frame`/`AudioBatch`
  /// run *before* the program starts — the embedder-seeds-then-runs
  /// pattern.
  ExternalWrite(&'static str, &'static [f32]),
  /// Overwrite one element of an `@external` array var (read-modify-write
  /// on the whole variable, per the API contract).
  ExternalWriteIndex(&'static str, u32, f32),
  /// Read an `@external` var through the handle, tracing its value —
  /// adopts the newest published snapshot first.
  ExternalRead(&'static str),
}
use Step::{
  AudioBatch, ExternalRead, ExternalWrite, ExternalWriteIndex, Frame,
};

impl Step {
  fn is_external(&self) -> bool {
    matches!(
      self,
      ExternalWrite(..) | ExternalWriteIndex(..) | ExternalRead(..)
    )
  }
}

fn format_f32_words(words: &[u32]) -> String {
  words
    .iter()
    .map(|w| f32::from_bits(*w).to_string())
    .collect::<Vec<_>>()
    .join(" ")
}

/// Executes one external step against the handle, returning its trace line.
fn run_external_step(handle: &ExternalVars, step: &Step) -> String {
  match step {
    ExternalWrite(name, values) => {
      let words: Vec<u32> = values.iter().map(|v| v.to_bits()).collect();
      handle
        .write_external_var_raw(name, &words)
        .unwrap_or_else(|e| panic!("external write of `{name}` failed: {e}"));
      format!("external-write: {name} = {}", format_f32_words(&words))
    }
    ExternalWriteIndex(name, index, value) => {
      handle
        .write_external_var_index_raw(name, *index, &[value.to_bits()])
        .unwrap_or_else(|e| panic!("external write of `{name}` failed: {e}"));
      format!("external-write: {name}[{index}] = {value}")
    }
    ExternalRead(name) => {
      let words = handle
        .read_external_var_raw(name)
        .unwrap_or_else(|e| panic!("external read of `{name}` failed: {e}"));
      format!("external-read: {name} = {}", format_f32_words(&words))
    }
    _ => unreachable!(),
  }
}

/// A sample rate that makes `t = sample_index / rate` exactly representable
/// (increments of 1/8), keeping sample goldens free of float noise.
const TEST_SAMPLE_RATE: f32 = 8.0;

/// Test IO manager: no real window, no real audio stream, but a real
/// (headless) GPU — dispatches execute genuinely through the shared
/// `GpuCore` frame paths, exactly like `CaptureIO`, so the GPU↔thread sync
/// interplay is exercised for real. `spawn-window` walks the scripted
/// schedule; `start-audio` stashes a `VmAudioDriver` instead of opening a
/// cpal stream, and `AudioBatch` steps drive it.
struct ThreadSyncIO {
  schedule: Vec<Step>,
  trace: Vec<String>,
  /// Delegate for all GPU machinery (headless GpuCore, buffer readback,
  /// queued frame events). Its windowing loop is never used.
  inner: StdoutIO,
  audio: Option<VmAudioDriver>,
  /// Shared-variable names of the audio artifact, index-aligned with the
  /// `u16` indices the batch-boundary trace hooks report.
  audio_shared_names: Vec<Arc<str>>,
  /// The embedder handle, present when the schedule has external steps.
  external: Option<Arc<ExternalVars>>,
  frame_index: usize,
  audio_batch_index: usize,
}

impl ThreadSyncIO {
  fn new(
    schedule: Vec<Step>,
    external: Option<Arc<ExternalVars>>,
    initial_trace: Vec<String>,
  ) -> Self {
    Self {
      schedule,
      trace: initial_trace,
      inner: StdoutIO::new(),
      audio: None,
      audio_shared_names: Vec::new(),
      external,
      frame_index: 0,
      audio_batch_index: 0,
    }
  }

  fn run_audio_batch(&mut self, frames: usize) {
    self
      .trace
      .push(format!("audio-batch {} x{frames}", self.audio_batch_index));
    self.audio_batch_index += 1;
    let Some(driver) = self.audio.as_mut() else {
      self.trace.push("(audio not started)".to_string());
      return;
    };
    // The hooks can't both borrow the trace while `run_batch` holds the
    // driver, so collect indices and append afterward — faithful to the
    // true order, since a batch is adopt-all, then samples, then
    // publish-all.
    let mut samples = Vec::with_capacity(frames);
    let mut adopted = Vec::new();
    let mut published = Vec::new();
    driver.run_batch(
      frames,
      TEST_SAMPLE_RATE,
      |sample| samples.push(sample),
      |index| adopted.push(index),
      |index| published.push(index),
    );
    for index in adopted {
      self.trace.push(format!(
        "audio-adopt: {}",
        self.audio_shared_names[index as usize]
      ));
    }
    self.trace.push(format!(
      "samples: {}",
      samples
        .iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>()
        .join(" ")
    ));
    for index in published {
      self.trace.push(format!(
        "audio-publish: {}",
        self.audio_shared_names[index as usize]
      ));
    }
  }
}

impl IOManager for ThreadSyncIO {
  fn println(&mut self, s: &str) {
    self.trace.push(format!("print: {s}"));
  }

  fn record_draw(
    &mut self,
    vert: u16,
    frag: u16,
    vert_name: &str,
    frag_name: &str,
    vert_count: u32,
    pre_upload: Vec<((u8, u8), BufferUpload)>,
    additive: bool,
    render_target: Option<(u8, u8)>,
  ) -> Result<(), EvalError> {
    self
      .trace
      .push(format!("dispatch-render-shaders {vert_name} {frag_name} {vert_count}"));
    self.inner.record_draw(
      vert,
      frag,
      vert_name,
      frag_name,
      vert_count,
      pre_upload,
      additive,
      render_target,
    )
  }

  fn record_compute(
    &mut self,
    entry: u16,
    entry_name: &str,
    workgroup_count: (u32, u32, u32),
    pre_upload: Vec<((u8, u8), BufferUpload)>,
  ) -> Result<(), EvalError> {
    self
      .trace
      .push(format!("dispatch-compute-shader {entry_name} {workgroup_count:?}"));
    self
      .inner
      .record_compute(entry, entry_name, workgroup_count, pre_upload)
  }

  fn take_frame_draw_calls(&mut self) -> Vec<WindowEvent> {
    self.inner.take_frame_draw_calls()
  }

  fn record_close_window(&mut self) {
    self.trace.push("close-window".to_string());
    self.inner.record_close_window();
  }

  fn sync_gpu_to_cpu(
    &mut self,
    group: u8,
    binding: u8,
    size: u64,
  ) -> Option<Vec<u8>> {
    self.inner.sync_gpu_to_cpu(group, binding, size)
  }

  fn get_gpu(
    &self,
  ) -> Option<std::sync::Arc<std::sync::RwLock<easl::window::GpuCore>>> {
    self.inner.get_gpu()
  }

  fn set_gpu(
    &mut self,
    gpu: std::sync::Arc<std::sync::RwLock<easl::window::GpuCore>>,
  ) {
    self.inner.set_gpu(gpu)
  }

  fn ensure_gpu_ready(
    &mut self,
    wgsl: &str,
    binding_infos: &[GpuBindingInfo],
    gpu_entries: &[GpuEntryInfo],
  ) {
    self.inner.ensure_gpu_ready(wgsl, binding_infos, gpu_entries)
  }

  fn get_buffer_byte_size(&self, group: u8, binding: u8) -> Option<u64> {
    self.inner.get_buffer_byte_size(group, binding)
  }

  fn flush_queued_compute(&mut self) {
    self.inner.flush_queued_compute();
  }

  fn record_gpu_to_cpu_sync(&mut self, name: &Arc<str>) {
    self.trace.push(format!("readback: {name}"));
  }

  fn record_cpu_to_gpu_sync(&mut self, name: &Arc<str>) {
    self.trace.push(format!("upload: {name}"));
  }

  fn record_shared_publish(&mut self, name: &Arc<str>) {
    self.trace.push(format!("main-publish: {name}"));
  }

  fn record_shared_adopt(&mut self, name: &Arc<str>) {
    self.trace.push(format!("main-adopt: {name}"));
  }

  fn window_size(&self) -> (u32, u32) {
    (800, 600)
  }

  fn window_frame_index(&self) -> u32 {
    self.frame_index as u32
  }

  fn run_spawn_window_driver<D: FrameDriver<IO = Self>>(
    driver: &mut D,
  ) -> Result<bool, EvalError> {
    driver.io_mut().trace.push("spawn-window".to_string());
    let schedule = driver.io_mut().schedule.clone();
    let mut frame_index = 0;
    for step in schedule {
      match step {
        Step::Frame => {
          driver.io_mut().frame_index = frame_index;
          driver.io_mut().trace.push(format!("frame {frame_index}"));
          frame_index += 1;
          let frame_result = driver.run_frame();
          // Execute the frame's remaining queued GPU events through the
          // same shared frame path the real winit loop and `CaptureIO` use,
          // skipping only screen-targeted draws (no surface headlessly).
          let events = driver.io_mut().take_frame_draw_calls();
          if !events.is_empty()
            && let Some(gpu) = driver.io_mut().get_gpu()
          {
            let mut gpu = gpu.write().unwrap();
            gpu.execute_frame_gpu_work(&events);
            gpu.execute_frame_screen_renders(&events, None);
          }
          match frame_result {
            Ok(()) => {}
            Err(EvalException::CloseWindow) => break,
            Err(e) => return Err(e.into()),
          }
        }
        Step::AudioBatch(frames) => {
          driver.io_mut().run_audio_batch(frames);
        }
        external_step => {
          let io = driver.io_mut();
          let handle = io
            .external
            .as_ref()
            .expect("schedule has external steps but no handle");
          let line = run_external_step(handle, &external_step);
          io.trace.push(line);
        }
      }
    }
    Ok(false)
  }

  fn start_audio(
    &mut self,
    entry_name: &str,
    source: Option<AudioSource>,
  ) -> Result<(), EvalError> {
    match source {
      Some(AudioSource::Bytecode {
        program,
        function_names,
        shared_table,
      }) => {
        self.trace.push(format!("start-audio: {entry_name}"));
        self.audio_shared_names = program
          .code
          .shared_vars
          .iter()
          .map(|info| info.name.clone())
          .collect();
        self.audio = Some(
          VmAudioDriver::new(
            entry_name,
            program,
            &function_names,
            shared_table,
          )
          .expect("failed to build audio driver"),
        );
      }
      Some(AudioSource::C(_)) => {
        panic!("thread-sync harness only supports the VM audio backend")
      }
      // Repeated start-audio call after the source was consumed: the
      // production no-op. Not traced — it happens every frame.
      None => {}
    }
    Ok(())
  }
}

fn run_thread_sync_test(name: &str, schedule: Vec<Step>) {
  let expected = fs::read_to_string(format!("./data/thread_sync/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/thread_sync/{name}.txt"));

  let source_path_str = format!("./data/thread_sync/{name}.easl");
  let source_path = Path::new(&source_path_str);

  let Ok(Ok((document, program_result))) =
    load_easl_program_from_file(source_path)
  else {
    panic!("{name}: failed to load program");
  };
  let mut program = match program_result {
    Ok(program) => program,
    Err(errors) => panic!("{}", errors.describe(&document)),
  };
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

  // External steps before the first Frame/AudioBatch are the embedder's
  // seed-then-run pattern: they execute against the handle before the
  // program starts.
  let boundary = schedule
    .iter()
    .position(|step| !step.is_external())
    .unwrap_or(schedule.len());
  let (pre_run_steps, loop_steps) = schedule.split_at(boundary);
  let has_external_steps =
    schedule.iter().any(|step| step.is_external());

  // Both main-thread runtimes must produce the identical trace: the frame
  // and batch boundaries are the semantics, not a runtime detail.
  for (runtime, label) in [
    (CpuRuntime::TreeWalking, "tree-walking"),
    (CpuRuntime::BytecodeVm, "bytecode VM"),
  ] {
    // A fresh handle per run — the table carries per-run version state.
    let external = has_external_steps.then(|| ExternalVars::new(&program));
    let initial_trace: Vec<String> = pre_run_steps
      .iter()
      .map(|step| {
        run_external_step(external.as_ref().unwrap(), step)
      })
      .collect();
    let (io, _) = run_program_entry_with_io_runtime_and_external_from_path(
      program.clone(),
      None,
      ThreadSyncIO::new(loop_steps.to_vec(), external.clone(), initial_trace),
      source_path,
      runtime,
      external,
    )
    .unwrap_or_else(|e| panic!("{name}: evaluation error ({label}): {e:#?}"));
    // Lifted audio-closure scope globals (`<scope>_audio_data_<capture>`)
    // and audio entry clones (`<closure>_audio`) carry gensym'd closure
    // names whose numbering isn't stable across runs — normalize the
    // gensym'd prefix, keeping the (meaningful) capture name.
    let normalize = |line: String| -> String {
      let line = if let Some(index) = line.find("_scope_audio_data_") {
        let field = &line[index + "_scope_audio_data_".len()..];
        let prefix_start = line[..index]
          .rfind(' ')
          .map(|space| space + 1)
          .unwrap_or(0);
        format!("{}<audio-scope>_{}", &line[..prefix_start], field)
      } else {
        line
      };
      if let Some(entry) = line.strip_prefix("start-audio: ")
        && entry.ends_with("_audio")
      {
        "start-audio: <audio-closure>".to_string()
      } else {
        line
      }
    };
    let trace: String = io
      .trace
      .into_iter()
      .map(|line| format!("{}\n", normalize(line)))
      .collect();
    assert_eq!(trace, expected, "{name}: trace mismatch ({label})");
  }
}

macro_rules! thread_sync_test {
  ($name:ident, $schedule:expr) => {
    #[test]
    fn $name() {
      run_thread_sync_test(stringify!($name), $schedule.to_vec());
    }
  };
}

thread_sync_test!(
  main_to_audio,
  [Frame, AudioBatch(2), Frame, AudioBatch(2), Frame, AudioBatch(2)]
);
thread_sync_test!(
  audio_to_main,
  [Frame, AudioBatch(3), Frame, AudioBatch(3), Frame]
);
thread_sync_test!(
  only_genuinely_shared_vars_sync,
  [Frame, AudioBatch(2), Frame, AudioBatch(2)]
);
thread_sync_test!(
  dynamic_array_shared,
  [Frame, AudioBatch(4), Frame, AudioBatch(4), Frame, AudioBatch(4)]
);
thread_sync_test!(
  no_start_audio_no_publish,
  [Frame, Frame, Frame]
);
thread_sync_test!(
  close_window_still_publishes,
  [Frame, Frame, AudioBatch(2), Frame, AudioBatch(2)]
);
thread_sync_test!(
  bidirectional_last_writer_wins,
  [Frame, Frame, AudioBatch(2), Frame, AudioBatch(2)]
);
thread_sync_test!(vec_shared, [Frame, AudioBatch(2), Frame, AudioBatch(2)]);
thread_sync_test!(
  audio_writes_array,
  [Frame, AudioBatch(3), Frame, AudioBatch(3), Frame]
);
thread_sync_test!(
  audio_write_gpu_read,
  [Frame, AudioBatch(2), Frame, AudioBatch(2), Frame]
);
thread_sync_test!(
  gpu_write_audio_read,
  [Frame, AudioBatch(2), Frame, AudioBatch(2), Frame, AudioBatch(2)]
);
thread_sync_test!(gpu_write_no_audio_no_readback, [Frame, Frame, Frame]);
thread_sync_test!(
  gpu_write_before_start_audio,
  [Frame, Frame, AudioBatch(2)]
);
thread_sync_test!(
  audio_and_gpu_write_cycle,
  [Frame, AudioBatch(2), Frame, AudioBatch(2), Frame]
);
thread_sync_test!(
  external_to_main_and_gpu,
  [
    ExternalWrite("sliders", &[0.25, 0., 0., 0.]),
    Frame,
    ExternalWrite("sliders", &[0.5, 0., 0., 0.]),
    Frame,
    Frame
  ]
);
thread_sync_test!(
  main_to_external,
  [Frame, ExternalRead("level"), Frame, ExternalRead("level")]
);
thread_sync_test!(
  external_to_audio_direct,
  [
    ExternalWrite("gain", &[0.1]),
    Frame,
    AudioBatch(2),
    ExternalWrite("gain", &[0.3]),
    AudioBatch(2),
    Frame,
    AudioBatch(2)
  ]
);
thread_sync_test!(
  external_array_index_rmw,
  [
    ExternalWriteIndex("params", 1, 0.5),
    Frame,
    ExternalWriteIndex("params", 2, 0.25),
    Frame
  ]
);
thread_sync_test!(no_handle_no_external_publish, [Frame, Frame]);
thread_sync_test!(
  external_seed_survives_start_audio,
  [ExternalWrite("gain", &[0.5]), Frame, AudioBatch(2)]
);
thread_sync_test!(
  audio_closure_scope,
  [Frame, AudioBatch(4), AudioBatch(4), Frame, AudioBatch(4)]
);
thread_sync_test!(
  audio_entry_one_arg,
  [Frame, AudioBatch(4)]
);
thread_sync_test!(
  local_never_shared,
  [Frame, AudioBatch(2), Frame, AudioBatch(2)]
);
