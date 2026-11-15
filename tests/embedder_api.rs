//! easl's API as embedders use it, named here so that making any of it
//! private fails this suite instead of breaking an embedder. It only has to
//! compile. Grouped by embedder; an item one needs goes under it.
#![cfg(feature = "window")]
#![allow(dead_code, unused_variables)]

use std::path::Path;

use easl::{
  CompilerTarget,
  audio::{AudioRestart, AudioSource, VmAudioDriver},
  compiler::{error::ErrorLog, program::Program, types::Type},
  external::ExternalVars,
  interpreter::{
    BufferUpload, CpuRuntime, DrawGeometry, EvalError, EvalException,
    FrameDriver, GpuBindingInfo, GpuEntryInfo, IOManager, MidiState, Primitive,
    StdoutIO, TextureHandle, UserspaceEvalError, Value, WindowEvent,
    run_program_entry_with_io_runtime_and_external_from_path,
  },
  load_easl_program_from_file,
  parse::EaslMultiDocument,
  render_settings::RenderSettings,
  window::{GpuCore, ScreenTarget},
};

/// The pi-Stomp guitar pedal host (`~/repos/pistomp/easl-host`): compiles a
/// patch, runs its CPU entry with an `IOManager` of its own (delegating GPU
/// work to `StdoutIO`), renders its screen headless to an LCD, writes knob
/// and footswitch values into its `@external` vars, and drives its audio
/// thread itself.
mod pistomp {
  use super::*;

  fn compiling(path: &Path) {
    let _ = load_easl_program_from_file;
    let _ = Program::validate_raw_program;
    let _ = CompilerTarget::WGSL;
    let _ = EaslMultiDocument::describe_parse_failures;
    let _ = ErrorLog::describe;
  }

  fn running<IO: IOManager + 'static>(program: Program, io: IO) {
    let vars = ExternalVars::new(&program);
    let _ = run_program_entry_with_io_runtime_and_external_from_path(
      program,
      None,
      io,
      Path::new("patch.easl"),
      CpuRuntime::BytecodeVm,
      Some(vars),
    );
    let _ = StdoutIO::new;
  }

  fn externals(vars: &ExternalVars) {
    let types: Vec<(&str, &Type)> = vars.external_vars().collect();
    let _ = ExternalVars::write_external_var;
    let _ = Value::Prim(Primitive::F32(0.));
  }

  fn screen(gpu: &mut GpuCore, events: &[WindowEvent]) {
    gpu.render_screen_to_texture((320, 240));
    let _: bool = gpu.render_frame(events);
    let _: Option<(u32, u32, Vec<u8>)> = gpu.read_screen();
    let _: (u32, u32) = gpu.window_size;
    let _ = ScreenTarget::Texture;
  }

  fn frames<D: FrameDriver>(driver: &mut D) {
    let _ = driver.io_mut();
    let _: Result<(), EvalException> = driver.run_frame();
  }

  fn audio(driver: &mut VmAudioDriver, previous: &mut VmAudioDriver) {
    let _ = VmAudioDriver::new;
    driver.run_batch(64, 48000., |sample| {}, |var| {}, |var| {});
    driver.follow(previous);
    let _: bool = driver.take_stop();
    let _ = driver.switch_entry("entry");
    driver.reset_time();
    let _ = &driver.audio_input_override;
    let _ = &driver.midi_override;
    let _ = AudioRestart::new;
    let _ = AudioRestart::driver;
    let _ = std::mem::size_of::<AudioSource>();
  }

  fn errors(error: EvalError, userspace: UserspaceEvalError) {
    match userspace {
      UserspaceEvalError::AudioRuntimeError(message) => {}
      UserspaceEvalError::AudioSourceMissing => {}
      _ => {}
    }
  }

  fn midi(state: &MidiState) {
    let _: [f32; 128] = state.cc;
    let _: u64 = state.generation;
  }

  fn io_types(
    upload: BufferUpload,
    geometry: DrawGeometry,
    binding: GpuBindingInfo,
    entry: GpuEntryInfo,
    texture: TextureHandle,
    settings: RenderSettings,
  ) {
  }
}

#[test]
fn embedder_api_is_public() {}
