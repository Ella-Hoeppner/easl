//! Audio runtime — invoked by `start-audio`.
//!
//! Two backends are available, selected via [`AudioBackend`] when the program
//! is run:
//!
//! - [`AudioBackend::VM`] (default, always available): the program's audio
//!   entry point is compiled to bytecode and the cpal callback runs
//!   [`BytecodeProgram::execute`] once per sample. Portable, no external
//!   dependencies.
//!
//! - [`AudioBackend::C`] (gated on the `c_audio` feature): the program's C
//!   backend output is JIT-compiled to a dylib via `clang`, loaded via
//!   `libloading`, and the cpal callback calls the loaded function pointer
//!   directly. Faster but requires `clang` on the host.

use std::sync::{Arc, Mutex};

use cpal::SampleRate;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

use crate::vm::bytecode::BytecodeProgram;

/// The backend used to drive the audio thread's per-sample function. Picked
/// by the caller when starting an audio-capable program. Defaults to `VM`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AudioBackend {
  /// Compile to bytecode and run the bytecode VM per sample. Default;
  /// portable; no external dependencies.
  VM,
  /// JIT-compile the C backend output to a dylib and dlopen it.
  /// Requires the `c_audio` feature and a working `clang` on the host.
  /// Selecting this without the feature enabled panics at audio startup.
  C,
}

impl Default for AudioBackend {
  fn default() -> Self {
    Self::VM
  }
}

/// The compiled artifact the interpreter hands to `start-audio` once it
/// actually fires. The bytecode variant is always available; the C variant
/// only carries usable content with the `c_audio` feature.
pub enum AudioSource {
  Bytecode {
    program: BytecodeProgram,
    function_names: Vec<Arc<str>>,
  },
  /// Pre-compiled C source string. Only constructible (and the audio path
  /// only wired up) when `c_audio` is enabled.
  C(String),
}

/// Active audio thread state. The cpal stream is `!Send`, so we stash it in a
/// Mutex on the thread that created it and never actually move it across
/// threads — the mutex is just a one-time-init slot.
struct AudioState {
  _stream: cpal::Stream,
  #[cfg(feature = "c_audio")]
  c: Option<c_audio::CAudioState>,
}

// SAFETY: AudioState is only stashed in a Mutex on the thread that created
// it; we never actually move it across thread boundaries.
unsafe impl Send for AudioState {}

static AUDIO_STATE: Mutex<Option<AudioState>> = Mutex::new(None);

/// True iff a `start_audio_thread_*` call has previously succeeded in this
/// process. Used by IO managers to decide what to do when the
/// `start-audio` builtin fires for a second time without a fresh source —
/// if the stream is already running this is a benign repeat (typical
/// "called inside the frame callback" pattern); if not, the run was
/// started without audio support.
pub fn is_audio_thread_started() -> bool {
  AUDIO_STATE.lock().unwrap().is_some()
}

/// Find the index of the audio entry function in `function_names`.
fn find_audio_fn_index(
  entry_name: &str,
  function_names: &[Arc<str>],
) -> Result<usize, String> {
  function_names
    .iter()
    .position(|n| &**n == entry_name)
    .ok_or_else(|| {
      format!("audio entry `{entry_name}` not found in compiled bytecode")
    })
}

/// Set up a cpal output stream with a callback driven by `sample_fn`.
/// Returns the `Stream` for storage in `AUDIO_STATE` (must be kept alive).
fn build_audio_stream<F>(sample_fn: F) -> Result<cpal::Stream, String>
where
  F: FnMut(f32, f32) -> f32 + Send + 'static,
{
  let host = cpal::default_host();
  let device = host
    .default_output_device()
    .ok_or_else(|| "no audio output device".to_string())?;
  let sample_rate = 44_100u32;
  let config = device
    .supported_output_configs()
    .map_err(|e| format!("query audio configs: {e}"))?
    .find(|c| {
      c.sample_format() == cpal::SampleFormat::F32
        && c.min_sample_rate() <= SampleRate(sample_rate)
        && c.max_sample_rate() >= SampleRate(sample_rate)
    })
    .ok_or_else(|| "no suitable f32@44.1kHz config".to_string())?
    .with_sample_rate(SampleRate(sample_rate));
  let channels = config.channels() as usize;

  let mut sample_fn = sample_fn;
  let mut sample_index: u64 = 0;
  let rate = sample_rate as f32;
  let stream = device
    .build_output_stream(
      &config.into(),
      move |output: &mut [f32], _: &cpal::OutputCallbackInfo| {
        for frame in output.chunks_mut(channels) {
          let t = sample_index as f32 / rate;
          sample_index = sample_index.wrapping_add(1);
          let sample = sample_fn(t, rate).clamp(-1.0, 1.0);
          for s in frame.iter_mut() {
            *s = sample;
          }
        }
      },
      |err| eprintln!("audio stream error: {err}"),
      None,
    )
    .map_err(|e| format!("build audio stream: {e}"))?;
  stream.play().map_err(|e| format!("start audio playback: {e}"))?;
  Ok(stream)
}

/// Boot the audio thread with the bytecode VM driver. The bytecode program
/// is moved into the cpal callback; subsequent calls to `start-audio` are
/// currently no-ops (logged) — we don't yet support hot-swapping the
/// underlying bytecode in the VM path.
pub fn start_audio_thread_vm(
  entry_name: &str,
  program: BytecodeProgram,
  function_names: Vec<Arc<str>>,
) -> Result<(), String> {
  let fn_index = find_audio_fn_index(entry_name, &function_names)?;
  let mut state_lock = AUDIO_STATE.lock().unwrap();
  if state_lock.is_some() {
    eprintln!(
      "Note: `start-audio` called more than once; VM-backed audio does not \
       yet support hot-swap, ignoring subsequent calls."
    );
    return Ok(());
  }
  let mut program = program;
  let return_position =
    program.get_function_return_position(fn_index) as usize;
  let sample_fn = move |t: f32, rate: f32| {
    // The audio entry has signature `(f32 t, f32 rate) -> f32`. Args live at
    // the function's stack frame start (== return_position) in slot order;
    // execute() overwrites slot[0] with the return value on the way out.
    program.stack[return_position] = t.to_bits();
    program.stack[return_position + 1] = rate.to_bits();
    program.prepare_to_run_function(fn_index);
    program.execute();
    f32::from_bits(program.stack[return_position])
  };
  let stream = build_audio_stream(sample_fn)?;
  *state_lock = Some(AudioState {
    _stream: stream,
    #[cfg(feature = "c_audio")]
    c: None,
  });
  Ok(())
}

/// Boot the audio thread with the C backend (JIT-compiled dylib). Requires
/// the `c_audio` feature.
#[cfg(feature = "c_audio")]
pub fn start_audio_thread_c(
  entry_name: &str,
  c_source: &str,
) -> Result<(), String> {
  c_audio::start_audio_thread_c(entry_name, c_source, &AUDIO_STATE)
}

#[cfg(not(feature = "c_audio"))]
pub fn start_audio_thread_c(
  _entry_name: &str,
  _c_source: &str,
) -> Result<(), String> {
  panic!(
    "AudioBackend::C was selected, but the `c_audio` cargo feature is not \
     enabled. Either enable `c_audio` (which adds a runtime dependency on \
     `clang` and `libloading`) or use AudioBackend::VM (the default)."
  )
}

#[cfg(feature = "c_audio")]
mod c_audio {
  //! C-backend audio path. Compiles the program's C source to a dylib via
  //! `clang`, dlopens it via `libloading`, and the cpal callback calls the
  //! loaded function pointer.

  use std::io::Write;
  use std::path::{Path, PathBuf};
  use std::process::Command;
  use std::sync::{Arc, Mutex};
  use std::sync::atomic::{AtomicPtr, AtomicU64, Ordering};

  use super::{AudioState, build_audio_stream};

  type AudioFn = unsafe extern "C" fn(f32, f32) -> f32;

  pub(super) struct CAudioState {
    pub _lib: libloading::Library,
    pub _counter: Arc<AtomicU64>,
    pub _fn_ptr: Arc<AtomicPtr<()>>,
  }

  fn easl_fn_name_to_c(name: &str) -> String {
    name.replace('-', "_")
  }

  fn inject_forward_decls(c_code: &str) -> String {
    let mut decls = String::new();
    for line in c_code.lines() {
      let trimmed = line.trim_start();
      if trimmed.starts_with("static")
        || trimmed.starts_with("inline")
        || trimmed.starts_with("typedef")
      {
        continue;
      }
      if !line.ends_with('{') {
        continue;
      }
      if !(line.contains('(') && line.contains(')')) {
        continue;
      }
      let decl = format!("{};\n", line.trim_end_matches('{').trim_end());
      decls.push_str(&decl);
    }
    format!("{decls}\n{c_code}")
  }

  fn compile_c_to_dylib(
    c_code: &str,
    tmp_dir: &Path,
    generation: u64,
  ) -> Result<PathBuf, String> {
    let c_path = tmp_dir.join(format!("audio_{generation}.c"));
    let dylib_name = if cfg!(target_os = "macos") {
      format!("libaudio_{generation}.dylib")
    } else if cfg!(target_os = "windows") {
      format!("audio_{generation}.dll")
    } else {
      format!("libaudio_{generation}.so")
    };
    let dylib_path = tmp_dir.join(dylib_name);
    {
      let mut f = std::fs::File::create(&c_path)
        .map_err(|e| format!("write C: {e}"))?;
      f.write_all(c_code.as_bytes())
        .map_err(|e| format!("write C: {e}"))?;
    }
    let output = Command::new("clang")
      .args([
        "-shared",
        "-fPIC",
        "-O2",
        "-lm",
        "-o",
        dylib_path.to_str().unwrap(),
        c_path.to_str().unwrap(),
      ])
      .output()
      .map_err(|e| format!("clang exec: {e}"))?;
    if !output.status.success() {
      return Err(format!(
        "clang:\n{}",
        String::from_utf8_lossy(&output.stderr)
      ));
    }
    Ok(dylib_path)
  }

  fn load_audio_fn(
    dylib_path: &Path,
    symbol_name: &str,
  ) -> Result<(libloading::Library, AudioFn), String> {
    let lib = unsafe { libloading::Library::new(dylib_path) }
      .map_err(|e| format!("load dylib: {e}"))?;
    let raw_fn = unsafe {
      let sym: libloading::Symbol<AudioFn> = lib
        .get(symbol_name.as_bytes())
        .map_err(|e| format!("symbol `{symbol_name}`: {e}"))?;
      *sym
    };
    Ok((lib, raw_fn))
  }

  pub(super) fn start_audio_thread_c(
    entry_name: &str,
    c_source: &str,
    audio_state: &Mutex<Option<AudioState>>,
  ) -> Result<(), String> {
    let tmp_dir = std::env::temp_dir().join("easl_audio");
    std::fs::create_dir_all(&tmp_dir)
      .map_err(|e| format!("create tmp dir: {e}"))?;
    let c_symbol = easl_fn_name_to_c(entry_name);
    let c_with_decls = inject_forward_decls(c_source);

    let mut state_lock = audio_state.lock().unwrap();

    if let Some(existing) = state_lock.as_ref() {
      // Hot-swap the function pointer in the already-running stream.
      let Some(c_state) = &existing.c else {
        eprintln!(
          "Note: `start-audio` called with the C backend, but the audio \
           thread was previously started via the VM backend. Ignoring."
        );
        return Ok(());
      };
      let generation = c_state._counter.load(Ordering::Relaxed);
      let dylib_path =
        compile_c_to_dylib(&c_with_decls, &tmp_dir, generation + 1)?;
      let (lib, new_fn) = load_audio_fn(&dylib_path, &c_symbol)?;
      c_state
        ._fn_ptr
        .store(new_fn as *mut (), Ordering::Release);
      c_state._counter.store(0, Ordering::Relaxed);
      // Leak the old library — code from it may be executing on the audio
      // thread; the stream's lifetime is the lifetime of the process.
      Box::leak(Box::new(lib));
      return Ok(());
    }

    let dylib_path = compile_c_to_dylib(&c_with_decls, &tmp_dir, 0)?;
    let (lib, raw_fn) = load_audio_fn(&dylib_path, &c_symbol)?;

    let fn_ptr: Arc<AtomicPtr<()>> =
      Arc::new(AtomicPtr::new(raw_fn as *mut ()));
    let cb_fn_ptr = fn_ptr.clone();
    let sample_fn = move |t: f32, rate: f32| {
      let fp = cb_fn_ptr.load(Ordering::Acquire);
      let func: AudioFn = unsafe { std::mem::transmute(fp) };
      unsafe { func(t, rate) }
    };
    let stream = build_audio_stream(sample_fn)?;
    let counter = Arc::new(AtomicU64::new(0));
    *state_lock = Some(AudioState {
      _stream: stream,
      c: Some(CAudioState {
        _lib: lib,
        _counter: counter,
        _fn_ptr: fn_ptr,
      }),
    });
    Ok(())
  }
}
