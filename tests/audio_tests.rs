mod common;

use easl::compiler::core::load_easl_program_from_file;
use easl::compiler::program::CompilerTarget;
use easl::interpreter::{
  CaptureIO, CpuRuntime, run_program_entry_with_io_and_runtime_from_path,
};
use easl::thread_sync::participant;
use std::fs;
use std::path::Path;

/// Runs `data/audio/<name>.easl` through the real from-path run entry point
/// — the path the CLI takes, including the eager audio-source compilation
/// that happens whenever the program has an `@audio` entry point — and
/// compares captured `(print ...)` output against `data/audio/<name>.txt`.
///
/// This is the only suite that exercises audio-source compilation: the
/// macro runners behind the cpu/buffer/window suites take a source path but
/// use it only for the source *dir*, so they never compile the audio
/// source at all. Anything about the audio runtime worth pinning belongs
/// here.
fn run_audio_test(name: &str) {
  let expected = fs::read_to_string(format!("./data/audio/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/audio/{name}.txt"));

  let source_path_str = format!("./data/audio/{name}.easl");
  let source_path = Path::new(&source_path_str);

  match load_easl_program_from_file(source_path) {
    Ok(Ok((_, Ok(mut program)))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");
      common::assert_valid_wgsl(&program);

      // Every test runs on both CPU runtimes and must produce identical
      // output on each.
      let (io, _) = run_program_entry_with_io_and_runtime_from_path(
        program.clone(),
        None,
        CaptureIO::new(),
        source_path,
        CpuRuntime::TreeWalking,
      )
      .unwrap_or_else(|e| {
        panic!("{name}: evaluation error (tree-walking): {e:#?}");
      });
      let output: String =
        io.prints.into_iter().map(|s| format!("{s}\n")).collect();
      assert_eq!(output, expected, "{name}: output mismatch (tree-walking)");

      let (vm_io, _) = run_program_entry_with_io_and_runtime_from_path(
        program,
        None,
        CaptureIO::new(),
        source_path,
        CpuRuntime::BytecodeVm,
      )
      .unwrap_or_else(|e| {
        panic!("{name}: evaluation error (bytecode VM): {e:#?}");
      });
      let vm_output: String =
        vm_io.prints.into_iter().map(|s| format!("{s}\n")).collect();
      assert_eq!(vm_output, expected, "{name}: output mismatch (bytecode VM)");
    }
    Ok(Ok((document, Err(errors)))) => {
      panic!("{}", errors.describe(&document));
    }
    Ok(Err(_)) => panic!("{name}: parse error"),
    Err(e) => panic!("{name}: io error: {e:#?}"),
  }
}

macro_rules! audio_test {
  ($name:ident) => {
    #[test]
    fn $name() {
      run_audio_test(stringify!($name));
    }
  };
}

audio_test!(audio_entry_with_dynamic_global);
audio_test!(load_wav);
audio_test!(load_wav_dynamic_path);
audio_test!(unreachable_hof_returning_closure);

#[test]
fn start_audio_bootstrap_publishes_current_globals() {
  // `start-audio` activates the shared-variable table and force-publishes
  // every thread-shared global from the main replica, so the audio
  // replica's first batch-boundary adopt sees the main side's current
  // values — slot-backed globals and runtime-sized arrays alike. This
  // drives the publish/adopt pair directly (compiling the same program in
  // cpu mode and audio mode) rather than through `start-audio`, which
  // would open a real audio stream.
  use easl::thread_sync::ThreadSharedTable;
  use easl::vm::bytecode::DynMemory;
  let source_path = Path::new("./data/audio/copy_globals.easl");
  let Ok(Ok((_, Ok(mut program)))) = load_easl_program_from_file(source_path)
  else {
    panic!("failed to load program");
  };
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  assert!(errors.is_empty(), "compile errors: {errors:#?}");
  common::assert_valid_wgsl(&program);
  let (mut audio_program, audio_names) =
    program.clone().compile_to_bytecode_program();
  let (mut main_program, _) = program.compile_to_bytecode_program_cpu();

  // both globals are read by the audio entry and written by main, so the
  // static analysis must classify both as shared, in sorted order, in both
  // artifacts
  let shared_names = |code: &easl::vm::bytecode::Code| {
    code
      .shared_vars
      .iter()
      .map(|info| info.name.to_string())
      .collect::<Vec<_>>()
  };
  assert_eq!(shared_names(&main_program.code), vec!["gain", "sample"]);
  assert_eq!(shared_names(&audio_program.code), vec!["gain", "sample"]);

  // simulate what the cpu program would have computed by start-audio time
  main_program.write_global("gain", &[0.75f32.to_bits()]);
  let (region, _) = main_program.get_dyn_memory_region("sample").unwrap();
  main_program.dyn_memory[region as usize] = DynMemory::Words(
    [1.0f32, 2.0, 3.0, 4.0]
      .iter()
      .map(|s| s.to_bits())
      .collect(),
  );

  // the start-audio bootstrap: the audio participant joins and main
  // force-publishes everything in its audience, then the audio replica's
  // first boundary adopts everything
  let table = ThreadSharedTable::new(main_program.code.shared_vars.len());
  table.join(participant::AUDIO);
  let mut published = Vec::new();
  main_program.publish_shared(
    &table,
    participant::MAIN,
    participant::AUDIO,
    |i| published.push(i),
  );
  assert_eq!(published, vec![0, 1]);
  let mut adopted = Vec::new();
  audio_program.adopt_shared(&table, participant::AUDIO, |i| adopted.push(i));
  assert_eq!(adopted, vec![0, 1]);

  let f_index = audio_names.iter().position(|n| &**n == "f").unwrap();
  audio_program.prepare_to_run_function(f_index);
  audio_program.execute();
  let slot = audio_program.get_function_return_position(f_index);
  let result = f32::from_bits(audio_program.stack[slot as usize]);
  // gain (0.75) * sample[2] (3.0)
  assert_eq!(result, 2.25);
}

#[test]
fn vm_audio_driver_entry_switch() {
  // `switch-entry` re-points a running driver at a different entry in the
  // same compiled program — the mechanism behind repeated `start-audio`
  // calls with a different function within one run — preserving program
  // state and the sample position. Driven directly on the driver rather
  // than through `start-audio`, which would open a real audio stream.
  use easl::audio::VmAudioDriver;
  let source_path = Path::new("./data/audio/entry_switch.easl");
  let Ok(Ok((_, Ok(mut program)))) = load_easl_program_from_file(source_path)
  else {
    panic!("failed to load program");
  };
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  assert!(errors.is_empty(), "compile errors: {errors:#?}");
  common::assert_valid_wgsl(&program);
  let (program, names) = program.compile_to_bytecode_program();

  let mut driver = VmAudioDriver::new("f", program, &names, None).unwrap();
  let rate = 8.0;
  let mut samples = Vec::new();
  driver.run_batch(4, rate, |s| samples.push(s), |_| {}, |_| {});
  assert_eq!(samples, vec![0.0, 0.125, 0.25, 0.375]);

  // t keeps advancing across the switch: `g` negates it
  driver.switch_entry("g").unwrap();
  samples.clear();
  driver.run_batch(4, rate, |s| samples.push(s), |_| {}, |_| {});
  assert_eq!(samples, vec![-0.5, -0.625, -0.75, -0.875]);

  // ...and back, still without resetting the sample position
  driver.switch_entry("f").unwrap();
  samples.clear();
  driver.run_batch(2, rate, |s| samples.push(s), |_| {}, |_| {});
  assert_eq!(samples, vec![1.0, 1.0]); // t = 1.0, 1.125, clamped

  assert!(driver.switch_entry("no-such-entry").is_err());
}

/// An audio program round-tripped through `Code::to_bytes` /
/// `from_bytes` (how the web runtime hands its page's compilation to the
/// audio worklet) renders the same samples and publishes the same shared
/// snapshots as the original, including a heap-involving shared var whose
/// wire encoding walks the serialized value layouts.
#[test]
fn serialized_audio_program() {
  use easl::audio::VmAudioDriver;
  use easl::thread_sync::ThreadSharedTable;
  use easl::vm::bytecode::{BytecodeProgram, Code};
  use std::sync::Arc;
  let source_path = Path::new("./data/audio/serialized_audio_program.easl");
  let Ok(Ok((_, Ok(mut program)))) = load_easl_program_from_file(source_path)
  else {
    panic!("failed to load program");
  };
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  assert!(errors.is_empty(), "compile errors: {errors:#?}");
  common::assert_valid_wgsl(&program);
  let (original, names) = program.compile_to_bytecode_program();
  let entry = names
    .iter()
    .find(|name| name.ends_with("_audio"))
    .expect("no audio clone of the closure entry")
    .to_string();
  let bytes = original.code.to_bytes();
  let copy = BytecodeProgram::from_code(Code::from_bytes(&bytes).unwrap());
  let shared_names = |program: &BytecodeProgram| -> Vec<Arc<str>> {
    program
      .code
      .shared_vars
      .iter()
      .map(|v| v.name.clone())
      .collect()
  };
  assert_eq!(shared_names(&original), shared_names(&copy));
  assert!(!original.code.shared_vars.is_empty());

  let run = |program: BytecodeProgram| {
    let table =
      Arc::new(ThreadSharedTable::new(program.code.shared_vars.len()));
    table.join(participant::AUDIO);
    let mut driver =
      VmAudioDriver::new(&entry, program, &names, Some(table.clone())).unwrap();
    let mut trace = vec![];
    for _ in 0..3 {
      let mut samples = vec![];
      driver.run_batch(4, 8.0, |s| samples.push(s), |_| {}, |_| {});
      let snapshots: Vec<Vec<u32>> = table
        .slots
        .iter()
        .map(|slot| {
          slot
            .adopt_if_newer(0)
            .map(|snapshot| snapshot.words.clone())
            .unwrap_or_default()
        })
        .collect();
      trace.push((samples, snapshots));
    }
    trace
  };
  let original_trace = run(original);
  assert_eq!(original_trace, run(copy));
  // the bank really went through the wire encoding: starting empty on the
  // audio replica (nothing seeded it), 12 samples pushed arrays of 0..12
  // zeroed elements, each count-prefixed, after the bank's own count
  let bank_words = &original_trace[2].1[0];
  assert_eq!(bank_words[0], 12);
  assert_eq!(bank_words.len(), 1 + (0..12).map(|n| 1 + n).sum::<usize>());
}

/// The audio thread iterating `down-midi-notes` across batches whose
/// MIDI state CHANGES — notes pressed and released between callbacks
/// (each change bumps the snapshot generation, so the driver rewrites
/// the down-notes region). Mimics a real controller session; drives
/// `VmAudioDriver` directly rather than opening a cpal stream.
#[test]
fn midi_down_notes_state_changes() {
  use easl::audio::VmAudioDriver;
  use easl::interpreter::{MidiNoteState, MidiState};
  use std::sync::Arc;
  let source_path = Path::new("./data/audio/midi_down_notes_voice.easl");
  let Ok(Ok((_, Ok(mut program)))) = load_easl_program_from_file(source_path)
  else {
    panic!("failed to load program");
  };
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  assert!(errors.is_empty(), "compile errors: {errors:#?}");
  common::assert_valid_wgsl(&program);
  let (audio_program, audio_names) =
    program.clone().compile_to_bytecode_program();
  let mut driver =
    VmAudioDriver::new("voice", audio_program, &audio_names, None)
      .expect("failed to build audio driver");

  let note = |note: u32, velocity: f32, aftertouch: f32| MidiNoteState {
    note,
    velocity,
    aftertouch,
  };
  let mut midi = MidiState::default();
  let mut generation = 0u64;
  let mut states: Vec<Arc<MidiState>> = vec![];
  // batch 1: silence; batch 2: two notes; batch 3: one released, the
  // other gaining aftertouch; batch 4: all released; batch 5: a new note
  states.push(Arc::new(midi.clone()));
  midi.down_notes = vec![note(60, 1., 0.), note(64, 0.5, 0.)];
  generation += 1;
  midi.generation = generation;
  states.push(Arc::new(midi.clone()));
  midi.down_notes = vec![note(60, 1., 0.5)];
  generation += 1;
  midi.generation = generation;
  states.push(Arc::new(midi.clone()));
  midi.down_notes = vec![];
  generation += 1;
  midi.generation = generation;
  states.push(Arc::new(midi.clone()));
  midi.down_notes = vec![note(72, 1., 0.)];
  generation += 1;
  midi.generation = generation;
  states.push(Arc::new(midi.clone()));

  let expected_per_batch: Vec<f32> =
    vec![0., 60. * 0.01 + 64. * 0.005, 1.5 * 60. * 0.01, 0., 0.72];
  for (state, expected) in states.into_iter().zip(expected_per_batch) {
    driver.midi_override = Some(state);
    let mut samples: Vec<f32> = vec![];
    driver.run_batch(4, 8., |s| samples.push(s), |_| {}, |_| {});
    for sample in samples {
      assert!(
        (sample - expected).abs() < 1e-6,
        "expected {expected}, got {sample}"
      );
    }
  }
}

/// Two audio drivers sharing atomics through one table, each running on
/// its own real thread: every atomic op is a single atomic operation on
/// the shared word, so no concurrent update is lost.
#[test]
fn shared_atomic_contention() {
  use easl::audio::VmAudioDriver;
  use easl::thread_sync::ThreadSharedTable;
  use easl::vm::bytecode::{BytecodeProgram, Code};
  use std::sync::Arc;
  let source_path = Path::new("./data/audio/shared_atomic_contention.easl");
  let Ok(Ok((_, Ok(mut program)))) = load_easl_program_from_file(source_path)
  else {
    panic!("failed to load program");
  };
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  assert!(errors.is_empty(), "compile errors: {errors:#?}");
  common::assert_valid_wgsl(&program);
  let (audio_program, names) = program.compile_to_bytecode_program();
  assert!(audio_program.code.shared_vars.iter().all(|v| v.atomic));
  let table =
    Arc::new(ThreadSharedTable::new(audio_program.code.shared_vars.len()));
  table.join(participant::AUDIO);
  const BATCHES: usize = 2000;
  const FRAMES: usize = 64;
  let threads: Vec<_> = ["up", "down"]
    .into_iter()
    .map(|entry| {
      let copy = BytecodeProgram::from_code(
        Code::from_bytes(&audio_program.code.to_bytes()).unwrap(),
      );
      let mut driver =
        VmAudioDriver::new(entry, copy, &names, Some(table.clone())).unwrap();
      std::thread::spawn(move || {
        for _ in 0..BATCHES {
          driver.run_batch(FRAMES, 44100.0, |_| {}, |_| {}, |_| {});
        }
      })
    })
    .collect();
  for thread in threads {
    thread.join().unwrap();
  }
  let samples = (BATCHES * FRAMES) as u32;
  let words = |name: &str| -> Vec<u32> {
    let index = audio_program
      .code
      .shared_vars
      .iter()
      .position(|v| &*v.name == name)
      .unwrap();
    let words = audio_program.code.shared_vars[index].layout.words() as usize;
    let atomic_words = table.slots[index]
      .atomic_words(words, || unreachable!("the drivers made the words"));
    (0..words).map(|word| atomic_words.load(word)).collect()
  };
  assert_eq!(words("counter"), vec![3 * samples]);
  assert_eq!(words("hits"), vec![3, (-(samples as i32)) as u32]);
}
