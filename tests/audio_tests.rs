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
    [1.0f32, 2.0, 3.0, 4.0].iter().map(|s| s.to_bits()).collect(),
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
  audio_program.adopt_shared(&table, participant::AUDIO, |i| {
    adopted.push(i)
  });
  assert_eq!(adopted, vec![0, 1]);

  let f_index = audio_names.iter().position(|n| &**n == "f").unwrap();
  audio_program.prepare_to_run_function(f_index);
  audio_program.execute();
  let slot = audio_program.get_function_return_position(f_index);
  let result = f32::from_bits(audio_program.stack[slot as usize]);
  // gain (0.75) * sample[2] (3.0)
  assert_eq!(result, 2.25);
}
