use easl::compiler::core::load_easl_program_from_file;
use easl::compiler::program::CompilerTarget;
use easl::interpreter::{
  CaptureIO, CpuRuntime, run_program_entry_with_io_and_runtime_from_path,
};
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
