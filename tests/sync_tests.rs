use easl::compiler::core::load_easl_program_from_file;
use easl::compiler::program::CompilerTarget;
use easl::interpreter::{
  CpuRuntime, run_program_capturing_io_with_runtime_from_path,
};
use std::fs;
use std::path::Path;

// Implicit GPU↔CPU sync behavior tests.
//
// Easl's coherence model is implicit: programs freely mix CPU and GPU access
// to the same globals with no explicit transfer calls, and blocking GPU→CPU
// readbacks are the most expensive implicit operation in the runtime. These
// tests assert exactly when transfers happen — both that spurious ones don't
// and that genuine ones still do. Each test runs data/sync/<name>.easl on
// the real GPU and golden-matches the ordered trace of uploads, readbacks,
// and prints against data/sync/<name>.txt (`upload: <var>` /
// `readback: <var>` / `print: <text>`, one per line).

fn run_sync_test(name: &str) {
  let expected = fs::read_to_string(format!("./data/sync/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/sync/{name}.txt"));
  let source_path_str = format!("./data/sync/{name}.easl");
  let source_path = Path::new(&source_path_str);
  match load_easl_program_from_file(source_path) {
    Ok(Ok((_, Ok(mut program)))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");
      // Both CPU runtimes must produce the identical transfer trace.
      run_sync_test_on(name, program.clone(), source_path, &expected, CpuRuntime::TreeWalking);
      run_sync_test_on(name, program, source_path, &expected, CpuRuntime::BytecodeVm);
    }
    Ok(Ok((document, Err(errors)))) => {
      panic!("{name}: {}", errors.describe(&document));
    }
    _ => panic!("{name}: couldn't load program"),
  }
}

fn run_sync_test_on(
  name: &str,
  program: easl::compiler::program::Program,
  source_path: &Path,
  expected: &str,
  runtime: CpuRuntime,
) {
  {
    {
      let io = run_program_capturing_io_with_runtime_from_path(
        program,
        source_path,
        runtime,
      )
      .unwrap_or_else(|e| {
        panic!("{name}: evaluation error ({runtime:?}): {e:#?}")
      });
      // The scope bindings that `extract_dispatched_closure_scopes` creates
      // for dispatched closures have gensym'd names whose numbering isn't
      // stable across runs — normalize them so traces stay deterministic.
      let actual: String = io
        .sync_trace
        .iter()
        .map(|line| {
          if line.ends_with("_scope_data")
            && let Some(direction) = line.split(": ").next()
          {
            format!("{direction}: <closure-scope>\n")
          } else {
            format!("{line}\n")
          }
        })
        .collect();
      assert_eq!(
        actual,
        *expected,
        "{name}: sync trace mismatch ({runtime:?})"
      );
    }
  }
}

macro_rules! sync_test {
  ($name:ident) => {
    #[test]
    fn $name() {
      run_sync_test(stringify!($name));
    }
  };
}

// Data flows GPU→GPU only (dispatch 2 reads what dispatch 1 wrote): the
// array uploads exactly once and no GPU→CPU readback ever happens.
sync_test!(gpu_only_dataflow_no_readback);
// Same, but with the dispatch wrapped in a higher-order helper and the
// callback capturing a local — the realistic sketch shape. The captured
// scopes travel CPU→GPU as implicit bindings; nothing ever comes back.
sync_test!(gpu_only_dataflow_closure_no_readback);
// Positive control: reading an element of a GPU-written array on the CPU
// syncs exactly once, before the first print consumes the value.
sync_test!(cpu_read_after_dispatch_syncs);
// Positive control: the CPU read happening inside a called function (rather
// than directly in the entry body) still triggers the sync.
sync_test!(cpu_fn_read_after_dispatch_syncs);
// `(array-length data)` in dispatch-count position must not read the array
// back from the GPU: the GPU can never resize a buffer, so lengths are
// CPU-authoritative even when the elements are GPU-dirty. Only the final
// element read at the end should sync.
sync_test!(array_length_no_readback);
// Two dispatches of the same entry queued within one frame, each carrying a
// different captured-scope value, must run with their own values: the
// windowed frame path may not collapse per-dispatch uploads of the same
// binding into one before running the passes.
sync_test!(windowed_scope_upload_ordering);
