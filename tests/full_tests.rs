#[cfg(not(feature = "window"))]
fn main() {}

#[cfg(feature = "window")]
use easl::compiler::core::load_easl_program_from_file;
#[cfg(feature = "window")]
use easl::interpreter::{
  StdoutIO, close_persistent_window, run_program_entry_with_io_from_path,
};
#[cfg(feature = "window")]
use std::path::Path;
#[cfg(feature = "window")]
use std::sync::Arc;
#[cfg(feature = "window")]
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(feature = "window")]
/// Runs an .easl program from `data/full/{name}.easl` exactly the way the CLI
/// does: real GPU, real window, real `StdoutIO`.  A pre-set reload flag causes
/// the window to close automatically after the first frame so the test doesn't
/// hang.
fn run_full_test(name: &str) {
  let source_path_str = format!("./data/full/{name}.easl");
  let source_path = Path::new(&source_path_str);

  match load_easl_program_from_file(source_path) {
    Ok(Ok((_, Ok(mut program)))) => {
      use easl::compiler::program::CompilerTarget;

      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

      // Use a pre-set reload flag so the window auto-closes after the first
      // frame, exactly like a hot-reload.  This lets the test exercise the
      // full StdoutIO path (headless GPU → window surface) without blocking.
      let reload_flag = Arc::new(AtomicBool::new(true));
      let mut io = StdoutIO::with_reload_flag(Arc::clone(&reload_flag));
      // Small window, don't steal focus.
      io.set_window_hints((64, 64), false);

      let result =
        run_program_entry_with_io_from_path(program, None, io, source_path);

      // Clean up the persistent window state so subsequent tests can
      // create fresh windows.
      close_persistent_window();

      // Reset the reload flag so it doesn't leak into the next test.
      reload_flag.store(false, Ordering::Relaxed);

      match result {
        Ok((_io, _did_reload)) => {
          // Success — the program ran through without crashing.
        }
        Err(e) => panic!("{name}: runtime error: {e:#?}"),
      }
    }
    Ok(Ok((document, Err(errors)))) => {
      panic!("{name}: compile errors:\n{}", errors.describe(&document));
    }
    Ok(Err(mut failed_documents)) => {
      let mut errors = vec![];
      std::mem::swap(
        &mut errors,
        &mut failed_documents
          .sources
          .last_mut()
          .unwrap()
          .0
          .parsing_failures,
      );
      let description = errors
        .into_iter()
        .map(|err| failed_documents.describe_parse_error(err))
        .collect::<Vec<String>>()
        .join("\n\n");
      panic!("Unexpected parse error in {name}:\n{description}");
    }
    Err(e) => panic!("IO error, couldn't load file {name}: \n{e:?}"),
  }
}

#[cfg(feature = "window")]
/// Custom harness: runs each test on the main thread (required on macOS where
/// winit's EventLoop must be created on the main thread).
fn main() {
  let tests: &[(&str, fn())] = &[("problem", || run_full_test("problem"))];

  let args: Vec<String> = std::env::args().collect();
  // Minimal filter support: if a positional arg is given after `--`,
  // only run tests whose name contains that substring.
  let filter = args
    .iter()
    .position(|a| a == "--")
    .and_then(|i| args.get(i + 1))
    .map(|s| s.as_str());

  let mut passed = 0;
  let mut failed = 0;
  let mut failed_names = vec![];

  for (name, f) in tests {
    if let Some(filter) = filter {
      if !name.contains(filter) {
        continue;
      }
    }
    eprint!("test {name} ... ");
    // Catch panics so one failure doesn't abort the rest.
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
      Ok(()) => {
        eprintln!("ok");
        passed += 1;
      }
      Err(_) => {
        eprintln!("FAILED");
        failed += 1;
        failed_names.push(*name);
      }
    }
  }

  eprintln!();
  if !failed_names.is_empty() {
    eprintln!("failures:");
    for name in &failed_names {
      eprintln!("    {name}");
    }
    eprintln!();
  }
  eprintln!(
    "test result: {}. {passed} passed; {failed} failed",
    if failed == 0 { "ok" } else { "FAILED" }
  );

  if failed > 0 {
    std::process::exit(1);
  }
}
