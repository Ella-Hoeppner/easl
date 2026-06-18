#[cfg(feature = "window")]
use easl::interpreter::{
  StdoutIO, run_program_entry_with_io_and_audio_backend_from_path,
};
use easl::{
  compile_easl_source_to_wgsl,
  compiler::{core::load_easl_program_from_file, program::CompilerTarget},
  interpreter::run_program_entry,
};
use std::fs;

#[allow(unused)]
fn benchmark_wgsl_compilation() {
  let mut entries: Vec<_> = fs::read_dir("./data/gpu/")
    .expect("Unable to read data/gpu/ directory")
    .filter_map(|e| e.ok())
    .filter(|e| e.path().extension().is_some_and(|ext| ext == "easl"))
    .collect();
  entries.sort_by_key(|e| e.file_name());

  let mut total_time = 0f64;
  for entry in &entries {
    let filename = entry.file_name();
    let name = filename.to_string_lossy();
    let easl_source = fs::read_to_string(entry.path())
      .unwrap_or_else(|_| panic!("Unable to read {name}"));
    print!("{name}...");
    let t = std::time::Instant::now();
    let _ = compile_easl_source_to_wgsl(&easl_source);
    let elapsed = t.elapsed();
    total_time += elapsed.as_secs_f64() * 1000.0;
    println!("{elapsed:?}");
  }
  println!("\n{} files, total: {total_time:.1}ms", entries.len());
}

#[cfg(feature = "window")]
#[allow(unused)]
fn run_window_demo(file_name: &str, entry: Option<&str>) {
  let path_str = format!("./data/window/{file_name}.easl");
  let path = std::path::Path::new(&path_str);
  let (_, program_or_errors) = load_easl_program_from_file(path)
    .unwrap_or_else(|e| panic!("IO error: {e}"))
    .unwrap_or_else(|_| panic!("Parse error in {file_name}"));
  let program = program_or_errors.unwrap_or_else(|errors| panic!("{errors:?}"));
  // Compile two copies: one for the interpreter (WGSL), one for the audio
  // thread (C). Both starts run on a fresh clone so per-target validation
  // mutations don't conflict.
  let mut wgsl_program = program.clone();
  let wgsl_errors = wgsl_program.validate_raw_program(CompilerTarget::WGSL);
  if !wgsl_errors.is_empty() {
    panic!("{wgsl_errors:?}");
  }
  // Audio (if used) is driven by the bytecode VM by default — no external
  // toolchain needed. To use the C backend instead, build with
  // `--features c_audio` and pass `AudioBackend::C` below.
  let audio_backend = easl::audio::AudioBackend::default();
  run_program_entry_with_io_and_audio_backend_from_path(
    wgsl_program,
    entry,
    StdoutIO::new(),
    path,
    audio_backend,
  )
  .unwrap();
}

#[allow(unused)]
fn run_buffer_demo(file_name: &str, entry: Option<&str>) {
  let path_str = format!("./data/buffer/{file_name}.easl");
  let (_, program_or_errors) =
    load_easl_program_from_file(std::path::Path::new(&path_str))
      .unwrap_or_else(|e| panic!("IO error: {e}"))
      .unwrap_or_else(|_| panic!("Parse error in {file_name}"));
  let mut program =
    program_or_errors.unwrap_or_else(|errors| panic!("{errors:?}"));
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  if !errors.is_empty() {
    panic!("{errors:?}");
  }
  run_program_entry(program, entry).unwrap();
}

fn main() {
  unsafe {
    std::env::set_var("RUST_BACKTRACE", "1");
  }
  benchmark_wgsl_compilation();
  // run_window_demo("print_after_close_window", None)
  // run_buffer_demo("array_assignment_cross_window", None)
}
