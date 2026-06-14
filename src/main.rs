use easl::{
  compile_easl_source_to_wgsl,
  compiler::{core::load_easl_program_from_file, program::CompilerTarget},
  interpreter::{
    StdoutIO, run_program_entry,
    run_program_entry_with_io_and_audio_c_source_from_path,
  },
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
  let audio_c_source = compile_to_c_if_audio(program.clone());
  run_program_entry_with_io_and_audio_c_source_from_path(
    wgsl_program,
    entry,
    StdoutIO::new(),
    path,
    audio_c_source,
  )
  .unwrap();
}

#[allow(unused)]
fn compile_to_c_if_audio(
  mut program: easl::compiler::program::Program,
) -> Option<String> {
  let errors = program.validate_raw_program(CompilerTarget::C);
  if !errors.is_empty() {
    // If the C-target validation fails, we still want the WGSL run to
    // succeed; the C source just isn't available and any start-audio call
    // will surface an error.
    eprintln!("C-target validation failed (audio will be unavailable):");
    return None;
  }
  match program.compile_to_target(CompilerTarget::C) {
    Ok(c_source) => Some(c_source),
    Err(e) => {
      eprintln!("C-target compilation failed: {e}");
      None
    }
  }
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
