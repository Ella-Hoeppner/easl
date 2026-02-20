use easl::{
  compile_easl_source_to_wgsl,
  compiler::{builtins::built_in_macros, program::Program},
  interpreter::run_program_entry,
  parse::parse_easl_without_comments,
};
use std::fs;

fn _benchmark_wgsl_compilation() {
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

fn run_demo(file_name: &str, entry: Option<&str>) {
  let src = fs::read_to_string(format!("./data/window/{file_name}.easl"))
    .unwrap_or_else(|_| panic!("Unable to read data/window/{file_name}.easl"));
  let document = parse_easl_without_comments(&src);
  let (mut program, errors) =
    Program::from_easl_document(&document, built_in_macros());
  if !errors.is_empty() {
    panic!("{errors:?}");
  }
  let errors = program.validate_raw_program();
  if !errors.is_empty() {
    panic!("{errors:?}");
  }
  run_program_entry(program, entry).unwrap();
}

fn main() {
  unsafe {
    std::env::set_var("RUST_BACKTRACE", "1");
  }
  // benchmark_wgsl_compilation();
  run_demo("global_frame_counter", None)
}
