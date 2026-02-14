use easl::{
  compile_easl_source_to_wgsl,
  compiler::program::{CompilerTarget, Program},
  interpreter::run_program,
  parse::parse_easl_without_comments,
};
use std::fs;

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

fn _cpu_examples() {
  unsafe {
    std::env::set_var("RUST_BACKTRACE", "1");
  }
  fs::create_dir_all("./out/").expect("Unable to create out directory");
  for filename in [
    "print",
    "def",
    "assignment",
    "field_assignment",
    "for",
    "while",
    "fn",
    "struct",
    "enum",
  ] {
    print!("running {filename}...");
    let easl_source =
      fs::read_to_string(&format!("./data/cpu/{filename}.easl"))
        .expect(&format!("Unable to read {filename}.easl"));
    let (mut program, errors) = Program::from_easl_document(
      &parse_easl_without_comments(&easl_source),
      easl::compiler::builtins::built_in_macros(),
    );
    if errors.is_empty() {
      let errors = program.validate_raw_program(CompilerTarget::CPU);
      if errors.is_empty() {
        match run_program(program) {
          Ok(_) => {
            println!("finished running successfully");
          }
          Err(err) => println!("encountered evaluation error: {err:#?}"),
        }
      } else {
        println!("failed to run {filename}:\n{errors:#?}");
      }
    } else {
      println!("failed to run {filename}:\n{errors:#?}");
    }
  }
}

fn main() {
  benchmark_wgsl_compilation();
}
