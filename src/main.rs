use easl::compile_easl_source_to_wgsl;
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

fn main() {
  benchmark_wgsl_compilation();
}
