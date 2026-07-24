use easl::compiler::core::load_easl_program_from_file;
use easl::compiler::program::CompilerTarget;
use easl::interpreter::CpuRuntime;
use std::path::Path;

fn main() {
  let path_str = std::env::args().nth(1).unwrap();
  let path = Path::new(&path_str);
  let Ok(Ok((_, Ok(mut program)))) = load_easl_program_from_file(path) else {
    panic!("load failed");
  };
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  assert!(errors.is_empty(), "compile errors: {errors:#?}");
  let prints = easl::interpreter::run_program_with_capture_and_runtime_from_path(
    program,
    path,
    CpuRuntime::BytecodeVm,
  )
  .unwrap();
  println!("VM PRINTS: {prints:?}");
}
