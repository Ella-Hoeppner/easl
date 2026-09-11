//! Video-input tests. Gated behind the `video` feature (needs the `ffmpeg`
//! binary on PATH) and run against both CPU runtimes, exactly like the cpu
//! suite. Sources + `.mp4` fixtures + expected output live in `data/video/`.
#![cfg(feature = "video")]

use easl::compiler::core::{
  compile_easl_file_to_wgsl, load_easl_program_from_file,
};
use easl::compiler::program::CompilerTarget;
use easl::interpreter::{
  CpuRuntime, run_program_capturing_output_with_runtime,
};
use std::fs;
use std::path::Path;

fn run_video_test(name: &str) {
  let expected = fs::read_to_string(format!("./data/video/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/video/{name}.txt"));
  let loaded = load_easl_program_from_file(Path::new(&format!(
    "./data/video/{name}.easl"
  )));
  let Ok(Ok((_, Ok(mut program)))) = loaded else {
    panic!("{name}: failed to load/parse program: {loaded:#?}");
  };
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

  let output = run_program_capturing_output_with_runtime(
    program.clone(),
    CpuRuntime::TreeWalking,
  )
  .unwrap_or_else(|e| {
    panic!("{name}: evaluation error (tree-walking): {e:#?}")
  });
  assert_eq!(output, expected, "{name}: output mismatch (tree-walking)");

  let vm_output =
    run_program_capturing_output_with_runtime(program, CpuRuntime::BytecodeVm)
      .unwrap_or_else(|e| {
        panic!("{name}: evaluation error (bytecode VM): {e:#?}")
      });
  assert_eq!(vm_output, expected, "{name}: output mismatch (bytecode VM)");
}

macro_rules! video_test {
  ($name:ident) => {
    #[test]
    fn $name() {
      run_video_test(stringify!($name));
    }
  };
}

video_test!(scrub);
video_test!(decode);

/// The playback demo (`data/video/demo.easl`) is a full windowed program: its
/// GPU entries must emit WGSL that naga accepts. This pins that a `Video`
/// captured by the frame closure doesn't leak into (or dangle in) the shader
/// output — the scope struct embedding it is correctly treated as CPU-only.
#[test]
fn demo_shaders_validate() {
  let wgsl =
    match compile_easl_file_to_wgsl(Path::new("./data/video/demo.easl")) {
      Ok(Ok(Ok(wgsl))) => wgsl,
      other => panic!("demo.easl failed to compile to WGSL: {other:#?}"),
    };
  let module = naga::front::wgsl::parse_str(&wgsl)
    .unwrap_or_else(|e| panic!("naga failed to parse demo WGSL:\n{e}\n{wgsl}"));
  naga::valid::Validator::new(
    naga::valid::ValidationFlags::all(),
    naga::valid::Capabilities::all(),
  )
  .validate(&module)
  .unwrap_or_else(|e| {
    panic!("naga validation failed on demo WGSL:\n{e}\n{wgsl}")
  });
}

video_test!(frame_loop);
