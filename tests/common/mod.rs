//! Helpers shared by the integration test suites.

use easl::compiler::program::{CompilerTarget, Program};
use naga::valid::{Capabilities, ValidationFlags, Validator};

/// Compiles a validated `program` to WGSL and checks the output with naga.
/// Every GPU-valid function a program defines is emitted, whether or not
/// anything runs it on the GPU, so this makes every suite's programs WGSL
/// emission tests too.
#[track_caller]
pub fn assert_valid_wgsl(program: &Program) {
  let wgsl = program
    .clone()
    .compile_to_target(CompilerTarget::WGSL)
    .unwrap_or_else(|e| panic!("WGSL emission failed: {e:?}"));
  let module = naga::front::wgsl::parse_str(&wgsl).unwrap_or_else(|e| {
    panic!(
      "emitted WGSL doesn't parse:\n{}\n{wgsl}",
      e.emit_to_string(&wgsl)
    )
  });
  Validator::new(ValidationFlags::all(), Capabilities::all())
    .validate(&module)
    .unwrap_or_else(|e| {
      panic!(
        "emitted WGSL is invalid:\n{}\n{wgsl}",
        e.emit_to_string(&wgsl)
      )
    });
}
