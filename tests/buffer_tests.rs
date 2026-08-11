use easl::compiler::core::load_easl_program_from_file;
use easl::compiler::program::CompilerTarget;
use easl::interpreter::{
  CpuRuntime, run_program_with_capture_and_runtime_from_path,
};
use std::fs;
use std::path::Path;

fn run_buffer_test(name: &str) {
  let expected = fs::read_to_string(format!("./data/buffer/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/buffer/{name}.txt"));

  let source_path_str = format!("./data/buffer/{name}.easl");
  let source_path = Path::new(&source_path_str);

  fs::create_dir_all("./out/buffer/").expect("Unable to create out directory");
  match load_easl_program_from_file(source_path) {
    Ok(Ok((_, Ok(mut program)))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

      // Every test runs on both CPU runtimes and must produce identical
      // output on each.
      let prints = run_program_with_capture_and_runtime_from_path(
        program.clone(),
        source_path,
        CpuRuntime::TreeWalking,
      )
      .unwrap_or_else(|e| {
        panic!("{name}: evaluation error (tree-walking): {e:#?}");
      });
      let output: String =
        prints.into_iter().map(|s| format!("{s}\n")).collect();
      assert_eq!(output, expected, "{name}: output mismatch (tree-walking)");
      let vm_prints = run_program_with_capture_and_runtime_from_path(
        program,
        source_path,
        CpuRuntime::BytecodeVm,
      )
      .unwrap_or_else(|e| {
        panic!("{name}: evaluation error (bytecode VM): {e:#?}");
      });
      let vm_output: String =
        vm_prints.into_iter().map(|s| format!("{s}\n")).collect();
      assert_eq!(vm_output, expected, "{name}: output mismatch (bytecode VM)");
    }
    Ok(Ok((document, Err(errors)))) => {
      let description = errors.describe(&document);
      fs::write(format!("./out/buffer/{name}.wgsl"), description.clone())
        .expect("Unable to write output file");
      panic!("{description}");
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
      fs::write(format!("./out/buffer/{name}.wgsl"), &description)
        .expect("Unable to write output file");
      panic!("Unexpected parse error in {name}:\n{description}");
    }
    Err(e) => panic!("IO error, couldn't load file {name}: \n{e:?}"),
  }
}

macro_rules! buffer_test {
  ($name:ident) => {
    #[test]
    fn $name() {
      run_buffer_test(stringify!($name));
    }
  };
}

buffer_test!(bidirectional_transfer);
buffer_test!(bidirectional_transfer_windowless);
buffer_test!(array_assignment);
buffer_test!(array_assignment_cross_window);
buffer_test!(break_in_match);
buffer_test!(break_in_nonunit_match);
buffer_test!(cpu_atomic_assignment);
buffer_test!(bidirectional_transfer_array);
buffer_test!(bidirectional_transfer_render);
buffer_test!(closure_compute_entry_point);
buffer_test!(load_red_pixel);
buffer_test!(set_render_target);
buffer_test!(struct_array_buffer);
buffer_test!(print_enum);
buffer_test!(buffer_size_consistency);
buffer_test!(iterative_compute_uniform);
buffer_test!(zero_length_zeroed_array);
buffer_test!(raymarching_sdf);
buffer_test!(abstracted_compute_dispatch);
buffer_test!(nested_closure_compute_dispatch);
buffer_test!(many_bindings_render);

/// A program whose vertex stage genuinely references more buffers than
/// Metal supports must fail with easl's pre-flight validation error (which
/// names the offending bindings), not an opaque wgpu-internal panic from
/// pipeline creation. Metal-specific budget, so macOS-only.
#[cfg(target_os = "macos")]
#[test]
#[should_panic(expected = "too many GPU buffer bindings in the vertex stage")]
fn too_many_vertex_bindings() {
  let source_path_str = "./data/buffer/too_many_vertex_bindings.easl";
  let source_path = Path::new(&source_path_str);
  let Ok(Ok((_, Ok(mut program)))) = load_easl_program_from_file(source_path)
  else {
    panic!("failed to load program")
  };
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  assert!(errors.is_empty(), "compile errors: {errors:#?}");
  let _ = run_program_with_capture_and_runtime_from_path(
    program,
    source_path,
    CpuRuntime::TreeWalking,
  )
  .unwrap();
}
buffer_test!(render_target_pingpong);
buffer_test!(offscreen_render_compute_order);
buffer_test!(dispatch_from_scoped_frame_closure);
buffer_test!(save_png_roundtrip);
buffer_test!(save_png_render_target);
buffer_test!(dispatch_closure_captures_dynamic_array);
buffer_test!(dispatch_closure_captures_two_dynamic_arrays);
buffer_test!(dispatch_closure_captures_closure_with_dynamic_array);
buffer_test!(dispatch_closure_captures_closure_three_deep);
buffer_test!(hof_dispatch_captures_local_dynamic_array);
buffer_test!(elided_bindings);
buffer_test!(default_shared_var);

/// Window-info queries used inside GPU code compile into implicit uniform
/// bindings that the runtime refreshes from the IO manager each frame.
/// Spoofed IO values must round-trip through the binding upload, the
/// shader, and the storage readback — identically on both CPU runtimes.
#[test]
fn gpu_window_info_spoofed() {
  use easl::interpreter::{
    CaptureIO, SpoofedWindowInfo, run_program_with_runtime,
  };
  let source_path_str = "./data/buffer/gpu_window_info.easl";
  let source_path = Path::new(&source_path_str);
  let spoof = SpoofedWindowInfo {
    size: (320, 240),
    time: 42.5,
    delta_time: 0.25,
    frame_index: 7,
    mouse_coords: (12, 34),
    mouse_present: true,
    mouse_down: false,
    mouse_just_down: true,
    keys_down: vec!["a".to_string()],
    keys_just_down: vec!["b".to_string()],
  };
  let expected =
    "[42.5 0.25 7. 320. 240. 12. 34. 1. 0. 1. 1. 0. 1.]".to_string();
  for runtime in [CpuRuntime::TreeWalking, CpuRuntime::BytecodeVm] {
    let Ok(Ok((_, Ok(mut program)))) = load_easl_program_from_file(source_path)
    else {
      panic!("failed to load program")
    };
    let errors = program.validate_raw_program(CompilerTarget::WGSL);
    assert!(errors.is_empty(), "compile errors: {errors:#?}");
    let mut io = CaptureIO::new();
    io.spoofed_window_info = Some(spoof.clone());
    let (io, _) = run_program_with_runtime(
      program,
      None,
      io,
      source_path.parent().map(|p| p.to_path_buf()),
      runtime,
    )
    .unwrap();
    assert_eq!(io.prints, vec![expected.clone()], "runtime {runtime:?}");
  }
}
