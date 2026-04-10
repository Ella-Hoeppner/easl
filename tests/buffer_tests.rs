use easl::compile_easl_source_to_wgsl;
use easl::compiler::program::Program;
use easl::interpreter::run_program_with_capture;
use easl::parse::parse_easl_without_comments;
use std::fs;

fn run_buffer_test(name: &str) {
  let easl_source = fs::read_to_string(format!("./data/buffer/{name}.easl"))
    .unwrap_or_else(|_| panic!("Unable to read data/buffer/{name}.easl"));
  let expected = fs::read_to_string(format!("./data/buffer/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/buffer/{name}.txt"));

  fs::create_dir_all("./out/").expect("Unable to create out directory");
  match compile_easl_source_to_wgsl(&easl_source) {
    Ok(Ok(wgsl)) => {
      fs::write(format!("./out/{name}.wgsl"), &wgsl)
        .expect("Unable to write output file");
    }
    Ok(Err((document, error_log))) => {
      fs::write(format!("./out/{name}.wgsl"), error_log.describe(&document))
        .expect("Unable to write output file");
    }
    Err(mut failed_document) => {
      let mut errors = vec![];
      std::mem::swap(&mut errors, &mut failed_document.parsing_failures);
      let description = errors
        .into_iter()
        .map(|err| err.describe(&failed_document))
        .collect::<Vec<String>>()
        .join("\n\n");
      fs::write(format!("./out/{name}.wgsl"), &description)
        .expect("Unable to write output file");
    }
  }

  let (mut program, errors) = Program::from_easl_document(
    &parse_easl_without_comments(&easl_source),
    easl::compiler::builtins::built_in_macros(),
  );
  assert!(errors.is_empty(), "{name}: parse errors: {errors:#?}");

  let errors = program.validate_raw_program();
  assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

  let prints = run_program_with_capture(program).unwrap_or_else(|e| {
    panic!("{name}: evaluation error: {e:#?}");
  });
  let output: String = prints.into_iter().map(|s| format!("{s}\n")).collect();
  assert_eq!(output, expected, "{name}: output mismatch");
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
