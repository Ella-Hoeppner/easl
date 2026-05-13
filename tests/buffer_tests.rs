use easl::compiler::core::load_easl_program_from_file;
use easl::interpreter::run_program_with_capture_from_path;
use std::fs;
use std::path::Path;

fn run_buffer_test(name: &str) {
  let expected = fs::read_to_string(format!("./data/buffer/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/buffer/{name}.txt"));

  let source_path_str = format!("./data/buffer/{name}.easl");
  let source_path = Path::new(&source_path_str);

  fs::create_dir_all("./out/").expect("Unable to create out directory");
  match load_easl_program_from_file(source_path) {
    Ok(Ok((_, Ok(mut program)))) => {
      let errors = program.validate_raw_program();
      assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

      let prints = run_program_with_capture_from_path(program, source_path)
        .unwrap_or_else(|e| {
          panic!("{name}: evaluation error: {e:#?}");
        });
      let output: String =
        prints.into_iter().map(|s| format!("{s}\n")).collect();
      assert_eq!(output, expected, "{name}: output mismatch");
    }
    Ok(Ok((document, Err(errors)))) => {
      let description = errors.describe(&document);
      fs::write(format!("./out/{name}.wgsl"), description.clone())
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
      fs::write(format!("./out/{name}.wgsl"), &description)
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
