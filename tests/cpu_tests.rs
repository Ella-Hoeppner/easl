use easl::compiler::core::load_easl_program_from_file;
use easl::compiler::program::CompilerTarget;
use easl::interpreter::{
  CpuRuntime, run_program_capturing_output_with_runtime,
};
use std::fs;
use std::path::Path;

fn run_cpu_test(name: &str) {
  let expected = fs::read_to_string(format!("./data/cpu/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/cpu/{name}.txt"));
  let x =
    load_easl_program_from_file(Path::new(&format!("./data/cpu/{name}.easl")));
  match x {
    Ok(Ok((_, Ok(mut program)))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

      // Every test runs on both CPU runtimes and must produce identical
      // output on each.
      let output = run_program_capturing_output_with_runtime(
        program.clone(),
        CpuRuntime::TreeWalking,
      )
      .unwrap_or_else(|e| {
        panic!("{name}: evaluation error (tree-walking): {e:#?}");
      });
      assert_eq!(output, expected, "{name}: output mismatch (tree-walking)");

      let vm_output = run_program_capturing_output_with_runtime(
        program,
        CpuRuntime::BytecodeVm,
      )
      .unwrap_or_else(|e| {
        panic!("{name}: evaluation error (bytecode VM): {e:#?}");
      });
      assert_eq!(vm_output, expected, "{name}: output mismatch (bytecode VM)");
    }
    Ok(Ok((document, Err(errors)))) => {
      let description = errors.describe(&document);
      fs::write(format!("./out/cpu/{name}.wgsl"), description.clone())
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
      fs::write(format!("./out/cpu/{name}.wgsl"), &description)
        .expect("Unable to write output file");
      panic!("Unexpected parse error in {name}:\n{description}");
    }
    Err(e) => panic!("IO error, couldn't load file {name}: \n{e:?}"),
  }
}

macro_rules! cpu_test {
  ($name:ident) => {
    #[test]
    fn $name() {
      run_cpu_test(stringify!($name));
    }
  };
}

cpu_test!(print);
cpu_test!(def);
cpu_test!(assignment);
cpu_test!(field_assignment);
cpu_test!(for_loop);
cpu_test!(while_loop);
cpu_test!(defn);
cpu_test!(struct_type);
cpu_test!(enum_type);
cpu_test!(print_vec);
cpu_test!(break_for);
cpu_test!(break_while);
cpu_test!(continue_for);
cpu_test!(continue_while);
cpu_test!(nested_break);
cpu_test!(nested_continue);
cpu_test!(nested_break_while);
cpu_test!(early_return);
cpu_test!(return_from_loop);
cpu_test!(bitwise_ops);
cpu_test!(reflect);
cpu_test!(refract);
cpu_test!(bitcast);
cpu_test!(array_length);
cpu_test!(mat_construct);
cpu_test!(mat_add_sub);
cpu_test!(mat_scalar_mul);
cpu_test!(mat_vec_mul);
cpu_test!(mat_mat_mul);
cpu_test!(vec_index_cpu);
cpu_test!(mat_index_cpu);
cpu_test!(mat_index_assign);
cpu_test!(vec_index_assign);
cpu_test!(bit_manip);
cpu_test!(data_packing);
cpu_test!(array_assignment);
cpu_test!(dynamic_array_assignment);
cpu_test!(dynamic_zeroed_array);
cpu_test!(static_zeroed_array);
cpu_test!(hof_avoids_skipping_calls);
cpu_test!(hof_calls_not_skipped);
cpu_test!(nested_associatives);
cpu_test!(any_all);
