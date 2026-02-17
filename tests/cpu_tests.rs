use easl::compiler::program::{CompilerTarget, Program};
use easl::interpreter::run_program_capturing_output;
use easl::parse::parse_easl_without_comments;
use std::fs;

fn run_cpu_test(name: &str) {
  let easl_source = fs::read_to_string(format!("./data/cpu/{name}.easl"))
    .unwrap_or_else(|_| panic!("Unable to read data/cpu/{name}.easl"));
  let expected = fs::read_to_string(format!("./data/cpu/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/cpu/{name}.txt"));

  let (mut program, errors) = Program::from_easl_document(
    &parse_easl_without_comments(&easl_source),
    easl::compiler::builtins::built_in_macros(),
  );
  assert!(errors.is_empty(), "{name}: parse errors: {errors:#?}");

  let errors = program.validate_raw_program(CompilerTarget::CPU);
  assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

  let output = run_program_capturing_output(program).unwrap_or_else(|e| {
    panic!("{name}: evaluation error: {e:#?}");
  });

  assert_eq!(output, expected, "{name}: output mismatch");
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
