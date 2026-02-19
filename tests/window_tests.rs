use easl::compiler::program::Program;
use easl::interpreter::{WindowEvent, run_program_test_io};
use easl::parse::parse_easl_without_comments;
use std::fs;

fn run_window_test(name: &str) {
  let easl_source = fs::read_to_string(format!("./data/window/{name}.easl"))
    .unwrap_or_else(|_| panic!("Unable to read data/window/{name}.easl"));
  let expected_txt = fs::read_to_string(format!("./data/window/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/window/{name}.txt"));

  let (mut program, errors) = Program::from_easl_document(
    &parse_easl_without_comments(&easl_source),
    easl::compiler::builtins::built_in_macros(),
  );
  assert!(errors.is_empty(), "{name}: parse errors: {errors:#?}");

  let errors = program.validate_raw_program();
  assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

  let io = run_program_test_io(program).unwrap_or_else(|e| {
    panic!("{name}: evaluation error: {e:#?}");
  });

  let expected: Vec<WindowEvent> = expected_txt
    .lines()
    .filter(|l| !l.trim().is_empty())
    .map(|line| {
      let parts: Vec<&str> = line.splitn(3, ' ').collect();
      assert_eq!(parts.len(), 3, "{name}: malformed expected line: {line:?}");
      WindowEvent {
        vert: parts[0].to_string(),
        frag: parts[1].to_string(),
        vert_count: parts[2].parse().expect("vert_count must be u32"),
      }
    })
    .collect();

  assert_eq!(io.dispatches, expected, "{name}: dispatch mismatch");
}

macro_rules! window_test {
  ($name:ident) => {
    #[test]
    fn $name() {
      run_window_test(stringify!($name));
    }
  };
}

window_test!(simple);
window_test!(indirect);
window_test!(vec4f_in_between);
window_test!(abstraction);
