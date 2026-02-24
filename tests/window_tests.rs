use easl::compiler::program::Program;
use easl::interpreter::{IOEvent, run_program_test_io};
use easl::parse::parse_easl_without_comments;
use std::fs;

fn parse_events(txt: &str, name: &str) -> Vec<IOEvent> {
  txt
    .lines()
    .filter(|l| !l.trim().is_empty())
    .map(|line| {
      if let Some(msg) = line.strip_prefix("print: ") {
        IOEvent::Print(msg.to_string())
      } else if line == "spawn-window" {
        IOEvent::SpawnWindow
      } else if let Some(rest) = line.strip_prefix("dispatch-render-shaders ") {
        let parts: Vec<&str> = rest.splitn(3, ' ').collect();
        assert_eq!(
          parts.len(),
          3,
          "{name}: malformed dispatch-render-shaders line: {line:?}"
        );
        IOEvent::DispatchShaders {
          vert: parts[0].to_string(),
          frag: parts[1].to_string(),
          vert_count: parts[2].parse().expect("vert_count must be u32"),
        }
      } else if let Some(rest) = line.strip_prefix("dispatch-compute-shader ") {
        let (entry, counts) =
          rest.split_once(" (vec3u ").unwrap_or_else(|| {
            panic!("{name}: malformed dispatch-compute-shader line: {line:?}")
          });
        let counts = counts.trim_end_matches(')');
        let parts: Vec<u32> = counts
          .split(' ')
          .map(|s| {
            s.trim_end_matches('u')
              .parse()
              .expect("workgroup count must be u32")
          })
          .collect();
        assert_eq!(
          parts.len(),
          3,
          "{name}: expected 3 workgroup counts in: {line:?}"
        );
        IOEvent::DispatchComputeShader {
          entry: entry.to_string(),
          workgroup_count: (parts[0], parts[1], parts[2]),
        }
      } else if line == "close-window" {
        IOEvent::CloseWindow
      } else {
        panic!("{name}: unknown event format: {line:?}");
      }
    })
    .collect()
}

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

  let expected = parse_events(&expected_txt, name);
  assert_eq!(io.events, expected, "{name}: event mismatch");
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
window_test!(print_and_render);
window_test!(multi_render);
window_test!(multi_window);
window_test!(global_frame_counter);
window_test!(compute_dispatch);
window_test!(close_window);
window_test!(immediate_close_window);
