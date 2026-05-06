use easl::compiler::core::load_easl_program_from_file;
use easl::interpreter::{IOEvent, run_program_test_io};
use std::fs;
use std::path::Path;

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
  let expected_txt = fs::read_to_string(format!("./data/window/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/window/{name}.txt"));

  fs::create_dir_all("./out/").expect("Unable to create out directory");
  match load_easl_program_from_file(Path::new(&format!(
    "./data/window/{name}.easl"
  ))) {
    Ok(Ok((_, Ok(mut program)))) => {
      let errors = program.validate_raw_program();
      assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

      let io = run_program_test_io(program).unwrap_or_else(|e| {
        panic!("{name}: evaluation error: {e:#?}");
      });

      let expected = parse_events(&expected_txt, name);
      assert_eq!(io.events, expected, "{name}: event mismatch");
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
window_test!(implicit_render_entry_points);
window_test!(closure_render_entry_points);
window_test!(print_after_close_window);
window_test!(spawn_window_captured_scope);
window_test!(print_string);
