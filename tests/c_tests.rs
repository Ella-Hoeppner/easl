use easl::compiler::core::load_easl_program_from_file;
use easl::compiler::error::{CompileErrorKind, ErrorLog};
use easl::compiler::program::CompilerTarget;
use std::fs;
use std::path::Path;
use std::process::Command;

/// Compiles a .easl file from data/c_backend/ through the C backend and writes
/// output to out/. Returns Ok(c_code) on success, or Err(Vec<CompileErrorKind>)
/// on compile error. Panics on parse errors, since those indicate a broken test
/// file rather than the kind of user-facing error we'd want to test for.
fn compile_c(name: &str) -> Result<String, Vec<CompileErrorKind>> {
  fs::create_dir_all("./out/c_backend/")
    .expect("Unable to create out directory");
  match load_easl_program_from_file(&Path::new(&format!(
    "./data/c_backend/{name}.easl"
  ))) {
    Ok(Ok((documents, Ok(mut program)))) => {
      let errors = program.validate_raw_program(CompilerTarget::C);
      if !errors.is_empty() {
        fs::write(
          format!("./out/c_backend/{name}.c"),
          errors.describe(&documents),
        )
        .expect("Unable to write output file");
        return Err(errors.into_iter().map(|e| e.kind).collect());
      }
      match program.compile_to_target(CompilerTarget::C) {
        Ok(c_code) => {
          fs::write(format!("./out/c_backend/{name}.c"), &c_code)
            .expect("Unable to write output file");
          Ok(c_code)
        }
        Err(e) => {
          let error_log = ErrorLog::from(e);
          fs::write(
            format!("./out/c_backend/{name}.c"),
            error_log.describe(&documents),
          )
          .expect("Unable to write output file");
          Err(error_log.into_iter().map(|e| e.kind).collect())
        }
      }
    }
    Ok(Ok((documents, Err(error_log)))) => {
      fs::write(
        format!("./out/c_backend/{name}.c"),
        error_log.describe(&documents),
      )
      .expect("Unable to write output file");
      Err(error_log.into_iter().map(|e| e.kind).collect())
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
      fs::write(format!("./out/c_backend/{name}.c"), &description)
        .expect("Unable to write output file");
      panic!("Unexpected parse error in {name}:\n{description}");
    }
    Err(e) => panic!("IO error, couldn't load file {name}: \n{e:?}"),
  }
}

/// Validate that a C string is well-formed and type-correct according to
/// clang. The generated code is already written to out/{name}.c by compile_c.
fn validate_c(name: &str, _c_code: &str) {
  let c_path = format!("./out/c_backend/{name}.c");
  let output = Command::new("clang")
    .args(["-fsyntax-only", "-std=c11", &c_path])
    .output()
    .unwrap_or_else(|e| {
      panic!(
        "Failed to run clang (is it installed?): {e}\n\
         Install with: xcode-select --install (macOS) or \
         apt install clang (Linux)"
      )
    });

  if !output.status.success() {
    let stderr = String::from_utf8_lossy(&output.stderr);
    panic!(
      "{name}: clang type-check failed on generated C:\n{stderr}\n\
       See out/c_backend/{name}.c for the generated code."
    );
  }
}

/// Assert that an easl file compiles successfully to C,
/// then validate the output with clang.
fn assert_c_compiles(name: &str) {
  let c_code = compile_c(name).unwrap_or_else(|errors| {
    panic!(
      "{name}.easl failed to compile to C: {errors:?}\n\
       See out/c_backend/{name}.c for details."
    )
  });
  validate_c(name, &c_code);
}

/// Assert that an easl file fails to compile with exactly the given errors.
#[allow(dead_code)]
fn assert_c_errors(name: &str, expected: &[CompileErrorKind]) {
  let actual = compile_c(name).expect_err(&format!(
    "{name}.easl compiled successfully but was expected to fail.\n\
     See out/c_backend/{name}.c for the produced C."
  ));

  for exp in expected {
    assert!(
      actual.iter().any(|a| a == exp),
      "Expected error `{exp}` not found.\n\
       Actual errors: {actual:?}\n\
       See out/c_backend/{name}.c for details."
    );
  }

  for act in &actual {
    assert!(
      expected.iter().any(|e| e == act),
      "Unexpected error `{act}`.\n\
       Expected errors: {expected:?}\n\
       See out/c_backend/{name}.c for details."
    );
  }
}

macro_rules! c_success_test {
  ($name:ident) => {
    #[test]
    fn $name() {
      assert_c_compiles(stringify!($name));
    }
  };
}

#[allow(unused_macros)]
macro_rules! c_error_test {
  ($name:ident, $($error:expr),+ $(,)?) => {
    #[test]
    fn $name() {
      assert_c_errors(stringify!($name), &[$($error),+]);
    }
  };
}

// --- Success tests ---

c_success_test!(double);
c_success_test!(global_var);
c_success_test!(sin);
c_success_test!(sin_twice);
c_success_test!(user_struct);
c_success_test!(user_enum);
// c_success_test!(vec2f);

// --- Error tests ---
