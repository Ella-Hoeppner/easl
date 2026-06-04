use easl::compiler::core::compile_easl_file_to_wgsl;
use easl::compiler::error::CompileErrorKind;
use std::fs;
use std::path::Path;

/// Compiles data/import/{name}/main.easl and writes output (WGSL or error
/// description) to out/. Returns Ok(wgsl) on success, or
/// Err(Vec<CompileErrorKind>) on compile error. Panics on parse errors.
fn compile_import(name: &str) -> Result<String, Vec<CompileErrorKind>> {
  fs::create_dir_all("./out/import/").expect("Unable to create out directory");
  match compile_easl_file_to_wgsl(Path::new(&format!(
    "./data/import/{name}/main.easl"
  ))) {
    Ok(Ok(Ok(wgsl))) => {
      fs::write(format!("./out/import/{name}.wgsl"), &wgsl)
        .expect("Unable to write output file");
      Ok(wgsl)
    }
    Ok(Ok(Err((document, error_log)))) => {
      fs::write(format!("./out/import/{name}.wgsl"), error_log.describe(&document))
        .expect("Unable to write output file");
      Err(error_log.errors.into_iter().map(|e| e.kind).collect())
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
      fs::write(format!("./out/import/{name}.wgsl"), &description)
        .expect("Unable to write output file");
      panic!("Unexpected parse error in {name}:\n{description}");
    }
    Err(e) => panic!("IO error, couldn't load file {name}: \n{e:?}"),
  }
}

/// Validate that a WGSL string is well-formed and type-correct according to naga.
fn validate_wgsl(name: &str, wgsl: &str) {
  let module = naga::front::wgsl::parse_str(wgsl).unwrap_or_else(|e| {
    panic!(
      "{name}: naga failed to parse generated WGSL:\n{e}\n\
       See out/import/{name}.wgsl for the generated code.",
    )
  });
  let mut validator = naga::valid::Validator::new(
    naga::valid::ValidationFlags::all(),
    naga::valid::Capabilities::all(),
  );
  validator.validate(&module).unwrap_or_else(|e| {
    panic!(
      "{name}: naga validation failed on generated WGSL:\n{e}\n\
       See out/import/{name}.wgsl for the generated code."
    )
  });
}

/// Assert that an import test compiles successfully and passes naga validation.
fn assert_compiles(name: &str) {
  let wgsl = compile_import(name).unwrap_or_else(|errors| {
    panic!(
      "{name}/main.easl failed to compile: {errors:?}\n\
       See out/import/{name}.wgsl for details."
    )
  });
  validate_wgsl(name, &wgsl);
}

macro_rules! import_test {
  ($name:ident) => {
    #[test]
    fn $name() {
      assert_compiles(stringify!($name));
    }
  };
}

import_test!(simple);
import_test!(dot_slash);
import_test!(indirect);
import_test!(redundant_import);
import_test!(folder_import);
import_test!(parent_import);
import_test!(recursive_reference);
