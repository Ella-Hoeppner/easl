use easl::compiler::core::compile_easl_file_to_wgsl;
use easl::compiler::error::CompileErrorKind;
use easl::compiler::types::{TypeDescription, TypeStateDescription};
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
      fs::write(
        format!("./out/import/{name}.wgsl"),
        error_log.describe(&document),
      )
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

/// Assert that an import test fails to compile with exactly `expected`.
fn assert_errors(name: &str, mut expected: Vec<CompileErrorKind>) {
  match compile_import(name) {
    Ok(_) => panic!(
      "{name}/main.easl compiled successfully but was expected to fail.\n\
       See out/import/{name}.wgsl for the produced WGSL."
    ),
    Err(mut errors) => {
      let key = |e: &CompileErrorKind| format!("{e:?}");
      errors.sort_by_key(key);
      errors.dedup();
      expected.sort_by_key(key);
      assert_eq!(
        errors, expected,
        "See out/import/{name}.wgsl for the error description."
      );
    }
  }
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
import_test!(redundant_import);
import_test!(folder_import);
import_test!(parent_import);

macro_rules! import_error_test {
  ($name:ident, $($error:expr),+ $(,)?) => {
    #[test]
    fn $name() {
      assert_errors(stringify!($name), vec![$($error),+]);
    }
  };
}

// Imports don't re-export: `main` sees only what `intermediate` defines.
import_error_test!(
  indirect,
  CompileErrorKind::UnboundName("color".into()),
  CompileErrorKind::CouldntInferTypes,
);
import_error_test!(
  recursive_reference,
  CompileErrorKind::ImportCycle(vec![
    "main.easl".into(),
    "color.easl".into(),
    "main.easl".into(),
  ])
);
// A definition in an inline module overloads a same-named function of the
// enclosing scope.
import_test!(mod_defn_overloads_enclosing);
// Entry points and types of an imported file emit under qualified names,
// distinct from the main file's.
import_test!(lib_entry_point);

import_error_test!(
  collision_two_imports,
  CompileErrorKind::NameCollision("scale".into())
);
import_error_test!(
  identical_signature_use,
  CompileErrorKind::DuplicateFunctionSignature("double".into())
);
import_error_test!(
  private_access,
  CompileErrorKind::PrivateName("lib/hidden".into()),
);
import_error_test!(
  private_use,
  CompileErrorKind::PrivateName("lib/hidden".into())
);
import_error_test!(
  unknown_member,
  CompileErrorKind::UnknownModuleMember("lib".into(), "unknown".into()),
);
// An imported file can't see the file importing it.
import_error_test!(
  importer_invisible,
  CompileErrorKind::UnboundName("tint".into()),
  CompileErrorKind::CouldntInferTypes,
);
import_error_test!(
  inline_mod_conflict,
  CompileErrorKind::NameCollision("Foo".into())
);
import_error_test!(
  module_as_value,
  CompileErrorKind::ModuleUsedAsValue("lib".into())
);
import_error_test!(
  use_non_namespace,
  CompileErrorKind::NotANamespace("g".into())
);
import_error_test!(
  local_shadows_import,
  CompileErrorKind::CantShadowTopLevelBinding("scale".into())
);
// Variants are qualified by their enum unless it's `@unpack`.
import_error_test!(
  unqualified_variant,
  CompileErrorKind::UnboundName("Dim".into()),
);
// A private overload isn't callable from outside its module, even through a
// public overload's name.
import_error_test!(
  private_overload,
  CompileErrorKind::FunctionArgumentTypesIncompatible {
    f: TypeStateDescription::Known(TypeDescription::Function {
      arg_types: vec![(
        TypeStateDescription::Known(TypeDescription::F32),
        vec![]
      )],
      return_type: Box::new(TypeStateDescription::Known(TypeDescription::F32)),
    }),
    args: vec![TypeStateDescription::Known(TypeDescription::U32)],
  },
  CompileErrorKind::IncompatibleTypes(
    TypeStateDescription::Known(TypeDescription::U32),
    TypeStateDescription::Known(TypeDescription::F32),
  ),
);
// A private overload of a struct's constructor isn't callable from outside
// its module either.
import_error_test!(
  private_constructor_overload,
  CompileErrorKind::CouldntInferTypes,
  CompileErrorKind::FunctionArgumentTypesIncompatible {
    f: TypeStateDescription::OneOf(vec![
      TypeDescription::Function {
        arg_types: vec![
          (TypeStateDescription::Known(TypeDescription::F32), vec![]),
          (TypeStateDescription::Known(TypeDescription::F32), vec![]),
        ],
        return_type: Box::new(TypeStateDescription::Known(
          TypeDescription::Struct("lib/Pair".into())
        )),
      },
      TypeDescription::Function {
        arg_types: vec![(
          TypeStateDescription::Known(TypeDescription::F32),
          vec![]
        )],
        return_type: Box::new(TypeStateDescription::Known(
          TypeDescription::Struct("lib/Pair".into())
        )),
      },
    ]),
    args: vec![TypeStateDescription::Known(TypeDescription::U32)],
  },
);
import_error_test!(
  builtin_type_in_module,
  CompileErrorKind::BuiltinTypeRedefinition("MidiNote".into())
);
import_error_test!(
  missing_import,
  CompileErrorKind::ImportNotFound("nowhere.easl".into())
);
