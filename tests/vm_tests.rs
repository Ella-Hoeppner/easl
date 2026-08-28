use easl::compiler::core::load_easl_program_from_file;
use easl::compiler::program::CompilerTarget;
use std::fs;
use std::path::Path;

fn run_vm_test(name: &str) {
  let expected_str = fs::read_to_string(format!("./data/vm/{name}.txt"))
    .unwrap_or_else(|_| panic!("Unable to read data/vm/{name}.txt"));
  let expected: f32 = expected_str.trim().parse().unwrap_or_else(|_| {
    panic!("{name}: couldn't parse expected float from {expected_str:?}")
  });

  let source_path_str = format!("./data/vm/{name}.easl");
  let source_path = Path::new(&source_path_str);

  match load_easl_program_from_file(source_path) {
    Ok(Ok((_, Ok(mut program)))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      assert!(errors.is_empty(), "{name}: compile errors: {errors:#?}");

      let (mut bytecode_program, function_names) =
        program.compile_to_bytecode_program();

      let function_index = function_names
        .iter()
        .position(|n| &**n == "f")
        .unwrap_or_else(|| panic!("{name}: no function named `f`"));

      bytecode_program.prepare_to_run_function(function_index);
      bytecode_program.execute();

      let return_position =
        bytecode_program.get_function_return_position(function_index);
      let result =
        f32::from_bits(bytecode_program.stack[return_position as usize]);

      assert!(
        (result - expected).abs() <= 0.0001,
        "{name}: expected {expected}, got {result}"
      );
    }
    Ok(Ok((document, Err(errors)))) => {
      panic!("{name}: {}", errors.describe(&document));
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
      panic!("Unexpected parse error in {name}:\n{description}");
    }
    Err(e) => panic!("IO error, couldn't load file {name}: \n{e:?}"),
  }
}

macro_rules! vm_test {
  ($name:ident) => {
    #[test]
    fn $name() {
      run_vm_test(stringify!($name));
    }
  };
}

vm_test!(cos);
vm_test!(plus);
vm_test!(nested_plus);
vm_test!(fn_call);
vm_test!(fn_call_with_arg);
vm_test!(fn_call_with_two_args);
vm_test!(let_binding);
vm_test!(if_true);
vm_test!(if_false);
vm_test!(if_equality_check);
vm_test!(match_int);
vm_test!(global_var_assignment);
vm_test!(local_var_assignment);
vm_test!(while_loop);
vm_test!(for_loop);
vm_test!(when);
vm_test!(while_loop_break);
vm_test!(for_loop_break);
vm_test!(while_loop_continue);
vm_test!(for_loop_continue);
vm_test!(early_return);
vm_test!(array_access);
vm_test!(struct_access);
vm_test!(generic_enum_match);

/// Loops must behave identically on repeated `execute` calls. The stack
/// persists between calls (as in the per-sample audio path), so a loop
/// variable that isn't reinitialized at loop entry poisons every run after
/// the first.
#[test]
fn for_loop_repeated_execute() {
  let source_path_str = "./data/vm/for_loop.easl".to_string();
  let source_path = Path::new(&source_path_str);
  let Ok(Ok((_, Ok(mut program)))) = load_easl_program_from_file(source_path)
  else {
    panic!("couldn't load data/vm/for_loop.easl");
  };
  let errors = program.validate_raw_program(CompilerTarget::WGSL);
  assert!(errors.is_empty(), "compile errors: {errors:#?}");
  let (mut bytecode_program, function_names) =
    program.compile_to_bytecode_program();
  let function_index = function_names
    .iter()
    .position(|n| &**n == "f")
    .expect("no function named `f`");
  let return_position =
    bytecode_program.get_function_return_position(function_index);
  for run in 0..3 {
    bytecode_program.prepare_to_run_function(function_index);
    bytecode_program.execute();
    let result =
      f32::from_bits(bytecode_program.stack[return_position as usize]);
    assert!(
      (result - 15.).abs() <= 0.0001,
      "run {run}: expected 15, got {result}"
    );
  }
}

/// Immutable `@ref` element args emit no write-back stores: the preload
/// temp is read-only, so storing it back would be pure waste (the
/// granular-sampler hot path — one redundant store per grain per sample).
/// The mutable variant of the same program must still contain a store,
/// proving the assertion's sensitivity.
#[test]
fn immutable_ref_elements_emit_no_stores() {
  use easl::compiler::core::load_easl_program_from_file_with_lookup_function;
  use easl::compiler::program::CompilerTarget;
  use easl::vm::bytecode::Op;
  fn store_count(source: &str) -> usize {
    let (_, program) = load_easl_program_from_file_with_lookup_function(
      std::path::Path::new("./data/vm/let_binding.easl"),
      |_| Ok(source.to_string()),
    )
    .unwrap()
    .unwrap();
    let mut program = program.unwrap();
    let errors = program.validate_raw_program(CompilerTarget::WGSL);
    assert!(errors.is_empty(), "compile errors: {errors:#?}");
    let (compiled, _) = program.compile_to_bytecode_program_cpu();
    compiled
      .code
      .function_instructions
      .iter()
      .filter(|instruction| {
        matches!(
          instruction.op,
          Op::DynStore | Op::HeapStore | Op::ArrayStore
        )
      })
      .count()
  }
  let immutable = "
(defn get [@ref x: f32]: f32
  (* x 2.))

@cpu
(defn main []
  (let [xs (into-dynamic-array [1. 2.])]
    (print (get (xs 0u)))))
";
  let mutable = "
(defn bump [@var @ref x: f32]: f32
  (+= x 1.)
  x)

@cpu
(defn main []
  (let [@var xs (into-dynamic-array [1. 2.])]
    (print (bump (xs 0u)))))
";
  assert_eq!(
    store_count(immutable),
    0,
    "immutable-ref element arg emitted a write-back store"
  );
  assert!(
    store_count(mutable) > 0,
    "mutable-ref contrast case emitted no store — assertion insensitive"
  );
}
