mod common;

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
      common::assert_valid_wgsl(&program);

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
vm_test!(const_generic_map_specialization);
vm_test!(fn_value_union_signature_collision);
vm_test!(fn_value_multi_member_set);
vm_test!(fn_value_const_generic_array);
vm_test!(returned_closure_hof);

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
  common::assert_valid_wgsl(&program);
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
    common::assert_valid_wgsl(&program);
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

// One merge's returned closure fed back in as a member of a second,
// same-signature merge: `[a b]` and `[m1 c]` have distinct representations,
// so the outer union's `merge` member dispatches over the inner union — a
// DAG, never a self-referential type.
vm_test!(fn_value_nested_union_recursion);

/// Passing a `let` to a `@ref` parameter copies nothing: the binding is made
/// addressable once, and each call binds the parameter to it. A call taking
/// a 1000-float struct by `@ref` must not add the struct's size to the
/// stack (it once added a whole copy per call, so 70 such calls overflowed
/// the 65536-slot stack).
#[test]
fn ref_args_reserve_no_copies() {
  use easl::compiler::core::load_easl_program_from_file_with_lookup_function;
  use easl::compiler::program::CompilerTarget;
  fn stack_size(calls: usize) -> usize {
    let mut source = String::from("(struct Big\n  data: [1000: f32])\n");
    for i in 0..calls {
      source += &format!("(defn f{i} [@ref b: Big]: f32\n  (b.data {i}u))\n");
    }
    let sum: String = (0..calls).map(|i| format!(" (f{i} big)")).collect();
    source += &format!(
      "@cpu\n(defn main []\n  (let [big (Big (zeroed-array))]\n    \
       (print (+ 0.{sum}))))\n"
    );
    let (_, program) = load_easl_program_from_file_with_lookup_function(
      std::path::Path::new("./data/vm/let_binding.easl"),
      |_| Ok(source.clone()),
    )
    .unwrap()
    .unwrap();
    let mut program = program.unwrap();
    let errors = program.validate_raw_program(CompilerTarget::WGSL);
    assert!(errors.is_empty(), "compile errors: {errors:#?}");
    common::assert_valid_wgsl(&program);
    program.compile_to_bytecode_program_cpu().0.stack.len()
  }
  let per_call = (stack_size(8) - stack_size(4)) / 4;
  assert!(per_call < 16, "each call adds {per_call} stack slots");
  stack_size(70);
}
