use easl::{
  compile_easl_source_to_wgsl, compiler::program::Program,
  interpreter::run_program, parse::parse_easl_without_comments,
};
use std::fs;

fn shader_examples() {
  unsafe {
    std::env::set_var("RUST_BACKTRACE", "1");
  }
  fs::create_dir_all("./out/").expect("Unable to create out directory");
  let mut total_time: f64 = 0.;
  for filename in [
    // "inversion",
    // "associative",
    // "assignment",
    // "simple_shader",
    // "variadic_vec",
    // "let",
    // "block",
    // "accessor",
    // "swizzle",
    // "equality",
    // "variable",
    // "generic_struct",
    // "nested_generic_struct",
    // "generic_fn",
    // "monomorphized_fn",
    // "nested_monomorphized_fn",
    // "bool_match",
    // "int_match",
    // "generic_vec",
    // "heterogenous_vec_constructor",
    // "specialized_vec_constructor",
    // "generic_constraint",
    // "for_loop",
    // "while_loop",
    // "return",
    // "math",
    // "if_macro",
    // "thread_macro",
    // "when_macro",
    // "array",
    // "argument_annotation",
    // "texture",
    // "cast",
    // "array_literal",
    // "fn_inlining",
    // "nested_fn_inlining",
    // "reference",
    // "generic_identity",
    // "shadow",
    // "let_in_let",
    // "block_in_let",
    // "match_in_let",
    // "block_in_application",
    // "match_in_application",
    // "let_in_application",
    // "mutating_block_lift",
    // "return_in_match",
    // "globally_mutating_block_lift",
    // "block_purity_filter",
    // "matrix",
    // "flowers",
    // "block_deexpressionify",
    // "block_deexpressionify_2",
    // "global_var_effect",
    // "array_assignment",
    // "mixed_vec",
    // "variadic_arithmetic",
    // "application_in_return",
    // "deexpressionify_in_return",
    // "duplicate_defn",
    // "enum",
    // "enum_generic_option",
    // "enum_generic_either",
    // "match_enum",
    // "match_enum_generic",
    // "option_map",
    // "swizzle_assignment",
    // "either_match",
    // "nested_option",
    // "enum_of_struct",
    // "address_space",
    // "monomorphized_name_collision",
    // "inlined_fn_name_collision",
    // "override",
    // "attributes",
    // "var_initialization",
    // "var_name_collision",
    // "shadowed_arg",
    // "var_arg",
    // "angle_bracket",
    // "overload",
    // "overload_builtin",
    // "trig",
    // "any_and_all",
    // "vecb",
    // "dpdx",
    // "discard",
    // "compute",
    // "arg_annotation",
    // "return_annotation",
    // "break_in_match",
    // "array_literal_access",
    // "exp_after_control_flow",
    // "location_inference",
    // "reserved_names",
    // "invalid_names",
    // "duplicate_field",
    // "duplicate_variant",
    // "double_monomorphize",
    // "complex_monomorphize",
    // "user_reference",
    // "nested_user_reference",
    // "ownership",
    // "underscore_name",
    // "bind_discard",
    // "non_return_if",
    // "unused_enum",
    // "vector_equality",
    // "match_non_name_scrutinee",
    // "type_order",
    // "nested_let_name_reuse",
    // "local_binding_inlining",
    // "local_fn",
    "returned_array", // "vec_match", // todo!
  ] {
    print!("compiling {filename}...");
    let t = std::time::Instant::now();
    let easl_source = fs::read_to_string(&format!("./data/{filename}.easl"))
      .expect(&format!("Unable to read {filename}.easl"));
    fs::write(
      &format!("./out/{filename}.wgsl"),
      match compile_easl_source_to_wgsl(&easl_source) {
        Ok(Ok(wgsl)) => {
          println!("{:?}", t.elapsed());
          total_time += t.elapsed().as_nanos() as f64 / 1000000.;
          wgsl
        }
        Ok(Err((document, error_log))) => {
          println!("failed!\n");
          error_log.describe(&document)
        }
        Err(mut failed_document) => {
          println!("parsing failed!\n");
          let mut errors = vec![];
          std::mem::swap(&mut errors, &mut failed_document.parsing_failures);
          format!(
            "{}",
            errors
              .into_iter()
              .map(|err| err.describe(&failed_document))
              .collect::<Vec<String>>()
              .join("\n\n")
          )
        }
      },
    )
    .expect("Unable to write file");
  }
  println!("total time: {total_time}");
}

fn cpu_examples() {
  unsafe {
    std::env::set_var("RUST_BACKTRACE", "1");
  }
  fs::create_dir_all("./out/").expect("Unable to create out directory");
  for filename in [
    "print",
    "def",
    "assignment",
    "field_assignment",
    "for",
    "while",
    "fn",
    "struct",
    "enum",
  ] {
    print!("running {filename}...");
    let easl_source =
      fs::read_to_string(&format!("./data/cpu/{filename}.easl"))
        .expect(&format!("Unable to read {filename}.easl"));
    let (mut program, errors) = Program::from_easl_document(
      &parse_easl_without_comments(&easl_source),
      easl::compiler::builtins::built_in_macros(),
    );
    if errors.is_empty() {
      let errors = program.validate_raw_program();
      if errors.is_empty() {
        match run_program(program) {
          Ok(_) => {
            println!("finished running successfully");
          }
          Err(err) => println!("encountered evaluation error: {err:#?}"),
        }
      } else {
        println!("failed to run {filename}:\n{errors:#?}");
      }
    } else {
      println!("failed to run {filename}:\n{errors:#?}");
    }
  }
}

fn main() {
  shader_examples();
  //cpu_examples();
}
