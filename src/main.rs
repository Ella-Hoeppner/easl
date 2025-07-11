use easl::{
  compiler::compile_easl_source_to_wgsl, format::format_document,
  parse::parse_easl,
};
use std::fs;

fn main() {
  std::env::set_var("RUST_BACKTRACE", "1");
  fs::create_dir_all("./out/").expect("Unable to create out directory");
  let mut total_time: f64 = 0.;
  for filename in [
    "inversion",
    "associative",
    "assignment",
    "simple_shader",
    "variadic_vec",
    "let",
    "block",
    "accessor",
    "swizzle",
    "equality",
    "variable",
    "generic_struct",
    "nested_generic_struct",
    "generic_fn",
    "monomorphized_fn",
    "nested_monomorphized_fn",
    "bool_match",
    "int_match",
    "generic_vec",
    "heterogenous_vec_constructor",
    "specialized_vec_constructor",
    "generic_constraint",
    "for_loop",
    "while_loop",
    "return",
    "math",
    "if_macro",
    "thread_macro",
    "when_macro",
    "array",
    "argument_metadata",
    "texture",
    "cast",
    "array_literal",
    "fn_inlining",
    "nested_fn_inlining",
    "reference",
    "generic_identity",
    "shadow",
    "let_in_let",
    "block_in_let",
    "match_in_let",
    "block_in_application",
    "match_in_application",
    "let_in_application",
    "mutating_block_lift",
    "return_in_match",
    "globally_mutating_block_lift",
    "block_purity_filter",
    "matrix",
    "flowers",
    "block_deexpressionify",
    "block_deexpressionify_2",
  ] {
    print!("compiling {filename}...");
    let t = std::time::Instant::now();
    let easl_source = fs::read_to_string(&format!("./data/{filename}.easl"))
      .expect(&format!("Unable to read {filename}.easl"));
    match compile_easl_source_to_wgsl(&easl_source) {
      Ok(wgsl) => {
        println!("{:?}", t.elapsed());
        total_time += t.elapsed().as_nanos() as f64 / 1000000.;
        fs::write(&format!("./out/{filename}.wgsl"), wgsl)
          .expect("Unable to write file");
      }
      Err(e) => {
        fs::write(&format!("./out/_failure.txt"), format!("{e:#?}"))
          .expect("Unable to write file");
        println!("failed!\n");
      }
    }
  }
  println!("total time: {total_time}");
  /*println!(
    "{:?}",
    eval(
      Exp {
        data: TypeState::Known(Type::F32).into(),
        source_trace: SourceTrace::empty(),
        kind: ExpKind::Application(
          Exp {
            data: TypeState::Known(Type::Function(
              FunctionSignature {
                abstract_ancestor: None,
                arg_types: vec![
                  (TypeState::Known(Type::F32).into(), vec![]),
                  (TypeState::Known(Type::F32).into(), vec![])
                ],
                mutated_args: vec![],
                return_type: TypeState::Known(Type::F32).into()
              }
              .into()
            ))
            .into(),
            source_trace: SourceTrace::empty(),
            kind: ExpKind::Name("+".into())
          }
          .into(),
          vec![
            Exp {
              data: TypeState::Known(Type::F32).into(),
              kind: ExpKind::NumberLiteral(Number::Float(1.)),
              source_trace: SourceTrace::empty()
            },
            Exp {
              data: TypeState::Known(Type::F32).into(),
              kind: ExpKind::NumberLiteral(Number::Float(2.)),
              source_trace: SourceTrace::empty()
            }
          ]
        )
      },
      &mut EvaluationEnvironment::from_program(Program::default())
    )
  );*/
}
