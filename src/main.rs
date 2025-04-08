use easl::{compiler::compile_easl_source_to_wgsl, parse::parse_easl};
use std::fs;

fn main() {
  println!("{:?}", parse_easl("@a b: c").unwrap().syntax_trees[0]);
  std::env::set_var("RUST_BACKTRACE", "1");
  fs::create_dir_all("./out/").expect("Unable to create out directory");
  for filename in [
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
  ] {
    print!("compiling {filename}...");
    let t = std::time::Instant::now();
    let easl_source = fs::read_to_string(&format!("./data/{filename}.easl"))
      .expect(&format!("Unable to read {filename}.easl"));
    match compile_easl_source_to_wgsl(&easl_source) {
      Ok(wgsl) => {
        println!("{:?}", t.elapsed());
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
}
