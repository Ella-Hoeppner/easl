use std::fs;
use tynt::compiler::compile_tynt_to_wgsl;

fn main() {
  std::env::set_var("RUST_BACKTRACE", "1");
  fs::create_dir_all("./out/").expect("Unable to create out directory");
  for filename in [
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
    //"specialized_vec_constructor",
    "generic_constraint",
  ] {
    println!("compiling {filename}...");
    let tynt_source = fs::read_to_string(&format!("./data/{filename}.tynt"))
      .expect(&format!("Unable to read {filename}.tynt"));
    match compile_tynt_to_wgsl(&tynt_source) {
      Ok(wgsl) => {
        fs::write(&format!("./out/{filename}.wgsl"), wgsl)
          .expect("Unable to write file");
      }
      Err(e) => {
        fs::write(&format!("./out/_failure.txt"), format!("{e:#?}"))
          .expect("Unable to write file");
        println!("   failed!\n");
      }
    }
  }
}
