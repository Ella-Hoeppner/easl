use std::fs;
use tynt::compiler::compile_tynt_to_wgsl;

fn main() {
  std::env::set_var("RUST_BACKTRACE", "1");
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
  ] {
    println!("compiling {filename}...");
    let tynt_source = fs::read_to_string(&format!("./data/{filename}.tynt"))
      .expect(&format!("Unable to read {filename}.tynt"));
    match compile_tynt_to_wgsl(&tynt_source) {
      Ok(wgsl) => {
        fs::create_dir_all("./out/").expect("Unable to create out directory");
        fs::write(&format!("./out/{filename}_compiled.wgsl"), wgsl)
          .expect("Unable to write file");
      }
      Err(e) => {
        panic!("failed to compile tynt source\n{e:#?}");
      }
    }
  }
}
