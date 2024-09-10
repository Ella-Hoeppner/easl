use std::fs;
use tynt::compiler::compile_tynt_to_wgsl;

fn main() {
  for filename in ["simplest"] {
    let tynt_source = fs::read_to_string(&format!("./data/{filename}.tynt"))
      .expect(&format!("Unable to read {filename}.tynt"));
    let wgsl = compile_tynt_to_wgsl(&tynt_source)
      .expect("failed to compile tynt source");
    fs::create_dir_all("./out/").expect("Unable to create out directory");
    fs::write(&format!("./out/{filename}_compiled.wgsl"), wgsl)
      .expect("Unable to write file");
  }
}
