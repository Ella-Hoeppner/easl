use std::fs;
use tynt::compiler::compile_tynt;

fn main() {
  let tynt_source = fs::read_to_string("./data/simple.tynt")
    .expect("Unable to read simple.tynt");
  let wgsl = compile_tynt(&tynt_source).expect("failed to compile tynt source");
  fs::create_dir_all("./out/").expect("Unable to create out directory");
  fs::write("./out/compiled.wgsl", wgsl).expect("Unable to write file");
}
