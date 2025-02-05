use crate::compiler::{
  builtins::built_in_macros,
  error::CompileResult,
  program::{EaslDocument, Program},
};

fn derive_struct(doc: EaslDocument, struct_name: &str) -> String {
  let mut program =
    Program::from_easl_document(&doc, built_in_macros()).unwrap();
  program.process_raw_program().unwrap();
  let s = program
    .global_context
    .structs
    .iter()
    .find(|s| &*s.name == struct_name)
    .unwrap();
  todo!()
}
