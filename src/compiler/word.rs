use crate::parse::TyntTree;

use super::error::CompileError;

pub fn tynt_word_to_wgsl_word(word: String) -> String {
  word.replace("-", "_")
}

pub fn compile_word(form: TyntTree) -> Result<String, CompileError> {
  match form {
    TyntTree::Leaf(_, s) => Ok(tynt_word_to_wgsl_word(s)),
    _ => Err(CompileError::ExpectedWord),
  }
}
