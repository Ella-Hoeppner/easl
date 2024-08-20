use crate::parse::TyntTree;

use super::error::CompileError;

pub fn tynt_word_to_wgsl_word(word: String) -> String {
  word.replace("-", "_")
}

pub fn compile_word(expression: TyntTree) -> Result<String, CompileError> {
  match expression {
    TyntTree::Leaf(_, s) => Ok(tynt_word_to_wgsl_word(s)),
    other => {
      println!("{other:?}");
      Err(CompileError::ExpectedWord)
    }
  }
}
