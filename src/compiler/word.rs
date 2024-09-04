pub fn tynt_word_to_wgsl_word(word: String) -> String {
  word.replace("-", "_")
}
