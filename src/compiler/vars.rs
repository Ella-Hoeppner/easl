use crate::compiler::util::compile_word;

use super::{metadata::Metadata, types::Type};

#[derive(Debug)]
pub struct TopLevelVar {
  pub name: String,
  pub metadata: Option<Metadata>,
  pub attributes: Vec<String>,
  pub var_type: Type,
}

impl TopLevelVar {
  pub fn compile(self) -> String {
    let metadata = Metadata::compile_optional(self.metadata);
    let attributes = if self.attributes.is_empty() {
      String::new()
    } else {
      format!("<{}>", self.attributes.join(", "))
    };
    let name = compile_word(self.name);
    let var_type = self.var_type.compile();
    format!("{metadata}var{attributes} {name}: {var_type}")
  }
}
