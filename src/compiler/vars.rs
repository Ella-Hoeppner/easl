use super::{metadata::Metadata, types::TyntType};

pub struct TopLevelVar {
  pub name: String,
  pub metadata: Option<Metadata>,
  pub attributes: Vec<String>,
  pub var_type: TyntType,
}

impl TopLevelVar {
  pub fn compile(self) -> String {
    todo!()
  }
}
