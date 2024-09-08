use std::collections::HashMap;

use super::{structs::Struct, types::Context};

pub fn built_in_structs() -> Vec<String> {
  vec![
    "vec2f".to_string(),
    "vec3f".to_string(),
    "vec4f".to_string(),
  ]
}

impl Context {
  pub fn default_global() -> Self {
    //todo!() need to add all the built-in functions here, e.g. +, vec4
    Context {
      bindings: HashMap::new(),
    }
  }
}
