use std::collections::HashMap;

use super::{
  functions::FunctionSignature,
  types::{Context, TyntType, TypeState},
};

pub fn built_in_structs() -> Vec<String> {
  vec![
    "vec2f".to_string(),
    "vec3f".to_string(),
    "vec4f".to_string(),
  ]
}

pub fn built_in_functions() -> Vec<(&'static str, FunctionSignature)> {
  vec![(
    "vec4f",
    FunctionSignature {
      arg_types: vec![
        TyntType::F32,
        TyntType::F32,
        TyntType::F32,
        TyntType::F32,
      ],
      return_type: TyntType::Struct("vec4f".to_string()),
    },
  )]
}
