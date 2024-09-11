use crate::compiler::structs::StructField;

use super::{functions::FunctionSignature, structs::Struct, types::TyntType};

fn n_sums(n: u8) -> Vec<Vec<u8>> {
  let mut matches = vec![];
  let mut stack = vec![1];
  loop {
    let sum = stack.iter().copied().reduce(|a, b| a + b).unwrap();
    if sum == n {
      matches.push(stack.clone());
      stack.pop();
      if stack.is_empty() {
        break matches;
      } else {
        *stack.last_mut().unwrap() += 1;
      }
    } else {
      stack.push(1);
    }
  }
}

fn multi_signature_vec_constructors(n: u8) -> Vec<FunctionSignature> {
  n_sums(n)
    .into_iter()
    .map(|nums| FunctionSignature {
      arg_types: nums
        .into_iter()
        .map(|n| match n {
          1 => TyntType::F32,
          2 => TyntType::Struct("vec2f".to_string()),
          3 => TyntType::Struct("vec3f".to_string()),
          4 => TyntType::Struct("vec4f".to_string()),
          _ => unreachable!(),
        })
        .collect(),
      return_type: TyntType::Struct(format!("vec{n}f")),
    })
    .collect()
}

pub fn built_in_structs() -> Vec<Struct> {
  vec![
    Struct {
      has_normal_constructor: false,
      name: "vec2f".to_string(),
      fields: vec![
        StructField {
          metadata: None,
          name: "x".to_string(),
          field_type: TyntType::F32,
        },
        StructField {
          metadata: None,
          name: "y".to_string(),
          field_type: TyntType::F32,
        },
      ],
    },
    Struct {
      has_normal_constructor: false,
      name: "vec3f".to_string(),
      fields: vec![
        StructField {
          metadata: None,
          name: "x".to_string(),
          field_type: TyntType::F32,
        },
        StructField {
          metadata: None,
          name: "y".to_string(),
          field_type: TyntType::F32,
        },
        StructField {
          metadata: None,
          name: "z".to_string(),
          field_type: TyntType::F32,
        },
      ],
    },
    Struct {
      has_normal_constructor: false,
      name: "vec4f".to_string(),
      fields: vec![
        StructField {
          metadata: None,
          name: "x".to_string(),
          field_type: TyntType::F32,
        },
        StructField {
          metadata: None,
          name: "y".to_string(),
          field_type: TyntType::F32,
        },
        StructField {
          metadata: None,
          name: "z".to_string(),
          field_type: TyntType::F32,
        },
        StructField {
          metadata: None,
          name: "w".to_string(),
          field_type: TyntType::F32,
        },
      ],
    },
  ]
}

pub fn built_in_multi_signature_functions(
) -> Vec<(&'static str, Vec<FunctionSignature>)> {
  vec![
    ("vec4f", multi_signature_vec_constructors(4)),
    ("vec3f", multi_signature_vec_constructors(3)),
    ("vec2f", multi_signature_vec_constructors(2)),
  ]
}

pub fn built_in_functions() -> Vec<(&'static str, FunctionSignature)> {
  vec![]
}
