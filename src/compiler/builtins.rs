use crate::compiler::structs::AbstractStructField;

use super::{
  functions::AbstractFunctionSignature,
  structs::{AbstractStruct, ConcreteStruct},
  types::TyntType,
};

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

fn multi_signature_vec_constructors(n: u8) -> Vec<AbstractFunctionSignature> {
  n_sums(n)
    .into_iter()
    .map(|nums| AbstractFunctionSignature {
      generic_args: vec![],
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

pub fn built_in_structs() -> Vec<AbstractStruct> {
  vec![
    AbstractStruct {
      name: "vec2f".to_string(),
      generic_args: vec![],
      fields: vec![
        AbstractStructField {
          metadata: None,
          name: "x".to_string(),
          field_type: TyntType::F32,
        },
        AbstractStructField {
          metadata: None,
          name: "y".to_string(),
          field_type: TyntType::F32,
        },
      ],
    },
    AbstractStruct {
      name: "vec3f".to_string(),
      generic_args: vec![],
      fields: vec![
        AbstractStructField {
          metadata: None,
          name: "x".to_string(),
          field_type: TyntType::F32,
        },
        AbstractStructField {
          metadata: None,
          name: "y".to_string(),
          field_type: TyntType::F32,
        },
        AbstractStructField {
          metadata: None,
          name: "z".to_string(),
          field_type: TyntType::F32,
        },
      ],
    },
    AbstractStruct {
      name: "vec4f".to_string(),
      generic_args: vec![],
      fields: vec![
        AbstractStructField {
          metadata: None,
          name: "x".to_string(),
          field_type: TyntType::F32,
        },
        AbstractStructField {
          metadata: None,
          name: "y".to_string(),
          field_type: TyntType::F32,
        },
        AbstractStructField {
          metadata: None,
          name: "z".to_string(),
          field_type: TyntType::F32,
        },
        AbstractStructField {
          metadata: None,
          name: "w".to_string(),
          field_type: TyntType::F32,
        },
      ],
    },
  ]
}

pub fn built_in_multi_signature_functions(
) -> Vec<(&'static str, Vec<AbstractFunctionSignature>)> {
  vec![
    ("vec4f", multi_signature_vec_constructors(4)),
    ("vec3f", multi_signature_vec_constructors(3)),
    ("vec2f", multi_signature_vec_constructors(2)),
  ]
}

pub fn built_in_functions() -> Vec<(&'static str, AbstractFunctionSignature)> {
  vec![
    (
      "&&",
      AbstractFunctionSignature {
        generic_args: vec![],
        arg_types: vec![TyntType::Bool, TyntType::Bool],
        return_type: TyntType::Bool,
      },
    ),
    (
      "==",
      AbstractFunctionSignature {
        generic_args: vec!["T".to_string()],
        arg_types: vec![
          TyntType::GenericVariable("T".to_string()),
          TyntType::GenericVariable("T".to_string()),
        ],
        return_type: TyntType::Bool,
      },
    ),
    (
      "=",
      AbstractFunctionSignature {
        generic_args: vec!["T".to_string()],
        arg_types: vec![
          TyntType::GenericVariable("T".to_string()),
          TyntType::GenericVariable("T".to_string()),
        ],
        return_type: TyntType::None,
      },
    ),
  ]
}

pub const ASSIGNMENT_OPS: [&'static str; 5] = ["=", "+=", "-=", "*=", "/="];

pub const INFIX_OPS: [&'static str; 7] = ["==", "||", "&&", "+", "-", "*", "/"];

pub const ABNORMAL_CONSTRUCTOR_STRUCTS: [&'static str; 3] =
  ["vec2f", "vec3f", "vec4f"];
