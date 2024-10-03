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

fn vec_n_type(n: u8) -> TyntType {
  match n {
    1 => TyntType::F32,
    2 => TyntType::AbstractStruct(get_builtin_struct("vec2f")),
    3 => TyntType::AbstractStruct(get_builtin_struct("vec3f")),
    4 => TyntType::AbstractStruct(get_builtin_struct("vec4f")),
    _ => unreachable!(),
  }
}

fn multi_signature_vec_constructors(n: u8) -> Vec<AbstractFunctionSignature> {
  n_sums(n)
    .into_iter()
    .map(|nums| AbstractFunctionSignature {
      generic_args: vec![],
      arg_types: nums.into_iter().map(vec_n_type).collect(),
      return_type: vec_n_type(n),
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

pub fn get_builtin_struct(name: &str) -> AbstractStruct {
  built_in_structs()
    .into_iter()
    .find(|s| s.name == name)
    .unwrap()
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
