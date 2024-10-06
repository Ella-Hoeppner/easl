use crate::compiler::types::TypeState;

use super::{
  functions::AbstractFunctionSignature,
  structs::{Struct, StructField},
  types::Type,
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

fn vec_n_type(n: u8) -> Type {
  match n {
    1 => Type::F32,
    2 => Type::Struct(get_builtin_struct("vec2f")),
    3 => Type::Struct(get_builtin_struct("vec3f")),
    4 => Type::Struct(get_builtin_struct("vec4f")),
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

pub fn built_in_structs() -> Vec<Struct> {
  vec![
    Struct {
      name: "vec2f".to_string(),
      fields: vec![
        StructField {
          metadata: None,
          name: "x".to_string(),
          field_type: TypeState::Known(Type::F32),
        },
        StructField {
          metadata: None,
          name: "y".to_string(),
          field_type: TypeState::Known(Type::F32),
        },
      ],
      generic_args: vec![],
    },
    Struct {
      name: "vec3f".to_string(),
      fields: vec![
        StructField {
          metadata: None,
          name: "x".to_string(),
          field_type: TypeState::Known(Type::F32),
        },
        StructField {
          metadata: None,
          name: "y".to_string(),
          field_type: TypeState::Known(Type::F32),
        },
        StructField {
          metadata: None,
          name: "z".to_string(),
          field_type: TypeState::Known(Type::F32),
        },
      ],
      generic_args: vec![],
    },
    Struct {
      name: "vec4f".to_string(),
      fields: vec![
        StructField {
          metadata: None,
          name: "x".to_string(),
          field_type: TypeState::Known(Type::F32),
        },
        StructField {
          metadata: None,
          name: "y".to_string(),
          field_type: TypeState::Known(Type::F32),
        },
        StructField {
          metadata: None,
          name: "z".to_string(),
          field_type: TypeState::Known(Type::F32),
        },
        StructField {
          metadata: None,
          name: "w".to_string(),
          field_type: TypeState::Known(Type::F32),
        },
      ],
      generic_args: vec![],
    },
  ]
}

pub fn get_builtin_struct(name: &str) -> Struct {
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
        arg_types: vec![Type::Bool, Type::Bool],
        return_type: Type::Bool,
      },
    ),
    (
      "==",
      AbstractFunctionSignature {
        generic_args: vec!["T".to_string()],
        arg_types: vec![
          Type::GenericVariable("T".to_string()),
          Type::GenericVariable("T".to_string()),
        ],
        return_type: Type::Bool,
      },
    ),
    (
      "=",
      AbstractFunctionSignature {
        generic_args: vec!["T".to_string()],
        arg_types: vec![
          Type::GenericVariable("T".to_string()),
          Type::GenericVariable("T".to_string()),
        ],
        return_type: Type::None,
      },
    ),
  ]
}

pub const ASSIGNMENT_OPS: [&'static str; 5] = ["=", "+=", "-=", "*=", "/="];

pub const INFIX_OPS: [&'static str; 7] = ["==", "||", "&&", "+", "-", "*", "/"];

pub const ABNORMAL_CONSTRUCTOR_STRUCTS: [&'static str; 3] =
  ["vec2f", "vec3f", "vec4f"];
