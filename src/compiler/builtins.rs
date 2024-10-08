use std::collections::HashMap;

use crate::compiler::structs::{AbstractStructField, TypeOrAbstractStruct};

use super::{
  functions::AbstractFunctionSignature,
  structs::AbstractStruct,
  types::{GenericOr, Type},
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
  let empty_generics = HashMap::new();
  match n {
    1 => Type::F32,
    2 => {
      Type::Struct(get_builtin_struct("vec2f").fill_generics(&empty_generics))
    }
    3 => {
      Type::Struct(get_builtin_struct("vec3f").fill_generics(&empty_generics))
    }
    4 => {
      Type::Struct(get_builtin_struct("vec4f").fill_generics(&empty_generics))
    }
    _ => unreachable!(),
  }
}

fn multi_signature_vec_constructors(n: u8) -> Vec<AbstractFunctionSignature> {
  n_sums(n)
    .into_iter()
    .map(|nums| AbstractFunctionSignature {
      generic_args: vec![],
      arg_types: nums
        .into_iter()
        .map(|n| {
          GenericOr::NonGeneric(TypeOrAbstractStruct::Type(vec_n_type(n)))
        })
        .collect(),
      return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
        vec_n_type(n),
      )),
    })
    .collect()
}

pub fn built_in_structs() -> Vec<AbstractStruct> {
  vec![
    AbstractStruct {
      name: "vec2f".to_string(),
      fields: vec![
        AbstractStructField {
          metadata: None,
          name: "x".to_string(),
          field_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
            Type::F32,
          )),
        },
        AbstractStructField {
          metadata: None,
          name: "y".to_string(),
          field_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
            Type::F32,
          )),
        },
      ],
      generic_args: vec![],
    },
    AbstractStruct {
      name: "vec3f".to_string(),
      fields: vec![
        AbstractStructField {
          metadata: None,
          name: "x".to_string(),
          field_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
            Type::F32,
          )),
        },
        AbstractStructField {
          metadata: None,
          name: "y".to_string(),
          field_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
            Type::F32,
          )),
        },
        AbstractStructField {
          metadata: None,
          name: "z".to_string(),
          field_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
            Type::F32,
          )),
        },
      ],
      generic_args: vec![],
    },
    AbstractStruct {
      name: "vec4f".to_string(),
      fields: vec![
        AbstractStructField {
          metadata: None,
          name: "x".to_string(),
          field_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
            Type::F32,
          )),
        },
        AbstractStructField {
          metadata: None,
          name: "y".to_string(),
          field_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
            Type::F32,
          )),
        },
        AbstractStructField {
          metadata: None,
          name: "z".to_string(),
          field_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
            Type::F32,
          )),
        },
        AbstractStructField {
          metadata: None,
          name: "w".to_string(),
          field_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
            Type::F32,
          )),
        },
      ],
      generic_args: vec![],
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
        arg_types: vec![
          GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::Bool)),
          GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::Bool)),
        ],
        return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
          Type::Bool,
        )),
      },
    ),
    (
      "==",
      AbstractFunctionSignature {
        generic_args: vec!["T".to_string()],
        arg_types: vec![
          GenericOr::Generic("T".to_string()),
          GenericOr::Generic("T".to_string()),
        ],
        return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
          Type::Bool,
        )),
      },
    ),
    (
      "=",
      AbstractFunctionSignature {
        generic_args: vec!["T".to_string()],
        arg_types: vec![
          GenericOr::Generic("T".to_string()),
          GenericOr::Generic("T".to_string()),
        ],
        return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
          Type::None,
        )),
      },
    ),
  ]
}

pub const ASSIGNMENT_OPS: [&'static str; 5] = ["=", "+=", "-=", "*=", "/="];

pub const INFIX_OPS: [&'static str; 7] = ["==", "||", "&&", "+", "-", "*", "/"];

pub const ABNORMAL_CONSTRUCTOR_STRUCTS: [&'static str; 3] =
  ["vec2f", "vec3f", "vec4f"];
