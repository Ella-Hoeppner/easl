use crate::compiler::structs::{AbstractStructField, TypeOrAbstractStruct};

use super::{
  functions::{AbstractFunctionSignature, FunctionImplementationKind},
  structs::AbstractStruct,
  types::{AbstractType, GenericOr, Type},
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

fn vec_n_type(n: u8) -> AbstractType {
  match n {
    1 => GenericOr::Generic("T".to_string()),
    2 => GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      get_builtin_struct("vec2"),
    )),
    3 => GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      get_builtin_struct("vec3"),
    )),
    4 => GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      get_builtin_struct("vec4"),
    )),
    _ => unreachable!(),
  }
}

fn multi_signature_vec_constructors(n: u8) -> Vec<AbstractFunctionSignature> {
  n_sums(n)
    .into_iter()
    .map(|nums| AbstractFunctionSignature {
      name: format!("vec{n}"),
      generic_args: (0..nums.len())
        .map(|i| format!("T{i}"))
        .chain(std::iter::once("T".to_string()))
        .map(|name| (name, vec![]))
        .collect(),
      arg_types: nums
        .into_iter()
        .enumerate()
        .map(|(i, n)| vec_n_type(n).rename_generic("T", &format!("T{i}")))
        .collect(),
      return_type: vec_n_type(n),
      implementation: FunctionImplementationKind::Builtin,
    })
    .collect()
}

pub fn vec2() -> AbstractStruct {
  AbstractStruct {
    name: "vec2".to_string(),
    fields: vec![
      AbstractStructField {
        metadata: None,
        name: "x".to_string(),
        field_type: GenericOr::Generic("T".to_string()),
      },
      AbstractStructField {
        metadata: None,
        name: "y".to_string(),
        field_type: GenericOr::Generic("T".to_string()),
      },
    ],
    generic_args: vec!["T".to_string()],
    filled_generics: vec![],
    abstract_ancestor: None,
  }
}

pub fn vec3() -> AbstractStruct {
  AbstractStruct {
    name: "vec3".to_string(),
    fields: vec![
      AbstractStructField {
        metadata: None,
        name: "x".to_string(),
        field_type: GenericOr::Generic("T".to_string()),
      },
      AbstractStructField {
        metadata: None,
        name: "y".to_string(),
        field_type: GenericOr::Generic("T".to_string()),
      },
      AbstractStructField {
        metadata: None,
        name: "z".to_string(),
        field_type: GenericOr::Generic("T".to_string()),
      },
    ],
    generic_args: vec!["T".to_string()],
    filled_generics: vec![],
    abstract_ancestor: None,
  }
}

pub fn vec4() -> AbstractStruct {
  AbstractStruct {
    name: "vec4".to_string(),
    fields: vec![
      AbstractStructField {
        metadata: None,
        name: "x".to_string(),
        field_type: GenericOr::Generic("T".to_string()),
      },
      AbstractStructField {
        metadata: None,
        name: "y".to_string(),
        field_type: GenericOr::Generic("T".to_string()),
      },
      AbstractStructField {
        metadata: None,
        name: "z".to_string(),
        field_type: GenericOr::Generic("T".to_string()),
      },
      AbstractStructField {
        metadata: None,
        name: "w".to_string(),
        field_type: GenericOr::Generic("T".to_string()),
      },
    ],
    generic_args: vec!["T".to_string()],
    filled_generics: vec![],
    abstract_ancestor: None,
  }
}

pub fn built_in_structs() -> Vec<AbstractStruct> {
  vec![vec2(), vec3(), vec4()]
}

pub fn built_in_type_aliases() -> Vec<(String, AbstractStruct)> {
  vec![
    (
      "vec2f".to_string(),
      vec2().generate_monomorphized(vec![Type::F32]).unwrap(),
    ),
    (
      "vec3f".to_string(),
      vec3().generate_monomorphized(vec![Type::F32]).unwrap(),
    ),
    (
      "vec4f".to_string(),
      vec4().generate_monomorphized(vec![Type::F32]).unwrap(),
    ),
    (
      "vec2i".to_string(),
      vec2().generate_monomorphized(vec![Type::I32]).unwrap(),
    ),
    (
      "vec3i".to_string(),
      vec3().generate_monomorphized(vec![Type::I32]).unwrap(),
    ),
    (
      "vec4i".to_string(),
      vec4().generate_monomorphized(vec![Type::I32]).unwrap(),
    ),
    (
      "vec2u".to_string(),
      vec2().generate_monomorphized(vec![Type::U32]).unwrap(),
    ),
    (
      "vec3u".to_string(),
      vec3().generate_monomorphized(vec![Type::U32]).unwrap(),
    ),
    (
      "vec4u".to_string(),
      vec4().generate_monomorphized(vec![Type::U32]).unwrap(),
    ),
  ]
}

pub fn get_builtin_struct(name: &str) -> AbstractStruct {
  built_in_structs()
    .into_iter()
    .find(|s| s.name == name)
    .unwrap()
}

pub fn built_in_functions() -> Vec<AbstractFunctionSignature> {
  let mut signatures = vec![
    AbstractFunctionSignature {
      name: "&&".to_string(),
      generic_args: vec![],
      arg_types: vec![
        GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::Bool)),
        GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::Bool)),
      ],
      return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
        Type::Bool,
      )),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "==".to_string(),
      generic_args: vec![("T".to_string(), vec![])],
      arg_types: vec![
        GenericOr::Generic("T".to_string()),
        GenericOr::Generic("T".to_string()),
      ],
      return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
        Type::Bool,
      )),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "=".to_string(),
      generic_args: vec![("T".to_string(), vec![])],
      arg_types: vec![
        GenericOr::Generic("T".to_string()),
        GenericOr::Generic("T".to_string()),
      ],
      return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
        Type::None,
      )),
      implementation: FunctionImplementationKind::Builtin,
    },
  ];
  signatures.append(&mut multi_signature_vec_constructors(4));
  signatures.append(&mut multi_signature_vec_constructors(3));
  signatures.append(&mut multi_signature_vec_constructors(2));
  signatures
}

pub const ASSIGNMENT_OPS: [&'static str; 5] = ["=", "+=", "-=", "*=", "/="];

pub const INFIX_OPS: [&'static str; 7] = ["==", "||", "&&", "+", "-", "*", "/"];

pub const ABNORMAL_CONSTRUCTOR_STRUCTS: [&'static str; 3] =
  ["vec2", "vec3", "vec4"];
