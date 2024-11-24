use std::collections::HashMap;

use sse::{document::DocumentPosition, syntax::EncloserOrOperator};

use crate::{
  compiler::{
    error::SourceTrace,
    structs::{AbstractStructField, TypeOrAbstractStruct},
  },
  parse::{EaslTree, Encloser},
};

use super::{
  functions::{AbstractFunctionSignature, FunctionImplementationKind},
  macros::Macro,
  structs::AbstractStruct,
  types::{AbstractType, GenericOr, Type, TypeConstraint},
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

fn specialized_vec_n_type(n: u8, suffix: &str) -> AbstractType {
  let vec_type =
    GenericOr::NonGeneric(TypeOrAbstractStruct::Type(match suffix {
      "f" => Type::F32,
      "i" => Type::I32,
      "u" => Type::U32,
      _ => panic!("unknown specialized vec suffix \"{suffix}\""),
    }));
  match n {
    1 => vec_type,
    2 => GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      get_builtin_struct("vec2").partially_fill_abstract_generics(
        [("T".to_string(), vec_type)].into_iter().collect(),
      ),
    )),
    3 => GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      get_builtin_struct("vec3").partially_fill_abstract_generics(
        [("T".to_string(), vec_type)].into_iter().collect(),
      ),
    )),
    4 => GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      get_builtin_struct("vec4").partially_fill_abstract_generics(
        [("T".to_string(), vec_type)].into_iter().collect(),
      ),
    )),
    _ => unreachable!(),
  }
}

fn multi_signature_vec_constructors(n: u8) -> Vec<AbstractFunctionSignature> {
  n_sums(n)
    .into_iter()
    .chain(std::iter::once(vec![1]))
    .map(|nums| {
      vec![
        AbstractFunctionSignature {
          name: format!("vec{n}"),
          generic_args: (0..nums.len())
            .map(|i| format!("A{i}"))
            .chain(std::iter::once("T".to_string()))
            .map(|name| (name, vec![TypeConstraint::scalar()]))
            .collect(),
          arg_types: nums
            .iter()
            .copied()
            .enumerate()
            .map(|(i, n)| vec_n_type(n).rename_generic("T", &format!("A{i}")))
            .collect(),
          return_type: vec_n_type(n),
          implementation: FunctionImplementationKind::Builtin,
        },
        AbstractFunctionSignature {
          name: format!("vec{n}f"),
          generic_args: (0..nums.len())
            .map(|i| format!("A{i}"))
            .map(|name| (name, vec![TypeConstraint::scalar()]))
            .collect(),
          arg_types: nums
            .iter()
            .copied()
            .enumerate()
            .map(|(i, n)| vec_n_type(n).rename_generic("T", &format!("A{i}")))
            .collect(),
          return_type: specialized_vec_n_type(n, "f"),
          implementation: FunctionImplementationKind::Builtin,
        },
        AbstractFunctionSignature {
          name: format!("vec{n}i"),
          generic_args: (0..nums.len())
            .map(|i| format!("A{i}"))
            .map(|name| (name, vec![TypeConstraint::scalar()]))
            .collect(),
          arg_types: nums
            .iter()
            .copied()
            .enumerate()
            .map(|(i, n)| vec_n_type(n).rename_generic("T", &format!("A{i}")))
            .collect(),
          return_type: specialized_vec_n_type(n, "i"),
          implementation: FunctionImplementationKind::Builtin,
        },
        AbstractFunctionSignature {
          name: format!("vec{n}u"),
          generic_args: (0..nums.len())
            .map(|i| format!("A{i}"))
            .map(|name| (name, vec![TypeConstraint::scalar()]))
            .collect(),
          arg_types: nums
            .iter()
            .copied()
            .enumerate()
            .map(|(i, n)| vec_n_type(n).rename_generic("T", &format!("A{i}")))
            .collect(),
          return_type: specialized_vec_n_type(n, "u"),
          implementation: FunctionImplementationKind::Builtin,
        },
      ]
    })
    .flatten()
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
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
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
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
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
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
  }
}

pub fn texture_2d() -> AbstractStruct {
  AbstractStruct {
    name: "Texture2D".to_string(),
    fields: vec![AbstractStructField {
      metadata: None,
      name: "_".to_string(),
      field_type: GenericOr::Generic("T".to_string()),
    }],
    generic_args: vec!["T".to_string()],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
  }
}

pub fn sampler() -> AbstractStruct {
  AbstractStruct {
    name: "sampler".to_string(),
    fields: vec![],
    generic_args: vec![],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
  }
}

pub fn built_in_structs() -> Vec<AbstractStruct> {
  vec![vec2(), vec3(), vec4(), texture_2d(), sampler()]
}

pub fn built_in_type_aliases() -> Vec<(String, AbstractStruct)> {
  vec![
    (
      "vec2f".to_string(),
      vec2()
        .generate_monomorphized(vec![Type::F32], SourceTrace::empty())
        .unwrap(),
    ),
    (
      "vec3f".to_string(),
      vec3()
        .generate_monomorphized(vec![Type::F32], SourceTrace::empty())
        .unwrap(),
    ),
    (
      "vec4f".to_string(),
      vec4()
        .generate_monomorphized(vec![Type::F32], SourceTrace::empty())
        .unwrap(),
    ),
    (
      "vec2i".to_string(),
      vec2()
        .generate_monomorphized(vec![Type::I32], SourceTrace::empty())
        .unwrap(),
    ),
    (
      "vec3i".to_string(),
      vec3()
        .generate_monomorphized(vec![Type::I32], SourceTrace::empty())
        .unwrap(),
    ),
    (
      "vec4i".to_string(),
      vec4()
        .generate_monomorphized(vec![Type::I32], SourceTrace::empty())
        .unwrap(),
    ),
    (
      "vec2u".to_string(),
      vec2()
        .generate_monomorphized(vec![Type::U32], SourceTrace::empty())
        .unwrap(),
    ),
    (
      "vec3u".to_string(),
      vec3()
        .generate_monomorphized(vec![Type::U32], SourceTrace::empty())
        .unwrap(),
    ),
    (
      "vec4u".to_string(),
      vec4()
        .generate_monomorphized(vec![Type::U32], SourceTrace::empty())
        .unwrap(),
    ),
  ]
}

pub fn get_builtin_struct(name: &str) -> AbstractStruct {
  built_in_structs()
    .into_iter()
    .find(|s| s.name == name)
    .unwrap()
}

fn arithmetic_functions(name: &str) -> Vec<AbstractFunctionSignature> {
  let assignment_name = format!("{}=", name);
  vec![
    AbstractFunctionSignature {
      name: name.to_string(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        GenericOr::Generic("T".to_string()),
        GenericOr::Generic("T".to_string()),
      ],
      return_type: GenericOr::Generic("T".to_string()),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: assignment_name.clone(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        GenericOr::Generic("T".to_string()),
        GenericOr::Generic("T".to_string()),
      ],
      return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
        Type::None,
      )),
      implementation: FunctionImplementationKind::Builtin,
    },
  ]
  .into_iter()
  .chain(foreach_vec_type(|vec| {
    [
      (false, vec![[true, true], [true, false], [false, true]]),
      (true, vec![[true, true], [true, false]]),
    ]
    .into_iter()
    .map(|(assignment_fn, arg_vecs_or_scalars)| {
      arg_vecs_or_scalars
        .into_iter()
        .map(|arg_vecs_or_scalars| AbstractFunctionSignature {
          name: if assignment_fn {
            assignment_name.clone()
          } else {
            name.to_string()
          },
          generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
          arg_types: arg_vecs_or_scalars
            .into_iter()
            .map(|vec_or_scalar| {
              if vec_or_scalar {
                GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
                  vec.clone(),
                ))
              } else {
                GenericOr::Generic("T".to_string())
              }
            })
            .collect(),
          return_type: if assignment_fn {
            GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::None))
          } else {
            GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
              vec.clone(),
            ))
          },
          implementation: FunctionImplementationKind::Builtin,
        })
        .collect::<Vec<_>>()
    })
    .flatten()
    .collect()
  }))
  .collect()
}

fn trigonometry_functions() -> Vec<AbstractFunctionSignature> {
  vec![AbstractFunctionSignature {
    name: "atan2".to_string(),
    generic_args: vec![],
    arg_types: vec![
      GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::F32)),
      GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::F32)),
    ],
    return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::F32)),
    implementation: FunctionImplementationKind::Builtin,
  }]
}

fn exp_functions() -> Vec<AbstractFunctionSignature> {
  vec![AbstractFunctionSignature {
    name: "log".to_string(),
    generic_args: vec![],
    arg_types: vec![GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
      Type::F32,
    ))],
    return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::F32)),
    implementation: FunctionImplementationKind::Builtin,
  }]
}

fn negation_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "-".to_string(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
      arg_types: vec![GenericOr::Generic("T".to_string())],
      return_type: GenericOr::Generic("T".to_string()),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "-".to_string(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
      arg_types: vec![GenericOr::NonGeneric(
        TypeOrAbstractStruct::AbstractStruct(vec2()),
      )],
      return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
        vec2(),
      )),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "-".to_string(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
      arg_types: vec![GenericOr::NonGeneric(
        TypeOrAbstractStruct::AbstractStruct(vec3()),
      )],
      return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
        vec3(),
      )),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "-".to_string(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
      arg_types: vec![GenericOr::NonGeneric(
        TypeOrAbstractStruct::AbstractStruct(vec4()),
      )],
      return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
        vec4(),
      )),
      implementation: FunctionImplementationKind::Builtin,
    },
  ]
}

fn comparison_functions() -> Vec<AbstractFunctionSignature> {
  vec![
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
      name: ">=".to_string(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
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
      name: ">".to_string(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
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
      name: "<=".to_string(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
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
      name: "<".to_string(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        GenericOr::Generic("T".to_string()),
        GenericOr::Generic("T".to_string()),
      ],
      return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(
        Type::Bool,
      )),
      implementation: FunctionImplementationKind::Builtin,
    },
  ]
}

fn assignment_function() -> Vec<AbstractFunctionSignature> {
  vec![AbstractFunctionSignature {
    name: "=".to_string(),
    generic_args: vec![("T".to_string(), vec![])],
    arg_types: vec![
      GenericOr::Generic("T".to_string()),
      GenericOr::Generic("T".to_string()),
    ],
    return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::None)),
    implementation: FunctionImplementationKind::Builtin,
  }]
}

fn boolean_functions() -> Vec<AbstractFunctionSignature> {
  vec![
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
      name: "||".to_string(),
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
  ]
}

fn float_and_float_vec_types() -> Vec<AbstractType> {
  let f32 = AbstractType::NonGeneric(TypeOrAbstractStruct::Type(Type::F32));
  vec![
    f32.clone(),
    AbstractType::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      vec2().fill_abstract_generics(vec![f32.clone()]),
    )),
    AbstractType::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      vec3().fill_abstract_generics(vec![f32.clone()]),
    )),
    AbstractType::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      vec4().fill_abstract_generics(vec![f32]),
    )),
  ]
}

fn float_vec_types() -> Vec<AbstractType> {
  let f32 = AbstractType::NonGeneric(TypeOrAbstractStruct::Type(Type::F32));
  vec![
    AbstractType::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      vec2().fill_abstract_generics(vec![f32.clone()]),
    )),
    AbstractType::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      vec3().fill_abstract_generics(vec![f32.clone()]),
    )),
    AbstractType::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      vec4().fill_abstract_generics(vec![f32]),
    )),
  ]
}

fn generic_and_vec_types() -> Vec<AbstractType> {
  vec![
    AbstractType::Generic("T".to_string()),
    AbstractType::NonGeneric(TypeOrAbstractStruct::AbstractStruct(vec2())),
    AbstractType::NonGeneric(TypeOrAbstractStruct::AbstractStruct(vec3())),
    AbstractType::NonGeneric(TypeOrAbstractStruct::AbstractStruct(vec4())),
  ]
}

fn foreach_vec_type(
  f: impl Fn(AbstractStruct) -> Vec<AbstractFunctionSignature>,
) -> Vec<AbstractFunctionSignature> {
  [vec2(), vec3(), vec4()]
    .into_iter()
    .map(f)
    .flatten()
    .collect()
}

fn vector_functions() -> Vec<AbstractFunctionSignature> {
  foreach_vec_type(|vec| {
    let vec = GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
      vec
        .generate_monomorphized(vec![Type::F32], SourceTrace::empty())
        .unwrap(),
    ));
    let float = GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::F32));
    [
      ("length", vec![vec.clone()], float.clone()),
      ("distance", vec![vec.clone(), vec.clone()], float.clone()),
      ("normalize", vec![vec.clone()], vec.clone()),
      ("dot", vec![vec.clone(), vec.clone()], float.clone()),
    ]
    .into_iter()
    .map(|(name, arg_types, return_type)| AbstractFunctionSignature {
      name: name.to_string(),
      generic_args: vec![],
      arg_types,
      return_type,
      implementation: FunctionImplementationKind::Builtin,
    })
    .collect()
  })
}

fn scalar_conversion_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "i32".to_string(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
      arg_types: vec![GenericOr::Generic("T".to_string())],
      return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::I32)),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "f32".to_string(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
      arg_types: vec![GenericOr::Generic("T".to_string())],
      return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::F32)),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "u32".to_string(),
      generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
      arg_types: vec![GenericOr::Generic("T".to_string())],
      return_type: GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::U32)),
      implementation: FunctionImplementationKind::Builtin,
    },
  ]
}

fn misc_math_functions() -> Vec<AbstractFunctionSignature> {
  generic_and_vec_types()
    .into_iter()
    .map(|t| {
      [("abs", 1), ("max", 2), ("min", 2)]
        .into_iter()
        .map(|(name, arg_count)| AbstractFunctionSignature {
          name: name.to_string(),
          generic_args: vec![("T".to_string(), vec![TypeConstraint::scalar()])],
          arg_types: std::iter::repeat(t.clone()).take(arg_count).collect(),
          return_type: t.clone(),
          implementation: FunctionImplementationKind::Builtin,
        })
        .collect::<Vec<_>>()
    })
    .chain(float_and_float_vec_types().into_iter().map(|t| {
      [
        ("floor", 1),
        ("ceil", 1),
        ("sqrt", 1),
        ("pow", 2),
        ("mix", 3),
        ("clamp", 3),
        ("smoothstep", 3),
      ]
      .into_iter()
      .map(|(name, arg_count)| AbstractFunctionSignature {
        name: name.to_string(),
        generic_args: vec![],
        arg_types: std::iter::repeat(t.clone()).take(arg_count).collect(),
        return_type: t.clone(),
        implementation: FunctionImplementationKind::Builtin,
      })
      .collect::<Vec<_>>()
    }))
    .chain(float_vec_types().into_iter().map(|t| {
      vec![AbstractFunctionSignature {
        name: "mix".to_string(),
        generic_args: vec![],
        arg_types: vec![
          t.clone(),
          t.clone(),
          GenericOr::NonGeneric(TypeOrAbstractStruct::Type(Type::F32)),
        ],
        return_type: t.clone(),
        implementation: FunctionImplementationKind::Builtin,
      }]
    }))
    .flatten()
    .collect()
}

fn texture_functions() -> Vec<AbstractFunctionSignature> {
  vec![AbstractFunctionSignature {
    name: "textureSample".to_string(),
    generic_args: vec![("T".to_string(), vec![])],
    arg_types: vec![
      GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(texture_2d())),
      GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(sampler())),
      GenericOr::NonGeneric(TypeOrAbstractStruct::AbstractStruct(
        vec2().fill_abstract_generics(vec![AbstractType::NonGeneric(
          TypeOrAbstractStruct::Type(Type::F32),
        )]),
      )),
    ],
    return_type: AbstractType::NonGeneric(
      TypeOrAbstractStruct::AbstractStruct(vec4()),
    ),
    implementation: FunctionImplementationKind::Builtin,
  }]
}

pub fn built_in_functions() -> Vec<AbstractFunctionSignature> {
  let mut signatures = vec![];
  signatures.append(&mut assignment_function());
  signatures.append(&mut boolean_functions());
  signatures.append(&mut comparison_functions());
  signatures.append(&mut arithmetic_functions("+"));
  signatures.append(&mut arithmetic_functions("*"));
  signatures.append(&mut arithmetic_functions("-"));
  signatures.append(&mut negation_functions());
  signatures.append(&mut arithmetic_functions("/"));
  signatures.append(&mut arithmetic_functions("%"));
  signatures.append(&mut multi_signature_vec_constructors(4));
  signatures.append(&mut multi_signature_vec_constructors(3));
  signatures.append(&mut multi_signature_vec_constructors(2));
  signatures.append(&mut vector_functions());
  signatures.append(&mut trigonometry_functions());
  signatures.append(&mut exp_functions());
  signatures.append(&mut scalar_conversion_functions());
  signatures.append(&mut misc_math_functions());
  signatures.append(&mut texture_functions());
  signatures
}

pub const ASSIGNMENT_OPS: [&'static str; 6] =
  ["=", "+=", "-=", "*=", "/=", "%="];

pub const INFIX_OPS: [&'static str; 12] = [
  "==", ">=", ">", "<=", "<", "||", "&&", "+", "-", "*", "/", "%",
];

pub const ABNORMAL_CONSTRUCTOR_STRUCTS: [&'static str; 3] =
  ["vec2", "vec3", "vec4"];

pub fn built_in_macros() -> Vec<Macro> {
  let if_macro = Macro(Box::new(|tree| match tree {
    EaslTree::Inner(
      (position, EncloserOrOperator::Encloser(Encloser::Parens)),
      mut children,
    ) => {
      if children.is_empty() {
        Err(EaslTree::Inner(
          (position, EncloserOrOperator::Encloser(Encloser::Parens)),
          children,
        ))
      } else {
        if let EaslTree::Leaf(_, leaf) = &children[0] {
          if leaf.as_str() == "if" {
            if children.len() == 4 {
              let false_branch = children.remove(3);
              let true_branch = children.remove(2);
              let condition = children.remove(1);
              Ok(Ok(EaslTree::Inner(
                (position, EncloserOrOperator::Encloser(Encloser::Parens)),
                vec![
                  EaslTree::Leaf(
                    DocumentPosition {
                      span: 0..0,
                      path: vec![],
                    },
                    "match".to_string(),
                  ),
                  condition,
                  EaslTree::Leaf(
                    DocumentPosition {
                      span: 0..0,
                      path: vec![],
                    },
                    "true".to_string(),
                  ),
                  true_branch,
                  EaslTree::Leaf(
                    DocumentPosition {
                      span: 0..0,
                      path: vec![],
                    },
                    "false".to_string(),
                  ),
                  false_branch,
                ],
              )))
            } else {
              Ok(Err((
                SourceTrace::from(position),
                format!(
                  "\"if\" statement expects 3 arguments, found {}",
                  children.len()
                ),
              )))
            }
          } else {
            Err(EaslTree::Inner(
              (position, EncloserOrOperator::Encloser(Encloser::Parens)),
              children,
            ))
          }
        } else {
          Err(EaslTree::Inner(
            (position, EncloserOrOperator::Encloser(Encloser::Parens)),
            children,
          ))
        }
      }
    }
    other => Err(other),
  }));
  let when_macro = Macro(Box::new(|tree| match tree {
    EaslTree::Inner(
      (position, EncloserOrOperator::Encloser(Encloser::Parens)),
      mut children,
    ) => {
      if children.is_empty() {
        Err(EaslTree::Inner(
          (position, EncloserOrOperator::Encloser(Encloser::Parens)),
          children,
        ))
      } else {
        if let EaslTree::Leaf(_, leaf) = &children[0] {
          if leaf.as_str() == "when" {
            if children.len() > 2 {
              let condition = children.remove(1);
              std::mem::swap(
                &mut children[0],
                &mut EaslTree::Leaf(
                  DocumentPosition {
                    span: 0..0,
                    path: vec![],
                  },
                  "block".to_string(),
                ),
              );
              Ok(Ok(EaslTree::Inner(
                (position, EncloserOrOperator::Encloser(Encloser::Parens)),
                vec![
                  EaslTree::Leaf(
                    DocumentPosition {
                      span: 0..0,
                      path: vec![],
                    },
                    "match".to_string(),
                  ),
                  condition,
                  EaslTree::Leaf(
                    DocumentPosition {
                      span: 0..0,
                      path: vec![],
                    },
                    "true".to_string(),
                  ),
                  EaslTree::Inner(
                    (
                      DocumentPosition {
                        span: 0..0,
                        path: vec![],
                      },
                      EncloserOrOperator::Encloser(Encloser::Parens),
                    ),
                    children,
                  ),
                ],
              )))
            } else {
              Ok(Err((
                SourceTrace::from(position),
                "\"when\" statement expects a condition and at least 1 body \
                statement"
                  .to_string(),
              )))
            }
          } else {
            Err(EaslTree::Inner(
              (position, EncloserOrOperator::Encloser(Encloser::Parens)),
              children,
            ))
          }
        } else {
          Err(EaslTree::Inner(
            (position, EncloserOrOperator::Encloser(Encloser::Parens)),
            children,
          ))
        }
      }
    }
    other => Err(other),
  }));
  let thread_macro = Macro(Box::new(|tree| match tree {
    EaslTree::Inner(
      (position, EncloserOrOperator::Encloser(Encloser::Parens)),
      children,
    ) => {
      if children.is_empty() {
        Err(EaslTree::Inner(
          (position, EncloserOrOperator::Encloser(Encloser::Parens)),
          children,
        ))
      } else {
        if let EaslTree::Leaf(_, leaf) = &children[0] {
          if leaf.as_str() == "->" {
            if children.len() <= 1 {
              return Ok(Err((
                SourceTrace::from(position),
                format!("\"->\" macro expects at least one inner form",),
              )));
            }
            let mut children_iter = children.into_iter();
            children_iter.next();
            let original_expression = children_iter.next().unwrap();
            Ok(children_iter.fold(
              Ok(original_expression),
              |maybe_previous_expression: Result<
                EaslTree,
                (SourceTrace, String),
              >,
               thread_expression| {
                maybe_previous_expression
                    .map(|previous_expression| {
                      fn walk_thread_expression(
                        tree: EaslTree,
                        inner_expression: Option<EaslTree>,
                        mut positioner_traces: Vec<SourceTrace>,
                      ) -> (EaslTree, Option<EaslTree>, Vec<SourceTrace>)
                      {
                        match tree {
                          EaslTree::Leaf(position, leaf) => {
                            if leaf.as_str() == "<>" {
                              positioner_traces
                                .push(SourceTrace::from(position.clone()));
                              if let Some(inner_expression) = inner_expression {
                                (inner_expression, None, positioner_traces)
                              } else {
                                (
                                  EaslTree::Leaf(position, leaf),
                                  None,
                                  positioner_traces,
                                )
                              }
                            } else {
                              (
                                EaslTree::Leaf(position, leaf),
                                inner_expression,
                                positioner_traces,
                              )
                            }
                          }
                          EaslTree::Inner((position, kind), subtrees) => {
                            let (
                              new_subtrees,
                              inner_expression,
                              position_traces,
                            ) = subtrees.into_iter().fold(
                              (vec![], inner_expression, positioner_traces),
                              |(
                                mut new_subtrees,
                                inner_expression,
                                positioner_traces,
                              ),
                               subtree| {
                                let (
                                  new_subtree,
                                  new_inner_expression,
                                  new_positioner_traces,
                                ) = walk_thread_expression(
                                  subtree,
                                  inner_expression,
                                  positioner_traces,
                                );
                                new_subtrees.push(new_subtree);
                                (
                                  new_subtrees,
                                  new_inner_expression,
                                  new_positioner_traces,
                                )
                              },
                            );
                            (
                              EaslTree::Inner((position, kind), new_subtrees),
                              inner_expression,
                              position_traces,
                            )
                          }
                        }
                      }
                      let (
                        new_thread_expression,
                        previous_expression,
                        positioner_traces,
                      ) = walk_thread_expression(
                        thread_expression,
                        Some(previous_expression),
                        vec![],
                      );
                      match positioner_traces.len() {
                        0 => match new_thread_expression {
                          EaslTree::Inner(
                            (
                              paren_position,
                              EncloserOrOperator::Encloser(Encloser::Parens),
                            ),
                            mut subtrees,
                          ) => {
                            subtrees.insert(1, previous_expression.unwrap());
                            Ok(EaslTree::Inner(
                              (
                                paren_position,
                                EncloserOrOperator::Encloser(Encloser::Parens),
                              ),
                              subtrees,
                            ))
                          }
                          sse::Sexp::Inner((paren_position, _), _) => Err((
                            SourceTrace::from(paren_position),
                            format!(
                              "\"->\" macro expects at least one inner form",
                            ),
                          )),
                          EaslTree::Leaf(leaf_position, leaf_string) => Ok({
                            EaslTree::Inner(
                              (
                                leaf_position.clone(),
                                EncloserOrOperator::Encloser(Encloser::Parens),
                              ),
                              vec![
                                EaslTree::Leaf(leaf_position, leaf_string),
                                previous_expression.unwrap(),
                              ],
                            )
                          }),
                        },
                        1 => Ok(new_thread_expression),
                        n => Err((
                          positioner_traces.into_iter().collect(),
                          format!(
                          "\"->\" expression must contain zero or one \"<>\" \
                          subexpressions, found {n}"
                        ),
                        )),
                      }
                    })
                    .flatten()
              },
            ))
          } else {
            Err(EaslTree::Inner(
              (position, EncloserOrOperator::Encloser(Encloser::Parens)),
              children,
            ))
          }
        } else {
          Err(EaslTree::Inner(
            (position, EncloserOrOperator::Encloser(Encloser::Parens)),
            children,
          ))
        }
      }
    }
    other => Err(other),
  }));
  vec![if_macro, when_macro, thread_macro]
}
