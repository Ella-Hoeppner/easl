use std::{
  collections::{HashMap, HashSet},
  rc::Rc,
};

use lazy_static::lazy_static;

use sse::{document::DocumentPosition, syntax::EncloserOrOperator, Ast};

use crate::{
  compiler::{
    error::SourceTrace, structs::AbstractStructField, types::ArraySize,
  },
  parse::{EaslTree, Encloser},
};

use super::{
  functions::{AbstractFunctionSignature, FunctionImplementationKind},
  macros::Macro,
  structs::AbstractStruct,
  types::{AbstractType, Type, TypeConstraint},
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
    1 => AbstractType::Generic("T".into()),
    2 => AbstractType::AbstractStruct(get_builtin_struct("vec2").into()),
    3 => AbstractType::AbstractStruct(get_builtin_struct("vec3").into()),
    4 => AbstractType::AbstractStruct(get_builtin_struct("vec4").into()),
    _ => unreachable!(),
  }
}

fn specialized_vec_n_type(n: u8, suffix: &str) -> AbstractType {
  let vec_type = AbstractType::Type(match suffix {
    "f" => Type::F32,
    "i" => Type::I32,
    "u" => Type::U32,
    _ => panic!("unknown specialized vec suffix \"{suffix}\""),
  });
  match n {
    1 => vec_type,
    2 => AbstractType::AbstractStruct(
      get_builtin_struct("vec2")
        .partially_fill_abstract_generics(
          [("T".into(), vec_type)].into_iter().collect(),
        )
        .into(),
    ),
    3 => AbstractType::AbstractStruct(
      get_builtin_struct("vec3")
        .partially_fill_abstract_generics(
          [("T".into(), vec_type)].into_iter().collect(),
        )
        .into(),
    ),
    4 => AbstractType::AbstractStruct(
      get_builtin_struct("vec4")
        .partially_fill_abstract_generics(
          [("T".into(), vec_type)].into_iter().collect(),
        )
        .into(),
    ),
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
          name: format!("vec{n}").into(),
          generic_args: (0..nums.len())
            .map(|i| format!("A{i}").into())
            .chain(std::iter::once("T".into()))
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
          name: format!("vec{n}f").into(),
          generic_args: (0..nums.len())
            .map(|i| format!("A{i}").into())
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
          name: format!("vec{n}i").into(),
          generic_args: (0..nums.len())
            .map(|i| format!("A{i}").into())
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
          name: format!("vec{n}u").into(),
          generic_args: (0..nums.len())
            .map(|i| format!("A{i}").into())
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
    name: "vec2".into(),
    fields: vec![
      AbstractStructField {
        metadata: None,
        name: "x".into(),
        field_type: AbstractType::Generic("T".into()),
      },
      AbstractStructField {
        metadata: None,
        name: "y".into(),
        field_type: AbstractType::Generic("T".into()),
      },
    ],
    generic_args: vec!["T".into()],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
  }
}

pub fn vec3() -> AbstractStruct {
  AbstractStruct {
    name: "vec3".into(),
    fields: vec![
      AbstractStructField {
        metadata: None,
        name: "x".into(),
        field_type: AbstractType::Generic("T".into()),
      },
      AbstractStructField {
        metadata: None,
        name: "y".into(),
        field_type: AbstractType::Generic("T".into()),
      },
      AbstractStructField {
        metadata: None,
        name: "z".into(),
        field_type: AbstractType::Generic("T".into()),
      },
    ],
    generic_args: vec!["T".into()],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
  }
}

pub fn vec4() -> AbstractStruct {
  AbstractStruct {
    name: "vec4".into(),
    fields: vec![
      AbstractStructField {
        metadata: None,
        name: "x".into(),
        field_type: AbstractType::Generic("T".into()),
      },
      AbstractStructField {
        metadata: None,
        name: "y".into(),
        field_type: AbstractType::Generic("T".into()),
      },
      AbstractStructField {
        metadata: None,
        name: "z".into(),
        field_type: AbstractType::Generic("T".into()),
      },
      AbstractStructField {
        metadata: None,
        name: "w".into(),
        field_type: AbstractType::Generic("T".into()),
      },
    ],
    generic_args: vec!["T".into()],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
  }
}

pub fn texture_2d() -> AbstractStruct {
  AbstractStruct {
    name: "Texture2D".into(),
    fields: vec![AbstractStructField {
      metadata: None,
      name: "_".into(),
      field_type: AbstractType::Generic("T".into()),
    }],
    generic_args: vec!["T".into()],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
  }
}

pub fn sampler() -> AbstractStruct {
  AbstractStruct {
    name: "sampler".into(),
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

pub fn built_in_type_aliases() -> Vec<(Rc<str>, Rc<AbstractStruct>)> {
  vec![
    (
      "vec2f".into(),
      vec2()
        .generate_monomorphized(vec![Type::F32], SourceTrace::empty())
        .unwrap()
        .into(),
    ),
    (
      "vec3f".into(),
      vec3()
        .generate_monomorphized(vec![Type::F32], SourceTrace::empty())
        .unwrap()
        .into(),
    ),
    (
      "vec4f".into(),
      vec4()
        .generate_monomorphized(vec![Type::F32], SourceTrace::empty())
        .unwrap()
        .into(),
    ),
    (
      "vec2i".into(),
      vec2()
        .generate_monomorphized(vec![Type::I32], SourceTrace::empty())
        .unwrap()
        .into(),
    ),
    (
      "vec3i".into(),
      vec3()
        .generate_monomorphized(vec![Type::I32], SourceTrace::empty())
        .unwrap()
        .into(),
    ),
    (
      "vec4i".into(),
      vec4()
        .generate_monomorphized(vec![Type::I32], SourceTrace::empty())
        .unwrap()
        .into(),
    ),
    (
      "vec2u".into(),
      vec2()
        .generate_monomorphized(vec![Type::U32], SourceTrace::empty())
        .unwrap()
        .into(),
    ),
    (
      "vec3u".into(),
      vec3()
        .generate_monomorphized(vec![Type::U32], SourceTrace::empty())
        .unwrap()
        .into(),
    ),
    (
      "vec4u".into(),
      vec4()
        .generate_monomorphized(vec![Type::U32], SourceTrace::empty())
        .unwrap()
        .into(),
    ),
  ]
}

pub fn get_builtin_struct(name: &str) -> AbstractStruct {
  built_in_structs()
    .into_iter()
    .find(|s| &*s.name == name)
    .unwrap()
}

fn arithmetic_functions(name: &str) -> Vec<AbstractFunctionSignature> {
  let assignment_name: Rc<str> = format!("{}=", name).into();
  vec![
    AbstractFunctionSignature {
      name: name.into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      return_type: AbstractType::Generic("T".into()),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: Rc::clone(&assignment_name),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      return_type: AbstractType::Type(Type::None),
      implementation: FunctionImplementationKind::Builtin,
    },
  ]
  .into_iter()
  .chain(foreach_vec_type(|vec| {
    let vec = Rc::new(vec);
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
            Rc::clone(&assignment_name)
          } else {
            name.into()
          },
          generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
          arg_types: arg_vecs_or_scalars
            .into_iter()
            .map(|vec_or_scalar| {
              if vec_or_scalar {
                AbstractType::AbstractStruct(vec.clone())
              } else {
                AbstractType::Generic("T".into())
              }
            })
            .collect(),
          return_type: if assignment_fn {
            AbstractType::Type(Type::None)
          } else {
            AbstractType::AbstractStruct(vec.clone())
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

fn bitwise_functions(name: &str) -> Vec<AbstractFunctionSignature> {
  let assignment_name: Rc<str> = format!("{}=", name).into();
  vec![
    AbstractFunctionSignature {
      name: name.into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::integer()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      return_type: AbstractType::Generic("T".into()),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: Rc::clone(&assignment_name),
      generic_args: vec![("T".into(), vec![TypeConstraint::integer()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      return_type: AbstractType::Type(Type::None),
      implementation: FunctionImplementationKind::Builtin,
    },
  ]
  .into_iter()
  .chain(foreach_vec_type(|vec| {
    let vec = Rc::new(vec);
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
            Rc::clone(&assignment_name)
          } else {
            name.into()
          },
          generic_args: vec![("T".into(), vec![TypeConstraint::integer()])],
          arg_types: arg_vecs_or_scalars
            .into_iter()
            .map(|vec_or_scalar| {
              if vec_or_scalar {
                AbstractType::AbstractStruct(vec.clone())
              } else {
                AbstractType::Generic("T".into())
              }
            })
            .collect(),
          return_type: if assignment_fn {
            AbstractType::Type(Type::None)
          } else {
            AbstractType::AbstractStruct(vec.clone())
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
  vec![
    AbstractFunctionSignature {
      name: "atan2".into(),
      generic_args: vec![],
      arg_types: vec![
        AbstractType::Type(Type::F32),
        AbstractType::Type(Type::F32),
      ],
      return_type: AbstractType::Type(Type::F32),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "cos".into(),
      generic_args: vec![],
      arg_types: vec![AbstractType::Type(Type::F32)],
      return_type: AbstractType::Type(Type::F32),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "sin".into(),
      generic_args: vec![],
      arg_types: vec![AbstractType::Type(Type::F32)],
      return_type: AbstractType::Type(Type::F32),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "tan".into(),
      generic_args: vec![],
      arg_types: vec![AbstractType::Type(Type::F32)],
      return_type: AbstractType::Type(Type::F32),
      implementation: FunctionImplementationKind::Builtin,
    },
  ]
}

fn exp_functions() -> Vec<AbstractFunctionSignature> {
  vec![AbstractFunctionSignature {
    name: "log".into(),
    generic_args: vec![],
    arg_types: vec![AbstractType::Type(Type::F32)],
    return_type: AbstractType::Type(Type::F32),
    implementation: FunctionImplementationKind::Builtin,
  }]
}

fn negation_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "-".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![AbstractType::Generic("T".into())],
      return_type: AbstractType::Generic("T".into()),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "-".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![AbstractType::AbstractStruct(vec2().into())],
      return_type: AbstractType::AbstractStruct(vec2().into()),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "-".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![AbstractType::AbstractStruct(vec3().into())],
      return_type: AbstractType::AbstractStruct(vec3().into()),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "-".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![AbstractType::AbstractStruct(vec4().into())],
      return_type: AbstractType::AbstractStruct(vec4().into()),
      implementation: FunctionImplementationKind::Builtin,
    },
  ]
}

fn comparison_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "==".into(),
      generic_args: vec![("T".into(), vec![])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: ">=".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: ">".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "<=".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "<".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin,
    },
  ]
}

fn assignment_function() -> Vec<AbstractFunctionSignature> {
  vec![AbstractFunctionSignature {
    name: "=".into(),
    generic_args: vec![("T".into(), vec![])],
    arg_types: vec![
      AbstractType::Generic("T".into()),
      AbstractType::Generic("T".into()),
    ],
    return_type: AbstractType::Type(Type::None),
    implementation: FunctionImplementationKind::Builtin,
  }]
}

fn boolean_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "!".into(),
      generic_args: vec![],
      arg_types: vec![AbstractType::Type(Type::Bool)],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "and".into(),
      generic_args: vec![],
      arg_types: vec![
        AbstractType::Type(Type::Bool),
        AbstractType::Type(Type::Bool),
      ],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "or".into(),
      generic_args: vec![],
      arg_types: vec![
        AbstractType::Type(Type::Bool),
        AbstractType::Type(Type::Bool),
      ],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin,
    },
  ]
}

fn float_and_float_vec_types() -> Vec<AbstractType> {
  let f32 = AbstractType::Type(Type::F32);
  vec![
    f32.clone(),
    AbstractType::AbstractStruct(
      vec2().fill_abstract_generics(vec![f32.clone()]).into(),
    ),
    AbstractType::AbstractStruct(
      vec3().fill_abstract_generics(vec![f32.clone()]).into(),
    ),
    AbstractType::AbstractStruct(
      vec4().fill_abstract_generics(vec![f32]).into(),
    ),
  ]
}

fn float_vec_types() -> Vec<AbstractType> {
  let f32 = AbstractType::Type(Type::F32);
  vec![
    AbstractType::AbstractStruct(
      vec2().fill_abstract_generics(vec![f32.clone()]).into(),
    ),
    AbstractType::AbstractStruct(
      vec3().fill_abstract_generics(vec![f32.clone()]).into(),
    ),
    AbstractType::AbstractStruct(
      vec4().fill_abstract_generics(vec![f32]).into(),
    ),
  ]
}

fn generic_and_vec_types() -> Vec<AbstractType> {
  vec![
    AbstractType::Generic("T".into()),
    AbstractType::AbstractStruct(vec2().into()),
    AbstractType::AbstractStruct(vec3().into()),
    AbstractType::AbstractStruct(vec4().into()),
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
    let vec = AbstractType::AbstractStruct(
      vec
        .generate_monomorphized(vec![Type::F32], SourceTrace::empty())
        .unwrap()
        .into(),
    );
    let float = AbstractType::Type(Type::F32);
    [
      ("length", vec![vec.clone()], float.clone()),
      ("distance", vec![vec.clone(), vec.clone()], float.clone()),
      ("normalize", vec![vec.clone()], vec.clone()),
      ("dot", vec![vec.clone(), vec.clone()], float.clone()),
      ("reflect", vec![vec.clone(), vec.clone()], vec.clone()),
      (
        "refract",
        vec![vec.clone(), vec.clone(), float.clone()],
        vec.clone(),
      ),
    ]
    .into_iter()
    .map(|(name, arg_types, return_type)| AbstractFunctionSignature {
      name: name.into(),
      generic_args: vec![],
      arg_types,
      return_type,
      implementation: FunctionImplementationKind::Builtin,
    })
    .collect()
  })
  .into_iter()
  .chain({
    let vec3 = AbstractType::AbstractStruct(
      vec3()
        .generate_monomorphized(vec![Type::F32], SourceTrace::empty())
        .unwrap()
        .into(),
    );
    std::iter::once(AbstractFunctionSignature {
      name: "cross".into(),
      generic_args: vec![],
      arg_types: vec![vec3.clone(), vec3.clone()],
      return_type: vec3,
      implementation: FunctionImplementationKind::Builtin,
    })
  })
  .collect()
}

fn scalar_conversion_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "i32".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![AbstractType::Generic("T".into())],
      return_type: AbstractType::Type(Type::I32),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "f32".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![AbstractType::Generic("T".into())],
      return_type: AbstractType::Type(Type::F32),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "u32".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![AbstractType::Generic("T".into())],
      return_type: AbstractType::Type(Type::U32),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "bitcast".into(),
      generic_args: vec![
        ("T".into(), vec![TypeConstraint::scalar()]),
        ("S".into(), vec![TypeConstraint::scalar()]),
      ],
      arg_types: vec![AbstractType::Generic("T".into())],
      return_type: AbstractType::Generic("S".into()),
      implementation: FunctionImplementationKind::Builtin,
    },
  ]
}

fn misc_math_functions() -> Vec<AbstractFunctionSignature> {
  generic_and_vec_types()
    .into_iter()
    .map(|t| {
      [("sign", 1), ("abs", 1), ("max", 2), ("min", 2)]
        .into_iter()
        .map(|(name, arg_count)| AbstractFunctionSignature {
          name: name.into(),
          generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
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
        ("round", 1),
        ("fract", 1),
        ("sqrt", 1),
        ("pow", 2),
        ("mix", 3),
        ("clamp", 3),
        ("smoothstep", 3),
      ]
      .into_iter()
      .map(|(name, arg_count)| AbstractFunctionSignature {
        name: name.into(),
        generic_args: vec![],
        arg_types: std::iter::repeat(t.clone()).take(arg_count).collect(),
        return_type: t.clone(),
        implementation: FunctionImplementationKind::Builtin,
      })
      .collect::<Vec<_>>()
    }))
    .chain(float_vec_types().into_iter().map(|t| {
      vec![AbstractFunctionSignature {
        name: "mix".into(),
        generic_args: vec![],
        arg_types: vec![t.clone(), t.clone(), AbstractType::Type(Type::F32)],
        return_type: t.clone(),
        implementation: FunctionImplementationKind::Builtin,
      }]
    }))
    .flatten()
    .collect()
}

fn texture_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "textureSample".into(),
      generic_args: vec![("T".into(), vec![])],
      arg_types: vec![
        AbstractType::AbstractStruct(texture_2d().into()),
        AbstractType::AbstractStruct(sampler().into()),
        AbstractType::AbstractStruct(
          vec2()
            .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
            .into(),
        ),
      ],
      return_type: AbstractType::AbstractStruct(vec4().into()),
      implementation: FunctionImplementationKind::Builtin,
    },
    AbstractFunctionSignature {
      name: "textureLoad".into(),
      generic_args: vec![("T".into(), vec![])],
      arg_types: vec![
        AbstractType::AbstractStruct(texture_2d().into()),
        AbstractType::AbstractStruct(
          vec2()
            .fill_abstract_generics(vec![AbstractType::Type(Type::I32)])
            .into(),
        ),
        AbstractType::Type(Type::I32),
      ],
      return_type: AbstractType::AbstractStruct(vec4().into()),
      implementation: FunctionImplementationKind::Builtin,
    },
  ]
}

fn array_functions() -> Vec<AbstractFunctionSignature> {
  vec![AbstractFunctionSignature {
    name: "arrayLength".into(),
    generic_args: vec![("T".into(), vec![])],
    arg_types: vec![AbstractType::Reference(
      AbstractType::AbstractArray {
        size: ArraySize::Unsized,
        inner_type: AbstractType::Generic("T".into()).into(),
      }
      .into(),
    )],
    return_type: AbstractType::Type(Type::U32),
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
  signatures.append(&mut bitwise_functions("^"));
  signatures.append(&mut bitwise_functions(">>"));
  signatures.append(&mut bitwise_functions("<<"));
  signatures.append(&mut multi_signature_vec_constructors(4));
  signatures.append(&mut multi_signature_vec_constructors(3));
  signatures.append(&mut multi_signature_vec_constructors(2));
  signatures.append(&mut vector_functions());
  signatures.append(&mut trigonometry_functions());
  signatures.append(&mut exp_functions());
  signatures.append(&mut scalar_conversion_functions());
  signatures.append(&mut misc_math_functions());
  signatures.append(&mut texture_functions());
  signatures.append(&mut array_functions());
  signatures
}

lazy_static! {
  pub static ref ASSIGNMENT_OPS: HashSet<&'static str> =
    ["=", "+=", "-=", "*=", "/=", "%=", "^=", ">>=", "<<="]
      .into_iter()
      .collect();
  pub static ref INFIX_OPS: HashSet<&'static str> = [
    "==", ">=", ">", "<=", "<", "||", "&&", "+", "-", "*", "/", "%", "^", ">>",
    "<<"
  ]
  .into_iter()
  .collect();
  pub static ref ABNORMAL_CONSTRUCTOR_STRUCTS: HashSet<&'static str> =
    ["vec2", "vec3", "vec4"].into_iter().collect();
}

pub fn built_in_macros() -> Vec<Macro> {
  let if_macro = Macro(Box::new(|tree| match tree {
    EaslTree::Inner(
      (position, EncloserOrOperator::Encloser(Encloser::Parens)),
      children,
    ) => {
      let mut children = children.clone();
      if children.is_empty() {
        None
      } else {
        if let EaslTree::Leaf(_, leaf) = &children[0] {
          if leaf.as_str() == "if" {
            if children.len() == 4 {
              let false_branch = children.remove(3);
              let true_branch = children.remove(2);
              let condition = children.remove(1);
              Some(Ok(EaslTree::Inner(
                (
                  position.clone(),
                  EncloserOrOperator::Encloser(Encloser::Parens),
                ),
                vec![
                  EaslTree::Leaf(
                    DocumentPosition {
                      span: 0..0,
                      path: vec![],
                    },
                    "match".into(),
                  ),
                  condition,
                  EaslTree::Leaf(
                    DocumentPosition {
                      span: 0..0,
                      path: vec![],
                    },
                    "true".into(),
                  ),
                  true_branch,
                  EaslTree::Leaf(
                    DocumentPosition {
                      span: 0..0,
                      path: vec![],
                    },
                    "false".into(),
                  ),
                  false_branch,
                ],
              )))
            } else {
              Some(Err((
                SourceTrace::from(position),
                format!(
                  "\"if\" statement expects 3 arguments, found {}",
                  children.len()
                )
                .into(),
              )))
            }
          } else {
            None
          }
        } else {
          None
        }
      }
    }
    _ => None,
  }));
  let when_macro = Macro(Box::new(|tree| match tree {
    EaslTree::Inner(
      (position, EncloserOrOperator::Encloser(Encloser::Parens)),
      children,
    ) => {
      let mut children = children.clone();
      if children.is_empty() {
        None
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
                  "block".into(),
                ),
              );
              Some(Ok(EaslTree::Inner(
                (
                  position.clone(),
                  EncloserOrOperator::Encloser(Encloser::Parens),
                ),
                vec![
                  EaslTree::Leaf(
                    DocumentPosition {
                      span: 0..0,
                      path: vec![],
                    },
                    "match".into(),
                  ),
                  condition,
                  EaslTree::Leaf(
                    DocumentPosition {
                      span: 0..0,
                      path: vec![],
                    },
                    "true".into(),
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
              Some(Err((
                SourceTrace::from(position),
                "\"when\" statement expects a condition and at least 1 body \
                statement"
                  .into(),
              )))
            }
          } else {
            None
          }
        } else {
          None
        }
      }
    }
    _ => None,
  }));
  let thread_macro = Macro(Box::new(|tree| match tree {
    EaslTree::Inner(
      (position, EncloserOrOperator::Encloser(Encloser::Parens)),
      children,
    ) => {
      if children.is_empty() {
        None
      } else {
        if let EaslTree::Leaf(_, leaf) = &children[0] {
          if leaf.as_str() == "->" {
            if children.len() <= 1 {
              return Some(Err((
                SourceTrace::from(position),
                format!("\"->\" macro expects at least one inner form").into(),
              )));
            }
            let mut children_iter = children.into_iter();
            children_iter.next();
            let original_expression = children_iter.next().unwrap();
            Some(children_iter.fold(
              Ok(original_expression.clone()),
              |maybe_previous_expression: Result<
                EaslTree,
                (SourceTrace, Rc<str>),
              >,
               thread_expression| {
                match maybe_previous_expression.map(|previous_expression| {
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
                        let (new_subtrees, inner_expression, position_traces) =
                          subtrees.into_iter().fold(
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
                    thread_expression.clone(),
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
                        if subtrees.is_empty() {
                          Err((
                            SourceTrace::from(paren_position),
                            format!(
                              "forms inside \"->\" macro must have at least
                                  one inner form",
                            )
                            .into(),
                          ))
                        } else {
                          subtrees.insert(1, previous_expression.unwrap());
                          Ok(EaslTree::Inner(
                            (
                              paren_position,
                              EncloserOrOperator::Encloser(Encloser::Parens),
                            ),
                            subtrees,
                          ))
                        }
                      }
                      EaslTree::Inner((inner_position, _), _) => Err((
                        SourceTrace::from(inner_position),
                        format!("\"->\" macro expects parenthesized forms",)
                          .into(),
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
                      )
                      .into(),
                    )),
                  }
                }) {
                  Ok(Err(x)) | Err(x) => Err(x),
                  Ok(Ok(x)) => Ok(x),
                }
              },
            ))
          } else {
            None
          }
        } else {
          None
        }
      }
    }
    _ => None,
  }));
  vec![if_macro, when_macro, thread_macro]
}

pub fn rename_builtin(name: &str) -> Option<String> {
  match &*name {
    "and" => Some("&&"),
    "or" => Some("||"),
    _ => None,
  }
  .map(|name| name.to_string())
}
