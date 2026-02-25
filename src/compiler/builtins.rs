use std::collections::{HashMap, HashSet};

use std::sync::Arc;

use lazy_static::lazy_static;

use fsexp::{document::DocumentPosition, syntax::EncloserOrOperator};

use crate::{
  compiler::{
    effects::Effect,
    entry::IOAttributes,
    error::SourceTrace,
    functions::{
      FunctionSignature, FunctionTargetConfiguration, Ownership,
      SpecialCasedBuiltinFunction,
    },
    program::TypeDefs,
    structs::AbstractStructField,
    types::{AbstractArraySize, GenericArgument, Variable, VariableKind},
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

pub trait ArgumentTypeHelpers: Sized {
  fn owned(self) -> (Self, Ownership) {
    (self, Ownership::Owned)
  }
}

impl ArgumentTypeHelpers for AbstractType {}

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
    "b" => Type::Bool,
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

pub(crate) fn builtin_vec_constructor_type(name: &str) -> Option<&'static str> {
  if name.len() != 5 {
    return None;
  }
  let size_char = name.chars().nth(3).unwrap();
  let type_char = name.chars().nth(4).unwrap();
  if &name[0..3] == "vec" && {
    size_char == '2' || size_char == '3' || size_char == '4'
  } {
    match type_char {
      'f' => Some("f"),
      'u' => Some("u"),
      'i' => Some("i"),
      _ => None,
    }
  } else {
    None
  }
}

fn multi_signature_vec_constructors(n: u8) -> Vec<AbstractFunctionSignature> {
  n_sums(n)
    .into_iter()
    .chain(std::iter::once(vec![1]))
    .flat_map(|nums| {
      ["f", "i", "u", "b"]
        .into_iter()
        .map(|suffix| AbstractFunctionSignature {
          name: format!("vec{n}{suffix}").into(),
          generic_args: (0..nums.len())
            .map(|i| format!("A{i}").into())
            .map(|name| {
              (
                name,
                GenericArgument::Type(vec![TypeConstraint::scalar_or_bool()]),
                SourceTrace::empty(),
              )
            })
            .collect(),
          arg_types: nums
            .iter()
            .copied()
            .enumerate()
            .map(|(i, n)| {
              vec_n_type(n).rename_generic("T", &format!("A{i}")).owned()
            })
            .collect(),
          return_type: specialized_vec_n_type(n, suffix),
          ..Default::default()
        })
        .chain(std::iter::once(AbstractFunctionSignature {
          name: format!("vec{n}").into(),
          generic_args: (0..nums.len())
            .map(|i| format!("A{i}").into())
            .chain(std::iter::once("T".into()))
            .map(|name| {
              (
                name,
                GenericArgument::Type(vec![TypeConstraint::scalar_or_bool()]),
                SourceTrace::empty(),
              )
            })
            .collect(),
          arg_types: nums
            .iter()
            .copied()
            .enumerate()
            .map(|(i, n)| {
              vec_n_type(n).rename_generic("T", &format!("A{i}")).owned()
            })
            .collect(),
          return_type: vec_n_type(n),
          ..Default::default()
        }))
        .collect::<Vec<AbstractFunctionSignature>>()
    })
    .collect()
}

pub fn vec2() -> AbstractStruct {
  AbstractStruct {
    name: ("vec2".into(), SourceTrace::empty()),
    fields: vec![
      AbstractStructField {
        attributes: IOAttributes::empty(SourceTrace::empty()),
        name: "x".into(),
        field_type: AbstractType::Generic("T".into()),
        source_trace: SourceTrace::empty(),
      },
      AbstractStructField {
        attributes: IOAttributes::empty(SourceTrace::empty()),
        name: "y".into(),
        field_type: AbstractType::Generic("T".into()),
        source_trace: SourceTrace::empty(),
      },
    ],
    generic_args: vec![(
      "T".into(),
      GenericArgument::Type(vec![]),
      SourceTrace::empty(),
    )],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
    opaque: false,
  }
}

pub fn vec3() -> AbstractStruct {
  AbstractStruct {
    name: ("vec3".into(), SourceTrace::empty()),
    fields: vec![
      AbstractStructField {
        attributes: IOAttributes::empty(SourceTrace::empty()),
        name: "x".into(),
        field_type: AbstractType::Generic("T".into()),
        source_trace: SourceTrace::empty(),
      },
      AbstractStructField {
        attributes: IOAttributes::empty(SourceTrace::empty()),
        name: "y".into(),
        field_type: AbstractType::Generic("T".into()),
        source_trace: SourceTrace::empty(),
      },
      AbstractStructField {
        attributes: IOAttributes::empty(SourceTrace::empty()),
        name: "z".into(),
        field_type: AbstractType::Generic("T".into()),
        source_trace: SourceTrace::empty(),
      },
    ],
    generic_args: vec![(
      "T".into(),
      GenericArgument::Type(vec![]),
      SourceTrace::empty(),
    )],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
    opaque: false,
  }
}

pub fn vec4() -> AbstractStruct {
  AbstractStruct {
    name: ("vec4".into(), SourceTrace::empty()),
    fields: vec![
      AbstractStructField {
        attributes: IOAttributes::empty(SourceTrace::empty()),
        name: "x".into(),
        field_type: AbstractType::Generic("T".into()),
        source_trace: SourceTrace::empty(),
      },
      AbstractStructField {
        attributes: IOAttributes::empty(SourceTrace::empty()),
        name: "y".into(),
        field_type: AbstractType::Generic("T".into()),
        source_trace: SourceTrace::empty(),
      },
      AbstractStructField {
        attributes: IOAttributes::empty(SourceTrace::empty()),
        name: "z".into(),
        field_type: AbstractType::Generic("T".into()),
        source_trace: SourceTrace::empty(),
      },
      AbstractStructField {
        attributes: IOAttributes::empty(SourceTrace::empty()),
        name: "w".into(),
        field_type: AbstractType::Generic("T".into()),
        source_trace: SourceTrace::empty(),
      },
    ],
    generic_args: vec![(
      "T".into(),
      GenericArgument::Type(vec![]),
      SourceTrace::empty(),
    )],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
    opaque: false,
  }
}

pub fn texture_2d() -> AbstractStruct {
  AbstractStruct {
    name: ("Texture2D".into(), SourceTrace::empty()),
    fields: vec![AbstractStructField {
      attributes: IOAttributes::empty(SourceTrace::empty()),
      name: "_".into(),
      field_type: AbstractType::Generic("T".into()),
      source_trace: SourceTrace::empty(),
    }],
    generic_args: vec![(
      "T".into(),
      GenericArgument::Type(vec![]),
      SourceTrace::empty(),
    )],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
    opaque: true,
  }
}

pub fn sampler() -> AbstractStruct {
  AbstractStruct {
    name: ("Sampler".into(), SourceTrace::empty()),
    fields: vec![],
    generic_args: vec![],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
    opaque: true,
  }
}

pub fn matrix(n: usize, m: usize) -> AbstractStruct {
  AbstractStruct {
    name: (format!("mat{n}x{m}").into(), SourceTrace::empty()),
    fields: vec![AbstractStructField {
      attributes: IOAttributes::empty(SourceTrace::empty()),
      name: "_".into(),
      field_type: AbstractType::Generic("T".into()),
      source_trace: SourceTrace::empty(),
    }],
    generic_args: vec![(
      "T".into(),
      GenericArgument::Type(vec![]),
      SourceTrace::empty(),
    )],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
    opaque: true,
  }
}

pub fn specialized_matrix_type(
  n: usize,
  m: usize,
  suffix: &str,
) -> AbstractType {
  let vec_type = AbstractType::Type(match suffix {
    "f" => Type::F32,
    "i" => Type::I32,
    "u" => Type::U32,
    _ => panic!("unknown specialized mat suffix \"{suffix}\""),
  });
  AbstractType::AbstractStruct(
    matrix(n, m)
      .partially_fill_abstract_generics(
        [("T".into(), vec_type)].into_iter().collect(),
      )
      .into(),
  )
}

pub fn matrix_constructors() -> Vec<AbstractFunctionSignature> {
  (2..=4)
    .flat_map(|n| {
      (2..=4).flat_map(move |m| {
        [
          AbstractFunctionSignature {
            name: format!("mat{n}x{m}").into(),
            generic_args: vec![(
              "T".into(),
              GenericArgument::Type(vec![TypeConstraint::scalar_or_bool()]),
              SourceTrace::empty(),
            )],
            arg_types: std::iter::repeat(
              AbstractType::Generic("T".into()).owned(),
            )
            .take(n * m)
            .collect(),
            return_type: AbstractType::AbstractStruct(matrix(n, m).into()),
            ..Default::default()
          },
          AbstractFunctionSignature {
            name: format!("mat{n}x{m}").into(),
            generic_args: vec![(
              "T".into(),
              GenericArgument::Type(vec![TypeConstraint::scalar_or_bool()]),
              SourceTrace::empty(),
            )],
            arg_types: std::iter::repeat(
              AbstractType::AbstractStruct(
                match m {
                  2 => vec2(),
                  3 => vec3(),
                  4 => vec4(),
                  _ => unreachable!(),
                }
                .into(),
              )
              .owned(),
            )
            .take(n)
            .collect(),
            return_type: AbstractType::AbstractStruct(matrix(n, m).into()),
            ..Default::default()
          },
        ]
        .into_iter()
        .chain(
          [("f", Type::F32), ("i", Type::I32), ("u", Type::U32)]
            .into_iter()
            .flat_map(move |(suffix, t)| {
              [
                AbstractFunctionSignature {
                  name: format!("mat{n}x{m}{suffix}").into(),
                  arg_types: std::iter::repeat(
                    AbstractType::Type(t.clone()).owned(),
                  )
                  .take(n * m)
                  .collect(),
                  return_type: specialized_matrix_type(n, m, suffix),
                  ..Default::default()
                },
                AbstractFunctionSignature {
                  name: format!("mat{n}x{m}{suffix}").into(),
                  arg_types: std::iter::repeat(
                    AbstractType::AbstractStruct(
                      match m {
                        2 => vec2(),
                        3 => vec3(),
                        4 => vec4(),
                        _ => unreachable!(),
                      }
                      .into(),
                    )
                    .fill_abstract_generics(
                      &[("T".into(), AbstractType::Type(t))]
                        .into_iter()
                        .collect(),
                    )
                    .owned(),
                  )
                  .take(n)
                  .collect(),
                  return_type: specialized_matrix_type(n, m, suffix),
                  ..Default::default()
                },
              ]
              .into_iter()
            }),
        )
      })
    })
    .collect()
}

pub fn built_in_structs() -> Vec<AbstractStruct> {
  vec![vec2(), vec3(), vec4(), texture_2d(), sampler()]
    .into_iter()
    .chain((2..=4).flat_map(|n| (2..=4).map(move |m| matrix(n, m))))
    .collect()
}

pub fn built_in_type_aliases() -> Vec<(Arc<str>, Arc<AbstractStruct>)> {
  [
    ("f", Type::F32),
    ("i", Type::I32),
    ("u", Type::U32),
    ("b", Type::Bool),
  ]
  .into_iter()
  .flat_map(|(suffix, t)| {
    [
      (
        format!("vec2{suffix}").into(),
        vec2()
          .generate_monomorphized(vec![t.clone()])
          .unwrap()
          .into(),
      ),
      (
        format!("vec3{suffix}").into(),
        vec3()
          .generate_monomorphized(vec![t.clone()])
          .unwrap()
          .into(),
      ),
      (
        format!("vec4{suffix}").into(),
        vec4()
          .generate_monomorphized(vec![t.clone()])
          .unwrap()
          .into(),
      ),
    ]
    .into_iter()
    .chain((2..=4).flat_map(move |n| {
      let t = t.clone();
      (2..=4).map(move |m| {
        (
          format!("mat{n}x{m}{suffix}").into(),
          matrix(n, m)
            .generate_monomorphized(vec![t.clone()])
            .unwrap()
            .into(),
        )
      })
    }))
  })
  .collect()
}

pub fn get_builtin_struct(name: &str) -> AbstractStruct {
  built_in_structs()
    .into_iter()
    .find(|s| &*s.name.0 == name)
    .unwrap()
}

fn arithmetic_functions(
  name: &str,
  associative: bool,
) -> Vec<AbstractFunctionSignature> {
  let assignment_name: Arc<str> = format!("{}=", name).into();
  vec![
    AbstractFunctionSignature {
      name: name.into(),
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![TypeConstraint::scalar()]),
        SourceTrace::empty(),
      )],
      arg_types: vec![
        AbstractType::Generic("T".into()).owned(),
        AbstractType::Generic("T".into()).owned(),
      ],
      return_type: AbstractType::Generic("T".into()),
      associative,
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: Arc::clone(&assignment_name),
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![TypeConstraint::scalar()]),
        SourceTrace::empty(),
      )],
      arg_types: vec![
        (
          AbstractType::Generic("T".into()),
          Ownership::MutableReference,
        ),
        AbstractType::Generic("T".into()).owned(),
      ],
      return_type: AbstractType::Type(Type::Unit),
      ..Default::default()
    },
  ]
  .into_iter()
  .chain(foreach_vec_type(|vec| {
    let vec = Arc::new(vec);
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
            Arc::clone(&assignment_name)
          } else {
            name.into()
          },
          generic_args: vec![(
            "T".into(),
            GenericArgument::Type(vec![TypeConstraint::scalar()]),
            SourceTrace::empty(),
          )],
          arg_types: arg_vecs_or_scalars
            .into_iter()
            .enumerate()
            .map(|(i, vec_or_scalar)| {
              (
                if vec_or_scalar {
                  AbstractType::AbstractStruct(vec.clone())
                } else {
                  AbstractType::Generic("T".into())
                },
                if i == 0 && assignment_fn {
                  Ownership::MutableReference
                } else {
                  Ownership::Owned
                },
              )
            })
            .collect(),
          return_type: if assignment_fn {
            AbstractType::Type(Type::Unit)
          } else {
            AbstractType::AbstractStruct(vec.clone())
          },
          associative: !assignment_fn && arg_vecs_or_scalars == [true, true],
          ..Default::default()
        })
        .collect::<Vec<_>>()
    })
    .flatten()
    .collect()
  }))
  .collect()
}

fn matrix_arithmetic_functions() -> Vec<AbstractFunctionSignature> {
  (2..=4)
    .flat_map(|n| {
      (2..=4).flat_map(move |m| {
        let vecn = |size: usize| match size {
          2 => AbstractType::AbstractStruct(vec2().into()),
          3 => AbstractType::AbstractStruct(vec3().into()),
          4 => AbstractType::AbstractStruct(vec4().into()),
          _ => unreachable!(),
        };
        let mat = AbstractType::AbstractStruct(matrix(n, m).into());
        let scalar_generic = vec![(
          "T".into(),
          GenericArgument::Type(vec![TypeConstraint::scalar()]),
          SourceTrace::empty(),
        )];
        [
          AbstractFunctionSignature {
            name: "+".into(),
            generic_args: scalar_generic.clone(),
            arg_types: vec![mat.clone().owned(), mat.clone().owned()],
            return_type: mat.clone(),
            associative: true,
            ..Default::default()
          },
          AbstractFunctionSignature {
            name: "-".into(),
            generic_args: scalar_generic.clone(),
            arg_types: vec![mat.clone().owned(), mat.clone().owned()],
            return_type: mat.clone(),
            ..Default::default()
          },
          AbstractFunctionSignature {
            name: "*".into(),
            generic_args: scalar_generic.clone(),
            arg_types: vec![
              AbstractType::Generic("T".into()).owned(),
              mat.clone().owned(),
            ],
            return_type: mat.clone(),
            ..Default::default()
          },
          AbstractFunctionSignature {
            name: "*".into(),
            generic_args: scalar_generic.clone(),
            arg_types: vec![
              mat.clone().owned(),
              AbstractType::Generic("T".into()).owned(),
            ],
            return_type: mat.clone(),
            ..Default::default()
          },
          AbstractFunctionSignature {
            name: "*".into(),
            generic_args: scalar_generic.clone(),
            arg_types: vec![mat.clone().owned(), vecn(n).owned()],
            return_type: vecn(m),
            ..Default::default()
          },
          AbstractFunctionSignature {
            name: "*".into(),
            generic_args: scalar_generic.clone(),
            arg_types: vec![vecn(m).owned(), mat.clone().owned()],
            return_type: vecn(n),
            ..Default::default()
          },
        ]
        .into_iter()
        .chain((2..=4).map(move |inner| AbstractFunctionSignature {
          name: "*".into(),
          generic_args: scalar_generic.clone(),
          arg_types: vec![
            AbstractType::AbstractStruct(matrix(inner, m).into()).owned(),
            AbstractType::AbstractStruct(matrix(n, inner).into()).owned(),
          ],
          return_type: AbstractType::AbstractStruct(matrix(n, m).into()),
          associative: true,
          ..Default::default()
        }))
      })
    })
    .collect()
}

pub fn misc_matrix_functions() -> Vec<AbstractFunctionSignature> {
  (2..4)
    .map(|i| {
      let m = specialized_matrix_type(i, i, "f");
      AbstractFunctionSignature {
        name: "determinant".into(),
        arg_types: vec![m.owned()],
        return_type: AbstractType::Type(Type::F32),
        ..Default::default()
      }
    })
    .chain((2..4).flat_map(|x| {
      (2..4).map(move |y| AbstractFunctionSignature {
        name: "transpose".into(),
        arg_types: vec![specialized_matrix_type(x, y, "f").owned()],
        return_type: specialized_matrix_type(y, x, "f"),
        ..Default::default()
      })
    }))
    .collect()
}

fn bitwise_functions(
  name: &str,
  associative: bool,
) -> Vec<AbstractFunctionSignature> {
  let assignment_name: Arc<str> = format!("{}=", name).into();
  vec![
    AbstractFunctionSignature {
      name: name.into(),
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![TypeConstraint::integer()]),
        SourceTrace::empty(),
      )],
      arg_types: vec![
        AbstractType::Generic("T".into()).owned(),
        AbstractType::Generic("T".into()).owned(),
      ],
      return_type: AbstractType::Generic("T".into()),
      associative,
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: Arc::clone(&assignment_name),
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![TypeConstraint::integer()]),
        SourceTrace::empty(),
      )],
      arg_types: vec![
        (
          AbstractType::Generic("T".into()),
          Ownership::MutableReference,
        ),
        AbstractType::Generic("T".into()).owned(),
      ],
      return_type: AbstractType::Type(Type::Unit),
      ..Default::default()
    },
  ]
  .into_iter()
  .chain(foreach_vec_type(|vec| {
    let vec = Arc::new(vec);
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
            Arc::clone(&assignment_name)
          } else {
            name.into()
          },
          generic_args: vec![(
            "T".into(),
            GenericArgument::Type(vec![TypeConstraint::integer()]),
            SourceTrace::empty(),
          )],
          arg_types: arg_vecs_or_scalars
            .into_iter()
            .map(|vec_or_scalar| {
              if vec_or_scalar {
                AbstractType::AbstractStruct(vec.clone())
              } else {
                AbstractType::Generic("T".into())
              }
              .owned()
            })
            .collect(),
          return_type: if assignment_fn {
            AbstractType::Type(Type::Unit)
          } else {
            AbstractType::AbstractStruct(vec.clone())
          },
          associative: !assignment_fn && arg_vecs_or_scalars == [true, true],
          ..Default::default()
        })
        .collect::<Vec<_>>()
    })
    .flatten()
    .collect()
  }))
  .collect()
}

fn trigonometry_functions() -> Vec<AbstractFunctionSignature> {
  [
    ("atan2", 2),
    ("cos", 1),
    ("sin", 1),
    ("tan", 1),
    ("acos", 1),
    ("asin", 1),
    ("atan", 1),
    ("sinh", 1),
    ("cosh", 1),
    ("tanh", 1),
    ("asinh", 1),
    ("acosh", 1),
    ("atanh", 1),
  ]
  .into_iter()
  .flat_map(|(name, arity)| {
    foreach_float_or_vecf_type(|t| {
      vec![AbstractFunctionSignature {
        name: name.into(),
        arg_types: std::iter::repeat_n(t.clone().owned(), arity).collect(),
        return_type: t,
        ..Default::default()
      }]
    })
  })
  .collect()
}

fn exp_functions() -> Vec<AbstractFunctionSignature> {
  foreach_float_or_vecf_type(|t| {
    ["exp", "exp2", "log", "log2"]
      .into_iter()
      .map(|name| AbstractFunctionSignature {
        name: name.into(),
        arg_types: vec![t.clone().owned()],
        return_type: t.clone(),
        ..Default::default()
      })
      .collect()
  })
  .into_iter()
  .chain(foreach_generic_scalar_or_vec_type(|t| {
    let generic_name: Arc<str> = "T".into();
    let f = t.clone().fill_abstract_generics(
      &[(generic_name.clone(), AbstractType::Type(Type::F32))]
        .into_iter()
        .collect(),
    );
    let i = t.fill_abstract_generics(
      &[(generic_name, AbstractType::Type(Type::I32))]
        .into_iter()
        .collect(),
    );
    vec![AbstractFunctionSignature {
      name: "ldexp".into(),
      arg_types: vec![f.clone().owned(), i.owned()],
      return_type: f,
      ..Default::default()
    }]
  }))
  .collect()
}

fn negation_functions() -> Vec<AbstractFunctionSignature> {
  foreach_generic_scalar_or_vec_type(|t| {
    let generic_name: Arc<str> = "T".into();
    [Type::F32, Type::I32]
      .into_iter()
      .map(|scalar| {
        let t = t.clone().fill_abstract_generics(
          &[(generic_name.clone(), AbstractType::Type(scalar))]
            .into_iter()
            .collect(),
        );
        AbstractFunctionSignature {
          name: "-".into(),
          arg_types: vec![t.clone().owned()],
          return_type: t,
          ..Default::default()
        }
      })
      .collect()
  })
}

fn inversion_functions() -> Vec<AbstractFunctionSignature> {
  foreach_float_or_vecf_type(|t| {
    vec![AbstractFunctionSignature {
      name: "/".into(),
      arg_types: vec![t.clone().owned()],
      return_type: t,
      ..Default::default()
    }]
  })
}

fn comparison_functions() -> Vec<AbstractFunctionSignature> {
  foreach_generic_scalar_or_vec_type(|t| {
    let b = t.clone().fill_abstract_generics(
      &[("T".into(), AbstractType::Type(Type::Bool))]
        .into_iter()
        .collect(),
    );
    vec![
      AbstractFunctionSignature {
        name: "==".into(),
        generic_args: vec![(
          "T".into(),
          GenericArgument::Type(vec![TypeConstraint::scalar_or_bool()]),
          SourceTrace::empty(),
        )],
        arg_types: vec![t.clone().owned(), t.clone().owned()],
        return_type: b.clone(),
        ..Default::default()
      },
      AbstractFunctionSignature {
        name: "!=".into(),
        generic_args: vec![(
          "T".into(),
          GenericArgument::Type(vec![TypeConstraint::scalar_or_bool()]),
          SourceTrace::empty(),
        )],
        arg_types: vec![t.clone().owned(), t.clone().owned()],
        return_type: b.clone(),
        ..Default::default()
      },
      AbstractFunctionSignature {
        name: ">=".into(),
        generic_args: vec![(
          "T".into(),
          GenericArgument::Type(vec![TypeConstraint::scalar()]),
          SourceTrace::empty(),
        )],
        arg_types: vec![t.clone().owned(), t.clone().owned()],
        return_type: b.clone(),
        ..Default::default()
      },
      AbstractFunctionSignature {
        name: ">".into(),
        generic_args: vec![(
          "T".into(),
          GenericArgument::Type(vec![TypeConstraint::scalar()]),
          SourceTrace::empty(),
        )],
        arg_types: vec![t.clone().owned(), t.clone().owned()],
        return_type: b.clone(),
        ..Default::default()
      },
      AbstractFunctionSignature {
        name: "<=".into(),
        generic_args: vec![(
          "T".into(),
          GenericArgument::Type(vec![TypeConstraint::scalar()]),
          SourceTrace::empty(),
        )],
        arg_types: vec![t.clone().owned(), t.clone().owned()],
        return_type: b.clone(),
        ..Default::default()
      },
      AbstractFunctionSignature {
        name: "<".into(),
        generic_args: vec![(
          "T".into(),
          GenericArgument::Type(vec![TypeConstraint::scalar()]),
          SourceTrace::empty(),
        )],
        arg_types: vec![t.clone().owned(), t.clone().owned()],
        return_type: b.clone(),
        ..Default::default()
      },
    ]
  })
}

pub fn assignment_function() -> AbstractFunctionSignature {
  AbstractFunctionSignature {
    name: "=".into(),
    generic_args: vec![(
      "T".into(),
      GenericArgument::Type(vec![]),
      SourceTrace::empty(),
    )],
    arg_types: vec![
      (
        AbstractType::Generic("T".into()),
        Ownership::MutableReference,
      ),
      AbstractType::Generic("T".into()).owned(),
    ],
    return_type: AbstractType::Type(Type::Unit),
    ..Default::default()
  }
}

fn boolean_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "!".into(),
      arg_types: vec![AbstractType::Type(Type::Bool).owned()],
      return_type: AbstractType::Type(Type::Bool),
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "not".into(),
      arg_types: vec![AbstractType::Type(Type::Bool).owned()],
      return_type: AbstractType::Type(Type::Bool),
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "&&".into(),
      arg_types: vec![
        AbstractType::Type(Type::Bool).owned(),
        AbstractType::Type(Type::Bool).owned(),
      ],
      return_type: AbstractType::Type(Type::Bool),
      associative: true,
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "and".into(),
      arg_types: vec![
        AbstractType::Type(Type::Bool).owned(),
        AbstractType::Type(Type::Bool).owned(),
      ],
      return_type: AbstractType::Type(Type::Bool),
      associative: true,
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "||".into(),
      arg_types: vec![
        AbstractType::Type(Type::Bool).owned(),
        AbstractType::Type(Type::Bool).owned(),
      ],
      return_type: AbstractType::Type(Type::Bool),
      associative: true,
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "or".into(),
      arg_types: vec![
        AbstractType::Type(Type::Bool).owned(),
        AbstractType::Type(Type::Bool).owned(),
      ],
      return_type: AbstractType::Type(Type::Bool),
      associative: true,
      ..Default::default()
    },
  ]
}

fn bit_manipulation_functions() -> Vec<AbstractFunctionSignature> {
  [
    ("dot-4-u8-packed", 2, Type::U32),
    ("dot-4-i8-packed", 2, Type::I32),
    ("extract-bits", 2, Type::I32),
  ]
  .into_iter()
  .map(|(name, arity, return_type)| AbstractFunctionSignature {
    name: name.into(),
    arg_types: std::iter::repeat_n(
      AbstractType::Type(Type::U32).owned(),
      arity,
    )
    .collect(),
    return_type: AbstractType::Type(return_type),
    ..Default::default()
  })
  .chain(
    [
      "first-leading-bit",
      "first-trailing-bit",
      "count-leading-zeros",
      "count-trailing-zeros",
      "count-one-bits",
      "reverse-bits",
    ]
    .into_iter()
    .flat_map(|name| {
      [Type::U32, Type::I32].into_iter().flat_map(move |t| {
        generic_and_vec_types().into_iter().map(move |v| {
          let generic_name: Arc<str> = "T".into();
          let v = v.fill_abstract_generics(
            &[(generic_name, AbstractType::Type(t.clone()))]
              .into_iter()
              .collect(),
          );
          AbstractFunctionSignature {
            name: name.into(),
            arg_types: vec![v.clone().owned()],
            return_type: v,
            ..Default::default()
          }
        })
      })
    }),
  )
  .chain(generic_and_vec_types().into_iter().flat_map(|v| {
    [Type::U32, Type::I32].into_iter().map(move |inner_type| {
      let generic_name: Arc<str> = "T".into();
      let v = v.clone().fill_abstract_generics(
        &[(generic_name, AbstractType::Type(inner_type.clone()))]
          .into_iter()
          .collect(),
      );
      AbstractFunctionSignature {
        name: "extract-bits".into(),
        arg_types: vec![
          v.clone().owned(),
          AbstractType::Type(Type::U32).owned(),
          AbstractType::Type(Type::U32).owned(),
        ],
        return_type: v,
        ..Default::default()
      }
    })
  }))
  .collect()
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

fn foreach_float_or_vecf_type(
  f: impl Fn(AbstractType) -> Vec<AbstractFunctionSignature>,
) -> Vec<AbstractFunctionSignature> {
  [vec2(), vec3(), vec4()]
    .into_iter()
    .map(|v| {
      AbstractType::AbstractStruct(
        v.generate_monomorphized(vec![Type::F32]).unwrap().into(),
      )
    })
    .chain(std::iter::once(AbstractType::Type(Type::F32)))
    .map(f)
    .flatten()
    .collect()
}

fn foreach_generic_scalar_or_vec_type(
  f: impl Fn(AbstractType) -> Vec<AbstractFunctionSignature>,
) -> Vec<AbstractFunctionSignature> {
  [vec2(), vec3(), vec4()]
    .into_iter()
    .map(|v| AbstractType::AbstractStruct(v.into()))
    .chain(std::iter::once(AbstractType::Generic("T".into())))
    .map(f)
    .flatten()
    .collect()
}

fn vector_functions() -> Vec<AbstractFunctionSignature> {
  foreach_vec_type(|vec| {
    let vec = AbstractType::AbstractStruct(
      vec.generate_monomorphized(vec![Type::F32]).unwrap().into(),
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
      arg_types: arg_types.into_iter().map(|t| t.owned()).collect(),
      return_type,
      ..Default::default()
    })
    .collect()
  })
  .into_iter()
  .chain({
    let vec3 = AbstractType::AbstractStruct(
      vec3()
        .generate_monomorphized(vec![Type::F32])
        .unwrap()
        .into(),
    );
    std::iter::once(AbstractFunctionSignature {
      name: "cross".into(),
      arg_types: vec![vec3.clone().owned(), vec3.clone().owned()],
      return_type: vec3,
      ..Default::default()
    })
  })
  .collect()
}

pub fn scalar_bitcast() -> AbstractFunctionSignature {
  AbstractFunctionSignature {
    name: "bitcast".into(),
    generic_args: vec![
      (
        "T".into(),
        GenericArgument::Type(vec![TypeConstraint::scalar()]),
        SourceTrace::empty(),
      ),
      (
        "S".into(),
        GenericArgument::Type(vec![TypeConstraint::scalar()]),
        SourceTrace::empty(),
      ),
    ],
    arg_types: vec![AbstractType::Generic("T".into()).owned()],
    return_type: AbstractType::Generic("S".into()),
    ..Default::default()
  }
}

pub fn bitcast_functions() -> Vec<AbstractFunctionSignature> {
  foreach_vec_type(|t| {
    let t = AbstractType::AbstractStruct(t.into());
    vec![AbstractFunctionSignature {
      name: "bitcast".into(),
      generic_args: vec![
        (
          "T".into(),
          GenericArgument::Type(vec![TypeConstraint::scalar()]),
          SourceTrace::empty(),
        ),
        (
          "S".into(),
          GenericArgument::Type(vec![TypeConstraint::scalar()]),
          SourceTrace::empty(),
        ),
      ],
      arg_types: vec![t.clone().owned()],
      return_type: t.rename_generic("T", "S"),
      ..Default::default()
    }]
  })
  .into_iter()
  .chain(std::iter::once(scalar_bitcast()))
  .collect()
}

fn scalar_conversion_functions() -> Vec<AbstractFunctionSignature> {
  let scalar_generic = vec![(
    "T".into(),
    GenericArgument::Type(vec![TypeConstraint::scalar()]),
    SourceTrace::empty(),
  )];
  vec![
    AbstractFunctionSignature {
      name: "i32".into(),
      generic_args: scalar_generic.clone(),
      arg_types: vec![AbstractType::Generic("T".into()).owned()],
      return_type: AbstractType::Type(Type::I32),
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "f32".into(),
      generic_args: scalar_generic.clone(),
      arg_types: vec![AbstractType::Generic("T".into()).owned()],
      return_type: AbstractType::Type(Type::F32),
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "u32".into(),
      generic_args: scalar_generic.clone(),
      arg_types: vec![AbstractType::Generic("T".into()).owned()],
      return_type: AbstractType::Type(Type::U32),
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "bool".into(),
      generic_args: scalar_generic,
      arg_types: vec![AbstractType::Generic("T".into()).owned()],
      return_type: AbstractType::Type(Type::Bool),
      ..Default::default()
    },
  ]
}

fn any_and_all() -> Vec<AbstractFunctionSignature> {
  foreach_generic_scalar_or_vec_type(|t| {
    let generic_name: Arc<str> = "T".into();
    let t = t.clone().fill_abstract_generics(
      &[(generic_name.clone(), AbstractType::Type(Type::Bool))]
        .into_iter()
        .collect(),
    );
    vec![
      AbstractFunctionSignature {
        name: "any".into(),
        arg_types: vec![t.clone().owned()],
        return_type: AbstractType::Type(Type::Bool),
        ..Default::default()
      },
      AbstractFunctionSignature {
        name: "all".into(),
        arg_types: vec![t.owned()],
        return_type: AbstractType::Type(Type::Bool),
        ..Default::default()
      },
    ]
  })
}

fn misc_math_functions() -> Vec<AbstractFunctionSignature> {
  generic_and_vec_types()
    .into_iter()
    .map(|t| {
      [
        ("sign", 1, false),
        ("abs", 1, false),
        ("clamp", 3, false),
        ("max", 2, true),
        ("min", 2, true),
      ]
      .into_iter()
      .map(|(name, arg_count, associative)| AbstractFunctionSignature {
        name: name.into(),
        generic_args: vec![(
          "T".into(),
          GenericArgument::Type(vec![TypeConstraint::scalar()]),
          SourceTrace::empty(),
        )],
        arg_types: std::iter::repeat(t.clone().owned())
          .take(arg_count)
          .collect(),
        return_type: t.clone(),
        associative,
        ..Default::default()
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
        ("trunc", 1),
        ("saturate", 1),
        ("pow", 2),
        ("mix", 3),
        ("smoothstep", 3),
        ("step", 3),
        ("fma", 3),
      ]
      .into_iter()
      .map(|(name, arg_count)| AbstractFunctionSignature {
        name: name.into(),
        arg_types: std::iter::repeat(t.clone().owned())
          .take(arg_count)
          .collect(),
        return_type: t.clone(),
        ..Default::default()
      })
      .collect::<Vec<_>>()
    }))
    .chain(float_vec_types().into_iter().map(|t| {
      vec![
        AbstractFunctionSignature {
          name: "mix".into(),
          arg_types: vec![
            t.clone().owned(),
            t.clone().owned(),
            AbstractType::Type(Type::F32).owned(),
          ],
          return_type: t.clone(),
          ..Default::default()
        },
        AbstractFunctionSignature {
          name: "face-forward".into(),
          arg_types: vec![
            t.clone().owned(),
            t.clone().owned(),
            t.clone().owned(),
          ],
          return_type: t.clone(),
          ..Default::default()
        },
      ]
    }))
    .flatten()
    .collect()
}

fn texture_functions() -> Vec<AbstractFunctionSignature> {
  let vec2f = || {
    AbstractType::AbstractStruct(
      vec2()
        .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
        .into(),
    )
  };
  let vec4f = || {
    AbstractType::AbstractStruct(
      vec4()
        .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
        .into(),
    )
  };
  let texture_2df = || {
    AbstractType::AbstractStruct(
      texture_2d()
        .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
        .into(),
    )
  };
  vec![
    AbstractFunctionSignature {
      name: "texture-dimensions".into(),
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      arg_types: vec![
        AbstractType::AbstractStruct(texture_2d().into()).owned(),
      ],
      return_type: AbstractType::AbstractStruct(
        vec2()
          .fill_abstract_generics(vec![AbstractType::Type(Type::U32)])
          .into(),
      ),
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "texture-dimensions".into(),
      generic_args: vec![
        (
          "T".into(),
          GenericArgument::Type(vec![]),
          SourceTrace::empty(),
        ),
        (
          "L".into(),
          GenericArgument::Type(vec![TypeConstraint::integer()]),
          SourceTrace::empty(),
        ),
      ],
      arg_types: vec![
        AbstractType::AbstractStruct(texture_2d().into()).owned(),
        AbstractType::Generic("L".into()).owned(),
      ],
      return_type: AbstractType::AbstractStruct(
        vec2()
          .fill_abstract_generics(vec![AbstractType::Type(Type::U32)])
          .into(),
      ),
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "texture-gather".into(),
      generic_args: vec![
        (
          "T".into(),
          GenericArgument::Type(vec![]),
          SourceTrace::empty(),
        ),
        (
          "C".into(),
          GenericArgument::Type(vec![TypeConstraint::integer()]),
          SourceTrace::empty(),
        ),
      ],
      arg_types: vec![
        AbstractType::Generic("C".into()).owned(),
        AbstractType::AbstractStruct(texture_2d().into()).owned(),
        AbstractType::AbstractStruct(sampler().into()).owned(),
        AbstractType::AbstractStruct(
          vec2()
            .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
            .into(),
        )
        .owned(),
      ],
      return_type: AbstractType::AbstractStruct(vec4().into()),
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "texture-sample".into(),
      arg_types: vec![
        texture_2df().owned(),
        AbstractType::AbstractStruct(sampler().into()).owned(),
        AbstractType::AbstractStruct(
          vec2()
            .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
            .into(),
        )
        .owned(),
      ],
      return_type: vec4f(),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: Effect::FragmentExclusiveFunction("texture-sample".into())
          .into(),
        target_configuration: FunctionTargetConfiguration::Default,
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "texture-sample-bias".into(),
      arg_types: vec![
        texture_2df().owned(),
        AbstractType::AbstractStruct(sampler().into()).owned(),
        AbstractType::AbstractStruct(
          vec2()
            .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
            .into(),
        )
        .owned(),
        AbstractType::Type(Type::F32).owned(),
      ],
      return_type: vec4f(),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: Effect::FragmentExclusiveFunction(
          "texture-sample-bias".into(),
        )
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "texture-sample-grad".into(),
      arg_types: vec![
        texture_2df().owned(),
        AbstractType::AbstractStruct(sampler().into()).owned(),
        vec2f().owned(),
        vec2f().owned(),
        vec2f().owned(),
      ],
      return_type: vec4f(),
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "texture-sample-level".into(),
      arg_types: vec![
        texture_2df().owned(),
        AbstractType::AbstractStruct(sampler().into()).owned(),
        vec2f().owned(),
        AbstractType::Type(Type::F32).owned(),
      ],
      return_type: vec4f(),
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "texture-sample-base-clamp-to-edge".into(),
      arg_types: vec![
        texture_2df().owned(),
        AbstractType::AbstractStruct(sampler().into()).owned(),
        vec2f().owned(),
      ],
      return_type: vec4f(),
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "texture-load".into(),
      generic_args: vec![
        (
          "T".into(),
          GenericArgument::Type(vec![]),
          SourceTrace::empty(),
        ),
        (
          "C".into(),
          GenericArgument::Type(vec![TypeConstraint::integer()]),
          SourceTrace::empty(),
        ),
        (
          "L".into(),
          GenericArgument::Type(vec![TypeConstraint::integer()]),
          SourceTrace::empty(),
        ),
      ],
      arg_types: vec![
        AbstractType::AbstractStruct(texture_2d().into()).owned(),
        AbstractType::AbstractStruct(
          vec2()
            .fill_abstract_generics(vec![AbstractType::Generic("C".into())])
            .into(),
        )
        .owned(),
        AbstractType::Generic("L".into()).owned(),
      ],
      return_type: AbstractType::AbstractStruct(vec4().into()),
      ..Default::default()
    },
  ]
}

fn data_packing_functions() -> Vec<AbstractFunctionSignature> {
  [
    ("4x8-snorm", vec4(), Type::F32, true),
    ("4x8-unorm", vec4(), Type::F32, true),
    ("4x8-i8", vec4(), Type::I32, true),
    ("4x8-u8", vec4(), Type::U32, true),
    ("4x8-i8-clamp", vec4(), Type::I32, false),
    ("4x8-u8-clamp", vec4(), Type::U32, false),
    ("2x16-snorm", vec2(), Type::F32, true),
    ("2x16-unorm", vec2(), Type::F32, true),
    ("2x16-float", vec2(), Type::F32, true),
  ]
  .into_iter()
  .flat_map(|(name, v, t, unpack)| {
    let mut signatures = vec![AbstractFunctionSignature {
      name: format!("pack-{name}").into(),
      arg_types: vec![
        AbstractType::AbstractStruct(
          v.clone()
            .fill_abstract_generics(vec![AbstractType::Type(t.clone())])
            .into(),
        )
        .owned(),
      ],
      return_type: AbstractType::Type(Type::U32),
      ..Default::default()
    }];
    if unpack {
      signatures.push(AbstractFunctionSignature {
        name: format!("unpack-{name}").into(),
        arg_types: vec![AbstractType::Type(Type::U32).owned()],
        return_type: AbstractType::AbstractStruct(
          v.fill_abstract_generics(vec![AbstractType::Type(t)]).into(),
        ),
        ..Default::default()
      });
    }
    signatures
  })
  .collect()
}

fn array_functions() -> Vec<AbstractFunctionSignature> {
  let generic_args = vec![
    (
      "T".into(),
      GenericArgument::Type(vec![]),
      SourceTrace::empty(),
    ),
    ("S".into(), GenericArgument::Constant, SourceTrace::empty()),
  ];
  vec![
    AbstractFunctionSignature {
      name: "array-length".into(),
      generic_args: generic_args.clone(),
      arg_types: vec![(
        AbstractType::AbstractArray {
          size: AbstractArraySize::Generic("S".into()),
          inner_type: AbstractType::Generic("T".into()).into(),
          source_trace: SourceTrace::empty(),
        }
        .into(),
        Ownership::Reference,
      )],
      return_type: AbstractType::Type(Type::U32),
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "array-length".into(),
      generic_args,
      arg_types: vec![(
        AbstractType::AbstractArray {
          size: AbstractArraySize::Unsized,
          inner_type: AbstractType::Generic("T".into()).into(),
          source_trace: SourceTrace::empty(),
        }
        .into(),
        Ownership::Reference,
      )],
      return_type: AbstractType::Type(Type::U32),
      ..Default::default()
    },
  ]
}

fn derivative_functions() -> Vec<AbstractFunctionSignature> {
  float_and_float_vec_types()
    .into_iter()
    .flat_map(|t| {
      [
        "dpdx",
        "dpdy",
        "dpdx-coarse",
        "dpdy-coarse",
        "dpdx-fine",
        "dpdy-fine",
      ]
      .into_iter()
      .map(move |name| AbstractFunctionSignature {
        name: name.into(),
        arg_types: vec![t.clone().owned()],
        return_type: t.clone(),
        implementation: FunctionImplementationKind::Builtin {
          effect_type: Effect::FragmentExclusiveFunction(name.into()).into(),
          target_configuration: FunctionTargetConfiguration::Default,
        },
        ..Default::default()
      })
    })
    .collect()
}

fn print_functions() -> Vec<AbstractFunctionSignature> {
  vec![AbstractFunctionSignature {
    name: "print".into(),
    generic_args: vec![(
      "T".into(),
      GenericArgument::Type(vec![]),
      SourceTrace::empty(),
    )],
    arg_types: vec![AbstractType::Generic("T".into()).owned()],
    return_type: AbstractType::Unit,
    implementation: FunctionImplementationKind::Builtin {
      effect_type: Effect::Print.into(),
      target_configuration: FunctionTargetConfiguration::SpecialCased(
        SpecialCasedBuiltinFunction::Print,
      ),
    },
    ..Default::default()
  }]
}

fn shader_dispatch_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "dispatch-render-shaders".into(),
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      arg_types: vec![
        AbstractType::Type(Type::Function(
          FunctionSignature {
            abstract_ancestor: None,
            args: vec![(
              Variable {
                kind: VariableKind::Let,
                var_type: Type::U32.known().into(),
              },
              vec![],
            )],
            return_type: Type::Skolem("T".into(), vec![]).known().into(),
          }
          .into(),
        ))
        .owned(),
        AbstractType::Type(Type::Function(
          FunctionSignature {
            abstract_ancestor: None,
            args: vec![(
              Variable {
                kind: VariableKind::Let,
                var_type: Type::Skolem("T".into(), vec![]).known().into(),
              },
              vec![],
            )],
            return_type: Type::Struct(
              AbstractStruct::concretize(
                Arc::new(
                  vec4().generate_monomorphized(vec![Type::F32]).unwrap(),
                ),
                &TypeDefs::empty(),
                &vec![],
                SourceTrace::empty(),
              )
              .unwrap(),
            )
            .known()
            .into(),
          }
          .into(),
        ))
        .owned(),
        AbstractType::Type(Type::U32).owned(),
      ],
      return_type: AbstractType::Unit,
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("dispatch-render-shaders".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "dispatch-compute-shader".into(),
      generic_args: vec![(
        "F".into(),
        GenericArgument::Type(vec![TypeConstraint::compute_entry_fn()]),
        SourceTrace::empty(),
      )],
      arg_types: vec![
        AbstractType::Generic("F".into()).owned(),
        AbstractType::AbstractStruct(
          vec3()
            .fill_abstract_generics(vec![AbstractType::Type(Type::U32)])
            .into(),
        )
        .owned(),
      ],
      return_type: AbstractType::Unit,
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("dispatch-compute-shader".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "spawn-window".into(),
      arg_types: vec![
        AbstractType::Type(Type::Function(
          FunctionSignature {
            abstract_ancestor: None,
            args: vec![],
            return_type: Type::Unit.known().into(),
          }
          .into(),
        ))
        .owned(),
      ],
      return_type: AbstractType::Unit,
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("spawn-window".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "close-window".into(),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("close-window".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
      },
      ..Default::default()
    },
  ]
}

fn dynamic_array_functions() -> Vec<AbstractFunctionSignature> {
  vec![AbstractFunctionSignature {
    generic_args: vec![
      (
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      ),
      ("C".into(), GenericArgument::Constant, SourceTrace::empty()),
    ],
    name: "into-dynamic-array".into(),
    arg_types: vec![
      AbstractType::AbstractArray {
        size: AbstractArraySize::Generic("C".into()),
        inner_type: AbstractType::Generic("T".into()).into(),
        source_trace: SourceTrace::empty(),
      }
      .owned(),
    ],
    return_type: AbstractType::AbstractArray {
      size: AbstractArraySize::Unsized,
      inner_type: AbstractType::Generic("T".into()).into(),
      source_trace: SourceTrace::empty(),
    },
    implementation: FunctionImplementationKind::Builtin {
      effect_type: Effect::CPUExclusiveFunction("into-dynamic-array".into())
        .into(),
      target_configuration: FunctionTargetConfiguration::Default,
    },
    ..Default::default()
  }]
}

pub fn built_in_functions() -> Vec<AbstractFunctionSignature> {
  let mut signatures = vec![assignment_function()];
  signatures.append(&mut boolean_functions());
  signatures.append(&mut comparison_functions());
  signatures.append(&mut arithmetic_functions("+", true));
  signatures.append(&mut arithmetic_functions("*", true));
  signatures.append(&mut arithmetic_functions("-", false));
  signatures.append(&mut arithmetic_functions("/", false));
  signatures.append(&mut arithmetic_functions("%", false));
  signatures.append(&mut negation_functions());
  signatures.append(&mut inversion_functions());
  signatures.append(&mut matrix_arithmetic_functions());
  signatures.append(&mut misc_matrix_functions());
  signatures.append(&mut bitwise_functions("&", true));
  signatures.append(&mut bitwise_functions("|", true));
  signatures.append(&mut bitwise_functions("^", true));
  signatures.append(&mut bitwise_functions(">>", false));
  signatures.append(&mut bitwise_functions("<<", false));
  signatures.append(&mut multi_signature_vec_constructors(4));
  signatures.append(&mut multi_signature_vec_constructors(3));
  signatures.append(&mut multi_signature_vec_constructors(2));
  signatures.append(&mut matrix_constructors());
  signatures.append(&mut vector_functions());
  signatures.append(&mut trigonometry_functions());
  signatures.append(&mut exp_functions());
  signatures.append(&mut scalar_conversion_functions());
  signatures.append(&mut bitcast_functions());
  signatures.append(&mut any_and_all());
  signatures.append(&mut misc_math_functions());
  signatures.append(&mut texture_functions());
  signatures.append(&mut array_functions());
  signatures.append(&mut data_packing_functions());
  signatures.append(&mut bit_manipulation_functions());
  signatures.append(&mut derivative_functions());
  signatures.append(&mut print_functions());
  signatures.append(&mut shader_dispatch_functions());
  signatures.append(&mut dynamic_array_functions());
  signatures
}

lazy_static! {
  pub static ref ASSIGNMENT_OPS: HashSet<&'static str> =
    ["=", "+=", "-=", "*=", "/=", "%=", "^=", ">>=", "<<="]
      .into_iter()
      .collect();
  pub static ref INFIX_OPS: HashSet<&'static str> = [
    "==", "!=", ">=", ">", "<=", "<", "||", "&&", "+", "-", "*", "/", "%", "^",
    ">>", "<<"
  ]
  .into_iter()
  .collect();
  pub static ref ABNORMAL_CONSTRUCTOR_STRUCTS: HashSet<&'static str> =
    ["vec2", "vec3", "vec4"].into_iter().collect();
}

pub fn built_in_macros() -> Vec<Macro> {
  let if_macro = Macro {
    reserved_names: vec!["if".into()],
    rewrite: Box::new(|tree, _names| match tree {
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
    }),
  };
  let when_macro = Macro {
    reserved_names: vec!["when".into()],
    rewrite: Box::new(|tree, _names| match tree {
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
                    "do".into(),
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
                    EaslTree::Leaf(
                      DocumentPosition {
                        span: 0..0,
                        path: vec![],
                      },
                      "false".into(),
                    ),
                    EaslTree::Inner(
                      (
                        DocumentPosition {
                          span: 0..0,
                          path: vec![],
                        },
                        EncloserOrOperator::Encloser(Encloser::Parens),
                      ),
                      vec![],
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
    }),
  };
  let thread_macro = Macro {
    reserved_names: vec!["->".into(), "<>".into()],
    rewrite: Box::new(|tree, names| match tree {
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
                  format!("\"->\" macro expects at least one inner form")
                    .into(),
                )));
              }
              fn walk_thread_expression(
                tree: EaslTree,
                binding_name: Arc<str>,
                mut positioner_traces: Vec<SourceTrace>,
              ) -> (EaslTree, Vec<SourceTrace>) {
                match tree {
                  EaslTree::Leaf(position, leaf) => {
                    if leaf.as_str() == "<>" {
                      positioner_traces
                        .push(SourceTrace::from(position.clone()));
                      (
                        EaslTree::Leaf(position, format!("{binding_name}")),
                        positioner_traces,
                      )
                    } else {
                      (EaslTree::Leaf(position, leaf), positioner_traces)
                    }
                  }
                  EaslTree::Inner((position, kind), subtrees) => {
                    let (new_subtrees, position_traces) =
                      subtrees.into_iter().fold(
                        (vec![], positioner_traces),
                        |(mut new_subtrees, positioner_traces), subtree| {
                          let (new_subtree, new_positioner_traces) =
                            walk_thread_expression(
                              subtree,
                              binding_name.clone(),
                              positioner_traces,
                            );
                          new_subtrees.push(new_subtree);
                          (new_subtrees, new_positioner_traces)
                        },
                      );
                    (
                      EaslTree::Inner((position, kind), new_subtrees),
                      position_traces,
                    )
                  }
                }
              }
              let binding_names: Vec<Arc<str>> =
                std::iter::repeat_with(|| names.gensym("thread_gensym"))
                  .take(children.len() - 1)
                  .collect();
              let null_position = DocumentPosition::new(0..0, vec![]);
              let bindings: Result<
                Vec<(Arc<str>, EaslTree)>,
                (SourceTrace, Arc<str>),
              > = children
                .into_iter()
                .skip(1)
                .cloned()
                .enumerate()
                .map(|(i, child_expression)| {
                  Ok((
                    binding_names[i].clone(),
                    if i == 0 {
                      child_expression
                    } else {
                      let prior_binding_name = binding_names[i - 1].clone();
                      let (mut new_child_expression, source_traces) =
                        walk_thread_expression(
                          child_expression,
                          prior_binding_name.clone(),
                          vec![],
                        );
                      if source_traces.is_empty() {
                        let binding_leaf = EaslTree::Leaf(
                          null_position.clone(),
                          format!("{prior_binding_name}"),
                        );
                        new_child_expression = match new_child_expression {
                          EaslTree::Inner(
                            (
                              paren_position,
                              EncloserOrOperator::Encloser(Encloser::Parens),
                            ),
                            mut subtrees,
                          ) => {
                            if subtrees.is_empty() {
                              return Err((
                                SourceTrace::from(paren_position),
                                format!(
                                  "forms inside \"->\" macro must have at least
                                      one inner form",
                                )
                                .into(),
                              ));
                            } else {
                              subtrees.insert(1, binding_leaf.clone());
                              EaslTree::Inner(
                                (
                                  paren_position,
                                  EncloserOrOperator::Encloser(
                                    Encloser::Parens,
                                  ),
                                ),
                                subtrees,
                              )
                            }
                          }
                          EaslTree::Inner((inner_position, _), _) => {
                            return Err((
                              SourceTrace::from(inner_position),
                              format!(
                                "\"->\" macro expects parenthesized forms",
                              )
                              .into(),
                            ));
                          }
                          EaslTree::Leaf(leaf_position, leaf_string) => {
                            EaslTree::Inner(
                              (
                                leaf_position.clone(),
                                EncloserOrOperator::Encloser(Encloser::Parens),
                              ),
                              vec![
                                EaslTree::Leaf(leaf_position, leaf_string),
                                binding_leaf.clone(),
                              ],
                            )
                          }
                        };
                      }
                      new_child_expression
                    },
                  ))
                })
                .collect();
              Some(bindings.map(|mut bindings| {
                let last_binding = bindings.pop().unwrap().1;
                {
                  EaslTree::Inner(
                    (
                      null_position.clone(),
                      EncloserOrOperator::Encloser(Encloser::Parens),
                    ),
                    vec![
                      EaslTree::Leaf(null_position.clone(), "let".into()),
                      EaslTree::Inner(
                        (
                          null_position.clone(),
                          EncloserOrOperator::Encloser(Encloser::Square),
                        ),
                        bindings
                          .into_iter()
                          .flat_map(|(name, value)| {
                            vec![
                              EaslTree::Leaf(
                                null_position.clone(),
                                format!("{name}"),
                              ),
                              value,
                            ]
                          })
                          .collect(),
                      ),
                      last_binding,
                    ],
                  )
                }
              }))
            } else {
              None
            }
          } else {
            None
          }
        }
      }
      _ => None,
    }),
  };
  vec![if_macro, when_macro, thread_macro]
}

pub fn rename_builtin_fn(name: &str) -> Option<String> {
  match &*name {
    "dpdx-coarse" => Some("dpdxCoarse"),
    "dpdy-coarse" => Some("dpdyCoarse"),
    "dpdx-fine" => Some("dpdxFine"),
    "dpdy-fine" => Some("dpdyFine"),
    "vec2b" => Some("vec2"),
    "vec3b" => Some("vec3"),
    "vec4b" => Some("vec4"),
    "not" => Some("!"),
    "and" => Some("&&"),
    "or" => Some("||"),
    "array-length" => Some("arrayLength"),
    "texture-sample" => Some("textureSample"),
    "texture-dimensions" => Some("textureDimensions"),
    "texture-gather" => Some("textureGather"),
    "texture-gather-compare" => Some("textureGatherCompare"),
    "texture-load" => Some("textureLoad"),
    "texture-sample-bias" => Some("textureSampleBias"),
    "texture-sample-compare" => Some("textureSampleCompare"),
    "texture-sample-compare-level" => Some("textureSampleCompareLevel"),
    "texture-sample-grad" => Some("textureSampleGrad"),
    "texture-sample-level" => Some("textureSampleLevel"),
    "texture-sample-base-clamp-to-edge" => Some("textureSampleBaseClampToEdge"),
    "texture-store" => Some("textureStore"),
    "pack-4x8-snorm" => Some("pack4x8snorm"),
    "pack-4x8-unorm" => Some("pack4x8unorm"),
    "pack-4x8-i8" => Some("pack4x8I8"),
    "pack-4x8-u8" => Some("pack4x8U8"),
    "pack-4x8-i8-clamp" => Some("pack4x8I8Clamp"),
    "pack-4x8-u8-clamp" => Some("pack4x8U8Clamp"),
    "pack-2x16-snorm" => Some("pack2x16snorm"),
    "pack-2x16-unorm" => Some("pack2x16unorm"),
    "pack-2x16-float" => Some("pack2x16float"),
    "face-forward" => Some("faceForward"),
    "dot-4-u8-packed" => Some("dot4U8Packed"),
    "dot-4-i8-packed" => Some("dot4I8Packed"),
    "extract-bits" => Some("extractBits"),
    "reverse-bits" => Some("reverseBits"),
    "first-leading-bit" => Some("firstLeadingBit"),
    "first-trailing-bit" => Some("firstTrailingBit"),
    "count-leading-zeros" => Some("countLeadingZeros"),
    "count-one-bits" => Some("countOneBits"),
    "count-trailing-zeros" => Some("countTrailingZeros"),
    _ => None,
  }
  .map(|name| name.to_string())
}
