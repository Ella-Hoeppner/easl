use std::{
  collections::{HashMap, HashSet},
  rc::Rc,
};

use lazy_static::lazy_static;

use sse::{document::DocumentPosition, syntax::EncloserOrOperator};

use crate::{
  compiler::{
    effects::{Effect, EffectType},
    entry::IOAttributes,
    error::SourceTrace,
    structs::AbstractStructField,
    types::ArraySize,
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
            .map(|name| (name, vec![TypeConstraint::scalar_or_bool()]))
            .collect(),
          arg_types: nums
            .iter()
            .copied()
            .enumerate()
            .map(|(i, n)| vec_n_type(n).rename_generic("T", &format!("A{i}")))
            .collect(),
          mutated_args: vec![],
          return_type: specialized_vec_n_type(n, suffix),
          implementation: FunctionImplementationKind::Builtin(
            EffectType::empty(),
          ),
          associative: false,
        })
        .chain(std::iter::once(AbstractFunctionSignature {
          name: format!("vec{n}").into(),
          generic_args: (0..nums.len())
            .map(|i| format!("A{i}").into())
            .chain(std::iter::once("T".into()))
            .map(|name| (name, vec![TypeConstraint::scalar_or_bool()]))
            .collect(),
          arg_types: nums
            .iter()
            .copied()
            .enumerate()
            .map(|(i, n)| vec_n_type(n).rename_generic("T", &format!("A{i}")))
            .collect(),
          mutated_args: vec![],
          return_type: vec_n_type(n),
          implementation: FunctionImplementationKind::Builtin(
            EffectType::empty(),
          ),
          associative: false,
        }))
        .collect::<Vec<AbstractFunctionSignature>>()
    })
    .collect()
}

pub fn vec2() -> AbstractStruct {
  AbstractStruct {
    name: "vec2".into(),
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
      attributes: IOAttributes::empty(SourceTrace::empty()),
      name: "_".into(),
      field_type: AbstractType::Generic("T".into()),
      source_trace: SourceTrace::empty(),
    }],
    generic_args: vec!["T".into()],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
  }
}

pub fn sampler() -> AbstractStruct {
  AbstractStruct {
    name: "Sampler".into(),
    fields: vec![],
    generic_args: vec![],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
  }
}

pub fn matrix(n: usize, m: usize) -> AbstractStruct {
  AbstractStruct {
    name: format!("mat{n}x{m}").into(),
    fields: vec![AbstractStructField {
      attributes: IOAttributes::empty(SourceTrace::empty()),
      name: "_".into(),
      field_type: AbstractType::Generic("T".into()),
      source_trace: SourceTrace::empty(),
    }],
    generic_args: vec!["T".into()],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
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
              vec![TypeConstraint::scalar_or_bool()],
            )],
            arg_types: std::iter::repeat(AbstractType::Generic("T".into()))
              .take(n * m)
              .collect(),
            mutated_args: vec![],
            return_type: AbstractType::AbstractStruct(matrix(n, m).into()),
            implementation: FunctionImplementationKind::Builtin(
              EffectType::empty(),
            ),
            associative: false,
          },
          AbstractFunctionSignature {
            name: format!("mat{n}x{m}").into(),
            generic_args: vec![(
              "T".into(),
              vec![TypeConstraint::scalar_or_bool()],
            )],
            arg_types: std::iter::repeat(AbstractType::AbstractStruct(
              match m {
                2 => vec2(),
                3 => vec3(),
                4 => vec4(),
                _ => unreachable!(),
              }
              .into(),
            ))
            .take(n)
            .collect(),
            mutated_args: vec![],
            return_type: AbstractType::AbstractStruct(matrix(n, m).into()),
            implementation: FunctionImplementationKind::Builtin(
              EffectType::empty(),
            ),
            associative: false,
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
                  generic_args: vec![],
                  arg_types: std::iter::repeat(AbstractType::Type(t.clone()))
                    .take(n * m)
                    .collect(),
                  mutated_args: vec![],
                  return_type: specialized_matrix_type(n, m, suffix),
                  implementation: FunctionImplementationKind::Builtin(
                    EffectType::empty(),
                  ),
                  associative: false,
                },
                AbstractFunctionSignature {
                  name: format!("mat{n}x{m}{suffix}").into(),
                  generic_args: vec![],
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
                    ),
                  )
                  .take(n)
                  .collect(),
                  mutated_args: vec![],
                  return_type: specialized_matrix_type(n, m, suffix),
                  implementation: FunctionImplementationKind::Builtin(
                    EffectType::empty(),
                  ),
                  associative: false,
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

pub fn built_in_type_aliases() -> Vec<(Rc<str>, Rc<AbstractStruct>)> {
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
    .find(|s| &*s.name == name)
    .unwrap()
}

fn arithmetic_functions(
  name: &str,
  associative: bool,
) -> Vec<AbstractFunctionSignature> {
  let assignment_name: Rc<str> = format!("{}=", name).into();
  vec![
    AbstractFunctionSignature {
      name: name.into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      mutated_args: vec![],
      return_type: AbstractType::Generic("T".into()),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative,
    },
    AbstractFunctionSignature {
      name: Rc::clone(&assignment_name),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      mutated_args: vec![0],
      return_type: AbstractType::Type(Type::Unit),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
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
          mutated_args: if assignment_fn { vec![0] } else { vec![] },
          return_type: if assignment_fn {
            AbstractType::Type(Type::Unit)
          } else {
            AbstractType::AbstractStruct(vec.clone())
          },
          implementation: FunctionImplementationKind::Builtin(
            EffectType::empty(),
          ),
          associative: !assignment_fn && arg_vecs_or_scalars == [true, true],
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
        [
          AbstractFunctionSignature {
            name: "+".into(),
            generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
            arg_types: vec![mat.clone(), mat.clone()],
            mutated_args: vec![],
            return_type: mat.clone(),
            implementation: FunctionImplementationKind::Builtin(
              EffectType::empty(),
            ),
            associative: true,
          },
          AbstractFunctionSignature {
            name: "-".into(),
            generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
            arg_types: vec![mat.clone(), mat.clone()],
            mutated_args: vec![],
            return_type: mat.clone(),
            implementation: FunctionImplementationKind::Builtin(
              EffectType::empty(),
            ),
            associative: false,
          },
          AbstractFunctionSignature {
            name: "*".into(),
            generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
            arg_types: vec![AbstractType::Generic("T".into()), mat.clone()],
            mutated_args: vec![],
            return_type: mat.clone(),
            implementation: FunctionImplementationKind::Builtin(
              EffectType::empty(),
            ),
            associative: false,
          },
          AbstractFunctionSignature {
            name: "*".into(),
            generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
            arg_types: vec![mat.clone(), AbstractType::Generic("T".into())],
            mutated_args: vec![],
            return_type: mat.clone(),
            implementation: FunctionImplementationKind::Builtin(
              EffectType::empty(),
            ),
            associative: false,
          },
          AbstractFunctionSignature {
            name: "*".into(),
            generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
            arg_types: vec![mat.clone(), vecn(n)],
            mutated_args: vec![],
            return_type: vecn(m),
            implementation: FunctionImplementationKind::Builtin(
              EffectType::empty(),
            ),
            associative: false,
          },
          AbstractFunctionSignature {
            name: "*".into(),
            generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
            arg_types: vec![vecn(m), mat.clone()],
            mutated_args: vec![],
            return_type: vecn(n),
            implementation: FunctionImplementationKind::Builtin(
              EffectType::empty(),
            ),
            associative: false,
          },
        ]
        .into_iter()
        .chain((2..=4).map(move |inner| AbstractFunctionSignature {
          name: "*".into(),
          generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
          arg_types: vec![
            AbstractType::AbstractStruct(matrix(inner, m).into()),
            AbstractType::AbstractStruct(matrix(n, inner).into()),
          ],
          mutated_args: vec![],
          return_type: vecn(n),
          implementation: FunctionImplementationKind::Builtin(
            EffectType::empty(),
          ),
          associative: true,
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
        generic_args: vec![],
        arg_types: vec![m],
        mutated_args: vec![],
        return_type: AbstractType::Type(Type::F32),
        implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
        associative: false,
      }
    })
    .chain((2..4).flat_map(|x| {
      (2..4).map(move |y| AbstractFunctionSignature {
        name: "transpose".into(),
        generic_args: vec![],
        arg_types: vec![specialized_matrix_type(x, y, "f")],
        mutated_args: vec![],
        return_type: specialized_matrix_type(y, x, "f"),
        implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
        associative: false,
      })
    }))
    .collect()
}

fn bitwise_functions(
  name: &str,
  associative: bool,
) -> Vec<AbstractFunctionSignature> {
  let assignment_name: Rc<str> = format!("{}=", name).into();
  vec![
    AbstractFunctionSignature {
      name: name.into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::integer()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      mutated_args: vec![],
      return_type: AbstractType::Generic("T".into()),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative,
    },
    AbstractFunctionSignature {
      name: Rc::clone(&assignment_name),
      generic_args: vec![("T".into(), vec![TypeConstraint::integer()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      mutated_args: vec![0],
      return_type: AbstractType::Type(Type::Unit),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
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
          mutated_args: vec![],
          return_type: if assignment_fn {
            AbstractType::Type(Type::Unit)
          } else {
            AbstractType::AbstractStruct(vec.clone())
          },
          implementation: FunctionImplementationKind::Builtin(
            EffectType::empty(),
          ),
          associative: !assignment_fn && arg_vecs_or_scalars == [true, true],
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
        generic_args: vec![],
        arg_types: std::iter::repeat_n(t.clone(), arity).collect(),
        mutated_args: vec![],
        return_type: t,
        implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
        associative: false,
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
        generic_args: vec![],
        arg_types: vec![t.clone()],
        mutated_args: vec![],
        return_type: t.clone(),
        implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
        associative: false,
      })
      .collect()
  })
  .into_iter()
  .chain(foreach_generic_scalar_or_vec_type(|t| {
    let generic_name: Rc<str> = "T".into();
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
      generic_args: vec![],
      arg_types: vec![f.clone(), i],
      mutated_args: vec![],
      return_type: f,
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    }]
  }))
  .collect()
}

fn negation_functions() -> Vec<AbstractFunctionSignature> {
  foreach_generic_scalar_or_vec_type(|t| {
    let generic_name: Rc<str> = "T".into();
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
          generic_args: vec![],
          arg_types: vec![t.clone()],
          mutated_args: vec![],
          return_type: t,
          implementation: FunctionImplementationKind::Builtin(
            EffectType::empty(),
          ),
          associative: false,
        }
      })
      .collect()
  })
}

fn inversion_functions() -> Vec<AbstractFunctionSignature> {
  foreach_float_or_vecf_type(|t| {
    vec![AbstractFunctionSignature {
      name: "/".into(),
      generic_args: vec![],
      arg_types: vec![t.clone()],
      mutated_args: vec![],
      return_type: t,
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    }]
  })
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
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
    AbstractFunctionSignature {
      name: "!=".into(),
      generic_args: vec![("T".into(), vec![])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
    AbstractFunctionSignature {
      name: ">=".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
    AbstractFunctionSignature {
      name: ">".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
    AbstractFunctionSignature {
      name: "<=".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
    AbstractFunctionSignature {
      name: "<".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![
        AbstractType::Generic("T".into()),
        AbstractType::Generic("T".into()),
      ],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
  ]
}

pub fn assignment_function() -> AbstractFunctionSignature {
  AbstractFunctionSignature {
    name: "=".into(),
    generic_args: vec![("T".into(), vec![])],
    arg_types: vec![
      AbstractType::Generic("T".into()),
      AbstractType::Generic("T".into()),
    ],
    mutated_args: vec![0],
    return_type: AbstractType::Type(Type::Unit),
    implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
    associative: false,
  }
}

fn boolean_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "!".into(),
      generic_args: vec![],
      arg_types: vec![AbstractType::Type(Type::Bool)],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
    AbstractFunctionSignature {
      name: "not".into(),
      generic_args: vec![],
      arg_types: vec![AbstractType::Type(Type::Bool)],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
    AbstractFunctionSignature {
      name: "&&".into(),
      generic_args: vec![],
      arg_types: vec![
        AbstractType::Type(Type::Bool),
        AbstractType::Type(Type::Bool),
      ],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: true,
    },
    AbstractFunctionSignature {
      name: "and".into(),
      generic_args: vec![],
      arg_types: vec![
        AbstractType::Type(Type::Bool),
        AbstractType::Type(Type::Bool),
      ],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: true,
    },
    AbstractFunctionSignature {
      name: "||".into(),
      generic_args: vec![],
      arg_types: vec![
        AbstractType::Type(Type::Bool),
        AbstractType::Type(Type::Bool),
      ],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: true,
    },
    AbstractFunctionSignature {
      name: "or".into(),
      generic_args: vec![],
      arg_types: vec![
        AbstractType::Type(Type::Bool),
        AbstractType::Type(Type::Bool),
      ],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: true,
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
    generic_args: vec![],
    arg_types: std::iter::repeat_n(AbstractType::Type(Type::U32), arity)
      .collect(),
    mutated_args: vec![],
    return_type: AbstractType::Type(return_type),
    implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
    associative: false,
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
          let generic_name: Rc<str> = "T".into();
          let v = v.fill_abstract_generics(
            &[(generic_name, AbstractType::Type(t.clone()))]
              .into_iter()
              .collect(),
          );
          AbstractFunctionSignature {
            name: name.into(),
            generic_args: vec![],
            arg_types: vec![v.clone()],
            mutated_args: vec![],
            return_type: v,
            implementation: FunctionImplementationKind::Builtin(
              EffectType::empty(),
            ),
            associative: false,
          }
        })
      })
    }),
  )
  .chain(generic_and_vec_types().into_iter().flat_map(|v| {
    [Type::U32, Type::I32].into_iter().map(move |inner_type| {
        let generic_name: Rc<str> = "T".into();
        let v = v.clone().fill_abstract_generics(
          &[(generic_name, AbstractType::Type(inner_type.clone()))]
            .into_iter()
            .collect(),
        );
        AbstractFunctionSignature {
          name: "extract-bits".into(),
          generic_args: vec![],
          arg_types: vec![
            v.clone(),
            AbstractType::Type(Type::U32),
            AbstractType::Type(Type::U32),
          ],
          mutated_args: vec![],
          return_type: v,
          implementation: FunctionImplementationKind::Builtin(
            EffectType::empty(),
          ),
          associative: false,
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
      generic_args: vec![],
      arg_types,
      mutated_args: vec![],
      return_type,
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
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
      generic_args: vec![],
      arg_types: vec![vec3.clone(), vec3.clone()],
      mutated_args: vec![],
      return_type: vec3,
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    })
  })
  .collect()
}

pub fn bitcast() -> AbstractFunctionSignature {
  AbstractFunctionSignature {
    name: "bitcast".into(),
    generic_args: vec![
      ("T".into(), vec![TypeConstraint::scalar()]),
      ("S".into(), vec![TypeConstraint::scalar()]),
    ],
    arg_types: vec![AbstractType::Generic("T".into())],
    mutated_args: vec![],
    return_type: AbstractType::Generic("S".into()),
    implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
    associative: false,
  }
}

fn scalar_conversion_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "i32".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![AbstractType::Generic("T".into())],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::I32),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
    AbstractFunctionSignature {
      name: "f32".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![AbstractType::Generic("T".into())],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::F32),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
    AbstractFunctionSignature {
      name: "u32".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![AbstractType::Generic("T".into())],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::U32),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
    AbstractFunctionSignature {
      name: "bool".into(),
      generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
      arg_types: vec![AbstractType::Generic("T".into())],
      mutated_args: vec![],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
  ]
}

fn any_and_all() -> Vec<AbstractFunctionSignature> {
  foreach_generic_scalar_or_vec_type(|t| {
    let generic_name: Rc<str> = "T".into();
    let t = t.clone().fill_abstract_generics(
      &[(generic_name.clone(), AbstractType::Type(Type::Bool))]
        .into_iter()
        .collect(),
    );
    vec![
      AbstractFunctionSignature {
        name: "any".into(),
        generic_args: vec![],
        arg_types: vec![t.clone()],
        mutated_args: vec![],
        return_type: AbstractType::Type(Type::Bool),
        implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
        associative: false,
      },
      AbstractFunctionSignature {
        name: "all".into(),
        generic_args: vec![],
        arg_types: vec![t],
        mutated_args: vec![],
        return_type: AbstractType::Type(Type::Bool),
        implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
        associative: false,
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
        ("max", 2, true),
        ("min", 2, true),
      ]
      .into_iter()
      .map(|(name, arg_count, associative)| AbstractFunctionSignature {
        name: name.into(),
        generic_args: vec![("T".into(), vec![TypeConstraint::scalar()])],
        arg_types: std::iter::repeat(t.clone()).take(arg_count).collect(),
        mutated_args: vec![],
        return_type: t.clone(),
        implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
        associative,
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
        ("clamp", 3),
        ("smoothstep", 3),
        ("step", 3),
        ("fma", 3),
      ]
      .into_iter()
      .map(|(name, arg_count)| AbstractFunctionSignature {
        name: name.into(),
        generic_args: vec![],
        arg_types: std::iter::repeat(t.clone()).take(arg_count).collect(),
        mutated_args: vec![],
        return_type: t.clone(),
        implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
        associative: false,
      })
      .collect::<Vec<_>>()
    }))
    .chain(float_vec_types().into_iter().map(|t| {
      vec![
        AbstractFunctionSignature {
          name: "mix".into(),
          generic_args: vec![],
          arg_types: vec![t.clone(), t.clone(), AbstractType::Type(Type::F32)],
          mutated_args: vec![],
          return_type: t.clone(),
          implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
          associative: false,
        },
        AbstractFunctionSignature {
          name: "face-forward".into(),
          generic_args: vec![],
          arg_types: vec![t.clone(), t.clone(), t.clone()],
          mutated_args: vec![],
          return_type: t.clone(),
          implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
          associative: false,
        },
      ]
    }))
    .flatten()
    .collect()
}

fn texture_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "texture-sample".into(),
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
      mutated_args: vec![],
      return_type: AbstractType::AbstractStruct(vec4().into()),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
    AbstractFunctionSignature {
      name: "texture-load".into(),
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
      mutated_args: vec![],
      return_type: AbstractType::AbstractStruct(vec4().into()),
      implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
      associative: false,
    },
  ]
}

fn array_functions() -> Vec<AbstractFunctionSignature> {
  vec![AbstractFunctionSignature {
    name: "array-length".into(),
    generic_args: vec![("T".into(), vec![])],
    arg_types: vec![AbstractType::Reference(
      AbstractType::AbstractArray {
        size: ArraySize::Unsized,
        inner_type: AbstractType::Generic("T".into()).into(),
        source_trace: SourceTrace::empty(),
      }
      .into(),
    )],
    mutated_args: vec![],
    return_type: AbstractType::Type(Type::U32),
    implementation: FunctionImplementationKind::Builtin(EffectType::empty()),
    associative: false,
  }]
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
        generic_args: vec![],
        arg_types: vec![t.clone()],
        mutated_args: vec![],
        return_type: t.clone(),
        implementation: FunctionImplementationKind::Builtin(
          Effect::FragmentExclusiveFunction(name.into()).into(),
        ),
        associative: false,
      })
    })
    .collect()
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
  signatures.append(&mut vec![bitcast()]);
  signatures.append(&mut any_and_all());
  signatures.append(&mut misc_math_functions());
  signatures.append(&mut texture_functions());
  signatures.append(&mut array_functions());
  signatures.append(&mut bit_manipulation_functions());
  signatures.append(&mut derivative_functions());
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
                binding_name: Rc<str>,
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
              let binding_names: Vec<Rc<str>> =
                std::iter::repeat_with(|| names.gensym("thread_gensym"))
                  .take(children.len() - 1)
                  .collect();
              let null_position = DocumentPosition::new(0..0, vec![]);
              let bindings: Result<
                Vec<(Rc<str>, EaslTree)>,
                (SourceTrace, Rc<str>),
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
    "texture-load" => Some("textureLoad"),
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
