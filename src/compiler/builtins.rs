use std::collections::{HashMap, HashSet};

use std::sync::Arc;

use lazy_static::lazy_static;

use fsexp::{document::DocumentPosition, syntax::EncloserOrOperator};

use crate::compiler::effects::EffectType;
use crate::compiler::entry::BuiltinIOAttribute;
use crate::compiler::functions::{extract_mat_size, extract_vec_size};
use crate::compiler::program::{CompilerTarget, NameContext};
use crate::compiler::util::compile_word;
use crate::{
  compiler::{
    effects::Effect,
    entry::IOAttributes,
    error::SourceTrace,
    functions::{
      FunctionSignature, FunctionTargetConfiguration, Ownership,
      SpecialCasedBuiltinFunction,
    },
    structs::AbstractStructField,
    types::{AbstractArraySize, GenericArgument},
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

pub(crate) fn is_builtin_mat_constructor_type(name: &str) -> bool {
  // Matches matNxMs where N,M are digits 2-4 and s is f/i/u
  name.len() == 7
    && &name[0..3] == "mat"
    && name.as_bytes()[3].is_ascii_digit()
    && name.as_bytes()[4] == b'x'
    && name.as_bytes()[5].is_ascii_digit()
    && matches!(name.as_bytes()[6], b'f' | b'i' | b'u')
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
          implementation: FunctionImplementationKind::Builtin {
            effect_type: EffectType::empty(),
            target_configuration: FunctionTargetConfiguration::Default,
            target_specific_emulations: [CompilerTarget::C].into(),
          },
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
          implementation: FunctionImplementationKind::Builtin {
            effect_type: EffectType::empty(),
            target_configuration: FunctionTargetConfiguration::Default,
            target_specific_emulations: [CompilerTarget::C].into(),
          },
          ..Default::default()
        }))
        .collect::<Vec<AbstractFunctionSignature>>()
    })
    .collect()
}

pub fn atomic() -> AbstractStruct {
  AbstractStruct {
    name: ("Atomic".into(), SourceTrace::empty()),
    fields: vec![AbstractStructField {
      attributes: IOAttributes::empty(SourceTrace::empty()),
      name: "_".into(),
      field_type: AbstractType::Generic("T".into()),
      source_trace: SourceTrace::empty(),
    }],
    generic_args: vec![(
      "T".into(),
      GenericArgument::Type(vec![TypeConstraint::integer()]),
      SourceTrace::empty(),
    )],
    filled_generics: HashMap::new(),
    abstract_ancestor: None,
    source_trace: SourceTrace::empty(),
    opaque: true,
  }
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
  vec![vec2(), vec3(), vec4(), texture_2d(), sampler(), atomic()]
    .into_iter()
    .chain((2..=4).flat_map(|n| (2..=4).map(move |m| matrix(n, m))))
    .collect()
}

pub fn built_in_structs_for_target(
  target: CompilerTarget,
) -> Vec<AbstractStruct> {
  match target {
    CompilerTarget::Interpreter | CompilerTarget::WGSL => built_in_structs(),
    CompilerTarget::C => vec![],
  }
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
      implementation: FunctionImplementationKind::Builtin {
        effect_type: EffectType::empty(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: if name == "%" {
          [CompilerTarget::C].into()
        } else {
          HashSet::new()
        },
      },
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
          associative: associative
            && !assignment_fn
            && arg_vecs_or_scalars[0]
            && arg_vecs_or_scalars[1],
          implementation: FunctionImplementationKind::Builtin {
            effect_type: EffectType::empty(),
            target_configuration: FunctionTargetConfiguration::Default,
            target_specific_emulations: [CompilerTarget::C].into(),
          },
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
        let c_emulated_builtin = || FunctionImplementationKind::Builtin {
          effect_type: EffectType::empty(),
          target_configuration: FunctionTargetConfiguration::Default,
          target_specific_emulations: [CompilerTarget::C].into(),
        };
        [
          AbstractFunctionSignature {
            name: "+".into(),
            generic_args: scalar_generic.clone(),
            arg_types: vec![mat.clone().owned(), mat.clone().owned()],
            return_type: mat.clone(),
            associative: true,
            implementation: c_emulated_builtin(),
            ..Default::default()
          },
          AbstractFunctionSignature {
            name: "-".into(),
            generic_args: scalar_generic.clone(),
            arg_types: vec![mat.clone().owned(), mat.clone().owned()],
            return_type: mat.clone(),
            implementation: c_emulated_builtin(),
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
            implementation: c_emulated_builtin(),
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
            implementation: c_emulated_builtin(),
            ..Default::default()
          },
          AbstractFunctionSignature {
            name: "*".into(),
            generic_args: scalar_generic.clone(),
            arg_types: vec![mat.clone().owned(), vecn(n).owned()],
            return_type: vecn(m),
            implementation: c_emulated_builtin(),
            ..Default::default()
          },
          AbstractFunctionSignature {
            name: "*".into(),
            generic_args: scalar_generic.clone(),
            arg_types: vec![vecn(m).owned(), mat.clone().owned()],
            return_type: vecn(n),
            implementation: c_emulated_builtin(),
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
          implementation: c_emulated_builtin(),
          ..Default::default()
        }))
      })
    })
    .collect()
}

pub fn misc_matrix_functions() -> Vec<AbstractFunctionSignature> {
  (2..=4)
    .map(|i| {
      let m = specialized_matrix_type(i, i, "f");
      AbstractFunctionSignature {
        name: "determinant".into(),
        arg_types: vec![m.owned()],
        return_type: AbstractType::Type(Type::F32),
        implementation: FunctionImplementationKind::Builtin {
          effect_type: EffectType::empty(),
          target_configuration: FunctionTargetConfiguration::Default,
          target_specific_emulations: [CompilerTarget::C].into(),
        },
        ..Default::default()
      }
    })
    .chain((2..=4).flat_map(|x| {
      (2..=4).map(move |y| AbstractFunctionSignature {
        name: "transpose".into(),
        arg_types: vec![specialized_matrix_type(x, y, "f").owned()],
        return_type: specialized_matrix_type(y, x, "f"),
        implementation: FunctionImplementationKind::Builtin {
          effect_type: EffectType::empty(),
          target_configuration: FunctionTargetConfiguration::Default,
          target_specific_emulations: [CompilerTarget::C].into(),
        },
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
        implementation: FunctionImplementationKind::Builtin {
          effect_type: EffectType::empty(),
          target_configuration: FunctionTargetConfiguration::Default,
          target_specific_emulations: [CompilerTarget::C].into(),
        },
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
        implementation: FunctionImplementationKind::Builtin {
          effect_type: EffectType::empty(),
          target_configuration: FunctionTargetConfiguration::Default,
          target_specific_emulations: [CompilerTarget::C].into(),
        },
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
      implementation: FunctionImplementationKind::Builtin {
        effect_type: EffectType::empty(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: [CompilerTarget::C].into(),
      },
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
    implementation: FunctionImplementationKind::Builtin {
      effect_type: EffectType::empty(),
      target_configuration: FunctionTargetConfiguration::Default,
      target_specific_emulations: [CompilerTarget::C].into(),
    },
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
            implementation: FunctionImplementationKind::Builtin {
              effect_type: EffectType::empty(),
              target_configuration: FunctionTargetConfiguration::Default,
              target_specific_emulations: [CompilerTarget::C].into(),
            },
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
        implementation: FunctionImplementationKind::Builtin {
          effect_type: EffectType::empty(),
          target_configuration: FunctionTargetConfiguration::Default,
          target_specific_emulations: [CompilerTarget::C].into(),
        },
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
      implementation: FunctionImplementationKind::Builtin {
        effect_type: EffectType::empty(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: [CompilerTarget::C].into(),
      },
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
      implementation: FunctionImplementationKind::Builtin {
        effect_type: EffectType::empty(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: [CompilerTarget::C].into(),
      },
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
    implementation: FunctionImplementationKind::Builtin {
      effect_type: EffectType::empty(),
      target_configuration: FunctionTargetConfiguration::Default,
      target_specific_emulations: [CompilerTarget::C].into(),
    },
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
      implementation: FunctionImplementationKind::Builtin {
        effect_type: EffectType::empty(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: [CompilerTarget::C].into(),
      },
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
      implementation: FunctionImplementationKind::Builtin {
        effect_type: EffectType::empty(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: [CompilerTarget::C].into(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "f32".into(),
      generic_args: scalar_generic.clone(),
      arg_types: vec![AbstractType::Generic("T".into()).owned()],
      return_type: AbstractType::Type(Type::F32),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: EffectType::empty(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: [CompilerTarget::C].into(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "u32".into(),
      generic_args: scalar_generic.clone(),
      arg_types: vec![AbstractType::Generic("T".into()).owned()],
      return_type: AbstractType::Type(Type::U32),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: EffectType::empty(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: [CompilerTarget::C].into(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "bool".into(),
      generic_args: scalar_generic,
      arg_types: vec![AbstractType::Generic("T".into()).owned()],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: EffectType::empty(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: [CompilerTarget::C].into(),
      },
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
        implementation: FunctionImplementationKind::Builtin {
          effect_type: EffectType::empty(),
          target_configuration: FunctionTargetConfiguration::Default,
          target_specific_emulations: [CompilerTarget::C].into(),
        },
        ..Default::default()
      },
      AbstractFunctionSignature {
        name: "all".into(),
        arg_types: vec![t.owned()],
        return_type: AbstractType::Type(Type::Bool),
        implementation: FunctionImplementationKind::Builtin {
          effect_type: EffectType::empty(),
          target_configuration: FunctionTargetConfiguration::Default,
          target_specific_emulations: [CompilerTarget::C].into(),
        },
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
        implementation: FunctionImplementationKind::Builtin {
          effect_type: EffectType::empty(),
          target_configuration: FunctionTargetConfiguration::Default,
          target_specific_emulations: [CompilerTarget::C].into(),
        },
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
        ("inverse-sqrt", 1),
        ("trunc", 1),
        ("saturate", 1),
        ("degrees", 1),
        ("radians", 1),
        ("pow", 2),
        ("mix", 3),
        ("smoothstep", 3),
        ("step", 2),
        ("fma", 3),
      ]
      .into_iter()
      .map(|(name, arg_count)| AbstractFunctionSignature {
        name: name.into(),
        arg_types: std::iter::repeat(t.clone().owned())
          .take(arg_count)
          .collect(),
        return_type: t.clone(),
        implementation: FunctionImplementationKind::Builtin {
          effect_type: EffectType::empty(),
          target_configuration: FunctionTargetConfiguration::Default,
          target_specific_emulations: [CompilerTarget::C].into(),
        },
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
          implementation: FunctionImplementationKind::Builtin {
            effect_type: EffectType::empty(),
            target_configuration: FunctionTargetConfiguration::Default,
            target_specific_emulations: [CompilerTarget::C].into(),
          },
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
          implementation: FunctionImplementationKind::Builtin {
            effect_type: EffectType::empty(),
            target_configuration: FunctionTargetConfiguration::Default,
            target_specific_emulations: [CompilerTarget::C].into(),
          },
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
        target_specific_emulations: HashSet::new(),
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
        target_specific_emulations: HashSet::new(),
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
      implementation: FunctionImplementationKind::Builtin {
        effect_type: EffectType::empty(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: [CompilerTarget::C].into(),
      },
      ..Default::default()
    }];
    if unpack {
      signatures.push(AbstractFunctionSignature {
        name: format!("unpack-{name}").into(),
        arg_types: vec![AbstractType::Type(Type::U32).owned()],
        return_type: AbstractType::AbstractStruct(
          v.fill_abstract_generics(vec![AbstractType::Type(t)]).into(),
        ),
        implementation: FunctionImplementationKind::Builtin {
          effect_type: EffectType::empty(),
          target_configuration: FunctionTargetConfiguration::Default,
          target_specific_emulations: [CompilerTarget::C].into(),
        },
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
      generic_args: generic_args.clone(),
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
      implementation: FunctionImplementationKind::Builtin {
        effect_type: EffectType::empty(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: [CompilerTarget::C].into(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "zeroed-array".into(),
      generic_args,
      arg_types: vec![],
      return_type: AbstractType::AbstractArray {
        size: AbstractArraySize::Generic("S".into()),
        inner_type: AbstractType::Generic("T".into()).into(),
        source_trace: SourceTrace::empty(),
      },
      implementation: FunctionImplementationKind::Builtin {
        effect_type: EffectType::empty(),
        target_configuration: FunctionTargetConfiguration::SpecialCased(
          SpecialCasedBuiltinFunction::ZeroedArray,
        ),
        target_specific_emulations: [CompilerTarget::C].into(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "zeroed-array".into(),
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      arg_types: vec![AbstractType::Type(Type::U32).owned()],
      return_type: AbstractType::AbstractArray {
        size: AbstractArraySize::Unsized,
        inner_type: AbstractType::Generic("T".into()).into(),
        source_trace: SourceTrace::empty(),
      },
      implementation: FunctionImplementationKind::Builtin {
        effect_type: Effect::CPUExclusiveFunction("zeroed-array".into()).into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: [CompilerTarget::C].into(),
      },
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
          target_specific_emulations: HashSet::new(),
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
      target_specific_emulations: HashSet::new(),
    },
    ..Default::default()
  }]
}

fn shader_dispatch_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      name: "dispatch-render-shaders".into(),
      generic_args: vec![
        (
          "V".into(),
          GenericArgument::Type(vec![TypeConstraint::function()]),
          SourceTrace::empty(),
        ),
        (
          "F".into(),
          GenericArgument::Type(vec![TypeConstraint::function()]),
          SourceTrace::empty(),
        ),
      ],
      arg_types: vec![
        AbstractType::Generic("V".into()).owned(),
        AbstractType::Generic("F".into()).owned(),
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
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    // 4-argument variant: (vert frag vert-count additive-blend?)
    AbstractFunctionSignature {
      name: "dispatch-render-shaders".into(),
      generic_args: vec![
        (
          "V".into(),
          GenericArgument::Type(vec![TypeConstraint::function()]),
          SourceTrace::empty(),
        ),
        (
          "F".into(),
          GenericArgument::Type(vec![TypeConstraint::function()]),
          SourceTrace::empty(),
        ),
      ],
      arg_types: vec![
        AbstractType::Generic("V".into()).owned(),
        AbstractType::Generic("F".into()).owned(),
        AbstractType::Type(Type::U32).owned(),
        AbstractType::Type(Type::Bool).owned(),
      ],
      return_type: AbstractType::Unit,
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("dispatch-render-shaders".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "dispatch-compute-shader".into(),
      generic_args: vec![(
        "F".into(),
        GenericArgument::Type(vec![TypeConstraint::function()]),
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
        target_specific_emulations: HashSet::new(),
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
        target_specific_emulations: HashSet::new(),
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
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "start-audio".into(),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("start-audio".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "window-resolution".into(),
      return_type: AbstractType::AbstractStruct(
        vec2()
          .fill_abstract_generics(vec![AbstractType::Type(Type::U32)])
          .into(),
      ),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("window-resolution".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "window-time".into(),
      return_type: AbstractType::Type(Type::F32),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("window-time".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "window-delta-time".into(),
      return_type: AbstractType::Type(Type::F32),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("window-delta-time".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "window-frame-index".into(),
      return_type: AbstractType::Type(Type::U32),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("window-frame-index".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "mouse-coords".into(),
      return_type: AbstractType::AbstractStruct(
        vec2()
          .fill_abstract_generics(vec![AbstractType::Type(Type::U32)])
          .into(),
      ),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("mouse-coords".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "mouse-present?".into(),
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("mouse-present?".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "mouse-down?".into(),
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("mouse-down?".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "mouse-just-down?".into(),
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("mouse-just-down?".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "key-down?".into(),
      arg_types: vec![AbstractType::Type(Type::String).owned()],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("key-down?".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "key-just-down?".into(),
      arg_types: vec![AbstractType::Type(Type::String).owned()],
      return_type: AbstractType::Type(Type::Bool),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("key-just-down?".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "load-image".into(),
      arg_types: vec![AbstractType::Type(Type::String).owned()],
      return_type: AbstractType::AbstractStruct(
        texture_2d()
          .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
          .into(),
      ),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: Effect::CPUExclusiveFunction("load-image".into()).into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "blank-texture".into(),
      arg_types: vec![
        AbstractType::Type(Type::U32).owned(),
        AbstractType::Type(Type::U32).owned(),
      ],
      return_type: AbstractType::AbstractStruct(
        texture_2d()
          .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
          .into(),
      ),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: Effect::CPUExclusiveFunction("blank-texture".into())
          .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "set-render-target".into(),
      arg_types: vec![
        AbstractType::AbstractStruct(
          texture_2d()
            .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
            .into(),
        )
        .owned(),
      ],
      return_type: AbstractType::Unit,
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("set-render-target".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "clear-render-target".into(),
      arg_types: vec![],
      return_type: AbstractType::Unit,
      implementation: FunctionImplementationKind::Builtin {
        effect_type: vec![
          Effect::Window,
          Effect::CPUExclusiveFunction("clear-render-target".into()),
        ]
        .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
    },
    AbstractFunctionSignature {
      name: "blank-texture".into(),
      arg_types: vec![
        AbstractType::AbstractStruct(
          vec2()
            .fill_abstract_generics(vec![AbstractType::Type(Type::U32)])
            .into(),
        )
        .owned(),
      ],
      return_type: AbstractType::AbstractStruct(
        texture_2d()
          .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
          .into(),
      ),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: Effect::CPUExclusiveFunction("blank-texture".into())
          .into(),
        target_configuration: FunctionTargetConfiguration::Default,
        target_specific_emulations: HashSet::new(),
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
      target_specific_emulations: [CompilerTarget::C].into(),
    },
    ..Default::default()
  }]
}

fn atomic_functions() -> Vec<AbstractFunctionSignature> {
  vec![
    AbstractFunctionSignature {
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      name: "atomic-store".into(),
      arg_types: vec![
        (
          AbstractType::AbstractStruct(atomic().into()),
          Ownership::MutableReference,
        ),
        AbstractType::Generic("T".into()).owned(),
      ],
      ..Default::default()
    },
    AbstractFunctionSignature {
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      name: "atomic-load".into(),
      arg_types: vec![(
        AbstractType::AbstractStruct(atomic().into()),
        Ownership::Reference,
      )],
      return_type: AbstractType::Generic("T".into()),
      ..Default::default()
    },
    AbstractFunctionSignature {
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      name: "atomic-add".into(),
      arg_types: vec![
        (
          AbstractType::AbstractStruct(atomic().into()),
          Ownership::MutableReference,
        ),
        AbstractType::Generic("T".into()).owned(),
      ],
      return_type: AbstractType::Generic("T".into()),
      ..Default::default()
    },
    AbstractFunctionSignature {
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      name: "atomic-sub".into(),
      arg_types: vec![
        (
          AbstractType::AbstractStruct(atomic().into()),
          Ownership::MutableReference,
        ),
        AbstractType::Generic("T".into()).owned(),
      ],
      return_type: AbstractType::Generic("T".into()),
      ..Default::default()
    },
    AbstractFunctionSignature {
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      name: "atomic-max".into(),
      arg_types: vec![
        (
          AbstractType::AbstractStruct(atomic().into()),
          Ownership::MutableReference,
        ),
        AbstractType::Generic("T".into()).owned(),
      ],
      return_type: AbstractType::Generic("T".into()),
      ..Default::default()
    },
    AbstractFunctionSignature {
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      name: "atomic-min".into(),
      arg_types: vec![
        (
          AbstractType::AbstractStruct(atomic().into()),
          Ownership::MutableReference,
        ),
        AbstractType::Generic("T".into()).owned(),
      ],
      return_type: AbstractType::Generic("T".into()),
      ..Default::default()
    },
    AbstractFunctionSignature {
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      name: "atomic-and".into(),
      arg_types: vec![
        (
          AbstractType::AbstractStruct(atomic().into()),
          Ownership::MutableReference,
        ),
        AbstractType::Generic("T".into()).owned(),
      ],
      return_type: AbstractType::Generic("T".into()),
      ..Default::default()
    },
    AbstractFunctionSignature {
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      name: "atomic-or".into(),
      arg_types: vec![
        (
          AbstractType::AbstractStruct(atomic().into()),
          Ownership::MutableReference,
        ),
        AbstractType::Generic("T".into()).owned(),
      ],
      return_type: AbstractType::Generic("T".into()),
      ..Default::default()
    },
    AbstractFunctionSignature {
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      name: "atomic-xor".into(),
      arg_types: vec![
        (
          AbstractType::AbstractStruct(atomic().into()),
          Ownership::MutableReference,
        ),
        AbstractType::Generic("T".into()).owned(),
      ],
      return_type: AbstractType::Generic("T".into()),
      ..Default::default()
    },
    AbstractFunctionSignature {
      generic_args: vec![(
        "T".into(),
        GenericArgument::Type(vec![]),
        SourceTrace::empty(),
      )],
      name: "atomic-exchange".into(),
      arg_types: vec![
        (
          AbstractType::AbstractStruct(atomic().into()),
          Ownership::MutableReference,
        ),
        AbstractType::Generic("T".into()).owned(),
      ],
      return_type: AbstractType::Generic("T".into()),
      ..Default::default()
    },
  ]
}

fn builtin_attribute_lookup_functions() -> Vec<AbstractFunctionSignature> {
  BuiltinIOAttribute::all()
    .into_iter()
    .map(|attribute| AbstractFunctionSignature {
      generic_args: vec![],
      name: attribute.name().into(),
      arg_types: vec![],
      return_type: attribute.abstract_type(),
      implementation: FunctionImplementationKind::Builtin {
        effect_type: Effect::LookupBuiltinAttribute(attribute).into(),
        target_configuration:
          FunctionTargetConfiguration::BuiltinAttributeLookup(attribute),
        target_specific_emulations: HashSet::new(),
      },
      ..Default::default()
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
  signatures.append(&mut atomic_functions());
  signatures.append(&mut builtin_attribute_lookup_functions());
  signatures
}

lazy_static! {
  pub static ref ASSIGNMENT_OPS: HashSet<&'static str> =
    ["=", "+=", "-=", "*=", "/=", "%=", "^=", ">>=", "<<="]
      .into_iter()
      .collect();
  pub static ref ATOMIC_MUTATION_OPS: HashSet<&'static str> = [
    "atomic-store",
    "atomic-add",
    "atomic-sub",
    "atomic-max",
    "atomic-min",
    "atomic-and",
    "atomic-or",
    "atomic-xor",
    "atomic-exchange",
  ]
  .into_iter()
  .collect();
  pub static ref INFIX_OPS: HashSet<&'static str> = [
    "==", "!=", ">=", ">", "<=", "<", "||", "&&", "+", "-", "*", "/", "%",
    ">>", "<<", "|", "&", "^",
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
        if !children.is_empty()
          && let EaslTree::Leaf(if_position, leaf) = &children[0]
          && leaf.as_str() == "if"
        {
          if children.len() == 4 {
            let if_position = if_position.clone();
            let false_branch = children.remove(3);
            let true_branch = children.remove(2);
            let condition = children.remove(1);
            let condition_position = condition.position().clone();
            Some(Ok(EaslTree::Inner(
              (
                position.clone(),
                EncloserOrOperator::Encloser(Encloser::Parens),
              ),
              vec![
                EaslTree::Leaf(if_position.clone(), "match".into()),
                condition,
                EaslTree::Leaf(condition_position.clone(), "true".into()),
                true_branch,
                EaslTree::Leaf(condition_position, "false".into()),
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
      }
      _ => None,
    }),
  };
  let when_macro = Macro {
    reserved_names: vec!["when".into()],
    rewrite: Box::new(|tree, _names| match tree {
      EaslTree::Inner(
        (enclosing_position, EncloserOrOperator::Encloser(Encloser::Parens)),
        children,
      ) => {
        let mut children = children.clone();
        if !children.is_empty()
          && let EaslTree::Leaf(when_position, leaf) = &children[0]
          && leaf.as_str() == "when"
        {
          let when_position = when_position.clone();
          if children.len() > 2 {
            let condition = children.remove(1);
            let condition_position = condition.position().clone();
            std::mem::swap(
              &mut children[0],
              &mut EaslTree::Leaf(when_position.clone(), "do".into()),
            );
            let unit_expression = EaslTree::Inner(
              (
                when_position.clone(),
                EncloserOrOperator::Encloser(Encloser::Parens),
              ),
              vec![],
            );
            children.push(unit_expression.clone());
            Some(Ok(EaslTree::Inner(
              (
                enclosing_position.clone(),
                EncloserOrOperator::Encloser(Encloser::Parens),
              ),
              vec![
                EaslTree::Leaf(when_position, "match".into()),
                condition,
                EaslTree::Leaf(condition_position.clone(), "true".into()),
                EaslTree::Inner(
                  (
                    enclosing_position.clone(),
                    EncloserOrOperator::Encloser(Encloser::Parens),
                  ),
                  children,
                ),
                EaslTree::Leaf(condition_position, "false".into()),
                unit_expression,
              ],
            )))
          } else {
            Some(Err((
              SourceTrace::from(enclosing_position),
              "\"when\" statement expects a condition and at least 1 body \
                  statement"
                .into(),
            )))
          }
        } else {
          None
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
    "atomic-store" => Some("atomicStore"),
    "atomic-load" => Some("atomicLoad"),
    "atomic-add" => Some("atomicAdd"),
    "atomic-sub" => Some("atomicSub"),
    "atomic-max" => Some("atomicMax"),
    "atomic-min" => Some("atomicMin"),
    "atomic-and" => Some("atomicAnd"),
    "atomic-or" => Some("atomicOr"),
    "atomic-xor" => Some("atomicXor"),
    "atomic-exchange" => Some("atomicExchange"),
    "inverse-sqrt" => Some("inverseSqrt"),
    _ => None,
  }
  .map(|name| name.to_string())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EmulatedFunctionSignature {
  pub name: String,
  pub arg_types: Vec<String>,
  pub return_type: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EmulatedFunctionResult {
  pub name: String,
  pub helper_chunks: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct EmulatedFunctionRecord {
  pub emulated_signatures: HashMap<EmulatedFunctionSignature, String>,
  pub helper_chunks: Vec<String>,
}

impl EmulatedFunctionRecord {
  pub fn empty() -> Self {
    Self {
      emulated_signatures: HashMap::new(),
      helper_chunks: vec![],
    }
  }
  fn track_emulated_elementwise_scalar_builtin(
    &mut self,
    name: &str,
    scalar_type: &str,
    arg_types: Vec<String>,
    _target: CompilerTarget,
    names: &mut NameContext,
  ) -> String {
    let signature = EmulatedFunctionSignature {
      name: name.to_string(),
      arg_types: arg_types,
      return_type: scalar_type.to_string(),
    };
    if let Some(existing_name) = self.emulated_signatures.get(&signature) {
      return existing_name.clone();
    }
    let new_name = match name {
      "acos" | "cos" | "sin" | "tan" | "atan" | "asin" | "floor" | "ceil"
      | "round" | "trunc" | "exp" | "exp2" | "log" | "log2" | "pow"
      | "sinh" | "cosh" | "tanh" | "asinh" | "atanh" | "acosh" | "fma"
      | "ldexp" | "sqrt" | "atan2" => name.to_string(),
      "inverse-sqrt" => {
        let new_name = names.gensym("inverse_sqrt");
        self.helper_chunks.push(format!(
          "{scalar_type} {new_name} ({scalar_type} x) {{\n  \
                return 1. / sqrt(x);\n\
              }}"
        ));
        new_name.to_string()
      }
      "abs" => {
        let new_name = names.gensym("abs");
        self.helper_chunks.push(match scalar_type {
          "float" => format!(
            "float {new_name}(float x) {{\n  \
              return fabs(x);\n\
            }}"
          ),
          "int32_t" => format!(
            "int32_t {new_name}(int32_t x) {{\n  \
              return abs(x);\n\
            }}"
          ),
          _ => panic!(),
        });
        new_name.to_string()
      }
      "sign" => {
        let new_name = names.gensym("sign");
        self.helper_chunks.push(match scalar_type {
          "float" => format!(
            "float {new_name}(float x) {{\n  \
             return (x > 0.) ? 1. : (x < 0.) ? -1. : 0.;\n\
           }}"
          ),
          "int32_t" => format!(
            "int32_t {new_name}(int32_t x) {{\n  \
             return (x > 0) ? 1 : (x < 0) ? -1 : 0;\n\
           }}"
          ),
          _ => panic!(),
        });
        new_name.to_string()
      }
      "fract" => {
        let new_name = names.gensym("fract");
        self.helper_chunks.push(format!(
          "float {new_name}(float x) {{\n  \
              return x - floor(x);\n\
            }}"
        ));
        new_name.to_string()
      }
      "saturate" => {
        let new_name = names.gensym("saturate");
        self.helper_chunks.push(format!(
          "float {new_name}(float x) {{\n  \
             return x < 0. ? 0. : x > 1. ? 1. : x;\n\
           }}"
        ));
        new_name.to_string()
      }
      "degrees" => {
        let new_name = names.gensym("degrees");
        self.helper_chunks.push(format!(
          "float {new_name}(float x) {{\n  \
             return x * (180. / 3.14159265358979323846);\n\
           }}"
        ));
        new_name.to_string()
      }
      "radians" => {
        let new_name = names.gensym("radians");
        self.helper_chunks.push(format!(
          "float {new_name}(float x) {{\n  \
             return x * (3.14159265358979323846 / 180.);\n\
           }}"
        ));
        new_name.to_string()
      }
      "step" => {
        let new_name = names.gensym("step");
        self.helper_chunks.push(format!(
          "float {new_name}(float edge, float x) {{\n  \
             return x >= edge ? 1. : 0.;\n\
           }}"
        ));
        new_name.to_string()
      }
      "mix" => {
        let new_name = names.gensym("mix");
        self.helper_chunks.push(format!(
          "float {new_name}(float x, float y, float p) {{\n  \
             return x * (1. - p) + y * p;\n\
           }}"
        ));
        new_name.to_string()
      }
      "smoothstep" => {
        let new_name = names.gensym("smoothstep");
        self.helper_chunks.push(format!(
          "float {new_name}(float edge0, float edge1, float x) {{\n  \
             float t = (x - edge0) / (edge1 - edge0);\n  \
             t = t < 0. ? 0. : t > 1. ? 1. : t;\n  \
             return t * t * (3. - 2. * t);\n\
           }}"
        ));
        new_name.to_string()
      }
      "clamp" => {
        let new_name = names.gensym("clamp");
        self.helper_chunks.push(format!(
          "{scalar_type} {new_name}({scalar_type} x, \
           {scalar_type} lo, {scalar_type} hi) {{\n  \
             return x < lo ? lo : x > hi ? hi : x;\n\
           }}"
        ));
        new_name.to_string()
      }
      "min" => {
        let new_name = names.gensym("min");
        self.helper_chunks.push(format!(
          "{scalar_type} {new_name}({scalar_type} x, {scalar_type} y) {{\n  \
             return x < y ? x : y;\n\
           }}"
        ));
        new_name.to_string()
      }
      "max" => {
        let new_name = names.gensym("max");
        self.helper_chunks.push(format!(
          "{scalar_type} {new_name}({scalar_type} x, {scalar_type} y) {{\n  \
             return x > y ? x : y;\n\
           }}"
        ));
        new_name.to_string()
      }
      "count-one-bits" => {
        let new_name = names.gensym("count_one_bits");
        self.helper_chunks.push(format!(
          "{scalar_type} {new_name}({scalar_type} x) {{\n  \
             return ({scalar_type})__builtin_popcount((uint32_t)x);\n\
           }}"
        ));
        new_name.to_string()
      }
      "count-leading-zeros" => {
        let new_name = names.gensym("count_leading_zeros");
        self.helper_chunks.push(format!(
          "{scalar_type} {new_name}({scalar_type} x) {{\n  \
             return ({scalar_type})(x == 0 ? 32 : __builtin_clz((uint32_t)x));\
             \n}}"
        ));
        new_name.to_string()
      }
      "count-trailing-zeros" => {
        let new_name = names.gensym("count_trailing_zeros");
        self.helper_chunks.push(format!(
          "{scalar_type} {new_name}({scalar_type} x) {{\n  \
             return ({scalar_type})(x == 0 ? 32 : __builtin_ctz((uint32_t)x));\
             \n}}"
        ));
        new_name.to_string()
      }
      "reverse-bits" => {
        let new_name = names.gensym("reverse_bits");
        self.helper_chunks.push(format!(
          "{scalar_type} {new_name}({scalar_type} x) {{\n  \
             uint32_t v = (uint32_t)x;\n  \
             v = ((v >> 1) & 0x55555555u) | ((v & 0x55555555u) << 1);\n  \
             v = ((v >> 2) & 0x33333333u) | ((v & 0x33333333u) << 2);\n  \
             v = ((v >> 4) & 0x0F0F0F0Fu) | ((v & 0x0F0F0F0Fu) << 4);\n  \
             v = ((v >> 8) & 0x00FF00FFu) | ((v & 0x00FF00FFu) << 8);\n  \
             v = (v >> 16) | (v << 16);\n  \
             return ({scalar_type})v;\n\
           }}"
        ));
        new_name.to_string()
      }
      "first-leading-bit" => {
        let new_name = names.gensym("first_leading_bit");
        self.helper_chunks.push(match scalar_type {
          "uint32_t" => format!(
            "uint32_t {new_name}(uint32_t x) {{\n  \
               return x == 0u ? 0xFFFFFFFFu : (uint32_t)(31 - __builtin_clz(x))\
               ;\n\
             }}"
          ),
          "int32_t" => format!(
            "int32_t {new_name}(int32_t x) {{\n  \
               if (x == 0 || x == -1) return -1;\n  \
               uint32_t v = x < 0 ? (uint32_t)~x : (uint32_t)x;\n  \
               return (int32_t)(31 - __builtin_clz(v));\n\
             }}"
          ),
          _ => panic!(),
        });
        new_name.to_string()
      }
      "first-trailing-bit" => {
        let new_name = names.gensym("first_trailing_bit");
        self.helper_chunks.push(format!(
          "{scalar_type} {new_name}({scalar_type} x) {{\n  \
             return x == 0 ? ({scalar_type})(-1) : ({scalar_type})__builtin_ctz\
             ((uint32_t)x);\n\
           }}"
        ));
        new_name.to_string()
      }
      "extract-bits" => {
        let new_name = names.gensym("extract_bits");
        self.helper_chunks.push(match scalar_type {
          "uint32_t" => format!(
            "uint32_t {new_name}(uint32_t e, uint32_t offset, uint32_t count) \
             {{\n  \
               uint32_t s = offset < 32u ? offset : 32u;\n  \
               uint32_t c = (count + s) < 32u ? count : (32u - s);\n  \
               if (c == 0u) return 0u;\n  \
               if (c == 32u) return e;\n  \
               return (e >> s) & ((1u << c) - 1u);\n\
             }}"
          ),
          "int32_t" => format!(
            "int32_t {new_name}(int32_t e, uint32_t offset, uint32_t count) \
             {{\n  \
               uint32_t s = offset < 32u ? offset : 32u;\n  \
               uint32_t c = (count + s) < 32u ? count : (32u - s);\n  \
               if (c == 0u) return 0;\n  \
               if (c == 32u) return e;\n  \
               uint32_t bits = ((uint32_t)e >> s) & ((1u << c) - 1u);\n  \
               uint32_t sign_bit = 1u << (c - 1u);\n  \
               return (int32_t)((bits ^ sign_bit) - sign_bit);\n\
             }}"
          ),
          _ => panic!(),
        });
        new_name.to_string()
      }
      _ => panic!(),
    };
    self.emulated_signatures.insert(signature, new_name.clone());
    new_name
  }
  pub fn track_emulated_builtin(
    &mut self,
    signature: EmulatedFunctionSignature,
    target: CompilerTarget,
    names: &mut NameContext,
  ) -> String {
    if let Some(existing_name) = self.emulated_signatures.get(&signature) {
      return existing_name.clone();
    }
    let name = signature.name.as_str();
    let get_vec_inner_scalar_name =
      |vec_name: &str| match vec_name.char_indices().last().unwrap().1 {
        'f' => "float",
        'i' => "int32_t",
        'u' => "uint32_t",
        'b' => "bool",
        _ => panic!(),
      };
    let is_scalar_type = |type_name: &str| -> bool {
      matches!(type_name, "float" | "int32_t" | "uint32_t" | "bool")
    };
    let new_name = if let Some(size) = extract_vec_size(name) {
      let inner_scalar_name = get_vec_inner_scalar_name(name);
      let new_name = names.gensym(
        &std::iter::once(format!("{name}_from"))
          .chain(signature.arg_types.iter().cloned())
          .collect::<Vec<String>>()
          .join("_"),
      );
      if signature.arg_types.len() == 1
        && let scalar_type_name = &signature.arg_types[0]
        && is_scalar_type(&scalar_type_name)
      {
        let args = std::iter::repeat(format!("({inner_scalar_name})x"))
          .take(size)
          .collect::<Vec<String>>()
          .join(", ");
        self.helper_chunks.push(format!(
          "{name} {new_name}({scalar_type_name} x) {{\n  \
             return ({name}){{{args}}};\n\
           }}"
        ));
      } else {
        let arg_names = ["a", "b", "c", "d"]
          .iter()
          .take(signature.arg_types.len())
          .copied()
          .collect::<Vec<_>>();
        let arg_string = signature
          .arg_types
          .iter()
          .zip(arg_names.iter())
          .map(|(type_name, arg_name)| format!("{type_name} {arg_name}"))
          .collect::<Vec<String>>()
          .join(", ");
        let mut function = format!(
          "{name} {new_name}({arg_string}) {{\n  \
             {name} v;\n  "
        );
        let mut dimension_index = 0;
        let dimensions = ["x", "y", "z", "w"];
        for (arg_type_name, arg_name) in
          signature.arg_types.iter().zip(arg_names.iter())
        {
          if is_scalar_type(arg_type_name) {
            let dim = dimensions[dimension_index];
            function +=
              &format!("v.{dim} = ({inner_scalar_name}){arg_name};\n  ");
            dimension_index += 1;
          } else {
            let inner_size = extract_vec_size(arg_type_name).unwrap();
            for i in 0..inner_size {
              let dim = dimensions[dimension_index];
              let arg_dim = dimensions[i];
              function += &format!(
                "v.{dim} = ({inner_scalar_name}){arg_name}.{arg_dim};\n  "
              );
              dimension_index += 1;
            }
          }
        }
        function += "return v;\n}";
        self.helper_chunks.push(function);
      }
      new_name.to_string()
    } else {
      match name {
        "acos"
        | "cos"
        | "sin"
        | "tan"
        | "atan"
        | "asin"
        | "floor"
        | "ceil"
        | "round"
        | "trunc"
        | "exp"
        | "exp2"
        | "log"
        | "log2"
        | "pow"
        | "sinh"
        | "cosh"
        | "tanh"
        | "asinh"
        | "atanh"
        | "acosh"
        | "fma"
        | "ldexp"
        | "sqrt"
        | "atan2"
        | "inverse-sqrt"
        | "abs"
        | "sign"
        | "fract"
        | "saturate"
        | "degrees"
        | "radians"
        | "step"
        | "mix"
        | "smoothstep"
        | "clamp"
        | "min"
        | "max"
        | "count-one-bits"
        | "count-leading-zeros"
        | "count-trailing-zeros"
        | "reverse-bits"
        | "first-leading-bit"
        | "first-trailing-bit"
        | "extract-bits" => {
          // All of these functions are implemented for individual floats, as well
          // as element-wise for each of the vec2-vec4 types. The vector
          // implementations can just be represented as an application of the
          // float implementation n times, so this block first generates the float
          // implementation and then, if necessary, implements the requested vec
          // version in terms of the float version.
          let to_scalar = |t: &str| match t {
            "float" => "float",
            "int32_t" => "int32_t",
            "uint32_t" => "uint32_t",
            "bool" => "bool",
            _ => get_vec_inner_scalar_name(t),
          };
          let scalar_fn_name = self.track_emulated_elementwise_scalar_builtin(
            &signature.name,
            to_scalar(&signature.return_type),
            signature
              .arg_types
              .iter()
              .map(|t| to_scalar(t).to_string())
              .collect(),
            target,
            names,
          );
          if is_scalar_type(signature.return_type.as_str()) {
            return scalar_fn_name;
          }
          let vec_type_name = signature.return_type.clone();
          let size = extract_vec_size(&vec_type_name).unwrap();
          let compiled_name = compile_word(name.into());
          let new_name = names.gensym(&format!("{compiled_name}_{size}"));
          let arg_names: Vec<&str> = ["a", "b", "c", "d"]
            .iter()
            .take(signature.arg_types.len())
            .copied()
            .collect();
          let arg_and_type_signature = arg_names
            .iter()
            .zip(signature.arg_types.iter())
            .map(|(arg_name, arg_type)| format!("{arg_type} {arg_name}"))
            .collect::<Vec<String>>()
            .join(", ");
          let mut body = format!(
            "{vec_type_name} {new_name} ({arg_and_type_signature}) {{\n  \
              {vec_type_name} y;\n  \
              "
          );
          for field in ["x", "y", "z", "w"].iter().take(size) {
            let arg_accessors = arg_names
              .iter()
              .zip(signature.arg_types.iter())
              .map(|(arg_name, arg_type)| {
                if is_scalar_type(arg_type) {
                  arg_name.to_string()
                } else {
                  format!("{arg_name}.{field}")
                }
              })
              .collect::<Vec<String>>()
              .join(", ");
            body +=
              &format!("y.{field} = {scalar_fn_name}({arg_accessors});\n  ");
          }
          body += "return y;\n}";
          self.helper_chunks.push(body);
          new_name.to_string()
        }
        "f32" => "(float)".to_string(),
        "u32" => "(uint32_t)".to_string(),
        "i32" => "(int32_t)".to_string(),
        "bool" => "(bool)".to_string(),
        "dot" => {
          let new_name = names.gensym("dot");
          let vec_type_name = &signature.arg_types[0];
          let mut function_body = format!(
            "float {new_name}({vec_type_name} a, {vec_type_name} b) \
          {{\n  \
            float sum = 0.;\n  \
          "
          );
          for field in ["x", "y", "z", "w"]
            .iter()
            .take(extract_vec_size(&vec_type_name).unwrap())
          {
            function_body += &format!("sum += a.{field} * b.{field};\n  ");
          }
          function_body += "return sum;\n}";
          self.helper_chunks.push(function_body);
          new_name.to_string()
        }
        "cross" => {
          let new_name = names.gensym("cross");
          self.helper_chunks.push(format!(
            "vec3f {new_name}(vec3f a, vec3f b) {{\n  \
             vec3f result;\n  \
             result.x = a.y * b.z - a.z * b.y;\n  \
             result.y = a.z * b.x - a.x * b.z;\n  \
             result.z = a.x * b.y - a.y * b.x;\n  \
             return result;\n\
           }}"
          ));
          new_name.to_string()
        }
        "length" => {
          let arg_type = signature.arg_types[0].clone();
          if arg_type == "float" {
            let new_name = names.gensym("length");
            self.helper_chunks.push(format!(
              "float {new_name}(float x) {{\n  \
               return fabs(x);\n\
             }}"
            ));
            new_name.to_string()
          } else {
            let size = extract_vec_size(&arg_type).unwrap();
            let dot_fn = self.track_emulated_builtin(
              EmulatedFunctionSignature {
                name: "dot".to_string(),
                arg_types: vec![arg_type.clone(), arg_type.clone()],
                return_type: "float".to_string(),
              },
              target,
              names,
            );
            let new_name = names.gensym(&format!("length_{size}"));
            self.helper_chunks.push(format!(
              "float {new_name}({arg_type} v) {{\n  \
                 return sqrt({dot_fn}(v, v));\n\
               }}"
            ));
            new_name.to_string()
          }
        }
        "normalize" => {
          let vec_type_name = signature.arg_types[0].clone();
          let size = extract_vec_size(&vec_type_name).unwrap();
          let length_fn = self.track_emulated_builtin(
            EmulatedFunctionSignature {
              name: "length".to_string(),
              arg_types: vec![vec_type_name.clone()],
              return_type: "float".to_string(),
            },
            target,
            names,
          );
          let new_name = names.gensym(&format!("normalize_{size}"));
          let mut body = format!(
            "{vec_type_name} {new_name}({vec_type_name} v) {{\n  \
             float len = {length_fn}(v);\n  \
             {vec_type_name} result;\n  "
          );
          for field in ["x", "y", "z", "w"].iter().take(size) {
            body += &format!("result.{field} = v.{field} / len;\n  ");
          }
          body += "return result;\n}";
          self.helper_chunks.push(body);
          new_name.to_string()
        }
        "distance" => {
          let arg_type = signature.arg_types[0].clone();
          if arg_type == "float" {
            let new_name = names.gensym("distance");
            self.helper_chunks.push(format!(
              "float {new_name}(float a, float b) {{\n  \
               return fabs(a - b);\n\
             }}"
            ));
            new_name.to_string()
          } else {
            let size = extract_vec_size(&arg_type).unwrap();
            let new_name = names.gensym(&format!("distance_{size}"));
            let mut body =
              format!("float {new_name}({arg_type} a, {arg_type} b) {{\n  ");
            let terms: Vec<String> = ["x", "y", "z", "w"]
              .iter()
              .take(size)
              .map(|f| format!("(a.{f} - b.{f}) * (a.{f} - b.{f})"))
              .collect();
            body += &format!("return sqrt({});\n}}", terms.join(" + "));
            self.helper_chunks.push(body);
            new_name.to_string()
          }
        }
        "face-forward" => {
          let vec_type_name = signature.arg_types[0].clone();
          let size = extract_vec_size(&vec_type_name).unwrap();
          let dot_fn = self.track_emulated_builtin(
            EmulatedFunctionSignature {
              name: "dot".to_string(),
              arg_types: vec![vec_type_name.clone(), vec_type_name.clone()],
              return_type: "float".to_string(),
            },
            target,
            names,
          );
          let new_name = names.gensym(&format!("face_forward_{size}"));
          let mut body = format!(
            "{vec_type_name} {new_name}({vec_type_name} e1, {vec_type_name} e2, {vec_type_name} e3) {{\n  \
             float s = {dot_fn}(e3, e2) < 0. ? 1. : -1.;\n  \
             {vec_type_name} result;\n  "
          );
          for field in ["x", "y", "z", "w"].iter().take(size) {
            body += &format!("result.{field} = s * e1.{field};\n  ");
          }
          body += "return result;\n}";
          self.helper_chunks.push(body);
          new_name.to_string()
        }
        "reflect" => {
          let vec_type_name = signature.arg_types[0].clone();
          let size = extract_vec_size(&vec_type_name).unwrap();
          let dot_fn = self.track_emulated_builtin(
            EmulatedFunctionSignature {
              name: "dot".to_string(),
              arg_types: vec![vec_type_name.clone(), vec_type_name.clone()],
              return_type: "float".to_string(),
            },
            target,
            names,
          );
          let new_name = names.gensym(&format!("reflect_{size}"));
          let mut body = format!(
            "{vec_type_name} {new_name}({vec_type_name} e1, {vec_type_name} e2) {{\n  \
             float d = {dot_fn}(e2, e1);\n  \
             {vec_type_name} result;\n  "
          );
          for field in ["x", "y", "z", "w"].iter().take(size) {
            body += &format!(
              "result.{field} = e1.{field} - 2. * d * e2.{field};\n  "
            );
          }
          body += "return result;\n}";
          self.helper_chunks.push(body);
          new_name.to_string()
        }
        "refract" => {
          let vec_type_name = signature.arg_types[0].clone();
          let size = extract_vec_size(&vec_type_name).unwrap();
          let dot_fn = self.track_emulated_builtin(
            EmulatedFunctionSignature {
              name: "dot".to_string(),
              arg_types: vec![vec_type_name.clone(), vec_type_name.clone()],
              return_type: "float".to_string(),
            },
            target,
            names,
          );
          let new_name = names.gensym(&format!("refract_{size}"));
          let mut body = format!(
            "{vec_type_name} {new_name}({vec_type_name} e1, {vec_type_name} e2, float eta) {{\n  \
             float d = {dot_fn}(e2, e1);\n  \
             float k = 1. - eta * eta * (1. - d * d);\n  \
             {vec_type_name} result = {{0.}};\n  \
             if (k >= 0.) {{\n    \
               float t = eta * d + sqrt(k);\n    "
          );
          for field in ["x", "y", "z", "w"].iter().take(size) {
            body += &format!(
              "result.{field} = eta * e1.{field} - t * e2.{field};\n    "
            );
          }
          body += "}\n  return result;\n}";
          self.helper_chunks.push(body);
          new_name.to_string()
        }
        "bitcast<float>" | "bitcast<int32_t>" | "bitcast<uint32_t>"
        | "bitcast<vec2u>" | "bitcast<vec3u>" | "bitcast<vec4u>"
        | "bitcast<vec2i>" | "bitcast<vec3i>" | "bitcast<vec4i>"
        | "bitcast<vec2f>" | "bitcast<vec3f>" | "bitcast<vec4f>" => {
          let from_type = signature.arg_types[0].as_str();
          let to_type =
            &name[name.find('<').unwrap() + 1..name.find('>').unwrap()];
          let new_name =
            names.gensym(&format!("bitcast_{from_type}_to_{to_type}"));
          self.helper_chunks.push(format!(
            "{to_type} {new_name}({from_type} x) {{\n  \
               {to_type} result;\n  \
               memcpy(&result, &x, sizeof(result));\n  \
               return result;\n\
             }}"
          ));
          new_name.to_string()
        }
        "determinant" => {
          let mat_type = &signature.arg_types[0];
          let new_name = names.gensym("determinant");
          // matNxN: N is at index 3, always square for determinant
          let n: usize = mat_type[3..4].parse().unwrap();
          self.helper_chunks.push(match n {
            2 => format!(
              "float {new_name}({mat_type} m) {{\n  \
                 return m.c0.x * m.c1.y - m.c1.x * m.c0.y;\n\
               }}"
            ),
            3 => format!(
              "float {new_name}({mat_type} m) {{\n  \
                 return m.c0.x * (m.c1.y * m.c2.z - m.c2.y * m.c1.z)\n       \
                      - m.c1.x * (m.c0.y * m.c2.z - m.c2.y * m.c0.z)\n       \
                      + m.c2.x * (m.c0.y * m.c1.z - m.c1.y * m.c0.z);\n\
               }}"
            ),
            4 => format!(
              "float {new_name}({mat_type} m) {{\n  \
                 float s0 = m.c0.x * m.c1.y - m.c1.x * m.c0.y;\n  \
                 float s1 = m.c0.x * m.c2.y - m.c2.x * m.c0.y;\n  \
                 float s2 = m.c0.x * m.c3.y - m.c3.x * m.c0.y;\n  \
                 float s3 = m.c1.x * m.c2.y - m.c2.x * m.c1.y;\n  \
                 float s4 = m.c1.x * m.c3.y - m.c3.x * m.c1.y;\n  \
                 float s5 = m.c2.x * m.c3.y - m.c3.x * m.c2.y;\n  \
                 float c5 = m.c2.z * m.c3.w - m.c3.z * m.c2.w;\n  \
                 float c4 = m.c1.z * m.c3.w - m.c3.z * m.c1.w;\n  \
                 float c3 = m.c1.z * m.c2.w - m.c2.z * m.c1.w;\n  \
                 float c2 = m.c0.z * m.c3.w - m.c3.z * m.c0.w;\n  \
                 float c1 = m.c0.z * m.c2.w - m.c2.z * m.c0.w;\n  \
                 float c0 = m.c0.z * m.c1.w - m.c1.z * m.c0.w;\n  \
                 return s0 * c5 - s1 * c4 + s2 * c3 + s3 * c2 - s4 * c1 + s5 * c0;\n\
               }}"
            ),
            _ => panic!("determinant not defined for {n}x{n} matrices"),
          });
          new_name.to_string()
        }
        "+" | "-" | "*" | "/" | "%" => {
          let return_type = &signature.return_type;
          let non_symbol_name = match name {
            "+" => "add",
            "-" => "subtract",
            "*" => "multiply",
            "/" => "divide",
            "%" => "modulo",
            _ => panic!(),
          };
          let a_type = &signature.arg_types[0];
          let b_type = &signature.arg_types[1];
          let a_mat_size = extract_mat_size(a_type);
          let b_mat_size = extract_mat_size(b_type);
          let return_mat_size = extract_mat_size(return_type);
          let new_name =
            names.gensym(&format!("{non_symbol_name}_{a_type}_{b_type}"));
          let fields = ["x", "y", "z", "w"];
          let function = if a_mat_size.is_some()
            || b_mat_size.is_some()
            || return_mat_size.is_some()
          {
            let mut body = format!(
              "{return_type} {new_name}({a_type} a, {b_type} b) {{\n  \
                 {return_type} y;\n  "
            );
            match (a_mat_size, b_mat_size, return_mat_size) {
              (Some((ac, ar)), Some((bc, br)), Some((rc, rr))) => {
                if name == "*" {
                  // matrix multiplication: A is matACxAR, B is matBCxBR,
                  // result is matBCxAR with shared dimension AC == BR
                  assert_eq!(ac, br);
                  assert_eq!(rc, bc);
                  assert_eq!(rr, ar);
                  for j in 0..rc {
                    for i in 0..rr {
                      let terms: Vec<String> = (0..ac)
                        .map(|k| {
                          format!("a.c{k}.{} * b.c{j}.{}", fields[i], fields[k])
                        })
                        .collect();
                      body += &format!(
                        "y.c{j}.{} = {};\n  ",
                        fields[i],
                        terms.join(" + ")
                      );
                    }
                  }
                } else {
                  // element-wise + or -
                  assert_eq!(ac, bc);
                  assert_eq!(ar, br);
                  assert_eq!(rc, ac);
                  assert_eq!(rr, ar);
                  for j in 0..rc {
                    for i in 0..rr {
                      body += &format!(
                        "y.c{j}.{} = a.c{j}.{} {name} b.c{j}.{};\n  ",
                        fields[i], fields[i], fields[i]
                      );
                    }
                  }
                }
              }
              (Some((ac, ar)), None, Some((rc, rr))) => {
                // matrix * scalar (element-wise)
                assert_eq!(rc, ac);
                assert_eq!(rr, ar);
                for j in 0..rc {
                  for i in 0..rr {
                    body += &format!(
                      "y.c{j}.{} = a.c{j}.{} {name} b;\n  ",
                      fields[i], fields[i]
                    );
                  }
                }
              }
              (None, Some((bc, br)), Some((rc, rr))) => {
                // scalar * matrix (element-wise)
                assert_eq!(rc, bc);
                assert_eq!(rr, br);
                for j in 0..rc {
                  for i in 0..rr {
                    body += &format!(
                      "y.c{j}.{} = a {name} b.c{j}.{};\n  ",
                      fields[i], fields[i]
                    );
                  }
                }
              }
              (Some((ac, ar)), None, None) => {
                // matrix * vector → vector
                // A is matACxAR, v is vecAC (column vec), result is vecAR
                let ret_size = extract_vec_size(return_type).unwrap();
                let b_size = extract_vec_size(b_type).unwrap();
                assert_eq!(ret_size, ar);
                assert_eq!(b_size, ac);
                for i in 0..ar {
                  let terms: Vec<String> = (0..ac)
                    .map(|k| format!("a.c{k}.{} * b.{}", fields[i], fields[k]))
                    .collect();
                  body +=
                    &format!("y.{} = {};\n  ", fields[i], terms.join(" + "));
                }
              }
              (None, Some((bc, br)), None) => {
                // vector * matrix → vector
                // v is vecBR (row vec), B is matBCxBR, result is vecBC
                let ret_size = extract_vec_size(return_type).unwrap();
                let a_size = extract_vec_size(a_type).unwrap();
                assert_eq!(ret_size, bc);
                assert_eq!(a_size, br);
                for j in 0..bc {
                  let terms: Vec<String> = (0..br)
                    .map(|k| format!("a.{} * b.c{j}.{}", fields[k], fields[k]))
                    .collect();
                  body +=
                    &format!("y.{} = {};\n  ", fields[j], terms.join(" + "));
                }
              }
              _ => panic!(
                "unrecognized matrix arithmetic signature: \
                 {a_type} {name} {b_type} -> {return_type}"
              ),
            }
            body += "return y;\n}";
            body
          } else {
            let a_vec_size = extract_vec_size(a_type);
            let b_vec_size = extract_vec_size(b_type);
            let mut function = format!(
              "{return_type} {new_name}({a_type} a, {b_type} b) {{\n  \
                 {return_type} y;\n  "
            );
            let combiner = |a_component: &str, b_component: &str| -> String {
              if name == "%"
                && (return_type == "float"
                  || return_type.char_indices().last().unwrap().1 == 'f')
              {
                format!("fmod({a_component}, {b_component})")
              } else {
                format!("{a_component} {name} {b_component}")
              }
            };
            if let Some(return_vec_size) = extract_vec_size(return_type) {
              for field in fields.iter().take(return_vec_size) {
                let a_component = if a_vec_size.is_some() {
                  format!("a.{field}")
                } else {
                  "a".to_string()
                };
                let b_component = if b_vec_size.is_some() {
                  format!("b.{field}")
                } else {
                  "b".to_string()
                };
                function += &format!("y.{field} = ");
                function += &combiner(&a_component, &b_component);
                function += ";\n  ";
              }
            } else if name == "%" {
              function += "y = ";
              function += &combiner("a", "b");
              function += ";\n  ";
            } else {
              panic!("unrecognized arithmetic signature")
            };
            function += "return y;\n}";
            function
          };
          self.helper_chunks.push(function);
          new_name.to_string()
        }
        "transpose" => {
          let mat_type = &signature.arg_types[0];
          let ret_type = &signature.return_type;
          let new_name = names.gensym("transpose");
          // matNxM: N (cols) at index 3, M (rows) at index 5
          let n: usize = mat_type[3..4].parse().unwrap();
          let m: usize = mat_type[5..6].parse().unwrap();
          let fields = ["x", "y", "z", "w"];
          // Transpose matNxM → matMxN:
          // output column j (0..M) has elements from row j of each input column
          let elements: Vec<String> = (0..m)
            .flat_map(|j| (0..n).map(move |i| format!("m.c{i}.{}", fields[j])))
            .collect();
          self.helper_chunks.push(format!(
            "{ret_type} {new_name}({mat_type} m) {{\n  \
               return ({ret_type}){{{elements}}};\n\
             }}",
            elements = elements.join(", "),
          ));
          new_name.to_string()
        }
        _ => panic!(
          "couldn't generate an emulation\ntarget: {target:?}\nsignature: {signature:#?}"
        ),
      }
    };
    self.emulated_signatures.insert(signature, new_name.clone());
    new_name
  }
}
