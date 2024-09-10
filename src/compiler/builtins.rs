use super::{functions::FunctionSignature, types::TyntType};

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

pub fn built_in_structs() -> Vec<String> {
  vec![
    "vec2f".to_string(),
    "vec3f".to_string(),
    "vec4f".to_string(),
  ]
}

pub fn built_in_multi_signature_functions(
) -> Vec<(&'static str, Vec<FunctionSignature>)> {
  vec![("vec4f", {
    n_sums(4)
      .into_iter()
      .map(|nums| FunctionSignature {
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
        return_type: TyntType::Struct("vec4f".to_string()),
      })
      .collect()
  })]
}

pub fn built_in_functions() -> Vec<(&'static str, FunctionSignature)> {
  /*vec![(
    "vec4f",
    FunctionSignature {
      arg_types: vec![
        TyntType::F32,
        TyntType::F32,
        TyntType::F32,
        TyntType::F32,
      ],
      return_type: TyntType::Struct("vec4f".to_string()),
    },
  )]*/
  vec![]
}
