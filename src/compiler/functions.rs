use crate::compiler::util::compile_word;

use super::{
  error::CompileError,
  expression::{ExpKind, ExpressionCompilationContext, TypedExp},
  metadata::Metadata,
  types::{TyntType, TypeState},
  util::indent,
};

#[derive(Debug)]
pub struct TopLevelFunction {
  pub name: String,
  pub arg_metadata: Vec<Option<Metadata>>,
  pub return_metadata: Option<Metadata>,
  pub metadata: Option<Metadata>,
  pub body: TypedExp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
  pub arg_types: Vec<TyntType>,
  pub return_type: TyntType,
}

pub struct BuiltInFunction {
  pub name: String,
  pub signature: FunctionSignature,
}

impl TopLevelFunction {
  pub fn compile(self) -> Result<String, CompileError> {
    let TypedExp { data, kind } = self.body;
    let (arg_types, return_type) =
      if let TypeState::Known(TyntType::Function(signature)) = data {
        (signature.arg_types, signature.return_type)
      } else {
        panic!("attempted to compile function with invalid type data")
      };
    let (arg_names, body) = if let ExpKind::Function(arg_names, body) = kind {
      (arg_names, *body)
    } else {
      panic!("attempted to compile function with invalid ExpKind")
    };
    let name = compile_word(self.name);
    let args = arg_names
      .into_iter()
      .zip(arg_types.into_iter())
      .zip(self.arg_metadata.into_iter())
      .map(|((name, arg_type), metadata)| {
        format!(
          "{}{}: {}",
          Metadata::compile_optional(metadata),
          compile_word(name),
          arg_type.compile()
        )
      })
      .collect::<Vec<String>>()
      .join(", ");

    Ok(format!(
      "fn {name}({args}) -> {}{} {{{}\n}}",
      Metadata::compile_optional(self.return_metadata),
      return_type.compile(),
      indent(body.compile(ExpressionCompilationContext::Return))
    ))
  }
}
