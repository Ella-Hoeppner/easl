use sse::{syntax::EncloserOrOperator, RawSexp};

use super::{
  error::CompileError,
  types::compile_form_possibly_with_type_annotation,
  word::{compile_word, tynt_word_to_wgsl_word},
};
use crate::parse::{Encloser, TyntTree};
use core::fmt::Debug;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ExpressionContext {
  return_value: bool,
  top_level: bool,
}
impl ExpressionContext {
  fn not_top_level(mut self) -> Self {
    self.top_level = false;
    self
  }
}

fn compile_let(
  binding_expression: TyntTree,
  body_expressions: Vec<TyntTree>,
  context: ExpressionContext,
) -> Result<String, CompileError> {
  if let TyntTree::Inner(
    (_, EncloserOrOperator::Encloser(Encloser::Square)),
    bindings,
  ) = binding_expression
  {
    if bindings.len() % 2 == 1 {
      return Err(CompileError::InvalidLetBindings);
    }
    let mut bindings_iter = bindings.into_iter();
    let mut lines = String::new();
    while let Some(binding_name) = bindings_iter.next() {
      let binding_value = bindings_iter.next().unwrap();
      if !lines.is_empty() {
        lines += "\n";
      }
      lines += "let ";
      lines += &compile_form_possibly_with_type_annotation(
        binding_name,
        compile_word,
      )?;
      lines += " = ";
      lines += &compile_expression(binding_value, context)?;
      lines += ";";
    }
    let mut body_expression_iter = body_expressions.into_iter().peekable();
    while let Some(expression) = body_expression_iter.next() {
      lines += "\n";
      lines += if context.return_value && body_expression_iter.peek().is_none()
      {
        "return "
      } else {
        ""
      };
      lines += &compile_expression(expression, context)?;
      lines += ";";
    }
    Ok(lines)
  } else {
    Err(CompileError::InvalidLetBindings)
  }
}

fn compile_expression(
  expression: TyntTree,
  context: ExpressionContext,
) -> Result<String, CompileError> {
  match expression {
    TyntTree::Leaf(_, name) => Ok(tynt_word_to_wgsl_word(name)),
    TyntTree::Inner(
      (_, EncloserOrOperator::Encloser(Encloser::Parens)),
      subexpressions,
    ) => {
      if subexpressions.is_empty() {
        return Err(CompileError::InvalidExpression);
      }
      let mut subexpressions_iter = subexpressions.into_iter();
      let first_expression = subexpressions_iter.next().unwrap();
      if let TyntTree::Leaf(_, form_name) = first_expression {
        match form_name.as_str() {
          "let" => subexpressions_iter.next().map_or(
            Err(CompileError::InvalidLetBlock),
            |binding_expression| {
              compile_let(
                binding_expression,
                subexpressions_iter.collect(),
                context.not_top_level(),
              )
            },
          ),
          _ => Ok(
            String::new()
              + if context.return_value && context.top_level {
                "return "
              } else {
                ""
              }
              + &tynt_word_to_wgsl_word(form_name)
              + "("
              + &subexpressions_iter
                .map(|subexpression| {
                  compile_expression(subexpression, context.not_top_level())
                })
                .collect::<Result<Vec<String>, CompileError>>()?
                .join(", ")
              + ")"
              + if context.top_level { ";" } else { "" },
          ),
        }
      } else {
        Err(CompileError::InvalidExpression)
      }
    }
    _ => Ok(RawSexp::from(expression).to_string()),
  }
}

pub fn compile_body_expression(
  expression: TyntTree,
  return_value: bool,
) -> Result<String, CompileError> {
  compile_expression(
    expression,
    ExpressionContext {
      return_value,
      top_level: true,
    },
  )
}
