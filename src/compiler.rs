use crate::parse::{parse_tynt, Encloser, Operator, TyntTree};
use sse::{syntax::EncloserOrOperator, ParseError, RawSexp};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
  Parse(ParseError),
  InvalidMetadataExpression,
  ExpectedWord,
  UnrecognizedTopLevelExpression,
  InvalidStructName,
  InvalidStructBody,
  InvalidStructField,
  InvalidName,
  InvalidTopLevelVar,
  InvalidTopLevelVarAttributes,
  ExpectedTypeAnnotatedName,
  InvalidFunctionBody,
  InvalidFunctionArgumentList,
}

impl From<ParseError> for CompileError {
  fn from(err: ParseError) -> Self {
    CompileError::Parse(err)
  }
}

#[derive(Debug, Clone)]
pub enum CompilationContext {
  Default,
}

pub fn tynt_word_to_wgsl_word(word: String) -> String {
  word.replace("-", "_")
}

pub fn compile_word(expression: TyntTree) -> Result<String, CompileError> {
  match expression {
    TyntTree::Leaf(_, s) => Ok(tynt_word_to_wgsl_word(s)),
    other => {
      println!("{other:?}");
      Err(CompileError::ExpectedWord)
    }
  }
}

pub fn compile_type(type_expression: TyntTree) -> Result<String, CompileError> {
  compile_word(type_expression)
}

pub fn compile_metadata(expression: TyntTree) -> Result<String, CompileError> {
  match expression {
    TyntTree::Leaf(_, label) => {
      Ok("@".to_string() + &tynt_word_to_wgsl_word(label) + "\n")
    }
    TyntTree::Inner((_, encloser_or_operator), children) => {
      match encloser_or_operator {
        EncloserOrOperator::Encloser(Encloser::Curly) => {
          if children.len() % 2 == 0 {
            Ok(
              children
                .chunks(2)
                .map(|x| -> Result<String, CompileError> {
                  let property = x[0].clone();
                  let value = x[1].clone();
                  let property_string = compile_word(property)?;
                  let value_string = compile_word(value)?;
                  Ok(
                    "@".to_string()
                      + &property_string
                      + "("
                      + &value_string
                      + ") ",
                  )
                })
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .reduce(|a, b| a + &b)
                .unwrap(),
            )
          } else {
            Err(CompileError::InvalidMetadataExpression)
          }
        }
        _ => Err(CompileError::InvalidMetadataExpression),
      }
    }
  }
}

pub fn compile_expression_possibly_with_metadata(
  expression: TyntTree,
  subexpression_compiler: impl Fn(TyntTree) -> Result<String, CompileError>,
) -> Result<String, CompileError> {
  if let TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::Metadata)),
    mut children,
  ) = expression
  {
    let expression = children.remove(1);
    let metadata_expression = children.remove(0);
    let expression_string = compile_expression_possibly_with_metadata(
      expression,
      subexpression_compiler,
    )?;
    Ok(compile_metadata(metadata_expression)? + &expression_string)
  } else {
    subexpression_compiler(expression)
  }
}

pub fn compile_tynt_expression(
  expression: TyntTree,
) -> Result<String, CompileError> {
  let original_expression = expression.clone();
  match expression {
    TyntTree::Leaf(_, label) => Ok(label.replace("-", "_")),
    TyntTree::Inner((_, encloser_or_operator), mut children) => {
      match encloser_or_operator {
        EncloserOrOperator::Encloser(encloser) => match encloser {
          Encloser::Parens => {
            Ok(RawSexp::from(original_expression).to_string())
          }
          Encloser::Square => {
            Ok(RawSexp::from(original_expression).to_string())
          }
          Encloser::Curly => Ok(RawSexp::from(original_expression).to_string()),
        },
        EncloserOrOperator::Operator(operator) => match operator {
          Operator::Metadata => {
            let body = compile_tynt_expression(children.remove(1))?;
            let metadata = compile_metadata(children.remove(0))?;
            Ok(metadata + &body)
          }
          Operator::TypeAnnotation => {
            Ok(RawSexp::from(original_expression).to_string())
          }
        },
      }
    }
  }
}

pub fn compile_struct(
  expressions: Vec<TyntTree>,
) -> Result<String, CompileError> {
  let mut expression_iter = expressions.into_iter();
  if let Some(TyntTree::Leaf(_, name)) = expression_iter.next() {
    let mut struct_string =
      "struct ".to_string() + &tynt_word_to_wgsl_word(name) + " {\n";
    while let Some(field) = expression_iter.next() {
      struct_string += "  ";
      struct_string += &compile_type_annotated_name(field)?;
      struct_string += ",\n";
    }
    struct_string += "}";
    Ok(struct_string)
  } else {
    Err(CompileError::InvalidStructName)
  }
}

pub fn compile_type_annotated_expression(
  expression: TyntTree,
  subexpression_compiler: impl Fn(TyntTree) -> Result<String, CompileError>,
) -> Result<String, CompileError> {
  if let TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut children,
  ) = expression
  {
    let type_expression = children.remove(1);
    let expression = children.remove(0);
    Ok(
      subexpression_compiler(expression)?
        + ": "
        + &compile_type(type_expression)?,
    )
  } else {
    Err(CompileError::ExpectedTypeAnnotatedName)
  }
}

pub fn compile_type_annotated_name(
  expression: TyntTree,
) -> Result<String, CompileError> {
  compile_type_annotated_expression(expression, |subexpression| {
    compile_expression_possibly_with_metadata(subexpression, compile_word)
  })
}

pub fn compile_top_level_var(
  mut expressions: Vec<TyntTree>,
) -> Result<String, CompileError> {
  match expressions.len() {
    1 => compile_type_annotated_name(expressions.remove(0))
      .map(|s| "var ".to_string() + &s + ";"),
    2 => {
      let name = compile_type_annotated_name(expressions.remove(1))?;
      if let TyntTree::Inner(
        (_, EncloserOrOperator::Encloser(Encloser::Square)),
        attributes,
      ) = expressions.remove(0)
      {
        if attributes.is_empty() {
          return Err(CompileError::InvalidTopLevelVarAttributes);
        }
        let attribute_strings: Vec<String> = attributes
          .into_iter()
          .map(|attribute| compile_word(attribute))
          .collect::<Result<Vec<String>, CompileError>>()?;
        Ok(
          "var<".to_string()
            + &attribute_strings
              .into_iter()
              .reduce(|a, b| a + ", " + &b)
              .unwrap()
            + "> "
            + &name
            + ";",
        )
      } else {
        Err(CompileError::InvalidTopLevelVarAttributes)
      }
    }
    _ => Err(CompileError::InvalidTopLevelVar),
  }
}

pub fn compile_function(
  expressions: Vec<TyntTree>,
) -> Result<String, CompileError> {
  if expressions.len() <= 2 {
    return Err(CompileError::InvalidFunctionBody);
  }
  let mut expressions_iter = expressions.into_iter();
  let mut fn_string = "fn ".to_string();
  fn_string += &compile_word(expressions_iter.next().unwrap())?;
  fn_string += "(";
  if let Some(TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut arg_list_and_return_type,
  )) = expressions_iter.next()
  {
    let return_type_expression = arg_list_and_return_type.remove(1);
    let arg_list_expression = arg_list_and_return_type.remove(0);
    if let TyntTree::Inner(
      (_, EncloserOrOperator::Encloser(Encloser::Square)),
      argument_expressions,
    ) = arg_list_expression
    {
      let arg_strings = argument_expressions
        .into_iter()
        .map(compile_type_annotated_name)
        .collect::<Result<Vec<String>, CompileError>>()?;
      fn_string += &arg_strings.join(", ");
      fn_string += ") -> ";
      if let TyntTree::Inner(
        (_, EncloserOrOperator::Operator(Operator::Metadata)),
        mut metadata_and_output_type,
      ) = return_type_expression
      {
        let output_type_expression = metadata_and_output_type.remove(1);
        let metadata = metadata_and_output_type.remove(0);
        fn_string += &compile_metadata(metadata)?;
        fn_string += &compile_word(output_type_expression)?;
      } else {
        fn_string += &compile_word(return_type_expression)?;
      }
      fn_string += " {\n";
      fn_string += "}";
    } else {
      return Err(CompileError::InvalidFunctionArgumentList);
    }
  } else {
    return Err(CompileError::InvalidFunctionArgumentList);
  }
  Ok(fn_string)
}

pub fn compile_top_level_tynt_expression(
  expression: TyntTree,
) -> Result<String, CompileError> {
  let original_expression = expression.clone();
  match expression {
    TyntTree::Leaf(_, label) => Ok(label.replace("-", "_")),
    TyntTree::Inner((_, encloser_or_operator), mut children) => {
      match encloser_or_operator {
        EncloserOrOperator::Encloser(Encloser::Parens) => {
          let mut children_iter = children.into_iter();
          if let Some(TyntTree::Leaf(_, first_symbol)) = children_iter.next() {
            match first_symbol.as_str() {
              "var" => compile_top_level_var(children_iter.collect()),
              "struct" => compile_struct(children_iter.collect()),
              "fn" => compile_function(children_iter.collect()),
              _ => Ok(RawSexp::from(original_expression).to_string()),
            }
          } else {
            Err(CompileError::UnrecognizedTopLevelExpression)
          }
        }
        EncloserOrOperator::Operator(Operator::Metadata) => {
          let body = compile_top_level_tynt_expression(children.remove(1))?;
          let metadata = compile_metadata(children.remove(0))?;
          Ok(metadata + &body)
        }
        _ => Err(CompileError::UnrecognizedTopLevelExpression),
      }
    }
  }
}

pub fn compile_tynt(tynt_source: &str) -> Result<String, CompileError> {
  let document = parse_tynt(tynt_source)?;
  Ok(
    document
      .syntax_trees
      .into_iter()
      .map(|tree| compile_top_level_tynt_expression(tree))
      .collect::<Result<Vec<String>, CompileError>>()?
      .into_iter()
      .fold(String::new(), |text, tree| text + "\n\n" + &tree)
      + "\n",
  )
}
