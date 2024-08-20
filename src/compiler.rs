use crate::parse::{parse_tynt, Encloser, Operator, TyntTree};
use sse::{
  document::Document, syntax::EncloserOrOperator, ParseError, RawSexp,
};

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

pub fn compile_word(word: String) -> String {
  word.replace("-", "_")
}

pub fn attempt_compile_word(
  expression: TyntTree,
) -> Result<String, CompileError> {
  match expression {
    TyntTree::Leaf(_, s) => Ok(compile_word(s)),
    _ => Err(CompileError::ExpectedWord),
  }
}

pub fn compile_name(expression: TyntTree) -> Result<String, CompileError> {
  match expression {
    TyntTree::Leaf(_, label) => Ok(label.to_string()),
    TyntTree::Inner(
      (_, EncloserOrOperator::Operator(Operator::Metadata)),
      mut children,
    ) => {
      let name_expression = children.remove(1);
      let metadata_expression = children.remove(0);
      Ok(
        compile_metadata_expression(metadata_expression)?
          + &attempt_compile_word(name_expression)?,
      )
    }
    _ => Err(CompileError::InvalidName),
  }
}

pub fn compile_metadata_expression(
  expression: TyntTree,
) -> Result<String, CompileError> {
  match expression {
    TyntTree::Leaf(_, label) => {
      Ok("@".to_string() + &compile_word(label) + "\n")
    }
    TyntTree::Inner((_, encloser_or_operator), mut children) => {
      match encloser_or_operator {
        EncloserOrOperator::Encloser(Encloser::Curly) => {
          if children.len() % 2 == 0 {
            Ok(
              children
                .chunks(2)
                .map(|x| -> Result<String, CompileError> {
                  let property = x[0].clone();
                  let value = x[1].clone();
                  let property_string = attempt_compile_word(property)?;
                  let value_string = attempt_compile_word(value)?;
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
            let metadata = compile_metadata_expression(children.remove(0))?;
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

pub fn compile_type(type_expression: TyntTree) -> Result<String, CompileError> {
  attempt_compile_word(type_expression)
}

pub fn compile_type_annotation(
  name_expression: TyntTree,
  type_expression: TyntTree,
) -> Result<String, CompileError> {
  Ok(compile_name(name_expression)? + ": " + &compile_type(type_expression)?)
}

pub fn compile_struct_body(
  expressions: Vec<TyntTree>,
) -> Result<String, CompileError> {
  let mut expression_iter = expressions.into_iter();
  if let Some(TyntTree::Leaf(_, name)) = expression_iter.next() {
    let mut struct_string =
      "struct ".to_string() + &compile_word(name) + " {\n";
    while let Some(field) = expression_iter.next() {
      if let TyntTree::Inner(
        (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
        mut children,
      ) = field
      {
        let type_expression = children.remove(1);
        let name_expression = children.remove(0);
        struct_string += "  ";
        struct_string +=
          &compile_type_annotation(name_expression, type_expression)?
            .to_string();
        struct_string += ",\n";
      } else {
        return Err(CompileError::InvalidStructField);
      }
    }
    struct_string += "}";
    Ok(struct_string)
  } else {
    Err(CompileError::InvalidStructName)
  }
}

pub fn try_compile_type_annotated_name(
  expression: TyntTree,
) -> Result<String, CompileError> {
  if let TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut children,
  ) = expression
  {
    let type_expression = children.remove(1);
    let name_expression = children.remove(0);
    compile_type_annotation(name_expression, type_expression)
  } else {
    Err(CompileError::ExpectedTypeAnnotatedName)
  }
}

pub fn compile_top_level_var_body(
  mut expressions: Vec<TyntTree>,
) -> Result<String, CompileError> {
  match expressions.len() {
    1 => try_compile_type_annotated_name(expressions.remove(0))
      .map(|s| "var ".to_string() + &s + ";"),
    2 => {
      let name = try_compile_type_annotated_name(expressions.remove(1))?;
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
          .map(|attribute| attempt_compile_word(attribute))
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
              "var" => compile_top_level_var_body(children_iter.collect()),
              "struct" => compile_struct_body(children_iter.collect()),
              "fn" => Ok(
                // todo!()
                "fn   ".to_string()
                  + &RawSexp::from(original_expression).to_string(),
              ),
              _ => Ok(RawSexp::from(original_expression).to_string()),
            }
          } else {
            Err(CompileError::UnrecognizedTopLevelExpression)
          }
        }
        EncloserOrOperator::Operator(Operator::Metadata) => {
          let body = compile_top_level_tynt_expression(children.remove(1))?;
          let metadata = compile_metadata_expression(children.remove(0))?;
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
