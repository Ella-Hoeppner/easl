use sse::syntax::EncloserOrOperator;

use crate::{
  compiler::ir::structs::UntypedStruct,
  parse::{Operator, TyntTree},
};

use super::{
  expression::Exp,
  metadata::Metadata,
  structs::Struct,
  types::{CompileError, TyntType},
};

struct TopLevelVar {
  name: String,
  metadata: Option<Metadata>,
  attributes: Vec<String>,
  var_type: TyntType,
}

struct TopLevelFunction {
  name: String,
  metadata: Option<Metadata>,
  arg_metadata: Vec<Option<Metadata>>,
  return_metadata: Option<Metadata>,
  exp: Exp<Option<TyntType>>,
}

struct Program {
  structs: Vec<Struct>,
  top_level_vars: Vec<TopLevelVar>,
  top_level_functions: Vec<TopLevelFunction>,
}

pub fn read_type_annotated_name(
  exp: TyntTree,
) -> Result<(String, String), CompileError> {
  if let TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut children,
  ) = exp
  {
    if let TyntTree::Leaf(_, type_name) = children.remove(1) {
      if let TyntTree::Leaf(_, name) = children.remove(0) {
        Ok((name, type_name))
      } else {
        Err(CompileError::ExpectedTypeAnnotatedName)
      }
    } else {
      Err(CompileError::ExpectedTypeAnnotatedName)
    }
  } else {
    Err(CompileError::ExpectedTypeAnnotatedName)
  }
}

fn extract_metadata(
  exp: TyntTree,
) -> Result<(Option<Metadata>, TyntTree), CompileError> {
  Ok(
    if let TyntTree::Inner(
      (_, EncloserOrOperator::Operator(Operator::MetadataAnnotation)),
      mut children,
    ) = exp
    {
      let exp = children.remove(1);
      (Some(Metadata::from_metadata_tree(children.remove(0))?), exp)
    } else {
      (None, exp)
    },
  )
}

pub fn extract_type(
  exp: TyntTree,
  struct_names: &Vec<String>,
) -> Result<(Option<TyntType>, TyntTree), CompileError> {
  Ok(
    if let TyntTree::Inner(
      (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
      mut children,
    ) = exp
    {
      let t = TyntType::from_tynt_tree(children.remove(1), struct_names)?;
      (Some(t), children.remove(0))
    } else {
      (None, exp)
    },
  )
}

impl Program {
  fn init_from_tynt_trees(trees: Vec<TyntTree>) -> Result<Self, CompileError> {
    let mut non_struct_trees = vec![];
    let mut untyped_structs = vec![];
    for tree in trees.into_iter() {
      use crate::parse::Encloser::*;
      use crate::parse::Operator::*;
      use sse::syntax::EncloserOrOperator::*;
      let (metadata, tree_body) = extract_metadata(tree.clone())?;
      if let TyntTree::Inner((_, Encloser(Parens)), children) = tree_body {
        let mut children_iter = children.into_iter();
        if let Some(TyntTree::Leaf(_, first_child)) = children_iter.next() {
          if &first_child == "struct" {
            if let Some(TyntTree::Leaf(_, struct_name)) = children_iter.next() {
              untyped_structs.push(UntypedStruct::from_field_trees(
                struct_name,
                children_iter.collect(),
              )?);
            } else {
              return Err(CompileError::InvalidStructName);
            }
          } else {
            non_struct_trees.push(tree)
          }
        } else {
          return Err(CompileError::UnrecognizedTopLevelForm);
        }
      } else {
        return Err(CompileError::UnrecognizedTopLevelForm);
      }
    }
    let struct_names: Vec<String> =
      untyped_structs.iter().map(|s| s.name.clone()).collect();
    let structs = untyped_structs
      .into_iter()
      .map(|untyped_struct| untyped_struct.assign_types(&struct_names))
      .collect::<Result<Vec<Struct>, CompileError>>()?;
    let mut top_level_vars = vec![];
    let mut top_level_functions = vec![];
    for tree in non_struct_trees.into_iter() {
      use crate::parse::Encloser::*;
      use crate::parse::Operator::*;
      use sse::syntax::EncloserOrOperator::*;
      let (metadata, tree_body) = extract_metadata(tree.clone())?;
      if let TyntTree::Inner((_, Encloser(Parens)), children) = tree_body {
        let mut children_iter = children.into_iter();
        if let Some(TyntTree::Leaf(_, first_child)) = children_iter.next() {
          match first_child.as_str() {
            "var" => {
              let (attributes, name_and_type_ast) = match children_iter.len() {
                1 => (vec![], children_iter.next().unwrap()),
                2 => {
                  let attributes_ast = children_iter.next().unwrap();
                  if let TyntTree::Inner(
                    (_, Encloser(Square)),
                    attribute_asts,
                  ) = attributes_ast
                  {
                    let attributes: Vec<String> = attribute_asts
                      .into_iter()
                      .map(|attribute_ast| {
                        if let TyntTree::Leaf(_, attribute_string) =
                          attribute_ast
                        {
                          Ok(attribute_string)
                        } else {
                          Err(CompileError::InvalidTopLevelVar(
                            "Expected leaf for attribute, found inner form"
                              .to_string(),
                          ))
                        }
                      })
                      .collect::<Result<_, CompileError>>()?;
                    (attributes, children_iter.next().unwrap())
                  } else {
                    return Err(CompileError::InvalidTopLevelVar(
                      "Expected square-bracket enclosed attributes".to_string(),
                    ));
                  }
                }
                _ => {
                  return Err(CompileError::InvalidTopLevelVar(
                    "Invalid number of inner forms".to_string(),
                  ))
                }
              };
              let (name, type_name) =
                read_type_annotated_name(name_and_type_ast)?;
              top_level_vars.push(TopLevelVar {
                name,
                metadata,
                attributes,
                var_type: TyntType::from_name(type_name, &struct_names)?,
              })
            }
            "def" => {
              let name_ast = children_iter
                .next()
                .ok_or(CompileError::InvalidDef("Missing Name".to_string()))?;
              if let TyntTree::Leaf(_, name) = name_ast {
                let value_ast = children_iter.next().ok_or(
                  CompileError::InvalidDef("Missing Value".to_string()),
                )?;
                match value_ast {
                  TyntTree::Inner(
                    (fn_range, Encloser(Parens)),
                    value_children,
                  ) => {
                    let mut value_children_iter = value_children.into_iter();
                    if let Some(TyntTree::Leaf(_, first_leaf)) =
                      value_children_iter.next()
                    {
                      if first_leaf == "fn".to_string() {
                        let signature_ast = value_children_iter.next().ok_or(
                          CompileError::InvalidFunction(
                            "Missing Argument List".to_string(),
                          ),
                        )?;
                        if let TyntTree::Inner(
                          (signature_range, Operator(TypeAnnotation)),
                          args_and_return_type,
                        ) = signature_ast
                        {
                          let (return_metadata, return_type_ast) =
                            extract_metadata(args_and_return_type[1].clone())?;
                          let arg_list = args_and_return_type[0].clone();
                          if let TyntTree::Inner(
                            (arg_list_range, Encloser(Square)),
                            arg_children,
                          ) = arg_list
                          {
                            let (arg_metadata, arg_asts): (
                              Vec<Option<Metadata>>,
                              Vec<TyntTree>,
                            ) = arg_children
                              .into_iter()
                              .map(|arg| extract_metadata(arg))
                              .collect::<Result<_, CompileError>>()?;
                            let metadata_stripped_fn_ast = TyntTree::Inner(
                              (fn_range, Encloser(Parens)),
                              std::iter::once(TyntTree::Inner(
                                (signature_range, Operator(TypeAnnotation)),
                                vec![
                                  TyntTree::Inner(
                                    (arg_list_range, Encloser(Square)),
                                    arg_asts,
                                  ),
                                  return_type_ast,
                                ],
                              ))
                              .chain(value_children_iter)
                              .collect(),
                            );
                            top_level_functions.push(TopLevelFunction {
                              name,
                              metadata,
                              arg_metadata,
                              return_metadata,
                              exp: Exp::try_from_tynt_tree(
                                metadata_stripped_fn_ast,
                                &struct_names,
                              )?,
                            })
                          } else {
                            return Err(CompileError::InvalidFunction(
                              "Invalid Argument List".to_string(),
                            ));
                          }
                        } else {
                          return Err(CompileError::InvalidFunction(
                            "Invalid Signature".to_string(),
                          ));
                        }
                      } else {
                        panic!("only fns are supported in def for now!!")
                      }
                    } else {
                      panic!("only fns are supported in def for now!!")
                    }
                  }
                  _ => panic!("only fns are supported in def for now!!"),
                }
              } else {
                return Err(CompileError::InvalidDef(
                  "Invalid Name".to_string(),
                ));
              }
            }
            _ => return Err(CompileError::UnrecognizedTopLevelForm),
          }
        } else {
          return Err(CompileError::UnrecognizedTopLevelForm);
        }
      } else {
        return Err(CompileError::UnrecognizedTopLevelForm);
      }
    }
    Ok(Self {
      structs,
      top_level_vars,
      top_level_functions,
    })
  }
}
