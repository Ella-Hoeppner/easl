use crate::{
  compiler::{
    metadata::extract_metadata, structs::UntypedStruct,
    util::read_type_annotated_name,
  },
  parse::TyntTree,
};

use super::{
  builtins::built_in_structs,
  error::CompileError,
  expression::Exp,
  functions::TopLevelFunction,
  metadata::Metadata,
  structs::Struct,
  types::{Context, TyntType},
  vars::TopLevelVar,
};

#[derive(Debug)]
pub struct Program {
  structs: Vec<Struct>,
  top_level_vars: Vec<TopLevelVar>,
  top_level_functions: Vec<TopLevelFunction>,
}

impl Program {
  pub fn init_from_tynt_trees(
    trees: Vec<TyntTree>,
  ) -> Result<Self, CompileError> {
    let mut non_struct_trees = vec![];
    let mut untyped_structs = vec![];
    for tree in trees.into_iter() {
      use crate::parse::Encloser::*;
      use sse::syntax::EncloserOrOperator::*;
      let (metadata, tree_body) = extract_metadata(tree.clone())?;
      if let TyntTree::Inner((_, Encloser(Parens)), children) = &tree_body {
        let mut children_iter = children.into_iter();
        if let Some(TyntTree::Leaf(_, first_child)) = children_iter.next() {
          if first_child == "struct" {
            if let Some(TyntTree::Leaf(_, struct_name)) = children_iter.next() {
              untyped_structs.push(UntypedStruct::from_field_trees(
                struct_name.clone(),
                children_iter.cloned().collect(),
              )?);
            } else {
              return Err(CompileError::InvalidStructName);
            }
          } else {
            non_struct_trees.push((metadata, tree_body))
          }
        } else {
          return Err(CompileError::UnrecognizedTopLevelForm);
        }
      } else {
        return Err(CompileError::UnrecognizedTopLevelForm);
      }
    }
    let mut struct_names: Vec<String> = built_in_structs();
    struct_names
      .append(&mut untyped_structs.iter().map(|s| s.name.clone()).collect());
    let structs = untyped_structs
      .into_iter()
      .map(|untyped_struct| untyped_struct.assign_types(&struct_names))
      .collect::<Result<Vec<Struct>, CompileError>>()?;
    let mut top_level_vars = vec![];
    let mut top_level_functions = vec![];
    for (metadata, tree) in non_struct_trees.into_iter() {
      use crate::parse::Encloser::*;
      use crate::parse::Operator::*;
      use sse::syntax::EncloserOrOperator::*;
      if let TyntTree::Inner((_, Encloser(Parens)), children) = tree {
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
                    if let Some(TyntTree::Leaf(fn_symbol_range, first_leaf)) =
                      value_children_iter.next()
                    {
                      if first_leaf == "fn".to_string() {
                        let signature_ast = value_children_iter.next().ok_or(
                          CompileError::FunctionSignatureMissingArgumentList,
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
                              vec![
                                TyntTree::Leaf(
                                  fn_symbol_range,
                                  "fn".to_string(),
                                ),
                                TyntTree::Inner(
                                  (signature_range, Operator(TypeAnnotation)),
                                  vec![
                                    TyntTree::Inner(
                                      (arg_list_range, Encloser(Square)),
                                      arg_asts,
                                    ),
                                    return_type_ast,
                                  ],
                                ),
                              ]
                              .into_iter()
                              .chain(value_children_iter)
                              .collect(),
                            );
                            top_level_functions.push(TopLevelFunction {
                              name,
                              arg_metadata,
                              return_metadata,
                              metadata,
                              body: Exp::try_from_tynt_tree(
                                metadata_stripped_fn_ast,
                                &struct_names,
                              )?,
                            })
                          } else {
                            return Err(
                              CompileError::InvalidFunctionArgumentList,
                            );
                          }
                        } else {
                          return Err(CompileError::InvalidFunctionSignature);
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
            _ => {
              panic!("here!!");
              return Err(CompileError::UnrecognizedTopLevelForm);
            }
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
  fn propagate_types(&mut self) -> Result<bool, CompileError> {
    let mut base_context =
      Context::default_global().with_struct_constructors(&self.structs);
    self.top_level_functions.iter_mut().try_fold(
      false,
      |did_type_states_change_so_far, f| {
        let did_f_type_states_change =
          f.body.propagate_types(&mut base_context)?;
        Ok(did_type_states_change_so_far || did_f_type_states_change)
      },
    )
  }
  fn is_fully_typed(&self) -> bool {
    self
      .top_level_functions
      .iter()
      .fold(true, |fully_typed_so_far, f| {
        fully_typed_so_far && f.body.is_fully_typed()
      })
  }
  pub fn fully_infer_types(mut self) -> Result<Self, CompileError> {
    loop {
      let did_type_states_change = self.propagate_types()?;
      if !did_type_states_change {
        return if self.is_fully_typed() {
          Ok(self)
        } else {
          Err(CompileError::CouldntInferTypes)
        };
      }
    }
  }
  pub fn compile_to_wgsl(self) -> Result<String, CompileError> {
    let mut wgsl = String::new();
    for v in self.top_level_vars {
      wgsl += &v.compile();
      wgsl += "\n";
    }
    wgsl += "\n";
    for s in self.structs {
      wgsl += &s.compile();
      wgsl += "\n\n";
    }
    for f in self.top_level_functions {
      wgsl += &f.compile()?;
      wgsl += "\n\n";
    }
    Ok(wgsl)
  }
}
