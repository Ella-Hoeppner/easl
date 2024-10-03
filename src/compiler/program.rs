use crate::{
  compiler::{
    error::err, metadata::extract_metadata, structs::UntypedStruct,
    util::read_type_annotated_name,
  },
  parse::TyntTree,
};

use super::{
  builtins::built_in_structs,
  error::{CompileErrorKind::*, CompileResult},
  expression::Exp,
  functions::TopLevelFunction,
  metadata::Metadata,
  structs::{AbstractStruct, ConcreteStruct},
  types::{Bindings, Context, TyntType},
  vars::TopLevelVar,
};

#[derive(Debug)]
pub struct Program {
  global_context: Context,
  top_level_vars: Vec<TopLevelVar>,
  top_level_functions: Vec<TopLevelFunction>,
}

impl Program {
  pub fn init_from_tynt_trees(trees: Vec<TyntTree>) -> CompileResult<Self> {
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
            if let Some(struct_name) = children_iter.next() {
              match struct_name {
                TyntTree::Leaf(_, struct_name) => {
                  untyped_structs.push(UntypedStruct::from_field_trees(
                    struct_name.clone(),
                    vec![],
                    children_iter.cloned().collect(),
                  )?)
                }
                _ => return err(InvalidStructName),
              }
            } else {
              return err(InvalidStructDefinition);
            }
          } else {
            non_struct_trees.push((metadata, tree_body))
          }
        } else {
          return err(UnrecognizedTopLevelForm);
        }
      } else {
        return err(UnrecognizedTopLevelForm);
      }
    }
    let mut structs = built_in_structs();
    let mut struct_names: Vec<String> =
      untyped_structs.iter().map(|s| s.name.clone()).collect();
    struct_names.append(&mut structs.iter().map(|s| s.name.clone()).collect());
    // todo! in order for this to reliably work when structs contain one
    // another, I need to topologically sort untyped_structs before this loop
    while let Some(untyped_struct) = untyped_structs.pop() {
      structs.push(untyped_struct.assign_types(&structs)?);
    }
    let global_context =
      Context::default_global().with_structs(structs.clone());
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
                          err(InvalidTopLevelVar(
                            "Expected leaf for attribute, found inner form"
                              .to_string(),
                          ))
                        }
                      })
                      .collect::<CompileResult<_>>()?;
                    (attributes, children_iter.next().unwrap())
                  } else {
                    return err(InvalidTopLevelVar(
                      "Expected square-bracket enclosed attributes".to_string(),
                    ));
                  }
                }
                _ => {
                  return err(InvalidTopLevelVar(
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
                var_type: TyntType::from_name(type_name, &structs)?,
              })
            }
            "def" => {
              let name_ast = children_iter
                .next()
                .ok_or(InvalidDef("Missing Name".to_string()))?;
              if let TyntTree::Leaf(_, name) = name_ast {
                let value_ast = children_iter
                  .next()
                  .ok_or(InvalidDef("Missing Value".to_string()))?;
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
                        let signature_ast = value_children_iter
                          .next()
                          .ok_or(FunctionSignatureMissingArgumentList)?;
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
                              .collect::<CompileResult<_>>()?;
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
                                &structs,
                              )?,
                            })
                          } else {
                            return err(InvalidFunctionArgumentList);
                          }
                        } else {
                          return err(InvalidFunctionSignature);
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
                return err(InvalidDef("Invalid Name".to_string()));
              }
            }
            _ => {
              return err(UnrecognizedTopLevelForm);
            }
          }
        } else {
          return err(UnrecognizedTopLevelForm);
        }
      } else {
        return err(UnrecognizedTopLevelForm);
      }
    }
    Ok(Self {
      global_context,
      top_level_vars,
      top_level_functions,
    })
  }
  fn propagate_types(&mut self) -> CompileResult<bool> {
    let mut base_context = self.global_context.clone();
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
  pub fn fully_infer_types(mut self) -> CompileResult<Self> {
    loop {
      let did_type_states_change = self.propagate_types()?;
      if !did_type_states_change {
        return if self.is_fully_typed() {
          Ok(self)
        } else {
          err(CouldntInferTypes)
        };
      }
    }
  }
  pub fn check_typed_program(self) -> CompileResult<Self> {
    for f in self.top_level_functions.iter() {
      f.body
        .check_assignment_validity(&mut Bindings::default_global())?;
    }
    Ok(self)
  }
  pub fn compile_to_wgsl(self) -> CompileResult<String> {
    let mut wgsl = String::new();
    for v in self.top_level_vars {
      wgsl += &v.compile();
      wgsl += "\n";
    }
    wgsl += "\n";
    let default_structs = built_in_structs();
    /*for s in self
      .global_context
      .structs
      .into_iter()
      .filter(|s| !default_structs.contains(s))
    {
      wgsl += &s.compile();
      wgsl += "\n\n";
    }*/
    for f in self.top_level_functions {
      wgsl += &f.compile()?;
      wgsl += "\n\n";
    }
    Ok(wgsl)
  }
}
