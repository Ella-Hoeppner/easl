use std::{cell::RefCell, rc::Rc};

use crate::{
  compiler::{
    error::err, expression::arg_list_and_return_type_from_tynt_tree,
    functions::AbstractFunctionSignature, metadata::extract_metadata,
    structs::UntypedStruct, types::TypeState, util::read_type_annotated_name,
  },
  parse::TyntTree,
};

use super::{
  builtins::built_in_structs,
  error::{CompileErrorKind::*, CompileResult},
  expression::{Exp, TypedExp},
  functions::{FunctionImplementationKind, TopLevelFunction},
  metadata::Metadata,
  types::{Context, Type},
  vars::TopLevelVar,
};

#[derive(Debug)]
pub struct Program {
  global_context: Context,
  top_level_vars: Vec<TopLevelVar>,
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
                TyntTree::Inner((_, Encloser(Parens)), signature_children) => {
                  let mut signature_leaves = signature_children
                    .into_iter()
                    .map(|child| match child {
                      TyntTree::Leaf(_, name) => Ok(name.clone()),
                      _ => err(InvalidStructName),
                    })
                    .collect::<CompileResult<Vec<String>>>()?
                    .into_iter();
                  if let Some(struct_name) = signature_leaves.next() {
                    if signature_leaves.is_empty() {
                      return err(InvalidStructName);
                    } else {
                      untyped_structs.push(UntypedStruct::from_field_trees(
                        struct_name,
                        signature_leaves.collect(),
                        children_iter.cloned().collect(),
                      )?)
                    }
                  } else {
                    return err(InvalidStructName);
                  }
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
    let mut global_context =
      Context::default_global().with_structs(structs.clone());
    let mut top_level_vars = vec![];
    //let mut top_level_functions = vec![];
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
                var_type: Type::from_name(type_name, &structs, &vec![])?,
              })
            }
            "def" => {
              todo!()
              /*let name_ast = children_iter
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
                            global_context.abstract_functions.push(
                              AbstractFunctionSignature {
                                name,
                                generic_args: vec![],
                                arg_types: todo!(),
                                return_type: todo!(),
                                implementation:
                                  FunctionImplementationKind::Composite(
                                    TopLevelFunction {
                                      name,
                                      arg_metadata,
                                      return_metadata,
                                      metadata,
                                      body: Exp::try_from_tynt_tree(
                                        metadata_stripped_fn_ast,
                                        &structs,
                                        &vec![],
                                      )?,
                                      generic_args: vec![],
                                    },
                                  ),
                              },
                            );
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
              }*/
            }
            "defn" => {
              let name_ast = children_iter
                .next()
                .ok_or(InvalidDefn("Missing Name".to_string()))?;
              let (name, generic_args): (String, Vec<String>) = match name_ast {
                TyntTree::Leaf(_, name) => (name, vec![]),
                TyntTree::Inner((_, Encloser(Parens)), subtrees) => {
                  let mut subtrees_iter = subtrees.into_iter();
                  if let Some(TyntTree::Leaf(_, name)) = subtrees_iter.next() {
                    (
                      name,
                      subtrees_iter
                        .map(|subtree| {
                          if let TyntTree::Leaf(_, generic_name) = subtree {
                            Ok(generic_name)
                          } else {
                            err(InvalidDefn("Invalid generic name".to_string()))
                          }
                        })
                        .collect::<CompileResult<_>>()?,
                    )
                  } else {
                    return err(InvalidDefn("Invalid name".to_string()));
                  }
                }
                _ => {
                  return err(InvalidDefn(
                    "Expected name or parens with name and generic arguments"
                      .to_string(),
                  ))
                }
              };
              let arg_list_ast = children_iter
                .next()
                .ok_or(InvalidDefn("Missing Argument List".to_string()))?;
              let (
                arg_names,
                arg_types,
                arg_metadata,
                return_type,
                return_metadata,
              ) = arg_list_and_return_type_from_tynt_tree(
                arg_list_ast,
                &structs,
                &generic_args,
              )?;
              let implementation = FunctionImplementationKind::Composite(
                Rc::new(RefCell::new(TopLevelFunction {
                  arg_metadata,
                  return_metadata,
                  metadata,
                  body: TypedExp::function_from_body_tree(
                    children_iter.collect(),
                    TypeState::Known(
                      return_type.concretize(&structs, &generic_args)?,
                    ),
                    arg_names,
                    arg_types
                      .iter()
                      .map(|t| {
                        Ok(TypeState::Known(
                          t.concretize(&structs, &generic_args)?,
                        ))
                      })
                      .collect::<CompileResult<Vec<TypeState>>>()?,
                    &structs,
                    &generic_args,
                  )?,
                })),
              );
              global_context.abstract_functions.push(
                AbstractFunctionSignature {
                  name,
                  generic_args,
                  arg_types,
                  return_type,
                  implementation,
                },
              );
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
    })
  }
  fn propagate_types(&mut self) -> CompileResult<bool> {
    let mut base_context = self.global_context.clone();
    self.global_context.abstract_functions.iter_mut().try_fold(
      false,
      |did_type_states_change_so_far, f| {
        let did_f_type_states_change =
          if let FunctionImplementationKind::Composite(implementation) =
            &mut f.implementation
          {
            implementation
              .borrow_mut()
              .body
              .propagate_types(&mut base_context)?
          } else {
            false
          };
        Ok(did_type_states_change_so_far || did_f_type_states_change)
      },
    )
  }
  fn find_untyped(&self) -> Vec<TypedExp> {
    self.global_context.abstract_functions.iter().fold(
      vec![],
      |mut untyped_so_far, f| {
        if let FunctionImplementationKind::Composite(implementation) =
          &f.implementation
        {
          untyped_so_far
            .append(&mut implementation.borrow_mut().body.find_untyped());
        }
        untyped_so_far
      },
    )
  }
  pub fn fully_infer_types(mut self) -> CompileResult<Self> {
    loop {
      let did_type_states_change = self.propagate_types()?;
      if !did_type_states_change {
        let untyped_expressions = self.find_untyped();
        return if untyped_expressions.is_empty() {
          Ok(self)
        } else {
          err(CouldntInferTypes(untyped_expressions))
        };
      }
    }
  }
  pub fn check_typed_program(self) -> CompileResult<Self> {
    for f in self.global_context.abstract_functions.iter() {
      if let FunctionImplementationKind::Composite(implementation) =
        &f.implementation
      {
        implementation
          .borrow_mut()
          .body
          .check_assignment_validity(&mut Context::default_global())?;
      }
    }
    Ok(self)
  }
  pub fn monomorphize(mut self) -> Self {
    let mut monomorphized_ctx = Context::empty();
    for f in self.global_context.abstract_functions.iter() {
      if f.generic_args.is_empty() {
        if let FunctionImplementationKind::Composite(implementation) =
          &f.implementation
        {
          implementation
            .borrow_mut()
            .body
            .monomorphize(&self.global_context, &mut monomorphized_ctx);
          let mut new_f = f.clone();
          new_f.implementation =
            FunctionImplementationKind::Composite(implementation.clone());
          monomorphized_ctx.add_abstract_function(new_f);
        }
      }
    }
    monomorphized_ctx.variables = self.global_context.variables;
    self.global_context = monomorphized_ctx;
    self
  }
  pub fn compile_to_wgsl(self) -> CompileResult<String> {
    let mut wgsl = String::new();
    for v in self.top_level_vars {
      wgsl += &v.compile();
      wgsl += "\n";
    }
    wgsl += "\n";
    let default_structs = built_in_structs();
    for s in self
      .global_context
      .structs
      .iter()
      .cloned()
      .filter(|s| !default_structs.contains(s))
    {
      if let Some(compiled_struct) =
        s.compile_if_non_generic(&self.global_context.structs)?
      {
        wgsl += &compiled_struct;
        wgsl += "\n\n";
      }
    }
    for f in self.global_context.abstract_functions {
      if let FunctionImplementationKind::Composite(implementation) =
        f.implementation
      {
        if f.generic_args.is_empty() {
          wgsl += &implementation.borrow().clone().compile(&f.name)?;
          wgsl += "\n\n";
        }
      }
    }
    Ok(wgsl)
  }
}
