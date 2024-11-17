use std::{cell::RefCell, rc::Rc};

use sse::syntax::EncloserOrOperator;

use crate::{
  compiler::{
    error::{err, CompileError},
    expression::arg_list_and_return_type_from_tynt_tree,
    functions::AbstractFunctionSignature,
    metadata::extract_metadata,
    structs::UntypedStruct,
    types::{parse_generic_argument, AbstractType, TypeConstraint, TypeState},
    util::read_type_annotated_name,
  },
  parse::TyntTree,
};

use super::{
  builtins::{built_in_structs, built_in_type_aliases},
  error::{CompileErrorKind::*, CompileResult},
  expression::TypedExp,
  functions::{FunctionImplementationKind, TopLevelFunction},
  types::Context,
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
      if let TyntTree::Inner((position, Encloser(Parens)), children) =
        &tree_body
      {
        let mut children_iter = children.into_iter();
        if let Some(TyntTree::Leaf(position, first_child)) =
          children_iter.next()
        {
          if first_child == "struct" {
            if let Some(struct_name) = children_iter.next() {
              match struct_name {
                TyntTree::Leaf(_, struct_name) => {
                  untyped_structs.push(UntypedStruct::from_field_trees(
                    struct_name.clone(),
                    vec![],
                    children_iter.cloned().collect(),
                    vec![position.path.clone()],
                  )?)
                }
                TyntTree::Inner(
                  (position, Encloser(Parens)),
                  signature_children,
                ) => {
                  let mut signature_leaves = signature_children
                    .into_iter()
                    .map(|child| match child {
                      TyntTree::Leaf(_, name) => Ok(name.clone()),
                      _ => err(InvalidStructName, vec![]),
                    })
                    .collect::<CompileResult<Vec<String>>>()?
                    .into_iter();
                  if let Some(struct_name) = signature_leaves.next() {
                    if signature_leaves.is_empty() {
                      return err(
                        InvalidStructName,
                        vec![position.path.clone()],
                      );
                    } else {
                      untyped_structs.push(UntypedStruct::from_field_trees(
                        struct_name,
                        signature_leaves.collect(),
                        children_iter.cloned().collect(),
                        vec![position.path.clone()],
                      )?)
                    }
                  } else {
                    return err(InvalidStructName, vec![position.path.clone()]);
                  }
                }
                TyntTree::Inner((position, _), _) => {
                  return err(InvalidStructName, vec![position.path.clone()])
                }
              }
            } else {
              return err(InvalidStructDefinition, vec![position.path.clone()]);
            }
          } else {
            non_struct_trees.push((metadata, tree_body))
          }
        } else {
          return err(
            UnrecognizedTopLevelForm(tree_body.clone()),
            vec![position.path.clone()],
          );
        }
      } else {
        return err(
          UnrecognizedTopLevelForm(tree_body),
          vec![tree.position().path.clone()],
        );
      }
    }
    let mut structs = built_in_structs();
    let mut struct_names: Vec<String> =
      untyped_structs.iter().map(|s| s.name.clone()).collect();
    struct_names.append(&mut structs.iter().map(|s| s.name.clone()).collect());
    // todo! in order for this to reliably work when structs contain one
    // another, I need to topologically sort untyped_structs before this loop
    while let Some(untyped_struct) = untyped_structs.pop() {
      structs
        .push(untyped_struct.assign_types(&structs, &built_in_type_aliases())?);
    }
    let mut global_context =
      Context::default_global().with_structs(structs.clone());
    let mut top_level_vars = vec![];

    for (metadata, tree) in non_struct_trees.into_iter() {
      use crate::parse::Encloser::*;
      use sse::syntax::EncloserOrOperator::*;
      if let TyntTree::Inner((parens_position, Encloser(Parens)), children) =
        tree
      {
        let mut children_iter = children.into_iter();
        let first_child = children_iter.next();
        if let Some(TyntTree::Leaf(first_child_position, first_child)) =
          first_child
        {
          match first_child.as_str() {
            "var" => {
              let (attributes, name_and_type_ast) = match children_iter.len() {
                1 => (vec![], children_iter.next().unwrap()),
                2 => {
                  let attributes_ast = children_iter.next().unwrap();
                  if let TyntTree::Inner(
                    (position, Encloser(Square)),
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
                          err(
                            InvalidTopLevelVar(
                              "Expected leaf for attribute, found inner form"
                                .to_string(),
                            ),
                            vec![position.path.clone()],
                          )
                        }
                      })
                      .collect::<CompileResult<_>>()?;
                    (attributes, children_iter.next().unwrap())
                  } else {
                    return err(
                      InvalidTopLevelVar(
                        "Expected square-bracket enclosed attributes"
                          .to_string(),
                      ),
                      vec![first_child_position.path.clone()],
                    );
                  }
                }
                _ => {
                  return err(
                    InvalidTopLevelVar(
                      "Invalid number of inner forms".to_string(),
                    ),
                    vec![first_child_position.path.clone()],
                  )
                }
              };
              let (name, type_ast) =
                read_type_annotated_name(name_and_type_ast)?;
              let type_source_path = type_ast.position().path.clone();
              top_level_vars.push(TopLevelVar {
                name,
                metadata,
                attributes,
                var_type: AbstractType::from_ast(
                  type_ast,
                  &structs,
                  &global_context.type_aliases,
                  &vec![],
                  &vec![],
                )?
                .concretize(
                  &structs,
                  &vec![],
                  vec![type_source_path],
                )?,
              })
            }
            "def" => {
              todo!()
            }
            "defn" => {
              let name_ast = children_iter.next().ok_or_else(|| {
                CompileError::new(
                  InvalidDefn("Missing Name".to_string()),
                  vec![parens_position.path.clone()],
                )
              })?;
              let (name, generic_args): (
                String,
                Vec<(String, Vec<TypeConstraint>)>,
              ) = match name_ast {
                TyntTree::Leaf(_, name) => (name, vec![]),
                TyntTree::Inner((_, Encloser(Parens)), subtrees) => {
                  let mut subtrees_iter = subtrees.into_iter();
                  if let Some(TyntTree::Leaf(_, name)) = subtrees_iter.next() {
                    (
                      name,
                      subtrees_iter
                        .map(|subtree| {
                          parse_generic_argument(
                            subtree,
                            &structs,
                            &global_context.type_aliases,
                            &vec![],
                          )
                        })
                        .collect::<CompileResult<_>>()?,
                    )
                  } else {
                    return err(
                      InvalidDefn("Invalid name".to_string()),
                      vec![first_child_position.path.clone()],
                    );
                  }
                }
                _ => {
                  return err(
                    InvalidDefn(
                      "Expected name or parens with name and generic arguments"
                        .to_string(),
                    ),
                    vec![first_child_position.path.clone()],
                  )
                }
              };
              let arg_list_ast = children_iter.next().ok_or_else(|| {
                CompileError::new(
                  InvalidDefn("Missing Argument List".to_string()),
                  vec![parens_position.path],
                )
              })?;
              let generic_arg_names: Vec<String> =
                generic_args.iter().map(|(name, _)| name.clone()).collect();
              let (
                source_path,
                arg_names,
                arg_types,
                arg_metadata,
                return_type,
                return_metadata,
              ) = arg_list_and_return_type_from_tynt_tree(
                arg_list_ast,
                &structs,
                &global_context.type_aliases,
                &generic_arg_names,
              )?;
              let implementation = FunctionImplementationKind::Composite(
                Rc::new(RefCell::new(TopLevelFunction {
                  arg_metadata,
                  return_metadata,
                  metadata,
                  body: TypedExp::function_from_body_tree(
                    source_path.clone(),
                    children_iter.collect(),
                    TypeState::Known(return_type.concretize(
                      &structs,
                      &generic_arg_names,
                      vec![source_path.clone()],
                    )?),
                    arg_names,
                    arg_types
                      .iter()
                      .map(|t| {
                        Ok(TypeState::Known(t.concretize(
                          &structs,
                          &generic_arg_names,
                          vec![source_path.clone()],
                        )?))
                      })
                      .collect::<CompileResult<Vec<TypeState>>>()?,
                    &structs,
                    &global_context.type_aliases,
                    &generic_arg_names,
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
              return err(
                UnrecognizedTopLevelForm(TyntTree::Leaf(
                  first_child_position.clone(),
                  first_child,
                )),
                vec![first_child_position.path],
              );
            }
          }
        } else {
          return err(
            UnrecognizedTopLevelForm(first_child.unwrap_or(TyntTree::Inner(
              (
                parens_position.clone(),
                EncloserOrOperator::Encloser(Parens),
              ),
              vec![],
            ))),
            vec![parens_position.path],
          );
        }
      } else {
        return err(
          UnrecognizedTopLevelForm(tree.clone()),
          vec![tree.position().path.clone()],
        );
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
          let source_paths = untyped_expressions
            .iter()
            .map(|exp| exp.source_paths.clone())
            .flatten()
            .collect();
          err(CouldntInferTypes(untyped_expressions), source_paths)
        };
      }
    }
  }
  pub fn check_assignment_validity(self) -> CompileResult<Self> {
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
  pub fn monomorphize(mut self) -> CompileResult<Self> {
    let mut monomorphized_ctx = Context::default_global();
    for f in self.global_context.abstract_functions.iter() {
      if f.generic_args.is_empty() {
        if let FunctionImplementationKind::Composite(implementation) =
          &f.implementation
        {
          implementation
            .borrow_mut()
            .body
            .monomorphize(&self.global_context, &mut monomorphized_ctx)?;
          let mut new_f = f.clone();
          new_f.implementation =
            FunctionImplementationKind::Composite(implementation.clone());
          monomorphized_ctx.add_abstract_function(new_f);
        }
      }
    }
    monomorphized_ctx.variables = self.global_context.variables;
    monomorphized_ctx.type_aliases = self.global_context.type_aliases;
    self.global_context = monomorphized_ctx;
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
