use std::{cell::RefCell, collections::HashMap, rc::Rc};

use sse::{document::Document, syntax::EncloserOrOperator};

use crate::{
  compiler::{
    error::{err, CompileError, SourceTrace},
    expression::{arg_list_and_return_type_from_easl_tree, Exp},
    functions::AbstractFunctionSignature,
    metadata::extract_metadata,
    structs::UntypedStruct,
    types::{
      parse_generic_argument, AbstractType, GenericOr, Type, TypeConstraint,
      TypeState, Variable, VariableKind,
    },
    util::read_type_annotated_name,
  },
  parse::{parse_easl, Context as SyntaxContext, EaslTree, Encloser, Operator},
};

use super::{
  builtins::{built_in_structs, built_in_type_aliases},
  error::{CompileErrorKind::*, CompileResult},
  expression::TypedExp,
  functions::{FunctionImplementationKind, TopLevelFunction},
  macros::{macroexpand, Macro},
  types::Context,
  vars::TopLevelVar,
};

pub type EaslDocument<'s> = Document<'s, SyntaxContext, Encloser, Operator>;

#[derive(Debug)]
pub struct Program {
  global_context: Context,
}

impl Program {
  pub fn from_easl_document(
    document: &'_ EaslDocument,
    macros: Vec<Macro>,
  ) -> CompileResult<Self> {
    let trees = document
      .syntax_trees
      .iter()
      .cloned()
      .map(|tree| macroexpand(tree, &macros))
      .collect::<Result<Vec<EaslTree>, (SourceTrace, String)>>()
      .map_err(|(source_trace, err_str)| {
        CompileError::new(MacroError(err_str), source_trace)
      })?;

    let mut non_struct_trees = vec![];
    let mut untyped_structs = vec![];

    for tree in trees.into_iter() {
      use crate::parse::Encloser::*;
      use sse::syntax::EncloserOrOperator::*;
      let (metadata, tree_body) = extract_metadata(tree.clone())?;
      if let EaslTree::Inner((position, Encloser(Parens)), children) =
        &tree_body
      {
        let source_trace: SourceTrace = position.clone().into();
        let mut children_iter = children.into_iter();
        if let Some(EaslTree::Leaf(position, first_child)) =
          children_iter.next()
        {
          let source_trace: SourceTrace = position.clone().into();
          if first_child == "struct" {
            if let Some(struct_name) = children_iter.next() {
              match struct_name {
                EaslTree::Leaf(_, struct_name) => {
                  untyped_structs.push(UntypedStruct::from_field_trees(
                    struct_name.clone(),
                    vec![],
                    children_iter.cloned().collect(),
                    source_trace,
                  )?)
                }
                EaslTree::Inner(
                  (position, Encloser(Parens)),
                  signature_children,
                ) => {
                  let source_trace: SourceTrace = position.clone().into();
                  let mut signature_leaves = signature_children
                    .into_iter()
                    .map(|child| match child {
                      EaslTree::Leaf(_, name) => Ok(name.clone()),
                      _ => {
                        err(InvalidStructName, child.position().clone().into())
                      }
                    })
                    .collect::<CompileResult<Vec<String>>>()?
                    .into_iter();
                  if let Some(struct_name) = signature_leaves.next() {
                    if signature_leaves.is_empty() {
                      return err(InvalidStructName, source_trace);
                    } else {
                      untyped_structs.push(UntypedStruct::from_field_trees(
                        struct_name,
                        signature_leaves.collect(),
                        children_iter.cloned().collect(),
                        source_trace,
                      )?)
                    }
                  } else {
                    return err(InvalidStructName, source_trace);
                  }
                }
                EaslTree::Inner((position, _), _) => {
                  return err(InvalidStructName, position.clone().into())
                }
              }
            } else {
              return err(InvalidStructDefinition, source_trace);
            }
          } else {
            non_struct_trees.push((metadata, tree_body))
          }
        } else {
          return err(
            UnrecognizedTopLevelForm(tree_body.clone()),
            source_trace,
          );
        }
      } else {
        return err(
          UnrecognizedTopLevelForm(tree_body),
          tree.position().clone().into(),
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

    for (metadata, tree) in non_struct_trees.into_iter() {
      use crate::parse::Encloser::*;
      use sse::syntax::EncloserOrOperator::*;
      if let EaslTree::Inner((parens_position, Encloser(Parens)), children) =
        tree
      {
        let parens_source_trace: SourceTrace = parens_position.clone().into();
        let mut children_iter = children.into_iter();
        let first_child = children_iter.next();
        if let Some(EaslTree::Leaf(first_child_position, first_child)) =
          first_child
        {
          let first_child_source_trace: SourceTrace =
            first_child_position.clone().into();
          match first_child.as_str() {
            "var" => {
              let (attributes, name_and_type_ast) = match children_iter.len() {
                1 => (vec![], children_iter.next().unwrap()),
                2 => {
                  let attributes_ast = children_iter.next().unwrap();
                  if let EaslTree::Inner(
                    (position, Encloser(Square)),
                    attribute_asts,
                  ) = attributes_ast
                  {
                    let attributes: Vec<String> = attribute_asts
                      .into_iter()
                      .map(|attribute_ast| {
                        if let EaslTree::Leaf(_, attribute_string) =
                          attribute_ast
                        {
                          Ok(attribute_string)
                        } else {
                          err(
                            InvalidTopLevelVar(
                              "Expected leaf for attribute, found inner form"
                                .to_string(),
                            ),
                            position.clone().into(),
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
                      first_child_source_trace,
                    );
                  }
                }
                _ => {
                  return err(
                    InvalidTopLevelVar(
                      "Invalid number of inner forms".to_string(),
                    ),
                    first_child_source_trace,
                  )
                }
              };
              let (name, type_ast) =
                read_type_annotated_name(name_and_type_ast)?;
              let type_source_path = type_ast.position().clone();
              global_context.top_level_vars.push(TopLevelVar {
                name,
                metadata,
                attributes,
                var: Variable::new(TypeState::Known(
                  AbstractType::from_ast(
                    type_ast,
                    &structs,
                    &global_context.type_aliases,
                    &vec![],
                    &vec![],
                  )?
                  .concretize(
                    &structs,
                    &vec![],
                    type_source_path.into(),
                  )?,
                ))
                .with_kind(VariableKind::Var),
                value: None,
                source_trace: parens_source_trace,
              })
            }
            "def" => {
              if children_iter.len() == 2 {
                let (name, type_ast) =
                  read_type_annotated_name(children_iter.next().unwrap())?;
                let value_expression = TypedExp::try_from_easl_tree(
                  children_iter.next().unwrap(),
                  &structs,
                  &vec![],
                  &vec![],
                  crate::compiler::expression::SyntaxTreeContext::Default,
                )?;
                global_context.top_level_vars.push(TopLevelVar {
                  name,
                  metadata: None,
                  attributes: vec![],
                  var: Variable::new(TypeState::Known(Type::from_easl_tree(
                    type_ast,
                    &structs,
                    &global_context.type_aliases,
                    &vec![],
                  )?)),
                  value: Some(value_expression),
                  source_trace: parens_source_trace,
                })
              } else {
                return err(
                  InvalidTopLevelVar(
                    "Expected two forms inside \"def\"".to_string(),
                  ),
                  first_child_source_trace,
                );
              }
            }
            "defn" => {
              let name_ast = children_iter.next().ok_or_else(|| {
                CompileError::new(
                  InvalidDefn("Missing Name".to_string()),
                  parens_source_trace.clone(),
                )
              })?;
              let (name, generic_args): (
                String,
                Vec<(String, Vec<TypeConstraint>)>,
              ) = match name_ast {
                EaslTree::Leaf(_, name) => (name, vec![]),
                EaslTree::Inner((_, Encloser(Parens)), subtrees) => {
                  let mut subtrees_iter = subtrees.into_iter();
                  if let Some(EaslTree::Leaf(_, name)) = subtrees_iter.next() {
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
                      first_child_source_trace,
                    );
                  }
                }
                _ => {
                  return err(
                    InvalidDefn(
                      "Expected name or parens with name and generic arguments"
                        .to_string(),
                    ),
                    first_child_source_trace,
                  )
                }
              };
              let arg_list_ast = children_iter.next().ok_or_else(|| {
                CompileError::new(
                  InvalidDefn("Missing Argument List".to_string()),
                  parens_source_trace.clone(),
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
              ) = arg_list_and_return_type_from_easl_tree(
                arg_list_ast,
                &structs,
                &global_context.type_aliases,
                &generic_arg_names,
              )?;
              let implementation =
                FunctionImplementationKind::Composite(Rc::new(RefCell::new(
                  TopLevelFunction {
                    arg_metadata,
                    return_metadata,
                    metadata,
                    body:
                      TypedExp::function_from_body_tree(
                        source_path.clone(),
                        children_iter.collect(),
                        TypeState::Known(return_type.concretize(
                          &structs,
                          &generic_arg_names,
                          source_path.clone().into(),
                        )?),
                        arg_names,
                        arg_types
                          .iter()
                          .map(|t| {
                            Ok((
                              TypeState::Known(t.concretize(
                                &structs,
                                &generic_arg_names,
                                source_path.clone().into(),
                              )?),
                              if let GenericOr::Generic(generic_name) = t {
                                generic_args
                                  .iter()
                                  .find_map(|(name, constraints)| {
                                    (generic_name == name)
                                      .then(|| constraints.clone())
                                  })
                                  .unwrap_or(vec![])
                              } else {
                                vec![]
                              },
                            ))
                          })
                          .collect::<CompileResult<
                            Vec<(TypeState, Vec<TypeConstraint>)>,
                          >>()?,
                        &structs,
                        &global_context.type_aliases,
                        &generic_arg_names,
                      )?,
                  },
                )));
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
                UnrecognizedTopLevelForm(EaslTree::Leaf(
                  first_child_position.clone(),
                  first_child,
                )),
                first_child_source_trace,
              );
            }
          }
        } else {
          return err(
            UnrecognizedTopLevelForm(first_child.unwrap_or(EaslTree::Inner(
              (
                parens_position.clone(),
                EncloserOrOperator::Encloser(Parens),
              ),
              vec![],
            ))),
            parens_source_trace,
          );
        }
      } else {
        return err(
          UnrecognizedTopLevelForm(tree.clone()),
          tree.position().clone().into(),
        );
      }
    }
    Ok(Self { global_context })
  }
  fn propagate_types(&mut self) -> CompileResult<bool> {
    let mut base_context = self.global_context.clone();
    let mut anything_changed = false;
    for var in self.global_context.top_level_vars.iter_mut() {
      if let Some(value_expression) = &mut var.value {
        anything_changed |= var.var.typestate.mutually_constrain(
          &mut value_expression.data,
          var.source_trace.clone(),
        )?;
      }
    }
    for f in self.global_context.abstract_functions.iter_mut() {
      if let FunctionImplementationKind::Composite(implementation) =
        &mut f.implementation
      {
        anything_changed |= implementation
          .borrow_mut()
          .body
          .propagate_types(&mut base_context)?;
      }
    }
    Ok(anything_changed)
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
  pub fn validate_match_blocks(mut self) -> CompileResult<Self> {
    for abstract_function in self.global_context.abstract_functions.iter_mut() {
      if let FunctionImplementationKind::Composite(implementation) =
        &mut abstract_function.implementation
      {
        (**implementation)
          .borrow_mut()
          .body
          .validate_match_blocks()?;
      }
    }
    Ok(self)
  }
  pub fn fully_infer_types(mut self) -> CompileResult<Self> {
    loop {
      let did_type_states_change = self.propagate_types()?;
      if !did_type_states_change {
        let untyped_expressions = self.find_untyped();
        return if untyped_expressions.is_empty() {
          Ok(self)
        } else {
          let source_trace = untyped_expressions
            .iter()
            .map(|exp| exp.source_trace.clone())
            .collect();
          err(CouldntInferTypes(untyped_expressions), source_trace)
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
    monomorphized_ctx.structs = self.global_context.structs;
    monomorphized_ctx.top_level_vars = self.global_context.top_level_vars;
    monomorphized_ctx.variables = self.global_context.variables;
    monomorphized_ctx.type_aliases = self.global_context.type_aliases;
    self.global_context = monomorphized_ctx;
    Ok(self)
  }
  pub fn compile_to_wgsl(self) -> CompileResult<String> {
    let mut wgsl = String::new();
    for v in self.global_context.top_level_vars {
      wgsl += &v.compile();
      wgsl += ";\n";
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
