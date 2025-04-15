use std::{cell::RefCell, cmp::Ordering, rc::Rc};

use sse::{document::Document, syntax::EncloserOrOperator};
use take_mut::take;

use crate::{
  compiler::{
    error::{CompileError, SourceTrace},
    expression::{arg_list_and_return_type_from_easl_tree, Exp, ExpKind},
    functions::AbstractFunctionSignature,
    metadata::extract_metadata,
    structs::UntypedStruct,
    types::{
      parse_generic_argument, AbstractType, ExpTypeInfo, Type, TypeConstraint,
      TypeState, Variable, VariableKind,
    },
    util::read_type_annotated_name,
  },
  parse::{Context as SyntaxContext, EaslTree, Encloser, Operator},
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
  pub global_context: Context,
}

impl Program {
  pub fn from_easl_document(
    document: &'_ EaslDocument,
    macros: Vec<Macro>,
  ) -> (Self, Vec<CompileError>) {
    let (trees, macro_errors) = document
      .syntax_trees
      .iter()
      .cloned()
      .map(|tree| macroexpand(tree, &macros))
      .collect::<(Vec<EaslTree>, Vec<Vec<(SourceTrace, Rc<str>)>>)>();
    let mut errors: Vec<CompileError> = macro_errors
      .into_iter()
      .flatten()
      .map(|(source_trace, err_str)| {
        CompileError::new(MacroError(err_str), source_trace)
      })
      .collect();

    let mut non_struct_trees = vec![];
    let mut untyped_structs = vec![];

    for tree in trees.into_iter() {
      use crate::parse::Encloser::*;
      use sse::syntax::EncloserOrOperator::*;
      let (tree_body, metadata, mut metadata_errors) =
        extract_metadata(tree.clone());
      errors.append(&mut metadata_errors);
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
                  match UntypedStruct::from_field_trees(
                    struct_name.clone().into(),
                    vec![],
                    children_iter.cloned().collect(),
                    source_trace,
                  ) {
                    Ok(s) => untyped_structs.push(s),
                    Err(e) => errors.push(e),
                  }
                }
                EaslTree::Inner(
                  (position, Encloser(Parens)),
                  signature_children,
                ) => {
                  let source_trace: SourceTrace = position.clone().into();
                  let (signature_leaves, signature_errors) = signature_children
                    .into_iter()
                    .map(|child| match child {
                      EaslTree::Leaf(_, name) => {
                        (Some(name.clone().into()), None)
                      }
                      _ => (
                        None,
                        Some(CompileError::new(
                          InvalidStructName,
                          child.position().clone().into(),
                        )),
                      ),
                    })
                    .collect::<(Vec<Option<Rc<str>>>, Vec<Option<CompileError>>)>();
                  let mut filtered_signature_errors: Vec<CompileError> =
                    signature_errors.into_iter().filter_map(|x| x).collect();
                  if filtered_signature_errors.is_empty() {
                    let mut filtered_signature_leaves = signature_leaves
                      .into_iter()
                      .filter_map(|x| x)
                      .collect::<Vec<_>>()
                      .into_iter();
                    if let Some(struct_name) = filtered_signature_leaves.next()
                    {
                      if filtered_signature_leaves.len() == 0 {
                        errors.push(CompileError::new(
                          InvalidStructName,
                          source_trace,
                        ));
                      } else {
                        match UntypedStruct::from_field_trees(
                          struct_name,
                          filtered_signature_leaves.collect(),
                          children_iter.cloned().collect(),
                          source_trace,
                        ) {
                          Ok(s) => untyped_structs.push(s),
                          Err(e) => errors.push(e),
                        }
                      }
                    } else {
                      errors.push(CompileError::new(
                        InvalidStructName,
                        source_trace,
                      ));
                    }
                  } else {
                    errors.append(&mut filtered_signature_errors);
                  }
                }
                EaslTree::Inner((position, _), _) => {
                  errors.push(CompileError::new(
                    InvalidStructName,
                    position.clone().into(),
                  ));
                }
              }
            } else {
              errors
                .push(CompileError::new(InvalidStructDefinition, source_trace));
            }
          } else {
            non_struct_trees.push((metadata, tree_body))
          }
        } else {
          errors.push(CompileError::new(
            UnrecognizedTopLevelForm(tree_body.clone()),
            source_trace,
          ));
        }
      } else {
        errors.push(CompileError::new(
          UnrecognizedTopLevelForm(tree_body),
          tree.position().clone().into(),
        ));
      }
    }
    untyped_structs.sort_by(|a, b| {
      if a.references_type_name(&b.name) {
        Ordering::Greater
      } else if b.references_type_name(&a.name) {
        Ordering::Less
      } else {
        Ordering::Equal
      }
    });
    let mut global_context = Context::default_global();
    for untyped_struct in untyped_structs {
      match untyped_struct
        .assign_types(&global_context.structs, &built_in_type_aliases())
      {
        Ok(s) => global_context = global_context.with_struct(s.into()),
        Err(e) => errors.push(e),
      }
    }

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
              if let Some((attributes, name_and_type_ast)) = match children_iter
                .len()
              {
                1 => Some((vec![], children_iter.next().unwrap())),
                2 => {
                  let attributes_ast = children_iter.next().unwrap();
                  if let EaslTree::Inner(
                    (position, Encloser(Square)),
                    attribute_asts,
                  ) = attributes_ast
                  {
                    let (attributes, attribute_errors): (
                      Vec<Option<Rc<str>>>,
                      Vec<Option<CompileError>>,
                    ) =
                      attribute_asts
                        .into_iter()
                        .map(|attribute_ast| {
                          if let EaslTree::Leaf(_, attribute_string) =
                            attribute_ast
                          {
                            (Some(attribute_string.into()), None)
                          } else {
                            (None, Some( CompileError::new(
                            InvalidTopLevelVar(
                              "Expected leaf for attribute, found inner form"
                                .into(),
                            ),
                            position.clone().into(),
                          )))
                          }
                        })
                        .collect();
                    let mut filtered_attribute_errors: Vec<CompileError> =
                      attribute_errors.into_iter().filter_map(|x| x).collect();
                    if !filtered_attribute_errors.is_empty() {
                      errors.append(&mut filtered_attribute_errors);
                    }
                    Some((
                      attributes.into_iter().filter_map(|x| x).collect(),
                      children_iter.next().unwrap(),
                    ))
                  } else {
                    errors.push(CompileError::new(
                      InvalidTopLevelVar(
                        "Expected square-bracket enclosed attributes".into(),
                      ),
                      first_child_source_trace,
                    ));
                    None
                  }
                }
                _ => {
                  errors.push(CompileError::new(
                    InvalidTopLevelVar("Invalid number of inner forms".into()),
                    first_child_source_trace,
                  ));
                  None
                }
              } {
                match read_type_annotated_name(name_and_type_ast) {
                  Ok((name, type_ast)) => {
                    let type_source_path = type_ast.position().clone();
                    match AbstractType::from_easl_tree(
                      type_ast,
                      &global_context.structs,
                      &global_context.type_aliases,
                      &vec![],
                    )
                    .map(|t| {
                      t.concretize(
                        &vec![],
                        &global_context.structs,
                        type_source_path.into(),
                      )
                    }) {
                      Err(e) | Ok(Err(e)) => errors.push(e),
                      Ok(Ok(t)) => {
                        if let Some((metadata, metadata_source_trace)) =
                          &metadata
                        {
                          if let Err(e) = metadata
                            .validate_for_top_level_variable(
                              &metadata_source_trace,
                            )
                          {
                            errors.push(e);
                          }
                        }
                        global_context.top_level_vars.push(TopLevelVar {
                          name,
                          metadata: metadata.map(|(a, _)| a),
                          attributes,
                          var: Variable::new(TypeState::Known(t).into())
                            .with_kind(VariableKind::Var),
                          value: None,
                          source_trace: parens_source_trace,
                        })
                      }
                    }
                  }
                  Err(e) => errors.push(e),
                }
              }
            }
            "def" | "override" => {
              if children_iter.len() == 2 {
                match read_type_annotated_name(children_iter.next().unwrap()) {
                  Ok((name, type_ast)) => {
                    match TypedExp::try_from_easl_tree(
                      children_iter.next().unwrap(),
                      &global_context.structs,
                      &vec![],
                      &vec![],
                      crate::compiler::expression::SyntaxTreeContext::Default,
                    ) {
                      Ok(value_expression) => {
                        match Type::from_easl_tree(
                          type_ast,
                          &global_context.structs,
                          &global_context.type_aliases,
                          &vec![],
                        ) {
                          Ok(t) => {
                            let mut var =
                              Variable::new(TypeState::Known(t).into());
                            if first_child.as_str() == "override" {
                              var = var.with_kind(VariableKind::Override)
                            }
                            if metadata.is_some() {
                              errors.push(CompileError::new(
                                ConstantMayNotHaveMetadata,
                                parens_source_trace.clone(),
                              ));
                            }
                            global_context.top_level_vars.push(TopLevelVar {
                              name,
                              metadata: None,
                              attributes: vec![],
                              var,
                              value: Some(value_expression),
                              source_trace: parens_source_trace,
                            })
                          }
                          Err(e) => errors.push(e),
                        }
                      }
                      Err(e) => errors.push(e),
                    }
                  }
                  Err(e) => errors.push(e),
                }
              } else {
                errors.push(CompileError::new(
                  InvalidTopLevelVar(
                    "Expected two forms inside \"def\"".into(),
                  ),
                  first_child_source_trace,
                ));
              }
            }
            "defn" => match children_iter.next() {
              Some(name_ast) => {
                let fn_and_generic_names: Option<(Rc<str>, Vec<_>)> =
                  match name_ast {
                    EaslTree::Leaf(_, name) => Some((name.into(), vec![])),
                    EaslTree::Inner((_, Encloser(Parens)), subtrees) => {
                      let mut subtrees_iter = subtrees.into_iter();
                      if let Some(EaslTree::Leaf(_, name)) =
                        subtrees_iter.next()
                      {
                        match subtrees_iter
                          .map(|subtree| {
                            parse_generic_argument(
                              subtree,
                              &global_context.structs,
                              &global_context.type_aliases,
                              &vec![],
                            )
                          })
                          .collect::<CompileResult<Vec<_>>>()
                        {
                          Ok(generic_args) => Some((name.into(), generic_args)),
                          Err(e) => {
                            errors.push(e);
                            None
                          }
                        }
                      } else {
                        errors.push(CompileError::new(
                          InvalidDefn("Invalid name".into()),
                          first_child_source_trace,
                        ));
                        None
                      }
                    }
                    _ => {
                      errors.push(CompileError::new(
                      InvalidDefn(
                        "Expected name or parens with name and generic arguments"
                          .into(),
                      ),
                      first_child_source_trace,
                    ));
                      None
                    }
                  };
                if let Some((fn_name, generic_args)) = fn_and_generic_names {
                  match children_iter.next() {
                    Some(arg_list_ast) => {
                      let generic_arg_names: Vec<Rc<str>> = generic_args
                        .iter()
                        .map(|(name, _)| name.clone())
                        .collect();
                      match arg_list_and_return_type_from_easl_tree(
                        arg_list_ast,
                        &global_context.structs,
                        &global_context.type_aliases,
                        &generic_arg_names,
                      ) {
                        Ok((
                          source_path,
                          arg_names,
                          arg_types,
                          arg_metadata,
                          return_type,
                          return_metadata,
                        )) => {
                          match return_type.concretize(
                            &generic_arg_names,
                            &global_context.structs,
                            source_path.clone().into(),
                          ) {
                            Ok(concrete_return_type) => {
                              match arg_types
                                .iter()
                                .map(|t| {
                                  Ok((
                                    TypeState::Known(t.concretize(
                                      &generic_arg_names,
                                      &global_context.structs,
                                      source_path.clone().into(),
                                    )?)
                                    .into(),
                                    if let AbstractType::Generic(generic_name) =
                                      t
                                    {
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
                                  Vec<(ExpTypeInfo, Vec<TypeConstraint>)>,
                                >>() {
                                Ok(concrete_arg_types) => {
                                  match TypedExp::function_from_body_tree(
                                    source_path.clone(),
                                    children_iter.collect(),
                                    TypeState::Known(concrete_return_type)
                                      .into(),
                                    arg_names.clone(),
                                    concrete_arg_types,
                                    &global_context.structs,
                                    &global_context.type_aliases,
                                    &generic_arg_names,
                                  ) {
                                    Ok(body) => {
                                      let associative = if let Some((
                                        metadata,
                                        metadata_source_trace,
                                      )) = &metadata
                                      {
                                        match metadata
                                          .validate_for_top_level_function(
                                            metadata_source_trace,
                                          ) {
                                          Ok(is_associative) => is_associative,
                                          Err(e) => {
                                            errors.push(e);
                                            false
                                          }
                                        }
                                      } else {
                                        false
                                      };
                                      let implementation =
                                        FunctionImplementationKind::Composite(
                                          Rc::new(RefCell::new(
                                            TopLevelFunction {
                                              arg_names,
                                              arg_metadata,
                                              return_metadata,
                                              metadata: metadata
                                                .map(|(a, _)| a),
                                              body,
                                            },
                                          )),
                                        );
                                      global_context.add_abstract_function(
                                        Rc::new(RefCell::new(
                                          AbstractFunctionSignature {
                                            name: fn_name,
                                            generic_args,
                                            arg_types,
                                            return_type,
                                            implementation,
                                            associative,
                                          },
                                        )),
                                      );
                                    }
                                    Err(e) => errors.push(e),
                                  }
                                }
                                Err(e) => {
                                  errors.push(e);
                                }
                              }
                            }
                            Err(e) => {
                              errors.push(e);
                            }
                          }
                        }
                        Err(e) => errors.push(e),
                      }
                    }
                    None => errors.push(CompileError::new(
                      InvalidDefn("Missing Argument List".into()),
                      parens_source_trace.clone(),
                    )),
                  }
                }
              }
              None => errors.push(CompileError::new(
                InvalidDefn("Missing Name".into()),
                parens_source_trace.clone(),
              )),
            },
            _ => {
              errors.push(CompileError::new(
                UnrecognizedTopLevelForm(EaslTree::Leaf(
                  first_child_position.clone(),
                  first_child,
                )),
                first_child_source_trace,
              ));
            }
          }
        } else {
          errors.push(CompileError::new(
            UnrecognizedTopLevelForm(first_child.unwrap_or(EaslTree::Inner(
              (
                parens_position.clone(),
                EncloserOrOperator::Encloser(Parens),
              ),
              vec![],
            ))),
            parens_source_trace,
          ));
        }
      } else {
        errors.push(CompileError::new(
          UnrecognizedTopLevelForm(tree.clone()),
          tree.position().clone().into(),
        ));
      }
    }
    (Self { global_context }, errors)
  }
  fn propagate_types(&mut self) -> (bool, Vec<CompileError>) {
    let mut errors = vec![];
    let mut base_context = self.global_context.clone();
    let mut anything_changed = false;
    for var in self.global_context.top_level_vars.iter_mut() {
      if let Some(value_expression) = &mut var.value {
        let (changed, mut sub_errors) = value_expression
          .data
          .constrain(var.var.typestate.kind.clone(), var.source_trace.clone());
        anything_changed |= changed;
        errors.append(&mut sub_errors);
        let (changed, mut sub_errors) =
          value_expression.propagate_types(&mut base_context);
        anything_changed |= changed;
        errors.append(&mut sub_errors);
      }
    }
    for f in self.global_context.abstract_functions_iter_mut() {
      if let FunctionImplementationKind::Composite(implementation) =
        &f.borrow().implementation
      {
        let (changed, mut f_errors) = implementation
          .borrow_mut()
          .body
          .propagate_types(&mut base_context);
        anything_changed |= changed;
        errors.append(&mut f_errors);
      }
    }
    (anything_changed, errors)
  }
  fn find_untyped(&mut self) -> Vec<TypedExp> {
    self
      .global_context
      .abstract_functions_iter()
      .map(|f| {
        if let FunctionImplementationKind::Composite(implementation) =
          &f.borrow().implementation
        {
          implementation.borrow_mut().body.find_untyped()
        } else {
          vec![]
        }
      })
      .collect::<Vec<_>>()
      .into_iter()
      .chain(self.global_context.top_level_vars.iter_mut().map(|v| {
        if let Some(value) = &mut v.value {
          value.find_untyped()
        } else {
          vec![]
        }
      }))
      .flatten()
      .collect()
  }
  pub fn validate_match_blocks(self) -> CompileResult<Self> {
    for abstract_function in self.global_context.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(implementation) =
        &abstract_function.borrow().implementation
      {
        (**implementation)
          .borrow_mut()
          .body
          .validate_match_blocks()?;
      }
    }
    Ok(self)
  }
  pub fn fully_infer_types(&mut self) -> Vec<CompileError> {
    let mut errors = vec![];
    loop {
      let (did_type_states_change, mut current_errors) = self.propagate_types();
      errors.append(&mut current_errors);
      if !did_type_states_change {
        let untyped_expressions = self.find_untyped();
        return if untyped_expressions.is_empty() {
          break;
        } else {
          untyped_expressions
            .into_iter()
            .map(|exp| {
              let source_trace = exp.source_trace.clone();
              CompileError::new(CouldntInferTypes(exp), source_trace)
            })
            .collect()
        };
      }
    }
    errors
  }
  pub fn check_assignment_validity(&mut self) -> Vec<CompileError> {
    let mut errors = vec![];
    for f in self.global_context.abstract_functions_iter() {
      let mut ctx = self.global_context.clone();
      if let FunctionImplementationKind::Composite(implementation) =
        &f.borrow().implementation
      {
        if let Err(e) = implementation
          .borrow_mut()
          .body
          .check_assignment_validity(&mut ctx)
        {
          errors.push(e);
        }
      }
    }
    errors
  }
  pub fn monomorphize(&mut self) -> Vec<CompileError> {
    let mut errors = vec![];
    let mut monomorphized_ctx = Context::default_global();
    for f in self.global_context.abstract_functions_iter() {
      if f.borrow().generic_args.is_empty() {
        if let FunctionImplementationKind::Composite(implementation) =
          &f.borrow().implementation
        {
          match implementation
            .borrow_mut()
            .body
            .monomorphize(&self.global_context, &mut monomorphized_ctx)
          {
            Ok(_) => {
              let mut new_f = (**f).borrow().clone();
              new_f.implementation =
                FunctionImplementationKind::Composite(implementation.clone());
              monomorphized_ctx
                .add_abstract_function(Rc::new(RefCell::new(new_f)));
            }
            Err(e) => errors.push(e),
          }
        }
      }
    }
    take(&mut self.global_context, |old_ctx| {
      monomorphized_ctx.structs = old_ctx.structs;
      monomorphized_ctx.top_level_vars = old_ctx.top_level_vars;
      monomorphized_ctx.variables = old_ctx.variables;
      monomorphized_ctx.type_aliases = old_ctx.type_aliases;
      monomorphized_ctx
    });
    errors
  }
  pub fn inline_all_higher_order_arguments(&mut self) -> Vec<CompileError> {
    let (changed, errors) = self.inline_higher_order_arguments();
    if !errors.is_empty() {
      return errors;
    }
    if changed {
      self.inline_all_higher_order_arguments()
    } else {
      vec![]
    }
  }
  pub fn inline_higher_order_arguments(&mut self) -> (bool, Vec<CompileError>) {
    let mut changed = false;
    let mut errors = vec![];
    let mut inlined_ctx = Context::default_global();
    for f in self.global_context.abstract_functions_iter() {
      if f.borrow().generic_args.is_empty()
        && f
          .borrow()
          .arg_types
          .iter()
          .find(|t| {
            if let AbstractType::Type(Type::Function(_)) = t {
              true
            } else {
              false
            }
          })
          .is_none()
      {
        if let FunctionImplementationKind::Composite(implementation) =
          &f.borrow().implementation
        {
          match implementation
            .borrow_mut()
            .body
            .inline_higher_order_arguments(&mut inlined_ctx)
          {
            Ok(added_new_function) => {
              changed |= added_new_function;
              let mut new_f = (**f).borrow().clone();
              new_f.implementation =
                FunctionImplementationKind::Composite(implementation.clone());
              inlined_ctx.add_abstract_function(Rc::new(RefCell::new(new_f)));
            }
            Err(e) => errors.push(e),
          }
        }
      }
    }
    take(&mut self.global_context, |old_ctx| {
      inlined_ctx.structs = old_ctx.structs;
      inlined_ctx.top_level_vars = old_ctx.top_level_vars;
      inlined_ctx.variables = old_ctx.variables;
      inlined_ctx.type_aliases = old_ctx.type_aliases;
      inlined_ctx
    });
    (changed, errors)
  }
  pub fn compile_to_wgsl(self) -> CompileResult<String> {
    let mut wgsl = String::new();
    for v in self.global_context.top_level_vars.iter() {
      wgsl += &v.clone().compile();
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
      if let Some(compiled_struct) = Rc::unwrap_or_clone(s)
        .compile_if_non_generic(&self.global_context.structs)?
      {
        wgsl += &compiled_struct;
        wgsl += "\n\n";
      }
    }
    for f in self.global_context.abstract_functions_iter() {
      let f = f.borrow().clone();
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
  pub fn expand_associative_applications(&mut self) {
    for f in self
      .global_context
      .abstract_functions
      .iter_mut()
      .map(|(_, fns)| fns.into_iter())
      .flatten()
    {
      if let FunctionImplementationKind::Composite(f) =
        &f.borrow().implementation
      {
        f.borrow_mut().body.walk_mut(
          &mut |exp| {
            take(
              &mut exp.kind, 
              |exp_kind| {
                if let ExpKind::Application(f, args) = exp_kind {
                  if let ExpKind::Name(_) = &f.kind {
                    let Type::Function(x) = f.data.kind.unwrap_known() else {
                      panic!("encountered application of non-fn in expand_associative_applications");
                    };
                    if let Some(abstract_ancestor) = &x.abstract_ancestor {
                      if abstract_ancestor.associative && args.len() != 2 {
                        let mut args_iter = args.into_iter();
                        let mut new_exp = args_iter.next().unwrap();
                        while let Some(next_arg) = args_iter.next() {
                          new_exp = Exp {
                            kind: ExpKind::Application(f.clone(), vec![new_exp, next_arg]),
                            data: exp.data.clone(),
                            source_trace: exp.source_trace.clone()
                          };
                        }
                        new_exp.kind
                      } else {
                        ExpKind::Application(f, args)
                      }
                    } else {
                      ExpKind::Application(f, args)
                    }
                  } else {
                    ExpKind::Application(f, args)
                  }
                } else {
                  exp_kind
                }
              }
            );
            Ok(true)
          }
        )
        .unwrap();
      }
    }
  }
  pub fn deshadow(&mut self) -> Vec<CompileError> {
    let globally_bound_names: Vec<Rc<str>> = self
      .global_context
      .top_level_vars
      .iter()
      .map(|v| Rc::clone(&v.name))
      .chain(
        self
          .global_context
          .abstract_functions
          .iter()
          .map(|(name, _)| Rc::clone(name)),
      )
      .collect();
    self
      .global_context
      .abstract_functions
      .iter_mut()
      .flat_map(|(_, signatures)| {
        signatures.iter_mut().flat_map(|signature| {
          if let FunctionImplementationKind::Composite(exp) =
            &mut signature.borrow_mut().implementation
          {
            exp.borrow_mut().body.deshadow(&globally_bound_names)
          } else {
            vec![]
          }
          .into_iter()
        })
      })
      .collect()
  }
  pub fn validate_raw_program(&mut self) -> Vec<CompileError> {
    let errors = self.fully_infer_types();
    if !errors.is_empty() {
      return errors;
    }
    self.expand_associative_applications();
    let errors = self.check_assignment_validity();
    if !errors.is_empty() {
      return errors;
    }
    let errors = self.monomorphize();
    if !errors.is_empty() {
      return errors;
    }
    let errors = self.inline_all_higher_order_arguments();
    if !errors.is_empty() {
      return errors;
    }
    let errors = self.deshadow();
    if !errors.is_empty() {
      return errors;
    }
    vec![]
  }
  pub fn gather_type_annotations(&self) -> Vec<(SourceTrace, TypeState)> {
    let mut type_annotations = vec![];
    for (_, signatures) in self.global_context.abstract_functions.iter() {
      for signature in signatures {
        if let FunctionImplementationKind::Composite(implementation) =
          &(&**signature).borrow().implementation
        {
          implementation
            .borrow()
            .body
            .walk(&mut |exp: &TypedExp| {
              type_annotations
                .push((exp.source_trace.clone(), exp.data.kind.clone()));
              Ok(true)
            })
            .unwrap();
        }
      }
    }
    type_annotations
  }
}
