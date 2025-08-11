use std::{cell::RefCell, cmp::Ordering, collections::HashMap, rc::Rc};

use sse::{document::Document, syntax::EncloserOrOperator};
use take_mut::take;

use crate::{
  Never,
  compiler::{
    builtins::built_in_functions,
    error::{CompileError, SourceTrace},
    expression::{Exp, ExpKind, arg_list_and_return_type_from_easl_tree},
    functions::AbstractFunctionSignature,
    metadata::extract_metadata,
    structs::UntypedStruct,
    types::{
      AbstractType, ExpTypeInfo, Type, TypeConstraint, TypeState, Variable,
      VariableKind, parse_generic_argument,
    },
    util::read_type_annotated_name,
  },
  parse::{Context as SyntaxContext, EaslTree, Encloser, Operator},
};

use super::{
  builtins::{
    ABNORMAL_CONSTRUCTOR_STRUCTS, built_in_structs, built_in_type_aliases,
  },
  error::{
    CompileErrorKind::{self, *},
    CompileResult, ErrorLog,
  },
  expression::TypedExp,
  functions::{FunctionImplementationKind, TopLevelFunction},
  macros::{Macro, macroexpand},
  structs::AbstractStruct,
  vars::TopLevelVar,
};

pub type EaslDocument<'s> = Document<'s, SyntaxContext, Encloser, Operator>;

thread_local! {
  pub static DEFAULT_PROGRAM: RefCell<Program> =
    RefCell::new(
      Program::empty()
        .with_functions(built_in_functions())
        .with_structs(
          built_in_structs().into_iter().map(|s| Rc::new(s)).collect(),
        )
        .with_type_aliases(built_in_type_aliases()));
}

#[derive(Debug, Clone)]
pub struct Program {
  pub structs: Vec<Rc<AbstractStruct>>,
  pub abstract_functions:
    HashMap<Rc<str>, Vec<Rc<RefCell<AbstractFunctionSignature>>>>,
  pub type_aliases: Vec<(Rc<str>, Rc<AbstractStruct>)>,
  pub top_level_vars: Vec<TopLevelVar>,
}

impl Default for Program {
  fn default() -> Self {
    DEFAULT_PROGRAM.with_borrow(|ctx| ctx.clone())
  }
}

impl Program {
  pub fn empty() -> Self {
    Self {
      structs: vec![],
      abstract_functions: HashMap::new(),
      type_aliases: vec![],
      top_level_vars: vec![],
    }
  }
  pub fn add_abstract_function(
    &mut self,
    signature: Rc<RefCell<AbstractFunctionSignature>>,
  ) {
    let name = Rc::clone(&signature.borrow().name);
    if let Some(bucket) = self.abstract_functions.get_mut(&name) {
      bucket.push(signature.into());
    } else {
      self.abstract_functions.insert(name, vec![signature.into()]);
    }
  }
  pub fn with_functions(
    mut self,
    functions: Vec<AbstractFunctionSignature>,
  ) -> Self {
    for f in functions {
      self.add_abstract_function(Rc::new(RefCell::new(f)));
    }
    self
  }
  pub fn with_struct(mut self, s: Rc<AbstractStruct>) -> Self {
    if !self.structs.contains(&s) {
      if !ABNORMAL_CONSTRUCTOR_STRUCTS.contains(&&*s.name) {
        self.add_abstract_function(Rc::new(RefCell::new(
          AbstractFunctionSignature {
            name: s.name.clone(),
            generic_args: s
              .generic_args
              .iter()
              .map(|name| (name.clone(), vec![]))
              .collect(),
            arg_types: s
              .fields
              .iter()
              .map(|field| field.field_type.clone())
              .collect(),
            mutated_args: vec![],
            return_type: AbstractType::AbstractStruct(s.clone()),
            implementation: FunctionImplementationKind::Constructor,
            associative: false,
          },
        )));
      }
      self.structs.push(s);
      self.structs.dedup();
    }
    self
  }
  pub fn with_structs(self, structs: Vec<Rc<AbstractStruct>>) -> Self {
    structs.into_iter().fold(self, |ctx, s| ctx.with_struct(s))
  }
  pub fn with_type_aliases(
    mut self,
    mut aliases: Vec<(Rc<str>, Rc<AbstractStruct>)>,
  ) -> Self {
    self.type_aliases.append(&mut aliases);
    self
  }
  pub fn add_monomorphized_struct(&mut self, s: Rc<AbstractStruct>) {
    if self
      .structs
      .iter()
      .find(|existing_struct| {
        existing_struct.name == s.name
          && existing_struct.filled_generics == s.filled_generics
      })
      .is_none()
    {
      self.structs.push(s);
    }
  }
  pub fn concrete_signatures(
    &mut self,
    fn_name: &Rc<str>,
    source_trace: SourceTrace,
  ) -> CompileResult<Option<Vec<Type>>> {
    if let Some(signatures) = self.abstract_functions.get(fn_name) {
      signatures
        .into_iter()
        .map(|signature| {
          Ok(Type::Function(Box::new(
            AbstractFunctionSignature::concretize(
              Rc::new(RefCell::new(signature.borrow().clone())),
              &self.structs,
              source_trace.clone(),
            )?,
          )))
        })
        .collect::<CompileResult<Vec<_>>>()
        .map(|x| Some(x))
    } else {
      Ok(None)
    }
  }
  pub fn abstract_functions_iter(
    &self,
  ) -> impl Iterator<Item = &Rc<RefCell<AbstractFunctionSignature>>> {
    self
      .abstract_functions
      .values()
      .map(|fs| fs.iter())
      .flatten()
  }
  pub fn abstract_functions_iter_mut(
    &mut self,
  ) -> impl Iterator<Item = &mut Rc<RefCell<AbstractFunctionSignature>>> {
    self
      .abstract_functions
      .values_mut()
      .map(|fs| fs.iter_mut())
      .flatten()
  }
  pub fn from_easl_document(
    document: &'_ EaslDocument,
    macros: Vec<Macro>,
  ) -> (Self, ErrorLog) {
    let mut errors = ErrorLog::new();
    let trees = document
      .syntax_trees
      .iter()
      .cloned()
      .map(|tree| macroexpand(tree, &macros, &mut errors))
      .collect::<Vec<EaslTree>>();

    let mut non_struct_trees = vec![];
    let mut untyped_structs = vec![];

    for tree in trees.into_iter() {
      use crate::parse::Encloser::*;
      use sse::syntax::EncloserOrOperator::*;
      let (tree_body, metadata) = extract_metadata(tree.clone(), &mut errors);
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
                    Err(e) => errors.log(e),
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
                  let filtered_signature_errors: Vec<CompileError> =
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
                        errors.log(CompileError::new(
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
                          Err(e) => errors.log(e),
                        }
                      }
                    } else {
                      errors.log(CompileError::new(
                        InvalidStructName,
                        source_trace,
                      ));
                    }
                  } else {
                    errors.log_all(filtered_signature_errors);
                  }
                }
                EaslTree::Inner((position, _), _) => {
                  errors.log(CompileError::new(
                    InvalidStructName,
                    position.clone().into(),
                  ));
                }
              }
            } else {
              errors
                .log(CompileError::new(InvalidStructDefinition, source_trace));
            }
          } else {
            non_struct_trees.push((metadata, tree_body))
          }
        } else {
          errors.log(CompileError::new(
            UnrecognizedTopLevelForm(tree_body.clone()),
            source_trace,
          ));
        }
      } else {
        errors.log(CompileError::new(
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
    let mut program = Program::default();
    for untyped_struct in untyped_structs {
      match untyped_struct
        .assign_types(&program.structs, &built_in_type_aliases())
      {
        Ok(s) => program = program.with_struct(s.into()),
        Err(e) => errors.log(e),
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
              if let Some((attributes, name_and_type_ast, value_ast)) =
                match children_iter.len() {
                  1 => Some((vec![], children_iter.next().unwrap(), None)),
                  2 | 3 => {
                    let attributes_ast = children_iter.next().unwrap();
                    if let EaslTree::Inner(
                      (position, Encloser(Square)),
                      attribute_asts,
                    ) = attributes_ast
                    {
                      let (attributes, attribute_errors): (
                        Vec<Option<Rc<str>>>,
                        Vec<Option<CompileError>>,
                      ) = attribute_asts
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
                      errors.log_all(
                        attribute_errors.into_iter().filter_map(|x| x),
                      );
                      Some((
                        attributes.into_iter().filter_map(|x| x).collect(),
                        children_iter.next().unwrap(),
                        children_iter.next(),
                      ))
                    } else {
                      errors.log(CompileError::new(
                        InvalidTopLevelVar(
                          "Expected square-bracket enclosed attributes".into(),
                        ),
                        first_child_source_trace,
                      ));
                      None
                    }
                  }
                  _ => {
                    errors.log(CompileError::new(
                      InvalidTopLevelVar(
                        "Invalid number of inner forms".into(),
                      ),
                      first_child_source_trace,
                    ));
                    None
                  }
                }
              {
                match read_type_annotated_name(name_and_type_ast) {
                  Ok((name, type_ast)) => {
                    let type_source_path = type_ast.position().clone();
                    match AbstractType::from_easl_tree(
                      type_ast,
                      &program.structs,
                      &program.type_aliases,
                      &vec![],
                    )
                    .map(|t| {
                      t.concretize(
                        &vec![],
                        &program.structs,
                        type_source_path.into(),
                      )
                    }) {
                      Err(e) | Ok(Err(e)) => errors.log(e),
                      Ok(Ok(t)) => {
                        if let Some((metadata, metadata_source_trace)) =
                          &metadata
                        {
                          if let Err(e) = metadata
                            .validate_for_top_level_variable(
                              &metadata_source_trace,
                            )
                          {
                            errors.log(e);
                          }
                        }
                        let value = value_ast.map(|value_ast| match TypedExp::try_from_easl_tree(
                          value_ast,
                          &program.structs,
                          &vec![],
                          &vec![],
                          crate::compiler::expression::SyntaxTreeContext::Default,
                        ) {
                            Ok(exp) => Some(exp),
                            Err(e) => {errors.log(e); None},
                        }).flatten();
                        program.top_level_vars.push(TopLevelVar {
                          name,
                          metadata: metadata.map(|(a, _)| a),
                          attributes,
                          var: Variable::new(TypeState::Known(t).into())
                            .with_kind(VariableKind::Var),
                          value,
                          source_trace: parens_source_trace,
                        })
                      }
                    }
                  }
                  Err(e) => errors.log(e),
                }
              }
            }
            "def" | "override" => {
              if children_iter.len() == 2 {
                match read_type_annotated_name(children_iter.next().unwrap()) {
                  Ok((name, type_ast)) => {
                    match TypedExp::try_from_easl_tree(
                      children_iter.next().unwrap(),
                      &program.structs,
                      &vec![],
                      &vec![],
                      crate::compiler::expression::SyntaxTreeContext::Default,
                    ) {
                      Ok(value_expression) => {
                        match Type::from_easl_tree(
                          type_ast,
                          &program.structs,
                          &program.type_aliases,
                          &vec![],
                        ) {
                          Ok(t) => {
                            let mut var =
                              Variable::new(TypeState::Known(t).into());
                            if first_child.as_str() == "override" {
                              var = var.with_kind(VariableKind::Override)
                            }
                            if metadata.is_some() {
                              errors.log(CompileError::new(
                                ConstantMayNotHaveMetadata,
                                parens_source_trace.clone(),
                              ));
                            }
                            program.top_level_vars.push(TopLevelVar {
                              name,
                              metadata: None,
                              attributes: vec![],
                              var,
                              value: Some(value_expression),
                              source_trace: parens_source_trace,
                            })
                          }
                          Err(e) => errors.log(e),
                        }
                      }
                      Err(e) => errors.log(e),
                    }
                  }
                  Err(e) => errors.log(e),
                }
              } else {
                errors.log(CompileError::new(
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
                              &program.structs,
                              &program.type_aliases,
                              &vec![],
                            )
                          })
                          .collect::<CompileResult<Vec<_>>>()
                        {
                          Ok(generic_args) => Some((name.into(), generic_args)),
                          Err(e) => {
                            errors.log(e);
                            None
                          }
                        }
                      } else {
                        errors.log(CompileError::new(
                          InvalidDefn("Invalid name".into()),
                          first_child_source_trace,
                        ));
                        None
                      }
                    }
                    _ => {
                      errors.log(CompileError::new(
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
                        &program.structs,
                        &program.type_aliases,
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
                            &program.structs,
                            source_path.clone().into(),
                          ) {
                            Ok(concrete_return_type) => {
                              match arg_types
                                .iter()
                                .map(|t| {
                                  Ok((
                                    TypeState::Known(t.concretize(
                                      &generic_arg_names,
                                      &program.structs,
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
                                    &program.structs,
                                    &program.type_aliases,
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
                                            errors.log(e);
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
                                      program.add_abstract_function(Rc::new(
                                        RefCell::new(
                                          AbstractFunctionSignature {
                                            name: fn_name,
                                            generic_args,
                                            arg_types,
                                            mutated_args: vec![],
                                            return_type,
                                            implementation,
                                            associative,
                                          },
                                        ),
                                      ));
                                    }
                                    Err(e) => errors.log(e),
                                  }
                                }
                                Err(e) => {
                                  errors.log(e);
                                }
                              }
                            }
                            Err(e) => {
                              errors.log(e);
                            }
                          }
                        }
                        Err(e) => errors.log(e),
                      }
                    }
                    None => errors.log(CompileError::new(
                      InvalidDefn("Missing Argument List".into()),
                      parens_source_trace.clone(),
                    )),
                  }
                }
              }
              None => errors.log(CompileError::new(
                InvalidDefn("Missing Name".into()),
                parens_source_trace.clone(),
              )),
            },
            _ => {
              errors.log(CompileError::new(
                UnrecognizedTopLevelForm(EaslTree::Leaf(
                  first_child_position.clone(),
                  first_child,
                )),
                first_child_source_trace,
              ));
            }
          }
        } else {
          errors.log(CompileError::new(
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
        errors.log(CompileError::new(
          UnrecognizedTopLevelForm(tree.clone()),
          tree.position().clone().into(),
        ));
      }
    }
    (program, errors)
  }
  fn propagate_types(&mut self, errors: &mut ErrorLog) -> bool {
    let mut base_context = self.clone();
    let mut anything_changed = false;
    for var in self.top_level_vars.iter_mut() {
      if let Some(value_expression) = &mut var.value {
        let changed = value_expression.data.constrain(
          var.var.typestate.kind.clone(),
          &var.source_trace,
          errors,
        );
        anything_changed |= changed;
        let changed =
          value_expression.propagate_types(&mut base_context, errors);
        anything_changed |= changed;
      }
    }
    for f in self.abstract_functions_iter_mut() {
      if let FunctionImplementationKind::Composite(implementation) =
        &f.borrow().implementation
      {
        let changed = implementation
          .borrow_mut()
          .body
          .propagate_types(&mut base_context, errors);
        anything_changed |= changed;
      }
    }
    anything_changed
  }
  fn find_untyped(&mut self) -> Vec<TypedExp> {
    self
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
      .chain(self.top_level_vars.iter_mut().map(|v| {
        if let Some(value) = &mut v.value {
          value.find_untyped()
        } else {
          vec![]
        }
      }))
      .flatten()
      .collect()
  }
  pub fn validate_match_blocks(&self, errors: &mut ErrorLog) {
    for abstract_function in self.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(implementation) =
        &abstract_function.borrow().implementation
      {
        (**implementation)
          .borrow_mut()
          .body
          .validate_match_blocks(errors);
      }
    }
  }
  pub fn fully_infer_types(&mut self, errors: &mut ErrorLog) {
    loop {
      let did_type_states_change = self.propagate_types(errors);
      if !did_type_states_change {
        let untyped_expressions = self.find_untyped();
        return if untyped_expressions.is_empty() {
          break;
        } else {
          for exp in untyped_expressions {
            let source_trace = exp.source_trace.clone();
            errors.log(CompileError::new(CouldntInferTypes, source_trace));
          }
        };
      }
    }
  }
  pub fn validate_assignments(&mut self, errors: &mut ErrorLog) {
    for f in self.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(implementation) =
        &f.borrow().implementation
      {
        if let Err(e) =
          implementation.borrow_mut().body.validate_assignments(self)
        {
          errors.log(e);
        }
      }
    }
  }
  pub fn monomorphize(&mut self, errors: &mut ErrorLog) {
    let mut monomorphized_ctx = Program::default();
    for f in self.abstract_functions_iter() {
      if f.borrow().generic_args.is_empty() {
        if let FunctionImplementationKind::Composite(implementation) =
          &f.borrow().implementation
        {
          match implementation
            .borrow_mut()
            .body
            .monomorphize(&self, &mut monomorphized_ctx)
          {
            Ok(_) => {
              let mut new_f = (**f).borrow().clone();
              new_f.implementation =
                FunctionImplementationKind::Composite(implementation.clone());
              monomorphized_ctx
                .add_abstract_function(Rc::new(RefCell::new(new_f)));
            }
            Err(e) => errors.log(e),
          }
        }
      }
    }
    take(self, |old_ctx| {
      monomorphized_ctx.structs = old_ctx.structs;
      monomorphized_ctx.top_level_vars = old_ctx.top_level_vars;
      monomorphized_ctx.type_aliases = old_ctx.type_aliases;
      monomorphized_ctx
    });
  }
  pub fn inline_all_higher_order_arguments(&mut self, errors: &mut ErrorLog) {
    let changed = self.inline_higher_order_arguments(errors);
    if errors.is_empty() && changed {
      self.inline_all_higher_order_arguments(errors);
    }
  }
  pub fn inline_higher_order_arguments(
    &mut self,
    errors: &mut ErrorLog,
  ) -> bool {
    let mut changed = false;
    let mut inlined_ctx = Program::default();
    for f in self.abstract_functions_iter() {
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
            Err(e) => errors.log(e),
          }
        }
      }
    }
    take(self, |old_ctx| {
      inlined_ctx.structs = old_ctx.structs;
      inlined_ctx.top_level_vars = old_ctx.top_level_vars;
      inlined_ctx.type_aliases = old_ctx.type_aliases;
      inlined_ctx
    });
    changed
  }
  pub fn compile_to_wgsl(self) -> CompileResult<String> {
    let mut wgsl = String::new();
    for v in self.top_level_vars.iter() {
      wgsl += &v.clone().compile();
      wgsl += ";\n";
    }
    wgsl += "\n";
    let default_structs = built_in_structs();
    for s in self
      .structs
      .iter()
      .cloned()
      .filter(|s| !default_structs.contains(s))
    {
      if let Some(compiled_struct) =
        Rc::unwrap_or_clone(s).compile_if_non_generic(&self.structs)?
      {
        wgsl += &compiled_struct;
        wgsl += "\n\n";
      }
    }
    for f in self.abstract_functions_iter() {
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
      .abstract_functions
      .iter_mut()
      .map(|(_, fns)| fns.into_iter())
      .flatten()
    {
      if let FunctionImplementationKind::Composite(f) =
        &f.borrow().implementation
      {
        f.borrow_mut()
          .body
          .walk_mut::<()>(&mut |exp| {
            take(&mut exp.kind, |exp_kind| {
              if let ExpKind::Application(f, args) = exp_kind {
                if let ExpKind::Name(_) = &f.kind
                  && let Type::Function(x) = f.data.kind.unwrap_known()
                  && let Some(abstract_ancestor) = &x.abstract_ancestor
                  && abstract_ancestor.associative
                  && args.len() != 2
                {
                  let mut args_iter = args.into_iter();
                  let mut new_exp = args_iter.next().unwrap();
                  while let Some(next_arg) = args_iter.next() {
                    new_exp = Exp {
                      kind: ExpKind::Application(
                        f.clone(),
                        vec![new_exp, next_arg],
                      ),
                      data: exp.data.clone(),
                      source_trace: exp.source_trace.clone(),
                    };
                  }
                  new_exp.kind
                } else {
                  ExpKind::Application(f, args)
                }
              } else {
                exp_kind
              }
            });
            Ok(true)
          })
          .unwrap();
      }
    }
  }
  fn deshadow(&mut self, errors: &mut ErrorLog) {
    let globally_bound_names: Vec<Rc<str>> = self
      .top_level_vars
      .iter()
      .map(|v| Rc::clone(&v.name))
      .chain(
        self
          .abstract_functions
          .iter()
          .map(|(name, _)| Rc::clone(name)),
      )
      .collect();
    for (_, signatures) in self.abstract_functions.iter_mut() {
      for signature in signatures.iter_mut() {
        if let FunctionImplementationKind::Composite(exp) =
          &mut signature.borrow_mut().implementation
        {
          exp
            .borrow_mut()
            .body
            .deshadow(&globally_bound_names, errors);
        }
      }
    }
  }
  fn validate_associative_signatures(&self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if signature.associative
        && (signature.arg_types.len() != 2
          || signature.arg_types[0] != signature.arg_types[1]
          || signature.arg_types[0] != signature.return_type)
      {
        if let FunctionImplementationKind::Composite(implementation) =
          &signature.implementation
        {
          errors.log(CompileError {
            kind: CompileErrorKind::InvalidAssociativeSignature,
            source_trace: implementation.borrow().body.source_trace.clone(),
          });
        }
      }
    }
  }
  fn catch_duplicate_signatures(&self, errors: &mut ErrorLog) {
    for (name, signatures) in self.abstract_functions.iter() {
      let mut normalized_signatures: Vec<(Option<SourceTrace>, _)> = vec![];
      for signature in signatures {
        if let FunctionImplementationKind::Builtin
        | FunctionImplementationKind::Constructor =
          signature.borrow().implementation
        {
          let normalized = signature.borrow().normalized_signature();
          normalized_signatures.push((None, normalized));
        }
      }
      for signature in signatures {
        if let FunctionImplementationKind::Composite(f) =
          &signature.borrow().implementation
        {
          let source = f.borrow().body.source_trace.clone();
          let normalized = signature.borrow().normalized_signature();
          for (previous_signature, previous_normalized) in
            normalized_signatures.iter()
          {
            if *previous_normalized == normalized {
              if let Some(previous_source) = previous_signature {
                errors.log(CompileError {
                  kind: CompileErrorKind::DuplicateFunctionSignature(
                    name.to_string(),
                  ),
                  source_trace: source
                    .clone()
                    .insert_as_secondary(previous_source.clone()),
                });
              } else {
                errors.log(CompileError {
                  kind: CompileErrorKind::FunctionSignatureConflictsWithBuiltin(
                    name.to_string(),
                  ),
                  source_trace: source.clone(),
                });
              }
            }
          }
          normalized_signatures.push((Some(source), normalized));
        }
      }
    }
  }
  fn ensure_no_typeless_bindings(&self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(implementation) =
        &signature.implementation
      {
        implementation
          .borrow()
          .body
          .walk(&mut |exp| {
            match &exp.kind {
              ExpKind::Let(items, _) => {
                for (_, _, value) in items.iter() {
                  if TypeState::Known(Type::Unit) == value.data.kind {
                    errors.log(CompileError {
                      kind: CompileErrorKind::TypelessBinding,
                      source_trace: value.source_trace.clone(),
                    });
                  }
                }
              }
              _ => {}
            }
            Ok::<_, Never>(true)
          })
          .unwrap();
      }
    }
  }
  pub fn validate_control_flow(&mut self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        f.borrow().body.validate_control_flow(errors, 0);
      }
    }
  }
  pub fn deexpressionify(&mut self) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        f.borrow_mut().body.deexpressionify(self);
      }
    }
  }
  pub fn validate_raw_program(&mut self) -> ErrorLog {
    let mut errors = ErrorLog::new();
    self.validate_associative_signatures(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.deshadow(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.fully_infer_types(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_control_flow(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.ensure_no_typeless_bindings(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.expand_associative_applications();
    self.validate_assignments(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_duplicate_signatures(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.monomorphize(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_match_blocks(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.inline_all_higher_order_arguments(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.deexpressionify();
    errors
  }
  pub fn gather_type_annotations(&self) -> Vec<(SourceTrace, TypeState)> {
    let mut type_annotations = vec![];
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      let FunctionImplementationKind::Composite(implementation) =
        &signature.implementation
      else {
        continue;
      };
      implementation
        .borrow()
        .body
        .walk(&mut |exp: &TypedExp| {
          type_annotations
            .push((exp.source_trace.clone(), exp.data.kind.clone()));
          Ok::<_, Never>(true)
        })
        .unwrap();
    }
    type_annotations
  }
}
