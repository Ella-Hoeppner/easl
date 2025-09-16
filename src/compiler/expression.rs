use core::fmt::Debug;
use sse::syntax::EncloserOrOperator;
use std::{
  cell::RefCell,
  collections::{HashMap, HashSet},
  rc::Rc,
};
use take_mut::take;

use crate::{
  Never,
  compiler::{
    builtins::{
      ASSIGNMENT_OPS, INFIX_OPS, builtin_vec_constructor_type,
      get_builtin_struct, rename_builtin,
    },
    effects::EffectType,
    enums::AbstractEnum,
    error::{CompileError, CompileErrorKind::*, CompileResult, err},
    functions::{AbstractFunctionSignature, FunctionSignature},
    metadata::{Metadata, extract_metadata},
    program::{Program, TypeDefs},
    structs::{AbstractStruct, Struct},
    types::{
      ArraySize, ExpTypeInfo, Type,
      TypeState::{self, *},
      Variable, VariableKind, extract_type_annotation,
      extract_type_annotation_ast,
    },
    util::{compile_word, indent},
  },
  parse::EaslTree,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
  Int(i64),
  Float(f64),
}
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
  static ref NUM_REGEX: Regex =
    Regex::new(r"^(-?\d+\.?\d*|\.\d+)(i|u|f)?$").unwrap();
}

fn parse_number(num_str: &str, source_trace: SourceTrace) -> Option<TypedExp> {
  if let Some(captures) = NUM_REGEX.captures(num_str) {
    let num_str = captures.get(1).unwrap().as_str();
    let contains_decimal = num_str.contains('.');
    if let Some(suffix) = captures.get(2).map(|m| m.as_str()) {
      match suffix {
        "f" => {
          return Some(Exp {
            kind: ExpKind::NumberLiteral(Number::Float(
              num_str.parse::<f64>().unwrap(),
            )),
            data: Known(Type::F32).into(),
            source_trace,
          });
        }
        "i" | "u" => {
          if !contains_decimal {
            return Some(Exp {
              kind: ExpKind::NumberLiteral(Number::Int(
                num_str.parse::<i64>().unwrap(),
              )),
              data: Known(match suffix {
                "i" => Type::I32,
                "u" => Type::U32,
                _ => unreachable!(),
              })
              .into(),
              source_trace,
            });
          }
        }
        _ => unreachable!(),
      }
    } else {
      if contains_decimal {
        return Some(Exp {
          kind: ExpKind::NumberLiteral(Number::Float(
            num_str.parse::<f64>().unwrap(),
          )),
          data: Known(Type::F32).into(),
          source_trace,
        });
      } else {
        return Some(Exp {
          kind: ExpKind::NumberLiteral(Number::Int(
            num_str.parse::<i64>().unwrap(),
          )),
          data: OneOf(if num_str.contains('-') {
            vec![Type::F32, Type::I32]
          } else {
            vec![Type::F32, Type::I32, Type::U32]
          })
          .into(),
          source_trace,
        });
      }
    }
  }

  None
}

#[derive(Debug, Clone, PartialEq)]
pub struct Exp<D: Debug + Clone + PartialEq> {
  pub data: D,
  pub kind: ExpKind<D>,
  pub source_trace: SourceTrace,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SwizzleField {
  X,
  Y,
  Z,
  W,
}

impl SwizzleField {
  fn name(&self) -> &str {
    match self {
      SwizzleField::X => "x",
      SwizzleField::Y => "y",
      SwizzleField::Z => "z",
      SwizzleField::W => "w",
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Accessor {
  Field(Rc<str>),
  Swizzle(Vec<SwizzleField>),
}

pub fn swizzle_accessor_typestate(
  accessed_typestate: &ExpTypeInfo,
  fields: &Vec<SwizzleField>,
) -> ExpTypeInfo {
  let vec_struct = match fields.len() {
    2 => get_builtin_struct("vec2"),
    3 => get_builtin_struct("vec3"),
    4 => get_builtin_struct("vec4"),
    n => unreachable!("swizzle with {n} elements, expected 2-4"),
  };
  if let TypeState::Known(accessed_type) = &accessed_typestate.kind {
    if let Type::Struct(s) = accessed_type {
      if &*s.name != "vec" {
        TypeState::Known(Type::Struct(
          AbstractStruct::fill_generics_ordered(
            vec_struct.into(),
            vec![match &s.fields[0].field_type.kind {
              TypeState::UnificationVariable(uvar) => {
                TypeState::UnificationVariable(uvar.clone()).into()
              }
              TypeState::Known(inner_type) => {
                TypeState::Known(inner_type.clone()).into()
              }
              _ => unreachable!(),
            }],
            &TypeDefs::empty(),
            SourceTrace::empty(),
          )
          .unwrap(),
        ))
        .into()
      } else {
        unreachable!()
      }
    } else {
      unreachable!()
    }
  } else {
    TypeState::Unknown.into()
  }
}

pub fn swizzle_accessed_possibilities(
  fields: &Vec<SwizzleField>,
) -> ExpTypeInfo {
  let max_accessed_index = fields.iter().fold(0, |max_so_far, field| {
    max_so_far.max(match field {
      SwizzleField::X => 1,
      SwizzleField::Y => 2,
      SwizzleField::Z => 3,
      SwizzleField::W => 4,
    })
  });
  TypeState::OneOf(
    ["vec4", "vec3", "vec2"]
      .iter()
      .take(3.min(5 - max_accessed_index))
      .map(|name| {
        Type::Struct(
          AbstractStruct::fill_generics_with_unification_variables(
            get_builtin_struct(name).into(),
            &TypeDefs::empty(),
            SourceTrace::empty(),
          )
          .unwrap(),
        )
      })
      .collect::<Vec<Type>>(),
  )
  .simplified()
  .into()
}

impl Accessor {
  pub fn new(name: Rc<str>) -> Self {
    let chars: Vec<char> = name.chars().collect();
    (chars.len() >= 2 && chars.len() <= 4)
      .then(|| {
        use SwizzleField::*;
        chars
          .into_iter()
          .map(|char| match char {
            'x' => Some(X),
            'y' => Some(Y),
            'z' => Some(Z),
            'w' => Some(W),
            _ => None,
          })
          .collect::<Option<Vec<SwizzleField>>>()
      })
      .flatten()
      .map(|indeces| Self::Swizzle(indeces))
      .unwrap_or(Accessor::Field(name))
  }
  pub fn compile(self) -> Rc<str> {
    match self {
      Accessor::Field(field_name) => {
        format!(".{}", compile_word(field_name)).into()
      }
      Accessor::Swizzle(field_indeces) => std::iter::once(".")
        .chain(
          field_indeces
            .into_iter()
            .map(|index| ["x", "y", "z", "w"][index as usize]),
        )
        .collect::<Vec<&str>>()
        .join("")
        .into(),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpKind<D: Debug + Clone + PartialEq> {
  Wildcard,
  Unit,
  Name(Rc<str>),
  NumberLiteral(Number),
  BooleanLiteral(bool),
  Function(Vec<Rc<str>>, Box<Exp<D>>),
  Application(Box<Exp<D>>, Vec<Exp<D>>),
  Access(Accessor, Box<Exp<D>>),
  Let(Vec<(Rc<str>, VariableKind, Exp<D>)>, Box<Exp<D>>),
  Match(Box<Exp<D>>, Vec<(Exp<D>, Exp<D>)>),
  Block(Vec<Exp<D>>),
  ForLoop {
    increment_variable_name: Rc<str>,
    increment_variable_type: Type,
    increment_variable_initial_value_expression: Box<Exp<D>>,
    continue_condition_expression: Box<Exp<D>>,
    update_condition_expression: Box<Exp<D>>,
    body_expression: Box<Exp<D>>,
  },
  WhileLoop {
    condition_expression: Box<Exp<D>>,
    body_expression: Box<Exp<D>>,
  },
  Break,
  Continue,
  Discard,
  Return(Box<Exp<D>>),
  ArrayLiteral(Vec<Exp<D>>),
  Reference(Box<Exp<D>>),
  Uninitialized,
  ZeroedArray,
}
use ExpKind::*;

use super::{
  effects::Effect,
  error::{CompileErrorKind, ErrorLog, SourceTrace},
  functions::FunctionImplementationKind,
  structs::{compiled_vec_or_mat_name, vec_and_mat_compile_names},
  types::{
    AbstractType, ImmutableProgramLocalContext, LocalContext,
    MutableProgramLocalContext, TypeConstraint,
  },
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ExpressionCompilationPosition {
  Return,
  InnerLine,
  InnerExpression,
}

pub type TypedExp = Exp<ExpTypeInfo>;

pub fn arg_list_and_return_type_from_easl_tree(
  tree: EaslTree,
  typedefs: &TypeDefs,
  skolems: &Vec<Rc<str>>,
) -> CompileResult<(
  SourceTrace,
  Vec<Rc<str>>,
  Vec<AbstractType>,
  Vec<Option<Metadata>>,
  AbstractType,
  Option<Metadata>,
)> {
  use crate::parse::Encloser::*;
  use crate::parse::Operator::*;
  use sse::syntax::EncloserOrOperator::*;
  if let EaslTree::Inner(
    (position, Operator(TypeAnnotation)),
    mut args_and_return_type,
  ) = tree
  {
    let return_type_ast = args_and_return_type.remove(1);
    let mut errors = ErrorLog::new();
    let (return_type_ast, return_metadata) =
      extract_metadata(return_type_ast, &mut errors);
    if let Some(metadata_error) = errors.into_iter().next() {
      return Err(metadata_error.clone());
    }
    let return_type =
      AbstractType::from_easl_tree(return_type_ast, typedefs, skolems)?;
    if let EaslTree::Inner((position, Encloser(Square)), arg_asts) =
      args_and_return_type.remove(0)
    {
      let source_path: SourceTrace = position.into();
      let ((arg_types, arg_metadata), arg_names) = arg_asts
        .into_iter()
        .map(|arg| -> CompileResult<_> {
          let (maybe_t_ast, arg_name_ast) = extract_type_annotation_ast(arg);
          let t_ast = maybe_t_ast.ok_or(CompileError::new(
            FunctionArgMissingType,
            source_path.clone(),
          ))?;
          let t = AbstractType::from_easl_tree(t_ast, typedefs, skolems)?;
          let mut errors = ErrorLog::new();
          let (arg_name_ast, arg_metadata) =
            extract_metadata(arg_name_ast, &mut errors);
          if let Some(metadata_error) = errors.into_iter().next() {
            return Err(metadata_error.clone());
          }
          if let EaslTree::Leaf(_, arg_name) = arg_name_ast {
            Ok(((t, arg_metadata.map(|(a, _)| a)), arg_name.into()))
          } else {
            err(InvalidArgumentName, source_path.clone())
          }
        })
        .collect::<CompileResult<(
          (Vec<AbstractType>, Vec<Option<Metadata>>),
          Vec<Rc<str>>,
        )>>()?;
      Ok((
        source_path,
        arg_names,
        arg_types,
        arg_metadata,
        return_type,
        return_metadata.map(|(a, _)| a),
      ))
    } else {
      err(FunctionSignatureNotSquareBrackets, position.into())
    }
  } else {
    err(
      FunctionSignatureMissingReturnType,
      tree.position().clone().into(),
    )
  }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SyntaxTreeContext {
  Default,
  MatchPattern,
}

impl TypedExp {
  pub fn function_from_body_tree(
    source_trace: SourceTrace,
    body_trees: Vec<EaslTree>,
    return_type: ExpTypeInfo,
    arg_names: Vec<Rc<str>>,
    arg_types: Vec<(ExpTypeInfo, Vec<TypeConstraint>)>,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
  ) -> CompileResult<Self> {
    let mut body_exps = body_trees
      .into_iter()
      .map(|t| {
        Self::try_from_easl_tree(
          t,
          typedefs,
          skolems,
          SyntaxTreeContext::Default,
        )
      })
      .collect::<CompileResult<Vec<TypedExp>>>()?;
    let body = if body_exps.len() == 1 {
      body_exps.remove(0)
    } else {
      Exp {
        data: Unknown.into(),
        source_trace: source_trace.clone(),
        kind: ExpKind::Block(body_exps),
      }
    };
    Ok(Exp {
      data: Known(Type::Function(Box::new(FunctionSignature {
        abstract_ancestor: None,
        arg_types,
        return_type,
        mutated_args: vec![],
      })))
      .into(),
      kind: ExpKind::Function(arg_names, Box::new(body)),
      source_trace,
    })
  }
  pub fn try_from_easl_tree(
    tree: EaslTree,
    typedefs: &TypeDefs,
    skolems: &Vec<Rc<str>>,
    ctx: SyntaxTreeContext,
  ) -> CompileResult<Self> {
    Ok(match tree {
      EaslTree::Leaf(position, leaf) => {
        let leaf: Rc<str> = leaf.into();
        let source_trace: SourceTrace = position.into();
        if &*leaf == "break" {
          Exp {
            kind: ExpKind::Break,
            data: Known(Type::Unit).into(),
            source_trace,
          }
        } else if &*leaf == "continue" {
          Exp {
            kind: ExpKind::Continue,
            data: Known(Type::Unit).into(),
            source_trace,
          }
        } else if &*leaf == "discard" {
          Exp {
            kind: ExpKind::Discard,
            data: TypeState::fresh_unification_variable().into(),
            source_trace,
          }
        } else if &*leaf == "true" || &*leaf == "false" {
          Exp {
            kind: ExpKind::BooleanLiteral(&*leaf == "true"),
            data: Known(Type::Bool).into(),
            source_trace,
          }
        } else if let Some(num_exp) = parse_number(&leaf, source_trace.clone())
        {
          num_exp
        } else if &*leaf == "_".to_string() {
          Exp {
            kind: ExpKind::Wildcard,
            data: Unknown.into(),
            source_trace,
          }
        } else if leaf.contains('.') && leaf.chars().next() != Some('.') {
          let mut segments = leaf.split('.');
          if let Some(root_name) = segments.next() {
            segments.fold(
              Exp {
                kind: ExpKind::Name(root_name.into()),
                data: Unknown.into(),
                source_trace: source_trace.clone(),
              },
              |inner_expression, accessor_name| Exp {
                kind: ExpKind::Access(
                  Accessor::new(accessor_name.into()),
                  Box::new(inner_expression),
                ),
                data: Unknown.into(),
                source_trace: source_trace.clone(),
              },
            )
          } else {
            return err(InvalidToken(leaf.to_string()), source_trace);
          }
        } else {
          Exp {
            kind: ExpKind::Name(leaf),
            data: Unknown.into(),
            source_trace,
          }
        }
      }
      EaslTree::Inner((position, encloser_or_operator), children) => {
        use crate::parse::Encloser::*;
        use crate::parse::Operator::*;
        use sse::syntax::EncloserOrOperator::*;
        let encloser_or_operator_source_trace: SourceTrace = position.into();
        let mut children_iter = children.into_iter();
        match encloser_or_operator {
          Encloser(e) => match e {
            Parens => {
              if let Some(first_child) = children_iter.next() {
                match &first_child {
                  EaslTree::Leaf(position, first_child_name) => {
                    let source_trace: SourceTrace = position.clone().into();
                    if first_child_name.starts_with(".") {
                      if children_iter.len() == 1 {
                        Some(Exp {
                          data: Unknown.into(),
                          kind: ExpKind::Access(
                            Accessor::new(
                              first_child_name
                                .chars()
                                .skip(1)
                                .collect::<String>()
                                .into(),
                            ),
                            Box::new(Self::try_from_easl_tree(
                              children_iter.next().unwrap(),
                              typedefs,
                              skolems,
                              ctx,
                            )?),
                          ),
                          source_trace,
                        })
                      } else {
                        return err(AccessorHadMultipleArguments, source_trace);
                      }
                    } else {
                      match first_child_name.as_str() {
                        "fn" => {
                          return err(
                            AnonymousFunctionsNotYetSupported,
                            source_trace,
                          );
                        }
                        "let" => {
                          if children_iter.len() < 2 {
                            return err(
                              NotEnoughLetBlockChildren,
                              source_trace,
                            );
                          }
                          let bindings_ast = children_iter.next().unwrap();
                          let mut child_exps = children_iter
                            .clone()
                            .map(|child| {
                              Self::try_from_easl_tree(
                                child, typedefs, skolems, ctx,
                              )
                            })
                            .collect::<CompileResult<Vec<Self>>>()?;
                          let body_exp = if child_exps.len() == 1 {
                            child_exps.remove(0)
                          } else {
                            Exp {
                              data: Unknown.into(),
                              kind: ExpKind::Block(child_exps),
                              source_trace: encloser_or_operator_source_trace
                                .clone(),
                            }
                          };
                          if let EaslTree::Inner(
                            (_, Encloser(Square)),
                            binding_asts,
                          ) = bindings_ast
                          {
                            if binding_asts.len() % 2 == 0 {
                              let mut binding_asts_iter =
                                binding_asts.into_iter();
                              let mut bindings = vec![];
                              while let Some(name_ast) =
                                binding_asts_iter.next()
                              {
                                let value_ast =
                                  binding_asts_iter.next().unwrap();
                                let (ty, name_ast) = extract_type_annotation(
                                  name_ast, typedefs, skolems,
                                )?;
                                let mut errors = ErrorLog::new();
                                let (name_ast, name_metadata) =
                                  extract_metadata(name_ast, &mut errors);
                                if let Some(metadata_error) =
                                  errors.into_iter().next()
                                {
                                  return Err(metadata_error.clone());
                                }
                                match name_ast {
                                  EaslTree::Leaf(position, name) => {
                                    let source_trace = position.clone().into();
                                    bindings.push((
                                      name.into(),
                                      match name_metadata {
                                        None => VariableKind::Let,
                                        Some((Metadata::Singular(tag), _)) => {
                                          match &*tag {
                                            "var" => VariableKind::Var,
                                            _ => {
                                              return err(
                                                InvalidVariableMetadata(
                                                  Metadata::Singular(tag)
                                                    .into(),
                                                ),
                                                source_trace,
                                              );
                                            }
                                          }
                                        }
                                        Some((
                                          metadata,
                                          metadata_source_trace,
                                        )) => {
                                          return err(
                                            InvalidVariableMetadata(
                                              metadata.into(),
                                            ),
                                            metadata_source_trace,
                                          );
                                        }
                                      },
                                      {
                                        let mut value_exp =
                                          Self::try_from_easl_tree(
                                            value_ast, typedefs, skolems, ctx,
                                          )?;
                                        if let Some(ty) = ty {
                                          value_exp.data =
                                            TypeState::Known(ty.concretize(
                                              skolems,
                                              typedefs,
                                              source_trace.clone(),
                                            )?)
                                            .into();
                                        }
                                        value_exp
                                      },
                                    ));
                                  }
                                  EaslTree::Inner((position, _), _) => {
                                    return err(
                                      ExpectedBindingName,
                                      position.into(),
                                    );
                                  }
                                }
                              }
                              Some(Exp {
                                data: Unknown.into(),
                                kind: ExpKind::Let(
                                  bindings,
                                  Box::new(body_exp),
                                ),
                                source_trace: encloser_or_operator_source_trace
                                  .clone(),
                              })
                            } else {
                              return err(
                                OddNumberOfChildrenInLetBindings,
                                source_trace,
                              );
                            }
                          } else {
                            return err(
                              LetBindingsNotSquareBracketed,
                              source_trace,
                            );
                          }
                        }
                        "do" => {
                          if children_iter.len() == 0 {
                            return err(EmptyBlock, source_trace);
                          }
                          let child_exps = children_iter
                            .clone()
                            .map(|child| {
                              Self::try_from_easl_tree(
                                child, typedefs, skolems, ctx,
                              )
                            })
                            .collect::<CompileResult<Vec<Self>>>()?;
                          Some(Exp {
                            data: Unknown.into(),
                            kind: ExpKind::Block(child_exps),
                            source_trace,
                          })
                        }
                        "match" => {
                          let scrutinee = Self::try_from_easl_tree(
                            children_iter.next().ok_or_else(|| {
                              CompileError::new(
                                MatchMissingScrutinee,
                                source_trace.clone(),
                              )
                            })?,
                            typedefs,
                            skolems,
                            ctx,
                          )?;
                          let mut arms = vec![];
                          while let Some(pattern_subtree) = children_iter.next()
                          {
                            let value_subtree =
                              children_iter.next().ok_or(CompileError::new(
                                MatchIncompleteArm,
                                source_trace.clone(),
                              ))?;
                            arms.push((
                              Self::try_from_easl_tree(
                                pattern_subtree,
                                typedefs,
                                skolems,
                                SyntaxTreeContext::MatchPattern,
                              )?,
                              Self::try_from_easl_tree(
                                value_subtree,
                                typedefs,
                                skolems,
                                ctx,
                              )?,
                            ));
                          }
                          if arms.is_empty() {
                            return err(MatchMissingArms, source_trace);
                          }
                          Some(Exp {
                            kind: Match(Box::new(scrutinee), arms),
                            data: Unknown.into(),
                            source_trace,
                          })
                        }
                        "for" => {
                          let header_tree =
                            children_iter.next().ok_or_else(|| {
                              CompileError::new(
                                InvalidForLoopHeader,
                                source_trace.clone(),
                              )
                            })?;
                          let (
                            increment_variable_name,
                            increment_variable_type,
                            increment_variable_initial_value_expression,
                            continue_condition_expression,
                            update_condition_expression,
                          ) = if let EaslTree::Inner(
                            (
                              header_source_position,
                              EncloserOrOperator::Encloser(Square),
                            ),
                            mut header_subtrees,
                          ) = header_tree
                          {
                            if header_subtrees.len() == 4 {
                              let var_name_subtree = header_subtrees.remove(0);
                              let increment_variable_initial_value_subtree =
                                header_subtrees.remove(0);
                              let continue_condition_subtree =
                                header_subtrees.remove(0);
                              let update_condition_subtree =
                                header_subtrees.remove(0);
                              let (var_type_subtree, var_name_subtree) =
                                extract_type_annotation_ast(var_name_subtree);
                              (
                                if let EaslTree::Leaf(_, name) =
                                  var_name_subtree
                                {
                                  name.into()
                                } else {
                                  return Err(CompileError::new(
                                    InvalidForLoopHeader,
                                    header_source_position.into(),
                                  ));
                                },
                                var_type_subtree
                                  .map(|var_type_subtree| {
                                    Type::from_easl_tree(
                                      var_type_subtree,
                                      typedefs,
                                      skolems,
                                    )
                                  })
                                  .unwrap_or(Ok(Type::I32))?,
                                TypedExp::try_from_easl_tree(
                                  increment_variable_initial_value_subtree,
                                  typedefs,
                                  skolems,
                                  ctx,
                                )?,
                                TypedExp::try_from_easl_tree(
                                  continue_condition_subtree,
                                  typedefs,
                                  skolems,
                                  ctx,
                                )?,
                                TypedExp::try_from_easl_tree(
                                  update_condition_subtree,
                                  typedefs,
                                  skolems,
                                  ctx,
                                )?,
                              )
                            } else {
                              return Err(CompileError::new(
                                InvalidForLoopHeader,
                                header_source_position.into(),
                              ));
                            }
                          } else {
                            return Err(CompileError::new(
                              InvalidForLoopHeader,
                              source_trace,
                            ));
                          };
                          let body_expressions = children_iter
                            .clone()
                            .map(|child| {
                              TypedExp::try_from_easl_tree(
                                child, typedefs, skolems, ctx,
                              )
                            })
                            .collect::<CompileResult<Vec<TypedExp>>>()?;
                          Some(Exp {
                            kind: ForLoop {
                              increment_variable_name,
                              increment_variable_type,
                              increment_variable_initial_value_expression:
                                Box::new(
                                  increment_variable_initial_value_expression,
                                ),
                              continue_condition_expression: Box::new(
                                continue_condition_expression,
                              ),
                              update_condition_expression: Box::new(
                                update_condition_expression,
                              ),
                              body_expression: Box::new(TypedExp {
                                data: TypeState::Known(Type::Unit).into(),
                                kind: ExpKind::Block(body_expressions),
                                source_trace: source_trace.clone(),
                              }),
                            },
                            data: Known(Type::Unit).into(),
                            source_trace,
                          })
                        }
                        "while" => {
                          let mut sub_expressions = children_iter
                            .clone()
                            .map(|child| {
                              TypedExp::try_from_easl_tree(
                                child, typedefs, skolems, ctx,
                              )
                            })
                            .collect::<CompileResult<Vec<TypedExp>>>()?;
                          if sub_expressions.len() < 1 {
                            return Err(CompileError::new(
                              InvalidWhileLoop,
                              source_trace,
                            ));
                          }
                          let condition_expression = sub_expressions.remove(0);
                          Some(Exp {
                            kind: ExpKind::WhileLoop {
                              condition_expression: Box::new(
                                condition_expression,
                              ),
                              body_expression: Box::new(TypedExp {
                                data: TypeState::Known(Type::Unit).into(),
                                kind: ExpKind::Block(sub_expressions),
                                source_trace: source_trace.clone(),
                              }),
                            },
                            data: Known(Type::Unit).into(),
                            source_trace,
                          })
                        }
                        "return" => {
                          if children_iter.len() == 1 {
                            let exp = TypedExp::try_from_easl_tree(
                              children_iter.next().unwrap(),
                              typedefs,
                              skolems,
                              ctx,
                            )?;
                            Some(Exp {
                              kind: ExpKind::Return(Box::new(exp)),
                              data: TypeState::Unknown.into(),
                              source_trace,
                            })
                          } else {
                            return Err(CompileError::new(
                              InvalidReturn,
                              source_trace,
                            ));
                          }
                        }
                        "zeroed-array" => {
                          if children_iter.len() == 0 {
                            Some(TypedExp {
                              data: TypeState::Known(Type::Array(
                                None,
                                Box::new(TypeState::Unknown.into()),
                              ))
                              .into(),
                              kind: ExpKind::ZeroedArray,
                              source_trace,
                            })
                          } else {
                            return Err(CompileError::new(
                              ZeroedArrayShouldntHaveChildren,
                              source_trace,
                            ));
                          }
                        }
                        _ => None,
                      }
                    }
                  }
                  _ => None,
                }
                .unwrap_or(Exp {
                  kind: Application(
                    Box::new(Self::try_from_easl_tree(
                      first_child,
                      typedefs,
                      skolems,
                      ctx,
                    )?),
                    children_iter
                      .map(|arg| {
                        Self::try_from_easl_tree(arg, typedefs, skolems, ctx)
                      })
                      .collect::<CompileResult<_>>()?,
                  ),
                  data: Unknown.into(),
                  source_trace: encloser_or_operator_source_trace,
                })
              } else {
                Exp {
                  data: TypeState::Known(Type::Unit).into(),
                  kind: ExpKind::Unit,
                  source_trace: encloser_or_operator_source_trace,
                }
              }
            }
            Square => Exp {
              data: TypeState::Known(Type::Array(
                Some(ArraySize::Literal(children_iter.len() as u32)),
                Box::new(TypeState::Unknown.into()),
              ))
              .into(),
              kind: ArrayLiteral(
                children_iter
                  .map(|ast| {
                    TypedExp::try_from_easl_tree(ast, typedefs, skolems, ctx)
                  })
                  .collect::<CompileResult<Vec<TypedExp>>>()?,
              ),
              source_trace: encloser_or_operator_source_trace,
            },
            Curly => {
              return err(
                AnonymousStructsNotYetSupported,
                encloser_or_operator_source_trace,
              );
            }
            LineComment => {
              return err(
                EncounteredCommentInSource,
                encloser_or_operator_source_trace,
              );
            }
            BlockComment => {
              return err(
                EncounteredCommentInSource,
                encloser_or_operator_source_trace,
              );
            }
          },
          Operator(o) => match o {
            MetadataAnnotation => {
              return err(
                MetadataNotAllowed,
                encloser_or_operator_source_trace,
              );
            }
            TypeAnnotation => {
              let mut exp = Self::try_from_easl_tree(
                children_iter.next().unwrap(),
                typedefs,
                skolems,
                ctx,
              )?;
              exp.data = TypeState::Known(Type::from_easl_tree(
                children_iter.next().unwrap(),
                typedefs,
                skolems,
              )?)
              .into();
              exp
            }
            ExpressionComment => unreachable!(
              "expression comment encountered, this should have been stripped"
            ),
            Reference => TypedExp {
              data: TypeState::Known(Type::Reference(Box::new(
                TypeState::Unknown.into(),
              )))
              .into(),
              kind: ExpKind::Reference(
                Self::try_from_easl_tree(
                  children_iter.next().unwrap(),
                  typedefs,
                  skolems,
                  ctx,
                )?
                .into(),
              ),
              source_trace: encloser_or_operator_source_trace,
            },
          },
        }
      }
    })
  }
  pub fn walk<E>(
    &self,
    prewalk_handler: &mut impl FnMut(&Self) -> Result<bool, E>,
  ) -> Result<(), E> {
    if prewalk_handler(self)? {
      match &self.kind {
        Application(f, args) => {
          f.walk(prewalk_handler)?;
          for arg in args.iter() {
            arg.walk(prewalk_handler)?;
          }
        }
        Function(_, body) => body.walk(prewalk_handler)?,
        Access(_, body) => body.walk(prewalk_handler)?,
        Let(bindings, body) => {
          for (_, _, value) in bindings {
            value.walk(prewalk_handler)?;
          }
          body.walk(prewalk_handler)?;
        }
        Match(scrutinee, arms) => {
          scrutinee.walk(prewalk_handler)?;
          for (pattern, value) in arms {
            pattern.walk(prewalk_handler)?;
            value.walk(prewalk_handler)?;
          }
        }
        Block(expressions) => {
          for subexp in expressions {
            subexp.walk(prewalk_handler)?;
          }
        }
        ForLoop {
          increment_variable_initial_value_expression,
          continue_condition_expression,
          update_condition_expression,
          body_expression,
          ..
        } => {
          increment_variable_initial_value_expression.walk(prewalk_handler)?;
          continue_condition_expression.walk(prewalk_handler)?;
          update_condition_expression.walk(prewalk_handler)?;
          body_expression.walk(prewalk_handler)?;
        }
        WhileLoop {
          condition_expression,
          body_expression,
        } => {
          condition_expression.walk(prewalk_handler)?;
          body_expression.walk(prewalk_handler)?;
        }
        Return(exp) => exp.walk(prewalk_handler)?,
        ArrayLiteral(children) => {
          for child in children {
            child.walk(prewalk_handler)?;
          }
        }
        _ => {}
      }
    }
    Ok(())
  }
  pub fn walk_mut<E>(
    &mut self,
    prewalk_handler: &mut impl FnMut(&mut Self) -> Result<bool, E>,
  ) -> Result<(), E> {
    if prewalk_handler(self)? {
      match &mut self.kind {
        Application(f, args) => {
          f.walk_mut(prewalk_handler)?;
          for arg in args.iter_mut() {
            arg.walk_mut(prewalk_handler)?;
          }
        }
        Function(_, body) => body.walk_mut(prewalk_handler)?,
        Access(_, body) => body.walk_mut(prewalk_handler)?,
        Let(bindings, body) => {
          for (_, _, value) in bindings {
            value.walk_mut(prewalk_handler)?;
          }
          body.walk_mut(prewalk_handler)?;
        }
        Match(scrutinee, arms) => {
          scrutinee.walk_mut(prewalk_handler)?;
          for (pattern, value) in arms {
            pattern.walk_mut(prewalk_handler)?;
            value.walk_mut(prewalk_handler)?;
          }
        }
        Block(expressions) => {
          for subexp in expressions {
            subexp.walk_mut(prewalk_handler)?;
          }
        }
        ForLoop {
          increment_variable_initial_value_expression,
          continue_condition_expression,
          update_condition_expression,
          body_expression,
          ..
        } => {
          increment_variable_initial_value_expression
            .walk_mut(prewalk_handler)?;
          continue_condition_expression.walk_mut(prewalk_handler)?;
          update_condition_expression.walk_mut(prewalk_handler)?;
          body_expression.walk_mut(prewalk_handler)?;
        }
        WhileLoop {
          condition_expression,
          body_expression,
        } => {
          condition_expression.walk_mut(prewalk_handler)?;
          body_expression.walk_mut(prewalk_handler)?;
        }
        Return(exp) => exp.walk_mut(prewalk_handler)?,
        ArrayLiteral(children) => {
          for child in children {
            child.walk_mut(prewalk_handler)?;
          }
        }
        _ => {}
      }
    }
    Ok(())
  }
  pub fn compile(self, position: ExpressionCompilationPosition) -> String {
    use ExpressionCompilationPosition::*;
    let wrap = |s: String| -> String {
      match position {
        Return => format!("\nreturn {s};"),
        InnerLine => format!("\n{s};"),
        InnerExpression => s,
      }
    };
    match self.kind {
      Wildcard => panic!("compiling wildcard"),
      Unit => panic!("compiling unit"),
      Uninitialized => panic!("compiling Uninitialized"),
      Name(name) => wrap(compile_word(name)),
      NumberLiteral(num) => wrap(match num {
        Number::Int(i) => {
          format!(
            "{i}{}",
            match self.data.kind {
              Known(Type::I32) => "",
              Known(Type::U32) => "u",
              Known(Type::F32) => "f",
              _ => panic!("{:?}", self.data.kind),
            }
          )
        }
        Number::Float(f) => format!("{f}f"),
      }),
      BooleanLiteral(b) => wrap(format!("{b}")),
      Function(_, _) => panic!("Attempting to compile internal function"),
      Application(f, mut args) => wrap(match f.data.unwrap_known() {
        Type::Function(_) => {
          let ExpKind::Name(name) = f.kind else {
            panic!("tried to compile application of non-name fn");
          };
          let f_str = rename_builtin(&*name).unwrap_or_else(|| {
            if ASSIGNMENT_OPS.contains(name.as_ref())
              || INFIX_OPS.contains(name.as_ref())
            {
              name.to_string()
            } else {
              compile_word(name)
            }
          });
          let arg_types = args
            .iter()
            .map(|a| a.data.unwrap_known())
            .collect::<Vec<_>>();
          let arg_strs: Vec<String> = args
            .into_iter()
            .map(|arg| arg.compile(InnerExpression))
            .collect();
          if ASSIGNMENT_OPS.contains(&f_str.as_str()) {
            if arg_strs.len() == 2 {
              format!("{} {} {}", arg_strs[0], f_str, arg_strs[1])
            } else {
              panic!(
                "{} arguments to assignment op, expected 2",
                arg_strs.len()
              )
            }
          } else if INFIX_OPS.contains(&f_str.as_str()) {
            if arg_strs.len() == 2 {
              format!("({} {} {})", arg_strs[0], f_str, arg_strs[1])
            } else if arg_strs.len() == 1 && &f_str == "-" {
              format!("(-{})", arg_strs[0])
            } else if arg_strs.len() == 1 && &f_str == "/" {
              format!("(1. / {})", arg_strs[0])
            } else {
              panic!("{} arguments to infix op, expected 2", arg_strs.len())
            }
          } else if let Some(wrapper_type) =
            builtin_vec_constructor_type(&f_str)
          {
            let wrapped_arg_strs = arg_strs
              .into_iter()
              .zip(arg_types)
              .map(|(s, t)| {
                if let Some(wrapper) = match t {
                  Type::F32 | Type::I32 | Type::U32 => {
                    Some(format!("{wrapper_type}32"))
                  }
                  Type::Struct(Struct { name, .. }) => match &*name {
                    "vec2" | "vec3" | "vec4" => {
                      Some(format!("{name}{wrapper_type}"))
                    }
                    _ => None,
                  },
                  _ => None,
                } {
                  format!("{wrapper}({s})")
                } else {
                  s
                }
              })
              .collect::<Vec<_>>();
            let args_str = wrapped_arg_strs.join(", ");
            format!("{f_str}({args_str})")
          } else {
            let args_str = arg_strs.join(", ");
            format!("{f_str}({args_str})")
          }
        }
        Type::Array(_, _) => {
          let f_str = f.compile(ExpressionCompilationPosition::InnerExpression);
          let index_str = args
            .remove(0)
            .compile(ExpressionCompilationPosition::InnerExpression);
          format!("{f_str}[{index_str}]")
        }
        _ => panic!("tried to compile application of non-fn, non-array"),
      }),
      Access(accessor, subexp) => wrap(format!(
        "{}{}",
        subexp.compile(InnerExpression),
        accessor.compile()
      )),
      Let(bindings, body) => {
        let binding_lines: Vec<String> = bindings
          .into_iter()
          .map(|(name, variable_kind, value_exp)| {
            if value_exp.kind == ExpKind::Uninitialized {
              format!(
                "{} {}: {};",
                variable_kind.compile(),
                compile_word(name),
                value_exp.data.compile(),
              )
            } else {
              format!(
                "{} {}: {} = {};",
                variable_kind.compile(),
                compile_word(name),
                value_exp.data.compile(),
                value_exp.compile(InnerExpression)
              )
            }
          })
          .collect();
        let value_line = body.compile(position);
        format!("\n{}{}", binding_lines.join("\n"), value_line)
      }
      Match(scrutinee, arms) => match scrutinee.data.unwrap_known() {
        Type::Bool => match arms.len() {
          1 => {
            let (pattern, case) = arms.into_iter().next().unwrap();
            let compiled_case = case.compile(position);
            match pattern.kind {
              ExpKind::Wildcard => compiled_case,
              ExpKind::BooleanLiteral(true) => format!(
                "\nif ({}) {{{}\n}}",
                scrutinee.compile(InnerExpression),
                indent(compiled_case),
              ),
              ExpKind::BooleanLiteral(false) => format!(
                "\nif (!{}) {{{}\n}}",
                scrutinee.compile(InnerExpression),
                indent(compiled_case),
              ),
              _ => unreachable!(),
            }
          }
          2 => {
            let mut true_case = None;
            let mut false_case = None;
            let mut wildcard_case = None;
            for (pattern, case) in arms {
              if pattern.kind == ExpKind::Wildcard {
                wildcard_case = Some(case);
              } else if pattern.kind == ExpKind::BooleanLiteral(true) {
                true_case = Some(case);
              } else if pattern.kind == ExpKind::BooleanLiteral(false) {
                false_case = Some(case);
              } else {
                unreachable!()
              }
            }
            let (true_case, false_case) = if let Some(true_case) = true_case {
              (true_case, false_case.or(wildcard_case).unwrap())
            } else {
              (true_case.or(wildcard_case).unwrap(), false_case.unwrap())
            };
            let (true_case, false_case) = (
              if true_case.kind == ExpKind::Unit {
                indent("\n".to_string())
              } else {
                indent(true_case.compile(position))
              },
              if false_case.kind == ExpKind::Unit {
                indent("\n".to_string())
              } else {
                indent(false_case.compile(position))
              },
            );
            let condition = scrutinee.compile(InnerExpression);
            if position == InnerExpression {
              format!("select({false_case}, {true_case}, {condition})")
            } else {
              format!(
                "\nif ({condition}) {{{true_case}\n}} \
                  else {{{false_case}\n}}",
              )
            }
          }
          _ => unreachable!(),
        },
        Type::Enum(_) => {
          let ExpKind::Name(scrutinee_name) = &scrutinee.kind else {
            panic!("scrutinee wasn't name in enum match block")
          };
          let arm_count = arms.len();
          format!(
            "\nswitch({}.discriminant) {{\n  {}\n}}",
            scrutinee.clone().compile(InnerExpression),
            indent(
              arms
                .into_iter()
                .enumerate()
                .map(|(i, (pattern, value))| {
                  let finisher =
                    if position == ExpressionCompilationPosition::Return {
                      ""
                    } else {
                      "  break;\n"
                    };
                  match pattern.kind {
                    Wildcard => format!(
                      "default: {{{}\n{finisher}}}",
                      indent(value.compile(position))
                    ),
                    ExpKind::Name(name) => {
                      let Type::Enum(e) = scrutinee.data.unwrap_known() else {
                        panic!("invalid pattern type in enum match block")
                      };
                      let suffix = e
                        .abstract_ancestor
                        .original_ancestor()
                        .monomorphized_suffix(
                          &e.variants
                            .iter()
                            .map(|variant| variant.inner_type.unwrap_known())
                            .collect(),
                        );
                      let Some(discriminant) = e
                        .abstract_ancestor
                        .original_ancestor()
                        .variants
                        .iter()
                        .enumerate()
                        .find_map(|(i, variant)| {
                          ((variant.name.to_string() + &suffix) == &*name)
                            .then(|| i)
                        })
                      else {
                        panic!(
                          "invalid enum unit-variant name in match pattern"
                        )
                      };
                      format!(
                        "{}: {{\n  {}\n{finisher}}}",
                        if i == arm_count - 1 {
                          "default".to_string()
                        } else {
                          format!("case {discriminant}u")
                        },
                        indent(value.compile(position))
                      )
                    }
                    ExpKind::Application(mut f, mut args) => {
                      let Some((f, _, inner_value_name)) =
                        Self::try_deconstruct_enum_pattern(&mut f, &mut args)
                      else {
                        panic!("invalid pattern type in enum match block")
                      };
                      let FunctionImplementationKind::EnumConstructor(
                        variant_name,
                      ) = &f.abstract_ancestor.clone().unwrap().implementation
                      else {
                        panic!("invalid pattern type in enum match block")
                      };
                      let Type::Enum(e) = scrutinee.data.unwrap_known() else {
                        panic!("invalid pattern type in enum match block")
                      };
                      let Some((discriminant, variant)) = e
                        .variants
                        .iter()
                        .enumerate()
                        .find(|(_, variant)| (variant.name == *variant_name))
                      else {
                        panic!("invalid pattern type in enum match block")
                      };
                      let bitcasted_value = variant
                        .inner_type
                        .unwrap_known()
                        .bitcasted_from_enum_data(scrutinee_name.clone(), &e);
                      format!(
                        "{}: {{\n  let {} = {};{}\n{finisher}}}",
                        if i == arm_count - 1 {
                          "default".to_string()
                        } else {
                          format!("case {discriminant}u")
                        },
                        compile_word(inner_value_name),
                        bitcasted_value.compile(
                          ExpressionCompilationPosition::InnerExpression
                        ),
                        indent(value.compile(position))
                      )
                    }
                    _ => panic!("invalid pattern type in enum match block"),
                  }
                })
                .collect::<Vec<String>>()
                .join("\n")
            )
          )
        }
        _ => {
          format!(
            "\nswitch ({}) {{\n  {}\n}}",
            scrutinee.compile(InnerExpression),
            indent(
              arms
                .into_iter()
                .map(|(pattern, value)| format!(
                  "{}: {{{}\n{}}}",
                  if pattern.kind == Wildcard {
                    "default".to_string()
                  } else {
                    "case ".to_string() + &pattern.compile(InnerExpression)
                  },
                  if value.kind == ExpKind::Unit {
                    "".to_string()
                  } else {
                    indent(value.compile(position))
                  },
                  if position == ExpressionCompilationPosition::Return {
                    ""
                  } else {
                    "  break;\n"
                  }
                ))
                .collect::<Vec<String>>()
                .join("\n")
            )
          )
        }
      },
      Block(expressions) => {
        let child_count = expressions.len();
        let child_strings: Vec<String> = expressions
          .into_iter()
          .enumerate()
          .map(|(i, child)| {
            child.compile(if i == child_count - 1 {
              position
            } else {
              ExpressionCompilationPosition::InnerLine
            })
          })
          .collect();
        format!("\n{{{}\n}}", indent(child_strings.join("")))
      }
      ForLoop {
        increment_variable_name,
        increment_variable_type,
        increment_variable_initial_value_expression,
        continue_condition_expression,
        update_condition_expression,
        body_expression,
      } => format!(
        "\nfor (var {}: {} = {}; {}; {}) {{{}\n}}",
        increment_variable_name,
        increment_variable_type.compile(),
        increment_variable_initial_value_expression
          .compile(ExpressionCompilationPosition::InnerExpression),
        continue_condition_expression
          .compile(ExpressionCompilationPosition::InnerExpression),
        update_condition_expression
          .compile(ExpressionCompilationPosition::InnerExpression),
        indent(
          body_expression.compile(ExpressionCompilationPosition::InnerLine)
        )
      ),
      WhileLoop {
        condition_expression,
        body_expression,
      } => format!(
        "\nwhile ({}) {{{}\n}}",
        condition_expression
          .compile(ExpressionCompilationPosition::InnerExpression),
        indent(
          body_expression.compile(ExpressionCompilationPosition::InnerLine)
        )
      ),
      Break => "\nbreak;".to_string(),
      Continue => "\ncontinue;".to_string(),
      Discard => "\ndiscard;".to_string(),
      ExpKind::Return(exp) => format!(
        "\nreturn {};",
        exp.compile(ExpressionCompilationPosition::InnerExpression)
      ),
      ArrayLiteral(children) => format!(
        "array({})",
        children
          .into_iter()
          .map(|child| child.compile(position))
          .collect::<Vec<String>>()
          .join(", ")
      ),
      Reference(exp) => format!(
        "&{}",
        exp.compile(ExpressionCompilationPosition::InnerExpression)
      ),
      ZeroedArray => self.data.kind.compile() + "()",
    }
  }

  pub fn find_untyped(&mut self) -> Vec<SourceTrace> {
    let mut untyped = vec![];
    self
      .walk_mut::<()>(&mut |exp| {
        if !exp.data.is_fully_known() {
          untyped.push(exp.source_trace.clone());
        }
        Ok(true)
      })
      .unwrap();
    untyped
  }
  pub fn validate_match_blocks(&mut self, errors: &mut ErrorLog) {
    self
      .walk_mut(&mut |exp| {
        match &mut exp.kind {
          Match(exp, items) => {
            if let TypeState::Known(t) = &exp.data.kind {
              let mut first_wildcard: Option<SourceTrace> = None;
              let mut all_pattern_values: Vec<&ExpKind<ExpTypeInfo>> = vec![];
              for (pattern, _) in items.iter_mut() {
                if let Some(first_wildcard) = &first_wildcard {
                  errors.log(CompileError {
                    kind: CompileErrorKind::PatternAfterWildcard,
                    source_trace: first_wildcard
                      .clone()
                      .insert_as_secondary(pattern.source_trace.clone()),
                  })
                }
                if all_pattern_values.contains(&&pattern.kind) {
                  errors.log(CompileError {
                    kind: CompileErrorKind::DuplicatePattern,
                    source_trace: pattern.source_trace.clone(),
                  })
                } else {
                  if !match &mut pattern.kind {
                    Wildcard => {
                      if first_wildcard.is_none() {
                        first_wildcard = Some(pattern.source_trace.clone())
                      }
                      true
                    }
                    NumberLiteral(_) | BooleanLiteral(_) | Name(_) => true,
                    Application(f, args) => {
                      Self::try_deconstruct_enum_pattern(f, args).is_some()
                    }
                    _ => false,
                  } {
                    errors.log(CompileError {
                      kind: CompileErrorKind::InvalidMatchPattern,
                      source_trace: pattern.source_trace.clone(),
                    })
                  }
                  all_pattern_values.push(&pattern.kind);
                }
              }
              if first_wildcard.is_none() {
                let valid = match t {
                  Type::Bool => {
                    all_pattern_values.len() == 2
                      && all_pattern_values
                        .iter()
                        .find(|value| {
                          if let ExpKind::BooleanLiteral(true) = value {
                            true
                          } else {
                            false
                          }
                        })
                        .is_some()
                      && all_pattern_values
                        .iter()
                        .find(|value| {
                          if let ExpKind::BooleanLiteral(false) = value {
                            true
                          } else {
                            false
                          }
                        })
                        .is_some()
                  }
                  Type::Enum(e) => {
                    all_pattern_values.len() == e.variants.len()
                      && e
                        .variants
                        .iter()
                        .find(|variant| {
                          all_pattern_values
                            .iter()
                            .find(|pattern_value| {
                              if variant.inner_type.unwrap_known() == Type::Unit
                              {
                                if let ExpKind::Name(name) = pattern_value {
                                  *name == variant.name
                                } else {
                                  false
                                }
                              } else {
                                if let ExpKind::Application(f, _) =
                                  pattern_value
                                  && let ExpKind::Name(f_name) = &f.kind
                                {
                                  *f_name == variant.name
                                } else {
                                  false
                                }
                              }
                            })
                            .is_none()
                        })
                        .is_none()
                  }
                  _ => false,
                };
                if !valid {
                  errors.log(CompileError {
                    kind: CompileErrorKind::NonexhaustiveMatch,
                    source_trace: exp.source_trace.clone(),
                  });
                }
              }
            }
          }
          _ => {}
        }
        Ok::<_, Never>(true)
      })
      .unwrap();
  }
  fn try_deconstruct_enum_pattern<'a>(
    f: &'a mut Box<Exp<ExpTypeInfo>>,
    args: &'a mut Vec<Exp<ExpTypeInfo>>,
  ) -> Option<(
    &'a mut Box<FunctionSignature>,
    &'a mut Exp<ExpTypeInfo>,
    Rc<str>,
  )> {
    if let TypeState::Known(Type::Function(f)) = &mut *f.data
      && let Some(abstract_f) = &f.abstract_ancestor
      && let FunctionImplementationKind::EnumConstructor(_) =
        &abstract_f.implementation
      && args.len() == 1
      && let arg = &mut args[0]
      && let ExpKind::Name(inner_value_name) = &arg.kind
    {
      let inner_value_name = inner_value_name.clone();
      return Some((f, arg, inner_value_name));
    }
    None
  }
  fn propagate_types_inner(
    &mut self,
    ctx: &mut MutableProgramLocalContext,
    errors: &mut ErrorLog,
  ) -> bool {
    if self.data.subtree_fully_typed {
      return false;
    }
    let changed = match &mut self.kind {
      Uninitialized => {
        self.data.subtree_fully_typed = true;
        false
      }
      Wildcard => {
        self.data.subtree_fully_typed = true;
        if !ctx.inside_pattern {
          errors.log(CompileError::new(
            WildcardOutsidePattern,
            self.source_trace.clone(),
          ))
        }
        false
      }
      Unit => {
        self.data.subtree_fully_typed = true;
        self.data.constrain(
          TypeState::Known(Type::Unit),
          &self.source_trace,
          errors,
        )
      }
      Name(name) => {
        self.data.subtree_fully_typed = true;
        if !ctx.is_bound(name) {
          errors.log(CompileError::new(
            UnboundName(name.to_string()),
            self.source_trace.clone(),
          ));
        }
        let changed = ctx.constrain_name_type(
          name,
          &self.source_trace,
          &mut self.data,
          errors,
        );
        changed
      }
      NumberLiteral(num) => {
        self.data.subtree_fully_typed = true;
        let changed = self.data.constrain(
          match num {
            Number::Int(_) => {
              TypeState::OneOf(vec![Type::I32, Type::U32, Type::F32])
            }
            Number::Float(_) => TypeState::Known(Type::F32),
          },
          &self.source_trace,
          errors,
        );
        changed
      }
      BooleanLiteral(_) => {
        self.data.subtree_fully_typed = true;
        let changed = self.data.constrain(
          TypeState::Known(Type::Bool),
          &self.source_trace,
          errors,
        );
        changed
      }
      Function(arg_names, body) => {
        ctx.push_enclosing_function_type(self.data.clone().kind);
        let changed = if let TypeState::Known(f_type) = &mut self.data.kind {
          match f_type {
            Type::Function(signature) => {
              let (name, arg_count, arg_type_states, return_type_state): (
                Option<Rc<str>>,
                usize,
                &Vec<ExpTypeInfo>,
                &mut ExpTypeInfo,
              ) = (
                signature.name(),
                signature.arg_types.len(),
                &signature.arg_types.iter().map(|(t, _)| t.clone()).collect(),
                &mut signature.return_type,
              );
              let return_type_changed = body.data.mutually_constrain(
                return_type_state,
                &self.source_trace,
                errors,
              );
              if arg_count == arg_names.len() {
                for (name, t) in arg_names.iter().zip(arg_type_states) {
                  ctx.bind(name, Variable::new(t.clone()))
                }
                let body_types_changed =
                  body.propagate_types_inner(ctx, errors);
                let argument_types = arg_names
                  .iter()
                  .map(|name| ctx.unbind(name).typestate.kind)
                  .collect::<Vec<_>>();
                let fn_type_changed = self.data.constrain_fn_by_argument_types(
                  argument_types,
                  &self.source_trace,
                  errors,
                );
                self.data.subtree_fully_typed = body.data.subtree_fully_typed;
                return_type_changed || body_types_changed || fn_type_changed
              } else {
                errors.log(CompileError::new(
                  WrongArity(name.map(|n| n.to_string())),
                  self.source_trace.clone(),
                ));
                false
              }
            }
            _ => {
              errors.log(CompileError::new(
                FunctionExpressionHasNonFunctionType(f_type.clone().into()),
                self.source_trace.clone(),
              ));
              false
            }
          }
        } else {
          false
        };
        ctx.pop_enclosing_function_type();
        changed
      }
      Application(f, args) => {
        let mut anything_changed = false;
        for arg in args.iter_mut() {
          anything_changed |= arg.propagate_types_inner(ctx, errors);
        }
        if let Name(name) = &f.kind {
          anything_changed |= ctx.constrain_name_type(
            name,
            &self.source_trace,
            &mut f.data,
            errors,
          );
        } else {
          anything_changed |= f.data.constrain(
            TypeState::Known(Type::Function(Box::new(FunctionSignature {
              abstract_ancestor: None,
              arg_types: args
                .iter()
                .map(|arg| (arg.data.clone(), vec![]))
                .collect(),
              mutated_args: vec![],
              return_type: self.data.clone(),
            }))),
            &self.source_trace,
            errors,
          );
        }
        if let TypeState::Known(f_type) = &mut f.data.kind {
          match f_type {
            Type::Function(signature) => {
              if signature
                .abstract_ancestor
                .as_ref()
                .map(|ancestor| ancestor.associative)
                .unwrap_or(false)
                || args.len() == signature.arg_types.len()
              {
                anything_changed |= self.data.mutually_constrain(
                  &mut signature.return_type,
                  &self.source_trace,
                  errors,
                );
                for (arg, (t, _)) in
                  args.iter_mut().zip(signature.arg_types.iter().cloned())
                {
                  anything_changed |=
                    arg.data.constrain(t.kind, &self.source_trace, errors);
                }
              } else {
                errors.log(CompileError::new(
                  WrongArity(signature.name().map(|n| n.to_string())),
                  self.source_trace.clone(),
                ));
              }
            }
            Type::Array(_, inner_type) => {
              if args.len() == 1 {
                anything_changed |= self.data.mutually_constrain(
                  inner_type,
                  &self.source_trace,
                  errors,
                );
                let first_arg = &mut args[0];
                first_arg.data.constrain(
                  TypeState::OneOf(vec![Type::I32, Type::U32]),
                  &first_arg.source_trace,
                  errors,
                );
              } else {
                errors.log(CompileError::new(
                  ArrayLookupInvalidArity(args.len()),
                  self.source_trace.clone(),
                ));
              }
            }
            _ => {
              errors.log(CompileError::new(
                AppliedNonFunction,
                self.source_trace.clone(),
              ));
            }
          }
        }
        anything_changed |= f.data.constrain_fn_by_argument_types(
          args.iter().map(|arg| arg.data.kind.clone()).collect(),
          &self.source_trace,
          errors,
        );
        anything_changed |= f.propagate_types_inner(ctx, errors);
        self.data.subtree_fully_typed =
          args.iter().fold(f.data.subtree_fully_typed, |acc, arg| {
            acc && arg.data.subtree_fully_typed
          });

        anything_changed
      }
      Access(accessor, subexp) => match accessor {
        Accessor::Field(field_name) => {
          let mut anything_changed = subexp.propagate_types_inner(ctx, errors);
          if let Known(t) = &mut subexp.data.kind {
            if let Type::Struct(s) = t {
              if let Some(x) =
                &mut s.fields.iter_mut().find(|f| f.name == *field_name)
              {
                anything_changed |= self.data.mutually_constrain(
                  &mut x.field_type,
                  &self.source_trace,
                  errors,
                );
              } else {
                errors.log(CompileError::new(
                  NoSuchField {
                    struct_name: s.abstract_ancestor.name.to_string(),
                    field_name: field_name.to_string(),
                  },
                  self.source_trace.clone(),
                ));
              }
            } else {
              errors.log(CompileError::new(
                AccessorOnNonStruct,
                self.source_trace.clone(),
              ));
            }
          }
          self.data.subtree_fully_typed = subexp.data.subtree_fully_typed;
          anything_changed
        }
        Accessor::Swizzle(fields) => {
          let mut anything_changed = self.data.constrain(
            swizzle_accessor_typestate(&subexp.data, fields).kind,
            &self.source_trace,
            errors,
          );
          anything_changed |= subexp.data.constrain(
            swizzle_accessed_possibilities(fields).kind,
            &self.source_trace,
            errors,
          );
          anything_changed |= subexp.propagate_types_inner(ctx, errors);
          self.data.subtree_fully_typed = subexp.data.subtree_fully_typed;
          anything_changed
        }
      },
      Let(bindings, body) => {
        let mut anything_changed = body.data.mutually_constrain(
          &mut self.data,
          &self.source_trace,
          errors,
        );
        for (name, _, value) in bindings.iter_mut() {
          anything_changed |= value.propagate_types_inner(ctx, errors);
          ctx.bind(name, Variable::new(value.data.clone()));
        }
        anything_changed |= body.propagate_types_inner(ctx, errors);
        for (name, _, value) in bindings.iter_mut() {
          anything_changed |= value.data.constrain(
            ctx.unbind(name).typestate.kind,
            &self.source_trace,
            errors,
          );
        }
        self.data.subtree_fully_typed = bindings.iter().fold(
          body.data.subtree_fully_typed,
          |acc, (_, _, binding_value)| {
            acc && binding_value.data.subtree_fully_typed
          },
        );
        anything_changed
      }
      Match(scrutinee, arms) => {
        let mut anything_changed = scrutinee.propagate_types_inner(ctx, errors);
        for (pattern, value) in arms.iter_mut() {
          if let Application(f, args) = &mut pattern.kind {
            f.propagate_types_inner(ctx, errors);
            if let Some((f, arg, inner_value_name)) =
              Self::try_deconstruct_enum_pattern(f, args)
            {
              anything_changed |= f.return_type.mutually_constrain(
                &mut scrutinee.data,
                &self.source_trace,
                errors,
              );
              anything_changed |= f.return_type.mutually_constrain(
                &mut pattern.data,
                &self.source_trace,
                errors,
              );
              anything_changed |= f.arg_types[0].0.mutually_constrain(
                &mut arg.data,
                &self.source_trace,
                errors,
              );
              ctx.bind(
                &inner_value_name,
                Variable {
                  kind: VariableKind::Let,
                  typestate: f.arg_types[0].0.clone(),
                },
              );
              anything_changed |= value.propagate_types_inner(ctx, errors);
              ctx.unbind(&inner_value_name);
            } else {
              errors.log(CompileError::new(
                CompileErrorKind::InvalidMatchPattern,
                self.source_trace.clone(),
              ))
            }
          } else {
            ctx.inside_pattern = true;
            anything_changed |= pattern.propagate_types_inner(ctx, errors);
            ctx.inside_pattern = false;
            anything_changed |= value.propagate_types_inner(ctx, errors);
            anything_changed |= pattern.data.mutually_constrain(
              &mut scrutinee.data,
              &self.source_trace,
              errors,
            );
          }
          anything_changed |= value.data.mutually_constrain(
            &mut self.data,
            &self.source_trace,
            errors,
          );
        }
        self.data.subtree_fully_typed = arms.iter().fold(
          scrutinee.data.subtree_fully_typed,
          |acc, (pattern, case)| {
            acc
              && pattern.data.subtree_fully_typed
              && case.data.subtree_fully_typed
          },
        );
        anything_changed
      }
      Block(expressions) => {
        let mut anything_changed = false;
        for child in expressions.iter_mut() {
          anything_changed |= child.propagate_types_inner(ctx, errors);
        }
        if let Some(exp) = expressions.last_mut() {
          anything_changed |= self.data.mutually_constrain(
            &mut exp.data,
            &self.source_trace,
            errors,
          );
        } else {
          errors.log(CompileError::new(EmptyBlock, self.source_trace.clone()));
        }
        self.data.subtree_fully_typed = expressions
          .iter()
          .fold(true, |acc, exp| acc && exp.data.subtree_fully_typed);
        anything_changed
      }
      ForLoop {
        increment_variable_name,
        increment_variable_type,
        increment_variable_initial_value_expression,
        continue_condition_expression,
        update_condition_expression,
        body_expression,
      } => {
        let mut variable_typestate =
          TypeState::Known(increment_variable_type.clone());
        let mut anything_changed = increment_variable_initial_value_expression
          .data
          .mutually_constrain(
            &mut variable_typestate,
            &increment_variable_initial_value_expression.source_trace,
            errors,
          );
        anything_changed |= increment_variable_initial_value_expression
          .propagate_types_inner(ctx, errors);
        ctx.bind(
          increment_variable_name,
          Variable {
            kind: VariableKind::Var,
            typestate: variable_typestate.into(),
          },
        );
        anything_changed |= continue_condition_expression.data.constrain(
          TypeState::Known(Type::Bool),
          &continue_condition_expression.source_trace,
          errors,
        );
        anything_changed |= update_condition_expression.data.constrain(
          TypeState::Known(Type::Unit),
          &update_condition_expression.source_trace,
          errors,
        );
        anything_changed |=
          continue_condition_expression.propagate_types_inner(ctx, errors);
        anything_changed |=
          update_condition_expression.propagate_types_inner(ctx, errors);
        anything_changed |= body_expression.propagate_types_inner(ctx, errors);
        ctx.unbind(&increment_variable_name);
        self.data.subtree_fully_typed =
          increment_variable_initial_value_expression
            .data
            .subtree_fully_typed
            && continue_condition_expression.data.subtree_fully_typed
            && update_condition_expression.data.subtree_fully_typed
            && body_expression.data.subtree_fully_typed;
        anything_changed
      }
      WhileLoop {
        condition_expression,
        body_expression,
      } => {
        let mut anything_changed = condition_expression.data.constrain(
          TypeState::Known(Type::Bool),
          &condition_expression.source_trace,
          errors,
        );
        anything_changed |= body_expression.data.constrain(
          TypeState::Known(Type::Unit),
          &condition_expression.source_trace,
          errors,
        );
        anything_changed |=
          condition_expression.propagate_types_inner(ctx, errors);
        anything_changed |= body_expression.propagate_types_inner(ctx, errors);
        self.data.subtree_fully_typed =
          condition_expression.data.subtree_fully_typed
            && body_expression.data.subtree_fully_typed;
        anything_changed
      }
      Break => {
        self.data.subtree_fully_typed = true;
        false
      }
      Continue => {
        self.data.subtree_fully_typed = true;
        false
      }
      Discard => {
        self.data.subtree_fully_typed = true;
        false
      }
      Return(exp) => {
        let mut changed = if let Some(t) = ctx.enclosing_function_type() {
          match t.as_fn_type_if_known(|| {
            CompileError::new(
              CompileErrorKind::EnclosingFunctionTypeWasntFunction,
              self.source_trace.clone(),
            )
          }) {
            Ok(fn_type) => {
              let changed = exp.data.mutually_constrain(
                &mut fn_type
                  .map(|fn_type| &mut fn_type.return_type)
                  .unwrap_or(&mut TypeState::Unknown.into()),
                &self.source_trace,
                errors,
              );
              changed
            }
            Err(e) => {
              errors.log(e);
              false
            }
          }
        } else {
          errors.log(CompileError::new(
            ReturnOutsideFunction,
            self.source_trace.clone(),
          ));
          false
        };
        changed |= exp.propagate_types_inner(ctx, errors);
        self.data.subtree_fully_typed = exp.data.subtree_fully_typed;
        changed
      }
      ArrayLiteral(children) => {
        let mut anything_changed = false;
        for child in children.iter_mut() {
          self.data.as_known_mut(|array_type| {
            if let Type::Array(_, inner_type) = array_type {
              anything_changed |= child.data.mutually_constrain(
                inner_type.as_mut(),
                &child
                  .source_trace
                  .clone()
                  .insert_as_secondary(self.source_trace.clone()),
                errors,
              );
              anything_changed |= child.propagate_types_inner(ctx, errors);
            } else {
              errors.log(CompileError::new(
                CompileErrorKind::ArrayLiteralMistyped,
                self.source_trace.clone(),
              ));
            }
          });
        }
        self.data.subtree_fully_typed =
          if let Known(Type::Array(Some(_), _)) = self.data.kind {
            true
          } else {
            false
          } && children
            .iter()
            .map(|child| child.data.subtree_fully_typed)
            .reduce(|a, b| a && b)
            .unwrap_or(true);
        anything_changed
      }
      Reference(exp) => {
        let mut anything_changed = exp.propagate_types_inner(ctx, errors);
        let TypeState::Known(Type::Reference(inner_type)) = &mut self.data.kind
        else {
          unreachable!()
        };
        anything_changed |=
          exp
            .data
            .mutually_constrain(inner_type, &self.source_trace, errors);
        self.data.subtree_fully_typed = exp.data.subtree_fully_typed;
        anything_changed
      }
      ZeroedArray => {
        self.data.subtree_fully_typed = true;
        false
      }
    };
    self.data.subtree_fully_typed &= self.data.check_is_fully_known();
    changed
  }
  pub fn propagate_types(
    &mut self,
    program: &mut Program,
    errors: &mut ErrorLog,
  ) -> bool {
    self.propagate_types_inner(&mut LocalContext::empty(program), errors)
  }
  fn name_or_inner_accessed_name(&self) -> Option<&Rc<str>> {
    let mut exp = self;
    loop {
      match &exp.kind {
        ExpKind::Access(_, inner_exp) => exp = inner_exp,
        ExpKind::Application(f, _) => match f.data.kind.unwrap_known() {
          Type::Array(_, _) => exp = f,
          _ => break,
        },
        _ => break,
      }
    }
    if let ExpKind::Name(name) = &exp.kind {
      Some(name)
    } else {
      None
    }
  }
  fn validate_assignments_inner(
    &self,
    ctx: &mut ImmutableProgramLocalContext,
  ) -> CompileResult<()> {
    self.walk(&mut |exp| {
      Ok(match &exp.kind {
        Application(f, args) => {
          if let ExpKind::Name(f_name) = &f.kind {
            if ASSIGNMENT_OPS.contains(&&**f_name) {
              if let Some(var_name) = args[0].name_or_inner_accessed_name() {
                if ctx.get_variable_kind(var_name) != VariableKind::Var {
                  return err(
                    AssignmentTargetMustBeVariable(var_name.to_string()),
                    exp.source_trace.clone(),
                  );
                }
              } else {
                return err(InvalidAssignmentTarget, exp.source_trace.clone());
              }
            }
          }
          for arg in args {
            arg.validate_assignments_inner(ctx)?;
          }
          f.validate_assignments_inner(ctx)?;
          false
        }
        Let(binding_names_and_values, body) => {
          for (name, kind, value) in binding_names_and_values {
            value.validate_assignments_inner(ctx)?;
            ctx.bind(
              name,
              Variable::new(value.data.clone()).with_kind(kind.clone()),
            )
          }
          body.validate_assignments_inner(ctx)?;
          for (name, _, _) in binding_names_and_values {
            ctx.unbind(name);
          }
          false
        }
        Function(arg_names, body_exp) => {
          if let TypeState::Known(Type::Function(f)) = &exp.data.kind {
            for (name, (ty, _)) in arg_names.iter().zip(f.arg_types.iter()) {
              ctx.bind(
                name,
                Variable {
                  kind: VariableKind::Let,
                  typestate: ty.clone(),
                },
              );
            }
            body_exp.validate_assignments_inner(ctx)?;
            for name in arg_names {
              ctx.unbind(name);
            }
            false
          } else {
            unreachable!()
          }
        }
        ForLoop {
          increment_variable_name,
          increment_variable_type,
          increment_variable_initial_value_expression,
          continue_condition_expression,
          update_condition_expression,
          body_expression,
        } => {
          ctx.bind(
            increment_variable_name,
            Variable::new(
              TypeState::Known(increment_variable_type.clone()).into(),
            )
            .with_kind(VariableKind::Var),
          );
          increment_variable_initial_value_expression
            .validate_assignments_inner(ctx)?;
          continue_condition_expression.validate_assignments_inner(ctx)?;
          update_condition_expression.validate_assignments_inner(ctx)?;
          body_expression.validate_assignments_inner(ctx)?;
          ctx.unbind(increment_variable_name);
          false
        }
        _ => true,
      })
    })
  }
  pub fn validate_assignments(&self, program: &Program) -> CompileResult<()> {
    self.validate_assignments_inner(&mut ImmutableProgramLocalContext::empty(
      program,
    ))
  }
  pub fn replace_skolems(&mut self, skolems: &HashMap<Rc<str>, Type>) {
    self
      .walk_mut::<()>(&mut |exp| {
        if let Known(t) = &mut exp.data.kind {
          t.replace_skolems(skolems);
        }
        Ok(true)
      })
      .unwrap()
  }
  pub fn monomorphize(
    &mut self,
    base_program: &Program,
    new_program: &mut Program,
  ) -> CompileResult<()> {
    let source_trace = self.source_trace.clone();
    self.walk_mut::<CompileError>(&mut |exp: &mut TypedExp| {
      match &mut exp.kind {
        Name(name) => {
          if let Some(original_e) = base_program
            .typedefs
            .enums
            .iter()
            .find(|e| e.has_unit_variant_named(name))
          {
            if let Type::Enum(e) = exp.data.kind.unwrap_known() {
              let variant_types = e
                .variants
                .iter()
                .map(|variant| variant.inner_type.unwrap_known())
                .collect::<Vec<Type>>();
              let suffix = original_e.monomorphized_suffix(&variant_types);
              *name = (name.to_string() + &suffix).into();
            }
          }
        }
        Application(f, args) => {
          if let ExpKind::Name(f_name) = &mut f.kind {
            if let Some(abstract_signature) =
              if let TypeState::Known(Type::Function(f)) = &f.data.kind {
                &f.abstract_ancestor
              } else {
                &None
              }
            {
              match &abstract_signature.implementation {
                FunctionImplementationKind::Builtin => {
                  if vec_and_mat_compile_names().contains(&**f_name) {
                    if let Type::Struct(s) = &exp.data.kind.unwrap_known() {
                      if let Some(mut compiled_name) = compiled_vec_or_mat_name(
                        &f_name,
                        s.fields[0].field_type.unwrap_known(),
                      ) {
                        std::mem::swap(f_name, &mut compiled_name);
                      }
                    } else {
                      unreachable!("")
                    }
                  } else if &**f_name == "bitcast" {
                    std::mem::swap(
                      f_name,
                      &mut format!(
                        "bitcast<{}>",
                        exp.data.kind.unwrap_known().compile()
                      )
                      .into(),
                    )
                  } else {
                    {}
                  }
                }
                FunctionImplementationKind::StructConstructor => {
                  if let Some(abstract_struct) = base_program
                    .typedefs
                    .structs
                    .iter()
                    .find(|s| s.name == *f_name)
                  {
                    let arg_types: Vec<Type> =
                      args.iter().map(|arg| arg.data.unwrap_known()).collect();
                    if abstract_struct.generic_args.len() > 0
                      && let Some(monomorphized_struct) = abstract_struct
                        .generate_monomorphized(arg_types.clone())
                    {
                      let monomorphized_struct = Rc::new(monomorphized_struct);
                      std::mem::swap(
                        f_name,
                        &mut AbstractStruct::concretized_name(
                          monomorphized_struct.clone(),
                          &base_program.typedefs,
                          exp.source_trace.clone(),
                        )?,
                      );
                      let mut new_typestate = TypeState::Known(Type::Struct(
                        AbstractStruct::fill_generics_ordered(
                          monomorphized_struct.clone(),
                          vec![],
                          &base_program.typedefs,
                          source_trace.clone(),
                        )?,
                      ));
                      exp.data.with_dereferenced_mut(|typestate| {
                        std::mem::swap(typestate, &mut new_typestate)
                      });
                      new_program
                        .add_monomorphized_struct(monomorphized_struct.into());
                    }
                  }
                }
                FunctionImplementationKind::EnumConstructor(variant_name) => {
                  let Type::Function(f) = f.data.unwrap_known() else {
                    unreachable!()
                  };
                  let Type::Enum(enum_type) = f.return_type.unwrap_known()
                  else {
                    unreachable!()
                  };
                  if let Some(abstract_enum) =
                    base_program.typedefs.enums.iter().find(|e| {
                      e.variants.iter().find(|v| v.name == *f_name).is_some()
                    })
                  {
                    if abstract_enum.generic_args.is_empty() {
                      new_program.add_monomorphized_enum(Rc::unwrap_or_clone(
                        abstract_enum.clone().into(),
                      ));
                    } else if let Some(monomorphized_enum) =
                      abstract_enum.generate_monomorphized(enum_type)
                    {
                      let monomorphized_enum = Rc::new(monomorphized_enum);
                      std::mem::swap(
                        f_name,
                        &mut (f_name.to_string()
                          + &AbstractEnum::concretized_suffix(
                            monomorphized_enum.clone(),
                            &base_program.typedefs,
                            exp.source_trace.clone(),
                          )?)
                          .into(),
                      );
                      let mut new_typestate = TypeState::Known(Type::Enum(
                        AbstractEnum::fill_generics_ordered(
                          monomorphized_enum.clone(),
                          vec![],
                          &base_program.typedefs,
                          source_trace.clone(),
                        )?,
                      ));
                      exp.data.with_dereferenced_mut(|typestate| {
                        std::mem::swap(typestate, &mut new_typestate)
                      });
                      new_program.add_abstract_function(Rc::new(RefCell::new(
                        AbstractFunctionSignature {
                          name: f_name.clone(),
                          generic_args: vec![],
                          arg_types: abstract_signature.arg_types.clone(),
                          mutated_args: vec![],
                          return_type: AbstractType::AbstractEnum(
                            monomorphized_enum.clone(),
                          ),
                          implementation:
                            FunctionImplementationKind::EnumConstructor(
                              variant_name.clone(),
                            ),
                          associative: false,
                        },
                      )));
                      new_program
                        .add_monomorphized_enum(monomorphized_enum.into());
                    }
                  }
                }
                FunctionImplementationKind::Composite(composite) => {
                  if !abstract_signature.generic_args.is_empty() {
                    let monomorphized = abstract_signature
                      .generate_monomorphized(
                        args
                          .iter()
                          .map(|arg| arg.data.unwrap_known())
                          .collect(),
                        exp.data.unwrap_known().clone(),
                        base_program,
                        new_program,
                        composite.borrow().body.source_trace.clone(),
                      )?;
                    std::mem::swap(f_name, &mut monomorphized.name.clone());
                    let monomorphized = Rc::new(RefCell::new(monomorphized));
                    std::mem::swap(
                      &mut f.data.kind,
                      &mut TypeState::Known(Type::Function(
                        AbstractFunctionSignature::concretize(
                          monomorphized.clone(),
                          &new_program.typedefs,
                          source_trace.clone(),
                        )
                        .unwrap()
                        .into(),
                      )),
                    );
                    new_program.add_abstract_function(monomorphized);
                  }
                }
              }
            }
          }
        }
        _ => {}
      }
      Ok(true)
    })?;
    Ok(())
  }
  pub fn inline_higher_order_arguments(
    &mut self,
    new_ctx: &mut Program,
  ) -> CompileResult<bool> {
    let mut changed = false;
    self.walk_mut::<CompileError>(&mut |exp: &mut TypedExp| {
      if let Application(f, args) = &mut exp.kind {
        if let ExpKind::Name(f_name) = &mut f.kind {
          if let Some(abstract_signature) =
            if let TypeState::Known(Type::Function(f)) = &f.data.kind {
              &f.abstract_ancestor
            } else {
              &None
            }
          {
            match &abstract_signature.implementation {
              FunctionImplementationKind::Composite(f) => {
                let (function_arg_positions, function_args) = args
                  .iter()
                  .enumerate()
                  .filter_map(|(i, arg)| {
                    if let Type::Function(_) = arg.data.kind.unwrap_known() {
                      Some(Ok((i, arg.clone())))
                    } else {
                      None
                    }
                  })
                  .collect::<CompileResult<(Vec<usize>, Vec<TypedExp>)>>()?;
                if !function_args.is_empty() {
                  let inlined = abstract_signature
                    .generate_higher_order_functions_inlined_version(
                      f_name,
                      function_args,
                      f.borrow().body.source_trace.clone(),
                    )?;
                  std::mem::swap(f_name, &mut inlined.name.clone());
                  for fn_arg_index in function_arg_positions.iter().rev() {
                    args.remove(*fn_arg_index);
                  }
                  new_ctx.add_abstract_function(Rc::new(RefCell::new(inlined)));
                  changed = true;
                }
              }
              _ => {}
            }
          }
        } else {
          todo!("can't inline this kind of higher-order argument yet")
        }
      }
      Ok(true)
    })?;
    Ok(changed)
  }
  pub fn inline_args(
    &mut self,
    param_names: &Vec<Rc<str>>,
    arg_values: &Vec<TypedExp>,
  ) {
    self
      .walk_mut::<()>(&mut |exp| {
        if let Name(original_name) = &mut exp.kind {
          if let Some(mut value) = (0..param_names.len()).find_map(|i| {
            (param_names[i] == *original_name).then(|| arg_values[i].clone())
          }) {
            std::mem::swap(exp, &mut value);
          }
        }
        Ok(true)
      })
      .unwrap()
  }

  fn deshadow_inner(
    &mut self,
    globally_bound_names: &Vec<Rc<str>>,
    bindings: &mut HashMap<Rc<str>, Vec<Rc<str>>>,
    errors: &mut ErrorLog,
    first_in_walk: bool,
  ) -> bool {
    let add_binding = |name: &mut Rc<str>,
                       source_trace: &SourceTrace,
                       bindings: &mut HashMap<Rc<str>, Vec<Rc<str>>>,
                       errors: &mut ErrorLog| {
      if globally_bound_names.contains(&name) {
        errors.log(CompileError::new(
          CompileErrorKind::CantShadowTopLevelBinding(name.to_string()),
          source_trace.clone(),
        ));
      }
      if let Some(renames) = bindings.get_mut(name) {
        let gensym_name: Rc<str> =
          format!("{}_deshadowed_{}", name.to_string(), renames.len()).into();
        std::mem::swap(name, &mut gensym_name.clone());
        renames.push(gensym_name);
      } else {
        bindings.insert(name.clone(), vec![]);
      }
    };
    match &mut self.kind {
      Let(let_bindings, body) => {
        for binding in let_bindings.iter_mut() {
          binding.2.deshadow_inner(
            globally_bound_names,
            bindings,
            errors,
            true,
          );
          add_binding(
            &mut binding.0,
            &binding.2.source_trace,
            bindings,
            errors,
          );
        }
        body.deshadow_inner(globally_bound_names, bindings, errors, true);
        false
      }
      ForLoop {
        increment_variable_name,
        continue_condition_expression,
        update_condition_expression,
        body_expression,
        ..
      } => {
        add_binding(
          increment_variable_name,
          &self.source_trace,
          bindings,
          errors,
        );
        continue_condition_expression.deshadow_inner(
          globally_bound_names,
          bindings,
          errors,
          true,
        );
        update_condition_expression.deshadow_inner(
          globally_bound_names,
          bindings,
          errors,
          true,
        );
        body_expression.deshadow_inner(
          globally_bound_names,
          bindings,
          errors,
          true,
        );
        false
      }
      Name(name) => {
        if let Some(renames) = bindings.get(name) {
          if let Some(rename) = renames.last() {
            std::mem::swap(name, &mut Rc::clone(rename));
          }
        }
        false
      }
      _ => {
        if first_in_walk {
          self
            .walk_mut::<()>(&mut |exp| {
              Ok(exp.deshadow_inner(
                globally_bound_names,
                bindings,
                errors,
                false,
              ))
            })
            .unwrap();
          false
        } else {
          true
        }
      }
    }
  }
  pub fn deshadow(
    &mut self,
    globally_bound_names: &Vec<Rc<str>>,
    errors: &mut ErrorLog,
  ) {
    self.deshadow_inner(
      globally_bound_names,
      &mut Default::default(),
      errors,
      true,
    );
  }
  pub fn validate_control_flow(
    &self,
    errors: &mut ErrorLog,
    enclosing_loop_count: usize,
  ) {
    self
      .walk(&mut |exp| {
        Ok::<_, Never>(match &exp.kind {
          ExpKind::Break | ExpKind::Continue => {
            if enclosing_loop_count == 0 {
              errors.log(CompileError {
                kind: if exp.kind == ExpKind::Break {
                  CompileErrorKind::BreakOutsideLoop
                } else {
                  CompileErrorKind::ContinueOutsideLoop
                },
                source_trace: exp.source_trace.clone(),
              });
            }
            true
          }
          ExpKind::ForLoop {
            increment_variable_initial_value_expression,
            continue_condition_expression,
            update_condition_expression,
            body_expression,
            ..
          } => {
            increment_variable_initial_value_expression
              .validate_control_flow(errors, enclosing_loop_count);
            continue_condition_expression
              .validate_control_flow(errors, enclosing_loop_count);
            update_condition_expression
              .validate_control_flow(errors, enclosing_loop_count);
            body_expression
              .validate_control_flow(errors, enclosing_loop_count + 1);
            false
          }
          ExpKind::WhileLoop {
            body_expression,
            condition_expression,
          } => {
            condition_expression
              .validate_control_flow(errors, enclosing_loop_count);
            body_expression
              .validate_control_flow(errors, enclosing_loop_count + 1);
            false
          }
          _ => true,
        })
      })
      .unwrap();
  }
  fn internally_referenced_names(&self) -> Vec<(Rc<str>, Type)> {
    let mut names = HashSet::new();
    let mut name_type_pairs = vec![];
    self
      .walk(&mut |exp| {
        match &exp.kind {
          Name(name) => {
            if !names.contains(name) {
              names.insert(name.clone());
              name_type_pairs.push((name.clone(), exp.data.unwrap_known()))
            }
          }
          _ => {}
        }
        Ok::<_, Never>(true)
      })
      .unwrap();
    name_type_pairs
  }
  pub fn effects(&self, program: &Program) -> EffectType {
    match &self.kind {
      Name(name) => Effect::ReadsVar(name.clone()).into(),
      Return(exp) | Reference(exp) => exp.effects(program),
      Block(exps) | ArrayLiteral(exps) => exps
        .into_iter()
        .map(|exp| exp.effects(program))
        .reduce(|mut a, b| {
          a.merge(b);
          a
        })
        .unwrap_or_else(|| EffectType::empty())
        .into(),
      Let(items, exp) => {
        let effects = items.iter().fold(
          exp.effects(program),
          |mut e, (_, _, value_exp)| {
            e.merge(value_exp.effects(program));
            e
          },
        );
        let bound_names: HashSet<Rc<str>> =
          items.iter().map(|(name, _, _)| name.clone()).collect();
        effects
          .0
          .into_iter()
          .filter_map(|e| match &e {
            Effect::ModifiesVar(name) | Effect::ReadsVar(name) => {
              (!bound_names.contains(name)).then(|| e)
            }
            _ => Some(e),
          })
          .collect::<HashSet<Effect>>()
          .into()
      }
      Application(f, args) => match f.data.kind.unwrap_known() {
        Type::Function(function_signature) => {
          let mut effects = function_signature.effects(program);
          for arg in args {
            effects.merge(arg.effects(program));
          }
          for i in function_signature.mutated_args.iter().copied() {
            effects.merge(Effect::ModifiesVar(
              args[i]
                .name_or_inner_accessed_name()
                .expect(
                  "No name found in mutated argument position. This should \
                  never happen if validate_assignments has passed.",
                )
                .clone(),
            ));
          }
          effects
        }
        Type::Array(_, _) => EffectType::empty(),
        _ => unreachable!(),
      },
      Access(_, exp) => exp.effects(program),
      Match(exp, items) => {
        let mut effects = exp.effects(program);
        for (_, arm) in items {
          effects.merge(arm.effects(program));
        }
        effects
      }
      ForLoop {
        increment_variable_name,
        increment_variable_initial_value_expression,
        continue_condition_expression,
        update_condition_expression,
        body_expression,
        ..
      } => {
        let mut effects =
          increment_variable_initial_value_expression.effects(program);
        effects.merge(continue_condition_expression.effects(program));
        effects.merge(update_condition_expression.effects(program));
        effects.merge(body_expression.effects(program));
        effects.remove(&Effect::ModifiesVar(increment_variable_name.clone()));
        effects.remove(&Effect::ReadsVar(increment_variable_name.clone()));
        effects.remove(&Effect::Break);
        effects.remove(&Effect::Continue);
        effects
      }
      WhileLoop {
        condition_expression,
        body_expression,
      } => {
        let mut effects = condition_expression.effects(program);
        effects.merge(body_expression.effects(program));
        effects.remove(&Effect::Break);
        effects.remove(&Effect::Continue);
        effects
      }
      Break => Effect::Break.into(),
      Continue => Effect::Continue.into(),
      Discard => Effect::Discard.into(),
      Wildcard => EffectType::empty(),
      Unit => EffectType::empty(),
      NumberLiteral(_) => EffectType::empty(),
      BooleanLiteral(_) => EffectType::empty(),
      Function(_, _) => EffectType::empty(),
      Uninitialized => EffectType::empty(),
      ZeroedArray => EffectType::empty(),
    }
  }
  fn replace_internal_names(&mut self, new_names: &HashMap<Rc<str>, Rc<str>>) {
    self
      .walk_mut::<()>(&mut |exp| {
        match &mut exp.kind {
          Name(name) => {
            if let Some(replacement) = new_names.get(name) {
              std::mem::swap(name, &mut replacement.clone());
            }
          }
          _ => {}
        }
        Ok(true)
      })
      .unwrap()
  }
  pub fn deexpressionify(&mut self, program: &Program) {
    let mut gensym_index = 0;
    loop {
      let mut changed = false;
      let placeholder_exp_kind = ExpKind::Wildcard;
      let placeholder_exp = TypedExp {
        kind: ExpKind::Wildcard,
        data: TypeState::Unknown.into(),
        source_trace: SourceTrace::empty(),
      };
      let _ = self.walk_mut::<()>(&mut |exp| {
        Ok(match &mut exp.kind {
          Application(_, _)
          | ArrayLiteral(_)
          | Reference(_)
          | Return(_)
          | Access(_, _)
          | Match(_, _) => {
            let mut slots: Vec<&mut TypedExp> = match &mut exp.kind {
              Application(_, args) | ArrayLiteral(args) => {
                args.iter_mut().collect()
              }
              Reference(inner_exp) | Return(inner_exp) => vec![inner_exp],
              Access(_, inner_exp) => {
                vec![inner_exp.as_mut()]
              }
              Match(scrutinee, _) => {
                vec![scrutinee]
              }
              _ => unreachable!(),
            };
            let mut previously_referenced_names: Vec<(Rc<str>, Type)> = vec![];
            let mut restructure_index: Option<usize> = None;
            for arg_index in 0..slots.len() {
              let arg = &mut slots[arg_index];
              match &arg.kind {
                Return(_) => {
                  restructure_index = Some(arg_index);
                }
                Block(_) | Match(_, _) | Let(_, _) => {
                  restructure_index = Some(arg_index);
                  let effects = arg.effects(program);
                  let mut overridden_names: Vec<(Rc<str>, Type)> = vec![];
                  for (name, t) in previously_referenced_names {
                    if effects.0.contains(&Effect::ModifiesVar(name.clone())) {
                      overridden_names.push((name, t));
                    }
                  }
                  if !overridden_names.is_empty() {
                    changed = true;
                    let mut replacement_types: HashMap<Rc<str>, Type> =
                      HashMap::new();
                    let mut replacement_names: HashMap<Rc<str>, Rc<str>> =
                      HashMap::new();
                    for (name, t) in overridden_names {
                      let replacement_name =
                        format!("original_{name}_{gensym_index}").into();
                      replacement_names.insert(name.clone(), replacement_name);
                      replacement_types.insert(name, t);
                      gensym_index += 1;
                    }
                    for arg in &mut slots[0..arg_index] {
                      arg.replace_internal_names(&replacement_names);
                    }
                    take(exp, |exp| TypedExp {
                      data: exp.data.clone(),
                      kind: ExpKind::Let(
                        replacement_names
                          .into_iter()
                          .map(|(old_name, new_name)| {
                            (
                              new_name,
                              VariableKind::Let,
                              TypedExp {
                                data: TypeState::Known(
                                  replacement_types.remove(&old_name).unwrap(),
                                )
                                .into(),
                                kind: ExpKind::Name(old_name),
                                source_trace: SourceTrace::empty(),
                              },
                            )
                          })
                          .collect(),
                        exp.into(),
                      ),
                      source_trace: SourceTrace::empty(),
                    });
                    return Ok(true);
                  }
                  break;
                }
                _ => {}
              }
              for (name, t) in arg.internally_referenced_names() {
                if previously_referenced_names
                  .iter()
                  .find(|(existing_name, _)| name == *existing_name)
                  .is_none()
                {
                  previously_referenced_names.push((name, t));
                }
              }
            }
            if let Some(restructure_index) = restructure_index {
              let arg = &mut slots[restructure_index];
              changed = true;
              match &mut arg.kind {
                Return(_) => {
                  let mut inner_expressions = vec![];
                  for i in 0..restructure_index + 1 {
                    let mut temp = placeholder_exp.clone();
                    std::mem::swap(slots[i], &mut temp);
                    inner_expressions.push(temp);
                  }
                  std::mem::swap(
                    exp,
                    &mut TypedExp {
                      data: inner_expressions.last().unwrap().data.clone(),
                      kind: ExpKind::Block(inner_expressions),
                      source_trace: SourceTrace::empty(),
                    },
                  );
                }
                Block(_) => {
                  let mut prefix_statements = vec![];
                  take(*arg, |arg| {
                    let Block(mut statements) = arg.kind else {
                      unreachable!()
                    };
                    let value = statements.pop().unwrap();
                    prefix_statements = statements;
                    value
                  });
                  take(exp, |exp| {
                    let t = exp.data.clone();
                    prefix_statements.push(exp);
                    TypedExp {
                      kind: ExpKind::Block(prefix_statements),
                      data: t,
                      source_trace: SourceTrace::empty(),
                    }
                  });
                }
                Match(_, _) => {
                  let name: Rc<str> =
                    format!("match_gensym_{gensym_index}").into();
                  gensym_index += 1;
                  let mut match_exp = TypedExp {
                    kind: Name(name.clone()),
                    data: arg.data.clone(),
                    source_trace: SourceTrace::empty(),
                  };
                  std::mem::swap(*arg, &mut match_exp);
                  take(exp, |exp| TypedExp {
                    data: exp.data.clone(),
                    kind: ExpKind::Let(
                      vec![(name, VariableKind::Let, match_exp)],
                      exp.into(),
                    ),
                    source_trace: SourceTrace::empty(),
                  });
                }
                Let(bindings, body) => {
                  let mut extracted_bindings = vec![];
                  std::mem::swap(&mut extracted_bindings, bindings);
                  let mut temp = placeholder_exp.clone();
                  std::mem::swap(&mut temp, body);
                  std::mem::swap(*arg, &mut temp);
                  take(exp, |exp| TypedExp {
                    data: exp.data.clone(),
                    kind: ExpKind::Let(extracted_bindings, exp.into()),
                    source_trace: SourceTrace::empty(),
                  });
                }
                _ => unreachable!(),
              };
            }
            true
          }
          Let(items, body) => {
            loop {
              let mut restructured = false;
              let mut index_to_remove: Option<usize> = None;
              for (index, (binding_name, variable_kind, value)) in
                items.iter_mut().enumerate()
              {
                match &value.kind {
                  Let(_, _) => {
                    restructured = true;
                    let mut inner_value = placeholder_exp_kind.clone();
                    std::mem::swap(&mut inner_value, &mut value.kind);
                    let Let(inner_bindings, mut inner_body) = inner_value
                    else {
                      unreachable!()
                    };
                    std::mem::swap(&mut *inner_body, &mut items[index].2);
                    items.splice(index..index, inner_bindings);
                    break;
                  }
                  Block { .. } => {
                    restructured = true;
                    let mut inner_statements = vec![];
                    let Block(expressions) = &mut value.kind else {
                      unreachable!()
                    };
                    std::mem::swap(&mut inner_statements, expressions);
                    match inner_statements.len() {
                      0 => {
                        index_to_remove = Some(index);
                      }
                      1 => {
                        std::mem::swap(value, &mut inner_statements[0]);
                      }
                      _ => {
                        let mut binding_value = inner_statements.pop().unwrap();
                        take(body, |body| {
                          let body_type = body.data.clone();
                          let mut inner_bindings = items.split_off(index);
                          std::mem::swap(
                            &mut inner_bindings.first_mut().unwrap().2,
                            &mut binding_value,
                          );
                          inner_statements.push(TypedExp {
                            kind: ExpKind::Let(inner_bindings, body.into()),
                            data: body_type.clone(),
                            source_trace: SourceTrace::empty(),
                          });
                          TypedExp {
                            kind: ExpKind::Block(inner_statements),
                            source_trace: SourceTrace::empty(),
                            data: body_type,
                          }
                          .into()
                        });
                      }
                    }
                    break;
                  }
                  Match(_, _) => {
                    restructured = true;
                    *variable_kind = VariableKind::Var;
                    let mut match_exp = TypedExp {
                      kind: ExpKind::Uninitialized,
                      data: value.data.clone(),
                      source_trace: SourceTrace::empty(),
                    };
                    std::mem::swap(&mut match_exp, value);
                    let binding_type = value.data.clone();
                    let binding_name = binding_name.clone();
                    let inner_bindings = items.split_off(index + 1);
                    let body_type = body.data.clone();
                    take(body, |body| {
                      let Match(_, arms) = &mut match_exp.kind else {
                        unreachable!()
                      };
                      for (_, arm_body) in arms.iter_mut() {
                        take(arm_body, |arm_body| TypedExp {
                          data: TypeState::Known(Type::Unit).into(),
                          kind: ExpKind::Application(
                            TypedExp {
                              kind: ExpKind::Name("=".into()),
                              data: TypeState::Known(Type::Function(
                                FunctionSignature {
                                  abstract_ancestor: None,
                                  arg_types: vec![
                                    (binding_type.clone(), vec![]),
                                    (binding_type.clone(), vec![]),
                                  ],
                                  mutated_args: vec![0],
                                  return_type: TypeState::Known(Type::Unit)
                                    .into(),
                                }
                                .into(),
                              ))
                              .into(),
                              source_trace: SourceTrace::empty(),
                            }
                            .into(),
                            vec![
                              TypedExp {
                                data: binding_type.clone(),
                                kind: ExpKind::Name(binding_name.clone()),
                                source_trace: SourceTrace::empty(),
                              },
                              arm_body,
                            ],
                          ),
                          source_trace: SourceTrace::empty(),
                        });
                      }
                      TypedExp {
                        kind: ExpKind::Block(vec![
                          match_exp,
                          TypedExp {
                            data: body_type.clone(),
                            kind: ExpKind::Let(inner_bindings, body.into()),
                            source_trace: SourceTrace::empty(),
                          },
                        ]),
                        source_trace: SourceTrace::empty(),
                        data: body_type,
                      }
                      .into()
                    });
                    break;
                  }
                  _ => {}
                }
              }
              if let Some(index) = index_to_remove {
                items.remove(index);
              }
              if !restructured {
                break;
              }
            }
            true
          }
          Block(inner_exps) => {
            let initial_len = inner_exps.len();
            take(inner_exps, |mut inner_exps| {
              if let Some(last) = inner_exps.pop() {
                inner_exps
                  .into_iter()
                  .filter_map(|exp| {
                    (!exp.effects(program).is_pure_but_for_reads()).then(|| exp)
                  })
                  .chain(std::iter::once(last))
                  .collect()
              } else {
                inner_exps
              }
            });
            if inner_exps.len() != initial_len {
              changed = true;
            }
            true
          }
          _ => true,
        })
      });
      if !changed {
        break;
      }
    }
  }
  pub fn desugar_swizzle_assignments(&mut self) {
    if let ExpKind::Application(f, args) = &self.kind
      && let ExpKind::Name(f_name) = &f.kind
      && &**f_name == "="
      && let Some(first_arg) = args.get(0)
      && let Some(second_arg) = args.get(1)
      && let ExpKind::Access(accessor, accessed) = &first_arg.kind
      && let Accessor::Swizzle(fields) = accessor
    {
      let gensym_name: Rc<str> = "gensym".into();
      std::mem::swap(
        self,
        &mut Exp {
          kind: ExpKind::Let(
            vec![(gensym_name.clone(), VariableKind::Let, second_arg.clone())],
            Exp {
              kind: ExpKind::Block(
                fields
                  .iter()
                  .enumerate()
                  .map(|(i, swizzle_field)| {
                    let Type::Struct(s) = accessed.data.unwrap_known() else {
                      unreachable!("swizzle on non-vector")
                    };
                    let field_name = swizzle_field.name();
                    let field_type = s
                      .fields
                      .iter()
                      .find_map(|struct_field| {
                        (&*struct_field.name == field_name)
                          .then(|| struct_field.field_type.clone())
                      })
                      .expect("couldn't find field when desugaring swizzle");
                    Exp {
                      kind: ExpKind::Application(f.clone(), {
                        vec![
                          Exp {
                            kind: ExpKind::Access(
                              Accessor::Field(field_name.into()),
                              accessed.clone(),
                            ),
                            data: field_type.clone(),
                            source_trace: self.source_trace.clone(),
                          },
                          Exp {
                            kind: ExpKind::Access(
                              Accessor::Field(
                                match i {
                                  0 => "x",
                                  1 => "y",
                                  2 => "z",
                                  3 => "w",
                                  _ => unreachable!(),
                                }
                                .into(),
                              ),
                              Exp {
                                kind: ExpKind::Name(gensym_name.clone()),
                                data: second_arg.data.clone(),
                                source_trace: self.source_trace.clone(),
                              }
                              .into(),
                            ),
                            data: field_type.clone(),
                            source_trace: self.source_trace.clone(),
                          },
                        ]
                      }),
                      data: TypeState::Known(Type::Unit).into(),
                      source_trace: self.source_trace.clone(),
                    }
                  })
                  .collect(),
              ),
              data: self.data.clone(),
              source_trace: self.source_trace.clone(),
            }
            .into(),
          ),
          data: self.data.clone(),
          source_trace: self.source_trace.clone(),
        },
      )
    }
  }
}
