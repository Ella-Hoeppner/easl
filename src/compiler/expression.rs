use core::fmt::Debug;
use sse::syntax::EncloserOrOperator;
use std::{
  cell::RefCell,
  collections::{HashMap, HashSet},
  rc::Rc,
};
use take_mut::take;

use crate::{
  compiler::{
    builtins::{get_builtin_struct, rename_builtin, ASSIGNMENT_OPS, INFIX_OPS},
    error::{err, CompileError, CompileErrorKind::*, CompileResult},
    functions::FunctionSignature,
    metadata::{extract_metadata, Metadata},
    program::Program,
    structs::AbstractStruct,
    types::{
      extract_type_annotation, extract_type_annotation_ast, ArraySize,
      ExpTypeInfo, Type,
      TypeState::{self, *},
      Variable, VariableKind,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Accessor {
  Field(Rc<str>),
  Swizzle(Vec<SwizzleField>),
  ArrayIndex(Box<TypedExp>),
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
            &vec![],
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
            &vec![],
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
      Accessor::ArrayIndex(exp) => format!(
        "[{}]",
        exp.compile(ExpressionCompilationPosition::InnerExpression)
      )
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
  error::{CompileErrorKind, SourceTrace, SourceTraceKind},
  functions::FunctionImplementationKind,
  structs::compiled_vec_name,
  types::{AbstractType, LocalContext, TypeConstraint},
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
  structs: &Vec<Rc<AbstractStruct>>,
  aliases: &Vec<(Rc<str>, Rc<AbstractStruct>)>,
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
    let (return_type_ast, return_metadata, metadata_errors) =
      extract_metadata(return_type_ast);
    if let Some(metadata_error) = metadata_errors.get(0) {
      return Err(metadata_error.clone());
    }
    let return_type =
      AbstractType::from_easl_tree(return_type_ast, structs, aliases, skolems)?;
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
          let t =
            AbstractType::from_easl_tree(t_ast, structs, aliases, skolems)?;
          let (arg_name_ast, arg_metadata, metadata_errors) =
            extract_metadata(arg_name_ast);
          if let Some(metadata_error) = metadata_errors.get(0) {
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
    structs: &Vec<Rc<AbstractStruct>>,
    aliases: &Vec<(Rc<str>, Rc<AbstractStruct>)>,
    skolems: &Vec<Rc<str>>,
  ) -> CompileResult<Self> {
    let mut body_exps = body_trees
      .into_iter()
      .map(|t| {
        Self::try_from_easl_tree(
          t,
          structs,
          aliases,
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
        source_trace: body_exps
          .iter()
          .map(|body_exp| body_exp.source_trace.clone())
          .collect(),
        kind: ExpKind::Block(body_exps),
      }
    };
    Ok(Exp {
      data: Known(Type::Function(Box::new(FunctionSignature {
        abstract_ancestor: None,
        arg_types,
        return_type,
      })))
      .into(),
      kind: ExpKind::Function(arg_names, Box::new(body)),
      source_trace,
    })
  }
  pub fn try_from_easl_tree(
    tree: EaslTree,
    structs: &Vec<Rc<AbstractStruct>>,
    aliases: &Vec<(Rc<str>, Rc<AbstractStruct>)>,
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
            return err(InvalidToken(leaf), source_trace);
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
        let source_trace: SourceTrace = position.into();
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
                              structs,
                              aliases,
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
                                child, structs, aliases, skolems, ctx,
                              )
                            })
                            .collect::<CompileResult<Vec<Self>>>()?;
                          let body_exp = if child_exps.len() == 1 {
                            child_exps.remove(0)
                          } else {
                            Exp {
                              data: Unknown.into(),
                              kind: ExpKind::Block(child_exps),
                              source_trace: source_trace.clone(),
                            }
                          };
                          if let EaslTree::Inner(
                            (position, Encloser(Square)),
                            binding_asts,
                          ) = bindings_ast
                          {
                            let source_trace: SourceTrace = position.into();
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
                                  name_ast, structs, aliases, skolems,
                                )?;
                                let (name_ast, name_metadata, metadata_errors) =
                                  extract_metadata(name_ast);
                                if let Some(metadata_error) =
                                  metadata_errors.get(0)
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
                                                  Metadata::Singular(tag),
                                                ),
                                                source_trace,
                                              )
                                            }
                                          }
                                        }
                                        Some((
                                          metadata,
                                          metadata_source_trace,
                                        )) => {
                                          return err(
                                            InvalidVariableMetadata(metadata),
                                            metadata_source_trace,
                                          )
                                        }
                                      },
                                      {
                                        let mut value_exp =
                                          Self::try_from_easl_tree(
                                            value_ast, structs, aliases,
                                            skolems, ctx,
                                          )?;
                                        if let Some(ty) = ty {
                                          value_exp.data =
                                            TypeState::Known(ty.concretize(
                                              skolems,
                                              structs,
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
                                source_trace,
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
                                child, structs, aliases, skolems, ctx,
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
                            structs,
                            aliases,
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
                                structs,
                                aliases,
                                skolems,
                                SyntaxTreeContext::MatchPattern,
                              )?,
                              Self::try_from_easl_tree(
                                value_subtree,
                                structs,
                                aliases,
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
                                      structs,
                                      aliases,
                                      skolems,
                                    )
                                  })
                                  .unwrap_or(Ok(Type::I32))?,
                                TypedExp::try_from_easl_tree(
                                  increment_variable_initial_value_subtree,
                                  structs,
                                  aliases,
                                  skolems,
                                  ctx,
                                )?,
                                TypedExp::try_from_easl_tree(
                                  continue_condition_subtree,
                                  structs,
                                  aliases,
                                  skolems,
                                  ctx,
                                )?,
                                TypedExp::try_from_easl_tree(
                                  update_condition_subtree,
                                  structs,
                                  aliases,
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
                                child, structs, aliases, skolems, ctx,
                              )
                            })
                            .collect::<CompileResult<Vec<TypedExp>>>()?;
                          let body_source_trace: SourceTrace = body_expressions
                            .iter()
                            .map(|exp| exp.source_trace.clone())
                            .collect();
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
                                source_trace: body_source_trace,
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
                                child, structs, aliases, skolems, ctx,
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
                          let body_source_trace: SourceTrace = sub_expressions
                            .iter()
                            .map(|exp| exp.source_trace.clone())
                            .collect();
                          Some(Exp {
                            kind: ExpKind::WhileLoop {
                              condition_expression: Box::new(
                                condition_expression,
                              ),
                              body_expression: Box::new(TypedExp {
                                data: TypeState::Known(Type::Unit).into(),
                                kind: ExpKind::Block(sub_expressions),
                                source_trace: body_source_trace,
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
                              structs,
                              aliases,
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
                      structs,
                      aliases,
                      skolems,
                      ctx,
                    )?),
                    children_iter
                      .map(|arg| {
                        Self::try_from_easl_tree(
                          arg, structs, aliases, skolems, ctx,
                        )
                      })
                      .collect::<CompileResult<_>>()?,
                  ),
                  data: Unknown.into(),
                  source_trace,
                })
              } else {
                Exp {
                  data: TypeState::Known(Type::Unit).into(),
                  kind: ExpKind::Unit,
                  source_trace,
                }
              }
            }
            ArrayLookup => {
              if children_iter.len() == 2 {
                let array_expression = TypedExp::try_from_easl_tree(
                  children_iter.next().unwrap(),
                  structs,
                  aliases,
                  skolems,
                  ctx,
                )?;
                let index_expression = TypedExp::try_from_easl_tree(
                  children_iter.next().unwrap(),
                  structs,
                  aliases,
                  skolems,
                  ctx,
                )?;
                Exp {
                  kind: Access(
                    Accessor::ArrayIndex(Box::new(index_expression)),
                    Box::new(array_expression),
                  ),
                  data: TypeState::Unknown.into(),
                  source_trace,
                }
              } else {
                return err(InvalidArrayAccessSyntax, source_trace);
              }
            }
            Square => Exp {
              data: TypeState::Known(Type::Array(
                Some(ArraySize::Constant(children_iter.len() as u32)),
                Box::new(TypeState::Unknown.into()),
              ))
              .into(),
              kind: ArrayLiteral(
                children_iter
                  .map(|ast| {
                    TypedExp::try_from_easl_tree(
                      ast, structs, aliases, skolems, ctx,
                    )
                  })
                  .collect::<CompileResult<Vec<TypedExp>>>()?,
              ),
              source_trace,
            },
            Curly => return err(AnonymousStructsNotYetSupported, source_trace),
            LineComment => {
              return err(EncounteredCommentInSource, source_trace)
            }
            BlockComment => {
              return err(EncounteredCommentInSource, source_trace)
            }
          },
          Operator(o) => match o {
            MetadataAnnotation => {
              return err(EncounteredMetadataInInternalExpression, source_trace)
            }
            TypeAnnotation => {
              let mut exp = Self::try_from_easl_tree(
                children_iter.next().unwrap(),
                structs,
                aliases,
                skolems,
                ctx,
              )?;
              exp.data = TypeState::Known(Type::from_easl_tree(
                children_iter.next().unwrap(),
                structs,
                aliases,
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
                  structs,
                  aliases,
                  skolems,
                  ctx,
                )?
                .into(),
              ),
              source_trace,
            },
          },
        }
      }
    })
  }
  pub fn walk(
    &self,
    prewalk_handler: &mut impl FnMut(&Self) -> CompileResult<bool>,
  ) -> CompileResult<()> {
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
      Application(f, args) => {
        let f_str = if let ExpKind::Name(name) = f.kind {
          rename_builtin(&*name).unwrap_or_else(|| {
            if ASSIGNMENT_OPS.contains(name.as_ref())
              || INFIX_OPS.contains(name.as_ref())
            {
              name.to_string()
            } else {
              compile_word(name)
            }
          })
        } else {
          f.compile(InnerExpression)
        };
        let arg_strs: Vec<String> = args
          .into_iter()
          .map(|arg| arg.compile(InnerExpression))
          .collect();
        wrap(if ASSIGNMENT_OPS.contains(&f_str.as_str()) {
          if arg_strs.len() == 2 {
            format!("{} {} {}", arg_strs[0], f_str, arg_strs[1])
          } else {
            panic!("{} arguments to assignment op, expected 2", arg_strs.len())
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
        } else {
          let args_str = arg_strs.join(", ");
          format!("{f_str}({args_str})")
        })
      }
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
      Match(scrutinee, arms) => {
        if scrutinee.data.unwrap_known() == Type::Bool {
          match arms.len() {
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
          }
        } else {
          format!(
            "\nswitch ({}) {{\n  {}\n}}",
            scrutinee.compile(InnerExpression),
            indent(
              arms
                .into_iter()
                .map(|(pattern, value)| format!(
                  "{}: {{{}\n}}",
                  if pattern.kind == Wildcard {
                    "default".to_string()
                  } else {
                    "case ".to_string() + &pattern.compile(InnerExpression)
                  },
                  if value.kind == ExpKind::Unit {
                    "".to_string()
                  } else {
                    indent(value.compile(position))
                  }
                ))
                .collect::<Vec<String>>()
                .join("\n")
            )
          )
        }
      }
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

  pub fn find_untyped(&mut self) -> Vec<TypedExp> {
    let mut untyped = vec![];
    self
      .walk_mut::<()>(&mut |exp| {
        if !exp.data.is_fully_known() {
          untyped.push(exp.clone());
        }
        Ok(true)
      })
      .unwrap();
    untyped
  }
  pub fn validate_match_blocks(&self, errors: &mut Vec<CompileError>) {
    self
      .walk(&mut |exp| {
        match &exp.kind {
          Match(exp, items) => {
            if let TypeState::Known(t) = &exp.data.kind {
              let mut first_wildcard: Option<SourceTrace> = None;
              let mut all_pattern_values: Vec<&ExpKind<ExpTypeInfo>> = vec![];
              for (pattern, _) in items.iter() {
                if let Some(first_wildcard) = &first_wildcard {
                  errors.push(CompileError {
                    kind: CompileErrorKind::PatternAfterWildcard,
                    source_trace: pattern
                      .source_trace
                      .clone()
                      .combine_with(first_wildcard.clone()),
                  })
                }
                if all_pattern_values.contains(&&pattern.kind) {
                  errors.push(CompileError {
                    kind: CompileErrorKind::DuplicatePattern,
                    source_trace: pattern.source_trace.clone(),
                  })
                } else {
                  match &pattern.kind {
                    Wildcard => {
                      if first_wildcard.is_none() {
                        first_wildcard = Some(pattern.source_trace.clone())
                      }
                    }
                    NumberLiteral(_) | BooleanLiteral(_) => {}
                    _ => errors.push(CompileError {
                      kind: CompileErrorKind::InvalidPattern,
                      source_trace: pattern.source_trace.clone(),
                    }),
                  }
                  all_pattern_values.push(&pattern.kind);
                }
              }
              if first_wildcard.is_none()
                && !(*t == Type::Bool && all_pattern_values.len() == 2)
              {
                errors.push(CompileError {
                  kind: CompileErrorKind::NonexhaustiveMatch,
                  source_trace: exp.source_trace.clone(),
                });
              }
            }
          }
          _ => {}
        }
        Ok(true)
      })
      .unwrap();
  }
  fn propagate_types_inner(
    &mut self,
    ctx: &mut LocalContext,
  ) -> (bool, Vec<CompileError>) {
    if self.data.subtree_fully_typed {
      return (false, vec![]);
    }
    let mut errors = vec![];
    let changed = match &mut self.kind {
      Uninitialized => {
        self.data.subtree_fully_typed = true;
        false
      }
      Wildcard => {
        self.data.subtree_fully_typed = true;
        false
      }
      Unit => {
        self.data.subtree_fully_typed = true;
        let (changed, mut inner_errors) = self
          .data
          .constrain(TypeState::Known(Type::Unit), self.source_trace.clone());
        errors.append(&mut inner_errors);
        changed
      }
      Name(name) => {
        self.data.subtree_fully_typed = true;
        if !ctx.is_bound(name) {
          errors.push(CompileError::new(
            UnboundName(name.clone()),
            self.source_trace.clone(),
          ));
        }
        let (changed, mut inner_errors) = ctx.constrain_name_type(
          name,
          self.source_trace.clone(),
          &mut self.data,
        );
        errors.append(&mut inner_errors);
        changed
      }
      NumberLiteral(num) => {
        self.data.subtree_fully_typed = true;
        let (changed, mut inner_errors) = self.data.constrain(
          match num {
            Number::Int(_) => {
              TypeState::OneOf(vec![Type::I32, Type::U32, Type::F32])
            }
            Number::Float(_) => TypeState::Known(Type::F32),
          },
          self.source_trace.clone(),
        );
        errors.append(&mut inner_errors);
        changed
      }
      BooleanLiteral(_) => {
        self.data.subtree_fully_typed = true;
        let (changed, mut inner_errors) = self
          .data
          .constrain(TypeState::Known(Type::Bool), self.source_trace.clone());
        errors.append(&mut inner_errors);
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
              let (return_type_changed, mut body_errors) =
                body.data.mutually_constrain(
                  return_type_state,
                  self.source_trace.clone(),
                );
              errors.append(&mut body_errors);
              if arg_count == arg_names.len() {
                for (name, t) in arg_names.iter().zip(arg_type_states) {
                  ctx.bind(name, Variable::new(t.clone()))
                }
                let (body_types_changed, mut body_type_errors) =
                  body.propagate_types_inner(ctx);
                errors.append(&mut body_type_errors);
                let argument_types = arg_names
                  .iter()
                  .map(|name| ctx.unbind(name).typestate.kind)
                  .collect::<Vec<_>>();
                let (fn_type_changed, mut fn_errors) =
                  self.data.constrain_fn_by_argument_types(
                    argument_types,
                    self.source_trace.clone(),
                  );
                errors.append(&mut fn_errors);
                self.data.subtree_fully_typed = body.data.subtree_fully_typed;
                return_type_changed || body_types_changed || fn_type_changed
              } else {
                errors.push(CompileError::new(
                  WrongArity(name),
                  self.source_trace.clone(),
                ));
                false
              }
            }
            _ => {
              errors.push(CompileError::new(
                FunctionExpressionHasNonFunctionType(f_type.clone()),
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
          let (arg_changed, mut arg_errors) = arg.propagate_types_inner(ctx);
          anything_changed |= arg_changed;
          errors.append(&mut arg_errors);
        }
        if let Name(name) = &f.kind {
          let (name_changed, mut name_errors) = ctx.constrain_name_type(
            name,
            self.source_trace.clone(),
            &mut f.data,
          );
          anything_changed |= name_changed;
          errors.append(&mut name_errors);
        } else {
          let (f_changed, mut f_errors) = f.data.constrain(
            TypeState::Known(Type::Function(Box::new(FunctionSignature {
              abstract_ancestor: None,
              arg_types: args
                .iter()
                .map(|arg| (arg.data.clone(), vec![]))
                .collect(),
              return_type: self.data.clone(),
            }))),
            f.source_trace.clone(),
          );
          anything_changed |= f_changed;
          errors.append(&mut f_errors);
        }
        if let TypeState::Known(f_type) = &mut f.data.kind {
          if let Type::Function(signature) = f_type {
            if signature
              .abstract_ancestor
              .as_ref()
              .map(|ancestor| ancestor.associative)
              .unwrap_or(false)
              || args.len() == signature.arg_types.len()
            {
              let (changed, mut return_errors) = self.data.mutually_constrain(
                &mut signature.return_type,
                self.source_trace.clone(),
              );
              anything_changed |= changed;
              errors.append(&mut return_errors);
              for (arg, (t, _)) in
                args.iter_mut().zip(signature.arg_types.iter().cloned())
              {
                let (changed, mut arg_errors) =
                  arg.data.constrain(t.kind, self.source_trace.clone());
                anything_changed |= changed;
                errors.append(&mut arg_errors);
              }
            } else {
              errors.push(CompileError::new(
                WrongArity(signature.name()),
                self.source_trace.clone(),
              ));
            }
          } else {
            errors.push(CompileError::new(
              AppliedNonFunction,
              self.source_trace.clone(),
            ));
          }
        }
        let (changed, mut f_errors) = f.data.constrain_fn_by_argument_types(
          args.iter().map(|arg| arg.data.kind.clone()).collect(),
          self.source_trace.clone(),
        );
        anything_changed |= changed;
        errors.append(&mut f_errors);
        let (changed, mut f_errors) = f.propagate_types_inner(ctx);
        anything_changed |= changed;
        errors.append(&mut f_errors);
        self.data.subtree_fully_typed =
          args.iter().fold(f.data.subtree_fully_typed, |acc, arg| {
            acc && arg.data.subtree_fully_typed
          });

        anything_changed
      }
      Access(accessor, subexp) => match accessor {
        Accessor::Field(field_name) => {
          let (mut anything_changed, mut subexp_errors) =
            subexp.propagate_types_inner(ctx);
          errors.append(&mut subexp_errors);
          if let Known(t) = &mut subexp.data.kind {
            if let Type::Struct(s) = t {
              if let Some(x) =
                &mut s.fields.iter_mut().find(|f| f.name == *field_name)
              {
                let (changed, mut sub_errors) = self.data.mutually_constrain(
                  &mut x.field_type,
                  self.source_trace.clone(),
                );
                anything_changed |= changed;
                errors.append(&mut sub_errors);
              } else {
                errors.push(CompileError::new(
                  NoSuchField {
                    struct_name: s.abstract_ancestor.name.clone(),
                    field_name: field_name.clone(),
                  },
                  self.source_trace.clone(),
                ));
              }
            } else {
              errors.push(CompileError::new(
                AccessorOnNonStruct,
                self.source_trace.clone(),
              ));
            }
          }
          self.data.subtree_fully_typed = subexp.data.subtree_fully_typed;
          anything_changed
        }
        Accessor::Swizzle(fields) => {
          let (mut anything_changed, mut sub_errors) = self.data.constrain(
            swizzle_accessor_typestate(&subexp.data, fields).kind,
            self.source_trace.clone(),
          );
          errors.append(&mut sub_errors);
          let (changed, mut sub_errors) = subexp.data.constrain(
            swizzle_accessed_possibilities(fields).kind,
            self.source_trace.clone(),
          );
          errors.append(&mut sub_errors);
          anything_changed |= changed;
          let (changed, mut sub_errors) = subexp.propagate_types_inner(ctx);
          anything_changed |= changed;
          errors.append(&mut sub_errors);
          self.data.subtree_fully_typed = subexp.data.subtree_fully_typed;
          anything_changed
        }
        Accessor::ArrayIndex(index_expression) => {
          let (mut anything_changed, mut index_errors) =
            index_expression.data.constrain(
              TypeState::OneOf(vec![Type::I32, Type::U32]),
              self.source_trace.clone(),
            );
          errors.append(&mut index_errors);
          if let TypeState::Known(t) = &mut subexp.data.kind {
            if let Type::Array(_, inner_type) = t {
              let (changed, mut sub_errors) = self.data.mutually_constrain(
                inner_type.as_mut(),
                self.source_trace.clone(),
              );
              anything_changed |= changed;
              errors.append(&mut sub_errors);
            } else {
              errors.push(CompileError::new(
                ArrayAccessOnNonArray,
                self.source_trace.clone(),
              ));
            }
          }
          let (changed, mut sub_errors) =
            index_expression.propagate_types_inner(ctx);
          anything_changed |= changed;
          errors.append(&mut sub_errors);
          let (changed, mut sub_errors) = subexp.propagate_types_inner(ctx);
          anything_changed |= changed;
          errors.append(&mut sub_errors);
          self.data.subtree_fully_typed = subexp.data.subtree_fully_typed
            && index_expression.data.subtree_fully_typed;
          anything_changed
        }
      },
      Let(bindings, body) => {
        let (mut anything_changed, mut body_errors) = body
          .data
          .mutually_constrain(&mut self.data, self.source_trace.clone());
        errors.append(&mut body_errors);
        for (name, _, value) in bindings.iter_mut() {
          let (changed, mut sub_errors) = value.propagate_types_inner(ctx);
          anything_changed |= changed;
          errors.append(&mut sub_errors);
          ctx.bind(name, Variable::new(value.data.clone()));
        }
        let (changed, mut sub_errors) = body.propagate_types_inner(ctx);
        anything_changed |= changed;
        errors.append(&mut sub_errors);
        for (name, _, value) in bindings.iter_mut() {
          let (changed, mut sub_errors) = value.data.constrain(
            ctx.unbind(name).typestate.kind,
            self.source_trace.clone(),
          );
          anything_changed |= changed;
          errors.append(&mut sub_errors);
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
        let (mut anything_changed, mut scrutinee_errors) =
          scrutinee.propagate_types_inner(ctx);
        errors.append(&mut scrutinee_errors);
        for (case, value) in arms.iter_mut() {
          let (changed, mut sub_errors) = case.propagate_types_inner(ctx);
          anything_changed |= changed;
          errors.append(&mut sub_errors);
          let (changed, mut sub_errors) = value.propagate_types_inner(ctx);
          anything_changed |= changed;
          errors.append(&mut sub_errors);
          let (changed, mut sub_errors) = case
            .data
            .mutually_constrain(&mut scrutinee.data, self.source_trace.clone());
          anything_changed |= changed;
          errors.append(&mut sub_errors);
          let (changed, mut sub_errors) = value
            .data
            .mutually_constrain(&mut self.data, self.source_trace.clone());
          anything_changed |= changed;
          errors.append(&mut sub_errors);
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
          let (changed, mut sub_errors) = child.propagate_types_inner(ctx);
          anything_changed |= changed;
          errors.append(&mut sub_errors);
        }
        if let Some(exp) = expressions.last_mut() {
          let (changed, mut sub_errors) = self
            .data
            .mutually_constrain(&mut exp.data, self.source_trace.clone());
          anything_changed |= changed;
          errors.append(&mut sub_errors);
        } else {
          errors.push(CompileError::new(EmptyBlock, self.source_trace.clone()));
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
        let (mut anything_changed, mut sub_errors) =
          increment_variable_initial_value_expression
            .data
            .mutually_constrain(
              &mut variable_typestate,
              increment_variable_initial_value_expression
                .source_trace
                .clone(),
            );
        errors.append(&mut sub_errors);
        let (changed, mut sub_errors) =
          increment_variable_initial_value_expression
            .propagate_types_inner(ctx);
        anything_changed |= changed;
        errors.append(&mut sub_errors);
        ctx.bind(
          increment_variable_name,
          Variable {
            kind: VariableKind::Var,
            typestate: variable_typestate.into(),
          },
        );
        let (changed, mut sub_errors) =
          continue_condition_expression.data.constrain(
            TypeState::Known(Type::Bool),
            continue_condition_expression.source_trace.clone(),
          );
        anything_changed |= changed;
        errors.append(&mut sub_errors);
        let (changed, mut sub_errors) =
          update_condition_expression.data.constrain(
            TypeState::Known(Type::Unit),
            update_condition_expression.source_trace.clone(),
          );
        anything_changed |= changed;
        errors.append(&mut sub_errors);
        let (changed, mut sub_errors) =
          continue_condition_expression.propagate_types_inner(ctx);
        anything_changed |= changed;
        errors.append(&mut sub_errors);
        let (changed, mut sub_errors) =
          update_condition_expression.propagate_types_inner(ctx);
        anything_changed |= changed;
        errors.append(&mut sub_errors);
        let (changed, mut sub_errors) =
          body_expression.propagate_types_inner(ctx);
        anything_changed |= changed;
        errors.append(&mut sub_errors);
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
        let (mut anything_changed, mut sub_errors) =
          condition_expression.data.constrain(
            TypeState::Known(Type::Bool),
            condition_expression.source_trace.clone(),
          );
        errors.append(&mut sub_errors);
        let (changed, mut sub_errors) = body_expression.data.constrain(
          TypeState::Known(Type::Unit),
          condition_expression.source_trace.clone(),
        );
        anything_changed |= changed;
        errors.append(&mut sub_errors);
        let (changed, mut sub_errors) =
          condition_expression.propagate_types_inner(ctx);
        anything_changed |= changed;
        errors.append(&mut sub_errors);
        let (changed, mut sub_errors) =
          body_expression.propagate_types_inner(ctx);
        anything_changed |= changed;
        errors.append(&mut sub_errors);
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
        let changed = if let Some(t) = ctx.enclosing_function_type() {
          match t.as_fn_type_if_known(|| {
            CompileError::new(
              CompileErrorKind::EnclosingFunctionTypeWasntFunction,
              self.source_trace.clone(),
            )
          }) {
            Ok(fn_type) => {
              let (changed, mut sub_errors) = exp.data.mutually_constrain(
                &mut fn_type
                  .map(|fn_type| &mut fn_type.return_type)
                  .unwrap_or(&mut TypeState::Unknown.into()),
                self.source_trace.clone(),
              );
              errors.append(&mut sub_errors);
              changed
            }
            Err(e) => {
              errors.push(e);
              false
            }
          }
        } else {
          errors.push(CompileError::new(
            ReturnOutsideFunction,
            self.source_trace.clone(),
          ));
          false
        };
        self.data.subtree_fully_typed = exp.data.subtree_fully_typed;
        changed
      }
      ArrayLiteral(children) => {
        let mut anything_changed = false;
        for child in children.iter_mut() {
          self.data.as_known_mut(|array_type| {
            if let Type::Array(_, inner_type) = array_type {
              let (changed, mut sub_errors) = child.data.mutually_constrain(
                inner_type.as_mut(),
                SourceTrace {
                  kind: SourceTraceKind::Combination(vec![
                    child.source_trace.clone(),
                    self.source_trace.clone(),
                  ])
                  .into(),
                },
              );
              anything_changed |= changed;
              errors.append(&mut sub_errors);
              let (changed, mut sub_errors) = child.propagate_types_inner(ctx);
              anything_changed |= changed;
              errors.append(&mut sub_errors);
            } else {
              unreachable!()
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
        let (mut anything_changed, mut sub_errors) =
          exp.propagate_types_inner(ctx);
        errors.append(&mut sub_errors);
        let TypeState::Known(Type::Reference(inner_type)) = &mut self.data.kind
        else {
          unreachable!()
        };
        let (changed, mut sub_errors) = exp
          .data
          .mutually_constrain(inner_type, self.source_trace.clone());
        anything_changed |= changed;
        errors.append(&mut sub_errors);
        self.data.subtree_fully_typed = exp.data.subtree_fully_typed;
        anything_changed
      }
      ZeroedArray => {
        self.data.subtree_fully_typed = true;
        false
      }
    };
    self.data.subtree_fully_typed &= self.data.check_is_fully_known();
    (changed, errors)
  }
  pub fn propagate_types(
    &mut self,
    program: &mut Program,
  ) -> (bool, Vec<CompileError>) {
    self.propagate_types_inner(&mut LocalContext::empty(program))
  }
  fn name_or_inner_accessed_name(&self) -> Option<&Rc<str>> {
    let mut exp = self;
    loop {
      if let ExpKind::Access(_, inner_exp) = &exp.kind {
        exp = inner_exp;
      } else {
        break;
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
    ctx: &mut LocalContext,
  ) -> CompileResult<()> {
    self.walk(&mut |exp| {
      Ok(match &exp.kind {
        Application(f, args) => {
          if let ExpKind::Name(f_name) = &f.kind {
            if ASSIGNMENT_OPS.contains(&&**f_name) {
              if let Some(var_name) = args[0].name_or_inner_accessed_name() {
                if ctx.get_variable_kind(var_name) != VariableKind::Var {
                  return err(
                    AssignmentTargetMustBeVariable(var_name.clone()),
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
  pub fn validate_assignments(
    &self,
    program: &mut Program,
  ) -> CompileResult<()> {
    self.validate_assignments_inner(&mut LocalContext::empty(program))
  }
  pub fn replace_skolems(&mut self, skolems: &Vec<(Rc<str>, Type)>) {
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
      if let Application(f, args) = &mut exp.kind {
        if let ExpKind::Name(f_name) = &mut f.kind {
          if let Some(abstract_signature) =
            if let TypeState::Known(Type::Function(f)) = &f.data.kind {
              &f.abstract_ancestor
            } else {
              unreachable!("encountered applied non-fn in monomorphization")
            }
          {
            match &abstract_signature.implementation {
              FunctionImplementationKind::Builtin => match &**f_name {
                "vec2" | "vec3" | "vec4" => {
                  if let Type::Struct(s) = &exp.data.kind.unwrap_known() {
                    if let Some(mut compiled_name) = compiled_vec_name(
                      &f_name,
                      s.fields[0].field_type.unwrap_known(),
                    ) {
                      std::mem::swap(f_name, &mut compiled_name);
                    }
                  } else {
                    unreachable!("")
                  }
                }
                "bitcast" => std::mem::swap(
                  f_name,
                  &mut format!(
                    "bitcast<{}>",
                    exp.data.kind.unwrap_known().compile()
                  )
                  .into(),
                ),
                _ => {}
              },
              FunctionImplementationKind::Constructor => {
                if let Some(abstract_struct) =
                  base_program.structs.iter().find(|s| s.name == *f_name)
                {
                  let arg_types: Vec<Type> =
                    args.iter().map(|arg| arg.data.unwrap_known()).collect();
                  if let Some(monomorphized_struct) = abstract_struct
                    .generate_monomorphized(
                      arg_types.clone(),
                      exp.source_trace.clone(),
                    )
                  {
                    let monomorphized_struct = Rc::new(monomorphized_struct);
                    std::mem::swap(
                      f_name,
                      &mut AbstractStruct::concretized_name(
                        monomorphized_struct.clone(),
                        &base_program.structs,
                        exp.source_trace.clone(),
                      )?,
                    );
                    let mut new_typestate = TypeState::Known(Type::Struct(
                      AbstractStruct::fill_generics_ordered(
                        monomorphized_struct.clone(),
                        vec![],
                        &base_program.structs,
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
              FunctionImplementationKind::Composite(f) => {
                if !abstract_signature.generic_args.is_empty() {
                  let monomorphized = abstract_signature
                    .generate_monomorphized(
                      args.iter().map(|arg| arg.data.unwrap_known()).collect(),
                      exp.data.unwrap_known().clone(),
                      base_program,
                      new_program,
                      f.borrow().body.source_trace.clone(),
                    )?;
                  std::mem::swap(f_name, &mut monomorphized.name.clone());
                  new_program.add_abstract_function(Rc::new(RefCell::new(
                    monomorphized,
                  )));
                }
              }
            }
          }
        }
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
              unreachable!(
                "encountered application of non-fn during function inlining"
              )
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
          todo!("")
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
    errors: &mut Vec<CompileError>,
    first_in_walk: bool,
  ) -> bool {
    let add_binding = |name: &mut Rc<str>,
                       source_trace: &SourceTrace,
                       bindings: &mut HashMap<Rc<str>, Vec<Rc<str>>>,
                       errors: &mut Vec<CompileError>| {
      if globally_bound_names.contains(&name) {
        errors.push(CompileError::new(
          CompileErrorKind::CantShadowTopLevelBinding(name.clone()),
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
    let remove_binding =
      |name: &Rc<str>, bindings: &mut HashMap<Rc<str>, Vec<Rc<str>>>| {
        bindings.remove(name);
        for (_, renames) in bindings.iter_mut() {
          if renames.last() == Some(name) {
            renames.pop();
          }
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
        for pair in let_bindings.iter_mut() {
          remove_binding(&pair.0, bindings);
        }
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
        remove_binding(&increment_variable_name, bindings);
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
  ) -> Vec<CompileError> {
    let mut errors = vec![];
    self.deshadow_inner(
      globally_bound_names,
      &mut Default::default(),
      &mut errors,
      true,
    );
    errors
  }
  pub fn validate_control_flow(
    &self,
    errors: &mut Vec<CompileError>,
    enclosing_loop_count: usize,
  ) {
    self
      .walk(&mut |exp| {
        Ok(match &exp.kind {
          ExpKind::Break | ExpKind::Continue => {
            if enclosing_loop_count == 0 {
              errors.push(CompileError {
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
        Ok(true)
      })
      .unwrap();
    name_type_pairs
  }
  pub fn effects(&self) -> HashSet<Effect> {
    let mut effects = HashSet::new();
    self
      .walk(&mut |exp| match &exp.kind {
        Let(items, exp) => {
          let inner_effects =
            items
              .iter()
              .fold(exp.effects(), |mut e, (_, _, value_exp)| {
                e.extend(value_exp.effects());
                e
              });
          let bound_names: HashSet<Rc<str>> =
            items.iter().map(|(name, _, _)| name.clone()).collect();
          effects.extend(inner_effects.into_iter().filter_map(|e| match &e {
            Effect::Modifies(name) => (!bound_names.contains(name)).then(|| e),
          }));
          Ok(true)
        }
        Application(f, args) => {
          let Type::Function(function_signature) = f.data.kind.unwrap_known()
          else {
            unreachable!()
          };
          effects.extend(function_signature.effects());
          if let Some(abstract_ancestor) = function_signature.abstract_ancestor
          {
            for i in abstract_ancestor.mutated_args.iter().copied() {
              effects.insert(Effect::Modifies(
                args[i]
                  .name_or_inner_accessed_name()
                  .expect(
                    "No name found in mutated argument position. This should \
                    never happen if validate_assignments has passed.",
                  )
                  .clone(),
              ));
            }
          }
          Ok(true)
        }
        _ => Ok(true),
      })
      .unwrap();
    effects
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
  pub fn deexpressionify(&mut self) {
    let mut changed = false;
    let mut gensym_index = 0;
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
        | Access(_, _) => {
          let mut slots: Vec<&mut TypedExp> = match &mut exp.kind {
            Application(_, args) | ArrayLiteral(args) => {
              args.iter_mut().collect()
            }
            Reference(inner_exp) | Return(inner_exp) => vec![inner_exp],
            Access(accessor, inner_exp) => {
              let mut slots = vec![inner_exp.as_mut()];
              if let Accessor::ArrayIndex(index_exp) = accessor {
                slots.push(index_exp);
              }
              slots
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
                let effects = arg.effects();
                let mut overridden_names: Vec<(Rc<str>, Type)> = vec![];
                for (name, t) in previously_referenced_names {
                  if effects.contains(&Effect::Modifies(name.clone())) {
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
            for (index, (binding_name, variable_kind, value)) in
              items.iter_mut().enumerate()
            {
              match &value.kind {
                Let(_, _) => {
                  restructured = true;
                  let mut inner_value = placeholder_exp_kind.clone();
                  std::mem::swap(&mut inner_value, &mut value.kind);
                  let Let(inner_bindings, mut inner_body) = inner_value else {
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
                  let mut binding_value = inner_statements.pop().unwrap();
                  take(body, |body| {
                    let body_type = body.data.clone();
                    let mut inner_bindings = items.split_off(index);
                    std::mem::swap(
                      &mut inner_bindings.last_mut().unwrap().2,
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
            if !restructured {
              break;
            }
          }
          true
        }
        _ => true,
      })
    });
    if changed {
      self.deexpressionify();
    }
  }
}
