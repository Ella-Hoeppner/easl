use core::fmt::Debug;
use sse::syntax::EncloserOrOperator;
use std::{rc::Rc, str::pattern::Pattern};

use crate::{
  compiler::{
    builtins::{get_builtin_struct, rename_builtin, ASSIGNMENT_OPS, INFIX_OPS},
    error::{err, CompileError, CompileErrorKind::*, CompileResult},
    functions::FunctionSignature,
    metadata::{extract_metadata, Metadata},
    structs::AbstractStruct,
    types::{
      extract_type_annotation_ast, Context, ExpTypeInfo, Type,
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

pub fn is_match_exhaustive(
  scrutinee_type: &ExpTypeInfo,
  arms: &Vec<(TypedExp, TypedExp)>,
) -> Option<bool> {
  if let TypeState::Known(t) = &scrutinee_type.kind {
    Some(t.do_patterns_exhaust(arms.iter().map(|(pattern, _)| pattern)))
  } else {
    None
  }
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
              _ => panic!("1???? {:?} ", s.fields[0].field_type),
            }],
            &vec![],
            SourceTrace::empty(),
          )
          .unwrap(),
        ))
        .into()
      } else {
        panic!("2???")
      }
    } else {
      panic!("3???")
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
  .unwrap()
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
  Name(Rc<str>),
  NumberLiteral(Number),
  BooleanLiteral(bool),
  Function(Vec<Rc<str>>, Box<Exp<D>>),
  Application(Box<Exp<D>>, Vec<Exp<D>>),
  Access(Accessor, Box<Exp<D>>),
  Let(Vec<(Rc<str>, VariableKind, Exp<D>)>, Box<Exp<D>>),
  Match(Box<Exp<D>>, Vec<(Exp<D>, Exp<D>)>),
  Block {
    expressions: Vec<Exp<D>>,
    bracketed: bool,
  },
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
}
use ExpKind::*;

use super::{
  error::{CompileErrorKind, SourceTrace, SourceTraceKind},
  functions::FunctionImplementationKind,
  structs::compiled_vec_name,
  types::{AbstractType, TypeConstraint},
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
  generic_args: &Vec<Rc<str>>,
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
    let (return_type_ast, return_metadata, metadata_error) =
      extract_metadata(return_type_ast);
    if let Some(metadata_error) = metadata_error {
      return Err(metadata_error);
    }
    let return_type = AbstractType::from_easl_tree(
      return_type_ast,
      structs,
      aliases,
      generic_args,
      &vec![],
    )?;
    if let EaslTree::Inner((position, Encloser(Square)), arg_asts) =
      args_and_return_type.remove(0)
    {
      let source_path: SourceTrace = position.into();
      let ((arg_types, arg_metadata), arg_names) = arg_asts
        .into_iter()
        .map(|arg| -> CompileResult<_> {
          let (maybe_t_ast, arg_name_ast) = extract_type_annotation_ast(arg)?;
          let t_ast = maybe_t_ast.ok_or(CompileError::new(
            FunctionArgMissingType,
            source_path.clone(),
          ))?;
          let t = AbstractType::from_easl_tree(
            t_ast,
            structs,
            aliases,
            generic_args,
            &vec![],
          )?;
          let (arg_name_ast, arg_metadata, metadata_error) =
            extract_metadata(arg_name_ast);
          if let Some(metadata_error) = metadata_error {
            return Err(metadata_error);
          }
          if let EaslTree::Leaf(_, arg_name) = arg_name_ast {
            Ok(((t, arg_metadata), arg_name.into()))
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
        return_metadata,
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
        kind: ExpKind::Block {
          expressions: body_exps,
          bracketed: false,
        },
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
            data: Known(Type::None).into(),
            source_trace,
          }
        } else if &*leaf == "continue" {
          Exp {
            kind: ExpKind::Continue,
            data: Known(Type::None).into(),
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
                    if ".".is_prefix_of(&first_child_name) {
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
                              kind: ExpKind::Block {
                                expressions: child_exps,
                                bracketed: false,
                              },
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
                                let (name_ast, name_metadata, metadata_error) =
                                  extract_metadata(name_ast);
                                if let Some(metadata_error) = metadata_error {
                                  return Err(metadata_error);
                                }
                                match name_ast {
                                  EaslTree::Leaf(position, name) => {
                                    let source_trace = position.clone().into();
                                    bindings.push((
                                      name.into(),
                                      match name_metadata {
                                        None => VariableKind::Let,
                                        Some(Metadata::Singular(tag)) => {
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
                                        Some(metadata) => {
                                          return err(
                                            InvalidVariableMetadata(metadata),
                                            source_trace,
                                          )
                                        }
                                      },
                                      Self::try_from_easl_tree(
                                        value_ast, structs, aliases, skolems,
                                        ctx,
                                      )?,
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
                        "block" => {
                          if children_iter.is_empty() {
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
                            kind: ExpKind::Block {
                              expressions: child_exps,
                              bracketed: true,
                            },
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
                                extract_type_annotation_ast(var_name_subtree)?;
                              (
                                if let EaslTree::Leaf(_, name) =
                                  var_name_subtree
                                {
                                  name.into()
                                } else {
                                  panic!()
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
                                data: TypeState::Known(Type::None).into(),
                                kind: ExpKind::Block {
                                  expressions: body_expressions,
                                  bracketed: false,
                                },
                                source_trace: body_source_trace,
                              }),
                            },
                            data: Known(Type::None).into(),
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
                                data: TypeState::Known(Type::None).into(),
                                kind: ExpKind::Block {
                                  expressions: sub_expressions,
                                  bracketed: false,
                                },
                                source_trace: body_source_trace,
                              }),
                            },
                            data: Known(Type::None).into(),
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
                              data: TypeState::Known(Type::None).into(),
                              source_trace,
                            })
                          } else {
                            return Err(CompileError::new(
                              InvalidReturn,
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
                return err(EmptyList, source_trace);
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
                Some(children_iter.len() as u32),
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
    custom_handler: &mut impl FnMut(&Self) -> CompileResult<bool>,
  ) -> CompileResult<()> {
    if custom_handler(self)? {
      match &self.kind {
        Application(f, args) => {
          f.walk(custom_handler)?;
          for arg in args.iter() {
            arg.walk(custom_handler)?;
          }
        }
        Function(_, body) => body.walk(custom_handler)?,
        Access(_, body) => body.walk(custom_handler)?,
        Let(bindings, body) => {
          for (_, _, value) in bindings {
            value.walk(custom_handler)?;
          }
          body.walk(custom_handler)?;
        }
        Match(scrutinee, arms) => {
          scrutinee.walk(custom_handler)?;
          for (pattern, value) in arms {
            pattern.walk(custom_handler)?;
            value.walk(custom_handler)?;
          }
        }
        Block { expressions, .. } => {
          for subexp in expressions {
            subexp.walk(custom_handler)?;
          }
        }
        ForLoop {
          increment_variable_initial_value_expression,
          continue_condition_expression,
          update_condition_expression,
          body_expression,
          ..
        } => {
          increment_variable_initial_value_expression.walk(custom_handler)?;
          continue_condition_expression.walk(custom_handler)?;
          update_condition_expression.walk(custom_handler)?;
          body_expression.walk(custom_handler)?;
        }
        WhileLoop {
          condition_expression,
          body_expression,
        } => {
          condition_expression.walk(custom_handler)?;
          body_expression.walk(custom_handler)?;
        }
        Return(exp) => exp.walk(custom_handler)?,
        ArrayLiteral(children) => {
          for child in children {
            child.walk(custom_handler)?;
          }
        }
        _ => {}
      }
    }
    Ok(())
  }
  pub fn walk_mut(
    &mut self,
    custom_handler: &mut impl FnMut(&mut Self) -> CompileResult<bool>,
  ) -> CompileResult<()> {
    if custom_handler(self)? {
      match &mut self.kind {
        Application(f, args) => {
          f.walk_mut(custom_handler)?;
          for arg in args.iter_mut() {
            arg.walk_mut(custom_handler)?;
          }
        }
        Function(_, body) => body.walk_mut(custom_handler)?,
        Access(_, body) => body.walk_mut(custom_handler)?,
        Let(bindings, body) => {
          for (_, _, value) in bindings {
            value.walk_mut(custom_handler)?;
          }
          body.walk_mut(custom_handler)?;
        }
        Match(scrutinee, arms) => {
          scrutinee.walk_mut(custom_handler)?;
          for (pattern, value) in arms {
            pattern.walk_mut(custom_handler)?;
            value.walk_mut(custom_handler)?;
          }
        }
        Block { expressions, .. } => {
          for subexp in expressions {
            subexp.walk_mut(custom_handler)?;
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
            .walk_mut(custom_handler)?;
          continue_condition_expression.walk_mut(custom_handler)?;
          update_condition_expression.walk_mut(custom_handler)?;
          body_expression.walk_mut(custom_handler)?;
        }
        WhileLoop {
          condition_expression,
          body_expression,
        } => {
          condition_expression.walk_mut(custom_handler)?;
          body_expression.walk_mut(custom_handler)?;
        }
        Return(exp) => exp.walk_mut(custom_handler)?,
        ArrayLiteral(children) => {
          for child in children {
            child.walk_mut(custom_handler)?;
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
      Name(name) => wrap(compile_word(name)),
      NumberLiteral(num) => wrap(match num {
        Number::Int(i) => format!(
          "{i}{}",
          match self.data.kind {
            Known(Type::I32) => "",
            Known(Type::U32) => "u",
            Known(Type::F32) => "f",
            _ => panic!("{:?}", self.data.kind),
          }
        ),
        Number::Float(f) => format!("{f}f"),
      }),
      BooleanLiteral(b) => wrap(format!("{b}")),
      Function(_, _) => panic!("Attempting to compile internal function"),
      Application(f, args) => {
        let f_str = if let ExpKind::Name(name) = f.kind {
          rename_builtin(&*name).unwrap_or_else(|| compile_word(name))
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
            format!(
              "{} {}: {} = {};",
              variable_kind.compile(),
              compile_word(name),
              value_exp.data.compile(),
              value_exp.compile(InnerExpression)
            )
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
                (
                  indent(true_case.compile(position)),
                  indent(
                    false_case.or(wildcard_case).unwrap().compile(position),
                  ),
                )
              } else {
                (
                  indent(
                    true_case.or(wildcard_case).unwrap().compile(position),
                  ),
                  indent(false_case.unwrap().compile(position)),
                )
              };
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
          if position == InnerExpression {
            let mut arms_iter = arms.into_iter().rev();
            let default = arms_iter.next().unwrap().1.compile(position);
            let scrutinee_string = scrutinee.compile(position);
            arms_iter.into_iter().fold(
              default,
              |subexpression, (pattern_subtree, value_subtree)| {
                format!(
                  "select({}, {}, {} == {})",
                  subexpression,
                  value_subtree.compile(position),
                  pattern_subtree.compile(position),
                  scrutinee_string
                )
              },
            )
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
                    indent(value.compile(position))
                  ))
                  .collect::<Vec<String>>()
                  .join("\n")
              )
            )
          }
        }
      }
      Block {
        expressions,
        bracketed,
      } => {
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
        if bracketed {
          format!("\n{{{}\n}}", indent(child_strings.join("")))
        } else {
          format!("{}", child_strings.join(""))
        }
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
    }
  }

  pub fn find_untyped(&mut self) -> Vec<TypedExp> {
    let mut untyped = vec![];
    self
      .walk_mut(&mut |exp| {
        if !exp.data.is_fully_known() {
          untyped.push(exp.clone());
        }
        Ok(true)
      })
      .unwrap();
    untyped
  }
  pub fn validate_match_blocks(&self) -> CompileResult<()> {
    Ok(()) // todo!
  }
  pub fn propagate_types(&mut self, ctx: &mut Context) -> CompileResult<bool> {
    if self.data.subtree_fully_typed {
      return Ok(false);
    }
    let changed = match &mut self.kind {
      Wildcard => {
        self.data.subtree_fully_typed = true;
        false
      }
      Name(name) => {
        self.data.subtree_fully_typed = true;
        if !ctx.is_bound(name) {
          return err(UnboundName(name.clone()), self.source_trace.clone());
        }
        let changed = ctx.constrain_name_type(
          name,
          self.source_trace.clone(),
          &mut self.data,
        )?;
        changed
      }
      NumberLiteral(num) => {
        self.data.subtree_fully_typed = true;
        self.data.constrain(
          match num {
            Number::Int(_) => {
              TypeState::OneOf(vec![Type::I32, Type::U32, Type::F32])
            }
            Number::Float(_) => TypeState::Known(Type::F32),
          },
          self.source_trace.clone(),
        )?
      }
      BooleanLiteral(_) => {
        self.data.subtree_fully_typed = true;
        self
          .data
          .constrain(TypeState::Known(Type::Bool), self.source_trace.clone())?
      }
      Function(arg_names, body) => {
        ctx.push_enclosing_function_type(self.data.clone().kind);
        let changed = if let TypeState::Known(f_type) = &mut self.data.kind {
          let (name, arg_count, arg_type_states, return_type_state): (
            Option<Rc<str>>,
            usize,
            &Vec<ExpTypeInfo>,
            &mut ExpTypeInfo,
          ) = match f_type {
            Type::Function(signature) => (
              signature.name(),
              signature.arg_types.len(),
              &signature.arg_types.iter().map(|(t, _)| t.clone()).collect(),
              &mut signature.return_type,
            ),
            _ => {
              return err(
                FunctionExpressionHasNonFunctionType(f_type.clone()),
                self.source_trace.clone(),
              )
            }
          };
          let return_type_changed = body
            .data
            .mutually_constrain(return_type_state, self.source_trace.clone())?;
          if arg_count == arg_names.len() {
            for (name, t) in arg_names.iter().zip(arg_type_states) {
              ctx.bind(name, Variable::new(t.clone()))
            }
            let body_types_changed = body.propagate_types(ctx)?;
            let argument_types = arg_names
              .iter()
              .map(|name| ctx.unbind(name).typestate.kind)
              .collect::<Vec<_>>();
            let fn_type_changed = self.data.constrain_fn_by_argument_types(
              argument_types,
              self.source_trace.clone(),
            )?;
            self.data.subtree_fully_typed = body.data.subtree_fully_typed;
            return_type_changed || body_types_changed || fn_type_changed
          } else {
            return err(WrongArity(name), self.source_trace.clone());
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
          anything_changed |= arg.propagate_types(ctx)?;
        }
        if let Name(name) = &f.kind {
          anything_changed |= ctx.constrain_name_type(
            name,
            self.source_trace.clone(),
            &mut f.data,
          )?;
        } else {
          anything_changed |= f.data.constrain(
            TypeState::Known(Type::Function(Box::new(FunctionSignature {
              abstract_ancestor: None,
              arg_types: args
                .iter()
                .map(|arg| (arg.data.clone(), vec![]))
                .collect(),
              return_type: self.data.clone(),
            }))),
            f.source_trace.clone(),
          )?;
        }
        if let TypeState::Known(f_type) = &mut f.data.kind {
          if let Type::Function(signature) = f_type {
            if args.len() == signature.arg_types.len() {
              anything_changed |= self.data.mutually_constrain(
                &mut signature.return_type,
                self.source_trace.clone(),
              )?;
              for (arg, (t, _)) in
                args.iter_mut().zip(signature.arg_types.iter().cloned())
              {
                anything_changed |=
                  arg.data.constrain(t.kind, self.source_trace.clone())?;
              }
            } else {
              return err(
                WrongArity(signature.name()),
                self.source_trace.clone(),
              );
            }
          } else {
            return err(AppliedNonFunction, self.source_trace.clone());
          }
        }
        anything_changed |= f.data.constrain_fn_by_argument_types(
          args.iter().map(|arg| arg.data.kind.clone()).collect(),
          self.source_trace.clone(),
        )?;
        anything_changed |= f.propagate_types(ctx)?;
        self.data.subtree_fully_typed =
          args.iter().fold(f.data.subtree_fully_typed, |acc, arg| {
            acc && arg.data.subtree_fully_typed
          });

        anything_changed
      }
      Access(accessor, subexp) => match accessor {
        Accessor::Field(field_name) => {
          let mut anything_changed = false;
          anything_changed |= subexp.propagate_types(ctx)?;
          if let Known(t) = &mut subexp.data.kind {
            if let Type::Struct(s) = t {
              anything_changed |= self.data.mutually_constrain(
                {
                  &mut s
                    .fields
                    .iter_mut()
                    .find(|f| f.name == *field_name)
                    .ok_or(CompileError::new(
                      NoSuchField {
                        struct_name: s.abstract_ancestor.name.clone(),
                        field_name: field_name.clone(),
                      },
                      self.source_trace.clone(),
                    ))?
                    .field_type
                },
                self.source_trace.clone(),
              )?;
            } else {
              return err(AccessorOnNonStruct, self.source_trace.clone())?;
            }
          }
          self.data.subtree_fully_typed = subexp.data.subtree_fully_typed;
          anything_changed
        }
        Accessor::Swizzle(fields) => {
          let mut anything_changed = self.data.constrain(
            swizzle_accessor_typestate(&subexp.data, fields).kind,
            self.source_trace.clone(),
          )?;
          anything_changed |= subexp.data.constrain(
            swizzle_accessed_possibilities(fields).kind,
            self.source_trace.clone(),
          )?;
          anything_changed |= subexp.propagate_types(ctx)?;
          self.data.subtree_fully_typed = subexp.data.subtree_fully_typed;
          anything_changed
        }
        Accessor::ArrayIndex(index_expression) => {
          let mut anything_changed = index_expression.data.constrain(
            TypeState::OneOf(vec![Type::I32, Type::U32]),
            self.source_trace.clone(),
          )?;
          if let TypeState::Known(t) = &mut subexp.data.kind {
            if let Type::Array(_, inner_type) = t {
              anything_changed |= self.data.mutually_constrain(
                inner_type.as_mut(),
                self.source_trace.clone(),
              )?;
            } else {
              return err(ArrayAccessOnNonArray, self.source_trace.clone());
            }
          }
          anything_changed |= index_expression.propagate_types(ctx)?;
          anything_changed |= subexp.propagate_types(ctx)?;
          self.data.subtree_fully_typed = subexp.data.subtree_fully_typed
            && index_expression.data.subtree_fully_typed;
          anything_changed
        }
      },
      Let(bindings, body) => {
        let mut anything_changed = body
          .data
          .mutually_constrain(&mut self.data, self.source_trace.clone())?;
        for (name, _, value) in bindings.iter_mut() {
          anything_changed |= value.propagate_types(ctx)?;
          ctx.bind(name, Variable::new(value.data.clone()));
        }
        anything_changed |= body.propagate_types(ctx)?;
        for (name, _, value) in bindings.iter_mut() {
          anything_changed |= value.data.constrain(
            ctx.unbind(name).typestate.kind,
            self.source_trace.clone(),
          )?;
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
        let mut anything_changed = false;
        anything_changed |= scrutinee.propagate_types(ctx)?;
        if let Some(false) = is_match_exhaustive(&scrutinee.data, &arms) {
          self.data.constrain(
            TypeState::Known(Type::None),
            self.source_trace.clone(),
          )?;
        }
        for (case, value) in arms.iter_mut() {
          anything_changed |= case.propagate_types(ctx)?;
          anything_changed |= value.propagate_types(ctx)?;
          anything_changed |= case.data.mutually_constrain(
            &mut scrutinee.data,
            self.source_trace.clone(),
          )?;
          anything_changed |= value
            .data
            .mutually_constrain(&mut self.data, self.source_trace.clone())?;
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
      Block { expressions, .. } => {
        let mut anything_changed = false;
        for child in expressions.iter_mut() {
          anything_changed |= child.propagate_types(ctx)?;
        }
        anything_changed |= self.data.mutually_constrain(
          &mut expressions
            .last_mut()
            .ok_or(CompileError::new(EmptyBlock, self.source_trace.clone()))?
            .data,
          self.source_trace.clone(),
        )?;
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
        let mut anything_changed = false;
        let mut variable_typestate =
          TypeState::Known(increment_variable_type.clone());
        anything_changed |= increment_variable_initial_value_expression
          .data
          .mutually_constrain(
            &mut variable_typestate,
            increment_variable_initial_value_expression
              .source_trace
              .clone(),
          )?;
        anything_changed |=
          increment_variable_initial_value_expression.propagate_types(ctx)?;
        ctx.bind(
          increment_variable_name,
          Variable {
            kind: VariableKind::Var,
            typestate: variable_typestate.into(),
          },
        );
        anything_changed |= continue_condition_expression.data.constrain(
          TypeState::Known(Type::Bool),
          continue_condition_expression.source_trace.clone(),
        )?;
        anything_changed |= update_condition_expression.data.constrain(
          TypeState::Known(Type::None),
          update_condition_expression.source_trace.clone(),
        )?;
        anything_changed |=
          continue_condition_expression.propagate_types(ctx)?;
        anything_changed |= update_condition_expression.propagate_types(ctx)?;
        anything_changed |= body_expression.propagate_types(ctx)?;
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
          condition_expression.source_trace.clone(),
        )?;
        anything_changed |= body_expression.data.constrain(
          TypeState::Known(Type::None),
          condition_expression.source_trace.clone(),
        )?;
        anything_changed |= condition_expression.propagate_types(ctx)?;
        anything_changed |= body_expression.propagate_types(ctx)?;
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
        let changed = exp.data.mutually_constrain(
          &mut ctx
            .enclosing_function_type()
            .ok_or(CompileError::new(
              ReturnOutsideFunction,
              self.source_trace.clone(),
            ))?
            .as_fn_type_if_known(|| {
              CompileError::new(
                CompileErrorKind::EnclosingFunctionTypeWasntFunction,
                self.source_trace.clone(),
              )
            })?
            .map(|fn_type| &mut fn_type.return_type)
            .unwrap_or(&mut TypeState::Unknown.into()),
          self.source_trace.clone(),
        )?;
        self.data.subtree_fully_typed = exp.data.subtree_fully_typed;
        changed
      }
      ArrayLiteral(children) => {
        let mut changed = false;
        for child in children.iter_mut() {
          self.data.as_known_mut(|array_type| {
            if let Type::Array(_, inner_type) = array_type {
              changed |= child.data.mutually_constrain(
                inner_type.as_mut(),
                SourceTrace {
                  kind: SourceTraceKind::Combination(vec![
                    child.source_trace.clone(),
                    self.source_trace.clone(),
                  ])
                  .into(),
                },
              )?;
              changed |= child.propagate_types(ctx)?;
              Ok::<(), CompileError>(())
            } else {
              unreachable!()
            }
          })?
        }
        changed
      }
      Reference(exp) => {
        let mut changed = exp.propagate_types(ctx)?;
        let TypeState::Known(Type::Reference(inner_type)) = &mut self.data.kind
        else {
          unreachable!()
        };
        changed |= exp
          .data
          .mutually_constrain(inner_type, self.source_trace.clone())?;
        changed
      }
    };
    self.data.subtree_fully_typed &= self.data.check_is_fully_known();
    Ok(changed)
  }
  pub fn check_assignment_validity(
    &self,
    ctx: &mut Context,
  ) -> CompileResult<()> {
    self.walk(&mut |exp| {
      Ok(match &exp.kind {
        Application(f, args) => {
          if let ExpKind::Name(f_name) = &f.kind {
            if ASSIGNMENT_OPS.contains(&&**f_name) {
              let mut var = &args[0];
              loop {
                if let ExpKind::Access(_, inner_exp) = &var.kind {
                  var = inner_exp;
                } else {
                  break;
                }
              }
              if let ExpKind::Name(var_name) = &var.kind {
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
            arg.check_assignment_validity(ctx)?;
          }
          f.check_assignment_validity(ctx)?;
          false
        }
        Let(binding_names_and_values, body) => {
          for (name, kind, value) in binding_names_and_values {
            value.check_assignment_validity(ctx)?;
            ctx.bind(
              name,
              Variable::new(value.data.clone()).with_kind(kind.clone()),
            )
          }
          body.check_assignment_validity(ctx)?;
          for (name, _, value) in binding_names_and_values {
            value.check_assignment_validity(ctx)?;
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
            body_exp.check_assignment_validity(ctx)?;
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
            .check_assignment_validity(ctx)?;
          continue_condition_expression.check_assignment_validity(ctx)?;
          update_condition_expression.check_assignment_validity(ctx)?;
          body_expression.check_assignment_validity(ctx)?;
          ctx.unbind(increment_variable_name);
          false
        }
        _ => true,
      })
    })
  }
  pub fn replace_skolems(&mut self, skolems: &Vec<(Rc<str>, Type)>) {
    self
      .walk_mut(&mut |exp| {
        if let Known(t) = &mut exp.data.kind {
          t.replace_skolems(skolems);
        }
        Ok(true)
      })
      .unwrap()
  }
  pub fn monomorphize(
    &mut self,
    base_ctx: &Context,
    new_ctx: &mut Context,
  ) -> CompileResult<()> {
    let source_trace = self.source_trace.clone();
    self.walk_mut(&mut |exp: &mut TypedExp| {
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
                  base_ctx.structs.iter().find(|s| s.name == *f_name)
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
                        &base_ctx.structs,
                        exp.source_trace.clone(),
                      )?,
                    );
                    let mut new_typestate = TypeState::Known(Type::Struct(
                      AbstractStruct::fill_generics_ordered(
                        monomorphized_struct.clone(),
                        vec![],
                        &base_ctx.structs,
                        source_trace.clone(),
                      )?,
                    ));
                    exp.data.with_dereferenced_mut(|typestate| {
                      std::mem::swap(typestate, &mut new_typestate)
                    });
                    new_ctx
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
                      base_ctx,
                      new_ctx,
                      f.borrow().body.source_trace.clone(),
                    )?;
                  std::mem::swap(f_name, &mut monomorphized.name.clone());
                  new_ctx.add_abstract_function(Rc::new(monomorphized));
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
    new_ctx: &mut Context,
  ) -> CompileResult<bool> {
    let mut changed = false;
    self.walk_mut(&mut |exp: &mut TypedExp| {
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
                  new_ctx.add_abstract_function(Rc::new(inlined));
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
      .walk_mut(&mut |exp| {
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
}
