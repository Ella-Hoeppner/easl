use core::fmt::Debug;
use sse::document::DocumentPosition;
use std::str::pattern::Pattern;

use crate::{
  compiler::{
    builtins::{get_builtin_struct, ASSIGNMENT_OPS, INFIX_OPS},
    error::{err, CompileError, CompileErrorKind::*, CompileResult},
    functions::FunctionSignature,
    metadata::{extract_metadata, Metadata},
    structs::AbstractStruct,
    types::{
      extract_type_annotation_ast, Context, Type,
      TypeState::{self, *},
      Variable, VariableKind,
    },
    util::{compile_word, indent},
  },
  parse::TyntTree,
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

fn parse_number(num_str: &str, source_path: &Vec<usize>) -> Option<TypedExp> {
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
            data: Known(Type::F32),
            source_paths: vec![source_path.clone()],
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
              }),
              source_paths: vec![source_path.clone()],
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
          data: Known(Type::F32),
          source_paths: vec![source_path.clone()],
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
          }),
          source_paths: vec![source_path.clone()],
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
  pub source_paths: Vec<Vec<usize>>,
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
  Field(String),
  Swizzle(Vec<SwizzleField>),
}

pub fn swizzle_accessor_typestate(
  accessed_typestate: &TypeState,
  fields: &Vec<SwizzleField>,
) -> TypeState {
  let vec_struct = match fields.len() {
    2 => get_builtin_struct("vec2"),
    3 => get_builtin_struct("vec3"),
    4 => get_builtin_struct("vec4"),
    n => unreachable!("swizzle with {n} elements, expected 2-4"),
  };
  if let TypeState::Known(accessed_type) = accessed_typestate {
    if let Type::Struct(s) = accessed_type {
      if &s.name != "vec" {
        TypeState::Known(Type::Struct(vec_struct.fill_generics_ordered(vec![
          match &s.fields[0].field_type {
            TypeState::UnificationVariable(uvar) => {
              TypeState::UnificationVariable(uvar.clone())
            }
            TypeState::Known(inner_type) => {
              TypeState::Known(inner_type.clone())
            }
            _ => panic!("1???? {:?} ", s.fields[0].field_type),
          },
        ])))
      } else {
        panic!("2???")
      }
    } else {
      panic!("3???")
    }
  } else {
    TypeState::Unknown
  }
}

pub fn swizzle_accessed_possibilities(fields: &Vec<SwizzleField>) -> TypeState {
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
          get_builtin_struct(name).fill_generics_with_unification_variables(),
        )
      })
      .collect::<Vec<Type>>(),
  )
  .simplified()
  .unwrap()
}

impl Accessor {
  pub fn new(name: String) -> Self {
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
  pub fn compile(self) -> String {
    match self {
      Accessor::Field(field_name) => field_name,
      Accessor::Swizzle(field_indeces) => field_indeces
        .into_iter()
        .map(|index| ["x", "y", "z", "w"][index as usize])
        .collect::<Vec<&str>>()
        .join(""),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpKind<D: Debug + Clone + PartialEq> {
  Wildcard,
  Name(String),
  NumberLiteral(Number),
  BooleanLiteral(bool),
  Function(Vec<String>, Box<Exp<D>>),
  Application(Box<Exp<D>>, Vec<Exp<D>>),
  Access(Accessor, Box<Exp<D>>),
  Let(Vec<(String, VariableKind, Exp<D>)>, Box<Exp<D>>),
  Match(Box<Exp<D>>, Vec<(Exp<D>, Exp<D>)>),
  Block(Vec<Exp<D>>),
}
use ExpKind::*;

use super::{
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

pub type TypedExp = Exp<TypeState>;

pub fn arg_list_and_return_type_from_tynt_tree(
  tree: TyntTree,
  structs: &Vec<AbstractStruct>,
  aliases: &Vec<(String, AbstractStruct)>,
  generic_args: &Vec<String>,
) -> CompileResult<(
  Vec<usize>,
  Vec<String>,
  Vec<AbstractType>,
  Vec<Option<Metadata>>,
  AbstractType,
  Option<Metadata>,
)> {
  use crate::parse::Encloser::*;
  use crate::parse::Operator::*;
  use sse::syntax::EncloserOrOperator::*;
  if let TyntTree::Inner(
    (position, Operator(TypeAnnotation)),
    mut args_and_return_type,
  ) = tree
  {
    let return_type_ast = args_and_return_type.remove(1);
    let (return_metadata, return_type_ast) = extract_metadata(return_type_ast)?;
    let return_type = AbstractType::from_tynt_tree(
      return_type_ast,
      structs,
      aliases,
      generic_args,
      &vec![],
    )?;
    if let TyntTree::Inner((position, Encloser(Square)), arg_asts) =
      args_and_return_type.remove(0)
    {
      let ((arg_types, arg_metadata), arg_names) = arg_asts
        .into_iter()
        .map(|arg| -> CompileResult<_> {
          let (maybe_t_ast, arg_name_ast) = extract_type_annotation_ast(arg)?;
          let t_ast = maybe_t_ast.ok_or(CompileError::new(
            FunctionArgMissingType,
            vec![position.path.clone()],
          ))?;
          let (arg_metadata, t_ast) = extract_metadata(t_ast)?;
          let t = AbstractType::from_tynt_tree(
            t_ast,
            structs,
            aliases,
            generic_args,
            &vec![],
          )?;
          if let TyntTree::Leaf(_, arg_name) = arg_name_ast {
            Ok(((t, arg_metadata), arg_name))
          } else {
            err(InvalidArgumentName, vec![position.path.clone()])
          }
        })
        .collect::<CompileResult<(
          (Vec<AbstractType>, Vec<Option<Metadata>>),
          Vec<String>,
        )>>()?;
      Ok((
        position.path,
        arg_names,
        arg_types,
        arg_metadata,
        return_type,
        return_metadata,
      ))
    } else {
      err(
        FunctionSignatureNotSquareBrackets,
        vec![position.path.clone()],
      )
    }
  } else {
    err(
      FunctionSignatureMissingReturnType,
      vec![tree.position().path.clone()],
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
    source_path: Vec<usize>,
    body_trees: Vec<TyntTree>,
    return_type: TypeState,
    arg_names: Vec<String>,
    arg_types: Vec<TypeState>,
    structs: &Vec<AbstractStruct>,
    aliases: &Vec<(String, AbstractStruct)>,
    skolems: &Vec<String>,
  ) -> CompileResult<Self> {
    let mut body_exps = body_trees
      .into_iter()
      .map(|t| {
        Self::try_from_tynt_tree(
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
        data: Unknown,
        source_paths: body_exps
          .iter()
          .map(|body_exp| body_exp.source_paths.clone())
          .flatten()
          .collect(),
        kind: ExpKind::Block(body_exps),
      }
    };
    Ok(Exp {
      data: Known(Type::Function(Box::new(FunctionSignature {
        abstract_ancestor: None,
        arg_types,
        return_type,
      }))),
      kind: ExpKind::Function(arg_names, Box::new(body)),
      source_paths: vec![source_path],
    })
  }
  pub fn try_from_tynt_tree(
    tree: TyntTree,
    structs: &Vec<AbstractStruct>,
    aliases: &Vec<(String, AbstractStruct)>,
    skolems: &Vec<String>,
    ctx: SyntaxTreeContext,
  ) -> CompileResult<Self> {
    Ok(match tree {
      TyntTree::Leaf(DocumentPosition { path, .. }, leaf) => {
        if leaf == "true" || leaf == "false" {
          Exp {
            kind: ExpKind::BooleanLiteral(leaf == "true"),
            data: Known(Type::Bool),
            source_paths: vec![path],
          }
        } else if let Some(num_exp) = parse_number(&leaf, &path) {
          num_exp
        } else if leaf == "_".to_string() {
          Exp {
            kind: ExpKind::Wildcard,
            data: Unknown,
            source_paths: vec![path],
          }
        } else {
          Exp {
            kind: ExpKind::Name(leaf),
            data: Unknown,
            source_paths: vec![path],
          }
        }
      }
      TyntTree::Inner((position, encloser_or_operator), children) => {
        use crate::parse::Encloser::*;
        use crate::parse::Operator::*;
        use sse::syntax::EncloserOrOperator::*;
        let mut children_iter = children.into_iter();
        match encloser_or_operator {
          Encloser(e) => match e {
            Parens => {
              if let Some(first_child) = children_iter.next() {
                match &first_child {
                  TyntTree::Leaf(position, first_child_name) => {
                    if ".".is_prefix_of(&first_child_name) {
                      if children_iter.len() == 1 {
                        Some(Exp {
                          data: TypeState::Unknown,
                          kind: ExpKind::Access(
                            Accessor::new(
                              first_child_name
                                .chars()
                                .skip(1)
                                .collect::<String>(),
                            ),
                            Box::new(Self::try_from_tynt_tree(
                              children_iter.next().unwrap(),
                              structs,
                              aliases,
                              skolems,
                              ctx,
                            )?),
                          ),
                          source_paths: vec![position.path.clone()],
                        })
                      } else {
                        return err(
                          AccessorHadMultipleArguments,
                          vec![position.path.clone()],
                        );
                      }
                    } else {
                      match first_child_name.as_str() {
                        "fn" => {
                          let (
                            source_path,
                            arg_names,
                            arg_types,
                            _arg_metadata,
                            return_type,
                            _return_metadata,
                          ) = arg_list_and_return_type_from_tynt_tree(
                            children_iter.next().ok_or(CompileError::new(
                              InvalidFunction,
                              vec![position.path.clone()],
                            ))?,
                            structs,
                            aliases,
                            &vec![],
                          )?;
                          Some(Self::function_from_body_tree(
                            source_path,
                            children_iter.clone().collect(),
                            TypeState::Known(
                              return_type.concretize(structs, skolems)?,
                            ),
                            arg_names,
                            arg_types
                              .into_iter()
                              .map(|t| {
                                Ok(TypeState::Known(
                                  t.concretize(structs, skolems)?,
                                ))
                              })
                              .collect::<CompileResult<Vec<TypeState>>>()?,
                            structs,
                            aliases,
                            skolems,
                          )?)
                        }
                        "let" => {
                          if children_iter.len() < 2 {
                            return err(
                              NotEnoughLetBlockChildren,
                              vec![position.path.clone()],
                            );
                          }
                          let bindings_ast = children_iter.next().unwrap();
                          let mut child_exps = children_iter
                            .clone()
                            .map(|child| {
                              Self::try_from_tynt_tree(
                                child, structs, aliases, skolems, ctx,
                              )
                            })
                            .collect::<CompileResult<Vec<Self>>>()?;
                          let body_exp = if child_exps.len() == 1 {
                            child_exps.remove(0)
                          } else {
                            Exp {
                              data: Unknown,
                              kind: ExpKind::Block(child_exps),
                              source_paths: vec![position.path.clone()],
                            }
                          };
                          if let TyntTree::Inner(
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
                                let (name_metadata, name_ast) =
                                  extract_metadata(name_ast)?;
                                match name_ast {
                                  TyntTree::Leaf(position, name) => {
                                    bindings.push((
                                      name,
                                      match name_metadata {
                                        None => VariableKind::Let,
                                        Some(Metadata::Singular(tag)) => {
                                          match tag.as_str() {
                                            "var" => VariableKind::Var,
                                            _ => {
                                              return err(
                                                InvalidVariableMetadata(
                                                  Metadata::Singular(tag),
                                                ),
                                                vec![position.path],
                                              )
                                            }
                                          }
                                        }
                                        Some(metadata) => {
                                          return err(
                                            InvalidVariableMetadata(metadata),
                                            vec![position.path],
                                          )
                                        }
                                      },
                                      Self::try_from_tynt_tree(
                                        value_ast, structs, aliases, skolems,
                                        ctx,
                                      )?,
                                    ));
                                  }
                                  TyntTree::Inner((position, _), _) => {
                                    return err(
                                      ExpectedBindingName,
                                      vec![position.path],
                                    );
                                  }
                                }
                              }
                              Some(Exp {
                                data: Unknown,
                                kind: ExpKind::Let(
                                  bindings,
                                  Box::new(body_exp),
                                ),
                                source_paths: vec![position.path.clone()],
                              })
                            } else {
                              return err(
                                OddNumberOfChildrenInLetBindings,
                                vec![position.path.clone()],
                              );
                            }
                          } else {
                            return err(
                              LetBindingsNotSquareBracketed,
                              vec![position.path.clone()],
                            );
                          }
                        }
                        "block" => {
                          if children_iter.is_empty() {
                            return err(
                              EmptyBlock,
                              vec![position.path.clone()],
                            );
                          }
                          let child_exps = children_iter
                            .clone()
                            .map(|child| {
                              Self::try_from_tynt_tree(
                                child, structs, aliases, skolems, ctx,
                              )
                            })
                            .collect::<CompileResult<Vec<Self>>>()?;
                          Some(Exp {
                            data: Unknown,
                            kind: ExpKind::Block(child_exps),
                            source_paths: vec![position.path.clone()],
                          })
                        }
                        "match" => {
                          let scrutinee = Self::try_from_tynt_tree(
                            children_iter.next().ok_or(CompileError::new(
                              MatchMissingScrutinee,
                              vec![position.path.clone()],
                            ))?,
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
                                vec![position.path.clone()],
                              ))?;
                            arms.push((
                              Self::try_from_tynt_tree(
                                pattern_subtree,
                                structs,
                                aliases,
                                skolems,
                                SyntaxTreeContext::MatchPattern,
                              )?,
                              Self::try_from_tynt_tree(
                                value_subtree,
                                structs,
                                aliases,
                                skolems,
                                ctx,
                              )?,
                            ));
                          }
                          if arms.is_empty() {
                            return err(
                              MatchMissingArms,
                              vec![position.path.clone()],
                            );
                          }
                          Some(Exp {
                            kind: Match(Box::new(scrutinee), arms),
                            data: Unknown,
                            source_paths: vec![position.path.clone()],
                          })
                        }
                        _ => None,
                      }
                    }
                  }
                  _ => None,
                }
                .unwrap_or(Exp {
                  kind: Application(
                    Box::new(Self::try_from_tynt_tree(
                      first_child,
                      structs,
                      aliases,
                      skolems,
                      ctx,
                    )?),
                    children_iter
                      .map(|arg| {
                        Self::try_from_tynt_tree(
                          arg, structs, aliases, skolems, ctx,
                        )
                      })
                      .collect::<CompileResult<_>>()?,
                  ),
                  data: Unknown,
                  source_paths: vec![position.path],
                })
              } else {
                return err(EmptyList, vec![position.path.clone()]);
              }
            }
            Square => todo!("array"),
            Curly => todo!("anonymous struct"),
            LineComment => todo!("comments"),
            BlockComment => todo!("comments"),
          },
          Operator(o) => match o {
            MetadataAnnotation => {
              todo!("Encountered metadata in internal expression")
            }
            TypeAnnotation => {
              let mut exp = Self::try_from_tynt_tree(
                children_iter.next().unwrap(),
                structs,
                aliases,
                skolems,
                ctx,
              )?;
              exp.data =
                TypeState::Known(match children_iter.next().unwrap() {
                  TyntTree::Leaf(position, type_name) => Type::from_name(
                    type_name,
                    position.path,
                    structs,
                    aliases,
                    skolems,
                  )?,
                  TyntTree::Inner(
                    (position, Encloser(Parens)),
                    struct_signature_children,
                  ) => {
                    let mut signature_leaves =
                      struct_signature_children.into_iter();
                    if let Some(TyntTree::Leaf(_, struct_name)) =
                      signature_leaves.next()
                    {
                      if signature_leaves.is_empty() {
                        return err(InvalidStructName, vec![position.path]);
                      } else {
                        let generic_args: Vec<TypeState> = signature_leaves
                          .map(|signature_arg| {
                            Ok(TypeState::Known(
                              AbstractType::from_tynt_tree(
                                signature_arg,
                                structs,
                                aliases,
                                &vec![],
                                skolems,
                              )?
                              .concretize(structs, skolems)?,
                            ))
                          })
                          .collect::<CompileResult<Vec<TypeState>>>()?;
                        if let Some(s) =
                          structs.iter().find(|s| s.name == struct_name)
                        {
                          Type::Struct(
                            s.clone().fill_generics_ordered(generic_args),
                          )
                        } else {
                          return err(UnknownStructName, vec![position.path]);
                        }
                      }
                    } else {
                      return err(InvalidStructName, vec![position.path]);
                    }
                  }
                  other => {
                    return err(InvalidType(other), vec![position.path]);
                  }
                });
              exp
            }
            ExpressionComment => todo!("comments"),
          },
        }
      }
    })
  }
  pub fn find_untyped(&self) -> Vec<TypedExp> {
    let mut children_untyped = match &self.kind {
      Wildcard => vec![],
      Name(_) => vec![],
      NumberLiteral(_) => vec![],
      BooleanLiteral(_) => vec![],
      Function(_, body) => body.find_untyped(),
      Application(f, args) => {
        args
          .iter()
          .fold(f.find_untyped(), |mut untyped_so_far, arg| {
            untyped_so_far.append(&mut arg.find_untyped());
            untyped_so_far
          })
      }
      Access(_, exp) => exp.find_untyped(),
      Let(bindings, body) => bindings.iter().fold(
        body.find_untyped(),
        |mut untyped_so_far, (_, _, binding_value)| {
          untyped_so_far.append(&mut binding_value.find_untyped());
          untyped_so_far
        },
      ),
      Match(scrutinee, arms) => arms.iter().fold(
        scrutinee.find_untyped(),
        |mut untyped_so_far, (pattern, arm_body)| {
          untyped_so_far.append(&mut pattern.find_untyped());
          untyped_so_far.append(&mut arm_body.find_untyped());
          untyped_so_far
        },
      ),
      Block(children) => children
        .iter()
        .map(|child| child.find_untyped())
        .fold(vec![], |mut a, mut b| {
          a.append(&mut b);
          a
        }),
    };
    if self.data.is_fully_known() {
      children_untyped
    } else {
      children_untyped.push(self.clone());
      children_untyped
    }
  }
  pub fn propagate_types(&mut self, ctx: &mut Context) -> CompileResult<bool> {
    Ok(match &mut self.kind {
      Wildcard => false,
      Name(name) => {
        if !ctx.is_bound(name) {
          return err(UnboundName(name.clone()), self.source_paths.clone());
        }
        ctx.constrain_name_type(
          name,
          self.source_paths.clone(),
          &mut self.data,
        )?
      }
      NumberLiteral(num) => self.data.constrain(match num {
        Number::Int(_) => TypeState::OneOf(vec![Type::I32, Type::U32]),
        Number::Float(_) => TypeState::Known(Type::F32),
      })?,
      BooleanLiteral(_) => self.data.constrain(TypeState::Known(Type::Bool))?,
      Function(arg_names, body) => {
        if let TypeState::Known(f_type) = &mut self.data {
          let (arg_count, arg_type_states, return_type_state): (
            usize,
            &Vec<TypeState>,
            &mut TypeState,
          ) = match f_type {
            Type::Function(signature) => (
              signature.arg_types.len(),
              &signature.arg_types.clone(),
              &mut signature.return_type,
            ),
            _ => {
              return err(
                FunctionExpressionHasNonFunctionType(f_type.clone()),
                self.source_paths.clone(),
              )
            }
          };
          let return_type_changed =
            body.data.mutually_constrain(return_type_state)?;
          if arg_count == arg_names.len() {
            for (name, t) in arg_names.iter().zip(arg_type_states) {
              ctx.bind(name, Variable::new(t.clone()))
            }
            let body_types_changed = body.propagate_types(ctx)?;
            let argument_types = arg_names
              .iter()
              .map(|name| ctx.unbind(name).typestate)
              .collect::<Vec<_>>();
            let fn_type_changed =
              self.data.constrain_fn_by_argument_types(argument_types)?;
            return_type_changed || body_types_changed || fn_type_changed
          } else {
            return err(WrongArity, self.source_paths.clone());
          }
        } else {
          todo!("I haven't implemented function type inference yet!!!")
        }
      }
      Application(f, args) => {
        let mut anything_changed = false;
        if let Name(name) = &f.kind {
          anything_changed |= ctx.constrain_name_type(
            name,
            self.source_paths.clone(),
            &mut f.data,
          )?;
        } else {
          todo!("I haven't implemented function type inference yet!!!")
        }
        if let TypeState::Known(f_type) = &mut f.data {
          if let Type::Function(signature) = f_type {
            if args.len() == signature.arg_types.len() {
              anything_changed |=
                self.data.mutually_constrain(&mut signature.return_type)?;
              for (arg, t) in
                args.iter_mut().zip(signature.arg_types.iter().cloned())
              {
                anything_changed |= arg.data.constrain(t)?;
              }
            } else {
              return err(WrongArity, self.source_paths.clone());
            }
          } else {
            return err(AppliedNonFunction, self.source_paths.clone());
          }
        }
        for arg in args.iter_mut() {
          anything_changed |= arg.propagate_types(ctx)?;
        }
        anything_changed |= f.data.constrain_fn_by_argument_types(
          args.iter().map(|arg| arg.data.clone()).collect(),
        )?;
        anything_changed |= f.propagate_types(ctx)?;
        anything_changed
      }
      Access(accessor, subexp) => match accessor {
        Accessor::Field(field_name) => {
          let mut anything_changed = false;
          anything_changed |= subexp.propagate_types(ctx)?;
          if let Known(t) = &mut subexp.data {
            if let Type::Struct(s) = t {
              anything_changed |= self.data.mutually_constrain({
                &mut s
                  .fields
                  .iter_mut()
                  .find(|f| f.name == *field_name)
                  .ok_or(CompileError::new(
                    NoSuchField,
                    self.source_paths.clone(),
                  ))?
                  .field_type
              })?;
            } else {
              return err(AccessorOnNonStruct, self.source_paths.clone())?;
            }
          }
          anything_changed
        }
        Accessor::Swizzle(fields) => {
          let mut anything_changed = self
            .data
            .constrain(swizzle_accessor_typestate(&subexp.data, fields))?;
          anything_changed |= subexp
            .data
            .constrain(swizzle_accessed_possibilities(fields))?;
          anything_changed |= subexp.propagate_types(ctx)?;
          anything_changed
        }
      },
      Let(bindings, body) => {
        let mut anything_changed =
          body.data.mutually_constrain(&mut self.data)?;
        for (name, _, value) in bindings.iter_mut() {
          anything_changed |= value.propagate_types(ctx)?;
          ctx.bind(name, Variable::new(value.data.clone()));
        }
        anything_changed |= body.propagate_types(ctx)?;
        for (name, _, value) in bindings.iter_mut() {
          anything_changed |=
            value.data.constrain(ctx.unbind(name).typestate)?;
        }
        anything_changed
      }
      Match(scrutinee, arms) => {
        let mut anything_changed = false;
        anything_changed |= scrutinee.propagate_types(ctx)?;
        for (case, value) in arms.iter_mut() {
          anything_changed |= case.propagate_types(ctx)?;
          anything_changed |= value.propagate_types(ctx)?;
          anything_changed |=
            case.data.mutually_constrain(&mut scrutinee.data)?;
          anything_changed |= value.data.mutually_constrain(&mut self.data)?;
        }
        anything_changed
      }
      Block(children) => {
        let mut anything_changed = false;
        for child in children.iter_mut() {
          anything_changed |= child.propagate_types(ctx)?;
        }
        anything_changed |= self.data.mutually_constrain(
          &mut children
            .last_mut()
            .ok_or(CompileError::new(EmptyBlock, self.source_paths.clone()))?
            .data,
        )?;
        anything_changed
      }
    })
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
          match self.data {
            Known(Type::I32) => "",
            Known(Type::U32) => "u",
            _ => unreachable!(),
          }
        ),
        Number::Float(f) => format!("{f}f"),
      }),
      BooleanLiteral(b) => wrap(format!("{b}")),
      Function(_, _) => panic!("Attempting to compile internal function"),
      Application(f, args) => {
        let f_str = f.compile(InnerExpression);
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
          } else {
            panic!("{} arguments to infix op, expected 2", arg_strs.len())
          }
        } else {
          let args_str = arg_strs.join(", ");
          format!("{f_str}({args_str})")
        })
      }
      Access(accessor, subexp) => wrap(format!(
        "{}.{}",
        subexp.compile(InnerExpression),
        compile_word(accessor.compile())
      )),
      Let(bindings, body) => {
        let binding_lines: Vec<String> = bindings
          .into_iter()
          .map(|(name, variable_kind, value_exp)| {
            format!(
              "{} {name}: {} = {};",
              variable_kind.compile(),
              value_exp.data.compile(),
              value_exp.compile(InnerExpression)
            )
          })
          .collect();
        let value_line = body.compile(position);
        format!(
          "\n{{{}\n}}",
          indent("\n".to_string() + &binding_lines.join("\n") + &value_line)
        )
      }
      Match(scrutinee, arms) => {
        if let ExpKind::Wildcard = arms.last().unwrap().0.kind {
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
        } else {
          if scrutinee.data.unwrap_known() == Type::Bool {
            let mut true_value = None;
            let mut false_value = None;
            for (pattern, value) in arms {
              if let ExpKind::BooleanLiteral(b) = pattern.kind {
                if b {
                  true_value = Some(value);
                } else {
                  false_value = Some(value);
                }
              } else {
                panic!("case of a Bool match block wasn't a BooleanLiteral")
              }
            }
            let true_value =
              true_value.expect("no 'true' case in match block on Bool");
            let false_value =
              false_value.expect("no 'false' case in match block on Bool");
            if position == InnerExpression {
              format!(
                "select({}, {}, {})",
                false_value.compile(InnerExpression),
                true_value.compile(InnerExpression),
                scrutinee.compile(InnerExpression)
              )
            } else {
              format!(
                "\nif ({}) {{\n  {}\n}} else {{\n  {}\n}}",
                scrutinee.compile(InnerExpression),
                true_value.compile(InnerExpression),
                false_value.compile(InnerExpression),
              )
            }
          } else {
            todo!("match block on non-bool scrutinee not yet supported")
          }
        }
      }
      Block(children) => {
        let child_count = children.len();
        let child_strings: Vec<String> = children
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
    }
  }
  pub fn check_assignment_validity(
    &self,
    ctx: &mut Context,
  ) -> CompileResult<()> {
    match &self.kind {
      Application(f, args) => {
        if let ExpKind::Name(f_name) = &f.kind {
          if ASSIGNMENT_OPS.contains(&f_name.as_str()) {
            let mut var = &args[0];
            loop {
              if let ExpKind::Access(_, inner_exp) = &var.kind {
                var = inner_exp;
              } else {
                break;
              }
            }
            if let ExpKind::Name(var_name) = &var.kind {
              if ctx.get_variable_kind(var_name) != &VariableKind::Var {
                return err(
                  AssignmentTargetMustBeVariable,
                  self.source_paths.clone(),
                );
              }
            } else {
              return err(InvalidAssignmentTarget, self.source_paths.clone());
            }
          }
        }
        for arg in args {
          arg.check_assignment_validity(ctx)?;
        }
        f.check_assignment_validity(ctx)
      }
      Function(_, body) => body.check_assignment_validity(ctx),
      Access(_, subexp) => subexp.check_assignment_validity(ctx),
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
        Ok(())
      }
      Match(scrutinee, arms) => {
        for (pattern, value) in arms {
          pattern.check_assignment_validity(ctx)?;
          value.check_assignment_validity(ctx)?;
        }
        scrutinee.check_assignment_validity(ctx)
      }
      Block(subexps) => {
        for subexp in subexps {
          subexp.check_assignment_validity(ctx)?;
        }
        Ok(())
      }
      _ => Ok(()),
    }
  }
  pub fn replace_skolems(&mut self, skolems: &Vec<(String, Type)>) {
    if let Known(t) = &mut self.data {
      t.replace_skolems(skolems)
    }
    match &mut self.kind {
      Application(f, args) => {
        f.replace_skolems(skolems);
        for arg in args.iter_mut() {
          arg.replace_skolems(skolems);
        }
      }
      Function(_, body) => body.replace_skolems(skolems),
      Access(_, body) => body.replace_skolems(skolems),
      Let(bindings, body) => {
        for (_, _, value) in bindings {
          value.replace_skolems(skolems);
        }
        body.replace_skolems(skolems);
      }
      Match(scrutinee, arms) => {
        scrutinee.replace_skolems(skolems);
        for (pattern, value) in arms {
          pattern.replace_skolems(skolems);
          value.replace_skolems(skolems);
        }
      }
      Block(subexps) => {
        for subexp in subexps {
          subexp.replace_skolems(skolems);
        }
      }
      _ => {}
    }
  }
  pub fn monomorphize(
    &mut self,
    base_ctx: &Context,
    new_ctx: &mut Context,
  ) -> CompileResult<()> {
    match &mut self.kind {
      Application(f, args) => {
        f.monomorphize(base_ctx, new_ctx)?;
        for arg in args.iter_mut() {
          arg.monomorphize(base_ctx, new_ctx)?;
        }
        if let ExpKind::Name(f_name) = &mut f.kind {
          if let Some(abstract_signature) =
            if let TypeState::Known(Type::Function(f)) = &f.data {
              &f.abstract_ancestor
            } else {
              unreachable!("encountered applied non-fn in monomorphization")
            }
          {
            match &abstract_signature.implementation {
              FunctionImplementationKind::Builtin => {
                std::mem::swap(
                  f_name,
                  &mut (f_name == "vec2"
                    || f_name == "vec3"
                    || f_name == "vec4")
                    .then(|| {
                      if let TypeState::Known(Type::Struct(s)) = &self.data {
                        compiled_vec_name(
                          &f_name,
                          s.fields[0].field_type.unwrap_known(),
                        )
                      } else {
                        unreachable!("")
                      }
                    })
                    .flatten()
                    .unwrap_or(f_name.clone()),
                );
              }
              FunctionImplementationKind::Constructor => {
                if let Some(abstract_struct) =
                  base_ctx.structs.iter().find(|s| s.name == *f_name)
                {
                  let arg_types: Vec<Type> =
                    args.iter().map(|arg| arg.data.unwrap_known()).collect();
                  if let Some(monomorphized_struct) =
                    abstract_struct.generate_monomorphized(arg_types.clone())
                  {
                    std::mem::swap(
                      f_name,
                      &mut monomorphized_struct
                        .concretized_name(&base_ctx.structs)?,
                    );
                    self.data.with_dereferenced_mut(|typestate| {
                      std::mem::swap(
                        typestate,
                        &mut TypeState::Known(Type::Struct(
                          monomorphized_struct
                            .clone()
                            .fill_generics_ordered(vec![]),
                        )),
                      )
                    });
                    new_ctx.add_monomorphized_struct(monomorphized_struct);
                  }
                }
              }
              FunctionImplementationKind::Composite(_) => {
                if !abstract_signature.generic_args.is_empty() {
                  let monomorphized = abstract_signature
                    .generate_monomorphized(
                      args.iter().map(|arg| arg.data.unwrap_known()).collect(),
                      self.data.unwrap_known().clone(),
                      base_ctx,
                      new_ctx,
                    )?;
                  std::mem::swap(f_name, &mut monomorphized.name.clone());
                  new_ctx.add_abstract_function(monomorphized);
                }
              }
            }
          }
        }
      }
      Function(_, body) => body.monomorphize(base_ctx, new_ctx)?,
      Access(_, body) => body.monomorphize(base_ctx, new_ctx)?,
      Let(bindings, body) => {
        for (_, _, value) in bindings {
          value.monomorphize(base_ctx, new_ctx)?
        }
        body.monomorphize(base_ctx, new_ctx)?
      }
      Match(scrutinee, arms) => {
        scrutinee.monomorphize(base_ctx, new_ctx)?;
        for (pattern, value) in arms {
          pattern.monomorphize(base_ctx, new_ctx)?;
          value.monomorphize(base_ctx, new_ctx)?;
        }
      }
      Block(subexps) => {
        for subexp in subexps {
          subexp.monomorphize(base_ctx, new_ctx)?;
        }
      }
      _ => {}
    }
    Ok(())
  }
}
