use core::fmt::Debug;
use std::str::pattern::Pattern;

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
  Int(i64),
  Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Exp<D: Debug + Clone + PartialEq> {
  pub data: D,
  pub kind: ExpKind<D>,
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

pub fn swizzle_accessor_typestate(fields: &Vec<SwizzleField>) -> TypeState {
  TypeState::Known(match fields.len() {
    2 => TyntType::Struct(get_builtin_struct("vec2f")),
    3 => TyntType::Struct(get_builtin_struct("vec3f")),
    4 => TyntType::Struct(get_builtin_struct("vec4f")),
    n => unreachable!("swizzle with {n} elements, expected 2-4"),
  })
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
    ["vec4f", "vec3f", "vec2f"]
      .iter()
      .take(3.min(5 - max_accessed_index))
      .map(|name| TyntType::Struct(get_builtin_struct(name)))
      .collect::<Vec<TyntType>>(),
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

use crate::{
  compiler::{
    builtins::{ASSIGNMENT_OPS, INFIX_OPS},
    error::{err, CompileError},
    functions::AbstractFunctionSignature,
    metadata::{extract_metadata, Metadata},
    types::extract_type_annotation,
    util::indent,
  },
  parse::TyntTree,
};

use super::{
  builtins::get_builtin_struct,
  error::{CompileErrorKind::*, CompileResult},
  structs::Struct,
  types::{
    Bindings, Context, TyntType,
    TypeState::{self, *},
    Variable, VariableKind,
  },
  util::compile_word,
};

#[derive(Clone, Copy)]
pub enum ExpressionCompilationPosition {
  Return,
  InnerLine,
  InnerExpression,
}

pub type TypedExp = Exp<TypeState>;

impl TypedExp {
  pub fn try_from_tynt_tree(
    tree: TyntTree,
    structs: &Vec<Struct>,
  ) -> CompileResult<Self> {
    Ok(match tree {
      TyntTree::Leaf(_, leaf) => {
        if leaf == "true" || leaf == "false" {
          Exp {
            kind: ExpKind::BooleanLiteral(leaf == "true"),
            data: Known(TyntType::Bool),
          }
        } else if let Ok(i) = leaf.parse::<i64>() {
          Exp {
            kind: ExpKind::NumberLiteral(Number::Int(i)),
            data: Known(TyntType::I32),
          }
        } else if let Ok(f) = leaf.parse::<f64>() {
          Exp {
            kind: ExpKind::NumberLiteral(Number::Float(f)),
            data: Known(TyntType::F32),
          }
        } else {
          Exp {
            kind: ExpKind::Name(leaf),
            data: Unknown,
          }
        }
      }
      TyntTree::Inner((_, encloser_or_operator), children) => {
        use crate::parse::Encloser::*;
        use crate::parse::Operator::*;
        use sse::syntax::EncloserOrOperator::*;
        let mut children_iter = children.into_iter();
        match encloser_or_operator {
          Encloser(e) => match e {
            Parens => {
              if let Some(first_child) = children_iter.next() {
                match &first_child {
                  TyntTree::Leaf(_, first_child_name) => {
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
                            )?),
                          ),
                        })
                      } else {
                        return err(AccessorHadMultipleArguments);
                      }
                    } else {
                      match first_child_name.as_str() {
                        "fn" => {
                          if let Some(TyntTree::Inner(
                            (_, Operator(TypeAnnotation)),
                            mut args_and_return_type,
                          )) = children_iter.next()
                          {
                            let return_type = TyntType::from_tynt_tree(
                              args_and_return_type.remove(1),
                              structs,
                            )?;
                            if let TyntTree::Inner(
                              (_, Encloser(Square)),
                              arg_asts,
                            ) = args_and_return_type.remove(0)
                            {
                              let mut children = children_iter
                                .clone()
                                .map(|child| {
                                  Self::try_from_tynt_tree(child, structs)
                                })
                                .collect::<CompileResult<Vec<_>>>()?;
                              let body = if children_iter.len() == 1 {
                                children.remove(0)
                              } else {
                                Exp {
                                  data: Unknown,
                                  kind: ExpKind::Block(children),
                                }
                              };
                              let (arg_types, arg_names) = arg_asts
                                .into_iter()
                                .map(|arg| -> CompileResult<_> {
                                  let (maybe_t, arg_name_ast) =
                                    extract_type_annotation(arg, structs)?;
                                  let t = maybe_t.ok_or(
                                    CompileError::from(FunctionArgMissingType),
                                  )?;
                                  if let TyntTree::Leaf(_, arg_name) =
                                    arg_name_ast
                                  {
                                    Ok((t, arg_name))
                                  } else {
                                    err(InvalidArgumentName)
                                  }
                                })
                                .collect::<CompileResult<
                                  (Vec<TyntType>, Vec<String>),
                                >>()?;
                              Some(Exp {
                                data: Known(TyntType::AbstractFunction(
                                  Box::new(AbstractFunctionSignature {
                                    generic_args: vec![],
                                    arg_types,
                                    return_type,
                                  }),
                                )),
                                kind: ExpKind::Function(
                                  arg_names,
                                  Box::new(body),
                                ),
                              })
                            } else {
                              return err(FunctionSignatureNotSquareBrackets);
                            }
                          } else {
                            return err(FunctionSignatureMissingReturnType);
                          }
                        }
                        "let" => {
                          if children_iter.len() < 2 {
                            return err(NotEnoughLetBlockChildren);
                          }
                          let bindings_ast = children_iter.next().unwrap();
                          let mut child_exps = children_iter
                            .clone()
                            .map(|child| {
                              Self::try_from_tynt_tree(child, structs)
                            })
                            .collect::<CompileResult<Vec<Self>>>()?;
                          let body_exp = if child_exps.len() == 1 {
                            child_exps.remove(0)
                          } else {
                            Exp {
                              data: Unknown,
                              kind: ExpKind::Block(child_exps),
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
                                if let TyntTree::Leaf(_, name) = name_ast {
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
                                            )
                                          }
                                        }
                                      }
                                      Some(metadata) => {
                                        return err(InvalidVariableMetadata(
                                          metadata,
                                        ))
                                      }
                                    },
                                    Self::try_from_tynt_tree(
                                      value_ast, structs,
                                    )?,
                                  ));
                                } else {
                                  return err(ExpectedBindingName);
                                }
                              }
                              Some(Exp {
                                data: Unknown,
                                kind: ExpKind::Let(
                                  bindings,
                                  Box::new(body_exp),
                                ),
                              })
                            } else {
                              return err(OddNumberOfChildrenInLetBindings);
                            }
                          } else {
                            return err(LetBindingsNotSquareBracketed);
                          }
                        }
                        "block" => {
                          if children_iter.is_empty() {
                            return err(EmptyBlock);
                          }
                          let child_exps = children_iter
                            .clone()
                            .map(|child| {
                              Self::try_from_tynt_tree(child, structs)
                            })
                            .collect::<CompileResult<Vec<Self>>>()?;
                          Some(Exp {
                            data: Unknown,
                            kind: ExpKind::Block(child_exps),
                          })
                        }
                        "match" => todo!("match block"),
                        _ => None,
                      }
                    }
                  }
                  _ => None,
                }
                .unwrap_or(Exp {
                  kind: Application(
                    Box::new(Self::try_from_tynt_tree(first_child, structs)?),
                    children_iter
                      .map(|arg| Self::try_from_tynt_tree(arg, structs))
                      .collect::<CompileResult<_>>()?,
                  ),
                  data: Unknown,
                })
              } else {
                return err(EmptyList);
              }
            }
            Square => todo!("array"),
            Curly => todo!("anonymous struct"),
          },
          Operator(o) => match o {
            MetadataAnnotation => {
              todo!("Encountered metadata in internal expression")
            }
            TypeAnnotation => {
              let mut exp = Self::try_from_tynt_tree(
                children_iter.next().unwrap(),
                structs,
              )?;
              if let TyntTree::Leaf(_, type_name) =
                children_iter.next().unwrap()
              {
                exp.data = Known(TyntType::from_name(type_name, structs)?);
                exp
              } else {
                return err(InvalidType);
              }
            }
          },
        }
      }
    })
  }
  pub fn is_fully_typed(&self) -> bool {
    if let Known(_) = self.data {
      match &self.kind {
        Name(_) => true,
        NumberLiteral(_) => true,
        BooleanLiteral(_) => true,
        Function(_, body) => body.is_fully_typed(),
        Application(f, args) => args
          .iter()
          .fold(f.is_fully_typed(), |fully_typed_so_far, arg| {
            fully_typed_so_far && arg.is_fully_typed()
          }),
        Access(_, exp) => exp.is_fully_typed(),
        Let(bindings, body) => bindings.iter().fold(
          body.is_fully_typed(),
          |fully_typed_so_far, (_, _, binding_value)| {
            fully_typed_so_far && binding_value.is_fully_typed()
          },
        ),
        Match(scrutinee, arms) => arms.iter().fold(
          scrutinee.is_fully_typed(),
          |fully_typed_so_far, (pattern, arm_body)| {
            fully_typed_so_far
              && pattern.is_fully_typed()
              && arm_body.is_fully_typed()
          },
        ),
        Block(children) => children
          .iter()
          .map(|child| child.is_fully_typed())
          .reduce(|a, b| a && b)
          .unwrap_or(true),
      }
    } else {
      false
    }
  }
  pub fn propagate_types(&mut self, ctx: &mut Context) -> CompileResult<bool> {
    Ok(match &mut self.kind {
      Name(name) => {
        if !ctx.bindings.is_bound(name) {
          return err(UnboundName(name.clone()));
        }
        ctx.constrain_name_type(name, &mut self.data)?
      }
      NumberLiteral(num) => {
        self.data.constrain(TypeState::Known(match num {
          Number::Int(_) => TyntType::I32,
          Number::Float(_) => TyntType::F32,
        }))?
      }
      BooleanLiteral(_) => {
        self.data.constrain(TypeState::Known(TyntType::Bool))?
      }
      Function(arg_names, body) => {
        if let TypeState::Known(f_type) = &mut self.data {
          let (arg_count, arg_type_states): (usize, Vec<TypeState>) =
            match f_type {
              TyntType::ConcreteFunction(signature) => (
                signature.arg_types.len(),
                signature.arg_types.iter().cloned().collect(),
              ),
              TyntType::AbstractFunction(signature) => (
                signature.arg_types.len(),
                signature
                  .arg_types
                  .iter()
                  .cloned()
                  .map(|t| TypeState::Known(t))
                  .collect(),
              ),
              _ => {
                return err(FunctionExpressionHasNonFunctionType(
                  f_type.clone(),
                ))
              }
            };
          if arg_count == arg_names.len() {
            for (name, t) in arg_names.iter().zip(arg_type_states) {
              ctx.bindings.bind(name, Variable::new(t))
            }
            let body_types_changed = body.propagate_types(ctx)?;
            let argument_types = arg_names
              .iter()
              .map(|name| ctx.bindings.unbind(name).typestate)
              .collect::<Vec<_>>();
            let fn_type_changed =
              self.data.constrain_fn_by_argument_types(argument_types)?;
            body_types_changed || fn_type_changed
          } else {
            return err(WrongArity);
          }
        } else {
          todo!("I haven't implemented function type inference yet!!!")
        }
      }
      Application(f, args) => {
        let mut anything_changed = false;
        if let Name(name) = &f.kind {
          anything_changed |= ctx.constrain_name_type(name, &mut f.data)?;
        } else {
          todo!("I haven't implemented function type inference yet!!!")
        }
        let replacement_concrete_signature =
          if let TypeState::Known(f_type) = &mut f.data {
            match f_type {
              TyntType::ConcreteFunction(signature) => {
                if args.len() == signature.arg_types.len() {
                  anything_changed |=
                    self.data.mutually_constrain(&mut signature.return_type)?;
                  for (arg, t) in
                    args.iter_mut().zip(signature.arg_types.iter().cloned())
                  {
                    anything_changed |= arg.data.constrain(t)?;
                  }
                  None
                } else {
                  return err(WrongArity);
                }
              }
              TyntType::AbstractFunction(signature) => {
                Some(signature.concretize())
              }
              _ => {
                return err(AppliedNonFunction);
              }
            }
          } else {
            None
          };
        if let Some(concrete_signature) = replacement_concrete_signature {
          std::mem::swap(
            &mut f.data,
            &mut TypeState::Known(TyntType::ConcreteFunction(Box::new(
              concrete_signature,
            ))),
          );
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
          let (field_possibilities, struct_possibilities): (
            Vec<TypeState>,
            Vec<TyntType>,
          ) = ctx
            .structs
            .iter()
            .filter_map(|s| {
              s.fields.iter().find(|field| field.name == *field_name).map(
                |field| (field.field_type.clone(), TyntType::Struct(s.clone())),
              )
            })
            .collect();
          if field_possibilities.len() == 0 {
            return err(NoSuchField);
          }
          anything_changed |= self
            .data
            .constrain(TypeState::any_of(field_possibilities)?)?;
          anything_changed |= subexp
            .data
            .constrain(TypeState::OneOf(struct_possibilities).simplified()?)?;
          anything_changed |= subexp.propagate_types(ctx)?;
          let field_type_possibilities: Vec<TypeState> = match &subexp.data {
            Unknown => unreachable!(),
            OneOf(possibilities) => possibilities
              .iter()
              .map(|t| -> CompileResult<TypeState> {
                Ok(match t {
                  TyntType::Struct(s) => ctx
                    .structs
                    .iter()
                    .find(|s| s.name == *s.name)
                    .unwrap()
                    .fields
                    .iter()
                    .find(|f| f.name == *field_name)
                    .ok_or(NoSuchField)?
                    .field_type
                    .clone(),
                  _ => unreachable!(),
                })
              })
              .collect::<CompileResult<Vec<TypeState>>>()?,
            Known(subexp_type) => match subexp_type {
              TyntType::Struct(s) => {
                vec![ctx
                  .structs
                  .iter()
                  .find(|other_s| other_s.name == s.name)
                  .unwrap()
                  .fields
                  .iter()
                  .find(|f| f.name == *field_name)
                  .ok_or(NoSuchField)?
                  .field_type
                  .clone()]
              }
              _ => unreachable!(),
            },
            UnificationVariable(_) => todo!("unification"),
          };
          anything_changed |= self
            .data
            .constrain(TypeState::any_of(field_type_possibilities)?)?;
          anything_changed
        }
        Accessor::Swizzle(fields) => {
          let mut anything_changed =
            self.data.constrain(swizzle_accessor_typestate(fields))?;
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
          ctx.bindings.bind(name, Variable::new(value.data.clone()));
        }
        anything_changed |= body.propagate_types(ctx)?;
        for (name, _, value) in bindings.iter_mut() {
          anything_changed |=
            value.data.constrain(ctx.bindings.unbind(name).typestate)?;
        }
        anything_changed
      }
      Match(_, _) => todo!("I haven't implemented match blocks yet!!!"),
      Block(children) => {
        let mut anything_changed = false;
        for child in children.iter_mut() {
          anything_changed |= child.propagate_types(ctx)?;
        }
        anything_changed |= self.data.mutually_constrain(
          &mut children.last_mut().ok_or(EmptyBlock)?.data,
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
      Name(name) => wrap(compile_word(name)),
      NumberLiteral(num) => wrap(match num {
        Number::Int(i) => format!("{i}"),
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
      Match(_, _) => todo!("I haven't implemented match blocks yet!!!"),
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
    bindings: &mut Bindings,
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
              if bindings.get_variable_kind(var_name) != &VariableKind::Var {
                return err(AssignmentTargetMustBeVariable);
              }
            } else {
              return err(InvalidAssignmentTarget);
            }
          }
        }
        for arg in args {
          arg.check_assignment_validity(bindings)?;
        }
        f.check_assignment_validity(bindings)
      }
      Function(_, body) => body.check_assignment_validity(bindings),
      Access(_, subexp) => subexp.check_assignment_validity(bindings),
      Let(binding_names_and_values, body) => {
        for (name, kind, value) in binding_names_and_values {
          value.check_assignment_validity(bindings)?;
          bindings.bind(
            name,
            Variable::new(value.data.clone()).with_kind(kind.clone()),
          )
        }
        body.check_assignment_validity(bindings)?;
        for (name, _, value) in binding_names_and_values {
          value.check_assignment_validity(bindings)?;
          bindings.unbind(name);
        }
        Ok(())
      }
      Match(scrutinee, arms) => {
        for (pattern, value) in arms {
          pattern.check_assignment_validity(bindings)?;
          value.check_assignment_validity(bindings)?;
        }
        scrutinee.check_assignment_validity(bindings)
      }
      Block(subexps) => {
        for subexp in subexps {
          subexp.check_assignment_validity(bindings)?;
        }
        Ok(())
      }
      _ => Ok(()),
    }
  }
}
