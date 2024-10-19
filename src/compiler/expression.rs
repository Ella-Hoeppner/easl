use core::fmt::Debug;
use std::{collections::HashMap, str::pattern::Pattern};

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
  let empty_generics = HashMap::new();
  TypeState::Known(match fields.len() {
    2 => {
      Type::Struct(get_builtin_struct("vec2f").fill_generics(&empty_generics))
    }
    3 => {
      Type::Struct(get_builtin_struct("vec3f").fill_generics(&empty_generics))
    }
    4 => {
      Type::Struct(get_builtin_struct("vec4f").fill_generics(&empty_generics))
    }
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
      .map(|name| {
        Type::Struct(get_builtin_struct(name).fill_generics(&HashMap::new()))
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

use super::{functions::FunctionImplementationKind, types::AbstractType};

#[derive(Clone, Copy)]
pub enum ExpressionCompilationPosition {
  Return,
  InnerLine,
  InnerExpression,
}

pub type TypedExp = Exp<TypeState>;

pub fn arg_list_and_return_type_from_tynt_tree(
  tree: TyntTree,
  structs: &Vec<AbstractStruct>,
  generic_args: &Vec<String>,
) -> CompileResult<(
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
    (_, Operator(TypeAnnotation)),
    mut args_and_return_type,
  ) = tree
  {
    let return_type_ast = args_and_return_type.remove(1);
    let (return_metadata, return_type_ast) = extract_metadata(return_type_ast)?;
    let return_type = AbstractType::from_tynt_tree(
      return_type_ast,
      structs,
      generic_args,
      &vec![],
    )?;
    if let TyntTree::Inner((_, Encloser(Square)), arg_asts) =
      args_and_return_type.remove(0)
    {
      let ((arg_types, arg_metadata), arg_names) = arg_asts
        .into_iter()
        .map(|arg| -> CompileResult<_> {
          let (maybe_t_ast, arg_name_ast) = extract_type_annotation_ast(arg)?;
          let t_ast =
            maybe_t_ast.ok_or(CompileError::from(FunctionArgMissingType))?;
          let (arg_metadata, t_ast) = extract_metadata(t_ast)?;
          let t = AbstractType::from_tynt_tree(
            t_ast,
            structs,
            generic_args,
            &vec![],
          )?;
          if let TyntTree::Leaf(_, arg_name) = arg_name_ast {
            Ok(((t, arg_metadata), arg_name))
          } else {
            err(InvalidArgumentName)
          }
        })
        .collect::<CompileResult<(
          (Vec<AbstractType>, Vec<Option<Metadata>>),
          Vec<String>,
        )>>()?;
      Ok((
        arg_names,
        arg_types,
        arg_metadata,
        return_type,
        return_metadata,
      ))
    } else {
      return err(FunctionSignatureNotSquareBrackets);
    }
  } else {
    return err(FunctionSignatureMissingReturnType);
  }
}

impl TypedExp {
  pub fn function_from_body_tree(
    body_trees: Vec<TyntTree>,
    return_type: TypeState,
    arg_names: Vec<String>,
    arg_types: Vec<TypeState>,
    structs: &Vec<AbstractStruct>,
    skolems: &Vec<String>,
  ) -> CompileResult<Self> {
    let mut body_exps = body_trees
      .into_iter()
      .map(|t| Self::try_from_tynt_tree(t, structs, skolems))
      .collect::<CompileResult<Vec<TypedExp>>>()?;
    let body = if body_exps.len() == 1 {
      body_exps.remove(0)
    } else {
      Exp {
        data: Unknown,
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
    })
  }
  pub fn try_from_tynt_tree(
    tree: TyntTree,
    structs: &Vec<AbstractStruct>,
    skolems: &Vec<String>,
  ) -> CompileResult<Self> {
    Ok(match tree {
      TyntTree::Leaf(_, leaf) => {
        if leaf == "true" || leaf == "false" {
          Exp {
            kind: ExpKind::BooleanLiteral(leaf == "true"),
            data: Known(Type::Bool),
          }
        } else if let Ok(i) = leaf.parse::<i64>() {
          Exp {
            kind: ExpKind::NumberLiteral(Number::Int(i)),
            data: Known(Type::I32),
          }
        } else if let Ok(f) = leaf.parse::<f64>() {
          Exp {
            kind: ExpKind::NumberLiteral(Number::Float(f)),
            data: Known(Type::F32),
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
                              skolems,
                            )?),
                          ),
                        })
                      } else {
                        return err(AccessorHadMultipleArguments);
                      }
                    } else {
                      match first_child_name.as_str() {
                        "fn" => {
                          let (
                            arg_names,
                            arg_types,
                            _arg_metadata,
                            return_type,
                            _return_metadata,
                          ) = arg_list_and_return_type_from_tynt_tree(
                            children_iter
                              .next()
                              .ok_or(CompileError::from(InvalidFunction))?,
                            structs,
                            &vec![],
                          )?;
                          Some(Self::function_from_body_tree(
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
                            skolems,
                          )?)
                        }
                        "let" => {
                          if children_iter.len() < 2 {
                            return err(NotEnoughLetBlockChildren);
                          }
                          let bindings_ast = children_iter.next().unwrap();
                          let mut child_exps = children_iter
                            .clone()
                            .map(|child| {
                              Self::try_from_tynt_tree(child, structs, skolems)
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
                                      value_ast, structs, skolems,
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
                              Self::try_from_tynt_tree(child, structs, skolems)
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
                    Box::new(Self::try_from_tynt_tree(
                      first_child,
                      structs,
                      skolems,
                    )?),
                    children_iter
                      .map(|arg| {
                        Self::try_from_tynt_tree(arg, structs, skolems)
                      })
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
                skolems,
              )?;
              exp.data =
                TypeState::Known(match children_iter.next().unwrap() {
                  TyntTree::Leaf(_, type_name) => {
                    Type::from_name(type_name, structs, skolems)?
                  }
                  TyntTree::Inner(
                    (_, Encloser(Parens)),
                    struct_signature_children,
                  ) => {
                    let mut signature_leaves =
                      struct_signature_children.into_iter();
                    if let Some(TyntTree::Leaf(_, struct_name)) =
                      signature_leaves.next()
                    {
                      if signature_leaves.is_empty() {
                        return err(InvalidStructName);
                      } else {
                        let generic_args: Vec<TypeState> = signature_leaves
                          .map(|signature_arg| {
                            Ok(TypeState::Known(
                              AbstractType::from_tynt_tree(
                                signature_arg,
                                structs,
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
                          return err(UnknownStructName);
                        }
                      }
                    } else {
                      return err(InvalidStructName);
                    }
                  }
                  other => {
                    return err(InvalidType(other));
                  }
                });
              exp
            }
          },
        }
      }
    })
  }
  pub fn find_untyped(&self) -> Vec<TypedExp> {
    let mut children_untyped = match &self.kind {
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
    if let Known(_) = self.data {
      children_untyped
    } else {
      children_untyped.push(self.clone());
      children_untyped
    }
  }
  pub fn propagate_types(&mut self, ctx: &mut Context) -> CompileResult<bool> {
    Ok(match &mut self.kind {
      Name(name) => {
        if !ctx.is_bound(name) {
          return err(UnboundName(name.clone()));
        }
        ctx.constrain_name_type(name, &mut self.data)?
      }
      NumberLiteral(num) => {
        self.data.constrain(TypeState::Known(match num {
          Number::Int(_) => Type::I32,
          Number::Float(_) => Type::F32,
        }))?
      }
      BooleanLiteral(_) => self.data.constrain(TypeState::Known(Type::Bool))?,
      Function(arg_names, body) => {
        if let TypeState::Known(f_type) = &mut self.data {
          let (arg_count, arg_type_states): (usize, Vec<TypeState>) =
            match f_type {
              Type::Function(signature) => (
                signature.arg_types.len(),
                signature.arg_types.iter().cloned().collect(),
              ),
              _ => {
                return err(FunctionExpressionHasNonFunctionType(
                  f_type.clone(),
                ))
              }
            };
          if arg_count == arg_names.len() {
            for (name, t) in arg_names.iter().zip(arg_type_states) {
              ctx.bind(name, Variable::new(t))
            }
            let body_types_changed = body.propagate_types(ctx)?;
            let argument_types = arg_names
              .iter()
              .map(|name| ctx.unbind(name).typestate)
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
            if let Type::Function(signature) = f_type {
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
            } else {
              return err(AppliedNonFunction);
            }
          } else {
            None
          };
        if let Some(concrete_signature) = replacement_concrete_signature {
          std::mem::swap(
            &mut f.data,
            &mut TypeState::Known(Type::Function(Box::new(concrete_signature))),
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
          anything_changed |= subexp.propagate_types(ctx)?;
          if let Known(t) = &mut subexp.data {
            if let Type::Struct(s) = t {
              anything_changed |= self.data.mutually_constrain({
                &mut s
                  .fields
                  .iter_mut()
                  .find(|f| f.name == *field_name)
                  .ok_or(CompileError::new(NoSuchField))?
                  .field_type
              })?;
            } else {
              return err(AccessorOnNonStruct)?;
            }
          }
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
          ctx.bind(name, Variable::new(value.data.clone()));
        }
        anything_changed |= body.propagate_types(ctx)?;
        for (name, _, value) in bindings.iter_mut() {
          anything_changed |=
            value.data.constrain(ctx.unbind(name).typestate)?;
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
                return err(AssignmentTargetMustBeVariable);
              }
            } else {
              return err(InvalidAssignmentTarget);
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
  pub fn monomorphize(&mut self, base_ctx: &Context, new_ctx: &mut Context) {
    match &mut self.kind {
      Application(f, args) => {
        f.monomorphize(base_ctx, new_ctx);
        for arg in args.iter_mut() {
          arg.monomorphize(base_ctx, new_ctx);
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
              FunctionImplementationKind::Builtin => {}
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
                      &mut monomorphized_struct.name.clone(),
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
              FunctionImplementationKind::Composite(f) => {
                if !abstract_signature.generic_args.is_empty() {
                  let monomorphized = abstract_signature
                    .generate_monomorphized(
                      args.iter().map(|arg| arg.data.unwrap_known()).collect(),
                      self.data.unwrap_known().clone(),
                    );
                  std::mem::swap(f_name, &mut monomorphized.name.clone());
                  new_ctx.add_abstract_function(monomorphized);
                }
              }
            }
          }
        }
      }
      Function(_, body) => body.monomorphize(base_ctx, new_ctx),
      Access(_, body) => body.monomorphize(base_ctx, new_ctx),
      Let(bindings, body) => {
        for (_, _, value) in bindings {
          value.monomorphize(base_ctx, new_ctx)
        }
        body.monomorphize(base_ctx, new_ctx)
      }
      Match(scrutinee, arms) => {
        scrutinee.monomorphize(base_ctx, new_ctx);
        for (pattern, value) in arms {
          pattern.monomorphize(base_ctx, new_ctx);
          value.monomorphize(base_ctx, new_ctx);
        }
      }
      Block(subexps) => {
        for subexp in subexps {
          subexp.monomorphize(base_ctx, new_ctx);
        }
      }
      _ => {}
    }
  }
}
