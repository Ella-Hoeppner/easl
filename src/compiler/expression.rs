use core::fmt::Debug;

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
pub enum ExpKind<D: Debug + Clone + PartialEq> {
  Name(String),
  NumberLiteral(Number),
  BooleanLiteral(bool),
  Function(Vec<String>, Box<Exp<D>>),
  Application(Box<Exp<D>>, Vec<Exp<D>>),
  Let(Vec<(String, Exp<D>)>, Box<Exp<D>>),
  Match(Box<Exp<D>>, Vec<(Exp<D>, Exp<D>)>),
}
use ExpKind::*;

use crate::{
  compiler::{functions::FunctionSignature, types::extract_type_annotation},
  parse::TyntTree,
};

use super::{
  error::CompileError,
  types::{
    Context, TyntType,
    TypeState::{self, *},
  },
  util::compile_word,
};

impl<D: Debug + Clone + PartialEq> Exp<D> {
  fn map_exp(self, f: impl Fn(ExpKind<D>) -> ExpKind<D>) -> Self {
    Self {
      data: self.data,
      kind: f(self.kind),
    }
  }
  pub fn walk<NewD: Debug + Clone + PartialEq, E>(
    self,
    prewalk_transformer: &mut impl FnMut(Self) -> Result<Self, E>,
    postwalk_transformer: &mut impl FnMut(
      Box<ExpKind<NewD>>,
      D,
    ) -> Result<Exp<NewD>, E>,
  ) -> Result<Exp<NewD>, E> {
    let prewalked_node = prewalk_transformer(self)?;
    let new_exp: ExpKind<NewD> = match prewalked_node.kind {
      Function(arg_names, body) => Function(
        arg_names,
        Box::new(body.walk(prewalk_transformer, postwalk_transformer)?),
      ),
      Application(f_expression, args) => Application(
        Box::new(f_expression.walk(prewalk_transformer, postwalk_transformer)?),
        args
          .into_iter()
          .map(|exp| exp.walk(prewalk_transformer, postwalk_transformer))
          .collect::<Result<_, E>>()?,
      ),
      Let(bindings, body) => Let(
        bindings
          .into_iter()
          .map(|(name, value)| -> Result<_, E> {
            Ok((name, value.walk(prewalk_transformer, postwalk_transformer)?))
          })
          .collect::<Result<_, E>>()?,
        Box::new(body.walk(prewalk_transformer, postwalk_transformer)?),
      ),
      Match(exp, cases) => Match(
        Box::new(exp.walk(prewalk_transformer, postwalk_transformer)?),
        cases
          .into_iter()
          .map(|(pattern, arm_bodies)| {
            Ok((
              pattern.walk(prewalk_transformer, postwalk_transformer)?,
              arm_bodies.walk(prewalk_transformer, postwalk_transformer)?,
            ))
          })
          .collect::<Result<_, E>>()?,
      ),
      Name(name) => Name(name),
      NumberLiteral(num) => NumberLiteral(num),
      BooleanLiteral(b) => BooleanLiteral(b),
    };
    postwalk_transformer(Box::new(new_exp), prewalked_node.data)
  }
  pub fn try_replace_data<NewD: Debug + Clone + PartialEq, E>(
    self,
    data_deriver: &mut impl FnMut(&ExpKind<NewD>, D) -> Result<NewD, E>,
  ) -> Result<Exp<NewD>, E> {
    self.walk(&mut |node| Ok(node), &mut |kind, data| {
      let new_data = data_deriver(&kind, data)?;
      Ok(Exp {
        kind: *kind,
        data: new_data,
      })
    })
  }
  fn replace_name(self, old_name: String, new_name: String) -> Self {
    self
      .walk(
        &mut |node| {
          Ok::<_, !>(node.map_exp(|exp| {
            match exp {
              Name(name) => Name(if name == old_name {
                new_name.clone()
              } else {
                name
              }),
              Let(bindings, args) => Let(
                bindings
                  .into_iter()
                  .map(|(name, value)| {
                    (
                      if name == old_name {
                        new_name.clone()
                      } else {
                        name
                      },
                      value,
                    )
                  })
                  .collect(),
                args,
              ),
              other => other,
            }
          }))
        },
        &mut |kind, data| Ok(Self { kind: *kind, data }),
      )
      .unwrap()
  }
  fn deshadow_bindings(self) -> Self {
    todo!()
  }
  fn lift_internal_lets(self) -> Self {
    todo!()
  }
}

pub type TypedExp = Exp<TypeState>;

impl TypedExp {
  pub fn try_from_tynt_tree(
    tree: TyntTree,
    struct_names: &Vec<String>,
  ) -> Result<Self, CompileError> {
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
          Encloser(e) => {
            match e {
              Parens => {
                if let Some(first_child) = children_iter.next() {
                  match &first_child {
                  TyntTree::Leaf(_, first_child_name) => {
                    match first_child_name.as_str() {
                      "fn" => {
                        if let Some(TyntTree::Inner(
                          (_, Operator(TypeAnnotation)),
                          mut args_and_return_type,
                        )) = children_iter.next()
                        {
                          let return_type = TyntType::from_tynt_tree(
                            args_and_return_type.remove(1),
                            struct_names,
                          )?;
                          if let TyntTree::Inner(
                            (_, Encloser(Square)),
                            arg_asts,
                          ) = args_and_return_type.remove(0)
                          {
                            if children_iter.len() == 1 {
                              let body = children_iter.next().unwrap();
                              let (arg_types, arg_names) = arg_asts
                                .into_iter()
                                .map(|arg| -> Result<_, CompileError> {
                                  let (maybe_t, arg_name_ast) =
                                    extract_type_annotation(arg, struct_names)?;
                                  let t = maybe_t.ok_or(
                                    CompileError::FunctionArgMissingType,
                                  )?;
                                  if let TyntTree::Leaf(_, arg_name) =
                                    arg_name_ast
                                  {
                                    Ok((t, arg_name))
                                  } else {
                                    Err(CompileError::InvalidArgumentName)
                                  }
                                })
                                .collect::<Result<
                                  (Vec<TyntType>, Vec<String>),
                                  CompileError,
                                >>()?;
                              Some(Exp {
                                data: Known(TyntType::Function(Box::new(
                                  FunctionSignature {
                                    arg_types,
                                    return_type,
                                  },
                                ))),
                                kind: ExpKind::Function(
                                  arg_names,
                                  Box::new(Self::try_from_tynt_tree(
                                    body,
                                    struct_names,
                                  )?),
                                ),
                              })
                            } else {
                              todo!("multi-form fn body")
                            }
                          } else {
                            return Err(CompileError::FunctionSignatureNotSquareBrackets);
                          }
                        } else {
                          return Err(CompileError::FunctionSignatureMissingReturnType);
                        }
                      }
                      "let" => todo!("let block"),
                      "match" => todo!("match block"),
                      _ => None,
                    }
                  }
                  _ => None,
                }
                .unwrap_or(Exp {
                  kind: Application(
                    Box::new(Self::try_from_tynt_tree(
                      first_child,
                      struct_names,
                    )?),
                    children_iter
                      .map(|arg| Self::try_from_tynt_tree(arg, struct_names))
                      .collect::<Result<_, _>>()?,
                  ),
                  data: Unknown,
                })
                } else {
                  return Err(CompileError::EmptyList);
                }
              }
              Square => todo!("array"),
              Curly => todo!("anonymous struct"),
            }
          }
          Operator(o) => match o {
            MetadataAnnotation => {
              todo!("Encountered metadata in internal expression")
            }
            TypeAnnotation => {
              let mut exp = Self::try_from_tynt_tree(
                children_iter.next().unwrap(),
                struct_names,
              )?;
              if let TyntTree::Leaf(_, type_name) =
                children_iter.next().unwrap()
              {
                exp.data = Known(TyntType::from_name(type_name, struct_names)?);
                exp
              } else {
                return Err(CompileError::InvalidType);
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
        Let(bindings, body) => bindings.iter().fold(
          body.is_fully_typed(),
          |fully_typed_so_far, (_, binding_value)| {
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
      }
    } else {
      false
    }
  }
  pub fn propagate_types(
    &mut self,
    ctx: &mut Context,
  ) -> Result<bool, CompileError> {
    Ok(match &mut self.kind {
      Name(name) => {
        if !ctx.is_bound(name) {
          return Err(CompileError::UnboundName);
        }
        self.data.mutually_constrain(ctx.get_typestate_mut(name)?)?
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
        if let TypeState::Known(t) = &self.data {
          if let TyntType::Function(signature) = t {
            if signature.arg_types.len() == arg_names.len() {
              for (name, t) in
                arg_names.iter().zip(signature.arg_types.iter().cloned())
              {
                ctx.bind(name, TypeState::Known(t))
              }
              let body_types_changed = body.propagate_types(ctx)?;
              for name in arg_names.iter() {
                ctx.unbind(name);
                // todo! once we're inferring fn types, the type returned here
                // from .unbind should be used to constrain the type in the
                // signature. This will let constraints inferred further down
                // in the tree propagate back up and constraint the fn type
              }
              body_types_changed
            } else {
              return Err(CompileError::IncompatibleTypes);
            }
          } else {
            return Err(CompileError::IncompatibleTypes);
          }
        } else {
          todo!("I haven't implemented function type inference yet!!!")
        }
      }
      Application(f, args) => {
        if let TypeState::Known(f_type) = &f.data {
          if let TyntType::Function(signature) = f_type {
            if args.len() == signature.arg_types.len() {
              let mut anything_changed = self
                .data
                .constrain(TypeState::Known(signature.return_type.clone()))?;
              for (arg, t) in
                args.iter_mut().zip(signature.arg_types.iter().cloned())
              {
                let arg_type_changed =
                  arg.data.constrain(TypeState::Known(t))?;
                anything_changed |= arg_type_changed;
                let arg_changed = arg.propagate_types(ctx)?;
                anything_changed |= arg_changed;
              }
              // todo! once we're inferring fn types, the now-updated argument
              // types should be used to constrain the type of f
              let f_changed = f.propagate_types(ctx)?;
              anything_changed |= f_changed;
              anything_changed
            } else {
              return Err(CompileError::WrongArity);
            }
          } else {
            return Err(CompileError::AppliedNonFunction);
          }
        } else {
          if let Name(name) = &(**f).kind {
            f.data.mutually_constrain(ctx.get_typestate_mut(name)?)?
          } else {
            todo!("I haven't implemented function type inference yet!!!")
          }
        }
      }
      Let(_, _) => todo!("I haven't implemented let blocks yet!!!"),
      Match(_, _) => todo!("I haven't implemented match blocks yet!!!"),
    })
  }
  pub fn compile(self) -> String {
    match self.kind {
      Name(name) => compile_word(name),
      NumberLiteral(num) => match num {
        Number::Int(i) => format!("{i}"),
        Number::Float(f) => format!("{f}f"),
      },
      BooleanLiteral(b) => format!("{b}"),
      Function(_, _) => panic!("Attempting to compile internal function"),
      Application(f, args) => {
        let f_str = f.compile();
        let args_str = args
          .into_iter()
          .map(|arg| arg.compile())
          .collect::<Vec<String>>()
          .join(", ");
        format!("{f_str}({args_str})")
      }
      Let(_, _) => todo!("I haven't implemented let blocks yet!!!"),
      Match(_, _) => todo!("I haven't implemented match blocks yet!!!"),
    }
  }
}
