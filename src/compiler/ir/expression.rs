use crate::{compiler::ir::program::extract_type, parse::TyntTree};

use super::types::{CompileError, TyntType};
use core::fmt::Debug;

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
  Int(i64),
  Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpNode<D: Debug + Clone + PartialEq> {
  pub data: D,
  pub exp: Box<Exp<D>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Exp<D: Debug + Clone + PartialEq> {
  Name(String),
  NumberLiteral(Number),
  BooleanLiteral(bool),
  Function(Vec<String>, ExpNode<D>),
  Application(ExpNode<D>, Vec<ExpNode<D>>),
  Let(Vec<(String, ExpNode<D>)>, ExpNode<D>),
  Match(Box<ExpNode<D>>, Vec<(ExpNode<D>, ExpNode<D>)>),
}
use Exp::*;

impl<D: Debug + Clone + PartialEq> ExpNode<D> {
  fn map_exp(self, f: impl Fn(Exp<D>) -> Exp<D>) -> Self {
    Self {
      data: self.data,
      exp: Box::new(f(*self.exp)),
    }
  }
  pub fn walk<NewD: Debug + Clone + PartialEq, E>(
    self,
    prewalk_transformer: &mut impl FnMut(Self) -> Result<Self, E>,
    postwalk_transformer: &mut impl FnMut(
      Box<Exp<NewD>>,
      D,
    ) -> Result<ExpNode<NewD>, E>,
  ) -> Result<ExpNode<NewD>, E> {
    let prewalked_node = prewalk_transformer(self)?;
    let new_exp: Exp<NewD> = match *prewalked_node.exp {
      Function(arg_names, body) => Function(
        arg_names,
        body.walk(prewalk_transformer, postwalk_transformer)?,
      ),
      Application(f_expression, args) => Application(
        f_expression.walk(prewalk_transformer, postwalk_transformer)?,
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
        body.walk(prewalk_transformer, postwalk_transformer)?,
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
    data_deriver: &mut impl FnMut(&Exp<NewD>, D) -> Result<NewD, E>,
  ) -> Result<ExpNode<NewD>, E> {
    self.walk(&mut |node| Ok(node), &mut |exp, data| {
      let new_data = data_deriver(&exp, data)?;
      Ok(ExpNode {
        exp,
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
        &mut |exp, data| Ok(Self { exp, data }),
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

impl ExpNode<Option<TyntType>> {
  pub fn try_from_tynt_tree(
    tree: TyntTree,
    struct_names: &Vec<String>,
  ) -> Result<Self, CompileError> {
    Ok(match tree {
      TyntTree::Leaf(_, leaf) => {
        if leaf == "true" || leaf == "false" {
          ExpNode {
            exp: Box::new(Exp::BooleanLiteral(leaf == "true")),
            data: Some(TyntType::Bool),
          }
        } else if let Ok(i) = leaf.parse::<i64>() {
          ExpNode {
            exp: Box::new(Exp::NumberLiteral(Number::Int(i))),
            data: Some(TyntType::I32),
          }
        } else if let Ok(f) = leaf.parse::<f64>() {
          ExpNode {
            exp: Box::new(Exp::NumberLiteral(Number::Float(f))),
            data: Some(TyntType::F32),
          }
        } else {
          ExpNode {
            exp: Box::new(Exp::Name(leaf)),
            data: None,
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
                            arg_exps,
                          ) = args_and_return_type.remove(0)
                          {
                            if children_iter.len() == 1 {
                              let body = children_iter.next().unwrap();
                              let (arg_types, arg_names) = arg_exps
                                .into_iter()
                                .map(|arg| -> Result<_, CompileError> {
                                  let (maybe_t, arg_name_exp) =
                                    extract_type(arg, struct_names)?;
                                  let t = maybe_t.ok_or(
                                    CompileError::FunctionArgMissingType,
                                  )?;
                                  if let TyntTree::Leaf(_, arg_name) =
                                    arg_name_exp
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
                              Some(ExpNode {
                                data: Some(TyntType::Function(
                                  arg_types,
                                  Box::new(return_type),
                                )),
                                exp: Box::new(Exp::Function(
                                  arg_names,
                                  Self::try_from_tynt_tree(body, struct_names)?,
                                )),
                              })
                            } else {
                              todo!("multi-form fn body")
                            }
                          } else {
                            return Err(CompileError::InvalidFunction(
                              "invalid argument list for fn: args list not \
                              square-bracket-enclosed"
                                .to_string(),
                            ));
                          }
                        } else {
                          return Err(CompileError::InvalidFunction(
                            "invalid argument list for fn: args list isn't a \
                            metadata annotation"
                              .to_string(),
                          ));
                        }
                      }
                      "let" => todo!("let block"),
                      "match" => todo!("match block"),
                      _ => None,
                    }
                  }
                  _ => None,
                }
                .unwrap_or(ExpNode {
                  exp: Box::new(Application(
                    Self::try_from_tynt_tree(first_child, struct_names)?,
                    children_iter
                      .map(|arg| Self::try_from_tynt_tree(arg, struct_names))
                      .collect::<Result<_, _>>()?,
                  )),
                  data: None,
                })
              } else {
                return Err(CompileError::EmptyList);
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
              let mut exp: ExpNode<Option<TyntType>> =
                Self::try_from_tynt_tree(
                  children_iter.next().unwrap(),
                  struct_names,
                )?;
              if let TyntTree::Leaf(_, type_name) =
                children_iter.next().unwrap()
              {
                exp.data = Some(TyntType::from_name(type_name, struct_names)?);
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
}
