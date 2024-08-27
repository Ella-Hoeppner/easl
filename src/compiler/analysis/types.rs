use crate::parse::TyntTree;

use super::{
  environment::Environment,
  expression::{
    Exp::{self, *},
    ExpNode, Number,
  },
};
use core::fmt::Debug;
use std::{cell::RefCell, rc::Rc};

pub enum TypeError {
  CouldntInfer,
  IncompatibleConstraints,
  UnknownFunction,
  InvalidArity,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyntType {
  None,
  Float,
  Int,
  UInt,
  Bool,
}

#[derive(Debug, Clone, PartialEq)]
enum TypeConstraints {
  Anything,
  OneOf(Vec<TyntType>),
  Definitely(TyntType),
}

use TypeConstraints::*;
impl TypeConstraints {
  fn restrict(&mut self, constraints: Self) -> Result<(), TypeError> {
    match (self.clone(), constraints) {
      (_, Anything) => Ok(()),
      (Anything, _) => Ok(()),
      (Definitely(a), Definitely(b)) => {
        if a == b {
          Ok(())
        } else {
          Err(TypeError::IncompatibleConstraints)
        }
      }
      (OneOf(a), OneOf(b)) => {
        let narrowed: Vec<_> =
          a.into_iter().filter(|a| b.contains(&a)).collect();
        if narrowed.is_empty() {
          Err(TypeError::IncompatibleConstraints)
        } else {
          std::mem::swap(
            self,
            &mut if narrowed.len() == 1 {
              Definitely(narrowed.into_iter().next().unwrap())
            } else {
              OneOf(narrowed)
            },
          );
          Ok(())
        }
      }
      (OneOf(a), Definitely(b)) => {
        if a.contains(&b) {
          std::mem::swap(self, &mut Definitely(b));
          Ok(())
        } else {
          Err(TypeError::IncompatibleConstraints)
        }
      }
      (Definitely(a), OneOf(b)) => {
        if b.contains(&a) {
          std::mem::swap(self, &mut Definitely(a));
          Ok(())
        } else {
          Err(TypeError::IncompatibleConstraints)
        }
      }
    }
  }
}

#[derive(Debug, Clone)]
pub struct ConstraintState {
  constraints: Rc<RefCell<TypeConstraints>>,
}
impl PartialEq for ConstraintState {
  fn eq(&self, other: &Self) -> bool {
    Rc::ptr_eq(&self.constraints, &other.constraints)
  }
}
impl From<TypeConstraints> for ConstraintState {
  fn from(constraints: TypeConstraints) -> Self {
    Self {
      constraints: Rc::new(RefCell::new(constraints)),
    }
  }
}
impl ConstraintState {
  fn restrict(&self, constraints: TypeConstraints) -> Result<(), TypeError> {
    self.constraints.borrow_mut().restrict(constraints)
  }
  fn restricted(self, constraints: TypeConstraints) -> Result<Self, TypeError> {
    self.restrict(constraints)?;
    Ok(self)
  }
}

impl From<TyntTree> for ExpNode<ConstraintState> {
  fn from(value: TyntTree) -> Self {
    match value {
      TyntTree::Leaf(_, leaf) => {
        if leaf == "true" || leaf == "false" {
          ExpNode {
            exp: Box::new(Exp::BooleanLiteral(leaf == "true")),
            data: TypeConstraints::Definitely(TyntType::Bool).into(),
          }
        } else if let Ok(i) = leaf.parse::<i64>() {
          ExpNode {
            exp: Box::new(Exp::NumberLiteral(Number::Int(i))),
            data: if i < 0 {
              TypeConstraints::OneOf(vec![TyntType::Int, TyntType::Float])
            } else {
              TypeConstraints::OneOf(vec![
                TyntType::Int,
                TyntType::UInt,
                TyntType::Float,
              ])
            }
            .into(),
          }
        } else if let Ok(f) = leaf.parse::<f64>() {
          ExpNode {
            exp: Box::new(Exp::NumberLiteral(Number::Float(f))),
            data: TypeConstraints::Definitely(TyntType::Float).into(),
          }
        } else {
          ExpNode {
            exp: Box::new(Exp::Name(leaf)),
            data: TypeConstraints::Anything.into(),
          }
        }
      }
      TyntTree::Inner((_, encloser_or_operator), children) => todo!(),
    }
  }
}

impl ExpNode<ConstraintState> {
  fn infer_initial_constraints(
    self,
    mut env: Environment,
  ) -> Result<Self, TypeError> {
    self.walk(
      &mut |node| Ok(node),
      &mut |exp: Box<Exp<ConstraintState>>, data| {
        let new_data = match &*exp {
          Name(_) => data,
          NumberLiteral(_) => data.restricted(
            OneOf(vec![TyntType::Int, TyntType::Float, TyntType::UInt]).into(),
          )?,
          BooleanLiteral(_) => {
            data.restricted(Definitely(TyntType::Bool).into())?
          }
          Application(f, _) => data.restricted(match &*f.exp {
            Name(name) => Definitely(
              env.fn_output_type(name).ok_or(TypeError::UnknownFunction)?,
            ),
            _ => todo!("I haven't implemented higher order functions yet!!"),
          })?,
          Let(binding_pairs, body) => {
            for (name, value) in binding_pairs {
              env.bind(name.clone(), value.data.clone());
            }
            if let Some(last_body_exp) = body.last() {
              last_body_exp.data.clone()
            } else {
              data.restricted(Definitely(TyntType::None))?
            }
          }
          Match(exp, cases) => {
            todo!()
            // merge match block type with each of the case bodies, and the
            // match input value type with each case value

            // if the match block is non-exhaustive, it's type should be
            // restricted to bottom. I think at first match blocks only
            // need to be able to match to literals, so the only exhaustive
            // case will be the case of 2 bools
          }
        };
        Ok(ExpNode {
          exp,
          data: new_data,
        })
      },
    )
  }
  fn propagate_constraints(self, env: &Environment) -> Result<Self, TypeError> {
    self.walk(
      &mut |node| {
        let constraints = node.data;
        let new_exp = match *node.exp {
          Application(f, args) => match *f.exp {
            Name(name) => {
              let arg_types = env
                .fn_input_types(&name)
                .ok_or(TypeError::UnknownFunction)?;
              if arg_types.len() != args.len() {
                return Err(TypeError::InvalidArity);
              }
              Application(
                ExpNode {
                  data: f.data,
                  exp: Box::new(Name(name)),
                },
                args
                  .into_iter()
                  .zip(arg_types.into_iter())
                  .map(|(arg, arg_type)| {
                    arg.data.restrict(TypeConstraints::Definitely(arg_type))?;
                    Ok(arg)
                  })
                  .collect::<Result<_, TypeError>>()?,
              )
            }
            _ => todo!("I haven't implemented higher order functions yet!!"),
          },
          other => other,
        };
        Ok(Self {
          exp: Box::new(new_exp),
          data: constraints,
        })
      },
      &mut |exp, data| Ok(Self { exp, data }),
    )
  }
  pub fn to_typed(
    self,
    env: Environment,
  ) -> Result<ExpNode<TyntType>, TypeError> {
    let mut node = self.infer_initial_constraints(env.clone())?;
    loop {
      // todo! eventually this could be optimized to thread a "changed" boolean
      // through the walk in propagate_type_constraints, and end this loop when
      // changed = false, so there wouldn't be a need for cloning or an equality
      // check
      let new_node = node.clone().propagate_constraints(&env)?;
      if new_node == node {
        break;
      }
      node = new_node;
    }
    node.walk(&mut |exp| Ok(exp), &mut |exp, data| {
      Ok(ExpNode {
        exp,
        data: match data.constraints.borrow().clone() {
          TypeConstraints::Definitely(t) => Ok(t),
          _ => Err(TypeError::CouldntInfer),
        }?,
      })
    })
  }
}
