use super::{
  environment::Environment,
  expression::{Exp::*, ExpNode},
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
}

impl<D: Debug + Clone + PartialEq> ExpNode<D> {}

impl ExpNode<ConstraintState> {
  fn infer_initial_constraints(
    self,
    env: Environment,
  ) -> Result<Self, TypeError> {
    self.walk(&|node| Ok(node), &|exp, data| {
      data.restrict(
        match &*exp {
          NumberLiteral(_) => {
            OneOf(vec![TyntType::Int, TyntType::Float, TyntType::UInt])
          }
          BooleanLiteral(_) => Definitely(TyntType::Bool),
          Application(f, _) => match &*f.exp {
            Name(name) => Definitely(
              env.fn_output_type(name).ok_or(TypeError::UnknownFunction)?,
            ),
            _ => todo!("I haven't implemented higher order functions yet!!"),
          },
          Let(_, _) => todo!(), // attach names and value constraints for each binding, and also attach constraint of let block and the last expression of it's body
          _ => Anything,
        }
        .into(),
      )?;
      Ok(ExpNode { exp, data })
    })
  }
  fn propagate_constraints(self, env: &Environment) -> Result<Self, TypeError> {
    self.walk(
      &|node| {
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
          Let(bindings, body) => todo!(),
          Match(value, cases) => todo!(),
          other => other,
        };
        Ok(Self {
          exp: Box::new(new_exp),
          data: constraints,
        })
      },
      &|exp, data| Ok(Self { exp, data }),
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
    node.walk(&|exp| Ok(exp), &|exp, data| {
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
