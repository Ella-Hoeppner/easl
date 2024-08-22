use super::{
  environment::Environment,
  expression::{Exp::*, ExpNode},
};
use core::fmt::Debug;
use std::{cell::RefCell, rc::Rc};

enum TypeInferenceError {
  CouldntInfer,
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

#[derive(Debug, Clone)]
struct InferenceState {
  possibilities: Rc<RefCell<TypeConstraints>>,
}
impl PartialEq for InferenceState {
  fn eq(&self, other: &Self) -> bool {
    Rc::ptr_eq(&self.possibilities, &other.possibilities)
  }
}
impl From<TypeConstraints> for InferenceState {
  fn from(possibilities: TypeConstraints) -> Self {
    Self {
      possibilities: Rc::new(RefCell::new(possibilities)),
    }
  }
}

impl<D: Debug + Clone + PartialEq> ExpNode<D> {
  fn initialize_type_inference(
    self,
    env: Environment,
  ) -> ExpNode<InferenceState> {
    self.replace_data(&|exp, _| {
      match exp {
        NumberLiteral(_) => {
          OneOf(vec![TyntType::Int, TyntType::Float, TyntType::UInt])
        }
        BooleanLiteral(_) => Definitely(TyntType::Bool),
        Application(f, _) => match &*f.exp {
          Name(name) => Definitely(Environment::get_fn_output_type(name)),
          _ => todo!("I haven't implemented higher order functions yet!!"),
        },
        _ => Anything,
      }
      .into()
    })
  }
}

impl ExpNode<InferenceState> {
  fn infer_types(self, env: Environment) -> Self {
    todo!()
  }
  fn to_typed(self) -> Result<ExpNode<TyntType>, TypeInferenceError> {
    self.try_replace_data(
      &|_, data| match data.possibilities.borrow().clone() {
        TypeConstraints::Definitely(t) => Ok(t),
        _ => Err(TypeInferenceError::CouldntInfer),
      },
    )
  }
}
