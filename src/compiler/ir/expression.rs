use super::metadata::Metadata;
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
  Function(Vec<(Option<Metadata>, String)>, ExpNode<D>),
  Application(ExpNode<D>, Vec<ExpNode<D>>),
  Let(Vec<(String, ExpNode<D>)>, ExpNode<D>),
  Match(Box<ExpNode<D>>, Vec<(ExpNode<D>, ExpNode<D>)>),
}
use Exp::*;

impl<D: Debug + Clone + PartialEq> ExpNode<D> {
  fn map_exp(self, f: impl Fn(Exp<D>) -> Exp<D>) -> Self {
    Self {
      data: self.data,
      exp: f(*self.exp).into(),
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
