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
  Application(ExpNode<D>, Vec<ExpNode<D>>),
  Let(Vec<(String, ExpNode<D>)>, Vec<ExpNode<D>>),
  Match(Box<ExpNode<D>>, Vec<(Vec<ExpNode<D>>, Vec<ExpNode<D>>)>),
}

impl<D: Debug + Clone + PartialEq> ExpNode<D> {
  fn map(self, f: impl Fn(Exp<D>) -> Exp<D>) -> Self {
    Self {
      data: self.data,
      exp: f(*self.exp).into(),
    }
  }
  pub fn try_replace_data<NewD: Debug + Clone + PartialEq, E>(
    self,
    data_deriver: &impl Fn(&Exp<NewD>, D) -> Result<NewD, E>,
  ) -> Result<ExpNode<NewD>, E> {
    let new_exp = match *self.exp {
      Exp::Application(f_expression, args) => Exp::Application(
        f_expression.try_replace_data(data_deriver)?,
        args
          .into_iter()
          .map(|exp| exp.try_replace_data(data_deriver))
          .collect::<Result<_, E>>()?,
      ),
      Exp::Let(bindings, body_expressions) => Exp::Let(
        bindings
          .into_iter()
          .map(|(name, value)| -> Result<_, E> {
            Ok((name, value.try_replace_data(data_deriver)?))
          })
          .collect::<Result<_, E>>()?,
        body_expressions
          .into_iter()
          .map(|exp| exp.try_replace_data(data_deriver))
          .collect::<Result<_, E>>()?,
      ),
      Exp::Match(exp, cases) => Exp::Match(
        Box::new(exp.try_replace_data(data_deriver)?),
        cases
          .into_iter()
          .map(|(potential_matches, body_expressions)| -> Result<_, E> {
            Ok((
              potential_matches
                .into_iter()
                .map(|exp| exp.try_replace_data(data_deriver))
                .collect::<Result<_, E>>()?,
              body_expressions
                .into_iter()
                .map(|exp| exp.try_replace_data(data_deriver))
                .collect::<Result<_, E>>()?,
            ))
          })
          .collect::<Result<_, E>>()?,
      ),
      Exp::Name(name) => Exp::Name(name),
      Exp::NumberLiteral(num) => Exp::NumberLiteral(num),
      Exp::BooleanLiteral(b) => Exp::BooleanLiteral(b),
    };
    let new_data = data_deriver(&new_exp, self.data)?;
    Ok(ExpNode {
      exp: Box::new(new_exp),
      data: new_data,
    })
  }
  pub fn replace_data<NewD: Debug + Clone + PartialEq>(
    self,
    data_deriver: &impl Fn(&Exp<NewD>, D) -> NewD,
  ) -> ExpNode<NewD> {
    self
      .try_replace_data(&|exp, data| Ok::<_, !>(data_deriver(exp, data)))
      .unwrap()
  }
  pub fn prewalk_modify(self, modifier: &impl Fn(Self) -> Self) -> Self {
    modifier(self).map(|exp| match exp {
      Exp::Application(f_expression, args) => Exp::Application(
        f_expression.prewalk_modify(modifier),
        args
          .into_iter()
          .map(|exp| exp.prewalk_modify(modifier))
          .collect(),
      ),
      Exp::Let(bindings, body_expressions) => Exp::Let(
        bindings
          .into_iter()
          .map(|(name, value)| (name, value.prewalk_modify(modifier)))
          .collect(),
        body_expressions
          .into_iter()
          .map(|exp| exp.prewalk_modify(modifier))
          .collect(),
      ),
      Exp::Match(exp, cases) => Exp::Match(
        Box::new(exp.prewalk_modify(modifier)),
        cases
          .into_iter()
          .map(|(potential_matches, body_expressions)| {
            (
              potential_matches
                .into_iter()
                .map(|exp| exp.prewalk_modify(modifier))
                .collect(),
              body_expressions
                .into_iter()
                .map(|exp| exp.prewalk_modify(modifier))
                .collect(),
            )
          })
          .collect(),
      ),
      other => other,
    })
  }
  fn replace_name(self, old_name: String, new_name: String) -> Self {
    self.prewalk_modify(&|expression| {
      expression.map(|exp| match exp {
        Exp::Name(name) => Exp::Name(if name == old_name {
          new_name.clone()
        } else {
          name
        }),
        Exp::Let(bindings, args) => Exp::Let(
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
      })
    })
  }
  fn deshadow_bindings(self) -> Self {
    todo!()
  }
  fn lift_internal_lets(self) -> Self {
    todo!()
  }
}
