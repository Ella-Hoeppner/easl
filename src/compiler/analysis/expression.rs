use core::fmt::Debug;

use super::types::TypePossibilities;

#[derive(Debug, Clone, PartialEq)]
enum Number {
  Int(i64),
  Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<D: Debug + Clone + PartialEq> {
  data: D,
  kind: Box<ExpressionKind<D>>,
}

#[derive(Debug, Clone, PartialEq)]
enum ExpressionKind<D: Debug + Clone + PartialEq> {
  Name(String),
  NumberLiteral(Number),
  BooleanLiteral(bool),
  Application(Expression<D>, Vec<Expression<D>>),
  Let(Vec<(String, Expression<D>)>, Vec<Expression<D>>),
  Match(
    Box<Expression<D>>,
    Vec<(Vec<Expression<D>>, Vec<Expression<D>>)>,
  ),
}

impl<D: Debug + Clone + PartialEq> Expression<D> {
  fn map_kind(
    self,
    f: impl Fn(ExpressionKind<D>) -> ExpressionKind<D>,
  ) -> Self {
    Self {
      data: self.data,
      kind: f(*self.kind).into(),
    }
  }
  fn prewalk_replace(self, replacer: &impl Fn(Self) -> Self) -> Self {
    replacer(self).map_kind(|kind| match kind {
      ExpressionKind::Application(f_expression, args) => {
        ExpressionKind::Application(
          f_expression.prewalk_replace(replacer),
          args
            .into_iter()
            .map(|exp| exp.prewalk_replace(replacer))
            .collect(),
        )
      }
      ExpressionKind::Let(bindings, body_expressions) => ExpressionKind::Let(
        bindings
          .into_iter()
          .map(|(name, value)| (name, value.prewalk_replace(replacer)))
          .collect(),
        body_expressions
          .into_iter()
          .map(|exp| exp.prewalk_replace(replacer))
          .collect(),
      ),
      ExpressionKind::Match(exp, cases) => ExpressionKind::Match(
        Box::new(exp.prewalk_replace(replacer)),
        cases
          .into_iter()
          .map(|(potential_matches, body_expressions)| {
            (
              potential_matches
                .into_iter()
                .map(|exp| exp.prewalk_replace(replacer))
                .collect(),
              body_expressions
                .into_iter()
                .map(|exp| exp.prewalk_replace(replacer))
                .collect(),
            )
          })
          .collect(),
      ),
      other => other,
    })
  }
  fn replace_name(self, old_name: String, new_name: String) -> Self {
    self.prewalk_replace(&|expression| {
      expression.map_kind(|kind| match kind {
        ExpressionKind::Name(name) => {
          ExpressionKind::Name(if name == old_name {
            new_name.clone()
          } else {
            name
          })
        }
        ExpressionKind::Let(bindings, args) => ExpressionKind::Let(
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
  fn initialize_type_possibilities(self) -> Expression<TypePossibilities> {
    todo!()
  }
}
