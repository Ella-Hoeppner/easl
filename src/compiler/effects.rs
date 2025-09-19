use std::{collections::HashSet, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Effect {
  ReadsVar(Rc<str>),
  ModifiesVar(Rc<str>),
  Break,
  Return,
  Continue,
  Discard,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectType(pub HashSet<Effect>);

impl EffectType {
  pub fn empty() -> Self {
    Self(HashSet::new())
  }
  pub fn is_pure(&self) -> bool {
    self.0.is_empty()
  }
  pub fn merge(&mut self, other: impl Into<Self>) {
    self.0.extend(other.into().0);
  }
  pub fn remove(&mut self, e: &Effect) {
    self.0.remove(e);
  }
  pub fn contains(&self, e: &Effect) -> bool {
    self.0.contains(e)
  }
  pub fn is_pure_but_for_reads(&self) -> bool {
    for e in self.0.iter() {
      match e {
        Effect::ReadsVar(_) => {}
        _ => return false,
      }
    }
    true
  }
}

impl From<HashSet<Effect>> for EffectType {
  fn from(e: HashSet<Effect>) -> Self {
    Self(e)
  }
}

impl From<Effect> for EffectType {
  fn from(e: Effect) -> Self {
    Self([e].into_iter().collect())
  }
}
