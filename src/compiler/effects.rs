use std::{collections::HashSet, rc::Rc};

use crate::compiler::{
  program::Program,
  vars::{TopLevelVariableKind, VariableAddressSpace},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Effect {
  ReadsVar(Rc<str>),
  ModifiesLocalVar(Rc<str>),
  ModifiesGlobalVar(Rc<str>),
  Break,
  Return,
  Continue,
  Discard,
  FragmentExclusiveFunction(Rc<str>),
  CPUExclusiveFunction(Rc<str>),
  Print,
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
  pub fn cpu_exclusive_functions(&self) -> Vec<Rc<str>> {
    self
      .0
      .iter()
      .filter_map(|e| {
        if let Effect::CPUExclusiveFunction(name) = e {
          Some(name.clone())
        } else {
          None
        }
      })
      .collect()
  }
  pub fn gpu_illegal_address_space_writes(
    &self,
    program: &Program,
  ) -> Vec<(Rc<str>, VariableAddressSpace)> {
    self
      .0
      .iter()
      .filter_map(|e| {
        if let Effect::ModifiesGlobalVar(name) = e
          && let Some(top_level_var) =
            program.top_level_vars.iter().find(|v| v.name == *name)
          && let TopLevelVariableKind::Var { address_space, .. } =
            top_level_var.kind
          && !address_space.may_write_from_gpu()
        {
          Some((name.clone(), address_space))
        } else {
          None
        }
      })
      .collect()
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
