use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Effect {
  Modifies(Rc<str>),
}
