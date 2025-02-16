use std::rc::Rc;

use crate::compiler::{
  expression::ExpressionCompilationPosition, types::VariableKind,
  util::compile_word,
};

use super::{
  error::SourceTrace, expression::TypedExp, metadata::Metadata, types::Variable,
};

#[derive(Debug, Clone)]
pub struct TopLevelVar {
  pub name: Rc<str>,
  pub metadata: Option<Metadata>,
  pub attributes: Vec<Rc<str>>,
  pub var: Variable,
  pub value: Option<TypedExp>,
  pub source_trace: SourceTrace,
}

impl TopLevelVar {
  pub fn compile(self) -> String {
    let metadata = Metadata::compile_optional(self.metadata);
    let attributes = if self.attributes.is_empty() {
      String::new()
    } else {
      format!("<{}>", self.attributes.join(", "))
    };
    let name = compile_word(self.name);
    let typ = self.var.typestate.compile();
    let kind_name = match self.var.kind {
      VariableKind::Let => "const",
      VariableKind::Var => "var",
      VariableKind::Override => "override",
    };
    let assignment = if let Some(value) = self.value {
      format!(
        " = {}",
        value.compile(ExpressionCompilationPosition::InnerExpression)
      )
    } else {
      String::new()
    };
    format!("{metadata}{kind_name}{attributes} {name}: {typ}{assignment}")
  }
}
