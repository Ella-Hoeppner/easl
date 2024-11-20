use crate::compiler::{
  expression::ExpressionCompilationPosition, types::VariableKind,
  util::compile_word,
};

use super::{
  error::SourceTrace, expression::TypedExp, metadata::Metadata, types::Variable,
};

#[derive(Debug, Clone)]
pub struct TopLevelVar {
  pub name: String,
  pub metadata: Option<Metadata>,
  pub attributes: Vec<String>,
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
    let var_or_const = match self.var.kind {
      VariableKind::Let => "const",
      VariableKind::Var => "var",
    };
    let assignment = if let Some(value) = self.value {
      format!(
        " = {}",
        value.compile(ExpressionCompilationPosition::InnerExpression)
      )
    } else {
      String::new()
    };
    format!("{metadata}{var_or_const}{attributes} {name}: {typ}{assignment}")
  }
}
