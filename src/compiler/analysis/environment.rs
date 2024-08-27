use std::collections::HashMap;

use super::types::{ConstraintState, TyntType};

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
  inputs: Vec<TyntType>,
  output: TyntType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
  functions: HashMap<String, FunctionSignature>,
  bindings: HashMap<String, ConstraintState>,
}

impl Environment {
  pub fn fn_output_type(&self, name: &str) -> Option<TyntType> {
    self.functions.get(name).map(|f| f.output.clone())
  }
  pub fn fn_input_types(&self, name: &str) -> Option<Vec<TyntType>> {
    self.functions.get(name).map(|f| f.inputs.clone())
  }
  pub fn bind(&mut self, name: String, constraints: ConstraintState) {
    self.bindings.insert(name, constraints);
  }
}
