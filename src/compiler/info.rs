use crate::compiler::{
  expression::{ExpKind, Number},
  functions::FunctionImplementationKind,
  program::Program,
  types::{ArraySize, Type},
  vars::{GroupAndBinding, TopLevelVariableKind, VariableAddressSpace},
};

pub enum TypeInfo {
  Unit,
  F32,
  I32,
  U32,
  Bool,
  Struct(String),
  Enum(String),
  Array(Option<ArraySize>, Box<Self>),
  InvalidType,
}

impl From<Type> for TypeInfo {
  fn from(t: Type) -> Self {
    use TypeInfo::*;
    match t {
      Type::Unit => Unit,
      Type::F32 => F32,
      Type::I32 => I32,
      Type::U32 => U32,
      Type::Bool => Bool,
      Type::Struct(s) => Struct(s.name.to_string()),
      Type::Enum(e) => Enum(e.name.to_string()),
      Type::Array(array_size, inner_type) => {
        Array(array_size, Box::new(Self::from(inner_type.unwrap_known())))
      }
      _ => InvalidType,
    }
  }
}

pub struct VariableInfo {
  pub name: String,
  pub value: Option<String>,
  pub variable_type: TypeInfo,
  pub uniform_info: Option<GroupAndBinding>,
}

pub struct ProgramInfo {
  pub global_vars: Vec<VariableInfo>,
  pub fragment_entries: Vec<String>,
  pub vertex_entries: Vec<String>,
  pub compute_entries: Vec<String>,
}

impl From<&Program> for ProgramInfo {
  fn from(program: &Program) -> Self {
    let find_entry_points = |entry_kind: &str| -> Vec<String> {
      program
        .abstract_functions_iter()
        .filter_map(|abstract_f| {
          let abstract_f = abstract_f.borrow();
          if let FunctionImplementationKind::Composite(f) =
            &abstract_f.implementation
            && let Some(annotation) = &f.borrow().annotation
          {
            annotation.properties().iter().find_map(|(name, value)| {
              if value.is_none() && &**name == entry_kind {
                Some(abstract_f.name.to_string())
              } else {
                None
              }
            })
          } else {
            None
          }
        })
        .collect()
    };
    Self {
      global_vars: program
        .top_level_vars
        .iter()
        .map(|var| VariableInfo {
          name: var.name.to_string(),
          variable_type: var.var_type.clone().into(),
          uniform_info: if let TopLevelVariableKind::Var {
            address_space: VariableAddressSpace::Uniform,
            group_and_binding,
          } = &var.kind
          {
            group_and_binding.clone()
          } else {
            None
          },
          value: var
            .value
            .clone()
            .map(|exp| match exp.kind {
              ExpKind::Name(name) => Some(name.to_string()),
              ExpKind::NumberLiteral(number) => Some(match number {
                Number::Int(i) => format!("{i}"),
                Number::Float(f) => format!("{f:?}"),
              }),
              ExpKind::BooleanLiteral(b) => Some(format!("{b}")),
              _ => None,
            })
            .flatten(),
        })
        .collect(),
      fragment_entries: find_entry_points("fragment"),
      vertex_entries: find_entry_points("vertex"),
      compute_entries: find_entry_points("compute"),
    }
  }
}
