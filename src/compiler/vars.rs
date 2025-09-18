use std::rc::Rc;

use crate::compiler::{
  expression::ExpressionCompilationPosition, program::NameContext,
  types::VariableKind, util::compile_word,
};

use super::{error::SourceTrace, expression::TypedExp, types::Variable};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VariableAddressSpace {
  Private,
  Workgroup,
  Uniform,
  StorageRead,
  StorageReadWrite,
  Handle,
}

impl VariableAddressSpace {
  pub fn can_write(&self) -> bool {
    use VariableAddressSpace::*;
    match self {
      Private | Workgroup | StorageReadWrite => true,
      Uniform | StorageRead | Handle => true,
    }
  }
  pub fn compile(&self) -> Option<&str> {
    use VariableAddressSpace::*;
    Some(match self {
      Private => "private",
      Workgroup => "workgroup",
      Uniform => "uniform",
      StorageRead => "storage",
      StorageReadWrite => "storage, read_write",
      Handle => return None,
    })
  }
  pub fn from_str(s: &str) -> Option<Self> {
    use VariableAddressSpace::*;
    Some(match s {
      "private" => Private,
      "workgroup" => Workgroup,
      "uniform" => Uniform,
      "storage" => StorageRead,
      "storage-write" => StorageReadWrite,
      "handle" => Handle,
      _ => return None,
    })
  }
}

#[derive(Debug, Clone)]
pub struct TopLevelVariableAttributes {
  pub group: Option<u8>,
  pub binding: Option<u8>,
  pub address_space: VariableAddressSpace,
}

impl Default for TopLevelVariableAttributes {
  fn default() -> Self {
    Self {
      group: None,
      binding: None,
      address_space: VariableAddressSpace::Private,
    }
  }
}

#[derive(Debug, Clone)]
pub struct TopLevelVar {
  pub name: Rc<str>,
  pub attributes: TopLevelVariableAttributes,
  pub var: Variable,
  pub value: Option<TypedExp>,
  pub source_trace: SourceTrace,
}

impl TopLevelVar {
  pub fn compile(self, names: &mut NameContext) -> String {
    let bind_group_decoration = String::new()
      + &match self.attributes.group {
        None => String::new(),
        Some(group) => format!("@group({group}) "),
      }
      + &match self.attributes.binding {
        None => String::new(),
        Some(binding) => format!("@binding({binding}) "),
      };
    let address_space =
      if let Some(address_space) = self.attributes.address_space.compile() {
        format!("<{address_space}>")
      } else {
        String::new()
      };
    let name = compile_word(self.name);
    let typ = self.var.typestate.monomorphized_name(names);
    let kind_name = match self.var.kind {
      VariableKind::Let => "const",
      VariableKind::Var => "var",
      VariableKind::Override => "override",
    };
    let assignment = if let Some(value) = self.value {
      format!(
        " = {}",
        value.compile(ExpressionCompilationPosition::InnerExpression, names)
      )
    } else {
      String::new()
    };
    format!(
      "{bind_group_decoration}{kind_name}{address_space} {name}: {typ}{assignment}"
    )
  }
}
