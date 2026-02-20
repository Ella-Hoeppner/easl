use std::{fmt::Display, rc::Rc};

use crate::{
  compiler::{
    annotation::Annotation,
    error::{CompileError, CompileErrorKind::*, ErrorLog},
    expression::ExpressionCompilationPosition,
    program::{CompilerTarget, EaslDocument, NameContext, Program},
    types::{Type, VariableKind},
    util::{compile_word, read_type_annotated_name},
  },
  parse::EaslTree,
};

use super::{error::SourceTrace, expression::TypedExp};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VariableAddressSpace {
  Private,
  Function,
  Workgroup,
  Uniform,
  StorageRead,
  StorageReadWrite,
  Handle,
}
impl Default for VariableAddressSpace {
  fn default() -> Self {
    Self::Private
  }
}
use VariableAddressSpace::*;

impl VariableAddressSpace {
  pub fn may_write_from_gpu(&self) -> bool {
    match self {
      Function | Private | Workgroup | StorageReadWrite => true,
      Uniform | StorageRead | Handle => false,
    }
  }
  pub fn needs_group_and_binding(&self) -> bool {
    match self {
      Private | Workgroup => false,
      _ => true,
    }
  }
  pub fn disallows_initialization(&self) -> bool {
    *self != Private
  }
  pub fn compile(&self) -> Option<&'static str> {
    match self {
      StorageReadWrite => Some("storage, read_write"),
      Handle => None,
      other => Some(other.name()),
    }
  }
  pub fn name(&self) -> &'static str {
    match self {
      Function => "function",
      Private => "private",
      Workgroup => "workgroup",
      Uniform => "uniform",
      StorageRead => "storage",
      StorageReadWrite => "storage-write",
      Handle => "handle",
    }
  }
  pub fn from_str(s: &str) -> Option<Self> {
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
  pub fn may_be_passed_as_reference(&self) -> bool {
    match self {
      Private | Function => true,
      _ => false,
    }
  }
}

impl Display for VariableAddressSpace {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name())
  }
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GroupAndBinding {
  pub group: u8,
  pub binding: u8,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TopLevelVariableKind {
  Const,
  Override,
  Var {
    address_space: VariableAddressSpace,
    group_and_binding: Option<GroupAndBinding>,
  },
}

#[derive(Debug, Clone)]
pub struct TopLevelVar {
  pub name: Rc<str>,
  pub kind: TopLevelVariableKind,
  pub var_type: Type,
  pub value: Option<TypedExp>,
  pub source_trace: SourceTrace,
}

impl TopLevelVar {
  pub fn from_ast(
    var_kind_name: &str,
    parens_source_trace: &SourceTrace,
    mut internal_forms: impl ExactSizeIterator<Item = EaslTree>,
    program: &Program,
    document: &EaslDocument,
    annotation: Option<Annotation>,
    errors: &mut ErrorLog,
  ) -> Option<Self> {
    match var_kind_name {
      "var" => {
        if let Some((name_and_type_ast, value_ast)) = match internal_forms.len()
        {
          1 => Some((internal_forms.next().unwrap(), None)),
          2 => Some((internal_forms.next().unwrap(), internal_forms.next())),
          _ => {
            errors.log(CompileError::new(
              InvalidTopLevelVar("Invalid number of inner forms".into()),
              parens_source_trace.clone(),
            ));
            None
          }
        } {
          match read_type_annotated_name(name_and_type_ast) {
            Ok((name, type_ast)) => {
              let type_ast_span = type_ast.position().span.clone();
              match Type::from_easl_tree(type_ast, &program.typedefs, &vec![]) {
                Err(e) => errors.log(e),
                Ok(t) => {
                  let (group_and_binding, address_space) =
                    if let Some(annotation) = &annotation {
                      match annotation.validate_as_top_level_var_data() {
                        Err(e) => {
                          errors.log(e);
                          None
                        }
                        Ok((group_and_binding, address_space)) => {
                          if let Some(address_space) = address_space {
                            if let Some(required) = t.required_address_space() {
                              if address_space == required {
                                if address_space.needs_group_and_binding()
                                  && group_and_binding == None
                                {
                                  errors.log(CompileError::new(
                                    NeedsGroupAndBinding(
                                      document.text[type_ast_span].to_string(),
                                    ),
                                    parens_source_trace.clone(),
                                  ));
                                  None
                                } else {
                                  Some((group_and_binding, address_space))
                                }
                              } else {
                                errors.log(CompileError::new(
                                  InvalidAddressSpace(required),
                                  annotation.source_trace.clone(),
                                ));
                                None
                              }
                            } else if address_space.needs_group_and_binding() {
                              if group_and_binding.is_none() {
                                errors.log(CompileError::new(
                                  NeedGroupAndBinding(address_space),
                                  annotation.source_trace.clone(),
                                ));
                                None
                              } else {
                                Some((group_and_binding, address_space))
                              }
                            } else {
                              if group_and_binding.is_some() {
                                errors.log(CompileError::new(
                                  DisallowedGroupAndBinding(address_space),
                                  annotation.source_trace.clone(),
                                ));
                                None
                              } else {
                                Some((group_and_binding, address_space))
                              }
                            }
                          } else {
                            if let Some(required) = t.required_address_space() {
                              Some((group_and_binding, required))
                            } else {
                              errors.log(CompileError::new(
                                NeedAddressAnnotation,
                                annotation.source_trace.clone(),
                              ));
                              None
                            }
                          }
                        }
                      }
                    } else {
                      if let Some(required_address_space) =
                        t.required_address_space()
                        && required_address_space.needs_group_and_binding()
                      {
                        errors.log(CompileError::new(
                          NeedsGroupAndBinding(
                            document.text[type_ast_span].to_string(),
                          ),
                          parens_source_trace.clone(),
                        ));
                      }
                      None
                    }
                    .unwrap_or_else(|| (None, VariableAddressSpace::default()));
                  let value = value_ast
                    .map(|value_ast| {
                      match TypedExp::try_from_easl_tree(
                        value_ast,
                        &program.typedefs,
                        &vec![],
                        crate::compiler::expression::SyntaxTreeContext::Default,
                      ) {
                        Ok(exp) => Some(exp),
                        Err(e) => {
                          errors.log(e);
                          None
                        }
                      }
                    })
                    .flatten();
                  if address_space.disallows_initialization() && value.is_some()
                  {
                    if value.is_some() {
                      errors.log(CompileError::new(
                        DisallowedInitializationValue(address_space),
                        parens_source_trace.clone(),
                      ));
                    }
                  }
                  return Some(Self {
                    name,
                    var_type: t,
                    value,
                    source_trace: parens_source_trace.clone(),
                    kind: TopLevelVariableKind::Var {
                      address_space,
                      group_and_binding,
                    },
                  });
                }
              }
            }
            Err(e) => errors.log(e),
          }
        }
      }
      "def" | "override" => {
        if internal_forms.len() == 2 {
          match read_type_annotated_name(internal_forms.next().unwrap()) {
            Ok((name, type_ast)) => {
              match TypedExp::try_from_easl_tree(
                internal_forms.next().unwrap(),
                &program.typedefs,
                &vec![],
                crate::compiler::expression::SyntaxTreeContext::Default,
              ) {
                Ok(value_expression) => {
                  match Type::from_easl_tree(
                    type_ast,
                    &program.typedefs,
                    &vec![],
                  ) {
                    Ok(t) => {
                      if annotation.is_some() {
                        errors.log(CompileError::new(
                          ConstantMayNotHaveAnnotation,
                          parens_source_trace.clone(),
                        ));
                      }
                      return Some(Self {
                        name,
                        var_type: t,
                        value: Some(value_expression),
                        source_trace: parens_source_trace.clone(),
                        kind: if var_kind_name == "override" {
                          TopLevelVariableKind::Override
                        } else {
                          TopLevelVariableKind::Const
                        },
                      });
                    }
                    Err(e) => errors.log(e),
                  }
                }
                Err(e) => errors.log(e),
              }
            }
            Err(e) => errors.log(e),
          }
        } else {
          errors.log(CompileError::new(
            InvalidTopLevelVar("Expected two forms inside \"def\"".into()),
            parens_source_trace.clone(),
          ));
        }
      }
      _ => {}
    }
    None
  }
  pub fn variable_kind(&self) -> VariableKind {
    match self.kind {
      TopLevelVariableKind::Const => VariableKind::Let,
      TopLevelVariableKind::Override => VariableKind::Override,
      TopLevelVariableKind::Var { .. } => VariableKind::Var,
    }
  }
  pub fn compile(
    self,
    names: &mut NameContext,
    target: CompilerTarget,
  ) -> String {
    let (bind_group_decoration, address_space) =
      if let TopLevelVariableKind::Var {
        group_and_binding,
        address_space,
      } = &self.kind
      {
        (
          if let Some(GroupAndBinding { group, binding }) = group_and_binding {
            format!("@group({group}) @binding({binding}) ")
          } else {
            String::new()
          },
          address_space
            .compile()
            .map(|s| format!("<{s}>"))
            .unwrap_or_default(),
        )
      } else {
        (String::new(), String::new())
      };

    let kind_name = match self.kind {
      TopLevelVariableKind::Var { .. } => "var",
      TopLevelVariableKind::Override => "override",
      TopLevelVariableKind::Const => "const",
    };

    let name = compile_word(self.name);
    let var_type = self.var_type.monomorphized_name(names);
    let assignment = if let Some(value) = self.value {
      format!(
        " = {}",
        value.compile(
          ExpressionCompilationPosition::InnerExpression,
          names,
          target
        )
      )
    } else {
      String::new()
    };
    format!(
      "{bind_group_decoration}{kind_name}{address_space} {name}: {var_type}{assignment}"
    )
  }
}
