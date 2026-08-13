use std::fmt::Display;

use std::sync::Arc;

use crate::{
  compiler::{
    annotation::Annotation,
    error::{CompileError, CompileErrorKind::*, ErrorLog},
    expression::ExpressionCompilationPosition,
    program::{CompilerTarget, NameContext, Program},
    types::{ConcreteArraySize, Type, VariableKind},
    util::{compile_word, read_type_annotated_name},
  },
  parse::EaslTree,
};

use super::{error::SourceTrace, expression::TypedExp};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VariableAddressSpace {
  Local,
  Function,
  Workgroup,
  Uniform,
  StorageRead,
  StorageReadWrite,
  Handle,
}
impl Default for VariableAddressSpace {
  fn default() -> Self {
    Self::Local
  }
}
use VariableAddressSpace::*;

impl VariableAddressSpace {
  pub fn may_write_from_gpu(&self) -> bool {
    match self {
      Function | Local | Workgroup | StorageReadWrite => true,
      Uniform | StorageRead | Handle => false,
    }
  }
  pub fn needs_group_and_binding(&self) -> bool {
    match self {
      Local | Workgroup => false,
      _ => true,
    }
  }
  pub fn disallows_initialization(&self) -> bool {
    *self != Local
  }
  pub fn compile(&self) -> Option<&'static str> {
    match self {
      // easl calls this address space `local` (a copy per execution
      // context), but WGSL's name for it is `private`.
      Local => Some("private"),
      StorageReadWrite => Some("storage, read_write"),
      Handle => None,
      other => Some(other.name()),
    }
  }
  pub fn name(&self) -> &'static str {
    match self {
      Function => "function",
      Local => "local",
      Workgroup => "workgroup",
      Uniform => "uniform",
      StorageRead => "storage",
      StorageReadWrite => "storage-write",
      Handle => "handle",
    }
  }
  pub fn from_str(s: &str) -> Option<Self> {
    Some(match s {
      "local" => Local,
      "workgroup" => Workgroup,
      "uniform" => Uniform,
      "storage" | "storage-read" => StorageRead,
      "storage-write" => StorageReadWrite,
      "handle" => Handle,
      _ => return None,
    })
  }
  pub fn may_be_passed_as_reference(&self) -> bool {
    match self {
      Local | Function => true,
      _ => false,
    }
  }
}

impl Display for VariableAddressSpace {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name())
  }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GroupAndBinding {
  pub group: u8,
  pub binding: u8,
}

/// A GPU-bound var's binding numbers: either explicitly written in the
/// source (an interface contract the compiler must preserve — e.g. for
/// coordination with an external host) or elided (`@[uniform]`), in which
/// case the compiler assigns free numbers at the end of validation
/// (`Program::assign_elided_bindings`). No `Elided` survives validation,
/// so post-validation consumers (runtimes, emission, embedders) always
/// see concrete numbers via `specified()`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingSpec {
  Specified(GroupAndBinding),
  Elided,
}

impl BindingSpec {
  pub fn specified(&self) -> GroupAndBinding {
    match self {
      BindingSpec::Specified(group_and_binding) => *group_and_binding,
      BindingSpec::Elided => panic!(
        "binding numbers read before elided bindings were assigned; \
         `assign_elided_bindings` must run first"
      ),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TopLevelVariableKind {
  Const,
  Override,
  Var {
    address_space: VariableAddressSpace,
    group_and_binding: Option<BindingSpec>,
  },
}

#[derive(Debug, Clone)]
pub struct TopLevelVar {
  pub name: Arc<str>,
  pub kind: TopLevelVariableKind,
  pub var_type: Type,
  pub value: Option<TypedExp>,
  pub source_trace: SourceTrace,
  /// Marked `@external`: an embedder may read/write this var through an
  /// `ExternalVars` handle at any time, so it's unconditionally included in
  /// the thread-shared set (the static analysis can't see external access).
  pub external: bool,
}

impl TopLevelVar {
  pub fn from_ast(
    var_kind_name: &str,
    parens_source_trace: &SourceTrace,
    mut internal_forms: impl ExactSizeIterator<Item = EaslTree>,
    program: &Program,
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
              match Type::from_easl_tree(type_ast, &program.typedefs, &vec![]) {
                Err(e) => errors.log(e),
                Ok(t) => {
                  let mut external = false;
                  let (group_and_binding, address_space) =
                    if let Some(annotation) = &annotation {
                      match annotation.validate_as_top_level_var_data() {
                        Err(e) => {
                          errors.log(e);
                          None
                        }
                        Ok((group_and_binding, address_space, is_external)) => {
                          external = is_external;
                          // With an explicit address space, the numbers may
                          // be elided (`@[uniform]`): the compiler assigns
                          // free ones at the end of validation.
                          let binding_spec = group_and_binding
                            .map(BindingSpec::Specified);
                          if let Some(address_space) = address_space {
                            let binding_spec = binding_spec.or_else(|| {
                              address_space
                                .needs_group_and_binding()
                                .then_some(BindingSpec::Elided)
                            });
                            if let Some(required) = t.required_address_space() {
                              if address_space == required {
                                Some((binding_spec, address_space))
                              } else {
                                errors.log(CompileError::new(
                                  InvalidAddressSpace(required),
                                  annotation.source_trace.clone(),
                                ));
                                None
                              }
                            } else if address_space.needs_group_and_binding() {
                              Some((binding_spec, address_space))
                            } else {
                              if binding_spec.is_some() {
                                errors.log(CompileError::new(
                                  DisallowedGroupAndBinding(address_space),
                                  annotation.source_trace.clone(),
                                ));
                                None
                              } else {
                                Some((binding_spec, address_space))
                              }
                            }
                          } else {
                            if let Some(required) = t.required_address_space() {
                              Some((
                                binding_spec.or(Some(BindingSpec::Elided)),
                                required,
                              ))
                            } else if binding_spec.is_none() && is_external {
                              // The annotation was just `@external`; the
                              // address space defaults the same way an
                              // unannotated var's would.
                              Some((
                                Some(BindingSpec::Elided),
                                StorageReadWrite,
                              ))
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
                      // No annotation: top-level vars default to the
                      // GPU-shared `storage-write` address space with
                      // elided binding numbers (types with a required
                      // address space — textures — default to that space
                      // instead). A per-execution-context copy requires an
                      // explicit `@local`.
                      Some((
                        Some(BindingSpec::Elided),
                        t.required_address_space()
                          .unwrap_or(StorageReadWrite),
                      ))
                    }
                    .unwrap_or_else(|| (None, VariableAddressSpace::default()));
                  if address_space == Uniform
                    && matches!(
                      t,
                      Type::Array(Some(ConcreteArraySize::Unsized), _)
                    )
                  {
                    errors.log(CompileError::new(
                      UnsizedArrayInUniform,
                      parens_source_trace.clone(),
                    ));
                  }
                  // Note: bool-/String-containing types in GPU address
                  // spaces are NOT rejected here — such a var is an
                  // ordinary CPU value as long as no GPU entry point
                  // touches it. The usage-based check lives in
                  // `Program::validate_gpu_used_binding_types`.
                  if external && address_space == Local {
                    // `@local` means one independent copy per execution
                    // context, never shared — with anything, including an
                    // embedder. External vars live in the GPU-space
                    // address spaces (the default, storage-write, works).
                    errors.log(CompileError::new(
                      ExternalLocalVar,
                      parens_source_trace.clone(),
                    ));
                  }
                  if external && address_space == Handle {
                    // Textures have no word serialization, so they can't
                    // travel through the shared-snapshot system.
                    errors.log(CompileError::new(
                      ExternalTextureVar,
                      parens_source_trace.clone(),
                    ));
                  }
                  if external && t.involves_string() {
                    // A string value's words are a heap id — a reference
                    // into the owning runtime's heap. Publishing it through
                    // the shared-snapshot system would hand other
                    // participants a number with no meaning in their own
                    // heaps.
                    errors.log(CompileError::new(
                      ExternalStringVar,
                      parens_source_trace.clone(),
                    ));
                  }
                  let value = value_ast
                    .map(|value_ast| {
                      match TypedExp::try_from_easl_tree(
                        value_ast,
                        &program.typedefs,
                        &vec![],
                        crate::compiler::expression::SyntaxTreeContext::Default,
                        &mut program.names.write().unwrap(),
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
                    external,
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
                &mut program.names.write().unwrap(),
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
                        external: false,
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
    match target {
      CompilerTarget::WGSL => {
        let (bind_group_decoration, address_space) =
          if let TopLevelVariableKind::Var {
            group_and_binding,
            address_space,
          } = &self.kind
          {
            (
              if let Some(binding_spec) = group_and_binding {
                let GroupAndBinding { group, binding } =
                  binding_spec.specified();
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
        let var_type = self.var_type.monomorphized_name(names, target);
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
      CompilerTarget::C => {
        let name = compile_word(self.name);
        let var_type = self.var_type.monomorphized_name(names, target);
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
        format!("{var_type} {name}{assignment}")
      }
      CompilerTarget::VM => panic!(),
    }
  }
}
