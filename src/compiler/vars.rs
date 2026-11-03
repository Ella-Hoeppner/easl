use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::Display;

use std::sync::Arc;

use crate::{
  Never,
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

use super::{
  effects::Effect,
  error::SourceTrace,
  expression::{ExpKind, TypedExp},
  functions::{EffectsMemo, FunctionImplementationKind},
  modules::display_name,
  types::TypeState,
};
use crate::midi_out::MidiOutKind;

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
  /// Address spaces with no CPU-side storage at all — passing one as a
  /// reference is rejected in every context (workgroup memory only
  /// exists inside a GPU workgroup; handles are opaque texture/sampler
  /// bindings). Storage and uniform spaces are CPU-referenceable — those
  /// are context-gated instead (`validate_context_exclusivity`: legal in
  /// cpu/audio code, rejected where the call site can run on the GPU,
  /// since base WGSL only permits function/private pointer params).
  pub fn may_never_be_passed_as_reference(&self) -> bool {
    matches!(self, Workgroup | Handle)
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

impl GroupAndBinding {
  /// The first (group, binding) not in `used`, in group-then-binding order.
  pub fn first_free(used: &HashSet<GroupAndBinding>) -> Self {
    (0u8..=u8::MAX)
      .flat_map(|group| {
        (0u8..=u8::MAX).map(move |binding| GroupAndBinding { group, binding })
      })
      .find(|candidate| !used.contains(candidate))
      .expect("all 65536 group/binding slots exhausted")
  }
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
  /// True only for vars declared in the user's source. Compiler-generated
  /// implicit vars (the `easl_midi_*`/window-info query bindings, dispatched-
  /// closure capture globals, audio-info locals) set this false. Used by WGSL
  /// emission: an unused GPU-space *user* var still emits its declaration for
  /// the easl-as-WGSL-library audience, but a compiler-generated one emits
  /// only when a shader actually uses it (nothing else could reference it,
  /// and its type may not even be emitted — e.g. `easl_midi_notes`'s
  /// `Option<MidiNote>`, which is only monomorphized when the enum is
  /// actually constructed/matched).
  pub directly_user_written: bool,
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
                          let binding_spec =
                            group_and_binding.map(BindingSpec::Specified);
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
                        t.required_address_space().unwrap_or(StorageReadWrite),
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
                    directly_user_written: true,
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
                        directly_user_written: true,
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

impl Program {
  /// The top-level vars whose initializers aren't WGSL constant
  /// expressions: they call a function WGSL emits as a function (a user
  /// function, an enum constructor, a generated function like enum `==`,
  /// or a builtin WGSL emulates with a helper), read a `var` or a builtin
  /// that becomes one (`sample-rate`, the MIDI queries), or read another
  /// such value.
  /// The CPU runtimes run them; WGSL can't declare them.
  pub(crate) fn non_constant_initializers(&self) -> HashSet<Arc<str>> {
    let initializers: HashMap<&Arc<str>, &TypedExp> = self
      .top_level_vars
      .iter()
      .filter_map(|v| Some((&v.name, v.value.as_ref()?)))
      .collect();
    // WGSL constant expressions can't read variables.
    let vars: HashSet<&Arc<str>> = self
      .top_level_vars
      .iter()
      .filter(|v| matches!(v.kind, TopLevelVariableKind::Var { .. }))
      .map(|v| &v.name)
      .collect();
    let mut non_constant: HashSet<Arc<str>> = initializers
      .iter()
      .filter(|(_, value)| {
        value.any_subexpression(|exp| {
          exp.calls_a_function()
            || matches!(&exp.kind, ExpKind::Name(name) if vars.contains(name))
            || exp.reads_input().is_some()
        })
      })
      .map(|(name, _)| (*name).clone())
      .collect();
    loop {
      let reads_non_constant: Vec<Arc<str>> = initializers
        .iter()
        .filter(|(name, value)| {
          !non_constant.contains(**name)
            && value.any_subexpression(|exp| {
              matches!(&exp.kind, ExpKind::Name(read)
                if non_constant.contains(read))
            })
        })
        .map(|(name, _)| (*name).clone())
        .collect();
      if reads_non_constant.is_empty() {
        return non_constant;
      }
      non_constant.extend(reads_non_constant);
    }
  }
}

/// The builtins that read input the program receives while it runs, with
/// what they read, and `sample-rate`, which reads the audio device's rate.
/// `extract_audio_info` turns each into a read of a variable.
const INPUT_BUILTINS: [(&str, &str); 8] = [
  ("audio-time", "the audio clock"),
  ("audio-input", "audio input"),
  ("midi-cc", "MIDI input"),
  ("midi-aftertouch", "MIDI input"),
  ("midi-pitch-bend", "MIDI input"),
  ("down-midi-notes", "MIDI input"),
  ("get-midi-note", "MIDI input"),
  ("sample-rate", "the sample rate"),
];

impl TypedExp {
  /// What this reads, when it's a call of one of the `INPUT_BUILTINS`.
  fn reads_input(&self) -> Option<&'static str> {
    let ExpKind::Application(callee, _) = &self.kind else {
      return None;
    };
    let ExpKind::Name(name) = &callee.kind else {
      return None;
    };
    INPUT_BUILTINS
      .iter()
      .find(|(builtin, _)| *builtin == &**name)
      .map(|(_, read)| *read)
  }

  /// What input this reads, directly or through the functions it calls,
  /// besides the sample rate (fixed for the whole run).
  fn input_reads(
    &self,
    visited: &mut HashSet<usize>,
  ) -> BTreeSet<&'static str> {
    let mut reads = BTreeSet::new();
    self
      .walk(&mut |exp| {
        if let Some(read) = exp.reads_input()
          && read != "the sample rate"
        {
          reads.insert(read);
        }
        if let ExpKind::Application(callee, _) = &exp.kind
          && let TypeState::Known(Type::Function(signature)) = &callee.data.kind
          && let Some(ancestor) = &signature.abstract_ancestor
          && let FunctionImplementationKind::Composite(implementation) =
            &ancestor.read().unwrap().implementation
          && visited.insert(Arc::as_ptr(implementation) as usize)
        {
          reads.extend(
            implementation
              .read()
              .unwrap()
              .expression
              .input_reads(visited),
          );
        }
        Ok::<bool, Never>(true)
      })
      .unwrap();
    reads
  }

  /// Whether this is an application WGSL emits as a function call, which
  /// a constant expression can't contain.
  fn calls_a_function(&self) -> bool {
    let ExpKind::Application(callee, _) = &self.kind else {
      return false;
    };
    let TypeState::Known(Type::Function(signature)) = &callee.data.kind else {
      return false;
    };
    let Some(ancestor) = &signature.abstract_ancestor else {
      return false;
    };
    match &ancestor.read().unwrap().implementation {
      FunctionImplementationKind::Composite(_)
      | FunctionImplementationKind::EnumConstructor(_) => true,
      FunctionImplementationKind::Builtin {
        target_specific_emulations,
        ..
      } => target_specific_emulations.contains(&CompilerTarget::WGSL),
      FunctionImplementationKind::StructConstructor => false,
    }
  }
}

/// The CPU-only builtins a top-level initializer may call: they only build
/// values.
const PURE_CPU_BUILTINS: [&str; 9] = [
  "resolve-path",
  "into-dynamic-array",
  "zeroed-array",
  "push",
  "insert",
  "remove",
  "concat",
  "reverse",
  "subarray",
];

/// The builtins that read files.
const FILE_READS: [&str; 7] = [
  "load-text",
  "load-image",
  "load-wav",
  "load-wav-raw",
  "get-wav-sample-rate",
  "load-video",
  "file-exists?",
];

/// The builtins that write files.
const FILE_WRITES: [&str; 3] = ["save-text", "save-png", "save-wav"];

/// What an initializer with `effect` does that a pure one can't, if
/// anything.
fn impure_description(effect: &Effect) -> Option<String> {
  match effect {
    Effect::ReadsVar(_)
    | Effect::ReadsArrayLength(_)
    | Effect::ModifiesLocalVar(_)
    | Effect::Break
    | Effect::Return
    | Effect::Continue
    | Effect::CPUExclusiveType(_)
    // described by the more specific effects that come with them
    | Effect::Window
    | Effect::FileWrite => None,
    Effect::CPUExclusiveFunction(name)
      if PURE_CPU_BUILTINS.contains(&&**name)
        || MidiOutKind::BUILTIN_NAMES.contains(&&**name) =>
    {
      None
    }
    Effect::CPUExclusiveFunction(name) if FILE_READS.contains(&&**name) => {
      Some(format!("reads a file (`{name}`)"))
    }
    Effect::CPUExclusiveFunction(name) if FILE_WRITES.contains(&&**name) => {
      Some(format!("writes a file (`{name}`)"))
    }
    Effect::CPUExclusiveFunction(name) => Some(format!("calls `{name}`")),
    Effect::ModifiesGlobalVar(var) | Effect::SeedsGlobalVar(var) => {
      Some(format!("writes `{}`", display_name(var)))
    }
    Effect::Print => Some("prints".to_string()),
    Effect::MidiOut => Some("sends MIDI".to_string()),
    Effect::WindowInfo(_) => Some("reads window or input state".to_string()),
    Effect::Discard
    | Effect::FragmentExclusiveFunction(_)
    | Effect::LookupBuiltinAttribute(_) => {
      Some("uses a shader-only builtin".to_string())
    }
    Effect::InvokesUnknownFunction => {
      Some("calls a function value".to_string())
    }
  }
}

impl Program {
  /// Rejects top-level initializers with effects beyond reading globals:
  /// they run once at startup, ordered only by what they read, so an
  /// effect (a print, a write to a global, a file read) would happen at a
  /// time, and in an order, the program doesn't say.
  pub fn validate_pure_initializers(&self, errors: &mut ErrorLog) {
    let mut memo = EffectsMemo::default();
    for v in self.top_level_vars.iter() {
      let Some(value) = &v.value else {
        continue;
      };
      let effects = value.effects_with(&mut memo);
      let mut descriptions: BTreeSet<String> = effects
        .0
        .iter()
        .filter_map(impure_description)
        .chain(
          value
            .input_reads(&mut HashSet::new())
            .into_iter()
            .map(|read| format!("reads {read}")),
        )
        .collect();
      if descriptions.is_empty() && effects.0.contains(&Effect::Window) {
        descriptions.insert("uses the window".to_string());
      }
      if !descriptions.is_empty() {
        errors.log(CompileError::new(
          ImpureInitializer(
            display_name(&v.name).to_string(),
            descriptions.into_iter().collect::<Vec<_>>().join(" and "),
          ),
          value.source_trace.clone(),
        ));
      }
    }
  }
}
