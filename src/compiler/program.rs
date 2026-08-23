use std::collections::{HashMap, HashSet};

use std::hash::Hash;
use std::sync::{Arc, RwLock};

use fsexp::{Ast, document::Document, syntax::EncloserOrOperator};
use take_mut::take;

use crate::compiler::builtins::{
  EmulatedFunctionRecord, EmulatedFunctionSignature,
  built_in_structs_for_target,
};
use crate::compiler::types::ExpTypeInfo;
use crate::compiler::vars::{
  BindingSpec, GroupAndBinding, VariableAddressSpace,
};
use crate::parse::EaslMultiDocument;
use crate::thread_sync::participant;
use crate::vm::bytecode::{BytecodeProgram, Instruction, Op};
use crate::vm::compile::{
  BytecodeCompilationState, PendingFrameFnUsage, PendingRefFnUsage,
  RefArgBinding, vm_type_size,
};
use crate::{
  Never,
  compiler::{
    annotation::extract_annotation,
    builtins::built_in_functions,
    effects::{Effect, WindowInfoBindingSource, WindowInfoKind},
    entry::{
      BuiltinIOAttribute, EntryPoint, IOAttribute, IOAttributeKind,
      IOAttributes, InputOrOutput,
    },
    enums::{AbstractEnum, UntypedEnum},
    error::{CompileError, SourceTrace, err},
    expression::{
      Accessor, Exp, ExpKind, ExpressionCompilationPosition, Number,
    },
    functions::{
      AbstractFunctionSignature, FunctionArgumentAnnotation, FunctionSignature,
      FunctionTargetConfiguration, Ownership, TopLevelFunction,
    },
    structs::{AbstractStructField, UntypedStruct},
    types::{
      AbstractArraySize, AbstractType, ConcreteArraySize,
      ConstGenericResolution, ImmutableProgramLocalContext,
      NameDefinitionSource, Type, TypeState, UntypedType, Variable,
      VariableKind, parse_generic_argument,
    },
    util::{compile_word, is_valid_name},
    vars::TopLevelVariableKind,
    wgsl::is_easl_reserved_word,
  },
  parse::{EaslSyntax, EaslTree, Encloser, Operator, parse_easl},
};

use super::{
  builtins::{
    ABNORMAL_CONSTRUCTOR_STRUCTS, built_in_structs, built_in_type_aliases,
  },
  error::{
    CompileErrorKind::{self, *},
    CompileResult, ErrorLog,
  },
  expression::TypedExp,
  functions::FunctionImplementationKind,
  macros::{Macro, macroexpand},
  structs::AbstractStruct,
  vars::TopLevelVar,
};

pub type EaslDocument = Document<EaslSyntax>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompilerTarget {
  C,
  WGSL,
  VM,
}

impl CompilerTarget {
  fn program_header(self) -> String {
    match self {
      CompilerTarget::C => {
        let mut header =
          "#include <stdlib.h>\n\
           #include <stdio.h>\n\
           #include <stdbool.h>\n\
           #include <stdint.h>\n\
           #include <string.h>\n\
           #include <math.h>\n"
            .to_string();
        header += r#"void print_f32(float v) {                                                                     
  char buf[32];                                                                           
  snprintf(buf, sizeof(buf), "%f", v);                                                      
  char *dot = strchr(buf, '.');                                                             
  if (dot) {                                                                                
    char *end = buf + strlen(buf) - 1;                                                    
    while (end > dot && *end == '0') {                      
      *end-- = '\0';                                                                    
    }                                                                                   
  }
  printf("%s", buf);
}
"#;
        for size in [2, 3, 4] {
          for (field_type, suffix) in
            [("float", "f"), ("int32_t", "i"), ("uint32_t", "u")]
          {
            header += "typedef struct {\n";
            for field in ["x", "y", "z", "w"].iter().take(size) {
              header += &format!("  {field_type} {field};\n")
            }
            header += &format!("}} vec{size}{suffix};\n");
          }
        }
        // Matrix types: matNxM has N columns, each a vecM
        for n in 2..=4 {
          for m in 2..=4 {
            for suffix in ["f", "i", "u"] {
              header += "typedef struct {\n";
              for i in 0..n {
                header += &format!("  vec{m}{suffix} c{i};\n");
              }
              header += &format!("}} mat{n}x{m}{suffix};\n");
            }
          }
        }
        for size in [2usize, 3, 4] {
          for (field_type, suffix) in
            [("float", "f"), ("int32_t", "i"), ("uint32_t", "u")]
          {
            header += &format!(
              "static inline {field_type} index_vec{size}{suffix}(vec{size}{suffix} v, int32_t i) {{ \
                 return (&v.x)[i]; \
               }}\n"
            );
          }
        }
        for n in 2..=4 {
          for m in 2..=4 {
            for suffix in ["f", "i", "u"] {
              header += &format!(
                "static inline vec{m}{suffix} index_mat{n}x{m}{suffix}(mat{n}x{m}{suffix} m, int32_t i) {{ \
                   return (&m.c0)[i]; \
                 }}\n"
              );
            }
          }
        }
        header
      }
      .to_string(),
      _ => String::new(),
    }
  }
}

pub trait EaslDocumentMethods {
  fn override_def(&mut self, def_name: &str, new_def_value: &str) -> bool;
}
impl EaslDocumentMethods for EaslDocument {
  fn override_def(&mut self, def_name: &str, new_def_value: &str) -> bool {
    let new_def_document = parse_easl(new_def_value);
    if let Some(new_value_ast) = new_def_document.syntax_trees.first() {
      for ast in self.syntax_trees.iter_mut() {
        if let Ast::Inner(
          (_, EncloserOrOperator::Encloser(Encloser::Parens)),
          children,
        ) = ast
          && let Some(Ast::Leaf(_, first_leaf)) = children.first()
          && first_leaf == "def"
          && let Some(Ast::Inner(
            (_, EncloserOrOperator::Operator(Operator::TypeAscription)),
            name_children,
          )) = children.get(2)
          && let Some(Ast::Leaf(_, binding_name)) = name_children.first()
          && binding_name == def_name
          && let Some(value) = children.get_mut(2)
        {
          *value = new_value_ast.clone();
          return true;
        }
      }
    }
    false
  }
}

thread_local! {
  pub static DEFAULT_PROGRAM: RwLock<Program> =
    RwLock::new(
      Program::empty()
        .with_functions(built_in_functions())
        .with_structs(
          built_in_structs().into_iter().map(|s| Arc::new(s)).collect(),
        )
        .with_type_aliases(built_in_type_aliases()));
}

#[derive(Debug, Clone)]
pub struct NameContext {
  user_names: HashSet<Arc<str>>,
  generated_names: HashSet<Arc<str>>,
  monomorphized_names: HashMap<(Arc<str>, Vec<Arc<str>>), Arc<str>>,
}

impl NameContext {
  fn empty() -> Self {
    Self {
      user_names: HashSet::new(),
      generated_names: HashSet::new(),
      monomorphized_names: HashMap::new(),
    }
  }
  fn track_all_ast_names(&mut self, ast: &EaslTree) {
    ast.walk(&mut |ast| {
      if let EaslTree::Leaf(_, name) = ast {
        self.track_user_name(name);
      }
    });
  }
  fn track_user_name(&mut self, name: &str) {
    self.user_names.insert(name.into());
    self.user_names.insert(compile_word(name.into()).into());
  }
  fn is_taken(&self, name: &str) -> bool {
    self.user_names.contains(name) || self.generated_names.contains(name)
  }
  pub fn gensym(&mut self, base_name: &str) -> Arc<str> {
    if self.is_taken(base_name) {
      let mut i = 0;
      let final_name: Arc<str> = loop {
        let modified_name = base_name.to_string() + &format!("_{i}");
        if !self.is_taken(&modified_name) {
          break modified_name.into();
        }
        i += 1;
      };
      self.generated_names.insert(final_name.clone());
      final_name
    } else {
      self.generated_names.insert(base_name.into());
      base_name.into()
    }
  }
  /// Reverse of `get_monomorphized_name`: maps each generated monomorphized
  /// name back to the base name it was derived from. Names are gensym'd on
  /// collision at generation time, so this cache-derived mapping is the only
  /// exact way to recover a base name — recomputing the mangle can diverge.
  pub(crate) fn monomorphized_to_base_names(
    &self,
  ) -> HashMap<Arc<str>, Arc<str>> {
    self
      .monomorphized_names
      .iter()
      .map(|((base, _), monomorphized)| (monomorphized.clone(), base.clone()))
      .collect()
  }
  pub(crate) fn get_monomorphized_name(
    &mut self,
    base_type_name: Arc<str>,
    generic_arg_names: Vec<Arc<str>>,
  ) -> Arc<str> {
    if generic_arg_names.is_empty() {
      return base_type_name;
    }
    let monomorphization_id = (base_type_name, generic_arg_names);
    self
      .monomorphized_names
      .get(&monomorphization_id)
      .map(|name| name.clone())
      .unwrap_or_else(|| {
        let full_name: Arc<str> = monomorphization_id
          .1
          .clone()
          .into_iter()
          .fold(
            monomorphization_id.0.to_string(),
            |full_name, generic_arg_name| full_name + "_" + &generic_arg_name,
          )
          .into();
        let final_name = self.gensym(&full_name);
        self.generated_names.insert(final_name.clone());
        self
          .monomorphized_names
          .insert(monomorphization_id, final_name.clone());
        final_name
      })
  }
}

#[derive(Debug, Clone)]
pub struct TypeDefs {
  pub structs: Vec<AbstractStruct>,
  pub enums: Vec<AbstractEnum>,
  pub type_aliases: Vec<(Arc<str>, Arc<AbstractStruct>)>,
}

impl TypeDefs {
  pub fn empty() -> Self {
    Self {
      structs: vec![],
      enums: vec![],
      type_aliases: vec![],
    }
  }
  pub fn get_attributable_components(
    &self,
    t: Type,
    input_or_output: InputOrOutput,
    source_trace: SourceTrace,
    errors: &mut ErrorLog,
  ) -> Vec<(Arc<AbstractStruct>, Arc<str>, IOAttributes)> {
    match t {
      Type::Struct(s) => s
        .fields
        .iter()
        .filter_map(|f| {
          if f.field_type.unwrap_known().is_attributable() {
            Some((
              s.abstract_ancestor.clone(),
              f.name.clone(),
              f.attributes.clone(),
            ))
          } else {
            errors.log(CompileError::new(
              CantAssignAttributesToFieldOfType(f.name.to_string()),
              source_trace
                .clone()
                .insert_as_secondary(s.abstract_ancestor.source_trace.clone()),
            ));
            None
          }
        })
        .collect(),
      Type::Unit => vec![],
      _ => {
        errors.log(CompileError::new(
          EntryInputOrOutputMustBeScalarOrStruct(input_or_output),
          source_trace,
        ));
        vec![]
      }
    }
  }
}

/// How one field of a dispatched closure's captured scope is rewritten by
/// `extract_dispatched_closure_scopes`: data captures become reads of
/// their own lifted binding global, and captured closures are called
/// through a GPU clone whose own captures are lifted recursively.
enum CaptureRewrite {
  Global(Arc<str>),
  CalleeClone {
    clone_name: Arc<str>,
    clone_signature: Arc<RwLock<AbstractFunctionSignature>>,
  },
}

/// The transitive set of lifted-capture global names for a closure chain
/// rooted at `scope_struct` (`<scope>_audio_data_<capture>` for every data
/// field, recursing through captured closures' own scopes) — the same
/// traversal the runtime seed writers walk, so the set matches exactly
/// what `start-audio` seeds at handoff.
fn collect_audio_scope_global_names(
  scope_struct: &AbstractStruct,
  out: &mut Vec<Arc<str>>,
) {
  for field in scope_struct.fields.iter() {
    let AbstractType::Type(field_type) = &field.field_type else {
      panic!("captured scope field with non-concrete type")
    };
    if let Type::Function(signature) = field_type {
      if let Some(field_ancestor) = &signature.abstract_ancestor
        && let Some(nested_struct) =
          field_ancestor.read().unwrap().captured_scope.clone()
      {
        collect_audio_scope_global_names(&nested_struct, out);
      }
    } else {
      out.push(
        format!("{}_audio_data_{}", scope_struct.name.0, field.name).into(),
      );
    }
  }
}

/// Execution-context bit indices shared by the context-exclusivity
/// validation (`validate_context_exclusivity`) and target emission
/// (`compile_to_target`), both driven by
/// `Program::compute_function_contexts`.
pub(crate) mod execution_context {
  use crate::compiler::entry::EntryPoint;

  pub const CONTEXT_COUNT: usize = 5;
  pub const CPU: usize = 0;
  pub const VERTEX: usize = 1;
  pub const FRAGMENT: usize = 2;
  pub const COMPUTE: usize = 3;
  pub const AUDIO: usize = 4;
  pub const CONTEXT_NAMES: [&str; CONTEXT_COUNT] =
    ["cpu", "vertex", "fragment", "compute", "audio"];
  /// representative EntryPoint per context, for
  /// `BuiltinIOAttribute::is_valid_input_for_stage`
  pub const CONTEXT_ENTRIES: [EntryPoint; CONTEXT_COUNT] = [
    EntryPoint::Cpu,
    EntryPoint::Vertex,
    EntryPoint::Fragment,
    EntryPoint::Compute(1),
    EntryPoint::Audio,
  ];
  pub fn bit(context: usize) -> u8 {
    1 << context
  }
  pub fn context_of_entry(entry: &EntryPoint) -> usize {
    match entry {
      EntryPoint::Cpu => CPU,
      EntryPoint::Vertex => VERTEX,
      EntryPoint::Fragment => FRAGMENT,
      EntryPoint::Compute(_) => COMPUTE,
      EntryPoint::Audio => AUDIO,
    }
  }
  /// The contexts whose code a compilation target actually compiles:
  /// a generated (non-user-written) function reachable only from
  /// contexts outside this set is skipped from the target's output.
  pub fn target_context_mask(
    target: crate::compiler::program::CompilerTarget,
  ) -> u8 {
    match target {
      crate::compiler::program::CompilerTarget::WGSL => {
        bit(VERTEX) | bit(FRAGMENT) | bit(COMPUTE)
      }
      crate::compiler::program::CompilerTarget::C => {
        bit(VERTEX) | bit(FRAGMENT) | bit(COMPUTE) | bit(AUDIO)
      }
      crate::compiler::program::CompilerTarget::VM => {
        panic!("VM compilation doesn't go through text emission")
      }
    }
  }
}

/// Which execution contexts a function can run in, with per-context
/// discovery bookkeeping for error traces (see
/// `Program::compute_function_contexts`).
pub(crate) struct FnContexts {
  pub(crate) implementation: Arc<RwLock<TopLevelFunction>>,
  pub(crate) mask: u8,
  /// Per context: the call edge that first placed this function in
  /// that context, and the entry-point function it traces back to.
  /// `None` for contexts the function is itself an entry of.
  pub(crate) discovery:
    [Option<(SourceTrace, Arc<str>)>; execution_context::CONTEXT_COUNT],
}

/// The two host-invoked closure-entry systems — GPU dispatch and audio —
/// share all closure-lift mechanics (clone a scope-less version of each
/// captured closure, lift every data capture to its own global, rewrite
/// scope accesses into global accesses via `CaptureRewrite` /
/// `rewrite_dispatched_scope_body`) and differ only in what a capture
/// *becomes* and how clones are named:
///
/// - **GpuDispatch**: captures are per-dispatch *inputs* — implicit
///   read-only storage bindings (elided numbers), re-written and uploaded
///   at every dispatch; mutating one is a compile error
///   (`catch_dispatched_closure_scope_mutations`). Read-only storage
///   rather than uniform because storage has relaxed layout rules and
///   permits runtime-sized array captures. Clones get gensym'd `_gpu`
///   names, resolved through the registry at dispatch-record time.
/// - **Audio**: captures are owned mutable *state* — ordinary
///   storage-write globals, shared between main and audio through the
///   standard usage-derived analysis (main's one-shot seed write at the
///   `start-audio` handoff is visible as a `SeedsGlobalVar` effect on
///   the call site), thereafter mutated per-sample by the audio thread
///   and synced at batch boundaries. No GPU entry ever references them,
///   so they never become runtime GPU bindings. Clones get deterministic
///   `<original>_audio` names so both runtimes can derive the entry name
///   at `start-audio` time. Clone bodies are CPU-semantics and are kept
///   out of WGSL/C output by usage-based emission (they're reachable
///   only from audio context — see `compile_to_target`).
#[derive(Clone, Copy, PartialEq)]
enum ClosureLiftTarget {
  GpuDispatch,
  Audio,
}

impl ClosureLiftTarget {
  fn capture_global_name(
    &self,
    scope_struct_name: &str,
    field_name: &str,
  ) -> Arc<str> {
    match self {
      ClosureLiftTarget::GpuDispatch => {
        format!("{scope_struct_name}_data_{field_name}").into()
      }
      ClosureLiftTarget::Audio => {
        format!("{scope_struct_name}_audio_data_{field_name}").into()
      }
    }
  }
  fn capture_address_space(&self) -> VariableAddressSpace {
    match self {
      ClosureLiftTarget::GpuDispatch => VariableAddressSpace::StorageRead,
      ClosureLiftTarget::Audio => VariableAddressSpace::StorageReadWrite,
    }
  }
  /// Which capture types each target can carry: GPU capture bindings obey
  /// the same rule as explicit bindings (may involve a runtime-sized
  /// array only by *being* one — `validate_gpu_runtime_sized_use` runs
  /// before the lift, so it can't see the bindings created here); audio
  /// captures cross threads through the shared-snapshot system, which
  /// speaks flat words per variable (no Strings — their words are heap
  /// ids — and runtime-sized arrays only as whole variables).
  fn validate_capture(
    &self,
    field_type: &Type,
    source_trace: &SourceTrace,
    errors: &mut ErrorLog,
  ) {
    let embeds_runtime_sized = field_type.involves_runtime_sized_array()
      && !matches!(
        field_type,
        Type::Array(Some(ConcreteArraySize::Unsized), _)
      );
    match self {
      ClosureLiftTarget::GpuDispatch => {
        if embeds_runtime_sized {
          errors.log(CompileError {
            kind: RuntimeSizedFieldInBinding,
            source_trace: source_trace.clone(),
          });
        }
      }
      ClosureLiftTarget::Audio => {
        if field_type.involves_string() {
          errors.log(CompileError {
            kind: UnshareableAudioCapture("String".to_string()),
            source_trace: source_trace.clone(),
          });
        } else if embeds_runtime_sized {
          errors.log(CompileError {
            kind: UnshareableAudioCapture(
              "type embedding a runtime-sized array".to_string(),
            ),
            source_trace: source_trace.clone(),
          });
        }
      }
    }
  }
}

/// Accumulator for a closure-entry lift pass: globals and clones are
/// created while iterating the registry, so they're collected here and
/// installed at the end of the pass.
struct ClosureLiftState {
  created_globals: HashSet<Arc<str>>,
  new_vars: Vec<TopLevelVar>,
  /// Original captured-closure name → its (memoized) clone.
  clones: HashMap<Arc<str>, (Arc<str>, Arc<RwLock<AbstractFunctionSignature>>)>,
  new_functions: Vec<Arc<RwLock<AbstractFunctionSignature>>>,
}

#[derive(Debug)]
pub struct Program {
  pub names: RwLock<NameContext>,
  pub typedefs: TypeDefs,
  pub abstract_functions:
    HashMap<Arc<str>, Vec<Arc<RwLock<AbstractFunctionSignature>>>>,
  pub top_level_vars: Vec<TopLevelVar>,
  pub emulated_functions: EmulatedFunctionRecord,
  pub has_been_validated: bool,
  /// Implicit uniform bindings generated by `extract_gpu_window_info` for
  /// window-info queries (`window-time` etc.) used in GPU code. The runtime
  /// refreshes each of these from the IO manager at the start of every
  /// frame, so GPU reads see a per-frame snapshot of the ambient state.
  pub window_info_bindings: Vec<(WindowInfoBindingSource, Arc<str>)>,
}
impl Clone for Program {
  fn clone(&self) -> Self {
    Self {
      names: RwLock::new(self.names.read().unwrap().clone()),
      typedefs: self.typedefs.clone(),
      abstract_functions: self.abstract_functions.clone(),
      top_level_vars: self.top_level_vars.clone(),
      emulated_functions: self.emulated_functions.clone(),
      has_been_validated: self.has_been_validated,
      window_info_bindings: self.window_info_bindings.clone(),
    }
  }
}

impl Default for Program {
  fn default() -> Self {
    DEFAULT_PROGRAM.with(|lock| lock.read().unwrap().clone())
  }
}

impl Program {
  pub fn empty() -> Self {
    Self {
      names: NameContext::empty().into(),
      typedefs: TypeDefs::empty(),
      abstract_functions: HashMap::new(),
      top_level_vars: vec![],
      emulated_functions: EmulatedFunctionRecord::empty(),
      has_been_validated: false,
      window_info_bindings: vec![],
    }
  }
  pub fn add_top_level_var(&mut self, var: TopLevelVar, errors: &mut ErrorLog) {
    if let Some(previous_var) = self
      .top_level_vars
      .iter()
      .find(|old_var| old_var.name == var.name)
    {
      errors.log(CompileError {
        kind: VariableNameCollision(var.name.to_string()),
        source_trace: var
          .source_trace
          .clone()
          .insert_as_secondary(previous_var.source_trace.clone()),
      })
    }
    self.names.write().unwrap().track_user_name(&var.name);
    self.top_level_vars.push(var);
  }
  pub fn add_abstract_function(
    &mut self,
    signature: Arc<RwLock<AbstractFunctionSignature>>,
  ) {
    let name = Arc::clone(&signature.read().unwrap().name);
    self.names.write().unwrap().track_user_name(&name);
    if let FunctionImplementationKind::Composite(f) =
      &signature.read().unwrap().implementation
    {
      let f = f.read().unwrap();
      for (arg_name, _) in f.arg_names.iter() {
        self.names.write().unwrap().track_user_name(&arg_name);
      }
      f.expression
        .walk(&mut |exp| {
          if let ExpKind::Name(name) = &exp.kind {
            self.names.write().unwrap().track_user_name(&name);
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
    if let Some(bucket) = self.abstract_functions.get_mut(&name) {
      let mut novel = true;
      for existing_signature in bucket.iter() {
        if *existing_signature.read().unwrap() == *signature.read().unwrap() {
          novel = false;
          break;
        }
      }
      if novel {
        bucket.push(signature.into());
      }
    } else {
      self.abstract_functions.insert(name, vec![signature.into()]);
    }
  }
  pub fn with_functions(
    mut self,
    functions: Vec<AbstractFunctionSignature>,
  ) -> Self {
    for f in functions {
      self.add_abstract_function(Arc::new(RwLock::new(f)));
    }
    self
  }
  pub fn with_struct(mut self, s: Arc<AbstractStruct>) -> Self {
    if !self.typedefs.structs.contains(&s) {
      if !ABNORMAL_CONSTRUCTOR_STRUCTS.contains(&&*s.name.0) {
        self.add_abstract_function(Arc::new(RwLock::new(
          AbstractFunctionSignature {
            name: s.name.0.clone(),
            generic_args: s.generic_args.clone(),
            arg_types: s
              .fields
              .iter()
              .map(|field| (field.field_type.clone(), Ownership::Owned))
              .collect(),
            return_type: AbstractType::AbstractStruct(s.clone()),
            implementation: FunctionImplementationKind::StructConstructor,
            associative: false,
            captured_scope: None,
            entry_point: None,
          },
        )));
      }
      self.typedefs.structs.push(s.as_ref().clone());
      self.typedefs.structs.dedup();
    }
    self
  }
  pub fn with_enum(mut self, e: AbstractEnum) -> Self {
    if !self.typedefs.enums.contains(&e) {
      if !ABNORMAL_CONSTRUCTOR_STRUCTS.contains(&&*e.name.0) {
        for variant in e.variants.iter() {
          if variant.inner_type != AbstractType::Type(Type::Unit) {
            self.add_abstract_function(Arc::new(RwLock::new(
              AbstractFunctionSignature {
                name: variant.name.clone(),
                generic_args: e.generic_args.clone(),
                arg_types: vec![(variant.inner_type.clone(), Ownership::Owned)],
                return_type: AbstractType::AbstractEnum(e.clone().into()),
                implementation: FunctionImplementationKind::EnumConstructor(
                  variant.name.clone(),
                ),
                associative: false,
                captured_scope: None,
                entry_point: None,
              },
            )));
          }
        }
      }
      self.typedefs.enums.push(e);
      self.typedefs.enums.dedup();
    }
    self
  }
  pub fn with_structs(self, structs: Vec<Arc<AbstractStruct>>) -> Self {
    structs.into_iter().fold(self, |ctx, s| ctx.with_struct(s))
  }
  pub fn with_type_aliases(
    mut self,
    mut aliases: Vec<(Arc<str>, Arc<AbstractStruct>)>,
  ) -> Self {
    self.typedefs.type_aliases.append(&mut aliases);
    self
  }
  pub fn add_monomorphized_struct(&mut self, s: AbstractStruct) {
    // See add_monomorphized_enum for why name.0 rather than the full
    // name tuple.
    if !self.typedefs.structs.iter().any(|existing_struct| {
      existing_struct.name.0 == s.name.0
        && existing_struct.filled_generics == s.filled_generics
    }) {
      self.typedefs.structs.push(s);
    }
  }
  pub fn add_monomorphized_enum(&mut self, e: AbstractEnum) {
    // `name.0`, not `name`: the name tuple's SourceTrace is not identity.
    // The filled-generics comparison is safe against representational
    // divergence (the same instantiation arriving from different
    // monomorphization paths) because the type family's `PartialEq` is
    // semantic — it dereferences resolved unification variables and
    // ignores abstract ancestors.
    if !self.typedefs.enums.iter().any(|existing_enum| {
      existing_enum.name.0 == e.name.0
        && existing_enum.filled_generics == e.filled_generics
    }) {
      self.typedefs.enums.push(e);
    }
  }
  pub fn concrete_signatures(
    &mut self,
    fn_name: &Arc<str>,
    source_trace: SourceTrace,
  ) -> CompileResult<Option<Vec<Type>>> {
    if let Some(signatures) = self.abstract_functions.get(fn_name) {
      signatures
        .into_iter()
        .map(|signature| {
          Ok(Type::Function(Box::new(
            AbstractFunctionSignature::concretize(
              Arc::new(RwLock::new(signature.read().unwrap().clone())),
              &self.typedefs,
              source_trace.clone(),
            )?,
          )))
        })
        .collect::<CompileResult<Vec<_>>>()
        .map(|x| Some(x))
    } else {
      Ok(None)
    }
  }
  pub fn abstract_functions_iter(
    &self,
  ) -> impl Iterator<Item = &Arc<RwLock<AbstractFunctionSignature>>> {
    self
      .abstract_functions
      .values()
      .map(|fs| fs.iter())
      .flatten()
  }
  pub fn abstract_functions_iter_mut(
    &mut self,
  ) -> impl Iterator<Item = &mut Arc<RwLock<AbstractFunctionSignature>>> {
    self
      .abstract_functions
      .values_mut()
      .map(|fs| fs.iter_mut())
      .flatten()
  }
  pub fn from_easl_documents(
    documents: &'_ EaslMultiDocument,
    macros: Vec<Macro>,
  ) -> (Self, ErrorLog) {
    let mut errors = ErrorLog::new();
    let mut names = NameContext::empty();
    let all_syntax_trees: Vec<EaslTree> = documents
      .sources
      .iter()
      .map(|(document, _, _)| document.syntax_trees.clone())
      .flatten()
      .collect();
    for tree in all_syntax_trees.iter() {
      names.track_all_ast_names(tree);
    }
    let trees = all_syntax_trees
      .into_iter()
      .map(|tree| macroexpand(tree, &macros, &mut names, &mut errors))
      .collect::<Vec<EaslTree>>();

    let mut non_typedef_trees = vec![];
    let mut untyped_types = vec![];

    for tree in trees.into_iter() {
      use crate::parse::Encloser::*;
      use fsexp::syntax::EncloserOrOperator::*;
      let (tree_body, annotation) =
        extract_annotation(tree.clone(), &mut errors);
      let EaslTree::Inner((position, Encloser(Parens)), children) = &tree_body
      else {
        errors.log(CompileError::new(
          UnrecognizedTopLevelForm(tree_body),
          tree.position().clone().into(),
        ));
        continue;
      };
      let source_trace: SourceTrace = position.clone().into();
      let mut children_iter = children.into_iter();
      let Some(EaslTree::Leaf(position, first_child)) = children_iter.next()
      else {
        errors.log(CompileError::new(
          UnrecognizedTopLevelForm(tree_body.clone()),
          source_trace,
        ));
        continue;
      };
      let source_trace: SourceTrace = position.clone().into();
      match first_child.as_str() {
        "struct" | "enum" => {
          if annotation.is_some() {
            errors.log(CompileError {
              kind: AnnotationNotAllowedOnType,
              source_trace: source_trace.clone(),
            });
          }
          if let Some(struct_name) = children_iter.next() {
            match struct_name {
              EaslTree::Leaf(pos, name) => match first_child.as_str() {
                "struct" => untyped_types.push(UntypedType::Struct(
                  UntypedStruct::from_field_trees(
                    (name.clone().into(), pos.into()),
                    vec![],
                    children_iter.cloned().collect(),
                    source_trace,
                    &mut errors,
                  ),
                )),
                "enum" => match UntypedEnum::from_field_trees(
                  (name.clone().into(), pos.into()),
                  vec![],
                  children_iter.cloned().collect(),
                  source_trace,
                ) {
                  Ok(e) => untyped_types.push(UntypedType::Enum(e)),
                  Err(e) => errors.log(e),
                },
                _ => unreachable!(),
              },
              EaslTree::Inner(
                (position, Encloser(Parens)),
                signature_children,
              ) => {
                let source_trace: SourceTrace = position.clone().into();
                let mut signature_iter = signature_children.iter().cloned();
                if let Some(EaslTree::Leaf(name_pos, type_name)) =
                  signature_iter.next()
                {
                  let type_name: Arc<str> = type_name.into();
                  let type_name_source: SourceTrace = name_pos.into();
                  match signature_iter
                    .map(|subtree| {
                      parse_generic_argument(
                        subtree,
                        &TypeDefs::empty(),
                        &vec![],
                      )
                    })
                    .collect::<CompileResult<Vec<_>>>()
                  {
                    Ok(generic_args) => {
                      if generic_args.is_empty() {
                        errors.log(CompileError::new(
                          InvalidTypeName,
                          source_trace,
                        ));
                      } else {
                        match first_child.as_str() {
                          "struct" => untyped_types.push(UntypedType::Struct(
                            UntypedStruct::from_field_trees(
                              (type_name, type_name_source),
                              generic_args,
                              children_iter.cloned().collect(),
                              source_trace,
                              &mut errors,
                            ),
                          )),
                          "enum" => {
                            match UntypedEnum::from_field_trees(
                              (type_name, type_name_source),
                              generic_args,
                              children_iter.cloned().collect(),
                              source_trace,
                            ) {
                              Ok(e) => untyped_types.push(UntypedType::Enum(e)),
                              Err(e) => errors.log(e),
                            }
                          }
                          _ => unreachable!(),
                        }
                      }
                    }
                    Err(e) => errors.log(e),
                  }
                } else {
                  errors.log(CompileError::new(InvalidTypeName, source_trace));
                }
              }
              EaslTree::Inner((position, _), _) => {
                errors.log(CompileError::new(
                  InvalidTypeName,
                  position.clone().into(),
                ));
              }
            }
          } else {
            errors.log(CompileError::new(InvalidTypeDefinition, source_trace));
          }
        }
        _ => non_typedef_trees.push((annotation, tree_body)),
      }
    }
    let mut program = Program::default();
    program.names = names.into();
    match UntypedType::sort_by_references(&untyped_types) {
      Ok(sorted_untyped_types) => {
        for name in macros.iter().flat_map(|m| m.reserved_names.iter().cloned())
        {
          program.names.write().unwrap().user_names.insert(name);
        }
        for untyped_type in sorted_untyped_types {
          match untyped_type {
            UntypedType::Struct(untyped_struct) => {
              match untyped_struct.assign_types(&program.typedefs) {
                Ok(s) => program = program.with_struct(s.into()),
                Err(e) => errors.log(e),
              }
            }
            UntypedType::Enum(untyped_enum) => {
              match untyped_enum.assign_types(&program.typedefs) {
                Ok(e) => program = program.with_enum(e.into()),
                Err(e) => errors.log(e),
              }
            }
          }
        }
      }
      Err(e) => {
        let source_trace = if let Some(first_name) = e.get(0)
          && let Some(primary_type) =
            untyped_types.iter().find(|t| t.name() == first_name)
        {
          let mut source_trace = primary_type.source_trace().clone();
          for i in 1..e.len() {
            if let Some(secondary_type) =
              untyped_types.iter().find(|t| t.name() == &e[i])
            {
              source_trace = source_trace
                .insert_as_secondary(secondary_type.source_trace().clone());
            }
          }
          source_trace
        } else {
          SourceTrace::empty()
        };
        errors.log(CompileError::new(
          TypeDependencyCycle(
            e.into_iter().map(|name| name.to_string()).collect(),
          ),
          source_trace,
        ));
      }
    }

    for (annotation, tree) in non_typedef_trees.into_iter() {
      use crate::parse::Encloser::*;
      use fsexp::syntax::EncloserOrOperator::*;
      if let EaslTree::Inner((parens_position, Encloser(Parens)), children) =
        tree
      {
        let parens_source_trace: SourceTrace = parens_position.clone().into();
        let mut children_iter = children.into_iter();
        let first_child = children_iter.next();
        if let Some(EaslTree::Leaf(first_child_position, first_child)) =
          first_child
        {
          let first_child_source_trace: SourceTrace =
            first_child_position.clone().into();
          match first_child.as_str() {
            "import" => {}
            "var" | "def" | "override" => {
              if let Some(var) = TopLevelVar::from_ast(
                first_child.as_str(),
                &parens_source_trace,
                children_iter,
                &program,
                annotation,
                &mut errors,
              ) {
                program.add_top_level_var(var, &mut errors);
              }
            }
            "defn" => {
              if let Some(f) = AbstractFunctionSignature::from_defn_ast(
                children_iter,
                first_child_source_trace,
                parens_source_trace,
                annotation,
                &program,
                &mut errors,
              ) {
                program.add_abstract_function(Arc::new(RwLock::new(f)));
              }
            }
            _ => {
              errors.log(CompileError::new(
                UnrecognizedTopLevelForm(EaslTree::Leaf(
                  first_child_position.clone(),
                  first_child,
                )),
                first_child_source_trace,
              ));
            }
          }
        } else {
          errors.log(CompileError::new(
            UnrecognizedTopLevelForm(first_child.unwrap_or(EaslTree::Inner(
              (
                parens_position.clone(),
                EncloserOrOperator::Encloser(Parens),
              ),
              vec![],
            ))),
            parens_source_trace,
          ));
        }
      } else {
        errors.log(CompileError::new(
          UnrecognizedTopLevelForm(tree.clone()),
          tree.position().clone().into(),
        ));
      }
    }
    (program, errors)
  }
  fn propagate_types(&mut self, errors: &mut ErrorLog) -> bool {
    let mut base_context = self.clone();
    let mut anything_changed = false;
    for var in self.top_level_vars.iter_mut() {
      if let Some(value_expression) = &mut var.value {
        let changed = value_expression.data.constrain(
          &var.var_type.clone().known(),
          &var.source_trace,
          errors,
        );
        anything_changed |= changed;
        let changed =
          value_expression.propagate_types(&mut base_context, errors);
        anything_changed |= changed;
      }
    }
    for f in self.abstract_functions_iter_mut() {
      if let FunctionImplementationKind::Composite(implementation) =
        &f.read().unwrap().implementation
      {
        let changed = implementation
          .write()
          .unwrap()
          .expression
          .propagate_types(&mut base_context, errors);
        anything_changed |= changed;
      }
    }
    anything_changed
  }
  fn find_untyped(&mut self) -> Vec<SourceTrace> {
    self
      .abstract_functions_iter()
      .map(|f| {
        if let FunctionImplementationKind::Composite(implementation) =
          &f.read().unwrap().implementation
        {
          implementation.write().unwrap().expression.find_untyped()
        } else {
          vec![]
        }
      })
      .collect::<Vec<_>>()
      .into_iter()
      .chain(self.top_level_vars.iter_mut().map(|v| {
        if let Some(value) = &mut v.value {
          value.find_untyped()
        } else {
          vec![]
        }
        .into_iter()
        .chain(
          (!v.var_type.check_is_fully_known())
            .then(|| v.source_trace.clone())
            .into_iter(),
        )
        .collect()
      }))
      .flatten()
      .collect()
  }
  pub fn validate_match_blocks(&self, errors: &mut ErrorLog) {
    for abstract_function in self.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(implementation) =
        &abstract_function.read().unwrap().implementation
      {
        (**implementation)
          .write()
          .unwrap()
          .expression
          .validate_match_blocks(errors);
      }
    }
  }
  pub fn catch_illegal_function_type_expressions(&self, errors: &mut ErrorLog) {
    for abstract_function in self.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(implementation) =
        &abstract_function.read().unwrap().implementation
      {
        (**implementation)
          .write()
          .unwrap()
          .expression
          .catch_illegal_function_type_expressions(errors);
      }
    }
  }
  pub fn catch_illegal_function_type_user_type_fields(
    &self,
    errors: &mut ErrorLog,
  ) {
    for s in self.typedefs.structs.iter() {
      for f in s.fields.iter() {
        match f.field_type {
          AbstractType::Type(Type::Function(_)) => {
            errors.log(CompileError::new(
              CantStoreFunctionInDataStructure,
              f.source_trace.clone(),
            ))
          }
          _ => {}
        }
      }
    }
    for e in self.typedefs.enums.iter() {
      for v in e.variants.iter() {
        match v.inner_type {
          AbstractType::Type(Type::Function(_)) => {
            errors.log(CompileError::new(
              CantStoreFunctionInDataStructure,
              v.source.clone(),
            ))
          }
          _ => {}
        }
      }
    }
  }
  pub fn catch_illegal_function_type_variables(&self, errors: &mut ErrorLog) {
    for v in self.top_level_vars.iter() {
      if matches!(v.var_type, Type::Function(_)) {
        errors.log(CompileError::new(
          CantHaveFunctionTypeVariable,
          v.source_trace.clone(),
        ));
      }
    }
  }
  pub fn fully_infer_types(&mut self, errors: &mut ErrorLog) {
    loop {
      let did_type_states_change = self.propagate_types(errors);
      if !did_type_states_change {
        let untyped_expressions = self.find_untyped();
        return if untyped_expressions.is_empty() {
          break;
        } else {
          for source_trace in untyped_expressions {
            let source_trace = source_trace;
            errors.log(CompileError::new(CouldntInferTypes, source_trace));
          }
        };
      }
    }
  }
  /// Rewrites pseudo-applications used for indexing data — `(arr i)`, `(v i)`,
  /// `(m i)` — into a uniform `Access(ArrayIndex(i), subexp)` form. Easl
  /// reuses the function-application syntax for indexing since the parser
  /// can't distinguish; this pass is run after type inference, when we know
  /// which Applications are *actually* indexing arrays/vectors/matrices, and
  /// converts them so later passes can treat them uniformly as `Access`
  /// expressions instead of overloading `Application`.
  pub fn normalize_pseudoapplication_data_accesses(&mut self) {
    for abstract_f in self.abstract_functions_iter() {
      let abstract_f = abstract_f.read().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &abstract_f.implementation
      {
        implementation
          .write()
          .unwrap()
          .expression
          .walk_mut(&mut |exp| {
            if let ExpKind::Application(f, _) = &exp.kind {
              let f_type = f.data.unwrap_known();
              let is_data_access = matches!(f_type, Type::Array(_, _))
                || f_type.is_vector()
                || f_type.is_matrix();
              if is_data_access {
                take(&mut exp.kind, |kind| {
                  let ExpKind::Application(f, mut args) = kind else {
                    panic!()
                  };
                  ExpKind::Access(
                    Accessor::ArrayIndex(args.remove(0).into()),
                    f.into(),
                  )
                });
              }
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
  }
  pub fn validate_assignments(&mut self, errors: &mut ErrorLog) {
    for abstract_f in self.abstract_functions_iter() {
      let abstract_f = abstract_f.read().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &abstract_f.implementation
      {
        let implementation = implementation.write().unwrap();
        if let Err(e) = implementation.expression.validate_assignments(self) {
          errors.log(e);
        }
      }
    }
  }
  pub fn monomorphize(
    &mut self,
    errors: &mut ErrorLog,
    target: CompilerTarget,
  ) {
    let mut monomorphized_ctx = Program::default();
    monomorphized_ctx.names = RwLock::new(self.names.read().unwrap().clone());
    for f in self.abstract_functions_iter() {
      if f.read().unwrap().generic_args.is_empty()
        && let FunctionImplementationKind::Composite(implementation) =
          &f.read().unwrap().implementation
      {
        let mut borrowed_implementation = implementation.write().unwrap();
        match borrowed_implementation.expression.monomorphize(
          &self,
          &mut monomorphized_ctx,
          target,
        ) {
          Ok(_) => {
            let mut new_f = (**f).read().unwrap().clone();
            new_f.implementation =
              FunctionImplementationKind::Composite(implementation.clone());
            drop(borrowed_implementation);
            monomorphized_ctx
              .add_abstract_function(Arc::new(RwLock::new(new_f)));
          }
          Err(e) => errors.log(e),
        }
      } else {
        monomorphized_ctx.add_abstract_function(Arc::clone(f));
      }
    }
    for s in self.typedefs.structs.iter() {
      if s.generic_args.is_empty() {
        monomorphized_ctx.add_monomorphized_struct(s.clone());
      }
    }
    for e in self.typedefs.enums.iter() {
      if e.generic_args.is_empty() {
        monomorphized_ctx.add_monomorphized_enum(e.clone());
      }
    }
    take(self, |old_ctx| {
      monomorphized_ctx.top_level_vars = old_ctx.top_level_vars;
      monomorphized_ctx.window_info_bindings = old_ctx.window_info_bindings;
      monomorphized_ctx
    });
  }
  pub fn extract_non_bound_mutable_references(&mut self) {
    for f in self.abstract_functions_iter() {
      let borrowed_f = f.read().unwrap();
      if borrowed_f.generic_args.is_empty()
        && !borrowed_f.has_uninlined_higher_order_arguments()
      {
        if let FunctionImplementationKind::Composite(implementation) =
          &borrowed_f.implementation
        {
          let mut implementation = implementation.write().unwrap();
          let exp = &mut implementation.expression;
          let ExpKind::Function(_, body) = &mut exp.kind else {
            panic!()
          };
          let pending = body.extract_non_bound_mutable_references(&self.names);
          if !pending.is_empty() {
            take(&mut **body, |old_body| TypedExp {
              data: old_body.data.clone(),
              source_trace: old_body.source_trace.clone(),
              kind: ExpKind::Let(pending, Box::new(old_body)),
            });
          }
        }
      }
    }
  }
  pub fn validate_argument_ownership(&mut self, errors: &mut ErrorLog) {
    for f in self.abstract_functions_iter() {
      let mut borrowed_f = f.write().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &mut borrowed_f.implementation
      {
        let mut implementation = implementation.write().unwrap();
        implementation
          .expression
          .walk_mut_with_ctx::<Never>(
            &mut |exp, ctx| {
              match &exp.kind {
                ExpKind::Application(f, args) => {
                  if let Type::Function(f) = f.data.unwrap_known()
                    && let Some(abstract_f) = f.abstract_ancestor
                    && let abstract_f = abstract_f.read().unwrap()
                    && let FunctionImplementationKind::Composite(_) =
                      abstract_f.implementation
                  {
                    for (i, (_, expected_ownership)) in
                      abstract_f.arg_types.iter().enumerate()
                    {
                      let arg = &args[i];
                      match expected_ownership {
                        Ownership::Owned => {
                          if arg.data.ownership != Ownership::Owned {
                            errors.log(CompileError::new(
                              ArgumentMustBeOwnedValue,
                              arg.source_trace.clone(),
                            ));
                          }
                        }
                        Ownership::Reference | Ownership::MutableReference => {
                          if let Some(name) = arg.name_or_inner_accessed_name()
                          {
                            let top_level_var = self
                              .top_level_vars
                              .iter()
                              .find(|v| v.name == *name);
                            if let Some(TopLevelVar {
                              kind:
                                TopLevelVariableKind::Var {
                                  address_space, ..
                                },
                              ..
                            }) = top_level_var
                              && !address_space.may_be_passed_as_reference()
                            {
                              errors.log(CompileError::new(
                                PassedReferenceFromInvalidAddressSpace(
                                  *address_space,
                                ),
                                arg.source_trace.clone(),
                              ));
                            }
                            if *expected_ownership
                              == Ownership::MutableReference
                            {
                              match arg.data.ownership {
                                Ownership::Reference => {
                                  errors.log(CompileError::new(
                                    ReferenceMustBeMutable,
                                    arg.source_trace.clone(),
                                  ));
                                }
                                Ownership::Owned => {
                                  if ctx
                                    .variables
                                    .get(&**name)
                                    .map(|(v, _)| v.kind)
                                    .or_else(|| {
                                      top_level_var
                                        .map(TopLevelVar::variable_kind)
                                    })
                                    .unwrap()
                                    != VariableKind::Var
                                  {
                                    errors.log(CompileError::new(
                                      ImmutableOwnedPassedAsMutableReference,
                                      arg.source_trace.clone(),
                                    ));
                                  }
                                }
                                _ => {}
                              }
                            }
                          } else {
                            errors.log(CompileError::new(
                              ReferenceArgumentMustBeName,
                              arg.source_trace.clone(),
                            ));
                          }
                        }
                        Ownership::Pointer(_) => {
                          unreachable!(
                            "unexpected Ownership::Pointer encountered"
                          )
                        }
                      }
                    }
                  }
                }
                _ => {}
              }
              Ok(true)
            },
            &mut ImmutableProgramLocalContext::empty(self),
          )
          .unwrap();
      }
    }
  }
  pub fn validate_field_type_constraints(&mut self, errors: &mut ErrorLog) {
    for v in self.top_level_vars.iter() {
      if let Type::Struct(s) = &v.var_type {
        s.check_type_constraints(&v.source_trace, errors);
      }
    }
    for f in self.abstract_functions_iter() {
      let mut borrowed_f = f.write().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &mut borrowed_f.implementation
      {
        let implementation = implementation.write().unwrap();
        implementation
          .expression
          .walk::<Never>(&mut |exp| {
            if let Type::Struct(s) = exp.data.unwrap_known() {
              s.check_type_constraints(&exp.source_trace, errors);
            }
            Ok(true)
          })
          .unwrap();
      }
    }
  }
  pub fn validate_dispatch_function_types_and_mark_implicit_entry_points(
    &mut self,
    errors: &mut ErrorLog,
  ) {
    for top_level_fn in self.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(implementation) =
        &top_level_fn.read().unwrap().implementation
      {
        implementation
          .read()
          .unwrap()
          .expression
          .walk(&mut |exp| {
            'breakable: {
              if let ExpKind::Application(f, args) = &exp.kind
                && let ExpKind::Name(f_name) = &f.kind
              {
                match &**f_name {
                  "dispatch-compute-shader" => {
                    if let Type::Function(compute_fn) =
                      args[0].data.unwrap_known()
                      && let Some(abstract_compute_fn) =
                        compute_fn.abstract_ancestor
                    {
                      let mut abstract_compute_fn =
                        abstract_compute_fn.write().unwrap();
                      if let Some(entry_point) = abstract_compute_fn.entry_point
                      {
                        if !matches!(entry_point, EntryPoint::Compute(_)) {
                          errors.log(CompileError::new(
                            WrongEntryPointTypeForDispatchComputeShader(
                              entry_point.name().into(),
                            ),
                            exp.source_trace.clone(),
                          ))
                        }
                      } else {
                        abstract_compute_fn.entry_point =
                          Some(EntryPoint::Compute(1))
                      }
                      for other_abstract_f in self.abstract_functions_iter() {
                        if let Ok(mut other_abstract_f) =
                          other_abstract_f.try_write()
                        {
                          if other_abstract_f.name == abstract_compute_fn.name
                            && let FunctionImplementationKind::Composite(
                              other_f,
                            ) = &other_abstract_f.implementation
                          {
                            other_f.write().unwrap().entry_point =
                              abstract_compute_fn.entry_point;
                            other_abstract_f.entry_point =
                              abstract_compute_fn.entry_point;
                          }
                        }
                      }
                    }
                  }
                  "dispatch-render-shaders" => {
                    if let Type::Function(vertex_fn) =
                      args[0].data.unwrap_known()
                      && let Some(abstract_vertex_fn) =
                        &vertex_fn.abstract_ancestor
                      && let Type::Function(fragment_fn) =
                        args[1].data.unwrap_known()
                      && let Some(abstract_fragment_fn) =
                        &fragment_fn.abstract_ancestor
                    {
                      let mut abstract_vertex_fn =
                        abstract_vertex_fn.write().unwrap();
                      if let Some(entry_point) = abstract_vertex_fn.entry_point
                      {
                        if entry_point != EntryPoint::Vertex {
                          errors.log(CompileError::new(
                            WrongEntryPointTypeForDispatchVertexShader(
                              entry_point.name().into(),
                            ),
                            exp.source_trace.clone(),
                          ))
                        }
                      } else {
                        abstract_vertex_fn.entry_point =
                          Some(EntryPoint::Vertex);
                        for other_abstract_f in self.abstract_functions_iter() {
                          if let Ok(mut other_abstract_f) =
                            other_abstract_f.try_write()
                          {
                            if other_abstract_f.name == abstract_vertex_fn.name
                              && let FunctionImplementationKind::Composite(
                                other_f,
                              ) = &other_abstract_f.implementation
                            {
                              other_f.write().unwrap().entry_point =
                                Some(EntryPoint::Vertex);
                              other_abstract_f.entry_point =
                                Some(EntryPoint::Vertex);
                            }
                          }
                        }
                      }
                      let mut abstract_fragment_fn =
                        abstract_fragment_fn.write().unwrap();
                      if let Some(entry_point) =
                        abstract_fragment_fn.entry_point
                      {
                        if entry_point != EntryPoint::Fragment {
                          errors.log(CompileError::new(
                            WrongEntryPointTypeForDispatchFragmentShader(
                              entry_point.name().into(),
                            ),
                            exp.source_trace.clone(),
                          ))
                        }
                      } else {
                        abstract_fragment_fn.entry_point =
                          Some(EntryPoint::Fragment);
                        for other_abstract_f in self.abstract_functions_iter() {
                          if let Ok(mut other_abstract_f) =
                            other_abstract_f.try_write()
                          {
                            if other_abstract_f.name
                              == abstract_fragment_fn.name
                              && let FunctionImplementationKind::Composite(
                                other_f,
                              ) = &other_abstract_f.implementation
                            {
                              other_f.write().unwrap().entry_point =
                                Some(EntryPoint::Fragment);
                              other_abstract_f.entry_point =
                                Some(EntryPoint::Fragment);
                            }
                          }
                        }
                      }

                      let FunctionImplementationKind::Composite(
                        frag_implementation,
                      ) = &abstract_fragment_fn.implementation
                      else {
                        errors.log(CompileError::new(
                          InvalidShaderEntry(
                            abstract_fragment_fn.name.to_string(),
                          ),
                          exp.source_trace.clone(),
                        ));
                        break 'breakable;
                      };
                      let mut output_locations: HashMap<usize, Type> =
                        HashMap::new();
                      vertex_fn
                        .return_type
                        .unwrap_known()
                        .gather_location_annotations(&mut output_locations);
                      let mut input_locations: HashMap<usize, Type> =
                        HashMap::new();
                      for ((arg, _), annotation) in fragment_fn.args.iter().zip(
                        frag_implementation
                          .read()
                          .unwrap()
                          .arg_annotations
                          .iter(),
                      ) {
                        let arg_type = arg.var_type.unwrap_known();
                        if let Some((location, _)) =
                          annotation.attributes.location()
                        {
                          input_locations.insert(location, arg_type);
                        } else {
                          arg_type
                            .gather_location_annotations(&mut input_locations);
                        }
                      }
                      if input_locations.len() != output_locations.len()
                        || input_locations.iter().any(|(location, in_ty)| {
                          if let Some(out_ty) = output_locations.get(location) {
                            in_ty.compatible(out_ty)
                          } else {
                            true
                          }
                        })
                      {
                        errors.log(CompileError::new(
                          IncompatibleRenderEntryPoints(
                            abstract_vertex_fn.name.to_string(),
                            abstract_fragment_fn.name.to_string(),
                          ),
                          exp.source_trace.clone(),
                        ));
                      }
                    }
                  }
                  "start-audio" => {
                    if let Type::Function(audio_fn) =
                      args[0].data.unwrap_known()
                      && let Some(abstract_audio_fn) =
                        audio_fn.abstract_ancestor
                    {
                      let mut abstract_audio_fn =
                        abstract_audio_fn.write().unwrap();
                      if let Some(entry_point) = abstract_audio_fn.entry_point {
                        if entry_point != EntryPoint::Audio {
                          errors.log(CompileError::new(
                            WrongEntryPointTypeForStartAudio(
                              entry_point.name().into(),
                            ),
                            exp.source_trace.clone(),
                          ))
                        }
                      } else {
                        abstract_audio_fn.entry_point = Some(EntryPoint::Audio);
                      }
                      for other_abstract_f in self.abstract_functions_iter() {
                        if let Ok(mut other_abstract_f) =
                          other_abstract_f.try_write()
                        {
                          if other_abstract_f.name == abstract_audio_fn.name
                            && let FunctionImplementationKind::Composite(
                              other_f,
                            ) = &other_abstract_f.implementation
                          {
                            other_f.write().unwrap().entry_point =
                              abstract_audio_fn.entry_point;
                            other_abstract_f.entry_point =
                              abstract_audio_fn.entry_point;
                          }
                        }
                      }
                    }
                  }
                  _ => {}
                }
              }
            }
            Ok::<bool, Never>(true)
          })
          .unwrap()
      }
    }
  }
  /// Emits an error for any closure dispatched to the GPU (via
  /// `dispatch-compute-shader` / `dispatch-render-shaders`) that mutates a
  /// variable captured in its scope, whether directly or inside a nested
  /// closure the scope is forwarded to. A dispatched closure's captured
  /// scope lives in a read-only storage binding (see
  /// `extract_dispatched_closure_scopes`) and its body runs once per GPU
  /// thread, so there's no meaningful semantics for such writes — and
  /// without this check they'd surface as naga validation failures at
  /// pipeline-creation time rather than as a compile error.
  pub fn catch_dispatched_closure_scope_mutations(
    &self,
    errors: &mut ErrorLog,
  ) {
    let mut checked_closures: HashSet<Arc<str>> = HashSet::new();
    for f in self.abstract_functions_iter() {
      let FunctionImplementationKind::Composite(implementation) =
        f.read().unwrap().implementation.clone()
      else {
        continue;
      };
      implementation
        .read()
        .unwrap()
        .expression
        .walk(&mut |exp| {
          if let ExpKind::Application(applied_f, args) = &exp.kind
            && let ExpKind::Name(applied_f_name) = &applied_f.kind
          {
            let dispatched_fn_count = match &**applied_f_name {
              "dispatch-compute-shader" => 1,
              "dispatch-render-shaders" => 2,
              _ => 0,
            };
            for arg in args.iter().take(dispatched_fn_count) {
              if let Type::Function(signature) = arg.data.unwrap_known()
                && let Some(ancestor) = signature.abstract_ancestor
              {
                self.check_dispatched_closure_scope_mutations(
                  ancestor,
                  errors,
                  &mut checked_closures,
                );
              }
            }
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
  }
  /// Checks one dispatched closure's body for mutations of its captured
  /// scope: any argument rooted at the scope parameter that's passed to a
  /// mutable-reference parameter counts as a mutation, except the trailing
  /// scope-forwarding argument of a call to a nested closure, which is
  /// checked recursively against that closure's own scope instead.
  fn check_dispatched_closure_scope_mutations(
    &self,
    closure: Arc<RwLock<AbstractFunctionSignature>>,
    errors: &mut ErrorLog,
    checked_closures: &mut HashSet<Arc<str>>,
  ) {
    let (scope_param_name, implementation) = {
      let closure = closure.read().unwrap();
      if closure.captured_scope.is_none()
        || !checked_closures.insert(closure.name.clone())
      {
        return;
      }
      let FunctionImplementationKind::Composite(implementation) =
        closure.implementation.clone()
      else {
        return;
      };
      let Some((scope_param_name, _)) =
        implementation.read().unwrap().arg_names.last().cloned()
      else {
        return;
      };
      (scope_param_name, implementation)
    };
    // For an access chain rooted at the scope parameter, returns the name of
    // the captured variable being accessed (the field directly on the scope).
    let scope_rooted_capture_name = |exp: &TypedExp| -> Option<Arc<str>> {
      let mut field: Option<Arc<str>> = None;
      let mut current = exp;
      loop {
        match &current.kind {
          ExpKind::Access(accessor, inner) => {
            if let Accessor::Field(name) = accessor {
              field = Some(name.clone());
            }
            current = inner;
          }
          ExpKind::Name(name) => {
            return (*name == scope_param_name)
              .then(|| field.unwrap_or_else(|| name.clone()));
          }
          _ => return None,
        }
      }
    };
    let implementation = implementation.read().unwrap();
    let ExpKind::Function(_, body) = &implementation.expression.kind else {
      return;
    };
    body
      .walk(&mut |exp| {
        if let ExpKind::Application(applied_f, args) = &exp.kind
          && let TypeState::Known(Type::Function(applied_signature)) =
            &applied_f.data.kind
        {
          let param_ownerships: Vec<Ownership> = if let Some(applied_ancestor) =
            &applied_signature.abstract_ancestor
          {
            applied_ancestor
              .read()
              .unwrap()
              .arg_types
              .iter()
              .map(|(_, ownership)| *ownership)
              .collect()
          } else {
            applied_signature
              .args
              .iter()
              .map(|(arg, _)| arg.var_type.ownership)
              .collect()
          };
          for (i, arg) in args.iter().enumerate() {
            if !matches!(
              param_ownerships.get(i),
              Some(Ownership::MutableReference)
            ) {
              continue;
            }
            let Some(captured_name) = scope_rooted_capture_name(arg) else {
              continue;
            };
            if i + 1 == args.len()
              && let Some(applied_ancestor) =
                &applied_signature.abstract_ancestor
              && applied_ancestor.read().unwrap().captured_scope.is_some()
            {
              self.check_dispatched_closure_scope_mutations(
                applied_ancestor.clone(),
                errors,
                checked_closures,
              );
              continue;
            }
            errors.log(CompileError::new(
              CantMutateDispatchedClosureCapture(captured_name.to_string()),
              exp.source_trace.clone(),
            ));
          }
        }
        Ok::<bool, Never>(true)
      })
      .unwrap();
  }
  /// Rewrites references to a dispatched closure's captured scope within
  /// `body` so the scope is accessed as lifted per-capture data rather
  /// than through the (removed) scope parameter: each `scope.field`
  /// access to a data capture is replaced with a read of that capture's
  /// own global, and each call forwarding a captured *closure* (its
  /// trailing `scope.field` arg) is repointed to the callee's GPU clone
  /// with the scope arg dropped — the clone reads its own lifted globals
  /// instead (see `cloneify_captured_closure`). Reference-ownership access
  /// chains rooted at the scope become owned — naga doesn't allow
  /// storage-space pointers, and scopes are read-only on the GPU.
  fn rewrite_dispatched_scope_body(
    &self,
    body: &mut TypedExp,
    scope_name: &Arc<str>,
    rewrites: &HashMap<Arc<str>, CaptureRewrite>,
  ) {
    let lifted_globals: HashSet<Arc<str>> = rewrites
      .values()
      .filter_map(|r| match r {
        CaptureRewrite::Global(global_name) => Some(global_name.clone()),
        CaptureRewrite::CalleeClone { .. } => None,
      })
      .collect();
    body
      .walk_mut(&mut |e| {
        let scope_field_name = |exp: &TypedExp| -> Option<Arc<str>> {
          if let ExpKind::Access(Accessor::Field(field_name), inner) = &exp.kind
            && matches!(&inner.kind, ExpKind::Name(name) if name == scope_name)
          {
            Some(field_name.clone())
          } else {
            None
          }
        };
        let scope_rooted = |exp: &TypedExp| {
          let mut root = exp;
          loop {
            match &root.kind {
              ExpKind::Access(_, inner) => root = inner,
              ExpKind::Name(name) => {
                break name == scope_name || lifted_globals.contains(name);
              }
              _ => break false,
            }
          }
        };
        if let ExpKind::Application(f_exp, args) = &mut e.kind
          && let Some(field_name) = args.last().and_then(&scope_field_name)
          && let Some(CaptureRewrite::CalleeClone {
            clone_name,
            clone_signature,
          }) = rewrites.get(&field_name)
        {
          args.pop();
          let ExpKind::Name(callee_name) = &mut f_exp.kind else {
            panic!(
              "dispatched closure calls a captured closure through a \
               non-Name callee"
            )
          };
          *callee_name = clone_name.clone();
          f_exp.data.as_known_mut(|t| {
            let Type::Function(signature) = t else {
              panic!("captured closure call had a non-function callee type")
            };
            signature.args.pop();
            signature.abstract_ancestor = Some(clone_signature.clone());
          });
        }
        if let Some(field_name) = scope_field_name(e) {
          match rewrites.get(&field_name) {
            Some(CaptureRewrite::Global(global_name)) => {
              e.kind = ExpKind::Name(global_name.clone());
              e.data.is_globally_bound = true;
              e.data.ownership = Ownership::Owned;
            }
            Some(CaptureRewrite::CalleeClone { .. }) => panic!(
              "captured closure `{field_name}` is used as a value inside a \
               dispatched closure; captured closures only support being \
               called"
            ),
            None => panic!(
              "dispatched closure scope field `{field_name}` has no lifted \
               rewrite"
            ),
          }
        } else if matches!(&e.kind, ExpKind::Name(name) if name == scope_name) {
          panic!(
            "dispatched closure body uses its whole captured scope as a \
             value; captures are lifted to per-field globals, which only \
             supports field accesses"
          );
        } else if matches!(&e.kind, ExpKind::Access(_, _))
          && scope_rooted(e)
          && matches!(
            e.data.ownership,
            Ownership::Reference | Ownership::MutableReference
          )
        {
          e.data.ownership = Ownership::Owned;
        }
        Ok::<bool, Never>(true)
      })
      .unwrap();
  }
  /// Whether a struct field of this type makes the struct's WGSL
  /// declaration impossible: runtime-sized arrays and strings have no
  /// (or no legal) shader representation, and function-typed fields
  /// declare as their representative captured-scope structs, so the
  /// check recurses through those scopes' own fields.
  pub(crate) fn type_makes_struct_cpu_only(&self, t: &Type) -> bool {
    match t {
      Type::Function(signature) => {
        let Some(ancestor) = &signature.abstract_ancestor else {
          return true;
        };
        let Some(scope_struct) =
          ancestor.read().unwrap().captured_scope.clone()
        else {
          return true;
        };
        scope_struct.fields.iter().any(|f| {
          let Ok(field_type) = f.field_type.clone().concretize(
            &vec![],
            &self.typedefs,
            f.source_trace.clone(),
          ) else {
            return false;
          };
          self.type_makes_struct_cpu_only(&field_type)
        })
      }
      other => other.involves_string() || other.involves_runtime_sized_array(),
    }
  }
  /// Lifts every capture of a closure-entry scope struct (see
  /// `ClosureLiftTarget` for what each target turns captures into): data
  /// captures each get their own implicit global, and captured *closures*
  /// recurse — their own captures are lifted the same way and the closure
  /// gets a memoized clone via `cloneify_captured_closure`. Returns the
  /// per-field rewrite map `rewrite_dispatched_scope_body` applies to the
  /// body.
  fn lift_closure_entry_captures(
    &self,
    scope_struct: &AbstractStruct,
    source_trace: &SourceTrace,
    target: ClosureLiftTarget,
    state: &mut ClosureLiftState,
    errors: &mut ErrorLog,
  ) -> HashMap<Arc<str>, CaptureRewrite> {
    let scope_struct_name = scope_struct.name.0.clone();
    scope_struct
      .fields
      .iter()
      .map(|field| {
        let AbstractType::Type(field_type) = &field.field_type else {
          panic!("captured scope field with non-concrete type")
        };
        if let Type::Function(signature) = field_type {
          let Some(field_ancestor) = &signature.abstract_ancestor else {
            panic!("captured closure without an abstract ancestor")
          };
          if field_ancestor.read().unwrap().captured_scope.is_none() {
            panic!(
              "captured scope-less closure; these are unit-like and \
               shouldn't be captured"
            )
          }
          let (clone_name, clone_signature) = self.cloneify_captured_closure(
            field_ancestor.clone(),
            target,
            state,
            errors,
          );
          (
            field.name.clone(),
            CaptureRewrite::CalleeClone {
              clone_name,
              clone_signature,
            },
          )
        } else {
          target.validate_capture(field_type, source_trace, errors);
          let global_name =
            target.capture_global_name(&scope_struct_name, &field.name);
          // The same closure definition can be captured along several
          // paths to an entry; its globals are keyed by its own scope
          // struct, so they're created once and shared.
          if state.created_globals.insert(global_name.clone()) {
            state.new_vars.push(TopLevelVar {
              name: global_name.clone(),
              kind: TopLevelVariableKind::Var {
                address_space: target.capture_address_space(),
                // Binding numbers are assigned centrally at the end of
                // validation. (Audio capture globals never become
                // runtime GPU bindings — no GPU entry references them —
                // but carry elided numbers like any storage var.)
                group_and_binding: Some(BindingSpec::Elided),
              },
              var_type: field_type.clone(),
              value: None,
              source_trace: source_trace.clone(),
              external: false,
            });
          }
          (field.name.clone(), CaptureRewrite::Global(global_name))
        }
      })
      .collect()
  }
  /// Creates (and memoizes) the scope-less clone of a closure captured —
  /// directly or transitively — by a closure entry: the clone's trailing
  /// scope parameter is dropped, its captures are lifted to per-capture
  /// globals (recursing into further captured closures), and its body
  /// reads those globals instead of the scope. The original definition is
  /// left untouched, so direct CPU calls of the closure keep working.
  /// Naming and capture semantics per `ClosureLiftTarget`.
  fn cloneify_captured_closure(
    &self,
    ancestor: Arc<RwLock<AbstractFunctionSignature>>,
    target: ClosureLiftTarget,
    state: &mut ClosureLiftState,
    errors: &mut ErrorLog,
  ) -> (Arc<str>, Arc<RwLock<AbstractFunctionSignature>>) {
    let (original_name, scope_struct, original_implementation) = {
      let ancestor = ancestor.read().unwrap();
      let Some(scope_struct) = ancestor.captured_scope.clone() else {
        panic!("cloneify_captured_closure called on a scope-less closure")
      };
      let FunctionImplementationKind::Composite(implementation) =
        ancestor.implementation.clone()
      else {
        panic!("captured closure wasn't composite")
      };
      (ancestor.name.clone(), scope_struct, implementation)
    };
    if let Some(existing) = state.clones.get(&original_name) {
      return existing.clone();
    }
    let scope_struct_name = scope_struct.name.0.clone();
    let rewrites = self.lift_closure_entry_captures(
      &scope_struct,
      &scope_struct.source_trace,
      target,
      state,
      errors,
    );
    let mut implementation =
      original_implementation.read().unwrap().derived_from();
    implementation.expression.data.as_known_mut(|t| {
      let Type::Function(signature) = t else {
        panic!("captured closure had a non-function type")
      };
      if let Some((v, _)) = signature.args.last()
        && let Type::Struct(s) = v.var_type.unwrap_known()
        && s.name == scope_struct_name
      {
        signature.args.pop();
      }
    });
    let scope_param_name = implementation.arg_names.pop().unwrap().0;
    implementation.arg_annotations.pop();
    let ExpKind::Function(fn_arg_names, body) =
      &mut implementation.expression.kind
    else {
      panic!("captured closure implementation wasn't a Function")
    };
    fn_arg_names.pop();
    self.rewrite_dispatched_scope_body(body, &scope_param_name, &rewrites);
    let clone_name: Arc<str> = match target {
      ClosureLiftTarget::GpuDispatch => self
        .names
        .write()
        .unwrap()
        .gensym(&format!("{original_name}_gpu"))
        .into(),
      ClosureLiftTarget::Audio => {
        // Deterministic name — both runtimes derive the audio entry name
        // from the closure's ancestor at `start-audio` time.
        let clone_name: Arc<str> = format!("{original_name}_audio").into();
        assert!(
          !self.abstract_functions.contains_key(&clone_name),
          "audio clone name `{clone_name}` collides with an existing function"
        );
        self.names.write().unwrap().track_user_name(&clone_name);
        clone_name
      }
    };
    let clone_signature = {
      let original = ancestor.read().unwrap();
      let mut arg_types = original.arg_types.clone();
      if let Some((AbstractType::AbstractStruct(s), _)) = arg_types.last()
        && s.name.0 == scope_struct_name
      {
        arg_types.pop();
      }
      Arc::new(RwLock::new(AbstractFunctionSignature {
        name: clone_name.clone(),
        generic_args: vec![],
        arg_types,
        return_type: original.return_type.clone(),
        implementation: FunctionImplementationKind::Composite(Arc::new(
          RwLock::new(implementation),
        )),
        associative: false,
        captured_scope: None,
        entry_point: None,
      }))
    };
    state.new_functions.push(clone_signature.clone());
    state
      .clones
      .insert(original_name, (clone_name.clone(), clone_signature.clone()));
    (clone_name, clone_signature)
  }
  /// The unified context-exclusivity pass: one analysis of *which
  /// execution contexts each function can run in* (CPU, vertex, fragment,
  /// compute, audio), and one scan applying every context-exclusive rule
  /// against it. Contexts propagate the way effects do — through *calls*,
  /// never through function-value *references* — via a bitmask fixpoint
  /// over application-callee edges, seeded from every marked entry point,
  /// plus two host-invocation edges: `spawn-window`'s function argument
  /// runs in CPU context (the frame loop calls it every frame), and
  /// `start-audio`'s function argument runs in audio context. A closure
  /// that is merely constructed in one context and handed off through
  /// `start-audio` therefore never inherits the constructing context —
  /// while the constructor call itself, which genuinely runs where it is
  /// written, does. Each (function, context) pair records the call edge
  /// that introduced it and the entry point it traces back to, so every
  /// violation reports the offending call as its primary source position
  /// with that edge and the root entry as secondary positions.
  ///
  /// The rules:
  /// - fragment-exclusive builtins (`dpdx`, `texture-sample`, ...) and
  ///   `discard`: fragment context only
  /// - CPU-exclusive builtins (`print`, `spawn-window`, dispatches, ...):
  ///   CPU context only (with the runtime-sized-error suppression for the
  ///   dynamic-array constructors, which would otherwise double-report)
  /// - builtin-attribute lookups (`vertex-index`,
  ///   `global-invocation-id`, ...): contexts per
  ///   `BuiltinIOAttribute::is_valid_input_for_stage`
  /// - `audio-time`: audio context only (`sample-rate` is deliberately
  ///   unrestricted — the stream rate is knowable before any stream
  ///   exists, and closure constructors legitimately need it to size
  ///   delay buffers)
  ///
  /// Must run after implicit entry-point marking (dispatched GPU clones
  /// count as roots) and before `extract_audio_info` erases the
  /// audio-info calls. Known gap: a closure produced by a *helper* and
  /// handed to `start-audio` has no statically-known ancestor at the
  /// argument position, so its body runs unchecked in audio context
  /// (direct `(fn [] ...)` arguments are covered).
  /// Computes which execution contexts each composite function can run
  /// in, as a bitmask fixpoint over call edges plus the two
  /// host-invocation edges (see `validate_context_exclusivity` for the
  /// propagation model). Shared by the context-exclusivity validation
  /// (which also consumes the discovery edges for error traces) and
  /// target emission (which consults only the masks — see
  /// `compile_to_target`).
  pub(crate) fn compute_function_contexts(
    &self,
  ) -> HashMap<Arc<str>, FnContexts> {
    use crate::compiler::expression::ExpKind;
    use execution_context::{AUDIO, CONTEXT_COUNT, CPU, bit, context_of_entry};
    let mut fns: HashMap<Arc<str>, FnContexts> = HashMap::new();
    let mut worklist: Vec<(Arc<str>, u8)> = vec![];
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &f.implementation
      {
        let mask = f
          .entry_point
          .map(|e| bit(context_of_entry(&e)))
          .unwrap_or(0);
        if mask != 0 {
          worklist.push((f.name.clone(), mask));
        }
        fns.insert(
          f.name.clone(),
          FnContexts {
            implementation: implementation.clone(),
            mask,
            discovery: [const { None }; CONTEXT_COUNT],
          },
        );
      }
    }

    // fixpoint: propagate context bits along call edges (and the two
    // host-invocation edges), recording discovery edges for new bits
    while let Some((name, bits)) = worklist.pop() {
      let Some(info) = fns.get(&name) else {
        continue;
      };
      let implementation = info.implementation.clone();
      // (root per context: this fn's own root, or itself where it's an
      // entry)
      let roots: [Option<Arc<str>>; CONTEXT_COUNT] =
        std::array::from_fn(|context| {
          if info.mask & bit(context) == 0 {
            None
          } else {
            Some(match &info.discovery[context] {
              Some((_, root)) => root.clone(),
              None => name.clone(),
            })
          }
        });
      let mut propagations: Vec<(Arc<str>, u8, SourceTrace)> = vec![];
      implementation
        .read()
        .unwrap()
        .expression
        .walk(&mut |exp| {
          if let ExpKind::Application(applied_f, args) = &exp.kind {
            if let TypeState::Known(Type::Function(signature)) =
              &applied_f.data.kind
              && let Some(ancestor) = &signature.abstract_ancestor
            {
              let ancestor = ancestor.read().unwrap();
              if matches!(
                ancestor.implementation,
                FunctionImplementationKind::Composite(_)
              ) {
                propagations.push((
                  ancestor.name.clone(),
                  bits,
                  exp.source_trace.clone(),
                ));
              }
            }
            if let ExpKind::Name(applied_name) = &applied_f.kind
              && let Some(arg) = args.first()
              && let TypeState::Known(Type::Function(signature)) =
                &arg.data.kind
              && let Some(ancestor) = &signature.abstract_ancestor
            {
              // host-invocation edges: the frame loop calls
              // spawn-window's argument on the CPU; the audio thread
              // calls start-audio's argument
              let host_context = match &**applied_name {
                "spawn-window" => Some(CPU),
                "start-audio" => Some(AUDIO),
                _ => None,
              };
              if let Some(context) = host_context {
                let ancestor = ancestor.read().unwrap();
                if matches!(
                  ancestor.implementation,
                  FunctionImplementationKind::Composite(_)
                ) {
                  propagations.push((
                    ancestor.name.clone(),
                    bit(context),
                    exp.source_trace.clone(),
                  ));
                }
              }
            }
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
      for (callee, callee_bits, edge) in propagations {
        let Some(callee_info) = fns.get_mut(&callee) else {
          continue;
        };
        let new_bits = callee_bits & !callee_info.mask;
        if new_bits == 0 {
          continue;
        }
        callee_info.mask |= new_bits;
        for context in 0..CONTEXT_COUNT {
          if new_bits & bit(context) != 0 {
            let root = roots[context].clone().unwrap_or_else(|| name.clone());
            callee_info.discovery[context] = Some((edge.clone(), root));
          }
        }
        worklist.push((callee, new_bits));
      }
    }
    fns
  }
  pub fn validate_context_exclusivity(&self, errors: &mut ErrorLog) {
    use crate::compiler::expression::ExpKind;
    use execution_context::{
      AUDIO, CONTEXT_COUNT, CONTEXT_ENTRIES, CONTEXT_NAMES, CPU, FRAGMENT, bit,
    };
    let fns = self.compute_function_contexts();
    // rule scan: check each function's direct uses against every context
    // it can run in
    let runtime_sized_error_present = errors.iter().any(|e| {
      matches!(
        e.kind,
        GpuFunctionReturnsRuntimeSizedArray
          | GpuFunctionAcceptsRuntimeSizedArray
          | RuntimeSizedLocalInGpuCode
          | RuntimeSizedFieldOnGpu
          | NestedRuntimeSizedArrayBinding
      )
    });
    for (name, info) in fns.iter() {
      if info.mask == 0 {
        continue;
      }
      // (use, allowed-context mask, error kind per offending context)
      let mut violations: Vec<(
        SourceTrace,
        u8,
        [Option<CompileErrorKind>; CONTEXT_COUNT],
      )> = vec![];
      info
        .implementation
        .read()
        .unwrap()
        .expression
        .walk(&mut |exp| {
          match &exp.kind {
            ExpKind::Discard => {
              violations.push((
                exp.source_trace.clone(),
                bit(FRAGMENT),
                std::array::from_fn(|_| Some(DiscardOutsideFragment)),
              ));
            }
            ExpKind::Application(applied_f, args) => {
              if let ExpKind::Name(applied_name) = &applied_f.kind
                && &**applied_name == "audio-time"
                && args.is_empty()
              {
                violations.push((
                  exp.source_trace.clone(),
                  bit(AUDIO),
                  std::array::from_fn(|_| {
                    Some(AudioInfoOutsideAudio("audio-time".to_string()))
                  }),
                ));
              }
              // builtin callees carry their exclusivity in their effect
              // sets; composite callees are covered by their own scan
              if let TypeState::Known(Type::Function(signature)) =
                &applied_f.data.kind
                && let Some(ancestor) = &signature.abstract_ancestor
                && let FunctionImplementationKind::Builtin {
                  effect_type, ..
                } = &ancestor.read().unwrap().implementation
              {
                for effect in effect_type.0.iter() {
                  match effect {
                    // `print` is the one CPU-only builtin whose CPU-ness
                    // is signaled by its observable effect alone (every
                    // other CPU builtin pairs one with
                    // `CPUExclusiveFunction`)
                    Effect::Print => {
                      violations.push((
                        exp.source_trace.clone(),
                        bit(CPU),
                        std::array::from_fn(|context| {
                          if context == AUDIO {
                            Some(CPUExclusiveFunctionInAudioFunction(
                              "print".to_string(),
                            ))
                          } else {
                            Some(CPUExclusiveFunctionInGPUEntryPoint(
                              "print".to_string(),
                            ))
                          }
                        }),
                      ));
                    }
                    Effect::FragmentExclusiveFunction(fn_name) => {
                      violations.push((
                        exp.source_trace.clone(),
                        bit(FRAGMENT),
                        std::array::from_fn(|_| {
                          Some(FragmentExclusiveFunctionOutsideFragment(
                            fn_name.to_string(),
                          ))
                        }),
                      ));
                    }
                    Effect::CPUExclusiveFunction(fn_name) => {
                      // dynamic-array constructor misuse on the GPU is
                      // already reported precisely by the runtime-sized
                      // validation; the generic complaint would be
                      // redundant noise there (but audio context keeps
                      // its error — that validation is GPU-only)
                      let suppress_gpu = runtime_sized_error_present
                        && matches!(
                          &**fn_name,
                          "into-dynamic-array" | "zeroed-array"
                        );
                      violations.push((
                        exp.source_trace.clone(),
                        bit(CPU),
                        std::array::from_fn(|context| {
                          if context == AUDIO {
                            Some(CPUExclusiveFunctionInAudioFunction(
                              fn_name.to_string(),
                            ))
                          } else if suppress_gpu {
                            None
                          } else {
                            Some(CPUExclusiveFunctionInGPUEntryPoint(
                              fn_name.to_string(),
                            ))
                          }
                        }),
                      ));
                    }
                    Effect::LookupBuiltinAttribute(attribute) => {
                      let allowed = (0..CONTEXT_COUNT)
                        .filter(|context| {
                          attribute.is_valid_input_for_stage(
                            &CONTEXT_ENTRIES[*context],
                          )
                        })
                        .fold(0u8, |mask, context| mask | bit(context));
                      let attribute = *attribute;
                      violations.push((
                        exp.source_trace.clone(),
                        allowed,
                        std::array::from_fn(|context| {
                          Some(InvalidBuiltinForEntryPoint(
                            attribute.name().into(),
                            InputOrOutput::Input,
                            CONTEXT_NAMES[context].into(),
                          ))
                        }),
                      ));
                    }
                    _ => {}
                  }
                }
              }
            }
            _ => {}
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
      for (use_trace, allowed, kinds) in violations {
        for context in 0..CONTEXT_COUNT {
          if info.mask & bit(context) == 0 || allowed & bit(context) != 0 {
            continue;
          }
          let Some(kind) = kinds[context].clone() else {
            continue;
          };
          let mut trace = use_trace.clone();
          if let Some((edge, root)) = &info.discovery[context] {
            trace = trace.insert_as_secondary(edge.clone());
            if let Some(root_info) = fns.get(root)
              && root != name
            {
              trace = trace.insert_as_secondary(
                root_info
                  .implementation
                  .read()
                  .unwrap()
                  .name_source_trace
                  .clone(),
              );
            }
          }
          errors.log(CompileError::new(kind, trace));
        }
      }
    }
  }
  /// Rewrites every `sample-rate`/`audio-time` call into a read of a
  /// fixed-name implicit `@local` f32 var (`easl_sample_rate` /
  /// `easl_audio_time`, created on first use). The audio driver writes
  /// the audio replica's copies directly — the rate once per batch, the
  /// time before every sample (`VmAudioDriver::run_batch`; the C backend's
  /// generated entry wrapper stores both) — so no backend ever compiles
  /// the calls themselves. The names are deliberately fixed rather than
  /// gensym'd: the C audio wrapper is generated against a *separate*
  /// compilation of the same source, and gensym numbering isn't stable
  /// across compilations. `@local` keeps the vars out of the
  /// thread-sharing system for free (per-execution-context, driver- or
  /// nobody-authoritative).
  pub fn extract_audio_info(&mut self) {
    use crate::compiler::expression::ExpKind;
    let mut new_vars: Vec<TopLevelVar> = vec![];
    let mut created: HashSet<&'static str> = HashSet::new();
    for f in self.abstract_functions_iter() {
      let FunctionImplementationKind::Composite(implementation) =
        f.read().unwrap().implementation.clone()
      else {
        continue;
      };
      implementation
        .write()
        .unwrap()
        .expression
        .walk_mut(&mut |exp| {
          let ExpKind::Application(applied_f, args) = &exp.kind else {
            return Ok::<bool, Never>(true);
          };
          let ExpKind::Name(applied_name) = &applied_f.kind else {
            return Ok(true);
          };
          if !args.is_empty() {
            return Ok(true);
          }
          let var_name = match &**applied_name {
            "sample-rate" => "easl_sample_rate",
            "audio-time" => "easl_audio_time",
            _ => return Ok(true),
          };
          if created.insert(var_name) {
            new_vars.push(TopLevelVar {
              name: var_name.into(),
              kind: TopLevelVariableKind::Var {
                address_space: VariableAddressSpace::Local,
                group_and_binding: None,
              },
              var_type: Type::F32,
              value: None,
              source_trace: exp.source_trace.clone(),
              external: false,
            });
          }
          exp.kind = ExpKind::Name(var_name.into());
          exp.data.is_globally_bound = true;
          Ok(true)
        })
        .unwrap();
    }
    self.top_level_vars.extend(new_vars);
  }
  /// Rewrites `start-audio` calls whose function is a *scoped closure* so
  /// they can run on the audio thread: the closure chain is cloned into
  /// scope-less audio versions (`cloneify_captured_closure`) whose
  /// captures live in plain thread-shared globals, and the clone becomes
  /// the `@audio` entry point in place of the original (which the
  /// reference-address-space rebuild would drop from the registry — it
  /// has a non-owned trailing scope param). At runtime, `start-audio`
  /// seeds the lifted globals from the closure value's scope and starts
  /// the audio thread on the clone; the existing bootstrap force-publish
  /// ships the seeds to the audio replica's first adopt.
  pub fn extract_audio_closure_scopes(&mut self, errors: &mut ErrorLog) {
    let mut state = ClosureLiftState {
      created_globals: HashSet::new(),
      new_vars: vec![],
      clones: HashMap::new(),
      new_functions: vec![],
    };
    for f in self.abstract_functions_iter() {
      let FunctionImplementationKind::Composite(implementation) =
        f.read().unwrap().implementation.clone()
      else {
        continue;
      };
      implementation
        .write()
        .unwrap()
        .expression
        .walk_mut(&mut |exp| {
          let source_trace = exp.source_trace.clone();
          if let ExpKind::Application(applied_f, args) = &mut exp.kind
            && let ExpKind::Name(applied_f_name) = &applied_f.kind
            && &**applied_f_name == "start-audio"
          {
            let Type::Function(signature) = args[0].data.unwrap_known() else {
              return Ok::<bool, Never>(true);
            };
            let Some(ancestor) = signature.abstract_ancestor else {
              errors.log(CompileError::new(
                UnresolvableAudioFunction,
                source_trace,
              ));
              return Ok(true);
            };
            let Some(scope_struct) =
              ancestor.read().unwrap().captured_scope.clone()
            else {
              return Ok(true);
            };
            // The entry-marking pass marked the original closure; move
            // the marking to the audio clone, which is what actually
            // runs on the audio thread (and survives the
            // reference-address-space rebuild). Both steps are idempotent
            // (`cloneify_captured_closure` memoizes), so repeat call
            // sites of the same closure need no guard.
            {
              let mut ancestor = ancestor.write().unwrap();
              ancestor.entry_point = None;
              if let FunctionImplementationKind::Composite(implementation) =
                &ancestor.implementation
              {
                implementation.write().unwrap().entry_point = None;
              }
            }
            let (_, clone_signature) = self.cloneify_captured_closure(
              ancestor.clone(),
              ClosureLiftTarget::Audio,
              &mut state,
              errors,
            );
            {
              let mut clone = clone_signature.write().unwrap();
              clone.entry_point = Some(EntryPoint::Audio);
              if let FunctionImplementationKind::Composite(implementation) =
                &clone.implementation
              {
                implementation.write().unwrap().entry_point =
                  Some(EntryPoint::Audio);
              }
            }
            // Record the seed writes on this call site's callee: at
            // runtime the builtin copies the closure value's captured
            // fields into the lifted globals on the call that starts the
            // audio thread, and this private ancestor clone makes those
            // writes visible to the sharing-audience analysis as
            // `SeedsGlobalVar` effects — the standard analysis then sees
            // the main thread touching the lifted globals, with no
            // force-membership flag anywhere.
            let mut seeded_names: Vec<Arc<str>> = vec![];
            collect_audio_scope_global_names(&scope_struct, &mut seeded_names);
            applied_f.data.as_known_mut(|t| {
              if let Type::Function(callee_signature) = t
                && let Some(builtin_ancestor) =
                  &callee_signature.abstract_ancestor
              {
                let mut augmented = builtin_ancestor.read().unwrap().clone();
                if let FunctionImplementationKind::Builtin {
                  effect_type, ..
                } = &mut augmented.implementation
                {
                  for name in &seeded_names {
                    effect_type.merge(Effect::SeedsGlobalVar(name.clone()));
                  }
                }
                callee_signature.abstract_ancestor =
                  Some(Arc::new(RwLock::new(augmented)));
              }
            });
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
    let ClosureLiftState {
      new_vars,
      new_functions,
      ..
    } = state;
    self.top_level_vars.extend(new_vars);
    for f in new_functions {
      self.add_abstract_function(f);
    }
  }
  /// Rewrites every window-info query (`window-time`, `mouse-coords`,
  /// `key-down?`, etc.) into a read of an implicit uniform binding, creating
  /// one binding per distinct query (zero-arg kinds get one binding each;
  /// key queries get one binding per distinct compile-time key string). The
  /// runtime refreshes these bindings from the IO manager at the start of
  /// every frame (see `Program::window_info_bindings`), so every query —
  /// CPU- or GPU-side — reads the same per-frame snapshot of the ambient
  /// state, and the GPU code behaves exactly like the hand-written pattern
  /// of assigning `(window-time)` into a uniform in the frame loop.
  /// Rewriting unconditionally (rather than only in GPU-reachable
  /// functions) keeps the semantics local: whether some other call site
  /// dispatches a helper to the GPU never changes what the helper's CPU
  /// calls observe.
  ///
  /// The one exception is a key query whose argument isn't a string
  /// literal (e.g. a helper taking a `String` parameter): those can't be
  /// resolved to a binding at compile time, so they stay live CPU queries —
  /// and are rejected with a clear error if reachable from GPU code. Must
  /// run after implicit entry points are marked (so dispatched closures
  /// count as GPU roots for that check) and before WGSL emission.
  pub fn extract_gpu_window_info(&mut self) {
    let mut binding_names: HashMap<WindowInfoBindingSource, Arc<str>> =
      HashMap::new();
    let mut new_vars: Vec<TopLevelVar> = vec![];
    let not_equal_ancestor = self
      .abstract_functions
      .get("!=")
      .and_then(|signatures| signatures.first())
      .expect("builtin != missing from registry")
      .clone();
    let functions: Vec<Arc<RwLock<AbstractFunctionSignature>>> =
      self.abstract_functions_iter().cloned().collect();
    for f in functions {
      let FunctionImplementationKind::Composite(implementation) =
        f.read().unwrap().implementation.clone()
      else {
        continue;
      };
      implementation
        .write()
        .unwrap()
        .expression
        .walk_mut(&mut |exp| {
          let ExpKind::Application(applied_f, args) = &exp.kind else {
            return Ok::<bool, Never>(true);
          };
          let ExpKind::Name(applied_name) = &applied_f.kind else {
            return Ok(true);
          };
          let Some(kind) = WindowInfoKind::from_fn_name(applied_name) else {
            return Ok(true);
          };
          let source = match kind {
            WindowInfoKind::KeyDown | WindowInfoKind::KeyJustDown => {
              // Key queries need the key at compile time; non-literal args
              // stay live CPU queries (rejected below if GPU-reachable).
              let Some(ExpKind::StringLiteral(key)) =
                args.first().map(|arg| &arg.kind)
              else {
                return Ok(true);
              };
              let key: Arc<str> = key.clone();
              if kind == WindowInfoKind::KeyDown {
                WindowInfoBindingSource::KeyDown(key)
              } else {
                WindowInfoBindingSource::KeyJustDown(key)
              }
            }
            _ => WindowInfoBindingSource::Simple(kind),
          };
          let binding_name = binding_names
            .entry(source.clone())
            .or_insert_with(|| {
              let base_name = match &source {
                WindowInfoBindingSource::Simple(kind) => {
                  kind.binding_base_name().to_string()
                }
                WindowInfoBindingSource::KeyDown(key)
                | WindowInfoBindingSource::KeyJustDown(key) => {
                  let sanitized_key: String = key
                    .chars()
                    .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
                    .collect();
                  format!("{}_{}", kind.binding_base_name(), sanitized_key)
                }
              };
              let binding_name: Arc<str> =
                self.names.write().unwrap().gensym(&base_name);
              let var_type = if kind.is_boolean() {
                Type::U32
              } else {
                // The binding's type is the builtin's own return type.
                exp.data.unwrap_known()
              };
              new_vars.push(TopLevelVar {
                name: binding_name.clone(),
                kind: TopLevelVariableKind::Var {
                  address_space: VariableAddressSpace::Uniform,
                  // Binding numbers are assigned centrally at the end of
                  // validation.
                  group_and_binding: Some(BindingSpec::Elided),
                },
                var_type,
                value: None,
                source_trace: exp.source_trace.clone(),
                external: false,
              });
              binding_name
            })
            .clone();
          if kind.is_boolean() {
            // Bools aren't host-shareable in WGSL uniforms: the binding is
            // a u32 and the query becomes `(!= binding 0u)`.
            let u32_type: ExpTypeInfo = Type::U32.known().into();
            let mut binding_read_type = u32_type.clone();
            binding_read_type.is_globally_bound = true;
            exp.kind = ExpKind::Application(
              Box::new(Exp {
                data: Type::Function(Box::new(FunctionSignature {
                  abstract_ancestor: Some(not_equal_ancestor.clone()),
                  args: vec![
                    (Variable::immutable(u32_type.clone()), vec![]),
                    (Variable::immutable(u32_type.clone()), vec![]),
                  ],
                  return_type: Type::Bool.known().into(),
                }))
                .known()
                .into(),
                kind: ExpKind::Name("!=".into()),
                source_trace: exp.source_trace.clone(),
              }),
              vec![
                Exp {
                  data: binding_read_type,
                  kind: ExpKind::Name(binding_name),
                  source_trace: exp.source_trace.clone(),
                },
                Exp {
                  data: u32_type,
                  kind: ExpKind::NumberLiteral(Number::Int(0)),
                  source_trace: exp.source_trace.clone(),
                },
              ],
            );
          } else {
            exp.kind = ExpKind::Name(binding_name);
            exp.data.is_globally_bound = true;
          }
          Ok(true)
        })
        .unwrap();
    }
    self.top_level_vars.extend(new_vars);
    let mut recorded: Vec<(WindowInfoBindingSource, Arc<str>)> =
      binding_names.into_iter().collect();
    recorded.sort_by_key(|(source, _)| match source {
      WindowInfoBindingSource::Simple(kind) => (
        WindowInfoKind::ALL.iter().position(|k| k == kind).unwrap(),
        Arc::from(""),
      ),
      WindowInfoBindingSource::KeyDown(key) => (usize::MAX - 1, key.clone()),
      WindowInfoBindingSource::KeyJustDown(key) => (usize::MAX, key.clone()),
    });
    self.window_info_bindings = recorded;
  }
  /// Rejects any window-info effect still present in a GPU-reachable
  /// function after `extract_gpu_window_info`: that can only be a key query
  /// with a non-literal key, which has no binding to read from on the GPU.
  /// Runs after implicit entry points are marked, so dispatched closures
  /// count as GPU roots.
  pub fn validate_gpu_window_info(&mut self, errors: &mut ErrorLog) {
    let by_name: HashMap<Arc<str>, Arc<RwLock<AbstractFunctionSignature>>> =
      self
        .abstract_functions_iter()
        .map(|f| (f.read().unwrap().name.clone(), f.clone()))
        .collect();
    let mut reachable: HashSet<Arc<str>> = HashSet::new();
    let mut queue: Vec<Arc<RwLock<AbstractFunctionSignature>>> = self
      .abstract_functions_iter()
      .filter(|f| {
        matches!(
          f.read().unwrap().entry_point,
          Some(
            EntryPoint::Vertex | EntryPoint::Fragment | EntryPoint::Compute(_)
          )
        )
      })
      .cloned()
      .collect();
    for f in queue.iter() {
      reachable.insert(f.read().unwrap().name.clone());
    }
    while let Some(f) = queue.pop() {
      let FunctionImplementationKind::Composite(implementation) =
        f.read().unwrap().implementation.clone()
      else {
        continue;
      };
      let mut found: Vec<Arc<str>> = vec![];
      implementation
        .read()
        .unwrap()
        .expression
        .walk(&mut |exp| {
          if let ExpKind::Name(name) = &exp.kind
            && by_name.contains_key(name)
          {
            found.push(name.clone());
          }
          if let ExpKind::Application(applied_f, _) = &exp.kind
            && let TypeState::Known(Type::Function(signature)) =
              &applied_f.data.kind
            && let Some(ancestor) = &signature.abstract_ancestor
          {
            found.push(ancestor.read().unwrap().name.clone());
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
      for name in found {
        if let Some(target) = by_name.get(&name)
          && reachable.insert(name)
        {
          queue.push(target.clone());
        }
      }
    }
    for name in reachable {
      let Some(f) = by_name.get(&name) else {
        continue;
      };
      let FunctionImplementationKind::Composite(implementation) =
        f.read().unwrap().implementation.clone()
      else {
        continue;
      };
      let implementation = implementation.read().unwrap();
      let remaining = implementation.effects().window_info_kinds();
      for kind in remaining {
        errors.log(CompileError {
          kind: GpuKeyQueryRequiresLiteralString(kind.fn_name().to_string()),
          source_trace: implementation.expression.source_trace.clone(),
        });
      }
    }
  }
  /// Dispatched GPU closures (e.g. a lambda passed to
  /// `dispatch-compute-shader` that captured local variables) can't receive
  /// their captured scope as a function argument — WGSL entry points only
  /// accept builtin-annotated arguments. This pass converts each dispatched
  /// closure's scope argument into an implicit binding: the scope struct
  pub fn extract_dispatched_closure_scopes(&mut self, errors: &mut ErrorLog) {
    let mut state = ClosureLiftState {
      created_globals: HashSet::new(),
      new_vars: vec![],
      clones: HashMap::new(),
      new_functions: vec![],
    };
    let mut processed_entries: HashSet<Arc<str>> = HashSet::new();
    for f in self.abstract_functions_iter() {
      let FunctionImplementationKind::Composite(implementation) =
        f.read().unwrap().implementation.clone()
      else {
        continue;
      };
      implementation
        .read()
        .unwrap()
        .expression
        .walk(&mut |exp| {
          if let ExpKind::Application(applied_f, args) = &exp.kind
            && let ExpKind::Name(applied_f_name) = &applied_f.kind
          {
            let dispatched_fn_count = match &**applied_f_name {
              "dispatch-compute-shader" => 1,
              "dispatch-render-shaders" => 2,
              _ => 0,
            };
            for arg in args.iter().take(dispatched_fn_count) {
              let Type::Function(signature) = arg.data.unwrap_known() else {
                continue;
              };
              let Some(ancestor) = signature.abstract_ancestor else {
                continue;
              };
              let (entry_name, scope_struct, entry_implementation) = {
                let ancestor = ancestor.read().unwrap();
                let Some(scope_struct) = ancestor.captured_scope.clone() else {
                  continue;
                };
                let FunctionImplementationKind::Composite(implementation) =
                  ancestor.implementation.clone()
                else {
                  continue;
                };
                (ancestor.name.clone(), scope_struct, implementation)
              };
              let scope_struct_name = scope_struct.name.0.clone();
              // Drop the trailing scope arg from this call site's ancestor
              // signature and from every registry copy of the entry's
              // signature. The name-match guard makes this idempotent when
              // the same signature Arc is reachable through multiple paths.
              let mut signatures_to_patch = vec![ancestor.clone()];
              if let Some(registry_signatures) =
                self.abstract_functions.get(&entry_name)
              {
                signatures_to_patch.extend(registry_signatures.iter().cloned());
              }
              for signature in signatures_to_patch {
                let mut signature = signature.write().unwrap();
                if let Some((AbstractType::AbstractStruct(s), _)) =
                  signature.arg_types.last()
                  && s.name.0 == scope_struct_name
                {
                  signature.arg_types.pop();
                }
              }
              if !processed_entries.insert(entry_name.clone()) {
                continue;
              }
              let mut entry_implementation =
                entry_implementation.write().unwrap();
              let popped_scope_arg = {
                let mut popped = false;
                entry_implementation.expression.data.as_known_mut(|t| {
                  let Type::Function(signature) = t else {
                    panic!("dispatched closure had a non-function type")
                  };
                  if let Some((v, _)) = signature.args.last()
                    && let Type::Struct(s) = v.var_type.unwrap_known()
                    && s.name == scope_struct_name
                  {
                    signature.args.pop();
                    popped = true;
                  }
                });
                popped
              };
              if !popped_scope_arg {
                continue;
              }
              let scope_arg_name =
                entry_implementation.arg_names.pop().unwrap().0;
              entry_implementation.arg_annotations.pop();
              let ExpKind::Function(fn_arg_names, body) =
                &mut entry_implementation.expression.kind
              else {
                panic!("dispatched closure implementation wasn't a Function")
              };
              fn_arg_names.pop();
              // Each captured var becomes its own binding rather than one
              // scope-struct binding: a capture's type is then an ordinary
              // global type, so runtime-sized captures work like any other
              // storage-bound array — including several of them per
              // closure, which a single struct binding could never
              // express (WGSL allows at most one runtime-sized member,
              // and only in last position). Captured *closures* recurse:
              // their captures are lifted the same way and the body calls
              // a GPU clone (see `cloneify_captured_closure`).
              let rewrites = self.lift_closure_entry_captures(
                &scope_struct,
                &exp.source_trace,
                ClosureLiftTarget::GpuDispatch,
                &mut state,
                errors,
              );
              self.rewrite_dispatched_scope_body(
                body,
                &scope_arg_name,
                &rewrites,
              );
            }
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
    let ClosureLiftState {
      new_vars,
      new_functions,
      ..
    } = state;
    self.top_level_vars.extend(new_vars);
    for f in new_functions {
      self.add_abstract_function(f);
    }
  }
  pub fn monomorphize_reference_address_spaces(&mut self) {
    loop {
      let mut monomorphized_ctx = Program::default();
      monomorphized_ctx.names = RwLock::new(self.names.read().unwrap().clone());
      monomorphized_ctx.typedefs = self.typedefs.clone();
      let mut changed = false;
      for f in self.abstract_functions_iter() {
        let borrowed_f = f.read().unwrap();
        if let FunctionImplementationKind::Composite(implementation) =
          &f.read().unwrap().implementation
        {
          if borrowed_f.reference_arg_positions().is_empty() {
            let mut borrowed_implementation = implementation.write().unwrap();
            changed |= borrowed_implementation
              .expression
              .monomorphize_reference_address_spaces(
                &self,
                &mut monomorphized_ctx,
              );
            let mut new_f = (**f).read().unwrap().clone();
            new_f.implementation =
              FunctionImplementationKind::Composite(implementation.clone());
            drop(borrowed_implementation);
            monomorphized_ctx
              .add_abstract_function(Arc::new(RwLock::new(new_f)));
          }
        } else {
          monomorphized_ctx.add_abstract_function(f.clone());
        }
      }
      take(self, |old_ctx| {
        monomorphized_ctx.top_level_vars = old_ctx.top_level_vars;
        monomorphized_ctx.window_info_bindings = old_ctx.window_info_bindings;
        monomorphized_ctx
      });
      if !changed {
        break;
      }
    }
  }
  pub fn extract_builtin_attribute_lookup_functions(&mut self) {
    let mut used_attributes: HashSet<BuiltinIOAttribute> = HashSet::new();
    for f in self.abstract_functions_iter() {
      let borrowed_f = f.read().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &f.read().unwrap().implementation
      {
        let mut implementation = implementation.write().unwrap();
        let attributes =
          implementation.effects().looked_up_builtin_attributes();
        let Type::Function(signature) =
          implementation.expression.data.unwrap_known()
        else {
          panic!()
        };
        for attribute in attributes {
          used_attributes.insert(attribute);
          if borrowed_f.entry_point.is_some() {
            // (stage validity of the lookup was already checked by
            // `validate_context_exclusivity`; this pass only rewrites)
            let global_var_name = attribute.compiled_name();
            let value_type_info: ExpTypeInfo =
              attribute.value_type().known().into();
            let assignment_value = if let Some(arg_name) = implementation
              .arg_annotations
              .iter()
              .zip(implementation.arg_names.iter())
              .find_map(|(annotation, arg_name)| {
                if annotation.attributes.has_builtin_io_attribute(attribute) {
                  Some(arg_name.0.clone())
                } else {
                  None
                }
              }) {
              Exp {
                data: value_type_info.clone(),
                kind: ExpKind::Name(arg_name),
                source_trace: SourceTrace::empty(),
              }
            } else if let Some((struct_type, arg_name, field_name)) = signature
              .args
              .iter()
              .zip(implementation.arg_names.iter())
              .find_map(|((arg, _), (name, _))| {
                if let Type::Struct(s) = arg.var_type.unwrap_known() {
                  s.fields.iter().find_map(|field| {
                    if field.attributes.has_builtin_io_attribute(attribute) {
                      Some((s.clone(), name.clone(), field.name.clone()))
                    } else {
                      None
                    }
                  })
                } else {
                  None
                }
              })
            {
              Exp {
                data: value_type_info.clone(),
                kind: ExpKind::Access(
                  Accessor::Field(field_name),
                  Exp {
                    data: Type::Struct(struct_type).known().into(),
                    kind: ExpKind::Name(arg_name),
                    source_trace: SourceTrace::empty(),
                  }
                  .into(),
                ),
                source_trace: SourceTrace::empty(),
              }
            } else {
              let arg_name =
                self.names.write().unwrap().gensym(&global_var_name);
              implementation
                .arg_annotations
                .push(FunctionArgumentAnnotation {
                  var: false,
                  ownership: Ownership::Owned,
                  attributes: IOAttributes {
                    attributes: vec![IOAttribute {
                      kind: IOAttributeKind::Builtin(attribute),
                      source_trace: SourceTrace::empty(),
                    }],
                    attributed_source: SourceTrace::empty(),
                  },
                });
              let ExpKind::Function(args, _) =
                &mut implementation.expression.kind
              else {
                panic!()
              };
              args.push((arg_name.clone(), SourceTrace::empty()));
              implementation.expression.data.as_known_mut(|t| {
                let Type::Function(signature) = t else {
                  panic!()
                };
                signature.args.push((
                  Variable {
                    kind: VariableKind::Let,
                    var_type: attribute.value_type().known().into(),
                  },
                  vec![],
                ));
              });
              Exp {
                data: value_type_info.clone(),
                kind: ExpKind::Name(arg_name),
                source_trace: SourceTrace::empty(),
              }
            };
            let ExpKind::Function(_, body) =
              &mut implementation.expression.kind
            else {
              panic!()
            };
            *body = Exp {
              data: body.data.clone(),
              kind: ExpKind::Block(vec![
                Exp {
                  data: Type::Unit.known().into(),
                  kind: ExpKind::Application(
                    TypedExp::assignment_function(value_type_info.clone())
                      .into(),
                    vec![
                      Exp {
                        data: value_type_info.clone(),
                        kind: ExpKind::Name(global_var_name.into()),
                        source_trace: SourceTrace::empty(),
                      },
                      assignment_value,
                    ],
                  ),
                  source_trace: SourceTrace::empty(),
                },
                *body.clone(),
              ]),
              source_trace: body.source_trace.clone(),
            }
            .into();
          }
        }
      }
    }
    for attribute in used_attributes {
      self.top_level_vars.push(TopLevelVar {
        name: attribute.compiled_name().into(),
        kind: TopLevelVariableKind::Var {
          address_space: VariableAddressSpace::Local,
          group_and_binding: None,
        },
        var_type: attribute.value_type(),
        value: None,
        source_trace: SourceTrace::empty(),
        external: false,
      })
    }
  }
  pub fn propagate_abstract_function_signatures(&mut self) {
    loop {
      let mut changed = false;
      let copy_program = self.clone();
      for top_level_f in self.abstract_functions_iter() {
        let mut borrowed_f = top_level_f.write().unwrap();
        match &borrowed_f.implementation {
          FunctionImplementationKind::Composite(implementation) => {
            let mut borrowed_implementation = implementation.write().unwrap();
            borrowed_implementation
              .expression
              .walk_mut_with_ctx(
                &mut |exp, ctx| {
                  exp.data.as_known_mut(|t| {
                    if let Type::Function(f) = t
                      && f.abstract_ancestor.is_none()
                    {
                      match &exp.kind {
                        ExpKind::Name(name) => {
                          if let Some((v, _)) = ctx.variables.get(name)
                            && let Type::Function(bound_f) =
                              v.var_type.unwrap_known()
                            && let Some(abstract_ancestor) =
                              bound_f.abstract_ancestor
                          {
                            f.abstract_ancestor = Some(abstract_ancestor);
                            changed = true;
                          }
                        }
                        ExpKind::Application(applied_f_exp, _) => {
                          if let ExpKind::Name(applied_f_name) =
                            &applied_f_exp.kind
                            && let Type::Function(applied_f_sig) =
                              applied_f_exp.data.unwrap_known()
                            && applied_f_sig.abstract_ancestor.is_some()
                            && let Type::Function(_) =
                              applied_f_sig.return_type.unwrap_known()
                            && let Some(signatures) =
                              self.abstract_functions.get(applied_f_name)
                            && let Some(signature) = signatures.get(0)
                            && let AbstractType::Type(Type::Function(
                              returned_f,
                            )) = &signature.read().unwrap().return_type
                            && let Some(returned_abstract_ancestor) =
                              &returned_f.abstract_ancestor
                          {
                            f.abstract_ancestor =
                              Some(returned_abstract_ancestor.clone());
                            changed = true;
                          }
                        }
                        _ => {}
                      }
                    }
                    Ok::<bool, Never>(true)
                  })
                },
                &mut ImmutableProgramLocalContext::empty(&copy_program),
              )
              .unwrap();
            borrowed_implementation
              .expression
              .walk_mut(&mut |exp| {
                exp.data.as_known_mut(|t| {
                  if let Type::Function(f) = t
                    && f.abstract_ancestor.is_none()
                    && let Some(inner_exp) = match &exp.kind {
                      ExpKind::Let(_, body) => Some(body.as_ref()),
                      ExpKind::Block(exps) => exps.last(),
                      _ => None,
                    }
                    && let Type::Function(inner_f) =
                      inner_exp.data.unwrap_known()
                    && let Some(inner_abstract_ancestor) =
                      inner_f.abstract_ancestor
                  {
                    f.abstract_ancestor = Some(inner_abstract_ancestor.clone());
                    changed = true;
                  }
                });
                Ok::<bool, Never>(true)
              })
              .unwrap();
            let inner_abstract_ancestor = if let ExpKind::Function(_, body) =
              &borrowed_implementation.expression.kind
              && let Type::Function(inner_f) = body.data.unwrap_known()
              && let Some(ancestor) = inner_f.abstract_ancestor
            {
              Some(ancestor)
            } else {
              None
            };
            borrowed_implementation
              .expression
              .data
              .with_dereferenced_mut(|ts| {
                if let TypeState::Known(Type::Function(signature)) = ts {
                  signature.return_type.with_dereferenced_mut(|rt| {
                    if let TypeState::Known(Type::Function(return_signature)) =
                      rt
                      && let Some(anc) = inner_abstract_ancestor.clone()
                      && return_signature.abstract_ancestor.is_none()
                    {
                      return_signature.abstract_ancestor = Some(anc);
                      changed = true;
                    }
                  });
                }
              });
            drop(borrowed_implementation);
            if let AbstractType::Type(Type::Function(f)) =
              &mut borrowed_f.return_type
              && f.abstract_ancestor.is_none()
            {
              if let Some(inner_abstract_ancestor) = inner_abstract_ancestor {
                f.abstract_ancestor = Some(inner_abstract_ancestor);
                changed = true;
              }
            }
          }
          _ => {}
        }
      }
      if !changed {
        break;
      }
    }
  }
  pub fn inline_local_bound_function_applications(&mut self) {
    let mut representative_structs = vec![];
    for f in self.abstract_functions_iter() {
      let borrowed_f = f.read().unwrap();
      match &borrowed_f.implementation {
        FunctionImplementationKind::Composite(implementation) => implementation
          .write()
          .unwrap()
          .expression
          .walk_mut_with_ctx(
            &mut |exp, ctx| match &mut exp.kind {
              ExpKind::Application(f, args) => {
                if let Type::Function(_) = f.data.unwrap_known() {
                  // Applying a closure held in a captured-scope field — the
                  // form a nested closure's inner call takes after
                  // extract_inner_functions rewrites capture references:
                  // `((. scope inner) args...)`. Same rewrite as the
                  // local-binding case below, with the Access expression
                  // itself appended as the trailing scope argument:
                  // `(inner_fn args... (. scope inner))`.
                  if let ExpKind::Access(Accessor::Field(_), _) = &f.kind {
                    // The Access expression's own function type never gets
                    // an abstract ancestor (the signature-propagation pass
                    // only fills Name and Application expressions) — the
                    // ancestor lives on the scope struct's field type.
                    let ancestor = {
                      let ExpKind::Access(
                        Accessor::Field(field_name),
                        accessed,
                      ) = &f.kind
                      else {
                        unreachable!()
                      };
                      if let Type::Struct(s) = accessed.data.unwrap_known() {
                        s.fields
                          .iter()
                          .find(|field| field.name == *field_name)
                          .and_then(|field| {
                            match field.field_type.unwrap_known() {
                              Type::Function(sig) => {
                                sig.abstract_ancestor.clone()
                              }
                              _ => None,
                            }
                          })
                      } else {
                        None
                      }
                    };
                    if let Some(ancestor) = ancestor {
                      let abstract_fn = ancestor.read().unwrap();
                      let new_name = abstract_fn.name.clone();
                      if let Some(captured_scope) =
                        abstract_fn.captured_scope.as_ref()
                      {
                        let scope_type = Type::Struct(
                          AbstractStruct::concretize(
                            Arc::new(captured_scope.clone()),
                            &self.typedefs,
                            &vec![],
                            f.source_trace.clone(),
                          )
                          .unwrap(),
                        );
                        let mut scope_arg = (**f).clone();
                        scope_arg.data = scope_type.clone().known().into();
                        // The scope argument is a reference-rooted place
                        // (its root is the enclosing closure's scope
                        // param) — the backends' argument emission keys
                        // explicit derefs off this ownership.
                        scope_arg.data.ownership = Ownership::MutableReference;
                        args.push(scope_arg);
                        // Keep the callee's static signature in sync with
                        // the appended argument, so per-arg ownership zips
                        // downstream (interpreter write-back, reference
                        // address-space monomorphization) see the scope
                        // param.
                        let mut var_type: ExpTypeInfo =
                          scope_type.known().into();
                        var_type.ownership = Ownership::MutableReference;
                        if let TypeState::Known(Type::Function(sig)) =
                          &mut f.data.kind
                        {
                          sig.args.push((
                            Variable {
                              kind: VariableKind::Var,
                              var_type,
                            },
                            vec![],
                          ));
                        }
                        representative_structs.push(captured_scope.clone());
                      }
                      drop(abstract_fn);
                      // Stamp the ancestor onto the rewritten callee — the
                      // HoF inliner, effects computation, and the backends
                      // all read it.
                      if let TypeState::Known(Type::Function(sig)) =
                        &mut f.data.kind
                      {
                        sig.abstract_ancestor = Some(ancestor);
                      }
                      f.kind = ExpKind::Name(new_name);
                    }
                    return Ok(true);
                  }
                  let ExpKind::Name(original_name) = &mut f.kind else {
                    panic!(
                      "non-name fn being applied: callee kind = {:?}",
                      f.kind
                    )
                  };
                  match ctx.get_name_definition_source(&original_name) {
                    Some(source) => match source {
                      NameDefinitionSource::LocalBinding(_) => {
                        let Type::Function(bound_signature) = ctx
                          .variables
                          .get(original_name)
                          .unwrap()
                          .0
                          .var_type
                          .unwrap_known()
                        else {
                          panic!()
                        };
                        if let Some(abstract_fn) =
                          bound_signature.abstract_ancestor
                        {
                          let abstract_fn = abstract_fn.read().unwrap();
                          let new_name = abstract_fn.name.clone();
                          if let Some(captured_scope) =
                            abstract_fn.captured_scope.as_ref()
                          {
                            args.push(Exp {
                              data: Type::Struct(
                                AbstractStruct::concretize(
                                  Arc::new(captured_scope.clone()),
                                  &self.typedefs,
                                  &vec![],
                                  f.source_trace.clone(),
                                )
                                .unwrap(),
                              )
                              .known()
                              .into(),
                              kind: ExpKind::Name(original_name.clone()),
                              source_trace: f.source_trace.clone(),
                            });
                            representative_structs.push(captured_scope.clone());
                          }
                          *original_name = new_name;
                        }
                      }
                      _ => {}
                    },
                    None => {}
                  }
                }
                Ok(true)
              }
              _ => Ok::<bool, Never>(true),
            },
            &mut ImmutableProgramLocalContext::empty(self),
          )
          .unwrap(),
        _ => {}
      }
    }
    for s in representative_structs {
      self.add_monomorphized_struct(s);
    }
  }
  pub fn catch_duplicate_closures_capturing_mutable_variables(
    &mut self,
    errors: &mut ErrorLog,
  ) {
    for f in self.abstract_functions_iter() {
      let borrowed_f = f.read().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &borrowed_f.implementation
      {
        let exp = &mut implementation.write().unwrap().expression;
        let ExpKind::Function(_, body) = &mut exp.kind else {
          panic!()
        };
        body.catch_duplicate_closures_capturing_mutable_variables(self, errors);
      }
    }
  }
  pub fn extract_inner_functions(&mut self, errors: &mut ErrorLog) -> bool {
    let mut any_extracted = false;
    loop {
      let mut new_signatures: Vec<AbstractFunctionSignature> = vec![];
      let mut new_structs: Vec<AbstractStruct> = vec![];
      for f in self.abstract_functions_iter() {
        let borrowed_f = f.read().unwrap();
        if !borrowed_f.generic_args.is_empty()
          || borrowed_f.has_uninlined_higher_order_arguments()
        {
          continue;
        }
        match &borrowed_f.implementation {
          FunctionImplementationKind::Composite(implementation) => {
            let mut root_encountered = false;
            implementation
              .write()
              .unwrap()
              .expression
              .walk_mut_with_ctx(
                &mut |exp, ctx| {
                  if !root_encountered {
                    root_encountered = true;
                    return Ok(true);
                  }
                  let effects = exp.effects();
                  if let ExpKind::Function(arg_names, body) = &mut exp.kind {
                    // If any captured variable is a function whose abstract
                    // ancestor hasn't been resolved yet, defer extraction until
                    // propagate_abstract_function_signatures has set it.
                    if effects.0.iter().any(|e| {
                      if let Effect::ReadsVar(var_name) = e
                        && let Some((var, _)) = ctx.variables.get(var_name)
                        && let Type::Function(f) = var.var_type.unwrap_known()
                      {
                        f.abstract_ancestor.is_none()
                      } else {
                        false
                      }
                    }) {
                      return Ok(true);
                    }
                    let name = self.names.write().unwrap().gensym("inner_fn");
                    let Type::Function(f_signature) = exp.data.unwrap_known()
                    else {
                      panic!()
                    };
                    let mut unitlike_fn_substitutions: HashMap<
                      Arc<str>,
                      (Arc<str>, Arc<RwLock<AbstractFunctionSignature>>),
                    > = HashMap::new();
                    let captured_vars: Vec<(&Arc<str>, Type, Ownership)> =
                      effects
                        .0
                        .iter()
                        .map(|e| match e {
                          Effect::ReadsVar(var_name)
                          | Effect::ReadsArrayLength(var_name) => {
                            Ok(match ctx.variables.get(var_name) {
                              Some((var, _)) => {
                                let var_type = var.var_type.unwrap_known();
                                if matches!(var_type, Type::Function(_))
                                  && var_type.is_unitlike(
                                    &mut *self.names.write().unwrap(),
                                  )
                                {
                                  if let Type::Function(sig) = var_type
                                    && let Some(ancestor) =
                                      &sig.abstract_ancestor
                                  {
                                    unitlike_fn_substitutions.insert(
                                      var_name.clone(),
                                      (
                                        ancestor.read().unwrap().name.clone(),
                                        ancestor.clone(),
                                      ),
                                    );
                                  }
                                  None
                                } else {
                                  Some((
                                    var_name,
                                    var.var_type.unwrap_known(),
                                    var.var_type.ownership,
                                  ))
                                }
                              }
                              None => None,
                            })
                          }

                          Effect::ModifiesLocalVar(_)
                          | Effect::CPUExclusiveFunction(_)
                          | Effect::CPUExclusiveType(_)
                          | Effect::WindowInfo(_)
                          | Effect::FragmentExclusiveFunction(_)
                          | Effect::Print
                          | Effect::FileWrite
                          | Effect::ModifiesGlobalVar(_)
                          | Effect::Window
                          | Effect::LookupBuiltinAttribute(_)
                          | Effect::InvokesUnknownFunction => Ok(None),
                          _ => err(
                            IllegalEffectsInClosure(format!("{e:?}")),
                            body.source_trace.clone(),
                          ),
                        })
                        .collect::<CompileResult<Vec<_>>>()
                        .unwrap_or_else(|e| {
                          errors.log(e);
                          vec![]
                        })
                        .into_iter()
                        .filter_map(|x| x)
                        .collect();
                    // A variable read both by element access and by
                    // `array-length` contributes two effects — capture it
                    // only once.
                    let mut seen_captured_names: HashSet<&Arc<str>> =
                      HashSet::new();
                    let captured_vars: Vec<(&Arc<str>, Type, Ownership)> =
                      captured_vars
                        .into_iter()
                        .filter(|(name, _, _)| seen_captured_names.insert(name))
                        .collect();
                    let captured_scope = if captured_vars.is_empty() {
                      None
                    } else {
                      Some(AbstractStruct {
                        name: (
                          self
                            .names
                            .write()
                            .unwrap()
                            .gensym(&format!("{name}_scope"))
                            .into(),
                          exp.source_trace.clone(),
                        ),
                        filled_generics: HashMap::new(),
                        fields: captured_vars
                          .iter()
                          .map(|(name, t, _)| AbstractStructField {
                            attributes: IOAttributes::empty(
                              exp.source_trace.clone(),
                            ),
                            name: (**name).clone(),
                            field_type: AbstractType::Type(t.clone()),
                            source_trace: exp.source_trace.clone(),
                          })
                          .collect(),
                        generic_args: vec![],
                        abstract_ancestor: None,
                        source_trace: exp.source_trace.clone(),
                        opaque: false,
                      })
                    };
                    let mut arg_types: Vec<(AbstractType, Ownership)> =
                      f_signature
                        .args
                        .iter()
                        .map(|(arg, _)| {
                          (
                            AbstractType::Type(arg.var_type.unwrap_known()),
                            Ownership::Owned,
                          )
                        })
                        .collect();
                    let captured_scope = captured_scope.map(|captured_scope| {
                      (
                        captured_scope.clone(),
                        AbstractType::AbstractStruct(Arc::new(captured_scope))
                          .concretize(
                            &vec![],
                            &self.typedefs,
                            exp.source_trace.clone(),
                          )
                          .unwrap(),
                        self.names.write().unwrap().gensym("scope"),
                      )
                    });
                    if let Some((
                      captured_scope,
                      concrete_captured_scope_type,
                      scope_name,
                    )) = &captured_scope
                    {
                      arg_names
                        .push((scope_name.clone(), exp.source_trace.clone()));
                      exp.data.as_known_mut(|t| {
                        let Type::Function(f) = t else {
                          panic!();
                        };
                        let mut var_type: ExpTypeInfo =
                          concrete_captured_scope_type.clone().known().into();
                        var_type.ownership = Ownership::MutableReference;
                        f.args.push((
                          Variable {
                            kind: VariableKind::Var,
                            var_type,
                          },
                          vec![],
                        ));
                      });
                      arg_types.push((
                        AbstractType::AbstractStruct(Arc::new(
                          captured_scope.clone(),
                        )),
                        Ownership::MutableReference,
                      ));
                    }
                    let signature = AbstractFunctionSignature {
                      name: name.clone(),
                      generic_args: vec![],
                      associative: false,
                      entry_point: None,
                      arg_types,
                      return_type: AbstractType::Type(
                        f_signature.return_type.unwrap_known(),
                      ),
                      implementation: FunctionImplementationKind::Composite(
                        Arc::new(RwLock::new(TopLevelFunction {
                          name_source_trace: exp.source_trace.clone(),
                          arg_names: arg_names.clone(),
                          arg_annotations: arg_names
                            .iter()
                            .map(|(_, arg_source_trace)| {
                              FunctionArgumentAnnotation::empty(
                                arg_source_trace.clone(),
                              )
                            })
                            .collect(),
                          return_attributes: IOAttributes::empty(
                            exp.source_trace.clone(),
                          ),
                          entry_point: None,
                          directly_user_written: false,
                          expression: {
                            let mut new_exp = exp.clone();
                            if !unitlike_fn_substitutions.is_empty() {
                              let ExpKind::Function(_, body) =
                                &mut new_exp.kind
                              else {
                                panic!()
                              };
                              body
                                .walk_mut(&mut |e| -> Result<bool, Never> {
                                  if let ExpKind::Name(name) = &mut e.kind {
                                    if let Some((concrete_name, signature)) =
                                      unitlike_fn_substitutions
                                        .get(name.as_ref())
                                    {
                                      *name = concrete_name.clone();
                                      e.data.as_known_mut(|t| {
                                        if let Type::Function(f) = t {
                                          f.abstract_ancestor =
                                            Some(signature.clone());
                                        }
                                      });
                                    }
                                  }
                                  Ok(true)
                                })
                                .unwrap();
                            }
                            if let Some((
                              _,
                              concrete_captured_scope_type,
                              scope_name,
                            )) = &captured_scope
                            {
                              let ExpKind::Function(_, body) =
                                &mut new_exp.kind
                              else {
                                panic!()
                              };
                              body
                                .walk_mut(&mut |e| {
                                  if let ExpKind::Name(name) = &mut e.kind {
                                    if captured_vars
                                      .iter()
                                      .any(|(arg_name, _, _)| *arg_name == name)
                                    {
                                      let name = name.clone();
                                      let mut t: ExpTypeInfo =
                                        concrete_captured_scope_type
                                          .clone()
                                          .known()
                                          .into();
                                      t.ownership = Ownership::MutableReference;
                                      e.kind = ExpKind::Access(
                                        Accessor::Field(name.clone()),
                                        Box::new(Exp {
                                          data: t,
                                          kind: ExpKind::Name(
                                            scope_name.clone(),
                                          ),
                                          source_trace: exp
                                            .source_trace
                                            .clone(),
                                        }),
                                      );
                                    }
                                  }
                                  Ok::<bool, Never>(true)
                                })
                                .unwrap();
                            }
                            new_exp
                          },
                        })),
                      ),
                      captured_scope: captured_scope
                        .as_ref()
                        .map(|(s, _, _)| s.clone()),
                    };
                    new_signatures.push(signature.clone());
                    if let Some((s, _, _)) = &captured_scope {
                      new_structs.push(s.clone());
                    }
                    *exp = Exp {
                      data: Type::Function(Box::new(FunctionSignature {
                        abstract_ancestor: Some(Arc::new(RwLock::new(
                          signature,
                        ))),
                        args: f_signature.args,
                        return_type: f_signature.return_type,
                      }))
                      .known()
                      .into(),
                      kind: if let Some((
                        captured_scope,
                        concrete_captured_scope_type,
                        _,
                      )) = captured_scope
                      {
                        ExpKind::Application(
                          Box::new(Exp {
                            data: Type::Function(Box::new(FunctionSignature {
                              // The scope construction is, at the value
                              // level, a construction of the scope struct —
                              // its callee gets the struct's constructor as
                              // an explicit ancestor, exactly like any other
                              // struct-constructor application. (The
                              // closure-ness of the node lives in the
                              // expression's own type: a function type whose
                              // ancestor is the extracted inner fn.)
                              abstract_ancestor: Some(Arc::new(RwLock::new(
                                AbstractFunctionSignature {
                                  name: captured_scope.name.0.clone(),
                                  generic_args: vec![],
                                  arg_types: captured_vars
                                    .iter()
                                    .map(|(_, t, _)| {
                                      (
                                        AbstractType::Type(t.clone()),
                                        Ownership::Owned,
                                      )
                                    })
                                    .collect(),
                                  return_type: AbstractType::Type(
                                    concrete_captured_scope_type.clone(),
                                  ),
                                  implementation:
                                    FunctionImplementationKind::StructConstructor,
                                  associative: false,
                                  captured_scope: None,
                                  entry_point: None,
                                },
                              ))),
                              args: captured_vars
                                .iter()
                                .map(|(_, t, _)| {
                                  (
                                    Variable {
                                      kind: VariableKind::Let,
                                      var_type: t.clone().known().into(),
                                    },
                                    vec![],
                                  )
                                })
                                .collect(),
                              return_type: exp.data.clone(),
                            }))
                            .known()
                            .into(),
                            kind: ExpKind::Name(captured_scope.name.0.clone()),
                            source_trace: exp.source_trace.clone(),
                          }),
                          captured_vars
                            .into_iter()
                            .map(|(name, t, ownership)| {
                              let mut data: ExpTypeInfo = t.known().into();
                              data.ownership = ownership;
                              Exp {
                                data,
                                kind: ExpKind::Name(name.clone()),
                                source_trace: exp.source_trace.clone(),
                              }
                            })
                            .collect(),
                        )
                      } else {
                        ExpKind::Name(name)
                      },
                      source_trace: exp.source_trace.clone(),
                    };

                    Ok(true)
                  } else {
                    Ok::<bool, Never>(true)
                  }
                },
                &mut ImmutableProgramLocalContext::empty(self),
              )
              .unwrap();
          }
          _ => {}
        }
      }
      if new_signatures.is_empty() {
        break;
      }
      any_extracted = true;
      for s in new_signatures {
        self.add_abstract_function(Arc::new(RwLock::new(s)));
      }
      for s in new_structs {
        self.add_monomorphized_struct(s);
      }
    }
    any_extracted
  }
  pub fn inline_all_higher_order_arguments(
    &mut self,
    errors: &mut ErrorLog,
  ) -> bool {
    let mut any_inlined = false;
    loop {
      let changed = self.inline_higher_order_arguments(errors);
      if !errors.is_empty() || !changed {
        break;
      }
      any_inlined = true;
    }
    any_inlined
  }
  pub fn inline_higher_order_arguments(
    &mut self,
    errors: &mut ErrorLog,
  ) -> bool {
    let mut changed = false;
    let mut inlined_ctx = Program::default();
    inlined_ctx.names = RwLock::new(self.names.read().unwrap().clone());
    inlined_ctx.typedefs = self.typedefs.clone();
    for f in self.abstract_functions_iter() {
      let borrowed_f = f.read().unwrap();
      if !borrowed_f.has_uninlined_higher_order_arguments() {
        match &borrowed_f.implementation {
          FunctionImplementationKind::Composite(implementation) => {
            let mut borrowed_implementation = implementation.write().unwrap();
            match borrowed_implementation
              .expression
              .inline_higher_order_arguments(&mut inlined_ctx)
            {
              Ok(added_new_function) => {
                changed |= added_new_function;
                let mut new_f = borrowed_f.clone();
                drop(borrowed_implementation);
                new_f.implementation =
                  FunctionImplementationKind::Composite(implementation.clone());
                inlined_ctx.add_abstract_function(Arc::new(RwLock::new(new_f)));
              }
              Err(e) => errors.log(e),
            }
          }
          FunctionImplementationKind::EnumConstructor(_) => {
            inlined_ctx.add_abstract_function(Arc::clone(f));
          }
          _ => {}
        }
      }
    }
    take(self, |old_ctx| {
      inlined_ctx.top_level_vars = old_ctx.top_level_vars;
      inlined_ctx.window_info_bindings = old_ctx.window_info_bindings;
      inlined_ctx
    });
    changed
  }
  pub fn remove_unitlike_values(&mut self) {
    let mut names = NameContext::empty();
    std::mem::swap(&mut names, &mut self.names.write().unwrap());
    take(&mut self.typedefs.structs, |structs| {
      structs
        .into_iter()
        .filter(|s| !s.is_unitlike(&mut names))
        .collect()
    });
    for f in self.abstract_functions_iter_mut() {
      let f = f.write().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &f.implementation
      {
        let mut implementation = implementation.write().unwrap();
        implementation
          .expression
          .walk_mut(&mut |exp| match &mut exp.kind {
            ExpKind::Name(_) => {
              if exp.data.unwrap_known() == Type::Unit {
                exp.kind = ExpKind::Unit;
              }
              Ok(true)
            }
            ExpKind::Application(applied_f, args) => {
              applied_f.data.with_dereferenced_mut(|t| match t {
                TypeState::Known(t) => match t {
                  Type::Function(applied_f_signature) => {
                    let mut args_to_remove = vec![];
                    if let Some(applied_f_abstract_signature) =
                      &mut applied_f_signature.abstract_ancestor
                    {
                      let cloned_sig =
                        applied_f_abstract_signature.read().unwrap().clone();
                      let composite_f =
                        if let FunctionImplementationKind::Composite(ref f) =
                          cloned_sig.implementation
                        {
                          Some(Arc::clone(f))
                        } else {
                          None
                        };
                      if let Some(f) = composite_f {
                        args_to_remove = (0..args.len())
                          .rev()
                          .filter(|i| {
                            args[*i].data.unwrap_known().is_unitlike(&mut names)
                          })
                          .collect();
                        let new_sig = Arc::new(RwLock::new(cloned_sig));
                        new_sig
                          .write()
                          .unwrap()
                          .remove_unitlike_arguments(&mut names);
                        applied_f_signature.abstract_ancestor =
                          Some(Arc::clone(&new_sig));
                        let mut f = f.write().unwrap();
                        for i in args_to_remove.iter() {
                          f.arg_names.remove(*i);
                          f.arg_annotations.remove(*i);
                        }
                      } else {
                        args_to_remove = (0..args.len())
                          .rev()
                          .filter(|i| {
                            let arg_type = args[*i].data.unwrap_known();
                            if matches!(arg_type, Type::Function(_)) {
                              false
                            } else {
                              arg_type.is_unitlike(&mut names)
                            }
                          })
                          .collect();
                      }
                    }
                    for i in args_to_remove {
                      args.remove(i);
                      applied_f_signature.args.remove(i);
                    }
                  }
                  _ => {}
                },
                _ => {}
              });

              Ok(true)
            }
            ExpKind::Let(bindings, _) => {
              take(bindings, |bindings| {
                bindings
                  .into_iter()
                  .filter(|(_, _, _, value_exp)| {
                    !(value_exp.data.unwrap_known().is_unitlike(&mut names)
                      && value_exp.effects().is_side_effect_free())
                  })
                  .collect()
              });
              Ok(true)
            }
            _ => Ok::<bool, Never>(true),
          })
          .unwrap();
      }
    }
    std::mem::swap(&mut names, &mut self.names.write().unwrap());
  }
  pub fn compile_to_target(
    self,
    target: CompilerTarget,
  ) -> CompileResult<String> {
    let mut names = self.names.write().unwrap();
    let mut compiled_string = target.program_header();
    for v in self.top_level_vars.iter() {
      // A runtime-sized array without a GPU binding is a CPU/audio-only
      // value (e.g. a `load-wav`ed sample buffer): WGSL has no
      // representation for an unsized array outside a storage binding, so
      // it's omitted from shader output. (Shader code can't legally
      // reference one anyway.)
      if target == CompilerTarget::WGSL
        && matches!(
          &v.var_type,
          Type::Array(
            Some(crate::compiler::types::ConcreteArraySize::Unsized),
            _
          )
        )
        && !matches!(
          v.kind,
          TopLevelVariableKind::Var {
            group_and_binding: Some(_),
            ..
          }
        )
      {
        continue;
      }
      // A GPU-space var whose type isn't host-shareable (bool- or
      // String-containing) is guaranteed GPU-unused by
      // `validate_gpu_used_binding_types` — it's an ordinary CPU value
      // with no legal WGSL declaration, so it's omitted from shader
      // output. (`@local` bool vars stay: `var<private> b: bool` is
      // valid WGSL.)
      if target == CompilerTarget::WGSL
        && matches!(
          v.kind,
          TopLevelVariableKind::Var {
            address_space: VariableAddressSpace::Uniform
              | VariableAddressSpace::StorageRead
              | VariableAddressSpace::StorageReadWrite,
            ..
          }
        )
        && (v.var_type.involves_bool() || v.var_type.involves_string())
      {
        continue;
      }
      // Strings exist only on the CPU runtimes' heaps — a String-typed
      // global has no C representation at all.
      if target == CompilerTarget::C && v.var_type.involves_string() {
        continue;
      }
      compiled_string += &v.clone().compile(&mut names, target);
      compiled_string += ";\n";
    }
    compiled_string += "\n";
    let default_structs = built_in_structs_for_target(target);
    for s in self.typedefs.structs.iter() {
      // Structs with runtime-sized or String fields are CPU-only values
      // (e.g. a dispatched closure's scope struct with runtime-sized
      // captures, which lives on in the typedefs even though its captures
      // travel as per-field bindings) — skip their emission entirely.
      // Function-typed fields emit as their representative scope-struct
      // names, so the check recurses through captured scopes: a scope
      // struct embedding a CPU-only closure is itself CPU-only.
      // Validation guarantees GPU code can't reference one: such types
      // are banned both as binding types (`RuntimeSizedFieldInBinding`)
      // and as values in GPU code (`RuntimeSizedFieldOnGpu` /
      // `CPUExclusiveType`), so nothing in the emitted output can miss
      // them.
      let cpu_only = s.fields.iter().any(|f| {
        let Ok(t) = f.field_type.clone().concretize(
          &vec![],
          &self.typedefs,
          f.source_trace.clone(),
        ) else {
          return false;
        };
        self.type_makes_struct_cpu_only(&t)
      });
      if cpu_only {
        continue;
      }
      if !s.opaque
        && !default_structs.contains(&s)
        && let Some(compiled_struct) = s.clone().compile_if_non_generic(
          &self.typedefs,
          &mut names,
          target,
        )?
      {
        compiled_string += &compiled_struct;
        compiled_string += "\n\n";
      }
    }
    for e in self.typedefs.enums.iter().cloned() {
      // Enums with runtime-sized payloads are CPU-only (their variants
      // have no fixed GPU layout) — skip their WGSL emission entirely,
      // like unbound unsized-array globals. GPU code referencing one is
      // caught by validation, so nothing in the emitted WGSL can miss it.
      if e
        .variants
        .iter()
        .any(|v| v.inner_type.data_size_in_u32s(&e.source_trace).is_err())
      {
        continue;
      }
      if let Some(compiled_enum) =
        e.compile_if_non_generic(&self.typedefs, &mut names, target)?
      {
        compiled_string += &compiled_enum;
        compiled_string += "\n\n";
      }
    }
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap().clone();
      // Functions whose signatures involve runtime-sized values (directly
      // or inside enums/structs) are CPU-only — they have no valid WGSL/C
      // representation. This gate covers the synthesized definitions this
      // loop emits (enum constructors), which never pass through
      // `TopLevelFunction::compile` and so can't be caught by its
      // `allowed_on_gpu` check; composite functions get the equivalent
      // signature check there. GPU code reaching a CPU-only function is a
      // validation error; here it's simply not emitted.
      let signature_is_cpu_only = f
        .arg_types
        .iter()
        .map(|(t, _)| t)
        .chain(std::iter::once(&f.return_type))
        .any(|t| match t {
          AbstractType::Type(t) => t.involves_runtime_sized_array(),
          AbstractType::AbstractEnum(e) => e.variants.iter().any(|v| {
            matches!(
              &v.inner_type,
              AbstractType::Type(t) if t.involves_runtime_sized_array()
            )
          }),
          _ => false,
        });
      if signature_is_cpu_only {
        continue;
      }
      if f.generic_args.is_empty() && !f.has_uninlined_higher_order_arguments()
      {
        match f.implementation {
          FunctionImplementationKind::EnumConstructor(
            original_variant_name,
          ) => {
            let variant_name = compile_word(f.name);
            let AbstractType::AbstractEnum(e) = f.return_type else {
              unreachable!("EnumConstructor fn had a non-enum type")
            };
            let (discriminant, variant) = e
              .variants
              .iter()
              .enumerate()
              .find(|(_, v)| v.name == original_variant_name)
              .expect("EnumConstructor fn name didn't match any variant");
            let AbstractType::Type(inner_type) = &variant.inner_type else {
              unreachable!()
            };
            let args_str = if *inner_type == Type::Unit {
              String::new()
            } else {
              let inner_type_name =
                inner_type.monomorphized_name(&mut names, target);
              match target {
                CompilerTarget::WGSL => {
                  format!("value: {inner_type_name}")
                }
                CompilerTarget::C => format!("{inner_type_name} value"),
                CompilerTarget::VM => panic!(),
              }
            };
            let enum_name = compile_word(
              e.original_ancestor().monomorphized_name(
                &e.variants
                  .iter()
                  .map(|variant| {
                    let AbstractType::Type(t) = &variant.inner_type else {
                      unreachable!()
                    };
                    t.clone()
                  })
                  .collect(),
                &mut names,
                target,
              ),
            );
            compiled_string += &match target {
              CompilerTarget::WGSL => {
                let bitcast_inner_values = inner_type
                  .bitcastable_chunk_accessors("value".into())
                  .into_iter()
                  .map(|exp| {
                    format!(
                      "bitcast<u32>({})",
                      exp.compile(
                        ExpressionCompilationPosition::InnerExpression,
                        &mut names,
                        target
                      )
                    )
                  })
                  .chain(std::iter::repeat("0u".into()))
                  .take(e.inner_data_size_in_u32s()?)
                  .collect::<Vec<String>>()
                  .join(", ");
                format!(
                  "fn {variant_name}({args_str}) -> {enum_name} {{\n  \
                    return {enum_name}({discriminant}u, array({bitcast_inner_values}));\n\
                  }}"
                )
              }
              CompilerTarget::C => {
                let memcpy_lines = inner_type
                  .bitcastable_chunk_accessors("value".into())
                  .into_iter()
                  .enumerate()
                  .map(|(i, exp)| {
                    format!(
                      "memcpy(&result.data[{i}], &{}, sizeof(uint32_t));",
                      exp.compile(
                        ExpressionCompilationPosition::InnerExpression,
                        &mut names,
                        target
                      )
                    )
                  })
                  .collect::<Vec<String>>()
                  .join("\n  ");
                format!(
                  "{enum_name} {variant_name}({args_str}) {{\n  \
                  {enum_name} result = {{{discriminant}}};\n  \
                  {memcpy_lines}\n  \
                  return result;\n\
                }}"
                )
              }
              CompilerTarget::VM => panic!(),
            };
            compiled_string += "\n\n";
          }
          _ => {}
        }
      }
    }
    for chunk in self.emulated_functions.helper_chunks.iter() {
      compiled_string += &chunk;
      compiled_string += "\n\n";
    }
    // Usage-based emission: a compiler-generated function reachable only
    // from contexts this target doesn't compile (e.g. an audio clone, or
    // a cpu-only monomorphized instance, in WGSL output) is skipped.
    // Directly user-written functions always emit (modulo the type and
    // effect gates in `TopLevelFunction::compile`) so external WGSL/C
    // that links against easl's output can call them, and functions no
    // entry reaches (mask 0) emit for the same reason.
    let function_contexts = self.compute_function_contexts();
    let target_contexts = execution_context::target_context_mask(target);
    for (f_name, implementation) in self.composite_functions_in_usage_order() {
      {
        let implementation = implementation.read().unwrap();
        let Type::Function(signature) =
          implementation.expression.data.unwrap_known()
        else {
          panic!()
        };
        let return_type = signature.return_type.unwrap_known();
        if matches!(return_type, Type::Function(_))
          && return_type.is_unitlike(&mut names)
        {
          continue;
        }
        if !implementation.directly_user_written
          && let Some(info) = function_contexts.get(&f_name)
          && info.mask != 0
          && info.mask & target_contexts == 0
        {
          continue;
        }
      }
      compiled_string += &implementation
        .read()
        .unwrap()
        .clone()
        .compile(&f_name, &mut names, &self, target)?;
      compiled_string += "\n\n";
    }
    Ok(compiled_string)
  }
  pub fn expand_associative_applications(&mut self) {
    for f in self
      .abstract_functions
      .iter_mut()
      .map(|(_, fns)| fns.into_iter())
      .flatten()
    {
      if let FunctionImplementationKind::Composite(f) =
        &f.read().unwrap().implementation
      {
        f.write()
          .unwrap()
          .expression
          .walk_mut::<()>(&mut |exp| {
            loop {
              let mut needs_another_loop = false;
              take(&mut exp.kind, |exp_kind| {
                if let ExpKind::Application(f, args) = exp_kind {
                  if let ExpKind::Name(_) = &f.kind
                    && let Type::Function(x) = f.data.kind.unwrap_known()
                    && let Some(abstract_ancestor) = &x.abstract_ancestor
                    && abstract_ancestor.read().unwrap().associative
                    && args.len() != 2
                  {
                    let mut args_iter = args.into_iter();
                    let mut new_exp = args_iter.next().unwrap();
                    if args_iter.len() == 0 {
                      needs_another_loop = true;
                    } else {
                      while let Some(next_arg) = args_iter.next() {
                        new_exp = Exp {
                          kind: ExpKind::Application(
                            f.clone(),
                            vec![new_exp, next_arg],
                          ),
                          data: exp.data.clone(),
                          source_trace: exp.source_trace.clone(),
                        };
                      }
                    }
                    new_exp.kind
                  } else {
                    ExpKind::Application(f, args)
                  }
                } else {
                  exp_kind
                }
              });
              if !needs_another_loop {
                break;
              }
            }
            Ok(true)
          })
          .unwrap();
      }
    }
  }
  fn deshadow(&mut self, errors: &mut ErrorLog) {
    let globally_bound_names: Vec<Arc<str>> = self
      .top_level_vars
      .iter()
      .map(|v| Arc::clone(&v.name))
      .chain(
        self
          .abstract_functions
          .iter()
          .map(|(name, _)| Arc::clone(name)),
      )
      .collect();
    for (_, signatures) in self.abstract_functions.iter_mut() {
      for signature in signatures.iter_mut() {
        let mut signature = signature.write().unwrap();
        if let FunctionImplementationKind::Composite(f) =
          &mut signature.implementation
        {
          f.write().unwrap().expression.deshadow(
            &globally_bound_names,
            errors,
            &mut self.names.write().unwrap(),
          );
        }
      }
    }
  }
  fn wrap_mutable_function_args(&mut self) {
    for signature in self.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(implementation) =
        &signature.read().unwrap().implementation
      {
        let mut implementation = implementation.write().unwrap();
        if let Type::Function(f) =
          &mut implementation.expression.data.unwrap_known()
          && let ExpKind::Function(arg_names, body) =
            &mut implementation.expression.kind
        {
          let mutable_args: Vec<_> = f
            .args
            .iter()
            .zip(arg_names.iter())
            .filter_map(|((var, _), arg_name)| {
              if var.kind == VariableKind::Var
                && var.var_type.ownership == Ownership::Owned
              {
                Some((arg_name.clone(), var.var_type.clone()))
              } else {
                None
              }
            })
            .collect();
          if mutable_args.len() > 0 {
            take(body, |body| {
              TypedExp {
                data: body.data.clone(),
                source_trace: body.source_trace.clone(),
                kind: ExpKind::Let(
                  mutable_args
                    .into_iter()
                    .map(|((arg_name, _), arg_type)| {
                      (
                        arg_name.clone(),
                        SourceTrace::empty(),
                        VariableKind::Var,
                        TypedExp {
                          data: arg_type.clone(),
                          kind: ExpKind::Name(arg_name),
                          source_trace: body.source_trace.clone(),
                        },
                      )
                    })
                    .collect(),
                  body,
                ),
              }
              .into()
            });
          }
        }
      }
    }
  }
  fn validate_names(&self, errors: &mut ErrorLog) {
    // Runs before any compiler-created names exist (implicit audio-info
    // vars, window-info bindings, lifted captures), so reserved-name
    // rejection here can only ever fire on user-written declarations.
    let log_if_reserved =
      |name: &Arc<str>, source: &SourceTrace, errors: &mut ErrorLog| {
        if is_easl_reserved_word(name) {
          errors.log(CompileError::new(
            CompileErrorKind::EaslReservedName(name.to_string()),
            source.clone(),
          ));
        }
      };
    for v in self.top_level_vars.iter() {
      log_if_reserved(&v.name, &v.source_trace, errors);
    }
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &signature.implementation
      {
        let implementation = implementation.read().unwrap();
        if !is_valid_name(&signature.name) {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidName,
            implementation.name_source_trace.clone(),
          ))
        }
        log_if_reserved(
          &signature.name,
          &implementation.name_source_trace,
          errors,
        );
        for (arg_name, arg_source) in implementation.arg_names.iter() {
          log_if_reserved(arg_name, arg_source, errors);
        }
        for (generic_name, _, source_trace) in signature.generic_args.iter() {
          if !is_valid_name(generic_name) {
            errors.log(CompileError::new(
              CompileErrorKind::InvalidName,
              source_trace.clone(),
            ))
          }
        }
        implementation
          .expression
          .walk(&mut |exp| {
            let names: Vec<_> = match &exp.kind {
              ExpKind::Let(items, _) => items
                .iter()
                .map(|(name, source, _, _)| (name, source))
                .collect(),
              ExpKind::Match(_, arms) => arms
                .iter()
                .flat_map(|(pattern, _)| {
                  if let ExpKind::Application(_, args) = &pattern.kind {
                    args
                      .iter()
                      .filter_map(|arg| {
                        if let ExpKind::Name(name) = &arg.kind {
                          Some((name, &arg.source_trace))
                        } else {
                          None
                        }
                      })
                      .collect()
                  } else {
                    vec![]
                  }
                })
                .collect(),
              ExpKind::ForLoop {
                increment_variable_name,
                ..
              } => {
                vec![(&increment_variable_name.0, &increment_variable_name.1)]
              }
              _ => vec![],
            };
            for (name, source) in names {
              if !is_valid_name(name) {
                errors.log(CompileError::new(
                  CompileErrorKind::InvalidName,
                  source.clone(),
                ));
              }
              log_if_reserved(name, source, errors);
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
    for e in self.typedefs.enums.iter() {
      if !is_valid_name(&e.name.0) {
        errors.log(CompileError::new(
          CompileErrorKind::InvalidName,
          e.name.1.clone(),
        ));
      }
      for (name, _, source) in e.generic_args.iter() {
        if !is_valid_name(name) {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidName,
            source.clone(),
          ));
        }
      }
      for variant in e.variants.iter() {
        if !is_valid_name(&variant.name) {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidName,
            variant.source.clone(),
          ));
        }
      }
    }
    for s in self.typedefs.structs.iter() {
      if !is_valid_name(&s.name.0) {
        errors.log(CompileError::new(
          CompileErrorKind::InvalidName,
          s.name.1.clone(),
        ));
      }
      for (name, _, source) in s.generic_args.iter() {
        if !is_valid_name(name) {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidName,
            source.clone(),
          ));
        }
      }
      for field in s.fields.iter() {
        if !is_valid_name(&field.name) {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidName,
            field.source_trace.clone(),
          ));
        }
      }
    }
  }
  fn validate_associative_signatures(&self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if signature.associative
        && (signature.arg_types.len() != 2
          || signature.arg_types[0] != signature.arg_types[1]
          || signature.arg_types[0].0 != signature.return_type)
      {
        if let FunctionImplementationKind::Composite(implementation) =
          &signature.implementation
        {
          errors.log(CompileError {
            kind: CompileErrorKind::InvalidAssociativeSignature,
            source_trace: implementation
              .read()
              .unwrap()
              .expression
              .source_trace
              .clone(),
          });
        }
      }
    }
  }
  fn catch_duplicate_signatures(&self, errors: &mut ErrorLog) {
    for (name, signatures) in self.abstract_functions.iter() {
      let mut normalized_signatures: Vec<(Option<SourceTrace>, _)> = vec![];
      for signature in signatures {
        if let FunctionImplementationKind::Builtin { .. }
        | FunctionImplementationKind::StructConstructor =
          signature.read().unwrap().implementation
        {
          let normalized = signature.read().unwrap().normalized_signature();
          normalized_signatures.push((None, normalized));
        }
      }
      for signature in signatures {
        if let FunctionImplementationKind::Composite(f) =
          &signature.read().unwrap().implementation
        {
          let source = f.read().unwrap().expression.source_trace.clone();
          let normalized = signature.read().unwrap().normalized_signature();
          for (previous_signature, previous_normalized) in
            normalized_signatures.iter()
          {
            if *previous_normalized == normalized {
              if let Some(previous_source) = previous_signature {
                errors.log(CompileError {
                  kind: CompileErrorKind::DuplicateFunctionSignature(
                    name.to_string(),
                  ),
                  source_trace: source
                    .clone()
                    .insert_as_secondary(previous_source.clone()),
                });
              } else {
                errors.log(CompileError {
                  kind: CompileErrorKind::FunctionSignatureConflictsWithBuiltin(
                    name.to_string(),
                  ),
                  source_trace: source.clone(),
                });
              }
            }
          }
          normalized_signatures.push((Some(source), normalized));
        }
      }
    }
  }
  fn catch_globally_shadowing_fn_args(&self, errors: &mut ErrorLog) {
    for (_, signatures) in self.abstract_functions.iter() {
      for signature in signatures {
        if let FunctionImplementationKind::Composite(f) =
          &signature.read().unwrap().implementation
        {
          let f = f.read().unwrap();
          for (arg_name, _) in f.arg_names.iter() {
            if self.abstract_functions.get(arg_name).is_some()
              || self
                .top_level_vars
                .iter()
                .find(|v| v.name == *arg_name)
                .is_some()
            {
              errors.log(CompileError::new(
                CantShadowTopLevelBinding(arg_name.to_string()),
                f.expression.source_trace.clone(),
              ))
            }
          }
        }
      }
    }
  }
  fn catch_duplicate_struct_fields(&self, errors: &mut ErrorLog) {
    for s in self.typedefs.structs.iter() {
      let mut names_so_far = HashSet::new();
      for field in s.fields.iter() {
        let name = &field.name;
        if names_so_far.contains(name) {
          errors.log(CompileError::new(
            CompileErrorKind::DuplicateStructFieldName,
            field.source_trace.clone(),
          ));
        } else {
          names_so_far.insert(name);
        }
      }
    }
  }
  fn catch_duplicate_enum_variants(&self, errors: &mut ErrorLog) {
    for e in self.typedefs.enums.iter() {
      let mut names_so_far = HashSet::new();
      for variant in e.variants.iter() {
        let name = &variant.name;
        if names_so_far.contains(name) {
          errors.log(CompileError::new(
            CompileErrorKind::DuplicateEnumVariantName,
            variant.source.clone(),
          ));
        } else {
          names_so_far.insert(name);
        }
      }
    }
  }
  fn catch_top_level_function_and_var_name_collisions(
    &self,
    errors: &mut ErrorLog,
  ) {
    for var in self.top_level_vars.iter() {
      if self.abstract_functions.get(&var.name).is_some() {
        errors.log(CompileError {
          kind: VariableFunctionNameCollision(var.name.to_string()),
          source_trace: var.source_trace.clone(),
        })
      }
    }
  }
  fn ensure_no_typeless_bindings(&self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &signature.implementation
      {
        implementation
          .read()
          .unwrap()
          .expression
          .walk(&mut |exp| {
            match &exp.kind {
              ExpKind::Let(items, _) => {
                for (_, source_trace, _, value) in items.iter() {
                  if Type::Unit.known() == value.data.kind {
                    errors.log(CompileError {
                      kind: CompileErrorKind::TypelessBinding,
                      source_trace: source_trace.clone(),
                    });
                  }
                }
              }
              _ => {}
            }
            Ok::<_, Never>(true)
          })
          .unwrap();
      }
    }
  }
  pub fn validate_control_flow(&mut self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        f.read()
          .unwrap()
          .expression
          .validate_control_flow(errors, 0);
      }
    }
  }
  pub fn deexpressionify(&mut self, target: CompilerTarget) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        let mut f = f.write().unwrap();
        f.expression.throw_away_inner_values_in_blocks(self);
        f.expression.deexpressionify(self, target);
      }
    }
  }
  pub fn separate_overloaded_fns(&mut self, target: CompilerTarget) {
    let mut renames = HashMap::new();
    for (_, signatures) in self.abstract_functions.iter() {
      if signatures.len() > 1 {
        for s in signatures.iter() {
          let mut s = s.write().unwrap();
          let base_name = s.name.clone();
          let type_signature = if s.generic_args.is_empty()
            && let FunctionImplementationKind::Composite(f) =
              &mut s.implementation
            && let Type::Function(f) =
              f.read().unwrap().expression.data.unwrap_known()
          {
            f.unwrap_type_signature()
          } else {
            continue;
          };
          let suffix_types: Vec<&Type> = if type_signature
            .iter()
            .any(|t| matches!(t, Type::Function(_)))
          {
            let non_fn: Vec<&Type> = type_signature
              .iter()
              .filter(|t| !matches!(t, Type::Function(_)))
              .collect();
            if non_fn.is_empty() {
              continue;
            }
            non_fn
          } else {
            type_signature.iter().collect()
          };
          let new_name = base_name.to_string()
            + "_"
            + &suffix_types
              .iter()
              .map(|t| {
                t.monomorphized_name(&mut self.names.write().unwrap(), target)
              })
              .collect::<Vec<String>>()
              .join("_");
          let new_name: Arc<str> = new_name.into();
          s.name = new_name.clone();
          if !renames.contains_key(&base_name) {
            renames.insert(base_name.clone(), vec![]);
          }
          renames
            .get_mut(&base_name)
            .unwrap()
            .push((type_signature, new_name));
        }
      }
    }
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        f.write()
          .unwrap()
          .expression
          .walk_mut(&mut |exp| {
            if let ExpKind::Name(name) = &mut exp.kind {
              if let Some(renames) = renames.get(name)
                && let Type::Function(f) = exp.data.unwrap_known()
              {
                let f_signature = f.unwrap_type_signature();
                for (signature, rename) in renames.iter() {
                  if signature == &f_signature {
                    *name = rename.clone();
                  }
                }
              }
              Ok::<bool, Never>(false)
            } else {
              Ok(true)
            }
          })
          .unwrap();
      }
    }
    // Rebuild abstract_functions to use new function names
    if renames.is_empty() {
      return;
    }
    let old_abstract_functions = std::mem::take(&mut self.abstract_functions);
    for sig in old_abstract_functions.into_values().flatten() {
      let name = sig.read().unwrap().name.clone();
      self.abstract_functions.entry(name).or_default().push(sig);
    }
  }
  pub fn inline_def_array_sizes(&mut self) {
    let u32_constants: HashMap<Arc<str>, u32> = self
      .top_level_vars
      .iter()
      .filter_map(|v| {
        if (v.var_type == Type::U32 || v.var_type == Type::I32)
          && v.kind == TopLevelVariableKind::Const
          && let Some(TypedExp {
            kind: ExpKind::NumberLiteral(Number::Int(n)),
            ..
          }) = v.value
          && let Ok(n) = n.try_into()
        {
          Some((v.name.clone(), n))
        } else {
          None
        }
      })
      .collect();
    for v in self.top_level_vars.iter_mut() {
      v.var_type.inline_def_array_sizes(&u32_constants);
    }
    for s in self.typedefs.structs.iter_mut() {
      for field in s.fields.iter_mut() {
        field
          .field_type
          .walk_mut(&mut |t| {
            if let AbstractType::AbstractArray { size, .. } = t
              && let AbstractArraySize::Constant(constant_name) = size
              && let Some(n) = u32_constants.get(constant_name)
            {
              *size = AbstractArraySize::Literal(*n);
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
    for f in self.abstract_functions_iter_mut() {
      let mut f = f.write().unwrap();

      f.inline_def_array_sizes(&u32_constants);

      if let FunctionImplementationKind::Composite(f) = &f.implementation {
        f.write()
          .unwrap()
          .expression
          .walk_mut(&mut |exp| {
            if let TypeState::Known(t) = &mut exp.data.kind {
              t.inline_def_array_sizes(&u32_constants);
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
  }
  pub fn inline_static_array_length_calls(&mut self) {
    for f in self.abstract_functions_iter_mut() {
      if let FunctionImplementationKind::Composite(f) =
        &f.write().unwrap().implementation
      {
        f.write()
          .unwrap()
          .expression
          .walk_mut(&mut |exp| {
            if let ExpKind::Application(f, args) = &exp.kind
              && let ExpKind::Name(f_name) = &f.kind
              && &**f_name == "array-length"
              && let Type::Array(Some(size), _) = args[0].data.unwrap_known()
              && size != ConcreteArraySize::Unsized
            {
              let size = match size {
                ConcreteArraySize::Literal(x) => Some(x),
                ConcreteArraySize::UnificationVariable(const_generic_value) => {
                  match &*const_generic_value.value.read().unwrap() {
                    Some(ConstGenericResolution::Literal(n)) => Some(*n),
                    _ => None,
                  }
                }
                ConcreteArraySize::Skolem(_) => None,
                _ => panic!("can't handle this kind of ConcreteArraySize here"),
              };
              if let Some(size) = size {
                *exp = TypedExp {
                  data: Type::U32.known().into(),
                  kind: ExpKind::NumberLiteral(Number::Int(size as i64)),
                  source_trace: exp.source_trace.clone(),
                }
              }
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
  }
  pub fn desugar_swizzle_assignments(&mut self) {
    let mut names = self.names.write().unwrap();
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        f.write()
          .unwrap()
          .expression
          .walk_mut(&mut |exp| {
            exp.desugar_swizzle_assignments(&mut names);
            Ok::<_, Never>(true)
          })
          .unwrap();
      }
    }
  }
  /// Validates the GPU boundary for runtime-sized values: functions
  /// reachable from GPU entry points may not return or accept
  /// runtime-sized arrays, may not create or locally bind them, and may
  /// not use struct/enum *values* containing them (storage-bound globals
  /// are exempt — WGSL itself permits a trailing runtime-sized member in
  /// a storage struct, which is what dispatched-closure scope bindings
  /// rely on). Nested runtime-sized arrays can never be GPU buffer
  /// bindings at all.
  pub fn validate_gpu_runtime_sized_use(&mut self, errors: &mut ErrorLog) {
    use crate::compiler::expression::ExpKind;
    // A binding may involve a runtime-sized array only by *being* one:
    // nested runtime-sized arrays have no flat host-shareable layout, and
    // runtime-sized struct/enum fields are banned outright (stricter than
    // WGSL, which permits one trailing runtime-sized member — binding the
    // array separately expresses the same thing, and the blanket rule
    // lets WGSL emission drop every runtime-sized-field struct
    // declaration, like the scope structs of dispatched closures with
    // runtime-sized captures).
    for v in self.top_level_vars.iter() {
      if let TopLevelVariableKind::Var {
        group_and_binding: Some(_),
        ..
      } = v.kind
      {
        if let Type::Array(Some(ConcreteArraySize::Unsized), inner) =
          &v.var_type
        {
          if inner.kind.unwrap_known().involves_runtime_sized_array() {
            errors.log(CompileError {
              kind: NestedRuntimeSizedArrayBinding,
              source_trace: v.source_trace.clone(),
            });
          }
        } else if v.var_type.involves_runtime_sized_array() {
          errors.log(CompileError {
            kind: RuntimeSizedFieldInBinding,
            source_trace: v.source_trace.clone(),
          });
        }
      }
    }
    // reachability from GPU entry roots, callees-first through composite
    // calls (the graph is a DAG — no recursion)
    let mut visited: HashSet<Arc<str>> = HashSet::new();
    let mut queue: Vec<Arc<RwLock<TopLevelFunction>>> = vec![];
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      if f
        .entry_point
        .map(|e| {
          matches!(
            e,
            EntryPoint::Vertex | EntryPoint::Fragment | EntryPoint::Compute(_)
          )
        })
        .unwrap_or(false)
        && let FunctionImplementationKind::Composite(implementation) =
          &f.implementation
        && visited.insert(f.name.clone())
      {
        queue.push(implementation.clone());
      }
    }
    while let Some(f) = queue.pop() {
      let f = f.read().unwrap();
      let Type::Function(signature) = f.expression.data.unwrap_known() else {
        continue;
      };
      let source_trace = f.expression.source_trace.clone();
      if matches!(
        signature.return_type.unwrap_known(),
        Type::Array(Some(ConcreteArraySize::Unsized), _)
      ) {
        errors.log(CompileError {
          kind: GpuFunctionReturnsRuntimeSizedArray,
          source_trace: source_trace.clone(),
        });
      }
      if signature.args.iter().any(|(arg, _)| {
        matches!(
          arg.var_type.unwrap_known(),
          Type::Array(Some(ConcreteArraySize::Unsized), _)
        )
      }) {
        errors.log(CompileError {
          kind: GpuFunctionAcceptsRuntimeSizedArray,
          source_trace: source_trace.clone(),
        });
      }
      let mut discovered: Vec<(Arc<str>, Arc<RwLock<TopLevelFunction>>)> =
        vec![];
      f.expression
        .walk(&mut |exp| {
          match &exp.kind {
            ExpKind::Let(bindings, _) => {
              for (_, _, _, value) in bindings.iter() {
                if matches!(
                  value.data.unwrap_known(),
                  Type::Array(Some(ConcreteArraySize::Unsized), _)
                ) {
                  errors.log(CompileError {
                    kind: RuntimeSizedLocalInGpuCode,
                    source_trace: value.source_trace.clone(),
                  });
                }
              }
            }
            _ => {}
          }
          // struct/enum *values* with runtime-sized contents
          if matches!(exp.data.unwrap_known(), Type::Struct(_) | Type::Enum(_))
            && exp.data.unwrap_known().involves_runtime_sized_array()
          {
            errors.log(CompileError {
              kind: RuntimeSizedFieldOnGpu,
              source_trace: exp.source_trace.clone(),
            });
          }
          // follow composite callees
          if let ExpKind::Application(applied_f, _) = &exp.kind
            && let TypeState::Known(Type::Function(sig)) = &applied_f.data.kind
            && let Some(ancestor) = &sig.abstract_ancestor
          {
            let ancestor = ancestor.read().unwrap();
            if let FunctionImplementationKind::Composite(implementation) =
              &ancestor.implementation
              && !visited.contains(&ancestor.name)
            {
              discovered.push((ancestor.name.clone(), implementation.clone()));
            }
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
      for (name, implementation) in discovered {
        if visited.insert(name) {
          queue.push(implementation);
        }
      }
    }
  }

  pub fn validate_top_level_fn_effects(&mut self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        let f = f.read().unwrap();
        if let Some(entry_point) = f.entry_point {
          let ExpKind::Function(_, body) = &f.expression.kind else {
            unreachable!()
          };
          let effects = body.effects();
          if let EntryPoint::Vertex
          | EntryPoint::Compute(_)
          | EntryPoint::Fragment = entry_point
          {
            // CPU-exclusive *function* checks live in
            // `validate_context_exclusivity`; the type- and write-shaped
            // checks below are effect-set-based and remain here.
            for type_name in effects.cpu_exclusive_types() {
              errors.log(CompileError {
                kind: CPUExclusiveTypeInGPUEntryPoint(type_name.to_string()),
                source_trace: f.expression.source_trace.clone(),
              });
            }
            for effect in effects.0.iter() {
              if let Effect::ModifiesGlobalVar(name) = effect
                && let Some(top_level_var) =
                  self.top_level_vars.iter().find(|v| v.name == *name)
                && let TopLevelVariableKind::Var { address_space, .. } =
                  top_level_var.kind
                && !address_space.may_write_from_gpu()
              {
                errors.log(CompileError {
                  kind: IllegalAddressSpaceGpuWrite(
                    name.to_string(),
                    address_space,
                  ),
                  source_trace: f.expression.source_trace.clone(),
                });
              }
            }
          }
        }
      }
    }
  }
  pub fn validate_entry_points(&mut self, errors: &mut ErrorLog) {
    let mut inferred_struct_field_locations: Vec<(
      Arc<AbstractStruct>,
      Arc<str>,
      usize,
    )> = vec![];
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        let mut f = f.write().unwrap();
        let f_source = f.expression.source_trace.clone();
        if let Some(entry) = f.entry_point {
          let Type::Function(signature) = f.expression.data.unwrap_known()
          else {
            unreachable!()
          };
          match entry {
            EntryPoint::Vertex => {
              if signature.return_type.unwrap_known().is_vec4f()
                && f.return_attributes.is_empty()
              {
                let return_source =
                  f.return_attributes.attributed_source.clone();
                f.return_attributes.try_add_attribute(
                  IOAttribute {
                    kind: IOAttributeKind::Builtin(
                      BuiltinIOAttribute::Position,
                    ),
                    source_trace: return_source,
                  },
                  errors,
                );
              }
            }
            EntryPoint::Compute(_) => {
              let mut errored = false;
              if Type::Unit != signature.return_type.unwrap_known() {
                errors.log(CompileError::new(
                  ComputeEntryReturnType,
                  f.expression.source_trace.clone(),
                ));
                errored = true;
              }
              if !f.return_attributes.is_empty() {
                errors.log(CompileError::new(
                  ComputeEntryReturnType,
                  f.expression.source_trace.clone(),
                ));
                errored = true;
              }
              if errored {
                continue;
              }
            }
            EntryPoint::Fragment => {}
            EntryPoint::Cpu => {
              let mut errored = false;
              if Type::Unit != signature.return_type.unwrap_known() {
                errors.log(CompileError::new(
                  CpuEntryHasReturnType,
                  f.expression.source_trace.clone(),
                ));
                errored = true;
              }
              if !signature.args.is_empty() {
                errors.log(CompileError::new(
                  CpuEntryHasArguments,
                  f.expression.source_trace.clone(),
                ));
                errored = true;
              }
              if errored {
                continue;
              }
            }
            EntryPoint::Audio => {
              let mut errored = false;
              if Type::F32 != signature.return_type.unwrap_known() {
                errors.log(CompileError::new(
                  AudioEntryHasWrongReturnType,
                  f.expression.source_trace.clone(),
                ));
                errored = true;
              }
              // Audio entries take at most one f32 arg: the time. (A
              // stateful closure entry usually takes none; ambient info
              // is available via `audio-time`/`sample-rate`.)
              if signature.args.len() > 1
                || signature
                  .args
                  .iter()
                  .any(|(arg, _)| arg.var_type.unwrap_known() != Type::F32)
              {
                errors.log(CompileError::new(
                  AudioEntryHasWrongArgumentTypes,
                  f.expression.source_trace.clone(),
                ));
                errored = true;
              }
              if errored {
                continue;
              }
            }
          }

          let check_for_duplicate_builtins =
            |attributables: &Vec<(
              Type,
              Result<
                &mut IOAttributes,
                (Arc<AbstractStruct>, Arc<str>, IOAttributes),
              >,
            )>|
             -> Vec<(String, SourceTrace)> {
              let mut duplicates = HashSet::new();
              let mut builtins = HashSet::new();
              for (_, attributable) in attributables.iter() {
                let attribute = match attributable {
                  Ok(a) => &*a,
                  Err((_, _, a)) => a,
                };
                if let Some((builtin, source_trace)) = attribute.builtin() {
                  if builtins.contains(builtin) {
                    duplicates.insert((
                      builtin.name().to_string(),
                      source_trace.clone(),
                    ));
                  } else {
                    builtins.insert(builtin.clone());
                  }
                }
              }
              duplicates.into_iter().collect()
            };

          let mut handle_inout_attributables =
            |attributables: Vec<(
              Type,
              Result<
                &mut IOAttributes,
                (Arc<AbstractStruct>, Arc<str>, IOAttributes),
              >,
            )>,
             input_or_output: InputOrOutput,
             errors: &mut ErrorLog|
             -> (
              HashMap<usize, (SourceTrace, Result<Type, Arc<AbstractStruct>>)>,
              HashMap<BuiltinIOAttribute, Result<Type, AbstractType>>,
            ) {
              for (name, source) in check_for_duplicate_builtins(&attributables)
              {
                errors.log(CompileError::new(
                  DuplicateBuiltinAttribute(input_or_output, name),
                  source,
                ))
              }
              let mut used_locations: HashMap<
                usize,
                (SourceTrace, Result<Type, Arc<AbstractStruct>>),
              > = HashMap::new();
              let mut used_builtins: HashMap<
                BuiltinIOAttribute,
                Result<Type, AbstractType>,
              > = HashMap::new();
              for (t, attributable) in attributables.iter() {
                let attributes = match attributable {
                  Ok(a) => &*a,
                  Err((_, _, a)) => a,
                };
                if let Some((builtin, source)) = attributes.builtin() {
                  if match input_or_output {
                    InputOrOutput::Input => {
                      !builtin.is_valid_input_for_stage(&entry)
                    }
                    InputOrOutput::Output => {
                      !builtin.is_valid_output_for_stage(&entry)
                    }
                  } {
                    errors.log(CompileError::new(
                      InvalidBuiltinForEntryPoint(
                        builtin.name().to_string(),
                        input_or_output,
                        entry.name().to_string(),
                      ),
                      source.clone(),
                    ));
                  } else {
                    used_builtins.insert(
                      *builtin,
                      match attributable {
                        Ok(_) => Ok(t.clone()),
                        Err((s, field_name, _)) => Err(
                          s.fields
                            .iter()
                            .find_map(|f| {
                              (f.name == *field_name)
                                .then(|| f.field_type.clone())
                            })
                            .unwrap()
                            .clone(),
                        ),
                      },
                    );
                  }
                  let t = match attributable {
                    Ok(_) => t,
                    Err((s, field_name, _)) => &s
                      .fields
                      .iter()
                      .find(|f| f.name == *field_name)
                      .unwrap()
                      .field_type
                      .concretize(&vec![], &self.typedefs, SourceTrace::empty())
                      .unwrap(),
                  };
                  if !builtin.is_type_compatible(t) {
                    errors.log(CompileError::new(
                      InvalidBuiltinType(builtin.name().to_string()),
                      attributes.attributed_source.clone(),
                    ))
                  }
                } else {
                  if let Some((location, source)) = attributes.location() {
                    used_locations.insert(location, (source, Ok(t.clone())));
                  }
                }
              }
              for (t, attributable) in attributables {
                let attributes = match &attributable {
                  Ok(a) => &*a,
                  Err((_, _, a)) => a,
                };
                if attributes.builtin().is_none()
                  && attributes.location().is_none()
                {
                  let untaken_location =
                    (0..).find(|i| !used_locations.contains_key(i)).unwrap();
                  match attributable {
                    Ok(a) => {
                      if t.is_location_attributable() {
                        let source_trace = a.attributed_source.clone();
                        a.try_add_attribute(
                          IOAttribute {
                            kind: IOAttributeKind::Location(untaken_location),
                            source_trace: source_trace.clone(),
                          },
                          errors,
                        );
                        used_locations
                          .insert(untaken_location, (source_trace, Ok(t)));
                      } else {
                        errors.log(CompileError::new(
                          InvalidTypeForEntryPoint(t.into(), input_or_output),
                          f_source.clone(),
                        ));
                      }
                    }
                    Err((t, field_name, a)) => {
                      inferred_struct_field_locations.push((
                        t.clone(),
                        field_name.clone(),
                        untaken_location,
                      ));
                      used_locations.insert(
                        untaken_location,
                        (a.attributed_source, Err(t.clone())),
                      );
                    }
                  }
                }
              }
              (used_locations, used_builtins)
            };

          let input_attributables: Vec<(
            Type,
            Result<
              &mut IOAttributes,
              (Arc<AbstractStruct>, Arc<str>, IOAttributes),
            >,
          )> = f
            .arg_annotations
            .iter_mut()
            .enumerate()
            .flat_map(|(i, annotation)| {
              let arg = &signature.args[i];
              let arg_type = arg.0.var_type.unwrap_known();
              if arg_type.is_attributable() {
                vec![(arg_type, Ok(&mut annotation.attributes))]
              } else {
                if let Some(source) =
                  annotation.attributes.source_trace_if_not_empty()
                {
                  errors
                    .log(CompileError::new(CantAssignAttributesToType, source));
                  vec![]
                } else {
                  self
                    .typedefs
                    .get_attributable_components(
                      arg_type.clone(),
                      InputOrOutput::Input,
                      f_source.clone(),
                      errors,
                    )
                    .into_iter()
                    .map(|(t, field_name, attributes)| {
                      (arg_type.clone(), Err((t, field_name, attributes)))
                    })
                    .collect()
                }
              }
            })
            .collect();
          handle_inout_attributables(
            input_attributables,
            InputOrOutput::Input,
            errors,
          );

          let return_type = signature.return_type.unwrap_known();
          let output_attributables: Vec<(
            Type,
            Result<
              &mut IOAttributes,
              (Arc<AbstractStruct>, Arc<str>, IOAttributes),
            >,
          )> = if return_type.is_attributable() {
            vec![(return_type, Ok(&mut f.return_attributes))]
          } else {
            if let Some(source) =
              f.return_attributes.source_trace_if_not_empty()
            {
              errors.log(CompileError::new(CantAssignAttributesToType, source));
              vec![]
            } else {
              self
                .typedefs
                .get_attributable_components(
                  return_type.clone(),
                  InputOrOutput::Output,
                  f_source.clone(),
                  errors,
                )
                .into_iter()
                .map(|(t, field_name, attributes)| {
                  (return_type.clone(), Err((t, field_name, attributes)))
                })
                .collect()
            }
          };
          let (used_output_locations, used_output_builtins) =
            handle_inout_attributables(
              output_attributables,
              InputOrOutput::Output,
              errors,
            );

          match entry {
            EntryPoint::Vertex => {
              if let Some(t) =
                used_output_builtins.get(&BuiltinIOAttribute::Position)
              {
                if match t {
                  Ok(t) => !t.is_vec4f(),
                  Err(t) => !t.is_vec4f(),
                } {
                  errors.log(CompileError::new(
                    VertexPositionOutputInvalidType,
                    f_source,
                  ));
                }
              } else {
                errors.log(CompileError::new(
                  VertexMustHavePositionOutput,
                  f_source,
                ));
              }
            }
            EntryPoint::Fragment => {
              if let Some((_, t)) = used_output_locations.get(&0) {
                if let Ok(Type::Struct(s)) = t
                  && &*s.name == "vec4"
                  && {
                    let field_type = s.fields[0].field_type.unwrap_known();
                    field_type == Type::F32
                      || field_type == Type::U32
                      || field_type == Type::I32
                  }
                {
                } else if let Err(s) = t
                  && &*s.name.0 == "vec4"
                  && {
                    match s.fields[0].field_type {
                      AbstractType::Type(Type::F32 | Type::U32 | Type::I32) => {
                        true
                      }
                      _ => false,
                    }
                  }
                {
                } else {
                  errors.log(CompileError::new(
                    Fragment0OutputInvalidType,
                    f_source,
                  ));
                }
              } else {
                errors.log(CompileError::new(
                  FragmentMustHaveLocation0Output,
                  f_source,
                ));
              }
            }
            _ => {}
          }
        } else {
          for attributes in f
            .arg_annotations
            .iter()
            .map(|a| &a.attributes)
            .chain(std::iter::once(&f.return_attributes))
          {
            if let Some(source_trace) = attributes.source_trace_if_not_empty() {
              errors
                .log(CompileError::new(IOAttributesOnNonEntry, source_trace));
            }
          }
        }
      }
    }
    while let Some((s, field_name, location)) =
      inferred_struct_field_locations.pop()
    {
      let struct_source_trace = s.source_trace.clone();
      let mut field_locations = vec![(field_name, location)];
      let mut remaining_inferred_struct_field_locations = vec![];
      for (other_s, field_name, location) in inferred_struct_field_locations {
        if s == other_s {
          let location = (field_name, location);
          if !field_locations.contains(&location) {
            field_locations.push(location);
          }
        } else {
          remaining_inferred_struct_field_locations
            .push((other_s, field_name, location));
        }
      }
      let s = self
        .typedefs
        .structs
        .iter_mut()
        .find(|existing_s| *s == **existing_s)
        .unwrap();
      for (field_name, location) in field_locations {
        s.fields
          .iter_mut()
          .find(|f| f.name == field_name)
          .unwrap()
          .attributes
          .try_add_attribute(
            IOAttribute {
              kind: IOAttributeKind::Location(location),
              source_trace: struct_source_trace.clone(),
            },
            errors,
          );
      }
      inferred_struct_field_locations =
        remaining_inferred_struct_field_locations;
    }
  }
  /// Assigns concrete group/binding numbers to every binding whose
  /// numbers were elided (`@[uniform]` in source, plus every
  /// compiler-created binding — window-info uniforms, dispatched-closure
  /// captures). Runs at the very end of validation, after all passes
  /// that create implicit bindings: vars are visited in declaration
  /// order and each elided binding takes the lowest free slot, filling
  /// gaps between explicitly-numbered bindings, in group 0 first and
  /// spilling to higher groups only if a group's 256 slots are
  /// exhausted. After this pass no `BindingSpec::Elided` remains
  /// anywhere.
  pub fn assign_elided_bindings(&mut self) {
    let mut used: HashSet<GroupAndBinding> = self
      .top_level_vars
      .iter()
      .filter_map(|v| {
        if let TopLevelVariableKind::Var {
          group_and_binding: Some(BindingSpec::Specified(group_and_binding)),
          ..
        } = v.kind
        {
          Some(group_and_binding)
        } else {
          None
        }
      })
      .collect();
    for var in self.top_level_vars.iter_mut() {
      if let TopLevelVariableKind::Var {
        group_and_binding: Some(binding_spec),
        ..
      } = &mut var.kind
        && *binding_spec == BindingSpec::Elided
      {
        let assigned = (0u8..=u8::MAX)
          .flat_map(|group| {
            (0u8..=u8::MAX)
              .map(move |binding| GroupAndBinding { group, binding })
          })
          .find(|candidate| !used.contains(candidate))
          .expect("all 65536 group/binding slots exhausted");
        used.insert(assigned);
        *binding_spec = BindingSpec::Specified(assigned);
      }
    }
  }
  /// Rewrites one expression's aliased-builtin calls in place (see
  /// `rewrite_aliased_builtin_calls`).
  fn rewrite_aliased_builtin_calls_in_exp(&self, exp: &mut TypedExp) {
    exp
      .walk_mut(&mut |e| {
        if let ExpKind::Application(f_exp, _) = &mut e.kind {
          let alias: Option<&'static str> =
            if let TypeState::Known(Type::Function(signature)) =
              &f_exp.data.kind
              && let Some(ancestor) = &signature.abstract_ancestor
              && let FunctionImplementationKind::Builtin {
                target_configuration:
                  FunctionTargetConfiguration::AliasedBuiltin(alias),
                ..
              } = ancestor.read().unwrap().implementation
            {
              Some(alias)
            } else {
              None
            };
          if let Some(alias) = alias {
            let ExpKind::Name(callee_name) = &mut f_exp.kind else {
              panic!("aliased builtin applied through a non-Name callee")
            };
            *callee_name = alias.into();
            // Swap in an abstract ancestor from the target builtin's
            // registry bucket: backends dispatch on the (rewritten)
            // callee name plus the concrete signature's types, which
            // inference already resolved, so among same-name candidates
            // only arity needs to match (for ownership zipping).
            let concrete_arity =
              if let TypeState::Known(Type::Function(signature)) =
                &f_exp.data.kind
              {
                signature.args.len()
              } else {
                unreachable!()
              };
            let replacement_ancestor = self
              .abstract_functions
              .get(alias)
              .and_then(|candidates| {
                candidates
                  .iter()
                  .find(|candidate| {
                    candidate.read().unwrap().arg_types.len() == concrete_arity
                  })
                  .or(candidates.first())
                  .cloned()
              })
              .unwrap_or_else(|| {
                panic!("alias target `{alias}` not found in registry")
              });
            if let TypeState::Known(Type::Function(signature)) =
              &mut f_exp.data.kind
            {
              signature.abstract_ancestor = Some(replacement_ancestor);
            }
          }
        }
        Ok::<bool, Never>(true)
      })
      .unwrap();
  }
  /// Replaces every resolved call to an `AliasedBuiltin` (e.g. the
  /// built-in `into` conversion overloads) with a call to its target
  /// builtin — callee name and abstract ancestor both — so no backend
  /// ever needs to know the alias name. Runs immediately after type
  /// inference, when overload resolution has already chosen the alias
  /// signature and the concrete argument/return types are final.
  pub fn rewrite_aliased_builtin_calls(&mut self) {
    for f in self.abstract_functions_iter() {
      let FunctionImplementationKind::Composite(implementation) =
        f.read().unwrap().implementation.clone()
      else {
        continue;
      };
      let mut implementation = implementation.write().unwrap();
      self.rewrite_aliased_builtin_calls_in_exp(&mut implementation.expression);
    }
    let mut top_level_vars = std::mem::take(&mut self.top_level_vars);
    for var in top_level_vars.iter_mut() {
      if let Some(value) = &mut var.value {
        self.rewrite_aliased_builtin_calls_in_exp(value);
      }
    }
    self.top_level_vars = top_level_vars;
  }
  pub fn catch_bind_group_collisions(&self, errors: &mut ErrorLog) {
    // Only explicitly-written numbers can collide; elided bindings are
    // assigned free numbers afterward (`assign_elided_bindings`).
    let mut existing_groups_and_bindings: HashMap<GroupAndBinding, String> =
      HashMap::new();
    for var in self.top_level_vars.iter() {
      if let TopLevelVariableKind::Var {
        group_and_binding: Some(BindingSpec::Specified(group_and_binding)),
        ..
      } = var.kind
      {
        if let Some(prior_name) =
          existing_groups_and_bindings.get(&group_and_binding)
        {
          errors.log(CompileError::new(
            BindGroupCollision(prior_name.clone(), var.name.to_string()),
            var.source_trace.clone(),
          ));
        } else {
          existing_groups_and_bindings
            .insert(group_and_binding, var.name.to_string());
        }
      }
    }
  }
  pub fn catch_non_constructible_bindings(&self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        f.read()
          .unwrap()
          .expression
          .walk(&mut |exp| {
            match &exp.kind {
              ExpKind::Let(bindings, _) => {
                for (_, _, _, value) in bindings {
                  if !value.data.unwrap_known().is_constructible() {
                    errors.log(CompileError::new(
                      CantBindNonConstructible,
                      exp.source_trace.clone(),
                    ));
                  }
                }
              }
              _ => {}
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
  }
  pub fn track_emulated_builtins(&mut self, target: CompilerTarget) {
    let mut names = self.names.write().unwrap();
    let mut emulated_functions = EmulatedFunctionRecord::empty();
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        f.write()
          .unwrap()
          .expression
          .walk_mut(&mut |exp| {
            if let ExpKind::Application(f, args) = &mut exp.kind
              && let ExpKind::Name(f_name) = &mut f.kind
              && let Type::Function(f) = f.data.unwrap_known()
              && let Some(abstract_f) = f.abstract_ancestor
              && let FunctionImplementationKind::Builtin {
                target_specific_emulations,
                ..
              } = &abstract_f.read().unwrap().implementation
              && target_specific_emulations.contains(&target)
            {
              let arg_types = args
                .iter()
                .map(|a| {
                  a.data.unwrap_known().monomorphized_name(&mut names, target)
                })
                .collect();
              let return_type =
                f.return_type.monomorphized_name(&mut names, target);
              let emulated_signature = EmulatedFunctionSignature {
                name: f_name.to_string(),
                arg_types,
                return_type,
              };

              *f_name = emulated_functions
                .track_emulated_builtin(
                  emulated_signature.clone(),
                  target,
                  &mut names,
                )
                .into();
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
    self.emulated_functions = emulated_functions;
  }
  pub fn catch_expressions_after_control_flow(
    &mut self,
    errors: &mut ErrorLog,
  ) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &signature.implementation
      {
        implementation
          .read()
          .unwrap()
          .expression
          .walk(&mut |exp| {
            match &exp.kind {
              ExpKind::Block(children) => {
                let mut encountered_control_flow_operator = None;
                for child in children.iter() {
                  match child.kind {
                    ExpKind::Break
                    | ExpKind::Continue
                    | ExpKind::Discard
                    | ExpKind::Return(_) => {
                      encountered_control_flow_operator =
                        Some(match child.kind {
                          ExpKind::Break => "break".to_string(),
                          ExpKind::Continue => "continue".to_string(),
                          ExpKind::Discard => "discard".to_string(),
                          ExpKind::Return(_) => "return".to_string(),
                          _ => unreachable!(),
                        });
                    }
                    ExpKind::Unit => {}
                    _ => {
                      if let Some(name) = &encountered_control_flow_operator {
                        errors.log(CompileError::new(
                          ExpressionAfterControlFlow(name.clone()),
                          child.source_trace.clone(),
                        ))
                      }
                    }
                  }
                }
              }
              _ => {}
            }
            Ok::<_, Never>(true)
          })
          .unwrap();
      }
    }
  }
  pub fn validate_raw_program(&mut self, target: CompilerTarget) -> ErrorLog {
    if self.has_been_validated {
      return ErrorLog::new();
    }
    let mut errors = ErrorLog::new();
    self.validate_names(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_associative_signatures(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.wrap_mutable_function_args();
    self.deshadow(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_globally_shadowing_fn_args(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_top_level_function_and_var_name_collisions(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_duplicate_struct_fields(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_duplicate_enum_variants(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.inline_def_array_sizes();
    self.fully_infer_types(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.rewrite_aliased_builtin_calls();
    self.validate_control_flow(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.ensure_no_typeless_bindings(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.expand_associative_applications();
    self.validate_assignments(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_duplicate_signatures(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_match_blocks(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_illegal_function_type_expressions(&mut errors);
    self.catch_illegal_function_type_user_type_fields(&mut errors);
    self.catch_illegal_function_type_variables(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.desugar_swizzle_assignments();
    self.deexpressionify(target);
    self.normalize_pseudoapplication_data_accesses();
    self.deshadow(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.monomorphize(&mut errors, target);
    if !errors.is_empty() {
      return errors;
    }
    self.separate_overloaded_fns(target);
    self.catch_duplicate_closures_capturing_mutable_variables(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    loop {
      let extracted = self.extract_inner_functions(&mut errors);
      if !errors.is_empty() {
        return errors;
      }
      self.propagate_abstract_function_signatures();
      self.inline_local_bound_function_applications();
      let inlined = self.inline_all_higher_order_arguments(&mut errors);
      if !errors.is_empty() {
        return errors;
      }
      if !extracted && !inlined {
        break;
      }
    }
    self.remove_unitlike_values();
    self.extract_non_bound_mutable_references();
    // Rewrites window-info queries into binding reads before effect
    // validation, so GPU entries no longer contain the queries (or the
    // String key literals they take).
    self.extract_gpu_window_info();
    self.validate_gpu_runtime_sized_use(&mut errors);
    self.validate_top_level_fn_effects(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_expressions_after_control_flow(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_argument_ownership(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_field_type_constraints(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_dispatched_closure_scope_mutations(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.extract_dispatched_closure_scopes(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    // Entry-point marking must happen before the reference-address-space
    // rebuild: that rebuild drops functions with reference args from the
    // registry — including a spawn-window frame closure with captured scope
    // (its trailing scope param) — and dispatch calls inside such a closure
    // are the only place implicitly-dispatched entry points are named. The
    // rebuild clones the signatures it keeps, so markings set here survive
    // it.
    self.validate_dispatch_function_types_and_mark_implicit_entry_points(
      &mut errors,
    );
    if !errors.is_empty() {
      return errors;
    }
    // Context-exclusivity validation needs entry markings (above) and
    // must see the original audio-info calls; the audio-info rewrite must
    // precede the audio-closure lift so clones inherit the rewritten
    // reads.
    self.validate_context_exclusivity(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.extract_audio_info();
    // Must run after entry marking (it moves `@audio` markings from
    // scoped closures to their audio clones) and before the
    // reference-address-space rebuild (which drops scoped closures from
    // the registry).
    self.extract_audio_closure_scopes(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.monomorphize_reference_address_spaces();
    self.inline_static_array_length_calls();
    self.validate_gpu_window_info(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.extract_builtin_attribute_lookup_functions();
    self.validate_entry_points(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_bind_group_collisions(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_non_constructible_bindings(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_gpu_used_binding_types(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.assign_elided_bindings();
    self.track_emulated_builtins(target);
    self.has_been_validated = true;
    errors
  }
  pub fn gather_type_annotations(&self) -> Vec<(SourceTrace, TypeState)> {
    let mut type_annotations = vec![];
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      let FunctionImplementationKind::Composite(implementation) =
        &signature.implementation
      else {
        continue;
      };
      implementation
        .read()
        .unwrap()
        .expression
        .walk(&mut |exp: &TypedExp| {
          type_annotations
            .push((exp.source_trace.clone(), exp.data.kind.clone()));
          if let ExpKind::Let(bindings, _) = &exp.kind {
            for (_, source_trace, _, bound_exp) in bindings.iter() {
              type_annotations
                .push((source_trace.clone(), bound_exp.data.kind.clone()))
            }
          }
          Ok::<_, Never>(true)
        })
        .unwrap();
    }
    type_annotations
  }
  pub fn gather_name_definition_sites(
    &self,
  ) -> HashMap<Vec<usize>, NameDefinitionSource> {
    let mut top_level_name_definitions = HashMap::new();
    for e in self.typedefs.enums.iter() {
      let e = e.original_ancestor();
      top_level_name_definitions.insert(
        e.name.0.clone(),
        NameDefinitionSource::Enum(e.name.1.primary_path()),
      );
    }
    for t in self.typedefs.enums.iter() {
      let t = t.original_ancestor();
      top_level_name_definitions.insert(
        t.name.0.clone(),
        NameDefinitionSource::Enum(t.name.1.primary_path()),
      );
    }
    let mut defn_locations: HashMap<Arc<str>, Vec<Vec<usize>>> = HashMap::new();
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &f.implementation
      {
        if !defn_locations.contains_key(&f.name) {
          defn_locations.insert(f.name.clone(), vec![]);
        }
        defn_locations.get_mut(&f.name).unwrap().push(
          implementation
            .read()
            .unwrap()
            .name_source_trace
            .primary_path(),
        );
      }
    }
    for (name, sources) in defn_locations {
      top_level_name_definitions
        .insert(name, NameDefinitionSource::Defn(sources));
    }
    let mut sites = HashMap::new();
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      if let FunctionImplementationKind::Composite(f) = &f.implementation {
        f.read()
          .unwrap()
          .expression
          .walk_with_ctx(
            &mut |exp, ctx| {
              match &exp.kind {
                ExpKind::Name(name) => {
                  if let Some(definition_source) = top_level_name_definitions
                    .get(name)
                    .cloned()
                    .or_else(|| ctx.get_name_definition_source(name))
                  {
                    sites.insert(
                      exp.source_trace.primary_path(),
                      definition_source,
                    );
                  }
                }
                _ => {}
              }
              Ok::<bool, Never>(true)
            },
            &mut ImmutableProgramLocalContext::empty(self),
          )
          .unwrap();
      }
    }
    sites
  }
  pub fn find_fn_names_by_entry_point(
    &self,
    entry_kind_predicate: impl Fn(EntryPoint) -> bool,
  ) -> Vec<String> {
    self
      .abstract_functions_iter()
      .filter_map(|abstract_f| {
        let abstract_f = abstract_f.read().unwrap();
        if let FunctionImplementationKind::Composite(f) =
          &abstract_f.implementation
          && let Some(entry_point) = f.read().unwrap().entry_point
          && entry_kind_predicate(entry_point)
        {
          Some(abstract_f.name.to_string())
        } else {
          None
        }
      })
      .collect()
  }
  pub fn cpu_entry_points(
    &self,
  ) -> Vec<Arc<RwLock<AbstractFunctionSignature>>> {
    self
      .abstract_functions_iter()
      .filter(|f| {
        let f = f.read().unwrap();
        if let FunctionImplementationKind::Composite(comp) = &f.implementation {
          comp.read().unwrap().entry_point == Some(EntryPoint::Cpu)
        } else {
          false
        }
      })
      .cloned()
      .collect()
  }
  pub fn find_definition(
    &self,
    name: &str,
    path: &Vec<usize>,
  ) -> Option<NameDefinitionSource> {
    for e in self.typedefs.enums.iter() {
      let e = e.original_ancestor();
      if &*e.name.0 == name {
        return Some(NameDefinitionSource::Enum(e.name.1.primary_path()));
      }
    }
    for t in self.typedefs.enums.iter() {
      let t = t.original_ancestor();
      if &*t.name.0 == name {
        return Some(NameDefinitionSource::Enum(t.name.1.primary_path()));
      }
    }
    let mut defn_locations: HashSet<Vec<usize>> = HashSet::new();
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      if &*f.name == name {
        if let FunctionImplementationKind::Composite(f) = &f.implementation {
          defn_locations
            .insert(f.read().unwrap().name_source_trace.primary_path());
        }
      }
    }
    if !defn_locations.is_empty() {
      return Some(NameDefinitionSource::Defn(
        defn_locations.into_iter().collect(),
      ));
    }
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      if let FunctionImplementationKind::Composite(f) = &f.implementation {
        let mut definition_source: Option<NameDefinitionSource> = None;
        fn is_prefix(a: &Vec<usize>, b: &Vec<usize>) -> bool {
          a.len() < b.len()
            && a.iter().zip(b.iter()).find(|(a, b)| a != b).is_none()
        }
        f.read()
          .unwrap()
          .expression
          .walk(&mut |exp| {
            let exp_path = exp.source_trace.primary_path();
            if is_prefix(&exp_path, path) {
              return Ok(false);
            }
            match &exp.kind {
              ExpKind::ForLoop {
                increment_variable_name,
                ..
              } => {
                if &*increment_variable_name.0 == name {
                  definition_source = Some(NameDefinitionSource::LocalBinding(
                    increment_variable_name.1.primary_path(),
                  ))
                }
              }
              ExpKind::Let(bindings, _) => {
                let bindings_to_consider = if path[exp_path.len()] == 1 {
                  // path being searched for is inside bindings
                  if let Some(internal_binding_index) =
                    path.get(exp_path.len() + 1)
                    && internal_binding_index % 2 == 1
                  {
                    let internal_binding_index = internal_binding_index / 2;
                    internal_binding_index.checked_sub(1).unwrap_or(0)
                  } else {
                    0
                  }
                } else {
                  // path being searched for is inside body
                  bindings.len()
                };
                for (binding_name, binding_source_trace, _, _) in
                  bindings.iter().take(bindings_to_consider).rev()
                {
                  if &**binding_name == name {
                    definition_source =
                      Some(NameDefinitionSource::LocalBinding(
                        binding_source_trace.primary_path(),
                      ));
                    break;
                  }
                }
              }
              ExpKind::Match(_, arms) => {
                for (pattern, arm_body) in arms.iter() {
                  if is_prefix(&arm_body.source_trace.primary_path(), path) {
                    match &pattern.kind {
                      ExpKind::Name(pattern_name) => {
                        if &**pattern_name == name {
                          definition_source =
                            Some(NameDefinitionSource::LocalBinding(
                              pattern.source_trace.primary_path(),
                            ));
                        }
                      }
                      ExpKind::Application(_, args) => {
                        for arg in args.iter() {
                          if let ExpKind::Name(pattern_name) = &arg.kind {
                            if &**pattern_name == name {
                              Some(NameDefinitionSource::LocalBinding(
                                arg.source_trace.primary_path(),
                              ));
                            }
                          }
                        }
                      }
                      _ => {}
                    }
                  }
                }
              }
              _ => {}
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
    None
  }
  pub fn composite_functions_in_usage_order(
    &self,
  ) -> Vec<(Arc<str>, Arc<RwLock<TopLevelFunction>>)> {
    self.composite_functions_in_usage_order_with_discovery(false)
  }
  /// With `discover_scope_closures`, also includes composite functions
  /// reachable only through type-level ancestor references (closures used
  /// exclusively via scope constructions, which the reference-address-space
  /// rebuild drops from the registry). The VM CPU runtime needs those
  /// compiled; WGSL/C/audio emission must not see them.
  pub fn composite_functions_in_usage_order_with_discovery(
    &self,
    discover_scope_closures: bool,
  ) -> Vec<(Arc<str>, Arc<RwLock<TopLevelFunction>>)> {
    let mut dependencies: HashMap<Arc<str>, HashSet<Arc<str>>> = HashMap::new();
    let mut fns: Vec<(Arc<str>, Arc<RwLock<TopLevelFunction>>)> = vec![];
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap().clone();
      if f.generic_args.is_empty()
        && !f.has_uninlined_higher_order_arguments()
        && let FunctionImplementationKind::Composite(implementation) =
          f.implementation
      {
        fns.push((f.name.clone(), implementation.clone()));
        dependencies.insert(f.name.clone(), HashSet::new());
      }
    }
    // Closures referenced only through scope constructions aren't in the
    // abstract-function registry (reference-address-space monomorphization
    // rebuilds the program from name-called functions only); they're
    // reachable exclusively through type-level ancestor Arcs, like the
    // interpreter reaches them. Discover those transitively.
    if discover_scope_closures {
      let mut queue: Vec<Arc<RwLock<TopLevelFunction>>> =
        fns.iter().map(|(_, f)| f.clone()).collect();
      while let Some(f) = queue.pop() {
        let mut discovered: Vec<(Arc<str>, Arc<RwLock<TopLevelFunction>>)> =
          vec![];
        f.read()
          .unwrap()
          .expression
          .walk(&mut |exp| {
            if let ExpKind::Application(_, _) = &exp.kind
              && let TypeState::Known(Type::Function(signature)) =
                &exp.data.kind
              && let Some(ancestor) = &signature.abstract_ancestor
            {
              let ancestor = ancestor.read().unwrap();
              if let FunctionImplementationKind::Composite(implementation) =
                &ancestor.implementation
                && !dependencies.contains_key(&ancestor.name)
              {
                discovered
                  .push((ancestor.name.clone(), implementation.clone()));
              }
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
        for (name, implementation) in discovered {
          dependencies.insert(name.clone(), HashSet::new());
          queue.push(implementation.clone());
          fns.push((name, implementation));
        }
      }
    }
    for (f_name, f) in fns.iter() {
      f.read()
        .unwrap()
        .expression
        .walk(&mut |exp| {
          if let ExpKind::Name(other_f_name) = &exp.kind {
            if dependencies.contains_key(other_f_name) {
              dependencies
                .get_mut(f_name)
                .unwrap()
                .insert(other_f_name.clone());
            }
          }
          // A closure's scope construction references the closure only
          // through its expression *type* (the applied name is the scope
          // struct's constructor), so also count type-level function
          // ancestors as usages.
          if let ExpKind::Application(_, _) = &exp.kind
            && let TypeState::Known(Type::Function(signature)) = &exp.data.kind
            && let Some(ancestor) = &signature.abstract_ancestor
          {
            let ancestor_name = ancestor.read().unwrap().name.clone();
            if dependencies.contains_key(&ancestor_name) {
              dependencies.get_mut(f_name).unwrap().insert(ancestor_name);
            }
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
    let mut final_fns: Vec<(Arc<str>, Arc<RwLock<TopLevelFunction>>)> = vec![];
    while !fns.is_empty() {
      let mut broke = false;
      for i in 0..fns.len() {
        if dependencies.get(&fns[i].0).unwrap().is_empty() {
          let (name, implementation) = fns.remove(i);
          for remaining_dependencies in dependencies.values_mut() {
            remaining_dependencies.remove(&name);
          }
          final_fns.push((name, implementation));
          broke = true;
          break;
        }
      }
      if !broke {
        panic!(
          "Couldn't find topological sort of user functions.\n\
           If you're seeing this, there's a compiler bug; an earlier compiler \
           stage should have caught the dependency loop "
        )
      }
    }
    final_fns
  }
  /// Statically determines which top-level vars are shared across CPU
  /// threads: touched (read or written) by code reachable from more than
  /// one thread root. Thread roots today are the `@cpu` entry points (the
  /// main thread — GPU work dispatched from main attributes to main, since
  /// the GPU syncs against main's replica through its own machinery) and
  /// the `@audio` entry points (the start-audio thread). Reachability
  /// follows function references — names and type-level function ancestors,
  /// so scoped closures count — EXCEPT the function argument of
  /// `start-audio`: that reference is where the other thread *begins*, not
  /// a main-thread use of the function.
  ///
  /// Returns the shared variable names sorted, so every compiled artifact
  /// of the program carries the same index-aligned list (the runtime's
  /// `ThreadSharedTable` slots are addressed by these indices).
  /// The thread-shared globals with their audience masks (see
  /// `thread_sync::participant`), sorted by name. A var is shared when
  /// it's reachable from both the `@cpu` and `@audio` entry roots, or when
  /// it's marked `@external` (embedder access is invisible to static
  /// analysis, so the annotation forces membership). The sorted order
  /// index-aligns every artifact's `Code::shared_vars`, the env's
  /// `shared_globals`, `ExternalVars` handles, and the
  /// `ThreadSharedTable`'s slots.
  /// The top-level vars actually referenced by GPU code: the union of every
  /// GPU entry point's effect-derived global reads and writes (including
  /// length-only reads — WGSL's `arrayLength` derives from buffer size, so
  /// a length-read still needs the binding uploaded), plus every
  /// handle-space (texture) var, since texture use isn't effect-tracked.
  /// Only vars in this set become runtime GPU bindings: a GPU-space var no
  /// shader touches is an ordinary CPU value with zero sync obligations
  /// (no buffer, no uploads, no readbacks), though its declaration still
  /// appears in emitted WGSL when its type is host-shareable.
  pub fn gpu_used_globals(&self) -> HashSet<Arc<str>> {
    let var_names: HashSet<Arc<str>> =
      self.top_level_vars.iter().map(|v| v.name.clone()).collect();
    let mut used: HashSet<Arc<str>> = HashSet::new();
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      if f
        .entry_point
        .map(|e| {
          matches!(
            e,
            EntryPoint::Vertex | EntryPoint::Fragment | EntryPoint::Compute(_)
          )
        })
        .unwrap_or(false)
        && let FunctionImplementationKind::Composite(implementation) =
          &f.implementation
      {
        let (reads, writes) = implementation
          .read()
          .unwrap()
          .effects()
          .gpu_read_and_written_globals();
        used.extend(
          reads
            .into_iter()
            .chain(writes.into_iter())
            .filter(|name| var_names.contains(name)),
        );
      }
    }
    used.extend(self.top_level_vars.iter().filter_map(|v| {
      matches!(
        v.kind,
        TopLevelVariableKind::Var {
          address_space: VariableAddressSpace::Handle,
          ..
        }
      )
      .then(|| v.name.clone())
    }));
    used
  }
  /// Rejects GPU-space vars whose types can't be host-shared (bool- or
  /// String-containing) — but only when GPU code actually touches them. A
  /// storage-write bool var no shader references is an ordinary CPU value
  /// (usable, e.g., as main↔audio shared state); it's simply skipped from
  /// WGSL emission. Must run after every pass that rewrites GPU entry
  /// bodies (window-info extraction, dispatched-closure lifts), since
  /// GPU usage is derived from entry-point effects.
  pub fn validate_gpu_used_binding_types(&self, errors: &mut ErrorLog) {
    let gpu_used = self.gpu_used_globals();
    for v in self.top_level_vars.iter() {
      if let TopLevelVariableKind::Var {
        address_space:
          VariableAddressSpace::Uniform
          | VariableAddressSpace::StorageRead
          | VariableAddressSpace::StorageReadWrite,
        ..
      } = v.kind
        && gpu_used.contains(&v.name)
      {
        if v.var_type.involves_bool() {
          errors.log(CompileError::new(
            UnshareableBindingType("bool".to_string()),
            v.source_trace.clone(),
          ));
        } else if v.var_type.involves_string() {
          errors.log(CompileError::new(
            UnshareableBindingType("String".to_string()),
            v.source_trace.clone(),
          ));
        }
      }
    }
  }
  pub fn thread_shared_globals(&self) -> Vec<(Arc<str>, u32)> {
    use crate::compiler::expression::ExpKind;
    let var_names: HashSet<Arc<str>> =
      self.top_level_vars.iter().map(|v| v.name.clone()).collect();
    let globals_reachable_from = |root_entry: fn(&EntryPoint) -> bool| {
      let mut visited: HashSet<Arc<str>> = HashSet::new();
      let mut queue: Vec<Arc<RwLock<TopLevelFunction>>> = vec![];
      for f in self.abstract_functions_iter() {
        let f = f.read().unwrap();
        if f.entry_point.map(|e| root_entry(&e)).unwrap_or(false)
          && let FunctionImplementationKind::Composite(implementation) =
            &f.implementation
          && visited.insert(f.name.clone())
        {
          queue.push(implementation.clone());
        }
      }
      let mut globals: HashSet<Arc<str>> = HashSet::new();
      while let Some(f) = queue.pop() {
        let f = f.read().unwrap();
        let effects = f.effects();
        let (reads, writes) = effects.read_and_written_globals();
        // Seed writes (`Effect::SeedsGlobalVar`) count as touches here —
        // this is how the analysis sees the main thread writing the lifted
        // audio-closure capture globals inside `start-audio` — but stay
        // invisible to the runtime dirty-marking machinery, which the
        // builtin drives itself on the call that actually seeds.
        globals.extend(
          reads
            .into_iter()
            .chain(writes.into_iter())
            .chain(effects.seeded_globals().into_iter())
            .filter(|name| var_names.contains(name)),
        );
        let mut discovered: Vec<(Arc<str>, Arc<RwLock<TopLevelFunction>>)> =
          vec![];
        f.expression
          .walk(&mut |exp| {
            // the function argument of `start-audio` belongs to the audio
            // thread, not to whichever thread calls `start-audio`
            if let ExpKind::Application(applied_f, _) = &exp.kind
              && let ExpKind::Name(applied_name) = &applied_f.kind
              && &**applied_name == "start-audio"
            {
              return Ok::<bool, Never>(false);
            }
            // any function-typed expression carrying a composite ancestor
            // is a reference this thread could invoke (covers plain names,
            // application callees, and scope constructions)
            if let TypeState::Known(Type::Function(signature)) = &exp.data.kind
              && let Some(ancestor) = &signature.abstract_ancestor
            {
              let ancestor = ancestor.read().unwrap();
              if let FunctionImplementationKind::Composite(implementation) =
                &ancestor.implementation
                && !visited.contains(&ancestor.name)
              {
                discovered
                  .push((ancestor.name.clone(), implementation.clone()));
              }
            }
            Ok(true)
          })
          .unwrap();
        for (name, implementation) in discovered {
          if visited.insert(name) {
            queue.push(implementation);
          }
        }
      }
      globals
    };
    let main_globals = globals_reachable_from(|e| matches!(e, EntryPoint::Cpu));
    let audio_globals =
      globals_reachable_from(|e| matches!(e, EntryPoint::Audio));
    let external_globals: HashSet<Arc<str>> = self
      .top_level_vars
      .iter()
      .filter(|v| v.external)
      .map(|v| v.name.clone())
      .collect();
    // `@local` vars are never shared: one independent copy per execution
    // context — a CPU thread's local is exactly like a GPU invocation's
    // `var<private>`. Only vars in the GPU-bindable (host-shareable)
    // address spaces participate in cross-thread sharing.
    let local_vars: HashSet<Arc<str>> = self
      .top_level_vars
      .iter()
      .filter(|v| {
        matches!(
          v.kind,
          TopLevelVariableKind::Var {
            address_space: VariableAddressSpace::Local,
            ..
          }
        )
      })
      .map(|v| v.name.clone())
      .collect();
    let mut shared: Vec<(Arc<str>, u32)> = var_names
      .iter()
      .filter_map(|name| {
        if local_vars.contains(name) {
          return None;
        }
        let audience = if main_globals.contains(name) {
          participant::MAIN
        } else {
          0
        } | if audio_globals.contains(name) {
          participant::AUDIO
        } else {
          0
        } | if external_globals.contains(name) {
          participant::EXTERNAL
        } else {
          0
        };
        let statically_shared = audience
          & (participant::MAIN | participant::AUDIO)
          == participant::MAIN | participant::AUDIO;
        (statically_shared || audience & participant::EXTERNAL != 0)
          .then(|| (name.clone(), audience))
      })
      .collect();
    shared.sort();
    shared
  }
  /// Audio-mode bytecode compilation: pure math only, CPU-exclusive
  /// functions skipped, no host calls emitted.
  pub fn compile_to_bytecode_program(self) -> (BytecodeProgram, Vec<Arc<str>>) {
    self.compile_to_bytecode_program_impl(false)
  }
  /// CPU-runtime-mode bytecode compilation: compiles the `@cpu` entry and
  /// its transitive callees, lowering CPU-exclusive builtins to host ops and
  /// emitting explicit GPU↔CPU sync instructions from effect analysis.
  pub fn compile_to_bytecode_program_cpu(
    self,
  ) -> (BytecodeProgram, Vec<Arc<str>>) {
    self.compile_to_bytecode_program_impl(true)
  }
  fn compile_to_bytecode_program_impl(
    self,
    cpu_mode: bool,
  ) -> (BytecodeProgram, Vec<Arc<str>>) {
    use crate::vm::bytecode::{
      HostBinding, HostBindingStorage, SharedVarInfo, SharedVarStorage,
    };
    let mut state = BytecodeCompilationState::new();
    state.cpu_mode = cpu_mode;
    state.monomorphized_to_base_names =
      self.names.read().unwrap().monomorphized_to_base_names();
    // Thread-shared globals: same sorted list in every compiled artifact,
    // so `MarkSharedDirty` indices and `ThreadSharedTable` slots agree
    // between the main program and the audio program.
    let shared_globals = self.thread_shared_globals();
    state.shared_vars = vec![None; shared_globals.len()];
    state.shared_var_indices = shared_globals
      .iter()
      .enumerate()
      .map(|(index, (name, _))| (name.clone(), index as u16))
      .collect();
    let shared_var_audiences: HashMap<Arc<str>, u32> =
      shared_globals.into_iter().collect();
    // Only vars actually referenced by GPU code carry sync obligations —
    // a GPU-space var no shader touches is an ordinary CPU value, so it
    // gets no host binding (no `CheckGpuToCpu`/`MarkCpuWritten` emission,
    // no upload serialization).
    let gpu_used = if cpu_mode {
      self.gpu_used_globals()
    } else {
      HashSet::new()
    };
    let mut dyn_memory_count: u16 = 0;
    for v in self.top_level_vars.iter() {
      let is_dynamic_array = matches!(
        &v.var_type,
        Type::Array(Some(ConcreteArraySize::Unsized), _)
      );
      let is_texture = matches!(
        v.kind,
        TopLevelVariableKind::Var {
          address_space: VariableAddressSpace::Handle,
          ..
        }
      );
      let binding_info = if cpu_mode
        && gpu_used.contains(&v.name)
        && let TopLevelVariableKind::Var {
          address_space,
          group_and_binding: Some(binding_spec),
        } = v.kind
        && matches!(
          address_space,
          VariableAddressSpace::Uniform
            | VariableAddressSpace::StorageRead
            | VariableAddressSpace::StorageReadWrite
            | VariableAddressSpace::Handle
        ) {
        Some((binding_spec.specified(), address_space))
      } else {
        None
      };
      if is_dynamic_array {
        // Runtime-sized arrays live in the VM's flat dynamic memory
        // (`BytecodeProgram::dyn_memory`), outside the u16-addressed stack;
        // element and length accesses compile to the direct `Dyn*` opcodes.
        // This applies in audio mode too: a program with an `@audio` entry
        // may declare runtime-sized globals its audio code never touches,
        // and they must not break the (eager) audio compile. They may or
        // may not be GPU-bound (a plain `(var x: [f32])` is legal).
        let Type::Array(_, element_type) = &v.var_type else {
          unreachable!()
        };
        // `vm_type_size`, not `data_size_in_u32s`: heap-backed element
        // types (nested arrays, strings) size as one id word — their
        // regions use `DynMemory::Cells` storage, where the stride is
        // never consulted.
        let element_stride = vm_type_size(&element_type.unwrap_known());
        let memory = dyn_memory_count;
        dyn_memory_count += 1;
        state
          .dynamic_array_memory
          .insert(v.name.clone(), (memory, element_stride));
        state.dynamic_array_types.insert(memory, v.var_type.clone());
        if let Some(shared_index) =
          state.shared_var_indices.get(&v.name).copied()
        {
          state.shared_vars[shared_index as usize] = Some(SharedVarInfo {
            name: v.name.clone(),
            ty: v.var_type.clone(),
            audience: shared_var_audiences[&v.name],
            storage: SharedVarStorage::DynMemory {
              region: memory,
              stride: element_stride,
            },
          });
        }
        if cpu_mode {
          // host-binding entry for GPU sync bookkeeping and whole-array
          // printing
          let index = state.host_bindings.len() as u16;
          state.host_bindings.push(HostBinding {
            name: v.name.clone(),
            ty: v.var_type.clone(),
            storage: HostBindingStorage::DynamicMemory { memory },
            gpu: binding_info
              .map(|(gb, address_space)| (gb.group, gb.binding, address_space)),
          });
          state.binding_indices.insert(v.name.clone(), index);
          state.dynamic_globals.insert(v.name.clone(), index);
        }
        continue;
      }
      if is_texture {
        if cpu_mode {
          // Textures live host-side as `Value`s; VM code accesses them
          // through host ops, so they get no slots.
          let index = state.host_bindings.len() as u16;
          state.host_bindings.push(HostBinding {
            name: v.name.clone(),
            ty: v.var_type.clone(),
            storage: HostBindingStorage::Dynamic,
            gpu: binding_info
              .map(|(gb, address_space)| (gb.group, gb.binding, address_space)),
          });
          state.binding_indices.insert(v.name.clone(), index);
          state.dynamic_globals.insert(v.name.clone(), index);
        }
        // audio mode: texture accesses are CPU-exclusive, so audio-reachable
        // code can never touch this global — skip it entirely
        continue;
      }
      let position = state.consumed_stack_space as u16;
      let size = v.var_type.data_size_in_u32s(&v.source_trace).unwrap() as u16;
      state.globals.insert(v.name.clone(), position);
      state.global_slots.push((v.name.clone(), position, size));
      state.global_types.push(v.var_type.clone());
      if let Some(shared_index) = state.shared_var_indices.get(&v.name).copied()
      {
        state.shared_vars[shared_index as usize] = Some(SharedVarInfo {
          name: v.name.clone(),
          ty: v.var_type.clone(),
          audience: shared_var_audiences[&v.name],
          storage: SharedVarStorage::Slots { position, size },
        });
      }
      state.consumed_stack_space += size;
      if let Some((gb, address_space)) = binding_info {
        let index = state.host_bindings.len() as u16;
        state.host_bindings.push(HostBinding {
          name: v.name.clone(),
          ty: v.var_type.clone(),
          storage: HostBindingStorage::Slots { position, size },
          gpu: Some((gb.group, gb.binding, address_space)),
        });
        state.binding_indices.insert(v.name.clone(), index);
      }
    }
    // If any top-level var has an initializer expression, compile a
    // synthetic "$init_globals" function that computes each one and Moves it
    // into the corresponding global slot. `BytecodeProgram::from_code` runs
    // it once at construction so globals are live before any user code.
    let init_function_index =
      if self.top_level_vars.iter().any(|v| v.value.is_some()) {
        state.open_function("$init_globals".into(), 0);
        for v in self.top_level_vars.iter() {
          if let Some(value_exp) = &v.value {
            let value_slot =
              value_exp.compile_to_bytecode(false, &mut state).unwrap();
            let var_size =
              v.var_type.data_size_in_u32s(&v.source_trace).unwrap() as u16;
            let global_slot = *state.globals.get(&v.name).unwrap();
            state.push_instruction(Instruction {
              op: Op::Move,
              arg_positions: [value_slot, var_size, 0],
              return_position: global_slot,
            });
          }
        }
        state.close_function();
        Some(state.finished_functions.len() - 1)
      } else {
        None
      };
    let ordered_functions =
      self.composite_functions_in_usage_order_with_discovery(cpu_mode);
    // CPU mode compiles only functions actually reachable from `@cpu`
    // entries. Anything referenced solely from GPU entry points (e.g. the
    // callbacks a compute shader invokes) must not be compiled for the CPU —
    // it may legally do GPU-only things like passing storage-array elements
    // by reference to atomics.
    let cpu_reachable: Option<HashSet<Arc<str>>> = if cpu_mode {
      let by_name: HashMap<Arc<str>, Arc<RwLock<TopLevelFunction>>> =
        ordered_functions
          .iter()
          .map(|(n, f)| (n.clone(), f.clone()))
          .collect();
      let is_cpu_compilable = |f: &Arc<RwLock<TopLevelFunction>>| {
        f.read()
          .unwrap()
          .entry_point
          .map(|e| matches!(e, EntryPoint::Cpu))
          .unwrap_or(true)
      };
      let mut reachable: HashSet<Arc<str>> = HashSet::new();
      let mut queue: Vec<Arc<RwLock<TopLevelFunction>>> = vec![];
      for (name, f) in &ordered_functions {
        let is_cpu_entry = f
          .read()
          .unwrap()
          .entry_point
          .map(|e| matches!(e, EntryPoint::Cpu))
          .unwrap_or(false);
        if is_cpu_entry && reachable.insert(name.clone()) {
          queue.push(f.clone());
        }
      }
      while let Some(f) = queue.pop() {
        let mut found: Vec<Arc<str>> = vec![];
        f.read()
          .unwrap()
          .expression
          .walk(&mut |exp| {
            if let ExpKind::Name(name) = &exp.kind
              && by_name.contains_key(name)
            {
              found.push(name.clone());
            }
            if let ExpKind::Application(_, _) = &exp.kind
              && let TypeState::Known(Type::Function(signature)) =
                &exp.data.kind
              && let Some(ancestor) = &signature.abstract_ancestor
            {
              found.push(ancestor.read().unwrap().name.clone());
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
        for name in found {
          if let Some(target) = by_name.get(&name)
            && is_cpu_compilable(target)
            && reachable.insert(name)
          {
            queue.push(target.clone());
          }
        }
      }
      Some(reachable)
    } else {
      None
    };
    for (f_name, implementation) in ordered_functions {
      if let Some(reachable) = &cpu_reachable
        && !reachable.contains(&f_name)
      {
        continue;
      }
      // Filter the same way the C backend does in
      // `TopLevelFunction::compile`: skip entry points whose kind doesn't
      // compile to VM (right now that's everything except `@audio`), and
      // skip any function whose effects include a CPU-exclusive call or
      // type. The second filter catches helper functions that are only
      // reachable through the `@cpu` entry — without it, `compile_to_
      // bytecode` would hit `todo!()` on `spawn-window` / `window-frame-
      // index` / etc.
      {
        let implementation_read = implementation.read().unwrap();
        // Ref-arg detection keys off signature-level ownership, not
        // arg_annotations: annotations only reflect user-written `@ref`
        // args, while params created by lowering (closure scope args from
        // extract_inner_functions) are reference-typed only in the
        // signature.
        let Type::Function(f_signature) =
          implementation_read.expression.data.unwrap_known()
        else {
          panic!()
        };
        let has_ref_args = f_signature
          .args
          .iter()
          .any(|(v, _)| v.var_type.ownership != Ownership::Owned);
        if has_ref_args {
          state
            .ref_arg_functions
            .push((f_name, implementation.clone()));
          continue;
        }
        let skip = if cpu_mode {
          // CPU mode: compile `@cpu` entries and plain functions; skip GPU
          // and audio entry points, plus any helper that's only meaningful
          // inside a shader (fragment-exclusive calls, GPU builtin-attribute
          // lookups like `global-invocation-id`) — those are reachable only
          // from GPU entries, and compiling them would hit shader-only
          // builtins.
          let is_non_cpu_entry = implementation_read
            .entry_point
            .map(|e| !matches!(e, EntryPoint::Cpu))
            .unwrap_or(false);
          let gpu_only = implementation_read.effects().0.iter().any(|e| {
            matches!(
              e,
              Effect::FragmentExclusiveFunction(_)
                | Effect::LookupBuiltinAttribute(_)
            )
          });
          is_non_cpu_entry || gpu_only
        } else {
          let skip_for_entry_point = implementation_read
            .entry_point
            .map(|e| !e.should_compile_to_target(CompilerTarget::VM))
            .unwrap_or(false);
          let effects = implementation_read.effects();
          let has_cpu_exclusive = !effects.cpu_exclusive_functions().is_empty()
            || !effects.cpu_exclusive_types().is_empty()
            || !effects.window_info_kinds().is_empty()
            // `print` has no audio-target implementation; a printing
            // function can only be meant for the CPU side. (A frame closure
            // that never calls a CPU-exclusive builtin would otherwise slip
            // through this filter and hit the `todo!()` on `print`.)
            || effects.0.contains(&Effect::Print);
          skip_for_entry_point || has_cpu_exclusive
        };
        if skip {
          continue;
        }
      }
      implementation.read().unwrap().compile_to_bytecode(
        &f_name,
        &mut state,
        &[],
      );
      while !state.pending_ref_arg_function_usages.is_empty()
        || !state.pending_frame_fn_usages.is_empty()
      {
        for PendingRefFnUsage {
          name,
          fn_dispatch_position,
          arg_move_positions,
          return_move_position,
          arg_positions,
        } in state
          .pending_ref_arg_function_usages
          .drain(..)
          .collect::<Vec<_>>()
        {
          state.instructions[fn_dispatch_position as usize].arg_positions[0] =
            state.finished_functions.len() as u16;
          let f = state
            .ref_arg_functions
            .iter()
            .find_map(|(f_name, f)| (name == *f_name).then(|| f))
            .unwrap()
            .clone();
          let f = f.read().unwrap();
          let Type::Function(f_signature) = f.expression.data.unwrap_known()
          else {
            panic!()
          };
          let mut ref_arg_positions = vec![];
          for (i, (v, _)) in f_signature.args.iter().enumerate() {
            if v.var_type.ownership != Ownership::Owned {
              ref_arg_positions.push((i, arg_positions[i]));
            }
          }
          f.compile_to_bytecode(&name, &mut state, &ref_arg_positions);
          let bytecode_fn = state.finished_functions.last().unwrap();
          for (owned_arg_index, move_position) in arg_move_positions {
            let move_instruction =
              &mut state.instructions[move_position as usize];
            move_instruction.arg_positions[0] =
              arg_positions[owned_arg_index].owned_slot();
            move_instruction.arg_positions[1] =
              bytecode_fn.arg_sizes[owned_arg_index];
            move_instruction.return_position =
              bytecode_fn.arg_positions[owned_arg_index];
          }
          state.instructions[return_move_position as usize].arg_positions[0] =
            bytecode_fn.stack_frame_start;
        }
        for PendingFrameFnUsage {
          name,
          host_op_index,
          scope_slot,
        } in state.pending_frame_fn_usages.drain(..).collect::<Vec<_>>()
        {
          let f = state
            .ref_arg_functions
            .iter()
            .find_map(|(f_name, f)| (name == *f_name).then(|| f))
            .unwrap()
            .clone();
          let f = f.read().unwrap();
          let Type::Function(f_signature) = f.expression.data.unwrap_known()
          else {
            panic!()
          };
          // The scope param is by construction the frame fn's trailing arg;
          // bind it directly to the slots materialized at the spawn-window
          // site, then point the HostOp at the specialized copy.
          let scope_arg_index = f_signature.args.len() - 1;
          f.compile_to_bytecode(
            &name,
            &mut state,
            &[(scope_arg_index, RefArgBinding::Slot(scope_slot))],
          );
          let frame_fn = (state.finished_functions.len() - 1) as u16;
          state.host_ops[host_op_index] =
            crate::vm::bytecode::HostOp::SpawnWindow { frame_fn };
        }
      }
    }
    state.finalize(init_function_index)
  }
}
