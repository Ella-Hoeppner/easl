use std::collections::{HashMap, HashSet};

use std::hash::Hash;
use std::sync::{Arc, RwLock};

use fsexp::{Ast, document::Document, syntax::EncloserOrOperator};
use take_mut::take;

use crate::compiler::builtins::{
  EmulatedFunctionRecord, EmulatedFunctionSignature,
  built_in_structs_for_target,
};
use crate::compiler::types::{ExpTypeInfo, PRIMITIVE_TYPE_NAMES};
use crate::compiler::vars::{
  BindingSpec, GroupAndBinding, VariableAddressSpace,
};
use crate::parse::EaslMultiDocument;
use crate::thread_sync::participant;
use crate::vm::bytecode::BytecodeProgram;
use crate::vm::compile::{
  BytecodeCompilationState, CompilePosition, PendingFrameFnUsage,
  PendingRefFnUsage, RefArgBinding, is_heap_value_type, vm_stack_size,
};
use crate::{
  Never,
  compiler::{
    annotation::extract_annotation,
    builtins::built_in_functions,
    effects::{Effect, EffectType, WindowInfoBindingSource, WindowInfoKind},
    entry::{
      BuiltinIOAttribute, EntryPoint, IOAttribute, IOAttributeKind,
      IOAttributes, InputOrOutput,
    },
    enums::{AbstractEnum, UntypedEnum},
    error::{CompileError, SourceTrace},
    exp_builder::{ExpBuilder, struct_constructor},
    expression::{
      Accessor, ExpKind, ExpressionCompilationPosition, Number,
      ReferenceVariants,
    },
    function_values::holds_function,
    functions::{
      AbstractFunctionSignature, EffectsMemo, FunctionArgumentAnnotation,
      FunctionSignature, FunctionTargetConfiguration, Ownership,
      TopLevelFunction,
    },
    graph::topological_order,
    structs::{AbstractStructField, UntypedStruct},
    types::{
      AbstractArraySize, AbstractType, ConcreteArraySize,
      ConstGenericResolution, GenericArgument, ImmutableProgramLocalContext,
      NameDefinitionSource, Type, TypeDescription, TypeState, UntypedType,
      Variable, VariableKind, parse_generic_argument,
    },
    util::{compile_word, is_valid_name, is_valid_qualified_name},
    vars::TopLevelVariableKind,
    wgsl::{FRAGMENT_POSITION_OFFSET_VAR, is_easl_reserved_word},
  },
  parse::{EaslSyntax, EaslTree, Encloser, Operator, parse_easl},
};

use super::{
  builtins::{
    ABNORMAL_CONSTRUCTOR_STRUCTS, built_in_enums, built_in_structs,
    built_in_type_aliases,
  },
  closure_lift::{CaptureLift, ClosureLiftTarget, LiftedCaptures, SplitGlobal},
  error::{
    CompileErrorKind::{self, *},
    CompileResult, ErrorLog,
  },
  expression::TypedExp,
  functions::FunctionImplementationKind,
  macros::{Macro, macroexpand},
  modules::{MainFileIndex, display_name, resolve_modules},
  structs::AbstractStruct,
  vars::TopLevelVar,
};

pub type EaslDocument = Document<EaslSyntax>;

/// Optional behavior for `Program::validate_raw_program_with`; every
/// option is off by default.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ValidationOptions {
  /// Compiles for a runtime that may draw a window a region at a time
  /// (`GpuCore::execute_render_batch_to_window_region`): fragment entries'
  /// `position` stays in window pixels there, at the cost of a uniform
  /// read and two adds per fragment (`Program::offset_fragment_positions`).
  /// Off, `position` is the raw WGSL builtin, and a region render reports
  /// positions relative to the target.
  pub window_regions: bool,
}

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

/// The registered signature a function reference denotes. References carry
/// signature copies, which share the registered signature's implementation
/// but not its name: overload separation renames only registered signatures.
/// So a name read after separation must come from here, never the copy.
pub(crate) fn registered_signature(
  reference: &Arc<RwLock<AbstractFunctionSignature>>,
  registered_by_implementation: &HashMap<
    usize,
    Arc<RwLock<AbstractFunctionSignature>>,
  >,
) -> Arc<RwLock<AbstractFunctionSignature>> {
  match &reference.read().unwrap().implementation {
    FunctionImplementationKind::Composite(implementation) => {
      registered_by_implementation
        .get(&(Arc::as_ptr(implementation) as usize))
        .cloned()
        .unwrap_or_else(|| reference.clone())
    }
    _ => reference.clone(),
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
          )) = children.get(1)
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
        .with_enums(built_in_enums())
        .with_type_aliases(built_in_type_aliases()));
}

#[derive(Debug, Clone)]
pub struct NameContext {
  user_names: HashSet<Arc<str>>,
  generated_names: HashSet<Arc<str>>,
  monomorphized_names: HashMap<(Arc<str>, Vec<Arc<str>>), Arc<str>>,
  /// For each base name `gensym` has suffixed, the suffix to try next: names
  /// are only ever added, so every lower suffix is still taken.
  next_gensym_suffixes: HashMap<Arc<str>, usize>,
}

impl NameContext {
  fn empty() -> Self {
    Self {
      user_names: HashSet::new(),
      generated_names: HashSet::new(),
      monomorphized_names: HashMap::new(),
      next_gensym_suffixes: HashMap::new(),
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
    // A name and its compiled form are tracked together, so a tracked name
    // needs nothing more (and skips allocating both again).
    if self.user_names.contains(name) {
      return;
    }
    self.user_names.insert(name.into());
    self.user_names.insert(compile_word(name.into()).into());
  }
  fn is_taken(&self, name: &str) -> bool {
    self.user_names.contains(name) || self.generated_names.contains(name)
  }
  pub fn gensym(&mut self, base_name: &str) -> Arc<str> {
    if self.is_taken(base_name) {
      let mut i = self
        .next_gensym_suffixes
        .get(base_name)
        .copied()
        .unwrap_or(0);
      let final_name: Arc<str> = loop {
        let modified_name = base_name.to_string() + &format!("_{i}");
        if !self.is_taken(&modified_name) {
          break modified_name.into();
        }
        i += 1;
      };
      self.next_gensym_suffixes.insert(base_name.into(), i + 1);
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
    EntryPoint::Compute([1, 1, 1]),
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

/// A target's emitted functions (see `Program::emitted_function_closure`).
struct EmittedFunctions {
  /// Every composite function, callees first (the deterministic emission
  /// order).
  ordered: Vec<(Arc<str>, Arc<RwLock<TopLevelFunction>>)>,
  /// The reachable subset of `ordered`: the functions that emit.
  emitted: HashSet<Arc<str>>,
  /// The functions that are themselves valid for the target
  /// (`TopLevelFunction::is_valid_for_target`); an emitted function outside
  /// this set compiles to an empty string.
  valid: HashSet<Arc<str>>,
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
  /// Lifted-capture records for audio closure entries, keyed by the entry
  /// closure's (unique) original function name — see [`LiftedCaptures`].
  /// Carried through registry-rebuilding passes like
  /// `window_info_bindings`.
  pub lifted_audio_captures: HashMap<Arc<str>, Arc<LiftedCaptures>>,
  /// As `lifted_audio_captures`, for dispatched GPU closure entries.
  pub lifted_gpu_captures: HashMap<Arc<str>, Arc<LiftedCaptures>>,
  /// Globals whose runtime-sized arrays live in globals of their own (see
  /// [`SplitGlobal`]), by name. Storage-ref inlining redirects accesses
  /// through the split paths of a substituted global.
  pub split_globals: HashMap<Arc<str>, SplitGlobal>,
  /// Overload groups created by module resolution (see
  /// [`crate::compiler::modules`]): each group's name and the registry
  /// buckets its candidates come from. References to a group are rewritten
  /// to the chosen member by `resolve_overload_groups`, right after
  /// inference.
  pub overload_groups: HashMap<Arc<str>, Vec<Arc<str>>>,
  /// What the main file's names refer to, for editor tooling.
  pub main_file_index: MainFileIndex,
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
      lifted_audio_captures: self.lifted_audio_captures.clone(),
      lifted_gpu_captures: self.lifted_gpu_captures.clone(),
      split_globals: self.split_globals.clone(),
      overload_groups: self.overload_groups.clone(),
      main_file_index: self.main_file_index.clone(),
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
      lifted_audio_captures: HashMap::new(),
      lifted_gpu_captures: HashMap::new(),
      split_globals: HashMap::new(),
      overload_groups: HashMap::new(),
      main_file_index: MainFileIndex::default(),
    }
  }
  pub fn add_top_level_var(&mut self, var: TopLevelVar) {
    self.names.write().unwrap().track_user_name(&var.name);
    self.top_level_vars.push(var);
  }
  /// Registers `signature`, returning the registered Arc: the existing one
  /// when an equal signature is already registered under the same name, so
  /// callers can point references at the registry's copy.
  pub fn add_abstract_function(
    &mut self,
    signature: Arc<RwLock<AbstractFunctionSignature>>,
  ) -> Arc<RwLock<AbstractFunctionSignature>> {
    let name = Arc::clone(&signature.read().unwrap().name);
    {
      let mut names = self.names.write().unwrap();
      names.track_user_name(&name);
      if let FunctionImplementationKind::Composite(f) =
        &signature.read().unwrap().implementation
      {
        let f = f.read().unwrap();
        for (arg_name, _) in f.arg_names.iter() {
          names.track_user_name(&arg_name);
        }
        f.expression
          .walk(&mut |exp| {
            if let ExpKind::Name(name) = &exp.kind {
              names.track_user_name(&name);
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
    let bucket = self.abstract_functions.entry(name).or_default();
    if let Some(existing) = bucket
      .iter()
      .find(|existing| *existing.read().unwrap() == *signature.read().unwrap())
    {
      return existing.clone();
    }
    bucket.push(signature.clone());
    signature
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
            specialized_from: None,
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
                specialized_from: None,
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
  pub fn with_enums(self, enums: Vec<AbstractEnum>) -> Self {
    enums.into_iter().fold(self, |ctx, e| ctx.with_enum(e))
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
  /// Registers the monomorphized instance of every generic struct and enum
  /// `t` mentions. Types still mentioning a generic (a signature not yet
  /// specialized) have no instances.
  fn add_type_instances(&mut self, t: &Type) {
    let mut skolems = vec![];
    t.track_skolem_names(&mut skolems);
    if !skolems.is_empty() {
      return;
    }
    let known = |info: &ExpTypeInfo| {
      info.with_dereferenced(|typestate| match typestate {
        TypeState::Known(t) => Some(t.clone()),
        _ => None,
      })
    };
    match t {
      Type::Struct(s) => {
        let field_types: Vec<Type> = s
          .fields
          .iter()
          .filter_map(|f| known(&f.field_type))
          .collect();
        for field_type in &field_types {
          self.add_type_instances(field_type);
        }
        // Builtin generic structs (vectors, matrices, atomics, textures)
        // are target-native types, never emitted definitions.
        let original = s.abstract_ancestor.original_ancestor();
        if field_types.len() == s.fields.len()
          && original.source_trace.primary_position.is_some()
          && let Some(instance) = original.generate_monomorphized(field_types)
        {
          self.add_monomorphized_struct(instance);
        }
      }
      Type::Enum(e) => {
        for variant in &e.variants {
          if let Some(inner_type) = known(&variant.inner_type) {
            self.add_type_instances(&inner_type);
          }
        }
        if let Some(instance) = e
          .abstract_ancestor
          .original_ancestor()
          .generate_monomorphized(e.clone())
        {
          self.add_monomorphized_enum(instance);
        }
      }
      Type::Array(_, inner) => {
        if let Some(inner_type) = known(inner) {
          self.add_type_instances(&inner_type);
        }
      }
      _ => {}
    }
  }
  /// The signatures `fn_name` names: its registry bucket, or every
  /// member's of an overload group.
  fn named_signatures(
    &self,
    fn_name: &Arc<str>,
  ) -> Option<Vec<Arc<RwLock<AbstractFunctionSignature>>>> {
    if let Some(members) = self.overload_groups.get(fn_name) {
      Some(
        members
          .iter()
          .flat_map(|member| {
            self.abstract_functions.get(member).into_iter().flatten()
          })
          .cloned()
          .collect(),
      )
    } else {
      self.abstract_functions.get(fn_name).cloned()
    }
  }
  /// A function type for one reference to `signature`, whose ancestor is a
  /// fresh copy of it.
  fn concrete_signature(
    &self,
    signature: &Arc<RwLock<AbstractFunctionSignature>>,
    source_trace: &SourceTrace,
  ) -> CompileResult<Type> {
    Ok(Type::Function(Box::new(
      AbstractFunctionSignature::concretize(
        Arc::new(RwLock::new(signature.read().unwrap().clone())),
        &self.typedefs,
        source_trace,
      )?,
    )))
  }
  pub fn concrete_signatures(
    &mut self,
    fn_name: &Arc<str>,
    source_trace: SourceTrace,
  ) -> CompileResult<Option<Vec<Type>>> {
    let Some(signatures) = self.named_signatures(fn_name) else {
      return Ok(None);
    };
    signatures
      .iter()
      .map(|signature| self.concrete_signature(signature, &source_trace))
      .collect::<CompileResult<Vec<_>>>()
      .map(Some)
  }
  /// The candidates `constrain_fn_by_argument_types` keeps out of
  /// `concrete_signatures(fn_name)` for arguments of `arg_types`, plus some
  /// it rejects, built without concretizing the overloads
  /// `AbstractFunctionSignature::may_accept_arguments` rules out. `None`
  /// unless `fn_name` has several overloads, some are ruled out, and some
  /// candidate is kept (an incompatibility error lists every overload);
  /// the caller then gives the name all of them as usual.
  pub fn concrete_signatures_accepting(
    &self,
    fn_name: &Arc<str>,
    arg_types: &[&mut TypeState],
    source_trace: &SourceTrace,
  ) -> Option<Vec<Type>> {
    let signatures = self.named_signatures(fn_name)?;
    if signatures.len() < 2 {
      return None;
    }
    let plausible: Vec<_> = signatures
      .iter()
      .filter(|signature| {
        signature.read().unwrap().may_accept_arguments(arg_types)
      })
      .collect();
    if plausible.len() == signatures.len() {
      return None;
    }
    let candidates = plausible
      .into_iter()
      .map(|signature| self.concrete_signature(signature, source_trace))
      .collect::<CompileResult<Vec<_>>>()
      .ok()?;
    candidates
      .iter()
      .any(|candidate| {
        matches!(candidate, Type::Function(signature)
          if signature.are_args_compatible(arg_types))
      })
      .then_some(candidates)
  }
  /// Whether `name` names functions: a registry bucket or an overload
  /// group.
  pub fn names_functions(&self, name: &str) -> bool {
    self.abstract_functions.contains_key(name)
      || self.overload_groups.contains_key(name)
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
    for (document, _, _) in documents.sources.iter() {
      for tree in document.syntax_trees.iter() {
        names.track_all_ast_names(tree);
      }
    }
    let expanded_documents: Vec<Vec<EaslTree>> = documents
      .sources
      .iter()
      .map(|(document, _, _)| {
        document
          .syntax_trees
          .iter()
          .map(|tree| {
            macroexpand(tree.clone(), &macros, &mut names, &mut errors)
          })
          .collect()
      })
      .collect();
    let builtins = Program::default();
    let builtin_types = PRIMITIVE_TYPE_NAMES
      .iter()
      .map(|name| Arc::from(*name))
      .chain(builtins.typedefs.structs.iter().map(|s| s.name.0.clone()))
      .chain(builtins.typedefs.enums.iter().map(|e| e.name.0.clone()))
      .chain(
        builtins
          .typedefs
          .type_aliases
          .iter()
          .map(|(alias, _)| alias.clone()),
      )
      .collect();
    let resolved = resolve_modules(
      documents,
      expanded_documents,
      builtins.abstract_functions.into_keys().collect(),
      builtin_types,
      builtins
        .typedefs
        .enums
        .iter()
        .flat_map(|e| e.variants.iter().map(|v| v.name.clone()))
        .filter(|name| name.contains('/'))
        .collect(),
      &mut errors,
    );
    for tree in resolved.trees.iter() {
      names.track_all_ast_names(tree);
    }
    let trees = resolved.trees;

    let mut non_typedef_trees = vec![];
    let mut untyped_types = vec![];

    for tree in trees.into_iter() {
      use crate::parse::Encloser::*;
      use fsexp::syntax::EncloserOrOperator::*;
      let tree_position = tree.position().clone();
      let (tree_body, annotation) = extract_annotation(tree, &mut errors);
      let EaslTree::Inner((position, Encloser(Parens)), children) = &tree_body
      else {
        errors.log(CompileError::new(
          UnrecognizedTopLevelForm(tree_body),
          tree_position.into(),
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
    program.overload_groups = resolved.overload_groups;
    program.main_file_index = resolved.main_file_index;
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
      Err(cycles) => {
        for cycle in cycles {
          let trace_of = |name: &Arc<str>| {
            untyped_types
              .iter()
              .find(|t| t.name() == name)
              .unwrap()
              .source_trace()
              .clone()
          };
          let source_trace = cycle[1..cycle.len() - 1]
            .iter()
            .fold(trace_of(&cycle[0]), |trace, name| {
              trace.insert_as_secondary(trace_of(name))
            });
          errors.log(CompileError::new(
            TypeDependencyCycle(
              cycle.into_iter().map(|name| name.to_string()).collect(),
            ),
            source_trace,
          ));
        }
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
            "var" | "def" | "override" => {
              if let Some(var) = TopLevelVar::from_ast(
                first_child.as_str(),
                &parens_source_trace,
                children_iter,
                &program,
                annotation,
                &mut errors,
              ) {
                program.add_top_level_var(var);
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
  fn find_untyped(&mut self) -> Vec<CompileError> {
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
            .then(|| {
              CompileError::new(CouldntInferTypes, v.source_trace.clone())
            })
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
  pub fn fully_infer_types(&mut self, errors: &mut ErrorLog) {
    loop {
      let did_type_states_change = self.propagate_types(errors);
      if !did_type_states_change {
        let untyped = self.find_untyped();
        // After a type error, an overload left unresolved is likely just a
        // consequence of it, so it isn't called ambiguous.
        let inference_failed = !errors.is_empty();
        errors.log_all(untyped.into_iter().map(|mut error| {
          if inference_failed
            && matches!(error.kind, CompileErrorKind::AmbiguousOverload(_))
          {
            error.kind = CouldntInferTypes;
          }
          error
        }));
        return;
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
          .normalize_pseudoapplication_data_accesses();
      }
    }
    for var in self.top_level_vars.iter_mut() {
      if let Some(value) = &mut var.value {
        value.normalize_pseudoapplication_data_accesses();
      }
    }
  }
  /// Rejects applications passing overlapping lvalues to two reference
  /// parameters when either is mutable (`AliasedRefArgs`) — easl's
  /// analogue of Rust's exclusive `&mut`. Because references are
  /// second-class (never stored, returned, or rebound; `let` copies),
  /// two references can only ever alias by being formed at the same
  /// application from the same root variable, so this purely local
  /// syntactic check is a *complete* alias analysis. Overlap is judged
  /// conservatively per accessor path: paths conflict unless they
  /// diverge at a compile-time-distinguishable step (different struct
  /// fields, different literal indices) — any runtime index is treated
  /// as potentially equal, so `(f (xs i) (xs j))` is rejected while
  /// `(f (xs 0u) (xs 1u))` and `(f p.left p.right)` are allowed. Runs
  /// after `normalize_pseudoapplication_data_accesses` (so indexing is
  /// uniformly `Access`) and before deshadowing (args of one application
  /// share a scope, so equal names mean equal bindings). This is also
  /// what makes the runtimes' differing call conventions unobservable:
  /// the tree-walker and the VM's per-usage ref bindings only disagree
  /// on aliased shapes (and the fixed-array-element write-back temps
  /// were memory-unsafe under them).
  pub fn validate_ref_arg_aliasing(&self, errors: &mut ErrorLog) {
    for abstract_f in self.abstract_functions_iter() {
      let abstract_f = abstract_f.read().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &abstract_f.implementation
      {
        implementation
          .read()
          .unwrap()
          .expression
          .walk(&mut |exp| {
            if let ExpKind::Application(callee, args) = &exp.kind
              && let TypeState::Known(Type::Function(signature)) =
                &callee.data.kind
            {
              let mut ref_lvalues: Vec<(
                Ownership,
                Arc<str>,
                Vec<RefArgPathStep>,
              )> = vec![];
              for ((var, _), arg) in signature.args.iter().zip(args.iter()) {
                if var.var_type.ownership == Ownership::Owned {
                  continue;
                }
                if let Some((root, path)) = ref_arg_lvalue_path(arg) {
                  ref_lvalues.push((var.var_type.ownership, root, path));
                }
              }
              for i in 0..ref_lvalues.len() {
                for j in (i + 1)..ref_lvalues.len() {
                  let (ownership_a, root_a, path_a) = &ref_lvalues[i];
                  let (ownership_b, root_b, path_b) = &ref_lvalues[j];
                  if root_a == root_b
                    && (*ownership_a == Ownership::MutableReference
                      || *ownership_b == Ownership::MutableReference)
                    && !ref_paths_provably_disjoint(path_a, path_b)
                  {
                    errors.log(CompileError::new(
                      CompileErrorKind::AliasedRefArgs(root_a.to_string()),
                      exp.source_trace.clone(),
                    ));
                  }
                }
              }
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
  }
  /// Enforces the two closure-capture rules for reference arguments.
  /// (1) A closure may never capture a `MutableReference` param
  /// (`ClosureCapturesMutableRefArg`): captures are by-value snapshots,
  /// so mutations through the capture would land in the closure's scope
  /// copy and silently never reach the caller — the one thing
  /// `@var @ref` exists to promise. (2) A closure capturing a bare
  /// `Reference` param may be used freely within the call (captures are
  /// owned shares, so local helper closures are sound and well-defined)
  /// but may not *escape* it (`EscapingClosureCapturesRefArg`) — via the
  /// return value, a `return` statement, or a host sink
  /// (`start-audio`/`spawn-window`) — because the reference does not
  /// outlive the call: the escaped closure would hold a copy, making the
  /// `@ref` annotation a no-op the user should remove. Escape tracking
  /// is a purely local taint walk: a ref-capturing closure can only leave
  /// through this function's return positions, sink calls, and stores into
  /// globals (closure arguments are compile-time inlined, and one held in a
  /// local array / struct / enum taints that local); taint propagates
  /// through let bindings, assignments (including into elements and
  /// fields), and calls whose result type can hold a function value. Runs
  /// after deshadowing, so any name inside a lambda that matches a param
  /// name *is* that param.
  pub fn validate_ref_captures(&self, errors: &mut ErrorLog) {
    for abstract_f in self.abstract_functions_iter() {
      let abstract_f = abstract_f.read().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &abstract_f.implementation
      {
        let implementation = implementation.read().unwrap();
        let ExpKind::Function(arg_names, body) =
          &implementation.expression.kind
        else {
          continue;
        };
        let TypeState::Known(Type::Function(signature)) =
          &implementation.expression.data.kind
        else {
          continue;
        };
        let ref_params: HashMap<Arc<str>, Ownership> = arg_names
          .iter()
          .zip(signature.args.iter())
          .filter_map(|((name, _), (var, _))| {
            (var.var_type.ownership != Ownership::Owned)
              .then(|| (name.clone(), var.var_type.ownership))
          })
          .collect();
        if ref_params.is_empty() {
          continue;
        }
        let mut analyzer = RefCaptureAnalyzer {
          ref_params: &ref_params,
          tainted: HashMap::new(),
          errors,
        };
        if let Some(param) = analyzer.taint_of(body) {
          let tail = tail_value_expression(body);
          errors.log(CompileError::new(
            EscapingClosureCapturesRefArg(param.to_string()),
            tail.source_trace.clone(),
          ));
        }
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
    // Generic instances that are only ever declared, never constructed or
    // matched (a var holding an `(Option (Step f32))` that CPU code merely
    // prints), still need their definitions for the declarations naming
    // them.
    for v in self.top_level_vars.iter() {
      monomorphized_ctx.add_type_instances(&v.var_type);
    }
    let signature_types: Vec<Type> = monomorphized_ctx
      .abstract_functions_iter()
      .filter(|f| f.read().unwrap().generic_args.is_empty())
      .flat_map(|f| {
        let f = f.read().unwrap();
        f.arg_types
          .iter()
          .map(|(t, _)| t.clone())
          .chain(std::iter::once(f.return_type.clone()))
          .filter_map(|t| match t {
            AbstractType::Type(t) => Some(t),
            _ => None,
          })
          .collect::<Vec<_>>()
      })
      .collect();
    for t in signature_types {
      monomorphized_ctx.add_type_instances(&t);
    }
    // And those only a local binding's type names (a
    // `[4: (Option f32)]` local filled by `zeroed-array`).
    let mut local_types: Vec<Type> = vec![];
    for f in monomorphized_ctx.abstract_functions_iter() {
      let f = f.read().unwrap();
      let FunctionImplementationKind::Composite(implementation) =
        &f.implementation
      else {
        continue;
      };
      if !f.generic_args.is_empty() {
        continue;
      }
      implementation
        .read()
        .unwrap()
        .expression
        .walk(&mut |exp| {
          if let ExpKind::Let(bindings, _) = &exp.kind {
            for (_, _, _, value) in bindings {
              if let Some(t) = value.data.kind.try_unwrap_known()
                && matches!(
                  t,
                  Type::Struct(_) | Type::Enum(_) | Type::Array(_, _)
                )
                && !local_types.contains(&t)
              {
                local_types.push(t);
              }
            }
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
    for t in local_types {
      monomorphized_ctx.add_type_instances(&t);
    }
    take(self, |old_ctx| {
      monomorphized_ctx.top_level_vars = old_ctx.top_level_vars;
      monomorphized_ctx.window_info_bindings = old_ctx.window_info_bindings;
      monomorphized_ctx.lifted_audio_captures = old_ctx.lifted_audio_captures;
      monomorphized_ctx.lifted_gpu_captures = old_ctx.lifted_gpu_captures;
      monomorphized_ctx.split_globals = old_ctx.split_globals;
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
            take(&mut **body, |old_body| {
              let data = old_body.data.clone();
              ExpBuilder::at(&old_body.source_trace)
                .with_data(ExpKind::Let(pending, Box::new(old_body)), data)
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
                        // A value read through a reference passed to an
                        // owned parameter is copied, like any value.
                        Ownership::Owned => {}
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
                              && address_space
                                .may_never_be_passed_as_reference()
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
                        Ownership::Pointer(_, _) => {
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
  /// Marks `held` — the signature a host builtin's function argument holds —
  /// as an entry point of kind `entry`, unless it already is one of that
  /// kind (`is_same_kind`); another kind is `wrong_kind`. The mark goes on
  /// the function's implementation, which every copy of its signature
  /// shares, and on every registered signature sharing that implementation.
  /// `held` may itself be the registered signature (function values repoint
  /// references at it), so it's marked directly rather than found by
  /// re-locking registry entries.
  fn mark_implicit_entry_point(
    &self,
    held: &mut AbstractFunctionSignature,
    entry: EntryPoint,
    is_same_kind: impl Fn(EntryPoint) -> bool,
    wrong_kind: impl FnOnce(String) -> CompileErrorKind,
    source: &SourceTrace,
    errors: &mut ErrorLog,
  ) {
    match held.entry_point {
      Some(existing) if !is_same_kind(existing) => {
        errors.log(CompileError::new(
          wrong_kind(existing.name().into()),
          source.clone(),
        ));
        return;
      }
      Some(_) => {}
      None => held.entry_point = Some(entry),
    }
    let FunctionImplementationKind::Composite(implementation) =
      &held.implementation
    else {
      return;
    };
    implementation.write().unwrap().entry_point = held.entry_point;
    for other in self.abstract_functions_iter() {
      if let Ok(mut other) = other.try_write()
        && let FunctionImplementationKind::Composite(other_implementation) =
          &other.implementation
        && Arc::ptr_eq(other_implementation, implementation)
      {
        other.entry_point = held.entry_point;
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
                      self.mark_implicit_entry_point(
                        &mut abstract_compute_fn.write().unwrap(),
                        EntryPoint::Compute([1, 1, 1]),
                        |e| matches!(e, EntryPoint::Compute(_)),
                        WrongEntryPointTypeForDispatchComputeShader,
                        &exp.source_trace,
                        errors,
                      );
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
                      self.mark_implicit_entry_point(
                        &mut abstract_vertex_fn.write().unwrap(),
                        EntryPoint::Vertex,
                        |e| e == EntryPoint::Vertex,
                        WrongEntryPointTypeForDispatchVertexShader,
                        &exp.source_trace,
                        errors,
                      );
                      self.mark_implicit_entry_point(
                        &mut abstract_fragment_fn.write().unwrap(),
                        EntryPoint::Fragment,
                        |e| e == EntryPoint::Fragment,
                        WrongEntryPointTypeForDispatchFragmentShader,
                        &exp.source_trace,
                        errors,
                      );
                      let abstract_fragment_fn =
                        abstract_fragment_fn.read().unwrap();

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
                      frag_implementation.write().unwrap().dispatched = true;
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
                            !in_ty.compatible(out_ty)
                          } else {
                            true
                          }
                        })
                      {
                        errors.log(CompileError::new(
                          IncompatibleRenderEntryPoints(
                            abstract_vertex_fn.read().unwrap().name.to_string(),
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
                      self.mark_implicit_entry_point(
                        &mut abstract_audio_fn.write().unwrap(),
                        EntryPoint::Audio,
                        |e| e == EntryPoint::Audio,
                        WrongEntryPointTypeForStartAudio,
                        &exp.source_trace,
                        errors,
                      );
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
  /// mutable-reference parameter is mutated (closure-reference parameters
  /// are mutable only when written through — see
  /// `refine_closure_reference_mutability`). A captured closure is mutated
  /// by its own body, so that body is checked, and the error names what it
  /// mutates.
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
            if let Some(captured_closure) = self.closure_of_value(arg) {
              self.check_dispatched_closure_scope_mutations(
                captured_closure,
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
  /// The closure a value is, when it is one: typed as the closure's function
  /// type, or as its scope struct (how a closure field or a closure passed
  /// by its scope is typed).
  fn closure_of_value(
    &self,
    exp: &TypedExp,
  ) -> Option<Arc<RwLock<AbstractFunctionSignature>>> {
    match exp.data.kind.try_unwrap_known()? {
      Type::Function(signature) => signature
        .abstract_ancestor
        .filter(|ancestor| ancestor.read().unwrap().captured_scope.is_some()),
      Type::Struct(s) => self
        .abstract_functions_iter()
        .find(|f| {
          f.read()
            .unwrap()
            .captured_scope
            .as_ref()
            .is_some_and(|scope| scope.name.0 == s.name)
        })
        .cloned(),
      _ => None,
    }
  }
  /// Whether a struct field of this type makes the struct's WGSL
  /// declaration impossible: runtime-sized arrays and strings have no
  /// (or no legal) shader representation, and function-typed fields
  /// declare as their representative captured-scope structs, so the
  /// check recurses through those scopes' own fields.
  /// Whether anything in the program references the `MidiNote` struct.
  /// It's registered in every program's typedefs so the type name always
  /// resolves, but only programs that actually use it — through
  /// `down-midi-notes`' implicit var, or by naming the type/constructor
  /// directly — should carry its declaration in emitted output: a plain
  /// shader program must not ship audio-runtime types.
  fn references_midi_note(&self) -> bool {
    fn type_mentions(t: &Type) -> bool {
      match t {
        Type::Struct(s) => {
          &*s.name == "MidiNote"
            || s.fields.iter().any(|f| {
              f.field_type
                .kind
                .try_unwrap_known()
                .map(|t| type_mentions(&t))
                .unwrap_or(false)
            })
        }
        Type::Enum(e) => e.variants.iter().any(|v| {
          v.inner_type
            .kind
            .try_unwrap_known()
            .map(|t| type_mentions(&t))
            .unwrap_or(false)
        }),
        Type::Array(_, inner) => inner
          .kind
          .try_unwrap_known()
          .map(|t| type_mentions(&t))
          .unwrap_or(false),
        Type::Function(signature) => {
          signature.args.iter().any(|(arg, _)| {
            arg
              .var_type
              .kind
              .try_unwrap_known()
              .map(|t| type_mentions(&t))
              .unwrap_or(false)
          }) || signature
            .return_type
            .kind
            .try_unwrap_known()
            .map(|t| type_mentions(&t))
            .unwrap_or(false)
        }
        _ => false,
      }
    }
    fn abstract_type_mentions(t: &AbstractType) -> bool {
      match t {
        AbstractType::Type(t) => type_mentions(t),
        AbstractType::AbstractStruct(s) => {
          &*s.name.0 == "MidiNote"
            || s
              .fields
              .iter()
              .any(|f| abstract_type_mentions(&f.field_type))
        }
        AbstractType::AbstractArray { inner_type, .. } => {
          abstract_type_mentions(inner_type)
        }
        _ => false,
      }
    }
    if self
      .top_level_vars
      .iter()
      .any(|v| type_mentions(&v.var_type))
    {
      return true;
    }
    // User struct/enum declarations embedding MidiNote emit references
    // to it even when otherwise unused.
    if self.typedefs.structs.iter().any(|s| {
      &*s.name.0 != "MidiNote"
        && s
          .fields
          .iter()
          .any(|f| abstract_type_mentions(&f.field_type))
    }) {
      return true;
    }
    let mut found = false;
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
          if let Some(t) = exp.data.kind.try_unwrap_known()
            && type_mentions(&t)
          {
            found = true;
            return Ok::<bool, Never>(false);
          }
          Ok(true)
        })
        .unwrap();
      if found {
        return true;
      }
    }
    false
  }
  pub(crate) fn type_makes_struct_cpu_only(&self, t: &Type) -> bool {
    t.involves_string()
      || t.involves_runtime_sized_array()
      || t.involves_video()
      || self.type_embeds_cpu_only_function(t)
  }
  /// Whether `t` embeds a function type with no emittable WGSL/C
  /// representation: a `(Fn ...)` with no abstract ancestor (an uncalled
  /// higher-order function's function-typed parameter), a scope-less
  /// closure, or a closure whose own captured scope is itself CPU-only. The
  /// check recurses through arrays, structs, and enums, so a scope struct
  /// capturing a `[N: (Fn ...)]` is filtered from emission the same as one
  /// capturing a bare `(Fn ...)`.
  fn type_embeds_cpu_only_function(&self, t: &Type) -> bool {
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
      Type::Array(_, inner) => {
        self.type_embeds_cpu_only_function(&inner.kind.unwrap_known())
      }
      Type::Struct(s) => s.fields.iter().any(|f| {
        self.type_embeds_cpu_only_function(&f.field_type.unwrap_known())
      }),
      Type::Enum(e) => e.variants.iter().any(|v| {
        self.type_embeds_cpu_only_function(&v.inner_type.unwrap_known())
      }),
      _ => false,
    }
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
      AUDIO, COMPUTE, CONTEXT_COUNT, CONTEXT_ENTRIES, CONTEXT_NAMES, CPU,
      FRAGMENT, bit,
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
                && &**applied_name == "workgroup-size"
                && args.is_empty()
              {
                violations.push((
                  exp.source_trace.clone(),
                  bit(COMPUTE),
                  std::array::from_fn(|_| Some(WorkgroupSizeOutsideCompute)),
                ));
              }
              if let ExpKind::Name(applied_name) = &applied_f.kind
                && matches!(&**applied_name, "audio-time" | "audio-input")
              {
                violations.push((
                  exp.source_trace.clone(),
                  bit(AUDIO),
                  std::array::from_fn(|_| {
                    Some(AudioInfoOutsideAudio(applied_name.to_string()))
                  }),
                ));
              }
              // Storage/uniform-space lvalues passed as references: a
              // ref to a whole global or a static field path into one is
              // legal in every context (the reference monomorphizer
              // inlines a direct usage of that place in place of the
              // parameter, so no pointer ever exists), and so is a
              // read-only element ref (a runtime index), which passes the
              // element's value. A mutable element ref becomes a real
              // pointer parameter, which base WGSL only permits in the
              // function/private address spaces — so those call sites
              // are restricted to cpu/audio contexts (the VM binds
              // regions/slots directly; the tree-walker copies + writes
              // back). (Workgroup/handle refs are rejected everywhere, in
              // the ownership walk — they have no CPU storage at all.)
              if let TypeState::Known(Type::Function(signature)) =
                &applied_f.data.kind
                && let Some(ancestor) = &signature.abstract_ancestor
                && matches!(
                  ancestor.read().unwrap().implementation,
                  FunctionImplementationKind::Composite(_)
                )
              {
                let ancestor = ancestor.read().unwrap();
                for (i, (_, ownership)) in ancestor.arg_types.iter().enumerate()
                {
                  if *ownership == Ownership::MutableReference
                    && let Some(arg) = args.get(i)
                    && arg.static_field_path().is_none()
                    && let Some(root) = arg.name_or_inner_accessed_name()
                    && let Some(TopLevelVar {
                      kind: TopLevelVariableKind::Var { address_space, .. },
                      ..
                    }) = self.top_level_vars.iter().find(|v| v.name == *root)
                    && matches!(
                      address_space,
                      VariableAddressSpace::Uniform
                        | VariableAddressSpace::StorageRead
                        | VariableAddressSpace::StorageReadWrite
                    )
                  {
                    violations.push((
                      arg.source_trace.clone(),
                      bit(CPU) | bit(AUDIO),
                      std::array::from_fn(|_| {
                        Some(PassedReferenceFromInvalidAddressSpace(
                          *address_space,
                        ))
                      }),
                    ));
                  }
                }
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
                      // validation, and `(audio-time period)` by the
                      // audio-info rule; the generic complaint would be
                      // redundant noise there
                      let suppress_gpu = (runtime_sized_error_present
                        && matches!(
                          &**fn_name,
                          "into-dynamic-array"
                            | "zeroed-array"
                            | "push"
                            | "insert"
                            | "remove"
                            | "concat"
                            | "reverse"
                        ))
                        || &**fn_name == "audio-time";
                      // The dynamic-array constructors are VM-native, so
                      // audio code may call them (allocating on the audio
                      // hot path is the user's tradeoff to make); the
                      // rest of the CPU-exclusive family has no
                      // audio-side implementation.
                      let allowed =
                        if EffectType::cpu_exclusive_function_allowed_in_audio(
                          fn_name,
                        ) {
                          bit(CPU) | bit(AUDIO)
                        } else {
                          bit(CPU)
                        };
                      violations.push((
                        exp.source_trace.clone(),
                        allowed,
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
  /// Rewrites every audio-info and MIDI-info call into a read of a
  /// fixed-name implicit `@local` var, created on first use:
  /// `sample-rate`/`audio-time`/`midi-aftertouch`/`midi-pitch-bend`
  /// become bare reads of f32 vars, `midi-cc` an array lookup into a
  /// `[128: f32]` var, and `down-midi-notes` a bare read of the
  /// runtime-sized `easl_midi_down_notes`. Each
  /// thread refreshes its own copies — the audio driver writes the rate
  /// once per batch and the time before every sample
  /// (`VmAudioDriver::run_batch`; the C backend's generated entry
  /// wrapper stores both) and the MIDI vars once per batch from the live
  /// listener, while main refreshes its MIDI copies once per frame
  /// through `IOManager::midi_state` — so no backend ever compiles the
  /// calls themselves. The names are deliberately fixed rather than
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
          if args.is_empty() {
            // Audio-info vars are `@local` (per-execution-context, driver-
            // authoritative); MIDI vars are storage-read bindings with
            // elided numbers, so shader code can read them too — the
            // main thread's per-frame refresh marks them CPU-written and
            // the ordinary dirty-upload machinery ships them to the GPU
            // (they only become runtime bindings when a shader actually
            // touches them, like any GPU-space var).
            let (var_name, address_space, group_and_binding) =
              match &**applied_name {
                "sample-rate" => {
                  ("easl_sample_rate", VariableAddressSpace::Local, None)
                }
                "audio-time" => {
                  ("easl_audio_time", VariableAddressSpace::Local, None)
                }
                "audio-input" => {
                  ("easl_audio_input", VariableAddressSpace::Local, None)
                }
                "midi-aftertouch" => (
                  "easl_midi_aftertouch",
                  VariableAddressSpace::StorageRead,
                  Some(BindingSpec::Elided),
                ),
                "midi-pitch-bend" => (
                  "easl_midi_pitch_bend",
                  VariableAddressSpace::StorageRead,
                  Some(BindingSpec::Elided),
                ),
                "down-midi-notes" => (
                  "easl_midi_down_notes",
                  VariableAddressSpace::StorageRead,
                  Some(BindingSpec::Elided),
                ),
                _ => return Ok(true),
              };
            if created.insert(var_name) {
              new_vars.push(TopLevelVar {
                name: var_name.into(),
                kind: TopLevelVariableKind::Var {
                  address_space,
                  group_and_binding,
                },
                // The call's inferred return type is exactly the var's
                // type (f32, or `[MidiNote]` for `down-midi-notes`).
                var_type: exp.data.unwrap_known(),
                value: None,
                source_trace: exp.source_trace.clone(),
                external: false,
                directly_user_written: false,
              });
            }
            exp.kind = ExpKind::Name(var_name.into());
            exp.data.is_globally_bound = true;
            return Ok(true);
          }
          // `get-midi-note` and `midi-cc` both index a fixed 128-entry
          // storage table by note/controller number: rewrite to an array
          // lookup into the implicit global. `midi-cc`'s element is f32;
          // `get-midi-note`'s is the call's inferred `(Option MidiNote)`
          // (concrete here — this pass runs after monomorphization).
          if args.len() == 1
            && matches!(&**applied_name, "midi-cc" | "get-midi-note")
          {
            let (var_name, element_type) = if &**applied_name == "midi-cc" {
              ("easl_midi_cc", Type::F32)
            } else {
              ("easl_midi_notes", exp.data.unwrap_known())
            };
            let array_type = Type::Array(
              Some(ConcreteArraySize::Literal(128)),
              Box::new(element_type.known().into()),
            );
            if created.insert(var_name) {
              new_vars.push(TopLevelVar {
                name: var_name.into(),
                kind: TopLevelVariableKind::Var {
                  address_space: VariableAddressSpace::StorageRead,
                  group_and_binding: Some(BindingSpec::Elided),
                },
                var_type: array_type.clone(),
                value: None,
                source_trace: exp.source_trace.clone(),
                external: false,
                directly_user_written: false,
              });
            }
            // The call becomes an ordinary array lookup on the global —
            // the `Access` form the backends expect (array-lookup
            // applications are normalized to `Access` before this pass),
            // with both nodes marked globally bound, mirroring what
            // inference produces for `(global i)`. Valid indices are
            // 0-127, MIDI's own 7-bit range — out-of-range indices
            // behave like any other out-of-range array access.
            let old_kind =
              std::mem::replace(&mut exp.kind, ExpKind::Name(var_name.into()));
            let ExpKind::Application(mut accessed, mut args) = old_kind else {
              unreachable!()
            };
            let index = args.remove(0);
            accessed.kind = ExpKind::Name(var_name.into());
            accessed.data.kind = TypeState::Known(array_type);
            accessed.data.is_globally_bound = true;
            exp.kind =
              ExpKind::Access(Accessor::ArrayIndex(Box::new(index)), accessed);
            exp.data.is_globally_bound = true;
          }
          Ok(true)
        })
        .unwrap();
    }
    self.top_level_vars.extend(new_vars);
  }
  /// Rewrites `start-audio` calls whose function is a *scoped closure* so
  /// they can run on the audio thread: the closure gets a scope-less clone
  /// (`{original}_audio`) whose captures live in plain thread-shared
  /// globals (`lift_closure_captures`), and the clone becomes the `@audio`
  /// entry point in place of the original (which the
  /// reference-address-space rebuild would drop from the registry — it has
  /// a non-owned trailing scope param). At runtime, `start-audio` seeds the
  /// lifted globals from the closure value's scope and starts the audio
  /// thread on the clone; the bootstrap force-publish ships the seeds to
  /// the audio replica's first adopt.
  pub fn extract_audio_closure_scopes(&mut self, errors: &mut ErrorLog) {
    let mut lift = CaptureLift::default();
    let mut lifted: HashMap<Arc<str>, Arc<LiftedCaptures>> = HashMap::new();
    let mut new_functions: Vec<Arc<RwLock<AbstractFunctionSignature>>> = vec![];
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
            let original_name = ancestor.read().unwrap().name.clone();
            let captures = match lifted.get(&original_name) {
              Some(captures) => captures.clone(),
              None => {
                let (clone, captures) = self.audio_entry_clone(
                  &ancestor,
                  &scope_struct,
                  &source_trace,
                  &mut lift,
                  errors,
                );
                new_functions.push(clone);
                let captures = Arc::new(captures);
                lifted.insert(original_name.clone(), captures.clone());
                captures
              }
            };
            // Record the seed writes on this call site's callee: at
            // runtime the builtin copies the closure value's captured
            // fields into the lifted globals on every call, and this
            // private ancestor clone makes those writes visible to the
            // sharing-audience analysis as `SeedsGlobalVar` effects — the
            // standard analysis then sees the main thread touching the
            // lifted globals, with no force-membership flag anywhere.
            let mut seeded_names: Vec<Arc<str>> = vec![];
            captures.global_names(&mut seeded_names);
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
    lift.install(self);
    for f in new_functions {
      self.add_abstract_function(f);
    }
    self.lifted_audio_captures.extend(lifted);
  }
  /// The audio-thread version of a `start-audio`d closure: a clone without
  /// the trailing scope parameter, its captures lifted to globals, marked
  /// as the `@audio` entry in place of the original. Its name is
  /// `{original}_audio`, which both runtimes derive at `start-audio` time.
  fn audio_entry_clone(
    &self,
    ancestor: &Arc<RwLock<AbstractFunctionSignature>>,
    scope_struct: &AbstractStruct,
    source_trace: &SourceTrace,
    lift: &mut CaptureLift,
    errors: &mut ErrorLog,
  ) -> (Arc<RwLock<AbstractFunctionSignature>>, LiftedCaptures) {
    let (original_name, original_implementation) = {
      let mut ancestor = ancestor.write().unwrap();
      // The entry-marking pass marked the original closure; the clone is
      // what actually runs on the audio thread.
      ancestor.entry_point = None;
      let FunctionImplementationKind::Composite(implementation) =
        ancestor.implementation.clone()
      else {
        panic!("audio entry closure wasn't composite")
      };
      implementation.write().unwrap().entry_point = None;
      (ancestor.name.clone(), implementation)
    };
    let mut implementation =
      original_implementation.read().unwrap().derived_from();
    let Type::Function(mut signature) =
      implementation.expression.data.unwrap_known()
    else {
      panic!("audio entry closure had a non-function type")
    };
    signature.args.pop();
    implementation.expression.data.kind =
      TypeState::Known(Type::Function(signature));
    let scope_param = implementation.arg_names.pop().unwrap().0;
    implementation.arg_annotations.pop();
    implementation.entry_point = Some(EntryPoint::Audio);
    let ExpKind::Function(fn_arg_names, body) =
      &mut implementation.expression.kind
    else {
      panic!("audio entry closure implementation wasn't a Function")
    };
    fn_arg_names.pop();
    let captures = self.lift_closure_captures(
      scope_struct,
      &scope_param,
      body,
      ClosureLiftTarget::Audio,
      source_trace,
      lift,
      errors,
    );
    let clone_name: Arc<str> = format!("{original_name}_audio").into();
    assert!(
      !self.abstract_functions.contains_key(&clone_name),
      "audio clone name `{clone_name}` collides with an existing function"
    );
    self.names.write().unwrap().track_user_name(&clone_name);
    let clone = {
      let original = ancestor.read().unwrap();
      let mut arg_types = original.arg_types.clone();
      if let Some((AbstractType::AbstractStruct(s), _)) = arg_types.last()
        && s.name.0 == scope_struct.name.0
      {
        arg_types.pop();
      }
      AbstractFunctionSignature {
        name: clone_name,
        arg_types,
        return_type: original.return_type.clone(),
        implementation: FunctionImplementationKind::Composite(Arc::new(
          RwLock::new(implementation),
        )),
        entry_point: Some(EntryPoint::Audio),
        ..Default::default()
      }
    };
    (Arc::new(RwLock::new(clone)), captures)
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
                directly_user_written: false,
              });
              binding_name
            })
            .clone();
          if kind.is_boolean() {
            // Bools aren't host-shareable in WGSL uniforms: the binding is
            // a u32 and the query becomes `(!= binding 0u)`.
            let b = ExpBuilder::at(&exp.source_trace);
            let mut binding_read = b.name(&binding_name, &Type::U32);
            binding_read.data.is_globally_bound = true;
            exp.kind = ExpKind::Application(
              Box::new(b.callee(
                &not_equal_ancestor,
                &[Type::U32, Type::U32],
                &Type::Bool,
              )),
              vec![binding_read, b.u32(0)],
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
    // Only reads function bodies, so one memo serves the whole loop.
    let mut memo = EffectsMemo::default();
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
      let remaining =
        implementation.effects_with(&mut memo).window_info_kinds();
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
    let mut lift = CaptureLift::default();
    let mut lifted_captures: Vec<(Arc<str>, Arc<LiftedCaptures>)> = vec![];
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
              let captures = self.lift_closure_captures(
                &scope_struct,
                &scope_arg_name,
                body,
                ClosureLiftTarget::GpuDispatch,
                &exp.source_trace,
                &mut lift,
                errors,
              );
              lifted_captures.push((entry_name.clone(), Arc::new(captures)));
            }
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
    lift.install(self);
    self.lifted_gpu_captures.extend(lifted_captures);
  }
  pub fn monomorphize_reference_address_spaces(&mut self) {
    // Walking a body points each of its calls to a function with reference
    // parameters at a variant without them, and the pass never changes a
    // walked body or the signatures it calls again, so each body is walked
    // once. The map holds the walked bodies, so none is freed and its
    // address reused while the pass runs.
    let mut walked: HashMap<usize, Arc<RwLock<TopLevelFunction>>> =
      HashMap::new();
    loop {
      let mut monomorphized_ctx = Program::default();
      monomorphized_ctx.names = RwLock::new(self.names.read().unwrap().clone());
      monomorphized_ctx.typedefs = self.typedefs.clone();
      let mut variants = ReferenceVariants::new();
      let mut changed = false;
      for f in self.abstract_functions_iter() {
        let borrowed_f = f.read().unwrap();
        if let FunctionImplementationKind::Composite(implementation) =
          &f.read().unwrap().implementation
        {
          if borrowed_f.reference_arg_positions().is_empty() {
            let key = Arc::as_ptr(implementation) as usize;
            if !walked.contains_key(&key) {
              walked.insert(key, implementation.clone());
              changed |= implementation
                .write()
                .unwrap()
                .expression
                .monomorphize_reference_address_spaces(
                  &self,
                  &mut monomorphized_ctx,
                  &mut variants,
                  &mut walked,
                );
            }
            let mut new_f = (**f).read().unwrap().clone();
            new_f.implementation =
              FunctionImplementationKind::Composite(implementation.clone());
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
        monomorphized_ctx.lifted_audio_captures = old_ctx.lifted_audio_captures;
        monomorphized_ctx.lifted_gpu_captures = old_ctx.lifted_gpu_captures;
        monomorphized_ctx.split_globals = old_ctx.split_globals;
        monomorphized_ctx
      });
      if !changed {
        break;
      }
    }
  }
  pub fn extract_builtin_attribute_lookup_functions(&mut self) {
    let mut used_attributes: HashSet<BuiltinIOAttribute> = HashSet::new();
    // Each body's lookups, gathered before any body is rewritten so one memo
    // serves every body. (The rewrite below adds only an assignment, which
    // looks nothing up.)
    let mut memo = EffectsMemo::default();
    let looked_up_attributes: HashMap<usize, Vec<BuiltinIOAttribute>> = self
      .abstract_functions_iter()
      .filter_map(|f| match &f.read().unwrap().implementation {
        FunctionImplementationKind::Composite(implementation) => Some((
          Arc::as_ptr(implementation) as usize,
          implementation
            .read()
            .unwrap()
            .effects_with(&mut memo)
            .looked_up_builtin_attributes(),
        )),
        _ => None,
      })
      .collect();
    drop(memo);
    for f in self.abstract_functions_iter() {
      let borrowed_f = f.read().unwrap();
      if let FunctionImplementationKind::Composite(implementation) =
        &f.read().unwrap().implementation
      {
        let attributes =
          looked_up_attributes[&(Arc::as_ptr(implementation) as usize)].clone();
        let mut implementation = implementation.write().unwrap();
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
            let value_type = attribute.value_type();
            let b = ExpBuilder::at(&SourceTrace::empty());
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
              b.name(&arg_name, &value_type)
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
              b.field(
                b.name(&arg_name, &Type::Struct(struct_type)),
                &field_name,
                value_type.clone(),
              )
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
              b.name(&arg_name, &value_type)
            };
            let ExpKind::Function(_, body) =
              &mut implementation.expression.kind
            else {
              panic!()
            };
            let assignment = b.apply(
              TypedExp::assignment_function(value_type.clone().known().into()),
              vec![
                b.name(&global_var_name.into(), &value_type),
                assignment_value,
              ],
              &Type::Unit,
            );
            *body = ExpBuilder::at(&body.source_trace)
              .with_data(
                ExpKind::Block(vec![assignment, *body.clone()]),
                body.data.clone(),
              )
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
        directly_user_written: false,
      })
    }
  }
  /// For `ValidationOptions::window_regions`: keeps the `position` builtin
  /// of every fragment entry the program draws with
  /// (`TopLevelFunction::dispatched`) in window coordinates when the
  /// runtime draws only a region of the window into a smaller target (`GpuCore::execute_render_batch_to_window_region`), where the
  /// hardware reports positions relative to the target: each entry adds the
  /// implicit `easl_fragment_position_offset` uniform (the region's origin
  /// in window pixels during such a pass, zero otherwise) to its position's
  /// x and y before running. Must run after
  /// `extract_builtin_attribute_lookup_functions`, which turns position
  /// lookups into arguments.
  pub fn offset_fragment_positions(&mut self) {
    let position_type = BuiltinIOAttribute::Position.value_type();
    let scalar_plus = self
      .abstract_functions
      .get("+")
      .and_then(|signatures| {
        signatures
          .iter()
          .find(|signature| {
            matches!(
              signature.read().unwrap().arg_types.first(),
              Some((AbstractType::Generic(_), _))
            )
          })
          .cloned()
      })
      .expect("builtin scalar + missing from registry");
    let offset_name: Arc<str> = FRAGMENT_POSITION_OFFSET_VAR.into();
    let mut offset_used = false;
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      if f.entry_point != Some(EntryPoint::Fragment) {
        continue;
      }
      let FunctionImplementationKind::Composite(implementation) =
        &f.implementation
      else {
        continue;
      };
      let mut implementation = implementation.write().unwrap();
      if !implementation.dispatched {
        continue;
      }
      let Type::Function(signature) =
        implementation.expression.data.unwrap_known()
      else {
        panic!()
      };
      // the argument holding the position, and its field holding it when
      // the argument is a struct
      let Some((index, field)) = implementation
        .arg_annotations
        .iter()
        .zip(signature.args.iter())
        .enumerate()
        .find_map(|(index, (annotation, (arg, _)))| {
          if annotation
            .attributes
            .has_builtin_io_attribute(BuiltinIOAttribute::Position)
          {
            return Some((index, None));
          }
          let Type::Struct(s) = arg.var_type.unwrap_known() else {
            return None;
          };
          s.fields
            .iter()
            .find(|field| {
              field
                .attributes
                .has_builtin_io_attribute(BuiltinIOAttribute::Position)
            })
            .map(|field| (index, Some(field.name.clone())))
        })
      else {
        continue;
      };
      offset_used = true;
      let arg_type = signature.args[index].0.var_type.unwrap_known();
      let implementation = &mut *implementation;
      let ExpKind::Function(args, body) = &mut implementation.expression.kind
      else {
        panic!()
      };
      let name = args[index].0.clone();
      let raw_name = self.names.write().unwrap().gensym(&format!("{name}_raw"));
      args[index].0 = raw_name.clone();
      // (arguments added by the lookup extraction aren't in `arg_names`)
      if let Some(arg_name) = implementation.arg_names.get_mut(index) {
        arg_name.0 = raw_name.clone();
      }
      let b = ExpBuilder::at(&body.source_trace);
      let position = || {
        let arg = b.name(&name, &arg_type);
        match &field {
          Some(field) => b.field(arg, field, position_type.clone()),
          None => arg,
        }
      };
      let mut offset = b.name(&offset_name, &position_type);
      offset.data.is_globally_bound = true;
      let mut exps: Vec<TypedExp> = ["x", "y"]
        .into_iter()
        .map(|component| {
          b.apply(
            TypedExp::assignment_function(Type::F32.known().into()),
            vec![
              b.field(position(), component, Type::F32),
              b.call(
                &scalar_plus,
                vec![
                  b.field(position(), component, Type::F32),
                  b.field(offset.clone(), component, Type::F32),
                ],
                &Type::F32,
              ),
            ],
            &Type::Unit,
          )
        })
        .collect();
      exps.push(std::mem::replace(&mut **body, b.unit()));
      **body = b.let_in(
        vec![(name, VariableKind::Var, b.name(&raw_name, &arg_type))],
        b.block(exps),
      );
    }
    if offset_used {
      self.top_level_vars.push(TopLevelVar {
        name: offset_name,
        kind: TopLevelVariableKind::Var {
          address_space: VariableAddressSpace::Uniform,
          group_and_binding: Some(BindingSpec::Elided),
        },
        var_type: position_type,
        value: None,
        source_trace: SourceTrace::empty(),
        external: false,
        directly_user_written: false,
      });
    }
  }
  /// Set each closure-returning function's declared return type from the
  /// scope struct its body's tail actually constructs. The scope-struct
  /// constructor is the one ancestor no later pass rewrites, so it is the
  /// unambiguous identity of the returned closure — unlike a construction
  /// node's own function-type ancestor, which the signature-propagation
  /// fixpoint can collapse between two structurally-identical sibling closures
  /// (e.g. a higher-order function returning a closure that captures its
  /// function argument, instantiated at two different arguments). Run right
  /// after extraction so the correct `Some` return type is in place before
  /// propagation's `is_none` fills — which would otherwise lock a
  /// specialization's return type onto its sibling's closure — get a chance to
  /// guess wrong. Deterministic and single-pass: it reads the construction's
  /// scope struct, never a churning inferred ancestor.
  pub fn stamp_returned_closure_return_types(&mut self) {
    let mut scope_to_dispatch: HashMap<
      Arc<str>,
      Arc<RwLock<AbstractFunctionSignature>>,
    > = HashMap::new();
    let mut fn_by_name: HashMap<
      Arc<str>,
      Arc<RwLock<AbstractFunctionSignature>>,
    > = HashMap::new();
    for f in self.abstract_functions_iter() {
      let bf = f.read().unwrap();
      if let Some(scope) = &bf.captured_scope {
        scope_to_dispatch.insert(scope.name.0.clone(), Arc::clone(f));
      }
      fn_by_name.insert(bf.name.clone(), Arc::clone(f));
    }
    // The dispatch function a body's tail returns, drilling through
    // `let`/block wrappers to the tail. Two shapes: a scope-struct
    // construction (a closure WITH captured data) — keyed off the constructed
    // scope struct; or a bare name (a SCOPE-LESS closure, e.g. one capturing
    // only other scope-less closures) — keyed off the name itself. In both
    // cases the anchor (the scope struct name / the callee name) is what
    // extraction stamped and no later pass rewrites, unlike the node's own
    // function-type ancestor which propagation can collapse between two
    // structurally-identical sibling closures.
    let tail_dispatch = |body: &TypedExp| {
      let mut cur = body;
      loop {
        match &cur.kind {
          ExpKind::Block(exps) => match exps.last() {
            Some(l) => cur = l,
            None => return None,
          },
          ExpKind::Let(_, b) => cur = b,
          _ => break,
        }
      }
      match &cur.kind {
        ExpKind::Application(callee, _)
          if let Some(callee_ancestor) =
            callee.data.with_known(|t| match t {
              Type::Function(callee_sig) => {
                callee_sig.abstract_ancestor.clone()
              }
              _ => None,
            }) =>
        {
          let scope_name = callee_ancestor.read().unwrap().name.clone();
          scope_to_dispatch.get(&scope_name).cloned()
        }
        ExpKind::Name(n)
          if cur.data.with_known(|t| matches!(t, Type::Function(_))) =>
        {
          fn_by_name.get(n).cloned()
        }
        _ => None,
      }
    };
    for f in self.abstract_functions_iter() {
      let implementation = {
        let bf = f.read().unwrap();
        match &bf.implementation {
          FunctionImplementationKind::Composite(impl_arc) => impl_arc.clone(),
          _ => continue,
        }
      };
      let mut impl_w = implementation.write().unwrap();
      let dispatch = if let ExpKind::Function(_, body) = &impl_w.expression.kind
      {
        tail_dispatch(body)
      } else {
        None
      };
      let Some(dispatch) = dispatch else {
        continue;
      };
      // The implementation's own function-type return signature (what the
      // emitter and dispatch-baking read). Replaced rather than written
      // through: a specialization's types can share unification variables
      // with the function it was cloned from, so writing through them would
      // stamp every sibling with whichever closure was stamped last.
      if let TypeState::Known(Type::Function(mut signature)) = impl_w
        .expression
        .data
        .kind
        .with_dereferenced(TypeState::clone)
        && let TypeState::Known(Type::Function(mut return_signature)) =
          signature
            .return_type
            .kind
            .with_dereferenced(TypeState::clone)
      {
        return_signature.abstract_ancestor = Some(dispatch.clone());
        signature.return_type.kind =
          TypeState::Known(Type::Function(return_signature));
        impl_w.expression.data.kind =
          TypeState::Known(Type::Function(signature));
      }
      drop(impl_w);
      if let AbstractType::Type(Type::Function(rf)) =
        &mut f.write().unwrap().return_type
      {
        rf.abstract_ancestor = Some(dispatch);
      }
    }
  }
  /// Returns whether any signature changed.
  pub fn propagate_abstract_function_signatures(&mut self) -> bool {
    let mut any_changed = false;
    loop {
      let mut changed = false;
      for top_level_f in self.abstract_functions_iter() {
        let mut borrowed_f = top_level_f.write().unwrap();
        match &borrowed_f.implementation {
          FunctionImplementationKind::Composite(implementation) => {
            let mut borrowed_implementation = implementation.write().unwrap();
            // Only a function-typed name, application, `let`, or block
            // without an ancestor can gain one below, so a body without
            // one has nothing to walk.
            let fillable =
              borrowed_implementation.expression.any_subexpression(|exp| {
                matches!(
                  exp.kind,
                  ExpKind::Name(_)
                    | ExpKind::Application(..)
                    | ExpKind::Let(..)
                    | ExpKind::Block(_)
                ) && exp.data.with_dereferenced(|t| {
                  matches!(
                    t,
                    TypeState::Known(Type::Function(f))
                      if f.abstract_ancestor.is_none()
                  )
                })
              });
            if fillable {
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
                              // Skip when the applied function is the one we're
                              // currently walking (which we hold write-locked):
                              // higher-order-argument inlining of mutually
                              // delegating overloads can produce a specialization
                              // whose body names itself, and `signature.read()`
                              // on our own already-write-locked lock would
                              // deadlock. Reading our own return-type ancestor
                              // mid-mutation would be meaningless anyway.
                              && !Arc::ptr_eq(top_level_f, signature)
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
                  &mut ImmutableProgramLocalContext::empty(self),
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
                      f.abstract_ancestor =
                        Some(inner_abstract_ancestor.clone());
                      changed = true;
                    }
                  });
                  Ok::<bool, Never>(true)
                })
                .unwrap();
            }
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
      any_changed = true;
    }
    any_changed
  }
  /// Returns whether any application was rewritten.
  pub fn inline_local_bound_function_applications(&mut self) -> bool {
    let mut changed = false;
    let mut representative_structs = vec![];
    for f in self.abstract_functions_iter() {
      let borrowed_f = f.read().unwrap();
      match &borrowed_f.implementation {
        // Only calls through a scope field or a local binding are rewritten.
        FunctionImplementationKind::Composite(implementation)
          if !implementation
            .read()
            .unwrap()
            .expression
            .may_apply_local_function() => {}
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
                      changed |= f.kind != ExpKind::Name(new_name.clone());
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
                        let original_name = original_name.clone();
                        if let Some(abstract_fn) =
                          bound_signature.abstract_ancestor
                        {
                          let abstract_fn = abstract_fn.read().unwrap();
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
                            args.push(
                              ExpBuilder::at(&f.source_trace)
                                .name(&original_name, &scope_type),
                            );
                            // As in the Access-callee arm above: the
                            // callee's own function-type view gains the
                            // scope param, so downstream consumers of
                            // that view (interpreter write-back
                            // bookkeeping, reference address-space
                            // monomorphization) stay aligned with the
                            // ancestor, the implementation, and the call
                            // args (pinned by
                            // closure_seeded_capture_read).
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
                          changed |= f.kind != ExpKind::Name(new_name.clone());
                          f.kind = ExpKind::Name(new_name);
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
    changed
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
  /// Repoints every function type's ancestor at the registered signature
  /// it denotes. Inference and monomorphization attach signature *copies*
  /// to references, and overload separation renames only the registered
  /// signatures, so after separation a copy's name no longer names any
  /// function; any later pass reading it would emit a dangling call. Run
  /// right after separation, this leaves no copies for later passes to
  /// trip over: every function reference's ancestor is the registered
  /// signature itself, current name included.
  pub fn canonicalize_function_references(&mut self) {
    let registered_by_implementation =
      self.registered_signatures_by_implementation();
    let mut canonicalize = |signature: &mut FunctionSignature| {
      if let Some(ancestor) = &signature.abstract_ancestor {
        signature.abstract_ancestor = Some(registered_signature(
          ancestor,
          &registered_by_implementation,
        ));
      }
    };
    for signature in self.abstract_functions_iter() {
      // A signature's own types may refer to the signature itself, so
      // they're canonicalized outside its lock.
      let (mut arg_types, mut return_type, mut captured_scope, implementation) = {
        let signature = signature.read().unwrap();
        (
          signature.arg_types.clone(),
          signature.return_type.clone(),
          signature.captured_scope.clone(),
          match &signature.implementation {
            FunctionImplementationKind::Composite(implementation) => {
              Some(implementation.clone())
            }
            _ => None,
          },
        )
      };
      for (arg_type, _) in arg_types.iter_mut() {
        arg_type.for_each_function_signature_mut(&mut canonicalize);
      }
      return_type.for_each_function_signature_mut(&mut canonicalize);
      if let Some(scope) = &mut captured_scope {
        for field in scope.fields.iter_mut() {
          field
            .field_type
            .for_each_function_signature_mut(&mut canonicalize);
        }
      }
      {
        let mut signature = signature.write().unwrap();
        signature.arg_types = arg_types;
        signature.return_type = return_type;
        signature.captured_scope = captured_scope;
      }
      if let Some(implementation) = implementation {
        implementation
          .write()
          .unwrap()
          .expression
          .walk_mut(&mut |exp| {
            exp
              .data
              .kind
              .for_each_function_signature_mut(&mut canonicalize);
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
    for var in self.top_level_vars.iter_mut() {
      var
        .var_type
        .for_each_function_signature_mut(&mut canonicalize);
      if let Some(value) = &mut var.value {
        value
          .walk_mut(&mut |exp| {
            exp
              .data
              .kind
              .for_each_function_signature_mut(&mut canonicalize);
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
  }

  /// Each registered composite function, keyed by its implementation's
  /// identity. See `registered_signature`.
  pub(crate) fn registered_signatures_by_implementation(
    &self,
  ) -> HashMap<usize, Arc<RwLock<AbstractFunctionSignature>>> {
    self
      .abstract_functions_iter()
      .filter_map(|signature| {
        let key = match &signature.read().unwrap().implementation {
          FunctionImplementationKind::Composite(implementation) => {
            Arc::as_ptr(implementation) as usize
          }
          _ => return None,
        };
        Some((key, signature.clone()))
      })
      .collect()
  }

  pub fn extract_inner_functions(&mut self) -> bool {
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
            // Only nested lambdas are extracted, so a body without one has
            // nothing to walk.
            let mut is_root = true;
            if !implementation.read().unwrap().expression.any_subexpression(
              |exp| {
                !std::mem::replace(&mut is_root, false)
                  && matches!(exp.kind, ExpKind::Function(..))
              },
            ) {
              continue;
            }
            // A lambda nested inside an already-extracted closure body
            // refers to the enclosing closure's captures as fields of the
            // enclosing scope param. Capturing that param *whole* would
            // make the nested lambda's scope-construction site pass the
            // bare scope value — a shape the dispatched/audio capture
            // lifts can't express (the param is removed and its fields
            // live in per-capture globals) — so such captures are split
            // into the individual fields the lambda actually reads.
            let enclosing_scope: Option<(Arc<str>, Type)> =
              if let Some(scope_struct) = borrowed_f.captured_scope.as_ref() {
                implementation.read().unwrap().arg_names.last().map(
                  |(scope_param_name, _)| {
                    (
                      scope_param_name.clone(),
                      AbstractType::AbstractStruct(Arc::new(
                        scope_struct.clone(),
                      ))
                      .concretize(
                        &vec![],
                        &self.typedefs,
                        scope_struct.source_trace.clone(),
                      )
                      .unwrap(),
                    )
                  },
                )
              } else {
                None
              };
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
                  if let ExpKind::Function(..) = &exp.kind {
                    let effects = exp.effects();
                    let ExpKind::Function(arg_names, body) = &mut exp.kind
                    else {
                      unreachable!()
                    };
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
                    // Fields of the enclosing closure's scope this lambda
                    // reads (access chains rooted at the enclosing scope
                    // param), captured individually in place of the whole
                    // scope param.
                    let mut scope_field_captures: Vec<(Arc<str>, Type)> =
                      vec![];
                    if let Some((scope_param_name, _)) = &enclosing_scope {
                      body
                        .walk(&mut |e| {
                          if let ExpKind::Access(
                            Accessor::Field(field_name),
                            inner,
                          ) = &e.kind
                            && matches!(
                              &inner.kind,
                              ExpKind::Name(n) if n == scope_param_name
                            )
                            && !scope_field_captures
                              .iter()
                              .any(|(existing, _)| existing == field_name)
                          {
                            scope_field_captures.push((
                              field_name.clone(),
                              e.data.unwrap_known(),
                            ));
                          }
                          Ok::<bool, Never>(true)
                        })
                        .unwrap();
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
                    let captured_vars: Vec<(Arc<str>, Type, Ownership)> =
                      effects
                        .0
                        .iter()
                        .filter_map(|e| match e {
                          Effect::ReadsVar(var_name)
                          | Effect::ReadsArrayLength(var_name) => {
                            if let Some((scope_param_name, _)) =
                              &enclosing_scope
                              && var_name == scope_param_name
                            {
                              // The scope param is compiler-minted, so
                              // every reference to it is a field access;
                              // a read effect with no field accesses
                              // means some pass produced a bare scope
                              // reference this splitting can't see.
                              assert!(
                                !scope_field_captures.is_empty(),
                                "closure reads enclosing scope param \
                                 `{var_name}` but its body contains no \
                                 field accesses rooted at it — \
                                 extraction's field-splitting invariant \
                                 violated"
                              );
                              return None;
                            }
                            match ctx.variables.get(var_name) {
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
                                    var_name.clone(),
                                    var.var_type.unwrap_known(),
                                    var.var_type.ownership,
                                  ))
                                }
                              }
                              None => None,
                            }
                          }

                          // Control flow never crosses a function
                          // boundary: a lambda's `return` exits the lambda
                          // (removed from its effects), and a `break` or
                          // `continue` escaping one is a control-flow error
                          // reported before extraction.
                          Effect::Break | Effect::Continue | Effect::Return => {
                            unreachable!("{e:?} escaped a function boundary")
                          }
                          // Everything else a closure does is fine to do;
                          // only reads can capture.
                          Effect::ModifiesLocalVar(_)
                          | Effect::ModifiesGlobalVar(_)
                          | Effect::SeedsGlobalVar(_)
                          | Effect::Discard
                          | Effect::FragmentExclusiveFunction(_)
                          | Effect::CPUExclusiveFunction(_)
                          | Effect::CPUExclusiveType(_)
                          | Effect::WindowInfo(_)
                          | Effect::Print
                          | Effect::FileWrite
                          | Effect::MidiOut
                          | Effect::Window
                          | Effect::LookupBuiltinAttribute(_)
                          | Effect::InvokesUnknownFunction => None,
                        })
                        .collect();
                    // A variable read both by element access and by
                    // `array-length` contributes two effects — capture it
                    // only once.
                    let mut seen_captured_names: HashSet<Arc<str>> =
                      HashSet::new();
                    let mut captured_vars: Vec<(Arc<str>, Type, Ownership)> =
                      captured_vars
                        .into_iter()
                        .filter(|(name, _, _)| {
                          seen_captured_names.insert(name.clone())
                        })
                        .collect();
                    // The split fields join the ordinary captures (their
                    // names can't collide with locals — scope field names
                    // are the enclosing closure's deshadowed local names).
                    let scope_split_fields: HashSet<Arc<str>> =
                      scope_field_captures
                        .iter()
                        .map(|(field_name, _)| field_name.clone())
                        .collect();
                    for (field_name, field_type) in scope_field_captures {
                      if seen_captured_names.insert(field_name.clone()) {
                        captured_vars.push((
                          field_name,
                          field_type,
                          Ownership::Owned,
                        ));
                      }
                    }
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
                            name: (*name).clone(),
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
                      specialized_from: None,
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
                          dispatched: false,
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
                                    if let Some((enclosing_scope_param, _)) =
                                      &enclosing_scope
                                      && name == enclosing_scope_param
                                      && !scope_split_fields.is_empty()
                                    {
                                      // Access chains rooted at the
                                      // enclosing scope param survive as
                                      // accesses of the same-named fields
                                      // on this lambda's own scope.
                                      *name = scope_name.clone();
                                      e.data = {
                                        let mut t: ExpTypeInfo =
                                          concrete_captured_scope_type
                                            .clone()
                                            .known()
                                            .into();
                                        t.ownership =
                                          Ownership::MutableReference;
                                        t
                                      };
                                    } else if let Some((_, captured_type, _)) =
                                      captured_vars.iter().find(
                                        |(arg_name, _, _)| arg_name == name,
                                      )
                                    {
                                      // The reference keeps the Name node's
                                      // own type, whose abstract ancestor
                                      // inference doesn't reliably stamp on
                                      // *uses* of a closure binding — and
                                      // the propagation pass has no arm for
                                      // field accesses, so a missing
                                      // ancestor here would silently stop
                                      // higher-order inlining from ever
                                      // specializing a call this capture is
                                      // passed to. The captured variable's
                                      // type is guaranteed resolved (fn
                                      // captures with unresolved ancestors
                                      // defer extraction), so stamp from it.
                                      if let TypeState::Known(Type::Function(
                                        fs,
                                      )) = &mut e.data.kind
                                        && fs.abstract_ancestor.is_none()
                                        && let Type::Function(captured_fs) =
                                          captured_type
                                        && let Some(ancestor) =
                                          &captured_fs.abstract_ancestor
                                      {
                                        fs.abstract_ancestor =
                                          Some(ancestor.clone());
                                      }
                                      let name = name.clone();
                                      e.kind = ExpKind::Access(
                                        Accessor::Field(name.clone()),
                                        Box::new(
                                          ExpBuilder::at(&exp.source_trace)
                                            .name_with(
                                              scope_name,
                                              concrete_captured_scope_type,
                                              Ownership::MutableReference,
                                            ),
                                        ),
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
                    let b = ExpBuilder::at(&exp.source_trace);
                    let closure_type =
                      Type::Function(Box::new(FunctionSignature {
                        abstract_ancestor: Some(Arc::new(RwLock::new(
                          signature,
                        ))),
                        args: f_signature.args,
                        return_type: f_signature.return_type,
                      }));
                    *exp = if let Some((
                      captured_scope,
                      concrete_captured_scope_type,
                      _,
                    )) = captured_scope
                    {
                      let capture_types: Vec<Type> = captured_vars
                        .iter()
                        .map(|(_, t, _)| t.clone())
                        .collect();
                      let callee_type =
                        Type::Function(Box::new(FunctionSignature {
                          // The scope construction is, at the value level, a
                          // construction of the scope struct — its callee gets
                          // the struct's constructor as an explicit ancestor,
                          // exactly like any other struct-constructor
                          // application. (The closure-ness of the node lives in
                          // the expression's own type: a function type whose
                          // ancestor is the extracted inner fn.)
                          abstract_ancestor: Some(struct_constructor(
                            &captured_scope.name.0,
                            &capture_types,
                            &concrete_captured_scope_type,
                          )),
                          args: capture_types
                            .iter()
                            .map(|t| {
                              (
                                Variable::immutable(t.clone().known().into()),
                                vec![],
                              )
                            })
                            .collect(),
                          return_type: exp.data.clone(),
                        }));
                      let args = captured_vars
                        .into_iter()
                        .map(|(name, t, ownership)| {
                          let mut arg = if scope_split_fields.contains(&name)
                            && let Some((
                              enclosing_scope_param,
                              enclosing_scope_type,
                            )) = &enclosing_scope
                          {
                            b.field(
                              b.name_with(
                                enclosing_scope_param,
                                enclosing_scope_type,
                                Ownership::MutableReference,
                              ),
                              &name,
                              t,
                            )
                          } else {
                            b.name(&name, &t)
                          };
                          arg.data.ownership = ownership;
                          arg
                        })
                        .collect();
                      b.apply(
                        b.name(&captured_scope.name.0, &callee_type),
                        args,
                        &closure_type,
                      )
                    } else {
                      b.name(&name, &closure_type)
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
    // Every surviving function is registered before any body is rewritten,
    // so a call site needing a specialization an earlier pass created finds
    // it by name rather than creating a duplicate, whatever order the
    // functions are visited in.
    let mut implementations: Vec<Arc<RwLock<TopLevelFunction>>> = vec![];
    for f in self.abstract_functions_iter() {
      let borrowed_f = f.read().unwrap();
      if borrowed_f.has_uninlined_higher_order_arguments() {
        continue;
      }
      match &borrowed_f.implementation {
        FunctionImplementationKind::Composite(implementation) => {
          if !implementations
            .iter()
            .any(|i| Arc::ptr_eq(i, implementation))
          {
            implementations.push(implementation.clone());
          }
          inlined_ctx
            .add_abstract_function(Arc::new(RwLock::new(borrowed_f.clone())));
        }
        FunctionImplementationKind::EnumConstructor(_) => {
          inlined_ctx.add_abstract_function(Arc::clone(f));
        }
        _ => {}
      }
    }
    for implementation in implementations {
      match implementation
        .write()
        .unwrap()
        .expression
        .inline_higher_order_arguments(&mut inlined_ctx)
      {
        Ok(added_new_function) => changed |= added_new_function,
        Err(e) => errors.log(e),
      }
    }
    take(self, |old_ctx| {
      inlined_ctx.top_level_vars = old_ctx.top_level_vars;
      inlined_ctx.window_info_bindings = old_ctx.window_info_bindings;
      inlined_ctx.lifted_audio_captures = old_ctx.lifted_audio_captures;
      inlined_ctx.lifted_gpu_captures = old_ctx.lifted_gpu_captures;
      inlined_ctx.split_globals = old_ctx.split_globals;
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
        .filter_map(|mut s| {
          if s.is_unitlike(&mut names) {
            // a wholly-unitlike struct is dropped entirely
            None
          } else {
            // prune unitlike FIELDS from a struct that survives, keeping the
            // definition consistent with the constructor-argument removal
            // below (which drops the matching unitlike args). Field ACCESSES of
            // these fields are rewritten to Unit in the body walk.
            s.fields.retain(|f| !f.field_type.is_unitlike(&mut names));
            Some(s)
          }
        })
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
            // A field access whose result is unitlike reads a field that was
            // pruned from the struct definition above — it yields Unit.
            ExpKind::Access(Accessor::Field(_), _)
              if exp.data.unwrap_known().is_unitlike(&mut names) =>
            {
              exp.kind = ExpKind::Unit;
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
                      if composite_f.is_some() {
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
            ExpKind::Let(bindings, body) => {
              take(bindings, |bindings| {
                bindings
                  .into_iter()
                  .filter(|(_, _, _, value_exp)| {
                    !(value_exp.data.unwrap_known().is_unitlike(&mut names)
                      && value_exp.effects().is_side_effect_free())
                  })
                  .collect()
              });
              // A unit binding with effects has no value to bind, but it
              // still runs: it becomes a statement between the bindings
              // before and after it (which the walk then visits).
              if let Some(i) = bindings.iter().position(|(_, _, _, value)| {
                value.data.unwrap_known().is_unitlike(&mut names)
              }) {
                let rest = bindings.split_off(i + 1);
                let (_, _, _, statement) = bindings.pop().unwrap();
                take(body.as_mut(), |body| {
                  let b = ExpBuilder::at(&body.source_trace);
                  let data = body.data.clone();
                  let rest = if rest.is_empty() {
                    body
                  } else {
                    b.with_data(ExpKind::Let(rest, body.into()), data.clone())
                  };
                  b.with_data(ExpKind::Block(vec![statement, rest]), data)
                });
              }
              Ok(true)
            }
            _ => Ok::<bool, Never>(true),
          })
          .unwrap();
      }
    }
    // The registry's own signatures lose their unitlike parameters too
    // (calls only strip their own signature copies), so no pass mistakes a
    // stripped parameter for a live one.
    for f in self.abstract_functions_iter() {
      f.write().unwrap().remove_unitlike_arguments(&mut names);
    }
    std::mem::swap(&mut names, &mut self.names.write().unwrap());
  }
  pub fn compile_to_target(
    self,
    target: CompilerTarget,
  ) -> CompileResult<String> {
    let mut names = self.names.write().unwrap();
    let mut compiled_string = target.program_header();
    // A compiler-generated GPU-space var (the implicit `easl_midi_*` /
    // window-info query bindings) emits its WGSL declaration only when a
    // shader actually uses it. Unlike a user var — whose unused
    // declaration still ships for the easl-as-WGSL-library audience —
    // nothing could reference one otherwise, and its type might not even
    // be emitted (`easl_midi_notes`'s `Option<MidiNote>` is monomorphized
    // into the typedefs only if the enum is actually constructed/matched,
    // which audio-only `get-midi-note` use never does).
    let functions = self.emitted_function_closure(target);
    let wgsl_referenced_globals = if target == CompilerTarget::WGSL {
      self.wgsl_referenced_globals(&functions)
    } else {
      HashSet::new()
    };
    for v in self.top_level_vars.iter() {
      if target == CompilerTarget::WGSL
        && !v.directly_user_written
        && matches!(
          v.kind,
          TopLevelVariableKind::Var {
            address_space: VariableAddressSpace::Uniform
              | VariableAddressSpace::StorageRead
              | VariableAddressSpace::StorageReadWrite,
            ..
          }
        )
        && !wgsl_referenced_globals.contains(&v.name)
      {
        continue;
      }
      if self.omits_global(v, target) {
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
      // `MidiNote` is registered in every program's typedefs so the
      // type name always resolves, but its declaration only ships when
      // the program actually references it.
      if &*s.name.0 == "MidiNote" && !self.references_midi_note() {
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
      // Still-generic enums never emit directly (only their
      // monomorphized instances do, via `compile_if_non_generic`), and
      // `flat_data_size_in_u32s` *panics* on a generic payload — so skip
      // them before the runtime-sized check. The builtin generic
      // `Option` is always present even in programs that never use it.
      if !e.generic_args.is_empty() {
        continue;
      }
      // Enums with CPU-only payloads (runtime-sized, String, or Video
      // ones; the predicate structs use) have no GPU layout: skip their
      // emission entirely. GPU code referencing one is caught by
      // validation, so nothing in the emitted output can miss it.
      if e.variants.iter().any(|v| {
        v.inner_type
          .flat_data_size_in_u32s(&e.source_trace)
          .is_err()
          || v
            .inner_type
            .concretize(&vec![], &self.typedefs, SourceTrace::empty())
            .is_ok_and(|t| self.type_makes_struct_cpu_only(&t))
      }) {
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
      // Functions whose signatures involve CPU-only types (runtime-sized,
      // String, or Video values, directly or inside enums/structs:
      // `type_makes_struct_cpu_only`, so they're skipped exactly when the
      // types they name are) have no valid WGSL/C representation. This gate covers the synthesized definitions this
      // loop emits (enum constructors), which never pass through
      // `TopLevelFunction::compile` and so can't be caught by its
      // `allowed_on_gpu` check; composite functions get the equivalent
      // signature check there. GPU code reaching a CPU-only function is a
      // validation error; here it's simply not emitted.
      // (A still-generic signature isn't emitted at all.)
      let signature_is_cpu_only = f.generic_args.is_empty()
        && f
          .arg_types
          .iter()
          .map(|(t, _)| t)
          .chain(std::iter::once(&f.return_type))
          .any(|t| {
            t.concretize(&vec![], &self.typedefs, SourceTrace::empty())
              .is_ok_and(|t| self.type_makes_struct_cpu_only(&t))
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
            let discriminant = e
              .variants
              .iter()
              .position(|v| v.name == original_variant_name)
              .expect("EnumConstructor fn name didn't match any variant");
            let variant_types: Vec<Type> = e
              .variants
              .iter()
              .map(|v| {
                v.inner_type
                  .concretize(&vec![], &self.typedefs, SourceTrace::empty())
                  .expect("a monomorphized enum's payloads are concrete")
              })
              .collect();
            let inner_type = &variant_types[discriminant];
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
            let enum_name =
              compile_word(e.original_ancestor().monomorphized_name(
                &variant_types,
                &mut names,
                target,
              ));
            compiled_string += &match target {
              CompilerTarget::WGSL => {
                let bitcast_inner_values = inner_type
                  .bitcastable_chunk_accessors("value".into())
                  .into_iter()
                  .map(|exp| {
                    // WGSL can't `bitcast` a `bool`, but it converts one
                    // to `u32` (false → 0, true → 1) with `u32(...)`;
                    // `bitcasted_from_enum_data_inner` unpacks it back
                    // with `!= 0u`.
                    let is_bool = matches!(exp.data.unwrap_known(), Type::Bool);
                    let compiled = exp.compile(
                      ExpressionCompilationPosition::InnerExpression,
                      &mut names,
                      target,
                    );
                    if is_bool {
                      format!("u32({compiled})")
                    } else {
                      format!("bitcast<u32>({compiled})")
                    }
                  })
                  .chain(std::iter::repeat("0u".into()))
                  .take(e.inner_flat_data_size_in_u32s()?)
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
                    let is_bool = matches!(exp.data.unwrap_known(), Type::Bool);
                    let compiled = exp.compile(
                      ExpressionCompilationPosition::InnerExpression,
                      &mut names,
                      target,
                    );
                    // A C `bool` is one byte, so a `sizeof(uint32_t)`
                    // memcpy would over-read it; store 0/1 directly
                    // (matching the WGSL `u32(bool)` pack and the
                    // `!= 0u` unpack).
                    if is_bool {
                      format!("result.data[{i}] = ({compiled}) ? 1u : 0u;")
                    } else {
                      format!(
                        "memcpy(&result.data[{i}], &{compiled}, \
                         sizeof(uint32_t));"
                      )
                    }
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
    // Closure-based emission (see `emitted_function_closure`): the output's
    // function set is the transitive call-graph closure of the root set, so
    // an emitted function can never reference an unemitted one, and audio
    // clones / CPU-only specializations fall away by unreachability. This is
    // also the set `wgsl_referenced_globals` walks to decide which
    // compiler-generated vars' declarations to emit.
    for (f_name, implementation) in functions.ordered {
      if !functions.emitted.contains(&f_name) {
        continue;
      }
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
      }
      compiled_string += &implementation.read().unwrap().clone().compile(
        &f_name,
        &mut names,
        functions.valid.contains(&f_name),
        target,
      )?;
      compiled_string += "\n\n";
    }
    Ok(compiled_string)
  }
  /// Expands n-ary applications of associative functions into nested
  /// binary ones, `(+ a b c)` into `(+ (+ a b) c)`, in every function body
  /// and top-level variable's value.
  pub fn expand_associative_applications(&mut self) {
    for f in self.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(f) =
        &f.read().unwrap().implementation
      {
        f.write()
          .unwrap()
          .expression
          .expand_associative_applications();
      }
    }
    for var in self.top_level_vars.iter_mut() {
      if let Some(value) = &mut var.value {
        value.expand_associative_applications();
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
          .keys()
          .chain(self.overload_groups.keys())
          .cloned(),
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
              let b = ExpBuilder::at(&body.source_trace);
              let data = body.data.clone();
              let bindings = mutable_args
                .into_iter()
                .map(|((arg_name, _), arg_type)| {
                  (
                    arg_name.clone(),
                    SourceTrace::empty(),
                    VariableKind::Var,
                    b.with_data(ExpKind::Name(arg_name), arg_type),
                  )
                })
                .collect();
              b.with_data(ExpKind::Let(bindings, body), data).into()
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
        if !is_valid_qualified_name(&signature.name) {
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
          if !is_valid_qualified_name(generic_name) {
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
              if !is_valid_qualified_name(name) {
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
      if !is_valid_qualified_name(&e.name.0) {
        errors.log(CompileError::new(
          CompileErrorKind::InvalidName,
          e.name.1.clone(),
        ));
      }
      for (name, _, source) in e.generic_args.iter() {
        if !is_valid_qualified_name(name) {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidName,
            source.clone(),
          ));
        }
      }
      for variant in e.variants.iter() {
        if !is_valid_qualified_name(&variant.name) {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidName,
            variant.source.clone(),
          ));
        }
      }
    }
    for s in self.typedefs.structs.iter() {
      if !is_valid_qualified_name(&s.name.0) {
        errors.log(CompileError::new(
          CompileErrorKind::InvalidName,
          s.name.1.clone(),
        ));
      }
      for (name, _, source) in s.generic_args.iter() {
        if !is_valid_qualified_name(name) {
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
  /// Rejects any generic parameter — type or const — a user function,
  /// struct, or enum declares but never references in its type signature
  /// (for a function: its argument and return types; for a struct/enum:
  /// its fields/variant payloads). Such a generic can never be inferred
  /// from a use site, so the definition is only ever usable if it's
  /// never actually used — a latent error that otherwise surfaces as a
  /// confusing inference failure at the first call. Runs before type
  /// inference, so only the user's own definitions and builtins are
  /// present; builtins are skipped (functions by their non-Composite
  /// implementation, structs/enums by their positionless source trace).
  fn validate_generic_usage(&self, errors: &mut ErrorLog) {
    let report_unused =
      |generic_args: &[(Arc<str>, GenericArgument, SourceTrace)],
       used: &HashSet<Arc<str>>,
       errors: &mut ErrorLog| {
        for (name, _, source_trace) in generic_args.iter() {
          if !used.contains(name) {
            errors.log(CompileError {
              kind: CompileErrorKind::UnusedGeneric(name.to_string()),
              source_trace: source_trace.clone(),
            });
          }
        }
      };
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if signature.generic_args.is_empty()
        || !matches!(
          signature.implementation,
          FunctionImplementationKind::Composite(_)
        )
      {
        continue;
      }
      let mut used = vec![];
      for (t, _) in signature.arg_types.iter() {
        t.track_all_generic_names(&mut used);
      }
      signature.return_type.track_all_generic_names(&mut used);
      report_unused(
        &signature.generic_args,
        &used.into_iter().collect(),
        errors,
      );
    }
    for s in self.typedefs.structs.iter() {
      if s.generic_args.is_empty() || s.source_trace.primary_position.is_none()
      {
        continue;
      }
      let mut used = vec![];
      for field in s.fields.iter() {
        field.field_type.track_all_generic_names(&mut used);
      }
      report_unused(&s.generic_args, &used.into_iter().collect(), errors);
    }
    for e in self.typedefs.enums.iter() {
      if e.generic_args.is_empty() || e.source_trace.primary_position.is_none()
      {
        continue;
      }
      let mut used = vec![];
      for variant in e.variants.iter() {
        variant.inner_type.track_all_generic_names(&mut used);
      }
      report_unused(&e.generic_args, &used.into_iter().collect(), errors);
    }
  }
  fn catch_duplicate_signatures(&self, errors: &mut ErrorLog) {
    for (name, signatures) in self.abstract_functions.iter() {
      Self::catch_duplicates_among(name, signatures, errors);
    }
  }
  /// Catches functions that meet in an overload group with signatures
  /// identical to each other or to a builtin's.
  fn catch_duplicate_overload_group_signatures(&self, errors: &mut ErrorLog) {
    for (group, members) in self.overload_groups.iter() {
      let signatures: Vec<Arc<RwLock<AbstractFunctionSignature>>> = members
        .iter()
        .flat_map(|member| {
          self.abstract_functions.get(member).into_iter().flatten()
        })
        .cloned()
        .collect();
      Self::catch_duplicates_among(
        &display_name(group).into(),
        &signatures,
        errors,
      );
    }
  }
  fn catch_duplicates_among(
    name: &Arc<str>,
    signatures: &Vec<Arc<RwLock<AbstractFunctionSignature>>>,
    errors: &mut ErrorLog,
  ) {
    {
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
            if self.names_functions(arg_name)
              || self
                .top_level_vars
                .iter()
                .find(|v| v.name == *arg_name)
                .is_some()
            {
              errors.log(CompileError::new(
                CantShadowTopLevelBinding(display_name(arg_name).to_string()),
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
  /// Reports every declaration whose type holds a texture or sampler
  /// inside an array, struct, or enum: top-level vars, struct fields, enum
  /// payloads, and function parameters and return types. Neither WGSL nor
  /// the VM can hold one there (WGSL textures are whole bindings; the VM
  /// keeps textures host-side, per global). Local bindings are rejected
  /// separately (`LocalTextureOrSampler`).
  pub fn catch_nested_textures_and_samplers(&self, errors: &mut ErrorLog) {
    let nested = |t: &Type, source: &SourceTrace| {
      t.nests_texture_or_sampler().then(|| {
        CompileError::new(
          NestedTextureOrSampler(TypeDescription::from(t.clone()).to_string()),
          source.clone(),
        )
      })
    };
    for var in self.top_level_vars.iter() {
      if let Some(error) = nested(&var.var_type, &var.source_trace) {
        errors.log(error);
      }
    }
    let concrete = |t: &AbstractType, source: &SourceTrace| {
      t.concretize(&vec![], &self.typedefs, source.clone()).ok()
    };
    // Generic definitions are checked through their instances, which only
    // reach code through the var types and signatures checked as concrete
    // types here.
    for s in self
      .typedefs
      .structs
      .iter()
      .filter(|s| !s.opaque && s.generic_args.is_empty())
    {
      for field in s.fields.iter() {
        if let Some(t) = concrete(&field.field_type, &field.source_trace)
          && t.involves_texture_or_sampler()
        {
          errors.log(CompileError::new(
            NestedTextureOrSampler(display_name(&s.name.0).to_string()),
            field.source_trace.clone(),
          ));
        }
      }
    }
    for e in self
      .typedefs
      .enums
      .iter()
      .filter(|e| e.generic_args.is_empty())
    {
      for variant in e.variants.iter() {
        if let Some(t) = concrete(&variant.inner_type, &variant.source)
          && t.involves_texture_or_sampler()
        {
          errors.log(CompileError::new(
            NestedTextureOrSampler(display_name(&e.name.0).to_string()),
            variant.source.clone(),
          ));
        }
      }
    }
    for signature in self.abstract_functions_iter() {
      let signature = signature.read().unwrap();
      if !signature.generic_args.is_empty() {
        continue;
      }
      let FunctionImplementationKind::Composite(f) = &signature.implementation
      else {
        continue;
      };
      let f = f.read().unwrap();
      if let Some(Type::Function(function_type)) =
        f.expression.data.kind.try_unwrap_known()
      {
        for ((arg, _), (_, source)) in
          function_type.args.iter().zip(f.arg_names.iter())
        {
          if let Some(t) = arg.var_type.kind.try_unwrap_known()
            && let Some(error) = nested(&t, source)
          {
            errors.log(error);
          }
        }
        if let Some(t) = function_type.return_type.kind.try_unwrap_known()
          && let Some(error) = nested(&t, &f.name_source_trace)
        {
          errors.log(error);
        }
      }
    }
  }
  /// Reports every integer literal outside its inferred type's range: a
  /// negative `u32` (`-1u`), or a value too big for `u32` or `i32`.
  pub fn validate_integer_literals(&self, errors: &mut ErrorLog) {
    let mut check = |exp: &TypedExp| {
      exp
        .walk(&mut |exp| {
          if let ExpKind::NumberLiteral(Number::Int(n)) = &exp.kind
            && let Some(t) = exp.data.kind.try_unwrap_known()
          {
            let (fits, type_name) = match t {
              Type::U32 => (u32::try_from(*n).is_ok(), "u32"),
              Type::I32 => (i32::try_from(*n).is_ok(), "i32"),
              _ => (true, ""),
            };
            if !fits {
              errors.log(CompileError::new(
                IntegerLiteralOutOfRange(*n, type_name.to_string()),
                exp.source_trace.clone(),
              ));
            }
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    };
    for signature in self.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(f) =
        &signature.read().unwrap().implementation
      {
        check(&f.read().unwrap().expression);
      }
    }
    for var in self.top_level_vars.iter() {
      if let Some(value) = &var.value {
        check(value);
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
        // Deexpressionification can rename parameters (to rebind them as
        // addressable `var`s).
        if let ExpKind::Function(arg_names, _) = &f.expression.kind {
          f.arg_names = arg_names.clone();
        }
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
          let suffix_types: Vec<&Type> =
            if type_signature.iter().any(|t| t.involves_function()) {
              let non_fn: Vec<&Type> = type_signature
                .iter()
                .filter(|t| !t.involves_function())
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
                *exp = ExpBuilder::at(&exp.source_trace).u32(size);
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
      for error in f.wgsl_runtime_sized_errors() {
        errors.log(error);
      }
      let mut discovered: Vec<(Arc<str>, Arc<RwLock<TopLevelFunction>>)> =
        vec![];
      f.expression
        .walk(&mut |exp| {
          // Struct/enum *values* with runtime-sized contents. (Only checked
          // here, before the capture lifts: afterwards a split global's
          // accesses keep its original type though they resolve to its
          // parts, so emission can't use this rule.)
          if exp.data.with_known(|t| {
            matches!(t, Type::Struct(_) | Type::Enum(_))
              && t.involves_runtime_sized_array()
          }) {
            errors.log(CompileError::new(
              RuntimeSizedFieldOnGpu,
              exp.source_trace.clone(),
            ));
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
                    display_name(name).to_string(),
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

          // The type of an attributable value: an argument or return
          // type, or the named field of a struct one (unknown when it's
          // still generic).
          let io_value_type =
            |t: &Type, field: Option<(&AbstractStruct, &str)>| match field {
              None => Some(t.clone()),
              Some((s, field_name)) => s
                .fields
                .iter()
                .find(|f| &*f.name == field_name)
                .and_then(|f| {
                  f.field_type
                    .concretize(&vec![], &self.typedefs, SourceTrace::empty())
                    .ok()
                }),
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
              HashMap<usize, Option<Type>>,
              HashMap<BuiltinIOAttribute, Result<Type, AbstractType>>,
            ) {
              for (name, source) in check_for_duplicate_builtins(&attributables)
              {
                errors.log(CompileError::new(
                  DuplicateBuiltinAttribute(input_or_output, name),
                  source,
                ))
              }
              // Each used location's value type (unknown for a generic
              // struct field).
              let mut used_locations: HashMap<usize, Option<Type>> =
                HashMap::new();
              let mut used_builtins: HashMap<
                BuiltinIOAttribute,
                Result<Type, AbstractType>,
              > = HashMap::new();
              for (t, attributable) in attributables.iter() {
                let attributes = match attributable {
                  Ok(a) => &*a,
                  Err((_, _, a)) => a,
                };
                let value_type = io_value_type(
                  t,
                  attributable
                    .as_ref()
                    .err()
                    .map(|(s, field_name, _)| (&**s, &**field_name)),
                );
                // WGSL requires integer values passed between stages to be
                // `flat` (emission adds it when nothing is written).
                if let Some((interpolation, source)) =
                  attributes.interpolation()
                  && !interpolation.is_flat()
                  && let Some(value_type) = &value_type
                  && value_type.is_integer_scalar_or_vector()
                {
                  errors.log(CompileError::new(
                    NonFlatIntegerInterpolation(interpolation.describe()),
                    source.clone(),
                  ));
                }
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
                  if let Some((location, _)) = attributes.location() {
                    used_locations.insert(location, value_type);
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
                        used_locations.insert(untaken_location, Some(t));
                      } else {
                        errors.log(CompileError::new(
                          InvalidTypeForEntryPoint(t.into(), input_or_output),
                          f_source.clone(),
                        ));
                      }
                    }
                    Err((s, field_name, _)) => {
                      used_locations.insert(
                        untaken_location,
                        io_value_type(&t, Some((&s, &field_name))),
                      );
                      inferred_struct_field_locations.push((
                        s,
                        field_name,
                        untaken_location,
                      ));
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
              if let Some(t) = used_output_locations.get(&0) {
                let valid = t.as_ref().is_some_and(|t| {
                  if let Type::Struct(s) = t
                    && &*s.name == "vec4"
                  {
                    matches!(
                      s.fields[0].field_type.unwrap_known(),
                      Type::F32 | Type::U32 | Type::I32
                    )
                  } else {
                    false
                  }
                });
                if !valid {
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
  /// Rewrites each reference to an overload group into a reference to the
  /// member inference chose, whose name its function type's ancestor
  /// carries.
  pub fn resolve_overload_groups(&mut self) {
    if self.overload_groups.is_empty() {
      return;
    }
    let groups = std::mem::take(&mut self.overload_groups);
    let resolve = |exp: &mut TypedExp| {
      exp
        .walk_mut(&mut |e| {
          if let ExpKind::Name(name) = &e.kind
            && groups.contains_key(name)
          {
            let TypeState::Known(Type::Function(signature)) = &e.data.kind
            else {
              panic!("overload group `{name}` resolved to a non-function")
            };
            let member = signature
              .abstract_ancestor
              .as_ref()
              .expect("overload group reference without an ancestor")
              .read()
              .unwrap()
              .name
              .clone();
            e.kind = ExpKind::Name(member);
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    };
    for f in self.abstract_functions_iter() {
      let FunctionImplementationKind::Composite(implementation) =
        f.read().unwrap().implementation.clone()
      else {
        continue;
      };
      resolve(&mut implementation.write().unwrap().expression);
    }
    for var in self.top_level_vars.iter_mut() {
      if let Some(value) = &mut var.value {
        resolve(value);
      }
    }
  }
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
            let concrete_arg_types: Vec<Type> =
              if let TypeState::Known(Type::Function(signature)) =
                &f_exp.data.kind
              {
                signature
                  .args
                  .iter()
                  .map(|(arg, _)| arg.var_type.unwrap_known())
                  .collect()
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
                    let candidate = candidate.read().unwrap();
                    candidate.arg_types.len() == concrete_arity
                      && candidate
                        .arg_types
                        .iter()
                        .zip(concrete_arg_types.iter())
                        .all(|((abstract_type, _), concrete)| {
                          abstract_type_shallow_matches(abstract_type, concrete)
                        })
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
                  let value_type = value.data.unwrap_known();
                  if !value_type.is_constructible() {
                    errors.log(CompileError::new(
                      if value_type.involves_texture_or_sampler() {
                        LocalTextureOrSampler
                      } else {
                        CantBindNonConstructible
                      },
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
    self.validate_raw_program_with(target, ValidationOptions::default())
  }
  /// [`Self::validate_raw_program`] with non-default `options`.
  pub fn validate_raw_program_with(
    &mut self,
    target: CompilerTarget,
    options: ValidationOptions,
  ) -> ErrorLog {
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
    self.validate_generic_usage(&mut errors);
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
    self.catch_duplicate_overload_group_signatures(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.inline_def_array_sizes();
    self.fully_infer_types(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.resolve_overload_groups();
    self.rewrite_aliased_builtin_calls();
    self.box_function_values(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_integer_literals(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_nested_textures_and_samplers(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
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
    self.desugar_swizzle_assignments();
    self.deexpressionify(target);
    self.normalize_pseudoapplication_data_accesses();
    self.validate_ref_arg_aliasing(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.deshadow(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_ref_captures(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.monomorphize(&mut errors, target);
    if !errors.is_empty() {
      return errors;
    }
    self.lower_enum_comparisons(target, &mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.separate_overloaded_fns(target);
    self.canonicalize_function_references();
    self.catch_duplicate_closures_capturing_mutable_variables(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    loop {
      let extracted = self.extract_inner_functions();
      // Stamp each closure-returning function's declared return type from the
      // scope struct its body actually constructs — the one anchor extraction
      // leaves unambiguous — BEFORE signature propagation runs. Two
      // higher-order specializations that return structurally-identical
      // sibling closures (`distortionify` applied to two different layers) are
      // indistinguishable to propagation's `is_none` ancestor fills, which
      // then lock one spec's return type onto the other's closure; stamping
      // the correct `Some` here makes those fills skip it, so each spec stays
      // its own directly-dispatched closure.
      self.stamp_returned_closure_return_types();
      // Extraction defers a lambda capturing a function whose ancestor isn't
      // resolved yet; propagation and local inlining are what resolve (or
      // remove) such captures, so their progress keeps the loop going too.
      let propagated = self.propagate_abstract_function_signatures();
      let locally_inlined = self.inline_local_bound_function_applications();
      let inlined = self.inline_all_higher_order_arguments(&mut errors);
      if !errors.is_empty() {
        return errors;
      }
      if !extracted && !inlined && !propagated && !locally_inlined {
        break;
      }
    }
    self.defunctionalize_boxed_functions(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_closure_state_aliasing(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.remove_unitlike_values();
    self.extract_non_bound_mutable_references();
    self.refine_closure_reference_mutability();
    // Rewrites window-info queries into binding reads before effect
    // validation, so GPU entries no longer contain the queries (or the
    // String key literals they take).
    self.extract_gpu_window_info();
    self.validate_gpu_runtime_sized_use(&mut errors);
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
    // Before the dispatched-closure lift and entry marking, which then
    // see each workgroup-sized copy as an ordinary dispatched function.
    self.specialize_dispatch_workgroup_sizes(&mut errors);
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
    // Needs the implicit entry markings (above): a dispatched closure or
    // unannotated dispatched function is checked like an annotated entry.
    self.validate_top_level_fn_effects(&mut errors);
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
    self.extract_workgroup_size();
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
    if options.window_regions {
      self.offset_fragment_positions();
    }
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
    self.validate_thread_shared_atomics(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.assign_elided_bindings();
    self.order_top_level_vars_by_dependency(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.mark_value_names_owned();
    self.track_emulated_builtins(target);
    self.has_been_validated = true;
    errors
  }
  /// Orders `top_level_vars` so each initializer comes after every
  /// top-level variable it reads, directly or through the functions it
  /// calls, and otherwise keeps source order: the runtimes run initializers
  /// in this order. Initializers that read each other in a cycle are
  /// `CyclicTopLevelDefinitions`.
  fn order_top_level_vars_by_dependency(&mut self, errors: &mut ErrorLog) {
    let index_of: HashMap<Arc<str>, usize> = self
      .top_level_vars
      .iter()
      .enumerate()
      .map(|(i, v)| (v.name.clone(), i))
      .collect();
    let mut memo = EffectsMemo::default();
    let dependencies: Vec<Vec<usize>> = self
      .top_level_vars
      .iter()
      .map(|v| {
        let Some(value) = &v.value else {
          return vec![];
        };
        let mut reads: Vec<usize> = value
          .effects_with(&mut memo)
          .0
          .iter()
          .filter_map(|effect| match effect {
            Effect::ReadsVar(name) | Effect::ReadsArrayLength(name) => {
              index_of.get(name).copied()
            }
            _ => None,
          })
          .collect();
        reads.sort();
        reads.dedup();
        reads
      })
      .collect();
    match topological_order(&dependencies) {
      Ok(order) => {
        let mut vars: Vec<Option<TopLevelVar>> =
          self.top_level_vars.drain(..).map(Some).collect();
        self.top_level_vars =
          order.into_iter().map(|i| vars[i].take().unwrap()).collect();
      }
      Err(cycles) => {
        for cycle in cycles {
          let vars = &self.top_level_vars;
          let names = cycle
            .iter()
            .chain(std::iter::once(&cycle[0]))
            .map(|i| display_name(&vars[*i].name).to_string())
            .collect();
          let source_trace = cycle[1..]
            .iter()
            .fold(vars[cycle[0]].source_trace.clone(), |trace, i| {
              trace.insert_as_secondary(vars[*i].source_trace.clone())
            });
          errors.log(CompileError::new(
            CyclicTopLevelDefinitions(names),
            source_trace,
          ));
        }
      }
    }
  }

  /// Marks every name bound to a value as owned, in every function body and
  /// top-level variable's value (see `TypedExp::mark_value_names_owned`).
  fn mark_value_names_owned(&mut self) {
    for f in self.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(f) =
        &f.read().unwrap().implementation
      {
        f.write().unwrap().expression.mark_value_names_owned();
      }
    }
    for var in self.top_level_vars.iter_mut() {
      if let Some(value) = &mut var.value {
        value.mark_value_names_owned();
      }
    }
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
      let f = f.read().unwrap();
      if f.generic_args.is_empty()
        && !f.has_uninlined_higher_order_arguments()
        && let FunctionImplementationKind::Composite(implementation) =
          &f.implementation
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
            // Both the closure a scope construction builds (its expression
            // type's ancestor) and the function an application calls (its
            // callee's): an unregistered closure's body can call functions
            // that are themselves unregistered, like a higher-order
            // specialization taking the closure's scope by reference.
            if let ExpKind::Application(f, _) = &exp.kind {
              for data in [&exp.data, &f.data] {
                if let TypeState::Known(Type::Function(signature)) = &data.kind
                  && let Some(ancestor) = &signature.abstract_ancestor
                {
                  let ancestor = ancestor.read().unwrap();
                  if let FunctionImplementationKind::Composite(implementation) =
                    &ancestor.implementation
                    && !dependencies.contains_key(&ancestor.name)
                    && !discovered
                      .iter()
                      .any(|(name, _)| *name == ancestor.name)
                  {
                    discovered
                      .push((ancestor.name.clone(), implementation.clone()));
                  }
                }
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
    let index_of: HashMap<&Arc<str>, usize> = fns
      .iter()
      .enumerate()
      .map(|(i, (name, _))| (name, i))
      .collect();
    let dependencies: Vec<Vec<usize>> = fns
      .iter()
      .map(|(name, _)| {
        dependencies[name]
          .iter()
          .map(|dependency| index_of[dependency])
          .collect()
      })
      .collect();
    let Ok(order) = topological_order(&dependencies) else {
      panic!(
        "Couldn't find topological sort of user functions.\n\
         If you're seeing this, there's a compiler bug; an earlier compiler \
         stage should have caught the dependency loop "
      )
    };
    order.into_iter().map(|i| fns[i].clone()).collect()
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
  /// Whether any function sends MIDI or picks MIDI output ports
  /// (`Effect::MidiOut`).
  pub fn sends_midi(&self) -> bool {
    let mut memo = EffectsMemo::default();
    self.abstract_functions_iter().any(|f| {
      match &f.read().unwrap().implementation {
        FunctionImplementationKind::Composite(implementation) => implementation
          .read()
          .unwrap()
          .effects_with(&mut memo)
          .contains(&Effect::MidiOut),
        _ => false,
      }
    })
  }
  pub fn gpu_used_globals(&self) -> HashSet<Arc<str>> {
    let var_names: HashSet<Arc<str>> =
      self.top_level_vars.iter().map(|v| v.name.clone()).collect();
    let mut used: HashSet<Arc<str>> = HashSet::new();
    let mut memo = EffectsMemo::default();
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
          .effects_with(&mut memo)
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
  /// The functions actually emitted to `target`: the transitive call-graph
  /// closure of the root set (this target's entry points plus every
  /// directly-user-written, transitively-target-valid function — the
  /// easl-as-a-library contract). Shared by `compile_to_target`'s function
  /// emission and `wgsl_referenced_globals`.
  fn emitted_function_closure(
    &self,
    target: CompilerTarget,
  ) -> EmittedFunctions {
    use crate::compiler::expression::ExpKind;
    let ordered_functions = self.composite_functions_in_usage_order();
    let registry_names: HashSet<Arc<str>> = ordered_functions
      .iter()
      .map(|(name, _)| name.clone())
      .collect();
    let mut callees: HashMap<Arc<str>, Vec<Arc<str>>> = HashMap::new();
    for (f_name, implementation) in ordered_functions.iter() {
      let mut found: Vec<Arc<str>> = vec![];
      implementation
        .read()
        .unwrap()
        .expression
        .walk(&mut |exp| {
          if let ExpKind::Name(name) = &exp.kind
            && registry_names.contains(name)
            && name != f_name
          {
            found.push(name.clone());
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
      callees.insert(f_name.clone(), found);
    }
    let unemittable_globals = if target == CompilerTarget::WGSL {
      self.wgsl_unemittable_globals()
    } else {
      HashSet::new()
    };
    // `ordered_functions` is topological (callees first), so one pass
    // suffices for the transitive-validity fixpoint.
    let mut valid: HashSet<Arc<str>> = HashSet::new();
    let mut transitively_valid: HashMap<Arc<str>, bool> = HashMap::new();
    let mut memo = EffectsMemo::default();
    for (f_name, implementation) in ordered_functions.iter() {
      let locally_valid = implementation.read().unwrap().is_valid_for_target(
        self,
        target,
        &unemittable_globals,
        &mut memo,
      );
      if locally_valid {
        valid.insert(f_name.clone());
      }
      transitively_valid.insert(
        f_name.clone(),
        locally_valid
          && callees[f_name]
            .iter()
            .all(|callee| transitively_valid[callee]),
      );
    }
    let mut emitted: HashSet<Arc<str>> = HashSet::new();
    let mut queue: Vec<Arc<str>> = ordered_functions
      .iter()
      .filter(|(f_name, implementation)| {
        let implementation = implementation.read().unwrap();
        implementation
          .entry_point
          .map(|entry| entry.should_compile_to_target(target))
          .unwrap_or(false)
          || (implementation.directly_user_written
            && transitively_valid[f_name])
      })
      .map(|(f_name, _)| f_name.clone())
      .collect();
    while let Some(f_name) = queue.pop() {
      if emitted.insert(f_name.clone()) {
        queue.extend(callees[&f_name].iter().cloned());
      }
    }
    EmittedFunctions {
      ordered: ordered_functions,
      emitted,
      valid,
    }
  }
  /// Whether `target`'s output leaves out the global `v`, whose type has
  /// no form there:
  /// - a runtime-sized array without a binding, in WGSL (e.g. a
  ///   `load-wav`ed sample buffer: WGSL has no unsized array outside a
  ///   storage binding);
  /// - a CPU-only type, in any target (Strings, videos, runtime-sized
  ///   arrays embedded in a struct or enum, closures that can't be
  ///   emitted); a runtime-sized array itself has a form as a storage
  ///   binding;
  /// - in WGSL, a GPU-space var involving a bool, which isn't
  ///   host-shareable and is guaranteed GPU-unused by
  ///   `validate_gpu_used_binding_types` (`var<private> b: bool` is valid
  ///   WGSL, so `@local` bools stay).
  fn omits_global(&self, v: &TopLevelVar, target: CompilerTarget) -> bool {
    let is_runtime_sized_array = matches!(
      &v.var_type,
      Type::Array(Some(ConcreteArraySize::Unsized), _)
    );
    let gpu_space = matches!(
      v.kind,
      TopLevelVariableKind::Var {
        address_space: VariableAddressSpace::Uniform
          | VariableAddressSpace::StorageRead
          | VariableAddressSpace::StorageReadWrite,
        ..
      }
    );
    let has_binding = matches!(
      v.kind,
      TopLevelVariableKind::Var {
        group_and_binding: Some(_),
        ..
      }
    );
    // A runtime-sized array is a valid binding when its elements are.
    let cpu_only = match &v.var_type {
      Type::Array(Some(ConcreteArraySize::Unsized), element_type) => {
        self.type_makes_struct_cpu_only(&element_type.kind.unwrap_known())
      }
      t => self.type_makes_struct_cpu_only(t),
    };
    (target == CompilerTarget::WGSL && is_runtime_sized_array && !has_binding)
      || cpu_only
      || (target == CompilerTarget::WGSL
        && gpu_space
        && v.var_type.involves_bool())
  }
  /// The globals a WGSL function can't refer to: GPU-space globals whose
  /// types can't be a WGSL binding, and globals WGSL output leaves out
  /// (`omits_global`). A function referring to one has no WGSL form
  /// either (see `TopLevelFunction::is_valid_for_target`), even when it
  /// only touches the global's plain fields.
  fn wgsl_unemittable_globals(&self) -> HashSet<Arc<str>> {
    self
      .top_level_vars
      .iter()
      .filter(|v| {
        let gpu_space_without_binding =
          matches!(
            v.kind,
            TopLevelVariableKind::Var {
              address_space: VariableAddressSpace::Uniform
                | VariableAddressSpace::StorageRead
                | VariableAddressSpace::StorageReadWrite,
              ..
            }
          ) && !v.var_type.is_valid_wgsl_binding_layout();
        gpu_space_without_binding || self.omits_global(v, CompilerTarget::WGSL)
      })
      .map(|v| v.name.clone())
      .collect()
  }
  /// The top-level vars referenced by name anywhere in the WGSL output:
  /// `gpu_used_globals` (entry-point effect reads/writes, textures) plus
  /// every top-level-var `Name` in an *actually-emitted* function body
  /// (`emitted_function_closure`). Decides whether a compiler-generated
  /// GPU-space var's declaration emits. `gpu_used_globals` alone misses a
  /// var read only by a library-root helper (emitted but not
  /// entry-reachable); the *emission closure* rather than the looser
  /// "any target-valid function" is essential the other way — an audio
  /// clone reading a capture global (e.g. a `Wavetable`) can pass the
  /// signature-only `is_valid_for_target` yet never be emitted (audio
  /// entries don't compile to WGSL), and counting it would emit a var
  /// whose type isn't even in the typedefs → dangling reference.
  /// `functions` is the WGSL `emitted_function_closure`.
  fn wgsl_referenced_globals(
    &self,
    functions: &EmittedFunctions,
  ) -> HashSet<Arc<str>> {
    use crate::compiler::expression::ExpKind;
    let var_names: HashSet<Arc<str>> =
      self.top_level_vars.iter().map(|v| v.name.clone()).collect();
    let mut referenced = self.gpu_used_globals();
    for (f_name, implementation) in functions.ordered.iter() {
      if !functions.emitted.contains(f_name) {
        continue;
      }
      implementation
        .read()
        .unwrap()
        .expression
        .walk(&mut |exp| {
          if let ExpKind::Name(name) = &exp.kind
            && var_names.contains(name)
          {
            referenced.insert(name.clone());
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
    referenced
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
  /// Atomics shared between threads live in words every thread operates on
  /// directly, which only works for an atomic or a fixed-size array of
  /// them, and only on the CPU: the GPU's copy is a separate buffer.
  pub fn validate_thread_shared_atomics(&self, errors: &mut ErrorLog) {
    if !self
      .top_level_vars
      .iter()
      .any(|v| v.var_type.involves_atomic())
    {
      return;
    }
    let gpu_used = self.gpu_used_globals();
    for (name, _) in self.thread_shared_globals() {
      let v = self
        .top_level_vars
        .iter()
        .find(|v| v.name == name)
        .expect("thread-shared global missing from top-level vars");
      if !v.var_type.involves_atomic() {
        continue;
      }
      let display_name = compile_word(name.clone());
      let supported_shape = match &v.var_type {
        Type::Struct(s) => &*s.name == "Atomic",
        Type::Array(Some(ConcreteArraySize::Literal(_)), inner) => {
          matches!(inner.unwrap_known(), Type::Struct(s) if &*s.name == "Atomic")
        }
        _ => false,
      };
      if !supported_shape {
        errors.log(CompileError::new(
          UnsupportedThreadSharedAtomicType(display_name),
          v.source_trace.clone(),
        ));
      } else if gpu_used.contains(&name) {
        errors.log(CompileError::new(
          GpuUsedThreadSharedAtomic(display_name),
          v.source_trace.clone(),
        ));
      }
    }
  }
  pub fn thread_shared_globals(&self) -> Vec<(Arc<str>, u32)> {
    use crate::compiler::expression::ExpKind;
    let var_names: HashSet<Arc<str>> =
      self.top_level_vars.iter().map(|v| v.name.clone()).collect();
    let mut memo = EffectsMemo::default();
    let mut globals_reachable_from = |root_entry: fn(&EntryPoint) -> bool| {
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
        let effects = f.effects_with(&mut memo);
        let (reads, writes) = effects.read_and_written_globals();
        // Length-only reads count as touches here, unlike in the GPU
        // readback set they're excluded from: the GPU can never resize a
        // buffer, but another *thread* can resize a shared array, so a
        // length read is a genuine cross-thread read.
        let length_reads: Vec<Arc<str>> = effects
          .0
          .iter()
          .filter_map(|e| {
            if let Effect::ReadsArrayLength(name) = e {
              Some(name.clone())
            } else {
              None
            }
          })
          .collect();
        // Seed writes (`Effect::SeedsGlobalVar`) count as touches here —
        // this is how the analysis sees the main thread writing the lifted
        // audio-closure capture globals inside `start-audio` — but stay
        // invisible to the runtime dirty-marking machinery, which the
        // builtin drives itself on the call that actually seeds.
        globals.extend(
          reads
            .into_iter()
            .chain(writes.into_iter())
            .chain(length_reads.into_iter())
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
        // The implicit MIDI vars are ambient per-replica state, not
        // program state: every thread refreshes its own copy from the
        // process-global listener (main per frame, audio per callback
        // batch), so carrying them through the sharing system would only
        // overwrite the audio replica's fresher per-batch values with
        // main's older per-frame ones. They're storage-space (for GPU
        // reads via the upload machinery) but excluded here like
        // `@local` vars; the names are compiler-reserved, so user vars
        // can never collide with this exclusion.
        if name.starts_with("easl_midi_") {
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
      ValueLayout,
    };
    let mut state = BytecodeCompilationState::new();
    state.cpu_mode = cpu_mode;
    state.lifted_audio_captures = self.lifted_audio_captures.clone();
    state.lifted_gpu_captures = self.lifted_gpu_captures.clone();
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
    let ordered_functions =
      self.composite_functions_in_usage_order_with_discovery(cpu_mode);
    // Both modes compile only the functions reachable from their own entry
    // points (`@cpu` for the CPU runtime, `@audio` for the audio runtime),
    // mirroring the closure-based emission the WGSL/C backends use: an
    // unreachable helper (say, a never-called higher-order function returning
    // a closure that isn't in the compiled set) must not be compiled, and
    // neither may anything reached solely from a wrong-target entry (e.g. a
    // GPU callback while compiling for the CPU).
    //
    // Audio mode is also used by the VM test harnesses (the conformance and
    // vm suites) to compile a bare `f` in a program with no `@audio` entry at
    // all; with no entry to seed reachability from, the whole program is
    // compiled.
    let reachable: Option<HashSet<Arc<str>>> = {
      let is_target_entry = |e: EntryPoint| {
        if cpu_mode {
          matches!(e, EntryPoint::Cpu)
        } else {
          matches!(e, EntryPoint::Audio)
        }
      };
      let by_name: HashMap<Arc<str>, Arc<RwLock<TopLevelFunction>>> =
        ordered_functions
          .iter()
          .map(|(n, f)| (n.clone(), f.clone()))
          .collect();
      let is_target_compilable = |f: &Arc<RwLock<TopLevelFunction>>| {
        f.read()
          .unwrap()
          .entry_point
          .map(|e| is_target_entry(e))
          .unwrap_or(true)
      };
      let mut reachable: HashSet<Arc<str>> = HashSet::new();
      let mut queue: Vec<Arc<RwLock<TopLevelFunction>>> = vec![];
      for (name, f) in &ordered_functions {
        let is_entry = f
          .read()
          .unwrap()
          .entry_point
          .map(|e| is_target_entry(e))
          .unwrap_or(false);
        if is_entry && reachable.insert(name.clone()) {
          queue.push(f.clone());
        }
      }
      if !cpu_mode && reachable.is_empty() {
        None
      } else {
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
              && is_target_compilable(target)
              && reachable.insert(name)
            {
              queue.push(target.clone());
            }
          }
        }
        Some(reachable)
      }
    };
    // Which globals the compiled code can touch. A var the user declared
    // that no compiled function (nor the initializer of a var that is
    // touched) refers to — say, a large buffer only shaders use — gets no
    // VM slots: the slot space is finite, and nothing on this side would
    // read or write them. Compiler-generated vars are always touched (the
    // runtime writes window-info, MIDI, audio-info, and capture vars into
    // their slots), as are shared and `@external` vars.
    let touched_globals: HashSet<Arc<str>> = {
      let var_names: HashSet<Arc<str>> =
        self.top_level_vars.iter().map(|v| v.name.clone()).collect();
      let mut touched: HashSet<Arc<str>> = self
        .top_level_vars
        .iter()
        .filter(|v| {
          !v.directly_user_written
            || v.external
            || state.shared_var_indices.contains_key(&v.name)
        })
        .map(|v| v.name.clone())
        .collect();
      let collect_names = |exp: &TypedExp, touched: &mut HashSet<Arc<str>>| {
        exp
          .walk(&mut |exp| {
            if let ExpKind::Name(name) = &exp.kind
              && var_names.contains(name)
            {
              touched.insert(name.clone());
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      };
      for (f_name, f) in &ordered_functions {
        if reachable
          .as_ref()
          .is_none_or(|reachable| reachable.contains(f_name))
        {
          collect_names(&f.read().unwrap().expression, &mut touched);
        }
      }
      // An initializer runs when its var has slots, reading what it names.
      loop {
        let before = touched.len();
        for v in self.top_level_vars.iter() {
          if touched.contains(&v.name)
            && let Some(value) = &v.value
          {
            collect_names(value, &mut touched);
          }
        }
        if touched.len() == before {
          break;
        }
      }
      touched
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
        // `vm_stack_size`, not `flat_data_size_in_u32s`: heap-backed element
        // types (nested arrays, strings) size as one id word — their
        // regions use `DynMemory::Cells` storage, where the stride is
        // never consulted.
        let element_type = element_type.unwrap_known();
        let element_stride = vm_stack_size(&element_type);
        let memory = dyn_memory_count;
        dyn_memory_count += 1;
        if is_heap_value_type(&element_type) {
          state.cell_regions.push(memory);
        }
        state
          .dynamic_array_memory
          .insert(v.name.clone(), (memory, element_stride));
        if let Some(shared_index) =
          state.shared_var_indices.get(&v.name).copied()
        {
          state.shared_vars[shared_index as usize] = Some(SharedVarInfo {
            name: v.name.clone(),
            layout: ValueLayout::of(&v.var_type),
            audience: shared_var_audiences[&v.name],
            atomic: false,
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
      if !touched_globals.contains(&v.name) {
        // No compiled code touches it: no slots. A GPU binding still gets
        // a host binding (with its value host-side, like a texture's) so
        // dispatches can account for it.
        if let Some((gb, address_space)) = binding_info {
          let index = state.host_bindings.len() as u16;
          state.host_bindings.push(HostBinding {
            name: v.name.clone(),
            ty: v.var_type.clone(),
            storage: HostBindingStorage::Dynamic,
            gpu: Some((gb.group, gb.binding, address_space)),
          });
          state.binding_indices.insert(v.name.clone(), index);
        }
        continue;
      }
      let position = state.consumed_stack_space as u16;
      // `vm_stack_size`, not `flat_data_size_in_u32s`: a slot-backed
      // global may embed a heap id (a struct/enum with a runtime-sized
      // field lifted from an audio capture), for which the flat/GPU size
      // errors — the two agree for every heap-free type (GPU bindings
      // included, which are always heap-free).
      let size = vm_stack_size(&v.var_type);
      state.globals.insert(v.name.clone(), position);
      state.global_slots.push((v.name.clone(), position, size));
      if let Some(shared_index) = state.shared_var_indices.get(&v.name).copied()
      {
        state.shared_vars[shared_index as usize] = Some(SharedVarInfo {
          name: v.name.clone(),
          layout: ValueLayout::of(&v.var_type),
          audience: shared_var_audiences[&v.name],
          storage: SharedVarStorage::Slots { position, size },
          atomic: v.var_type.involves_atomic(),
        });
      }
      state.consumed_stack_space = state
        .consumed_stack_space
        .checked_add(size)
        .unwrap_or_else(|| {
          panic!(
            "global `{}` ({size} slots) doesn't fit in the VM's 65536 slots",
            v.name
          )
        });
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
    state.shared_atomics = state
      .shared_vars
      .iter()
      .flatten()
      .filter(|info| info.atomic)
      .map(|info| info.name.clone())
      .collect();
    // If any top-level var has an initializer expression, compile a
    // synthetic "$init_globals" function that computes each one and Moves it
    // into the corresponding global slot. `BytecodeProgram::from_code` runs
    // it once at construction so globals are live before any user code.
    let init_function_index =
      if self.top_level_vars.iter().any(|v| v.value.is_some()) {
        state.open_function("$init_globals".into(), 0);
        for v in self.top_level_vars.iter() {
          if let Some(value_exp) = &v.value
            && touched_globals.contains(&v.name)
          {
            let value_slot = value_exp
              .compile_to_bytecode(CompilePosition::Value, &mut state)
              .unwrap();
            if let Some((memory, _)) =
              state.dynamic_array_memory.get(&v.name).copied()
            {
              // A runtime-sized global lives in its own region.
              let Type::Array(_, element_type) = &v.var_type else {
                unreachable!("a region-backed global is a runtime-sized array")
              };
              state.emit_region_assignment(
                memory,
                value_slot,
                &element_type.unwrap_known(),
              );
            } else {
              // Slot-sized like the global itself, and copied so the global
              // owns any heap values (Strings, struct fields) it holds.
              let var_size = vm_stack_size(&v.var_type) as u16;
              let global_slot = *state.globals.get(&v.name).unwrap();
              state.emit_value_copy(
                value_slot,
                var_size,
                global_slot,
                &v.var_type,
              );
            }
          }
        }
        state.close_function();
        Some(state.finished_functions.len() - 1)
      } else {
        None
      };
    for (f_name, implementation) in ordered_functions {
      if let Some(reachable) = &reachable
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
          let gpu_only = implementation_read
            .effects_with(&mut state.effects_memo)
            .0
            .iter()
            .any(|e| {
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
          let effects =
            implementation_read.effects_with(&mut state.effects_memo);
          let has_cpu_exclusive = effects
            .cpu_exclusive_functions()
            .iter()
            .any(|name| {
              !EffectType::cpu_exclusive_function_allowed_in_audio(name)
            })
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
          for (owned_arg_index, copy_group) in arg_move_positions {
            // The group's sources, sizes, and jump targets were filled at
            // emission; only the destination-relative operands it recorded
            // depend on the specialized function's arg slots.
            let dest = bytecode_fn.arg_positions[owned_arg_index];
            copy_group.apply(&mut state.instructions, dest);
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

/// One accessor step in a reference argument's lvalue path, as compared
/// by `Program::validate_ref_arg_aliasing`. `Opaque` covers anything the
/// check can't statically distinguish (runtime indices, swizzles) and is
/// never provably disjoint from any other step.
pub(crate) enum RefArgPathStep {
  Field(Arc<str>),
  LiteralIndex(i64),
  Opaque,
}

/// Peels an lvalue expression to its root variable and accessor path
/// (root-outward). Returns None for non-lvalues, which can never alias —
/// a reference to a temporary is the only holder of its storage.
pub(crate) fn ref_arg_lvalue_path(
  exp: &TypedExp,
) -> Option<(Arc<str>, Vec<RefArgPathStep>)> {
  match &exp.kind {
    ExpKind::Name(name) => Some((name.clone(), vec![])),
    ExpKind::Access(accessor, inner) => {
      let (root, mut path) = ref_arg_lvalue_path(inner)?;
      path.push(match accessor {
        Accessor::Field(field) => RefArgPathStep::Field(field.clone()),
        Accessor::ArrayIndex(index) => match &index.kind {
          ExpKind::NumberLiteral(Number::Int(i)) => {
            RefArgPathStep::LiteralIndex(*i)
          }
          _ => RefArgPathStep::Opaque,
        },
        Accessor::Swizzle(_) => RefArgPathStep::Opaque,
      });
      Some((root, path))
    }
    _ => None,
  }
}

/// Whether two same-root accessor paths provably address disjoint
/// storage: they must diverge at some depth on two statically-comparable
/// steps (different fields, different literal indices). Equal prefixes
/// that run out (one path extends the other) overlap, and `Opaque` steps
/// can never establish divergence.
pub(crate) fn ref_paths_provably_disjoint(
  a: &[RefArgPathStep],
  b: &[RefArgPathStep],
) -> bool {
  for (step_a, step_b) in a.iter().zip(b.iter()) {
    match (step_a, step_b) {
      (RefArgPathStep::Field(f_a), RefArgPathStep::Field(f_b)) => {
        if f_a != f_b {
          return true;
        }
      }
      (
        RefArgPathStep::LiteralIndex(i_a),
        RefArgPathStep::LiteralIndex(i_b),
      ) => {
        if i_a != i_b {
          return true;
        }
      }
      _ => return false,
    }
  }
  false
}

/// The walk behind `Program::validate_ref_captures`. `taint_of` returns
/// the name of the reference param responsible when the expression's
/// value may be (or contain) a closure capturing one, logging
/// `ClosureCapturesMutableRefArg` for mutable-ref captures and
/// `EscapingClosureCapturesRefArg` for taints reaching `return`
/// statements or host-sink calls as it goes; the caller checks the
/// body's own value position.
struct RefCaptureAnalyzer<'a, 'e> {
  ref_params: &'a HashMap<Arc<str>, Ownership>,
  /// Names let-bound or assigned to tainted values, mapped to the
  /// responsible param.
  tainted: HashMap<Arc<str>, Arc<str>>,
  errors: &'e mut ErrorLog,
}

impl RefCaptureAnalyzer<'_, '_> {
  fn taint_of(&mut self, exp: &TypedExp) -> Option<Arc<str>> {
    match &exp.kind {
      ExpKind::Name(name) => self.tainted.get(name).cloned(),
      ExpKind::Function(_, lambda_body) => self.scan_lambda(lambda_body),
      ExpKind::Let(bindings, body) => {
        for (name, _, _, value) in bindings.iter() {
          if let Some(param) = self.taint_of(value) {
            self.tainted.insert(name.clone(), param);
          }
        }
        self.taint_of(body)
      }
      ExpKind::Application(callee, args) => {
        if let ExpKind::Name(callee_name) = &callee.kind {
          match &**callee_name {
            "start-audio" | "spawn-window" => {
              for arg in args.iter() {
                if let Some(param) = self.taint_of(arg) {
                  self.errors.log(CompileError::new(
                    EscapingClosureCapturesRefArg(param.to_string()),
                    arg.source_trace.clone(),
                  ));
                }
              }
              return None;
            }
            "=" => {
              let taint = self.taint_of(&args[1]);
              if let Some(param) = taint {
                if args[0].data.is_globally_bound {
                  // Stored in a global, the closure outlives the call.
                  self.errors.log(CompileError::new(
                    EscapingClosureCapturesRefArg(param.to_string()),
                    exp.source_trace.clone(),
                  ));
                } else if let Some(target) =
                  args[0].name_or_inner_accessed_name()
                {
                  // Storing into a local — or an element / field of one —
                  // makes the whole local carry the closure.
                  self.tainted.insert(target.clone(), param);
                }
              }
              return None;
            }
            _ => {}
          }
        }
        let mut taint = self.taint_of(callee);
        for arg in args.iter() {
          let arg_taint = self.taint_of(arg);
          taint = taint.or(arg_taint);
        }
        // A call result can only carry a closure onward if its type can
        // hold one; calling a tainted closure for its (non-function)
        // value doesn't propagate.
        taint.filter(|_| {
          exp.data.with_dereferenced(|state| match state {
            TypeState::Known(t) => holds_function(t),
            _ => false,
          })
        })
      }
      ExpKind::Access(_, inner) => self.taint_of(inner),
      ExpKind::Match(scrutinee, arms) => {
        self.taint_of(scrutinee);
        let mut taint = None;
        for (_, arm_body) in arms.iter() {
          let arm_taint = self.taint_of(arm_body);
          taint = taint.or(arm_taint);
        }
        taint
      }
      ExpKind::Block(expressions) => {
        let mut taint = None;
        for expression in expressions.iter() {
          taint = self.taint_of(expression);
        }
        taint
      }
      ExpKind::Return(inner) => {
        if let Some(param) = self.taint_of(inner) {
          self.errors.log(CompileError::new(
            EscapingClosureCapturesRefArg(param.to_string()),
            exp.source_trace.clone(),
          ));
        }
        None
      }
      ExpKind::ForLoop {
        increment_variable_initial_value_expression,
        continue_condition_expression,
        update_expression,
        body_expression,
        ..
      } => {
        self.taint_of(increment_variable_initial_value_expression);
        self.taint_of(continue_condition_expression);
        if let Some(update) = update_expression {
          self.taint_of(update);
        }
        self.taint_of(body_expression);
        None
      }
      ExpKind::WhileLoop {
        condition_expression,
        body_expression,
      } => {
        self.taint_of(condition_expression);
        self.taint_of(body_expression);
        None
      }
      ExpKind::ArrayLiteral(elements) => {
        let mut taint = None;
        for element in elements.iter() {
          let element_taint = self.taint_of(element);
          taint = taint.or(element_taint);
        }
        taint
      }
      _ => None,
    }
  }
  /// A lambda captures whatever free names its body (including nested
  /// lambdas) references. Mutable-ref captures error immediately; any
  /// ref-param or tainted-name capture taints the lambda value.
  fn scan_lambda(&mut self, lambda_body: &TypedExp) -> Option<Arc<str>> {
    let mut taint = None;
    let mut reported: HashSet<Arc<str>> = HashSet::new();
    lambda_body
      .walk(&mut |exp| {
        if let ExpKind::Name(name) = &exp.kind {
          if let Some(ownership) = self.ref_params.get(name) {
            if *ownership == Ownership::MutableReference
              && reported.insert(name.clone())
            {
              self.errors.log(CompileError::new(
                ClosureCapturesMutableRefArg(name.to_string()),
                exp.source_trace.clone(),
              ));
            }
            taint.get_or_insert_with(|| name.clone());
          } else if let Some(param) = self.tainted.get(name) {
            let param = param.clone();
            taint.get_or_insert(param);
          }
        }
        Ok::<bool, Never>(true)
      })
      .unwrap();
    taint
  }
}

/// Whether an alias target candidate's abstract argument shape could
/// have produced a resolved concrete type — the discriminator
/// `rewrite_aliased_builtin_calls` uses to pick among same-name,
/// same-arity target overloads (e.g. `array-length`'s sized vs unsized
/// signatures, whose ancestors differ in target emulations). Shallow by
/// design: generics match anything, arrays match on sized/unsized-ness
/// (a generic or literal abstract size can't have produced an unsized
/// concrete array, and vice versa), exact types match exactly, and
/// anything else is permissive — inference already proved the call
/// resolves; this only distinguishes shapes.
fn abstract_type_shallow_matches(
  abstract_type: &AbstractType,
  concrete: &Type,
) -> bool {
  match (abstract_type, concrete) {
    (AbstractType::Generic(_), _) => true,
    (AbstractType::Type(t), concrete) => t == concrete,
    (
      AbstractType::AbstractArray { size, .. },
      Type::Array(concrete_size, _),
    ) => match size {
      AbstractArraySize::Unsized => {
        matches!(concrete_size, Some(ConcreteArraySize::Unsized))
      }
      _ => !matches!(concrete_size, Some(ConcreteArraySize::Unsized)),
    },
    (AbstractType::AbstractArray { .. }, _) => false,
    _ => true,
  }
}

/// The expression whose value a function body evaluates to — descends
/// let bodies and block tails — used to position the implicit-return
/// escape error.
fn tail_value_expression(exp: &TypedExp) -> &TypedExp {
  match &exp.kind {
    ExpKind::Let(_, body) => tail_value_expression(body),
    ExpKind::Block(expressions) => {
      expressions.last().map(tail_value_expression).unwrap_or(exp)
    }
    _ => exp,
  }
}

#[cfg(test)]
mod tests {
  use fsexp::Ast;

  use super::EaslDocumentMethods;
  use crate::parse::parse_easl;

  #[test]
  fn override_def_replaces_the_value() {
    let mut document = parse_easl("(def gain: f32 0.5)\n(def other: f32 1.)");
    assert!(document.override_def("gain", "0.25"));
    assert!(!document.override_def("missing", "0.25"));
    let Ast::Inner(_, children) = &document.syntax_trees[0] else {
      panic!("expected the def form")
    };
    let Ast::Leaf(_, value) = &children[2] else {
      panic!("expected a leaf value")
    };
    assert_eq!(value, "0.25");
  }
}
