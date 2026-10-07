use std::sync::Arc;

use fsexp::document::DocumentPosition;

use crate::compiler::{
  entry::EntryPoint,
  error::SourceTrace,
  expression::{ExpKind, Number, TypedExp},
  functions::{FunctionImplementationKind, Ownership},
  modules::display_name,
  program::Program,
  types::{
    AbstractType, ConcreteArraySize, GenericArgument, Type,
    TypeConstraintDescription, TypeDescription,
  },
  vars::{
    BindingSpec, GroupAndBinding, TopLevelVariableKind, VariableAddressSpace,
  },
};
use crate::parse::EaslMultiDocument;

pub enum TypeInfo {
  Unit,
  F32,
  I32,
  U32,
  Bool,
  Struct(String),
  Enum(String),
  Array(Option<ConcreteArraySize>, Box<Self>),
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
            // Info extraction can run on unvalidated programs, where
            // elided bindings don't have numbers yet.
            group_and_binding.and_then(|spec| match spec {
              BindingSpec::Specified(group_and_binding) => {
                Some(group_and_binding)
              }
              BindingSpec::Elided => None,
            })
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
      fragment_entries: program
        .find_fn_names_by_entry_point(&|e| e == EntryPoint::Fragment),
      vertex_entries: program
        .find_fn_names_by_entry_point(&|e| e == EntryPoint::Vertex),
      compute_entries: program
        .find_fn_names_by_entry_point(&|e| matches!(e, EntryPoint::Compute(_))),
    }
  }
}

/// Each definition written in `documents` (functions, structs, enums and
/// their variants, top-level variables), as it would be declared in easl,
/// keyed by the position of its name (for a variant, of the variant). Run on
/// a program before validation, which rewrites its definitions.
pub fn definition_signatures(
  program: &Program,
  documents: &EaslMultiDocument,
) -> Vec<(DocumentPosition, String)> {
  let source_text = |position: &DocumentPosition| -> Option<String> {
    let (_, _, text) = documents.sources.get(*position.path.first()?)?;
    text.get(position.span.clone()).map(|text| text.to_string())
  };
  let mut signatures = vec![];
  for signature in program.abstract_functions_iter() {
    let signature = signature.read().unwrap();
    let FunctionImplementationKind::Composite(implementation) =
      &signature.implementation
    else {
      continue;
    };
    let implementation = implementation.read().unwrap();
    let Some(position) = &implementation.name_source_trace.primary_position
    else {
      continue;
    };
    let Some(name) = source_text(position) else {
      continue;
    };
    let head = if signature.generic_args.is_empty() {
      name
    } else {
      format!(
        "({name} {})",
        describe_generic_args(&signature.generic_args)
      )
    };
    let args: Vec<String> = signature
      .arg_types
      .iter()
      .zip(implementation.arg_names.iter())
      .zip(implementation.arg_annotations.iter())
      .map(|(((arg_type, ownership), (arg_name, _)), annotation)| {
        let prefix = match ownership {
          Ownership::MutableReference => "@var @ref ",
          Ownership::Reference => "@ref ",
          _ if annotation.var => "@var ",
          _ => "",
        };
        format!("{prefix}{arg_name}: {}", arg_type.describe())
      })
      .collect();
    let return_type = match &signature.return_type {
      AbstractType::Unit => String::new(),
      AbstractType::Type(Type::Unit) => String::new(),
      return_type => format!(": {}", return_type.describe()),
    };
    signatures.push((
      position.clone(),
      format!("(defn {head} [{}]{return_type})", args.join(" ")),
    ));
  }
  for s in program.typedefs.structs.iter() {
    let Some(position) = &s.name.1.primary_position else {
      continue;
    };
    let Some(name) = source_text(position) else {
      continue;
    };
    let fields: String = s
      .fields
      .iter()
      .map(|field| {
        format!("\n  {}: {}", field.name, field.field_type.describe())
      })
      .collect();
    signatures.push((
      position.clone(),
      format!("(struct {}{fields})", type_head(&name, &s.generic_args)),
    ));
  }
  for e in program.typedefs.enums.iter() {
    let Some(position) = &e.name.1.primary_position else {
      continue;
    };
    let Some(name) = source_text(position) else {
      continue;
    };
    let variants: String = e
      .variants
      .iter()
      .map(|variant| {
        let variant_name = display_name(&variant.name);
        match &variant.inner_type {
          AbstractType::Unit | AbstractType::Type(Type::Unit) => {
            format!("\n  {variant_name}")
          }
          inner_type => {
            format!("\n  ({variant_name} {})", inner_type.describe())
          }
        }
      })
      .collect();
    let declaration =
      format!("(enum {}{variants})", type_head(&name, &e.generic_args));
    signatures.push((position.clone(), declaration.clone()));
    for variant in e.variants.iter() {
      if let Some(position) = &variant.source.primary_position {
        signatures.push((position.clone(), declaration.clone()));
      }
    }
  }
  for var in program.top_level_vars.iter() {
    let Some(position) = &var.source_trace.primary_position else {
      continue;
    };
    let keyword = match var.kind {
      TopLevelVariableKind::Const => "def",
      TopLevelVariableKind::Override => "override",
      TopLevelVariableKind::Var { .. } => "var",
    };
    let name = display_name(&var.name);
    let var_type = TypeDescription::from(var.var_type.clone());
    signatures
      .push((position.clone(), format!("({keyword} {name}: {var_type})")));
  }
  signatures
}

/// A type definition's name, applied to its generic parameters if it has
/// any: `(Pair T U)`.
fn type_head(
  name: &str,
  generic_args: &[(Arc<str>, GenericArgument, SourceTrace)],
) -> String {
  if generic_args.is_empty() {
    name.to_string()
  } else {
    format!("({name} {})", describe_generic_args(generic_args))
  }
}

/// Generic parameters as declared: `T`, `N: u32`, `T: [Scalar]`.
fn describe_generic_args(
  generic_args: &[(Arc<str>, GenericArgument, SourceTrace)],
) -> String {
  generic_args
    .iter()
    .map(|(name, argument, _)| match argument {
      GenericArgument::Constant => format!("{name}: u32"),
      GenericArgument::Type(constraints) if constraints.is_empty() => {
        name.to_string()
      }
      GenericArgument::Type(constraints) => format!(
        "{name}: [{}]",
        constraints
          .iter()
          .map(
            |constraint| TypeConstraintDescription::from(constraint.clone())
              .name
          )
          .collect::<Vec<_>>()
          .join(" ")
      ),
    })
    .collect::<Vec<_>>()
    .join(" ")
}

/// Each name written in a function body or a top-level variable's value
/// that refers to a local binding (a parameter, `let` binding, lambda
/// parameter, `match` pattern binding, or `for` variable), with the position
/// of that binding. Run on a program before validation, which renames
/// bindings.
pub fn local_references(
  program: &Program,
) -> Vec<(DocumentPosition, DocumentPosition)> {
  let mut references = vec![];
  for signature in program.abstract_functions_iter() {
    let signature = signature.read().unwrap();
    let FunctionImplementationKind::Composite(implementation) =
      &signature.implementation
    else {
      continue;
    };
    let implementation = implementation.read().unwrap();
    let mut scope = vec![];
    // The body is a lambda binding the parameters; if it isn't, they're
    // bound around it.
    if !matches!(implementation.expression.kind, ExpKind::Function(_, _)) {
      bind_all(&implementation.arg_names, &mut scope);
    }
    collect_local_references(
      &implementation.expression,
      &mut scope,
      &mut references,
    );
  }
  for var in program.top_level_vars.iter() {
    if let Some(value) = &var.value {
      collect_local_references(value, &mut vec![], &mut references);
    }
  }
  references
}

type LocalScope = Vec<(Arc<str>, DocumentPosition)>;

fn bind_all<'a>(
  bindings: impl IntoIterator<Item = &'a (Arc<str>, SourceTrace)>,
  scope: &mut LocalScope,
) {
  for (name, source) in bindings {
    if let Some(position) = &source.primary_position {
      scope.push((name.clone(), position.clone()));
    }
  }
}

fn collect_local_references(
  exp: &TypedExp,
  scope: &mut LocalScope,
  references: &mut Vec<(DocumentPosition, DocumentPosition)>,
) {
  let outer_scope = scope.len();
  match &exp.kind {
    ExpKind::Name(name) => {
      if let Some((_, binding)) =
        scope.iter().rev().find(|(bound, _)| bound == name)
        && let Some(position) = &exp.source_trace.primary_position
      {
        references.push((position.clone(), binding.clone()));
      }
    }
    ExpKind::Function(args, body) => {
      bind_all(args, scope);
      collect_local_references(body, scope, references);
    }
    ExpKind::Let(bindings, body) => {
      // Each binding's value sees the bindings before it.
      for (name, source, _, value) in bindings {
        collect_local_references(value, scope, references);
        bind_all([&(name.clone(), source.clone())], scope);
      }
      collect_local_references(body, scope, references);
    }
    ExpKind::Match(scrutinee, arms) => {
      collect_local_references(scrutinee, scope, references);
      for (pattern, body) in arms {
        // A pattern's arguments are the arm's bindings.
        if let ExpKind::Application(_, args) = &pattern.kind {
          for arg in args {
            if let ExpKind::Name(name) = &arg.kind {
              bind_all([&(name.clone(), arg.source_trace.clone())], scope);
            }
          }
        }
        collect_local_references(body, scope, references);
        scope.truncate(outer_scope);
      }
    }
    ExpKind::ForLoop {
      increment_variable_name,
      increment_variable_initial_value_expression,
      continue_condition_expression,
      update_expression,
      body_expression,
      ..
    } => {
      collect_local_references(
        increment_variable_initial_value_expression,
        scope,
        references,
      );
      bind_all([increment_variable_name], scope);
      collect_local_references(
        continue_condition_expression,
        scope,
        references,
      );
      if let Some(update) = update_expression {
        collect_local_references(update, scope, references);
      }
      collect_local_references(body_expression, scope, references);
    }
    ExpKind::WhileLoop {
      condition_expression,
      body_expression,
    } => {
      collect_local_references(condition_expression, scope, references);
      collect_local_references(body_expression, scope, references);
    }
    ExpKind::Application(f, args) => {
      collect_local_references(f, scope, references);
      for arg in args {
        collect_local_references(arg, scope, references);
      }
    }
    ExpKind::Block(exps) | ExpKind::ArrayLiteral(exps) => {
      for exp in exps {
        collect_local_references(exp, scope, references);
      }
    }
    ExpKind::Access(_, exp) | ExpKind::Return(exp) => {
      collect_local_references(exp, scope, references);
    }
    ExpKind::Wildcard
    | ExpKind::Unit
    | ExpKind::NumberLiteral(_)
    | ExpKind::BooleanLiteral(_)
    | ExpKind::StringLiteral(_)
    | ExpKind::Break
    | ExpKind::Continue
    | ExpKind::Discard
    | ExpKind::Uninitialized => {}
  }
  scope.truncate(outer_scope);
}
