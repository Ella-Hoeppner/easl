//! Module resolution: the front-end pass that turns a program's files and
//! `mod` forms into one flat namespace for the rest of the compiler.
//!
//! Every top-level definition gets a program-wide *internal name*: its
//! module's prefix plus its own name (`lib/sort`, `lib/shapes/Circle`).
//! The main file's prefix is empty, so a single-file program's names are
//! unchanged. Enum variants are qualified by their enum (`Shape/Circle`)
//! unless the enum is `@unpack`, which makes them module-level names. Every
//! name written in a definition is then rewritten to the internal name of
//! whatever that name refers to in its scope, after which `import`, `use`,
//! and `mod` forms are gone and the definitions are flattened into one list.
//!
//! What a scope sees:
//! - its module's own definitions;
//! - for an inline `mod`, everything its enclosing scope sees (a file
//!   module never sees its importer);
//! - names pulled in by `import` and `use`, which behave exactly as if they
//!   were defined there.
//!
//! When several functions with the same name meet in a scope (across
//! modules, or a definition sharing a builtin's name), references to that
//! name resolve to an *overload group*: a synthetic name whose candidates
//! are the union of its members' signatures. Inference picks a member, and
//! `Program::resolve_overload_groups` rewrites the reference to that
//! member's own name. A definition's own name never refers to a group, and
//! a main-file function sharing a builtin's name is qualified with a
//! `root`-style prefix so it joins only the overload sets of scopes that
//! can see it.

use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::sync::Arc;

use fsexp::{document::DocumentPosition, syntax::EncloserOrOperator};

use crate::{
  compiler::error::{CompileError, CompileErrorKind, ErrorLog, SourceTrace},
  parse::{EaslMultiDocument, EaslTree, Encloser, Operator},
};

/// The resolved program: every definition from every module, with names
/// rewritten to internal names.
pub struct ResolvedModules {
  pub trees: Vec<EaslTree>,
  /// Each overload group's name and the names of the function registry
  /// buckets it draws candidates from.
  pub overload_groups: HashMap<Arc<str>, Vec<Arc<str>>>,
  pub main_file_index: MainFileIndex,
}

/// What the main file can refer to, and what its names refer to — for
/// editor tooling.
#[derive(Clone, Debug, Default)]
pub struct MainFileIndex {
  /// Every name the main file can write that refers to a definition: the
  /// bare names in its scope, and paths through the modules and enums it
  /// can see (`geo/Point`, `geo/Shape/Circle`), with what each names.
  /// Sorted by name.
  pub names: Vec<(Arc<str>, NameKind)>,
  /// Each name written in the main file that refers to top-level
  /// definitions, with the positions of those definitions' names (several
  /// for an overloaded function; builtins have none).
  pub references: Vec<(DocumentPosition, Vec<DocumentPosition>)>,
  /// The position of the name of every definition written in the main file.
  pub definitions: Vec<DocumentPosition>,
  /// How the main file writes the definitions it can refer to whose
  /// internal names differ (an imported `geometry/Point` imported as `geo`
  /// is `geo/Point`): the shortest name it can write for each, by internal
  /// name.
  pub written_names: WrittenNames,
}

pub type WrittenNames = HashMap<Arc<str>, Arc<str>>;

/// What kind of definition a name refers to.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NameKind {
  Function,
  /// A top-level `var`, `def`, or `override`.
  Variable,
  Struct,
  Enum,
  Variant,
  Module,
}

/// How a name written in source should be displayed to a user: an internal
/// name's last segment, without any overload-group marker.
pub fn display_name(name: &str) -> &str {
  let name = name.split_once('@').map_or(name, |(base, _)| base);
  match name.rsplit_once('/') {
    Some((_, last)) if !last.is_empty() => last,
    _ => name,
  }
}

/// Whether `name` is a path like `lib/sort` rather than an operator like
/// `/` or `/=`.
fn is_path(name: &str) -> bool {
  name.contains('/') && name.split('/').all(|segment| !segment.is_empty())
}

type ModuleId = usize;
type EnumId = usize;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum OriginKind {
  Function,
  /// A top-level `var`, `def`, or `override`.
  Value,
  Struct,
  Enum(EnumId),
  Variant,
  Module(ModuleId),
}

/// A definition a name can refer to.
#[derive(Clone, Debug)]
struct Origin {
  internal: Arc<str>,
  kind: OriginKind,
  /// Where the definition's name is written.
  source: SourceTrace,
}

impl Origin {
  fn name_kind(&self) -> NameKind {
    match self.kind {
      OriginKind::Function => NameKind::Function,
      OriginKind::Value => NameKind::Variable,
      OriginKind::Struct => NameKind::Struct,
      OriginKind::Enum(_) => NameKind::Enum,
      OriginKind::Variant => NameKind::Variant,
      OriginKind::Module(_) => NameKind::Module,
    }
  }
  fn is_function(&self) -> bool {
    self.kind == OriginKind::Function
  }
  fn namespace(&self) -> Option<Namespace> {
    match self.kind {
      OriginKind::Module(id) => Some(Namespace::Module(id)),
      OriginKind::Enum(id) => Some(Namespace::Enum(id)),
      _ => None,
    }
  }
}

#[derive(Clone, Copy)]
enum Namespace {
  Module(ModuleId),
  Enum(EnumId),
}

#[derive(Clone)]
struct Member {
  origin: Origin,
  private: bool,
}

struct EnumNamespace {
  variants: Vec<(Arc<str>, Origin)>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum DefinitionKind {
  Function,
  Struct,
  Enum(EnumId),
  Variable,
}

enum Form {
  Definition {
    annotations: Vec<(DocumentPosition, EaslTree)>,
    body: EaslTree,
    kind: DefinitionKind,
    internal: Arc<str>,
  },
  Mod(ModuleId),
  Import {
    alias: Option<(Arc<str>, SourceTrace)>,
    target: ModuleId,
    source: SourceTrace,
  },
  Use(EaslTree),
  Other(EaslTree),
}

struct Module {
  /// Prepended to the names of this module's definitions.
  prefix: String,
  /// The enclosing module, for an inline `mod`.
  parent: Option<ModuleId>,
  /// The index of the document this module is written in.
  file: usize,
  /// Each name's definitions: one, or several overloads of a function.
  members: HashMap<Arc<str>, Vec<Member>>,
  forms: Vec<Form>,
}

type Scope = HashMap<Arc<str>, Vec<Origin>>;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Position {
  Value,
  Type,
}

struct Resolver<'a> {
  modules: Vec<Module>,
  enums: Vec<EnumNamespace>,
  scopes: Vec<Scope>,
  builtin_functions: HashSet<Arc<str>>,
  builtin_types: HashSet<Arc<str>>,
  /// The qualified variant names of the builtin enums (`FilterMode/Linear`).
  builtin_variants: HashSet<Arc<str>>,
  /// Qualifies main-file functions that share a builtin's name.
  root_builtin_prefix: String,
  /// The main file's unqualified internal names, which a name in another
  /// file can never refer to.
  root_names: HashSet<Arc<str>>,
  groups: HashMap<Vec<Arc<str>>, Arc<str>>,
  /// Where each definition's name is written, by internal name (several for
  /// overloads).
  definition_sites: HashMap<Arc<str>, Vec<DocumentPosition>>,
  /// The names written in the main file that refer to definitions, with
  /// the internal names they refer to.
  main_file_references: Vec<(DocumentPosition, Vec<Arc<str>>)>,
  errors: &'a mut ErrorLog,
}

fn peel_annotations(
  mut tree: EaslTree,
) -> (Vec<(DocumentPosition, EaslTree)>, EaslTree) {
  let mut annotations = vec![];
  loop {
    match tree {
      EaslTree::Inner(
        (position, EncloserOrOperator::Operator(Operator::Annotation)),
        mut children,
      ) if children.len() == 2 => {
        let body = children.pop().unwrap();
        annotations.push((position, children.pop().unwrap()));
        tree = body;
      }
      _ => return (annotations, tree),
    }
  }
}

fn wrap_annotations(
  annotations: Vec<(DocumentPosition, EaslTree)>,
  body: EaslTree,
) -> EaslTree {
  annotations
    .into_iter()
    .rev()
    .fold(body, |tree, (position, annotation)| {
      EaslTree::Inner(
        (position, EncloserOrOperator::Operator(Operator::Annotation)),
        vec![annotation, tree],
      )
    })
}

/// Removes a singular annotation like `@private`, returning whether it
/// was present.
fn take_flag(
  annotations: &mut Vec<(DocumentPosition, EaslTree)>,
  flag: &str,
) -> bool {
  let before = annotations.len();
  annotations.retain(
    |(_, annotation)| !matches!(annotation, EaslTree::Leaf(_, s) if s == flag),
  );
  annotations.len() != before
}

fn form_head(tree: &EaslTree) -> Option<&str> {
  if let EaslTree::Inner(
    (_, EncloserOrOperator::Encloser(Encloser::Parens)),
    children,
  ) = tree
    && let Some(EaslTree::Leaf(_, head)) = children.first()
  {
    Some(head.as_str())
  } else {
    None
  }
}

fn children(tree: &EaslTree) -> &[EaslTree] {
  match tree {
    EaslTree::Inner(_, children) => children,
    EaslTree::Leaf(_, _) => &[],
  }
}

/// The name a definition form defines: `(defn name ...)`,
/// `(defn (name T) ...)`, `(var name: T)`, and so on.
fn definition_name(
  kind: &str,
  tree: &EaslTree,
) -> Option<(Arc<str>, SourceTrace)> {
  let name_tree = children(tree).get(1)?;
  let name_tree = match (kind, name_tree) {
    (_, EaslTree::Leaf(_, _)) => name_tree,
    (
      "defn" | "struct" | "enum",
      EaslTree::Inner(
        (_, EncloserOrOperator::Encloser(Encloser::Parens)),
        signature,
      ),
    ) => signature.first()?,
    (
      "var" | "def" | "override",
      EaslTree::Inner(
        (_, EncloserOrOperator::Operator(Operator::TypeAscription)),
        ascription,
      ),
    ) => ascription.first()?,
    _ => return None,
  };
  if let EaslTree::Leaf(position, name) = name_tree {
    Some((name.as_str().into(), position.into()))
  } else {
    None
  }
}

fn variant_name(variant: &EaslTree) -> Option<(Arc<str>, SourceTrace)> {
  match variant {
    EaslTree::Leaf(position, name) => {
      Some((name.as_str().into(), position.into()))
    }
    EaslTree::Inner(
      (_, EncloserOrOperator::Encloser(Encloser::Parens)),
      children,
    ) => match children.first() {
      Some(EaslTree::Leaf(position, name)) => {
        Some((name.as_str().into(), position.into()))
      }
      _ => None,
    },
    _ => None,
  }
}

fn file_stem(path: &str) -> String {
  Path::new(path)
    .file_stem()
    .map(|stem| stem.to_string_lossy().into_owned())
    .unwrap_or_else(|| "module".to_string())
}

/// Resolves the modules of `trees` (each document's top-level forms,
/// already macroexpanded, in document order).
pub fn resolve_modules(
  documents: &EaslMultiDocument,
  trees: Vec<Vec<EaslTree>>,
  builtin_functions: HashSet<Arc<str>>,
  builtin_types: HashSet<Arc<str>>,
  builtin_variants: HashSet<Arc<str>>,
  errors: &mut ErrorLog,
) -> ResolvedModules {
  // Module prefixes share one namespace with the main file's inline
  // modules, which are qualified by their bare names.
  let mut taken_prefixes: HashSet<String> = trees
    .first()
    .into_iter()
    .flatten()
    .filter_map(|tree| {
      let (_, body) = peel_annotations(tree.clone());
      (form_head(&body) == Some("mod")).then(|| {
        if let Some(EaslTree::Leaf(_, name)) = children(&body).get(1) {
          Some(name.clone())
        } else {
          None
        }
      })?
    })
    .collect();
  let mut allocate_prefix = |base: String| {
    let mut candidate = base.clone();
    let mut i = 2;
    while taken_prefixes.contains(&candidate) {
      candidate = format!("{base}{i}");
      i += 1;
    }
    taken_prefixes.insert(candidate.clone());
    candidate + "/"
  };
  let root_builtin_prefix = allocate_prefix("root".to_string());
  let file_prefixes: Vec<String> = documents
    .sources
    .iter()
    .enumerate()
    .map(|(i, (_, path, _))| {
      if i == 0 {
        String::new()
      } else {
        allocate_prefix(file_stem(path))
      }
    })
    .collect();
  let mut resolver = Resolver {
    modules: vec![],
    enums: vec![],
    scopes: vec![],
    builtin_functions,
    builtin_types,
    builtin_variants,
    root_builtin_prefix,
    root_names: HashSet::new(),
    groups: HashMap::new(),
    definition_sites: HashMap::new(),
    main_file_references: vec![],
    errors,
  };
  for (file, prefix) in file_prefixes.into_iter().enumerate() {
    resolver.modules.push(Module {
      prefix,
      parent: None,
      file,
      members: HashMap::new(),
      forms: vec![],
    });
  }
  for (file, file_trees) in trees.into_iter().enumerate() {
    resolver.collect_module(file, file_trees, documents);
  }
  resolver.check_import_cycles(documents);
  resolver.root_names = resolver.modules[0]
    .members
    .iter()
    .filter(|(name, members)| {
      members
        .iter()
        .any(|member| member.origin.internal == **name)
    })
    .map(|(name, _)| name.clone())
    .collect();
  for id in 0..resolver.modules.len() {
    let scope = resolver.build_scope(id);
    resolver.scopes.push(scope);
  }
  let (names, written_names) = resolver.main_file_names();
  let mut output = vec![];
  for file in 0..documents.sources.len() {
    resolver.emit_module(file, &mut output);
  }
  let main_file_index = resolver.main_file_index(names, written_names);
  ResolvedModules {
    trees: output,
    main_file_index,
    overload_groups: resolver
      .groups
      .into_iter()
      .map(|(members, name)| (name, members))
      .collect(),
  }
}

impl<'a> Resolver<'a> {
  fn error(&mut self, kind: CompileErrorKind, source: SourceTrace) {
    self.errors.log(CompileError::new(kind, source));
  }

  fn define(
    &mut self,
    module: ModuleId,
    name: Arc<str>,
    origin: Origin,
    private: bool,
  ) {
    if is_path(&name) {
      self.error(CompileErrorKind::InvalidName, origin.source);
      return;
    }
    self.record_definition_site(&origin);
    let members = self.modules[module]
      .members
      .entry(name.clone())
      .or_default();
    if members.is_empty()
      || (origin.is_function()
        && members.iter().all(|member| member.origin.is_function()))
    {
      // Overloads with the same privacy share an internal name.
      if !members
        .iter()
        .any(|member| member.origin.internal == origin.internal)
      {
        members.push(Member { origin, private });
      }
    } else {
      let source = origin
        .source
        .insert_as_secondary(members[0].origin.source.clone());
      self.error(CompileErrorKind::NameCollision(name.to_string()), source)
    }
  }

  fn collect_module(
    &mut self,
    id: ModuleId,
    trees: Vec<EaslTree>,
    documents: &EaslMultiDocument,
  ) {
    // A function with both private and public overloads gives the private
    // ones their own internal name, so importers reach only the public
    // ones.
    let mut function_privacies: HashMap<Arc<str>, (bool, bool)> =
      HashMap::new();
    for tree in trees.iter() {
      let (mut annotations, body) = peel_annotations(tree.clone());
      if form_head(&body) == Some("defn")
        && let Some((name, _)) = definition_name("defn", &body)
      {
        let privacies = function_privacies.entry(name).or_default();
        if take_flag(&mut annotations, "private") {
          privacies.0 = true;
        } else {
          privacies.1 = true;
        }
      }
    }
    for tree in trees {
      let tree_source: SourceTrace = tree.position().into();
      let (mut annotations, body) = peel_annotations(tree);
      let private = take_flag(&mut annotations, "private");
      let unpack = take_flag(&mut annotations, "unpack");
      let head = form_head(&body).map(|head| head.to_string());
      let form = match head.as_deref() {
        Some("import") => {
          let target = documents.imports.get(body.position()).copied();
          let alias = match children(&body) {
            [_, EaslTree::Leaf(position, alias), _] => {
              Some((alias.as_str().into(), position.into()))
            }
            [_, _] => None,
            _ => {
              self.error(CompileErrorKind::InvalidImportStatement, tree_source);
              continue;
            }
          };
          let Some(target) = target else {
            self.error(CompileErrorKind::InvalidImportStatement, tree_source);
            continue;
          };
          Form::Import {
            alias,
            target,
            source: body.position().into(),
          }
        }
        Some("use") => Form::Use(body),
        Some("mod") => {
          let Some(EaslTree::Leaf(name_position, name)) =
            children(&body).get(1).cloned()
          else {
            self.error(CompileErrorKind::InvalidModuleForm, tree_source);
            continue;
          };
          let child = self.modules.len();
          let prefix = format!("{}{name}/", self.modules[id].prefix);
          self.modules.push(Module {
            prefix: prefix.clone(),
            parent: Some(id),
            file: self.modules[id].file,
            members: HashMap::new(),
            forms: vec![],
          });
          self.define(
            id,
            name.as_str().into(),
            Origin {
              internal: prefix.into(),
              kind: OriginKind::Module(child),
              source: name_position.into(),
            },
            private,
          );
          let EaslTree::Inner(_, mod_children) = body else {
            unreachable!()
          };
          self.collect_module(
            child,
            mod_children.into_iter().skip(2).collect(),
            documents,
          );
          Form::Mod(child)
        }
        Some(
          keyword @ ("defn" | "struct" | "enum" | "var" | "def" | "override"),
        ) => match definition_name(keyword, &body) {
          Some((name, name_source)) => {
            let prefix = self.modules[id].prefix.clone();
            let mut internal = if keyword == "defn"
              && prefix.is_empty()
              && self.builtin_functions.contains(&name)
            {
              format!("{}{name}", self.root_builtin_prefix)
            } else {
              format!("{prefix}{name}")
            };
            if keyword == "defn"
              && private
              && function_privacies.get(&name) == Some(&(true, true))
            {
              internal += "/private";
            }
            let internal: Arc<str> = internal.into();
            if matches!(keyword, "struct" | "enum")
              && self.builtin_types.contains(&name)
            {
              self.error(
                CompileErrorKind::BuiltinTypeRedefinition(name.to_string()),
                name_source.clone(),
              );
            }
            let kind = match keyword {
              "defn" => DefinitionKind::Function,
              "struct" => DefinitionKind::Struct,
              "enum" => {
                let enum_id = self.enums.len();
                let mut variants = vec![];
                for variant in children(&body).iter().skip(2) {
                  if let Some((variant, variant_source)) = variant_name(variant)
                  {
                    let variant_internal: Arc<str> = if unpack {
                      format!("{prefix}{variant}").into()
                    } else {
                      format!("{internal}/{variant}").into()
                    };
                    let origin = Origin {
                      internal: variant_internal,
                      kind: OriginKind::Variant,
                      source: variant_source,
                    };
                    self.record_definition_site(&origin);
                    if unpack {
                      self.define(id, variant.clone(), origin.clone(), private);
                    }
                    variants.push((variant, origin));
                  }
                }
                self.enums.push(EnumNamespace { variants });
                DefinitionKind::Enum(enum_id)
              }
              _ => DefinitionKind::Variable,
            };
            if unpack && !matches!(kind, DefinitionKind::Enum(_)) {
              self.error(
                CompileErrorKind::InvalidAnnotation(
                  "`@unpack` only applies to enums".into(),
                ),
                tree_source.clone(),
              );
            }
            self.define(
              id,
              name,
              Origin {
                internal: internal.clone(),
                kind: match kind {
                  DefinitionKind::Function => OriginKind::Function,
                  DefinitionKind::Struct => OriginKind::Struct,
                  DefinitionKind::Enum(enum_id) => OriginKind::Enum(enum_id),
                  DefinitionKind::Variable => OriginKind::Value,
                },
                source: name_source,
              },
              private,
            );
            Form::Definition {
              annotations,
              body,
              kind,
              internal,
            }
          }
          None => Form::Other(wrap_annotations(annotations, body)),
        },
        _ => Form::Other(wrap_annotations(annotations, body)),
      };
      self.modules[id].forms.push(form);
    }
  }

  /// Reports every import that closes a cycle of files importing each
  /// other.
  fn check_import_cycles(&mut self, documents: &EaslMultiDocument) {
    let file_count = documents.sources.len();
    let mut edges: Vec<Vec<(usize, SourceTrace)>> = vec![vec![]; file_count];
    for module in self.modules.iter() {
      for form in module.forms.iter() {
        if let Form::Import { target, source, .. } = form {
          edges[module.file].push((*target, source.clone()));
        }
      }
    }
    #[derive(Clone, Copy, PartialEq)]
    enum State {
      Unvisited,
      Active,
      Done,
    }
    fn visit(
      file: usize,
      edges: &Vec<Vec<(usize, SourceTrace)>>,
      states: &mut Vec<State>,
      stack: &mut Vec<usize>,
      cycles: &mut Vec<(Vec<usize>, SourceTrace)>,
    ) {
      states[file] = State::Active;
      stack.push(file);
      for (target, source) in edges[file].iter() {
        match states[*target] {
          State::Unvisited => visit(*target, edges, states, stack, cycles),
          State::Active => {
            let start = stack.iter().position(|f| f == target).unwrap();
            let mut cycle = stack[start..].to_vec();
            cycle.push(*target);
            cycles.push((cycle, source.clone()));
          }
          State::Done => {}
        }
      }
      stack.pop();
      states[file] = State::Done;
    }
    let mut states = vec![State::Unvisited; file_count];
    let mut cycles = vec![];
    for file in 0..file_count {
      if states[file] == State::Unvisited {
        visit(file, &edges, &mut states, &mut vec![], &mut cycles);
      }
    }
    for (cycle, source) in cycles {
      let files = cycle
        .into_iter()
        .map(|file| {
          Path::new(&documents.sources[file].1)
            .file_name()
            .map(|name| name.to_string_lossy().into_owned())
            .unwrap_or_default()
        })
        .collect();
      self.error(CompileErrorKind::ImportCycle(files), source);
    }
  }

  /// Whether code in `from` can see the private members of `owner`: only
  /// `owner` itself and the inline modules nested in it can.
  fn can_see_private(&self, owner: ModuleId, from: ModuleId) -> bool {
    let mut current = Some(from);
    while let Some(id) = current {
      if id == owner {
        return true;
      }
      current = self.modules[id].parent;
    }
    false
  }

  /// Binds `name` to `origin` in `scope`. Functions with the same name
  /// overload; any other repeated name is a collision, unless it's the
  /// same definition reached twice.
  fn bind(
    &mut self,
    scope: &mut Scope,
    name: Arc<str>,
    origin: Origin,
    source: &SourceTrace,
  ) {
    let origins = scope.entry(name.clone()).or_default();
    if origins
      .iter()
      .any(|existing| existing.internal == origin.internal)
    {
      return;
    }
    if origins.is_empty()
      || (origin.is_function() && origins.iter().all(Origin::is_function))
    {
      origins.push(origin);
    } else {
      let source = source
        .clone()
        .insert_as_secondary(origins[0].source.clone());
      self.error(CompileErrorKind::NameCollision(name.to_string()), source);
    }
  }

  /// The members of `namespace` that code in `from` can see.
  fn visible_members(
    &self,
    namespace: Namespace,
    from: ModuleId,
  ) -> Vec<(Arc<str>, Origin)> {
    match namespace {
      Namespace::Module(id) => {
        let can_see_private = self.can_see_private(id, from);
        let mut members: Vec<(Arc<str>, Origin)> = self.modules[id]
          .members
          .iter()
          .flat_map(|(name, members)| {
            members
              .iter()
              .filter(|member| can_see_private || !member.private)
              .map(|member| (name.clone(), member.origin.clone()))
          })
          .collect();
        members.sort_by(|(a, _), (b, _)| a.cmp(b));
        members
      }
      Namespace::Enum(id) => self.enums[id].variants.clone(),
    }
  }

  /// The definitions `name` refers to in `namespace`, as seen from `from`.
  fn member(
    &mut self,
    namespace: Namespace,
    namespace_name: &str,
    name: &str,
    from: ModuleId,
    source: &SourceTrace,
  ) -> Option<Vec<Origin>> {
    let (candidates, can_see_private): (Vec<(Origin, bool)>, bool) =
      match namespace {
        Namespace::Module(id) => (
          self.modules[id]
            .members
            .get(name)
            .into_iter()
            .flatten()
            .map(|member| (member.origin.clone(), member.private))
            .collect(),
          self.can_see_private(id, from),
        ),
        Namespace::Enum(id) => (
          self.enums[id]
            .variants
            .iter()
            .filter(|(variant, _)| &**variant == name)
            .map(|(_, origin)| (origin.clone(), false))
            .collect(),
          true,
        ),
      };
    if candidates.is_empty() {
      self.error(
        CompileErrorKind::UnknownModuleMember(
          namespace_name.to_string(),
          name.to_string(),
        ),
        source.clone(),
      );
      return None;
    }
    let visible: Vec<Origin> = candidates
      .into_iter()
      .filter(|(_, private)| can_see_private || !private)
      .map(|(origin, _)| origin)
      .collect();
    if visible.is_empty() {
      self.error(
        CompileErrorKind::PrivateName(format!("{namespace_name}/{name}")),
        source.clone(),
      );
      return None;
    }
    Some(visible)
  }

  /// Resolves a path like `lib/shapes/Circle` written in `from`, whose
  /// first segment is looked up in `scope`.
  fn resolve_path(
    &mut self,
    scope: &Scope,
    from: ModuleId,
    path: &str,
    source: &SourceTrace,
  ) -> Option<Vec<Origin>> {
    let segments: Vec<&str> = path.split('/').collect();
    let head = segments[0];
    let Some(mut origins) = scope.get(head).cloned() else {
      self.error(
        CompileErrorKind::UnboundName(head.to_string()),
        source.clone(),
      );
      return None;
    };
    for (i, segment) in segments.iter().enumerate().skip(1) {
      let namespace_name = segments[..i].join("/");
      let Some(namespace) = origins.iter().find_map(Origin::namespace) else {
        self.error(
          CompileErrorKind::NotANamespace(namespace_name),
          source.clone(),
        );
        return None;
      };
      origins =
        self.member(namespace, &namespace_name, segment, from, source)?;
    }
    Some(origins)
  }

  fn build_scope(&mut self, id: ModuleId) -> Scope {
    let mut scope = match self.modules[id].parent {
      Some(parent) => self.scopes[parent].clone(),
      None => Scope::new(),
    };
    let mut members: Vec<(Arc<str>, Member)> = self.modules[id]
      .members
      .iter()
      .flat_map(|(name, members)| {
        members.iter().map(|member| (name.clone(), member.clone()))
      })
      .collect();
    members.sort_by(|(a, _), (b, _)| a.cmp(b));
    for (name, member) in members {
      let source = member.origin.source.clone();
      self.bind(&mut scope, name, member.origin, &source);
    }
    let forms = std::mem::take(&mut self.modules[id].forms);
    for form in forms.iter() {
      match form {
        Form::Import {
          alias: Some((alias, alias_source)),
          target,
          ..
        } => {
          let origin = Origin {
            internal: self.modules[*target].prefix.clone().into(),
            kind: OriginKind::Module(*target),
            source: alias_source.clone(),
          };
          self.bind(&mut scope, alias.clone(), origin, alias_source);
        }
        Form::Import {
          alias: None,
          target,
          source,
        } => {
          for (name, origin) in
            self.visible_members(Namespace::Module(*target), id)
          {
            self.bind(&mut scope, name, origin, source);
          }
        }
        Form::Use(tree) => self.apply_use(&mut scope, id, tree),
        _ => {}
      }
    }
    self.modules[id].forms = forms;
    // Functions that meet in this scope form an overload group even when
    // nothing here refers to them, so conflicting signatures are caught.
    let mut names: Vec<&Arc<str>> = scope.keys().collect();
    names.sort();
    for name in names {
      let origins = &scope[name];
      if origins.iter().all(Origin::is_function) {
        let members = self.function_members(name, origins);
        self.overload_group(name, members);
      }
    }
    scope
  }
  fn record_definition_site(&mut self, origin: &Origin) {
    if let Some(position) = &origin.source.primary_position {
      let sites = self
        .definition_sites
        .entry(origin.internal.clone())
        .or_default();
      if !sites.contains(position) {
        sites.push(position.clone());
      }
    }
  }

  /// Records that the name at `source`, written in module `id`, refers to
  /// `origins`, when that's in the main file.
  fn record_reference(
    &mut self,
    id: ModuleId,
    source: &SourceTrace,
    origins: &[Origin],
  ) {
    if self.modules[id].file == 0
      && let Some(position) = &source.primary_position
    {
      self.main_file_references.push((
        position.clone(),
        origins
          .iter()
          .map(|origin| origin.internal.clone())
          .collect(),
      ));
    }
  }

  fn main_file_index(
    &self,
    names: Vec<(Arc<str>, NameKind)>,
    written_names: WrittenNames,
  ) -> MainFileIndex {
    let sites_of = |internal: &Arc<str>| {
      self
        .definition_sites
        .get(internal)
        .into_iter()
        .flatten()
        .cloned()
    };
    let mut definitions: Vec<DocumentPosition> = self
      .definition_sites
      .values()
      .flatten()
      .filter(|position| position.path.first() == Some(&0))
      .cloned()
      .collect();
    definitions.sort_by_key(|position| position.span.start);
    MainFileIndex {
      names,
      written_names,
      references: self
        .main_file_references
        .iter()
        .map(|(position, internals)| {
          (
            position.clone(),
            internals.iter().flat_map(sites_of).collect(),
          )
        })
        .collect(),
      definitions,
    }
  }

  /// See [`MainFileIndex::names`].
  fn main_file_names(&self) -> (Vec<(Arc<str>, NameKind)>, WrittenNames) {
    fn add_members(
      resolver: &Resolver,
      namespace: Namespace,
      path: &str,
      found: &mut Vec<(Arc<str>, Origin)>,
    ) {
      for (member, origin) in resolver.visible_members(namespace, 0) {
        let member_path = format!("{path}/{member}");
        if let Some(namespace) = origin.namespace() {
          add_members(resolver, namespace, &member_path, found);
        }
        found.push((member_path.as_str().into(), origin));
      }
    }
    let mut found: Vec<(Arc<str>, Origin)> = vec![];
    for (name, origins) in self.scopes[0].iter() {
      if let Some(namespace) = origins.iter().find_map(Origin::namespace) {
        add_members(self, namespace, name, &mut found);
      }
      for origin in origins {
        found.push((name.clone(), origin.clone()));
      }
    }
    let mut written_names = WrittenNames::new();
    for (written, origin) in found.iter() {
      let shortest = written_names
        .entry(origin.internal.clone())
        .or_insert_with(|| written.clone());
      if (written.len(), written) < (shortest.len(), &*shortest) {
        *shortest = written.clone();
      }
    }
    written_names.retain(|internal, written| internal != written);
    let mut names: Vec<(Arc<str>, NameKind)> = found
      .into_iter()
      .map(|(written, origin)| (written, origin.name_kind()))
      .collect();
    names.sort_by(|(a, _), (b, _)| a.cmp(b));
    names.dedup_by(|(a, _), (b, _)| a == b);
    (names, written_names)
  }

  /// The registry buckets a reference to `name` draws candidates from,
  /// when `name` refers to the functions `origins`.
  fn function_members(&self, name: &str, origins: &[Origin]) -> Vec<Arc<str>> {
    let mut members: Vec<Arc<str>> = origins
      .iter()
      .map(|origin| origin.internal.clone())
      .collect();
    if self.builtin_functions.contains(name) {
      members.push(name.into());
    }
    members
  }

  /// Applies `(use path)` or `(use path [a b])` to `scope`.
  fn apply_use(&mut self, scope: &mut Scope, id: ModuleId, tree: &EaslTree) {
    let source: SourceTrace = tree.position().into();
    let (path_tree, selection) = match children(tree) {
      [_, path] => (path, None),
      [
        _,
        path,
        EaslTree::Inner(
          (_, EncloserOrOperator::Encloser(Encloser::Square)),
          selection,
        ),
      ] => (path, Some(selection)),
      _ => {
        self.error(CompileErrorKind::InvalidUseStatement, source);
        return;
      }
    };
    let EaslTree::Leaf(path_position, path) = path_tree else {
      self.error(CompileErrorKind::InvalidUseStatement, source);
      return;
    };
    let path_source: SourceTrace = path_position.into();
    let origins = if is_path(path) {
      match self.resolve_path(scope, id, path, &path_source) {
        Some(origins) => origins,
        None => return,
      }
    } else {
      match scope.get(path.as_str()) {
        Some(origins) => origins.clone(),
        None => {
          self.error(CompileErrorKind::UnboundName(path.clone()), path_source);
          return;
        }
      }
    };
    let Some(namespace) = origins.iter().find_map(Origin::namespace) else {
      self.error(CompileErrorKind::NotANamespace(path.clone()), path_source);
      return;
    };
    match selection {
      None => {
        for (name, origin) in self.visible_members(namespace, id) {
          self.bind(scope, name, origin, &source);
        }
      }
      Some(selection) => {
        for selected in selection {
          let EaslTree::Leaf(position, name) = selected else {
            self.error(
              CompileErrorKind::InvalidUseStatement,
              selected.position().into(),
            );
            continue;
          };
          let selected_source: SourceTrace = position.into();
          let origins = self
            .member(namespace, path, name, id, &selected_source)
            .unwrap_or_default();
          self.record_reference(id, &selected_source, &origins);
          for origin in origins {
            self.bind(scope, name.as_str().into(), origin, &selected_source);
          }
        }
      }
    }
  }

  fn emit_module(&mut self, id: ModuleId, output: &mut Vec<EaslTree>) {
    let forms = std::mem::take(&mut self.modules[id].forms);
    for form in forms {
      match form {
        Form::Definition {
          annotations,
          body,
          kind,
          internal,
        } => {
          let body = self.rewrite_definition(id, body, kind, &internal);
          output.push(wrap_annotations(annotations, body));
        }
        Form::Mod(child) => self.emit_module(child, output),
        Form::Other(tree) => {
          let rewritten =
            self.rewrite(tree, Position::Value, id, &HashSet::new());
          output.push(rewritten);
        }
        Form::Import { .. } | Form::Use(_) => {}
      }
    }
  }

  fn rewrite_definition(
    &mut self,
    id: ModuleId,
    body: EaslTree,
    kind: DefinitionKind,
    internal: &Arc<str>,
  ) -> EaslTree {
    let EaslTree::Inner(parens, children) = body else {
      unreachable!()
    };
    let mut children = children.into_iter();
    let mut output = vec![children.next().unwrap()];
    let mut generics = HashSet::new();
    let name_tree = children.next().unwrap();
    output.push(match name_tree {
      EaslTree::Leaf(position, _) => {
        EaslTree::Leaf(position, internal.to_string())
      }
      EaslTree::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Parens)),
        signature,
      ) => {
        let mut signature = signature.into_iter();
        let EaslTree::Leaf(name_position, _) = signature.next().unwrap() else {
          unreachable!()
        };
        let generic_trees: Vec<EaslTree> = signature.collect();
        for generic in generic_trees.iter() {
          let generic_name = match generic {
            EaslTree::Inner(
              (_, EncloserOrOperator::Operator(Operator::TypeAscription)),
              ascription,
            ) => ascription.first(),
            _ => Some(generic),
          };
          if let Some(EaslTree::Leaf(_, name)) = generic_name {
            generics.insert(Arc::<str>::from(name.as_str()));
          }
        }
        let mut new_signature =
          vec![EaslTree::Leaf(name_position, internal.to_string())];
        for generic in generic_trees {
          new_signature.push(self.rewrite(
            generic,
            Position::Type,
            id,
            &generics,
          ));
        }
        EaslTree::Inner(
          (position, EncloserOrOperator::Encloser(Encloser::Parens)),
          new_signature,
        )
      }
      EaslTree::Inner(
        (position, EncloserOrOperator::Operator(Operator::TypeAscription)),
        ascription,
      ) => {
        let mut ascription = ascription.into_iter();
        let EaslTree::Leaf(name_position, _) = ascription.next().unwrap()
        else {
          unreachable!()
        };
        let mut new_ascription =
          vec![EaslTree::Leaf(name_position, internal.to_string())];
        for rest in ascription {
          new_ascription.push(self.rewrite(
            rest,
            Position::Type,
            id,
            &generics,
          ));
        }
        EaslTree::Inner(
          (
            position,
            EncloserOrOperator::Operator(Operator::TypeAscription),
          ),
          new_ascription,
        )
      }
      other => unreachable!("{other:?}"),
    });
    for child in children {
      output.push(match kind {
        DefinitionKind::Struct => self.rewrite_field(child, id, &generics),
        DefinitionKind::Enum(enum_id) => {
          self.rewrite_variant(child, enum_id, id, &generics)
        }
        DefinitionKind::Function | DefinitionKind::Variable => {
          self.rewrite(child, Position::Value, id, &generics)
        }
      });
    }
    EaslTree::Inner(parens, output)
  }

  /// A struct field: its name is left alone, its type is resolved.
  fn rewrite_field(
    &mut self,
    field: EaslTree,
    id: ModuleId,
    generics: &HashSet<Arc<str>>,
  ) -> EaslTree {
    match field {
      EaslTree::Inner(
        (position, EncloserOrOperator::Operator(Operator::Annotation)),
        mut children,
      ) if children.len() == 2 => {
        let field = children.pop().unwrap();
        let field = self.rewrite_field(field, id, generics);
        children.push(field);
        EaslTree::Inner(
          (position, EncloserOrOperator::Operator(Operator::Annotation)),
          children,
        )
      }
      EaslTree::Inner(
        (position, EncloserOrOperator::Operator(Operator::TypeAscription)),
        children,
      ) => {
        let mut children = children.into_iter();
        let mut output = vec![children.next().unwrap()];
        for child in children {
          output.push(self.rewrite(child, Position::Type, id, generics));
        }
        EaslTree::Inner(
          (
            position,
            EncloserOrOperator::Operator(Operator::TypeAscription),
          ),
          output,
        )
      }
      other => self.rewrite(other, Position::Value, id, generics),
    }
  }

  fn rewrite_variant(
    &mut self,
    variant: EaslTree,
    enum_id: EnumId,
    id: ModuleId,
    generics: &HashSet<Arc<str>>,
  ) -> EaslTree {
    let internal_name = |name: &str, enums: &Vec<EnumNamespace>| {
      enums[enum_id]
        .variants
        .iter()
        .find(|(variant, _)| &**variant == name)
        .map(|(_, origin)| origin.internal.to_string())
        .unwrap_or_else(|| name.to_string())
    };
    match variant {
      EaslTree::Leaf(position, name) => {
        EaslTree::Leaf(position, internal_name(&name, &self.enums))
      }
      EaslTree::Inner(
        (position, EncloserOrOperator::Encloser(Encloser::Parens)),
        children,
      ) => {
        let mut children = children.into_iter();
        let mut output = vec![];
        if let Some(name) = children.next() {
          output.push(match name {
            EaslTree::Leaf(name_position, name) => {
              EaslTree::Leaf(name_position, internal_name(&name, &self.enums))
            }
            other => other,
          });
        }
        for child in children {
          output.push(self.rewrite(child, Position::Type, id, generics));
        }
        EaslTree::Inner(
          (position, EncloserOrOperator::Encloser(Encloser::Parens)),
          output,
        )
      }
      other => self.rewrite(other, Position::Value, id, generics),
    }
  }

  fn rewrite(
    &mut self,
    tree: EaslTree,
    position: Position,
    id: ModuleId,
    generics: &HashSet<Arc<str>>,
  ) -> EaslTree {
    use EncloserOrOperator as EO;
    match tree {
      EaslTree::Leaf(leaf_position, name) => {
        let source: SourceTrace = (&leaf_position).into();
        let resolved =
          self.resolve_leaf(&name, position, id, generics, &source);
        EaslTree::Leaf(leaf_position, resolved)
      }
      EaslTree::Inner(
        (tree_position, EO::Operator(Operator::Annotation)),
        mut children,
      ) if children.len() == 2 => {
        let body = children.pop().unwrap();
        let body = self.rewrite(body, position, id, generics);
        children.push(body);
        EaslTree::Inner(
          (tree_position, EO::Operator(Operator::Annotation)),
          children,
        )
      }
      EaslTree::Inner(
        (tree_position, EO::Operator(Operator::TypeAscription)),
        children,
      ) => {
        let mut children = children.into_iter();
        let mut output = vec![];
        if let Some(value) = children.next() {
          output.push(self.rewrite(value, position, id, generics));
        }
        for child in children {
          output.push(self.rewrite(child, Position::Type, id, generics));
        }
        EaslTree::Inner(
          (tree_position, EO::Operator(Operator::TypeAscription)),
          output,
        )
      }
      EaslTree::Inner(
        (tree_position, EO::Operator(Operator::Into)),
        children,
      ) => {
        let source: SourceTrace = (&tree_position).into();
        let into =
          self.resolve_leaf("into", Position::Value, id, generics, &source);
        let children: Vec<EaslTree> = children
          .into_iter()
          .map(|child| self.rewrite(child, position, id, generics))
          .collect();
        if into == "into" {
          EaslTree::Inner(
            (tree_position, EO::Operator(Operator::Into)),
            children,
          )
        } else {
          let mut application =
            vec![EaslTree::Leaf(tree_position.clone(), into)];
          application.extend(children);
          EaslTree::Inner(
            (tree_position, EO::Encloser(Encloser::Parens)),
            application,
          )
        }
      }
      tree @ EaslTree::Inner(
        (
          _,
          EO::Operator(Operator::ExpressionComment | Operator::Annotation)
          | EO::Encloser(
            Encloser::Quote | Encloser::LineComment | Encloser::BlockComment,
          ),
        ),
        _,
      ) => tree,
      EaslTree::Inner(
        (
          tree_position,
          EO::Encloser(
            encloser @ (Encloser::Parens | Encloser::Square | Encloser::Curly),
          ),
        ),
        children,
      ) => EaslTree::Inner(
        (tree_position, EO::Encloser(encloser)),
        children
          .into_iter()
          .map(|child| self.rewrite(child, position, id, generics))
          .collect(),
      ),
    }
  }

  /// A name that refers to nothing in scope. If it's one of the main file's
  /// definitions, which another file can't see, it's renamed so it can't
  /// be mistaken for that definition later.
  fn unbound(&self, name: &str, id: ModuleId) -> String {
    let module = &self.modules[id];
    if module.file != 0 && self.root_names.contains(name) {
      format!("{}{name}", module.prefix)
    } else {
      name.to_string()
    }
  }

  fn overload_group(
    &mut self,
    name: &str,
    mut members: Vec<Arc<str>>,
  ) -> String {
    members.sort();
    members.dedup();
    if members.len() == 1 {
      return members[0].to_string();
    }
    let next_index = self.groups.len();
    self
      .groups
      .entry(members)
      .or_insert_with(|| format!("{name}@{next_index}").into())
      .to_string()
  }

  fn resolve_leaf(
    &mut self,
    name: &str,
    position: Position,
    id: ModuleId,
    generics: &HashSet<Arc<str>>,
    source: &SourceTrace,
  ) -> String {
    // `a.b.c` accesses fields of whatever `a` names.
    if let Some((head, fields)) = name.split_once('.')
      && !head.is_empty()
    {
      let head = self.resolve_leaf(head, position, id, generics, source);
      return format!("{head}.{fields}");
    }
    // A builtin enum's variants (`FilterMode/Linear`) are builtin names
    // themselves, unless the program binds the head to something else.
    if self.builtin_variants.contains(name)
      && let Some((head, _)) = name.split_once('/')
      && self.scopes[id].get(head).is_none()
    {
      return name.to_string();
    }
    if is_path(name) {
      let scope = std::mem::take(&mut self.scopes[id]);
      let origins = self.resolve_path(&scope, id, name, source);
      self.scopes[id] = scope;
      return match origins.as_deref() {
        None => name.to_string(),
        Some(origins)
          if origins
            .iter()
            .any(|origin| matches!(origin.kind, OriginKind::Module(_))) =>
        {
          self.error(
            CompileErrorKind::ModuleUsedAsValue(name.to_string()),
            source.clone(),
          );
          name.to_string()
        }
        Some(origins) => {
          self.record_reference(id, source, origins);
          match origins {
            [origin] => origin.internal.to_string(),
            functions => {
              let members = functions
                .iter()
                .map(|origin| origin.internal.clone())
                .collect();
              self.overload_group(display_name(name), members)
            }
          }
        }
      };
    }
    if generics.contains(name) {
      return self.unbound(name, id);
    }
    let origins: Vec<Origin> = self.scopes[id]
      .get(name)
      .into_iter()
      .flatten()
      .filter(|origin| position == Position::Value || !origin.is_function())
      .cloned()
      .collect();
    match origins.as_slice() {
      [] => self.unbound(name, id),
      [
        Origin {
          kind: OriginKind::Module(_),
          ..
        },
      ] => {
        self.error(
          CompileErrorKind::ModuleUsedAsValue(name.to_string()),
          source.clone(),
        );
        name.to_string()
      }
      [origin] if !origin.is_function() => {
        self.record_reference(id, source, &origins);
        origin.internal.to_string()
      }
      functions => {
        self.record_reference(id, source, functions);
        let members = self.function_members(name, functions);
        self.overload_group(name, members)
      }
    }
  }
}
