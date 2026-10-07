use std::{
  collections::{HashMap, HashSet},
  io,
  path::{Component, Path, PathBuf},
  sync::LazyLock,
};

use fsexp::{
  Context as SSEContext, DocumentSyntaxTree, Encloser as SSEEncloser,
  EncloserOrOperator, Operator as SSEOperator, ParseError,
  document::{Document, DocumentPosition},
  standard_whitespace_chars,
  syntax::{ContextId, Syntax},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Context {
  Default,
  StructuredComment,
  UnstructuredComment,
  String,
}

impl ContextId for Context {
  fn is_comment(&self) -> bool {
    use Context::*;
    match self {
      StructuredComment | UnstructuredComment => true,
      _ => false,
    }
  }
}

use crate::compiler::{
  error::{CompileError, CompileErrorKind, ErrorLog},
  program::EaslDocument,
};
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Encloser {
  Parens,
  Square,
  Curly,
  LineComment,
  BlockComment,
  Quote,
}
impl SSEEncloser for Encloser {
  fn opening_encloser_str(&self) -> &str {
    use Encloser::*;
    match self {
      Parens => "(",
      Square => "[",
      Curly => "{",
      LineComment => ";",
      BlockComment => ";*",
      Quote => "\"",
    }
  }

  fn closing_encloser_str(&self) -> &str {
    use Encloser::*;
    match self {
      Parens => ")",
      Square => "]",
      Curly => "}",
      LineComment => "\n",
      BlockComment => "*;",
      Quote => "\"",
    }
  }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
  Annotation,
  TypeAscription,
  ExpressionComment,
  /// Unary prefix `~`, sugar for an application of `into`: `~a` desugars
  /// to `(into a)` during expression parsing (the `O::Into` arm of
  /// `TypedExp::try_from_easl_tree`).
  Into,
}
impl SSEOperator for Operator {
  fn left_args(&self) -> usize {
    match self {
      Operator::Annotation => 0,
      Operator::TypeAscription => 1,
      Operator::ExpressionComment => 0,
      Operator::Into => 0,
    }
  }

  fn right_args(&self) -> usize {
    match self {
      Operator::Annotation => 2,
      Operator::TypeAscription => 1,
      Operator::ExpressionComment => 1,
      Operator::Into => 1,
    }
  }

  fn op_str(&self) -> &str {
    match self {
      Operator::Annotation => "@",
      Operator::TypeAscription => ":",
      Operator::ExpressionComment => "#_",
      Operator::Into => "~",
    }
  }
}

static DEFAULT_CTX: LazyLock<SSEContext<Encloser, Operator>> =
  LazyLock::new(|| {
    SSEContext::new(
      // Openers match in list order, so `;*` (block comment) must come
      // before its prefix `;` (line comment).
      vec![
        Encloser::Parens,
        Encloser::Square,
        Encloser::Curly,
        Encloser::BlockComment,
        Encloser::LineComment,
        Encloser::Quote,
      ],
      vec![
        Operator::Annotation,
        Operator::TypeAscription,
        Operator::ExpressionComment,
        Operator::Into,
      ],
      None,
      standard_whitespace_chars(),
    )
  });

static TRIVIAL_CTX: LazyLock<SSEContext<Encloser, Operator>> =
  LazyLock::new(|| SSEContext::trivial());

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EaslSyntax;

impl Syntax for EaslSyntax {
  type C = Context;
  type E = Encloser;
  type O = Operator;

  fn root_context(&self) -> Self::C {
    Context::Default
  }
  fn context<'a>(&'a self, id: &Self::C) -> &'a SSEContext<Self::E, Self::O> {
    match id {
      Context::Default | Context::StructuredComment => &*DEFAULT_CTX,
      Context::UnstructuredComment | Context::String => &*TRIVIAL_CTX,
    }
  }
  fn encloser_context(&self, encloser: &Self::E) -> Option<Self::C> {
    match encloser {
      Encloser::LineComment | Encloser::BlockComment => {
        Some(Context::UnstructuredComment)
      }
      Encloser::Quote => Some(Context::String),
      _ => None,
    }
  }
  fn operator_context(&self, operator: &Self::O) -> Option<Self::C> {
    match operator {
      Operator::ExpressionComment => Some(Context::StructuredComment),
      _ => None,
    }
  }
  fn reserved_tokens(&self) -> impl Iterator<Item = &str> {
    ["||"].into_iter()
  }
}

pub fn parse_easl(easl_source: &str) -> EaslDocument {
  Document::from_text_with_syntax(EaslSyntax, easl_source)
}

pub fn parse_easl_without_comments(easl_source: &str) -> EaslDocument {
  let mut doc = parse_easl(easl_source);
  doc.strip_comments();
  doc
}

pub type EaslTree = DocumentSyntaxTree<Encloser, Operator>;

#[derive(Debug)]
pub struct EaslMultiDocument {
  pub sources: Vec<(EaslDocument, String, String)>,
  /// Which document each `import` form loads: the form's position (its
  /// path naming its own document) to the imported document's index.
  pub imports: HashMap<DocumentPosition, usize>,
}

impl EaslMultiDocument {
  fn empty() -> Self {
    Self {
      sources: vec![],
      imports: HashMap::new(),
    }
  }
  pub fn from_singular_document_sourceless(document: EaslDocument) -> Self {
    Self::from_singular_document(document, String::new(), String::new())
  }
  pub fn from_singular_document(
    document: EaslDocument,
    path: String,
    source: String,
  ) -> Self {
    let mut docs = Self::empty();
    docs.add_document(document, path, source);
    docs
  }
  pub fn add_document(
    &mut self,
    mut document: EaslDocument,
    path: String,
    source: String,
  ) {
    for ast in document.syntax_trees.iter_mut() {
      ast.walk_mut(&mut |subast| {
        match subast {
          fsexp::Ast::Leaf(pos, _) | fsexp::Ast::Inner((pos, _), _) => {
            pos.path.insert(0, self.sources.len())
          }
        };
      });
    }
    self.sources.push((document, path, source));
  }
  pub fn describe_document_position(
    &self,
    mut pos: DocumentPosition,
  ) -> String {
    if pos.path.is_empty() {
      return "[INTERNAL CODE]".to_string();
    }
    let (source_document, source_path, source_text) =
      &self.sources[pos.path.remove(0)];
    let inner_pos_string =
      source_document.describe_document_position(pos.span, source_text);
    if source_path.is_empty() {
      inner_pos_string
    } else {
      format!("{}\n{}", source_path, inner_pos_string)
    }
  }
  /// Describes every parse failure across the documents.
  pub fn describe_parse_failures(&self) -> String {
    self
      .sources
      .iter()
      .flat_map(|(document, _, source)| {
        document
          .parsing_failures
          .iter()
          .map(|err| err.describe(document, source))
      })
      .collect::<Vec<_>>()
      .join("\n\n")
  }
  pub fn describe_parse_error(&self, err: ParseError) -> String {
    let (source_document, _, source_text) = self.sources.last().unwrap();
    err.describe(source_document, source_text)
  }
}

pub fn load_and_parse_easl_multidocument_with_lookup_function(
  primary_easl_file_path: &Path,
  lookup: impl FnMut(&Path) -> std::io::Result<String>,
) -> std::io::Result<
  Result<
    Result<EaslMultiDocument, (EaslMultiDocument, ErrorLog)>,
    EaslMultiDocument,
  >,
> {
  load_and_parse_easl_multidocument_with_resolution(
    primary_easl_file_path,
    lookup,
    Path::canonicalize,
  )
}

/// Parses a program from in-memory sources keyed by path, for hosts without
/// a filesystem (the web runtime). Paths, including imports, resolve
/// lexically: `.` and `..` components are folded away, and relative imports
/// join the importing file's directory.
pub fn load_and_parse_easl_multidocument_from_sources(
  primary_easl_file_path: &Path,
  sources: &HashMap<PathBuf, String>,
) -> std::io::Result<
  Result<
    Result<EaslMultiDocument, (EaslMultiDocument, ErrorLog)>,
    EaslMultiDocument,
  >,
> {
  let sources: HashMap<PathBuf, &String> = sources
    .iter()
    .map(|(path, source)| (normalize_path_lexically(path), source))
    .collect();
  load_and_parse_easl_multidocument_with_resolution(
    primary_easl_file_path,
    |path| {
      sources
        .get(path)
        .map(|source| (*source).clone())
        .ok_or_else(|| {
          io::Error::new(
            io::ErrorKind::NotFound,
            format!("no source file at {}", path.display()),
          )
        })
    },
    |path| Ok(normalize_path_lexically(path)),
  )
}

/// Folds away `.` and `..` components without touching a filesystem.
fn normalize_path_lexically(path: &Path) -> PathBuf {
  let mut normalized = PathBuf::new();
  for component in path.components() {
    match component {
      Component::CurDir => {}
      Component::ParentDir => {
        normalized.pop();
      }
      other => normalized.push(other),
    }
  }
  normalized
}

/// Parses a program and everything it imports, identifying each file by the
/// path `resolve` gives it: the lookup key, and the key that deduplicates
/// repeated imports.
fn load_and_parse_easl_multidocument_with_resolution(
  primary_easl_file_path: &Path,
  mut lookup: impl FnMut(&Path) -> std::io::Result<String>,
  resolve: impl Fn(&Path) -> std::io::Result<PathBuf>,
) -> std::io::Result<
  Result<
    Result<EaslMultiDocument, (EaslMultiDocument, ErrorLog)>,
    EaslMultiDocument,
  >,
> {
  let primary_easl_file_path = resolve(primary_easl_file_path)?;
  let easl_source = lookup(&primary_easl_file_path)?;
  let document = parse_easl_without_comments(&easl_source);
  let mut documents = EaslMultiDocument::from_singular_document(
    document,
    primary_easl_file_path
      .as_os_str()
      .to_str()
      .unwrap()
      .to_string(),
    easl_source,
  );
  if !documents.sources[0].0.parsing_failures.is_empty() {
    return Ok(Err(documents));
  }
  let mut unprocessed_imports: Vec<PathBuf> = vec![];
  let mut encountered_imports: HashSet<PathBuf> = HashSet::new();
  encountered_imports.insert(primary_easl_file_path.clone());
  // Each import form's position and the path it names, resolved to a
  // document index once every document is loaded.
  let mut import_forms: Vec<(DocumentPosition, PathBuf)> = vec![];
  // `(import "path")` or `(import alias "path")`, at a file's top level or
  // inside a `mod` form.
  fn check_as_import_statement(
    ast: &EaslTree,
    current_file_path: &Path,
    resolve: &impl Fn(&Path) -> std::io::Result<PathBuf>,
    unprocessed_imports: &mut Vec<PathBuf>,
    encountered_imports: &mut HashSet<PathBuf>,
    import_forms: &mut Vec<(DocumentPosition, PathBuf)>,
    errors: &mut ErrorLog,
  ) -> Result<(), std::io::Error> {
    let EaslTree::Inner(
      (_, EncloserOrOperator::Encloser(Encloser::Parens)),
      children,
    ) = ast
    else {
      return Ok(());
    };
    let Some(EaslTree::Leaf(_, leaf)) = children.get(0) else {
      return Ok(());
    };
    match leaf.as_str() {
      "mod" => {
        for child in children.iter().skip(2) {
          check_as_import_statement(
            child,
            current_file_path,
            resolve,
            unprocessed_imports,
            encountered_imports,
            import_forms,
            errors,
          )?;
        }
      }
      "import" => {
        let path_tree = match children.len() {
          2 => Some(&children[1]),
          3 if matches!(children[1], EaslTree::Leaf(_, _)) => {
            Some(&children[2])
          }
          _ => None,
        };
        if let Some(EaslTree::Inner(
          (_, EncloserOrOperator::Encloser(Encloser::Quote)),
          string_children,
        )) = path_tree
          && string_children.len() == 1
          && let EaslTree::Leaf(_, import_path_string) = &string_children[0]
        {
          let resolved = if import_path_string.starts_with("/") {
            resolve(&PathBuf::from(import_path_string))
          } else {
            resolve(
              &current_file_path.parent().unwrap().join(import_path_string),
            )
          };
          let Ok(canonicalized_import_path) = resolved else {
            errors.log(CompileError::new(
              CompileErrorKind::ImportNotFound(import_path_string.clone()),
              ast.position().into(),
            ));
            return Ok(());
          };
          import_forms
            .push((ast.position().clone(), canonicalized_import_path.clone()));
          if !encountered_imports.contains(&canonicalized_import_path) {
            encountered_imports.insert(canonicalized_import_path.clone());
            unprocessed_imports.push(canonicalized_import_path);
          }
        } else {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidImportStatement,
            ast.position().into(),
          ));
        }
      }
      _ => {}
    }
    Ok(())
  }
  let mut errors = ErrorLog::new();
  for ast in documents.sources[0].0.syntax_trees.iter() {
    check_as_import_statement(
      ast,
      &primary_easl_file_path,
      &resolve,
      &mut unprocessed_imports,
      &mut encountered_imports,
      &mut import_forms,
      &mut errors,
    )?;
  }
  if !errors.is_empty() {
    record_imports(&mut documents, import_forms);
    return Ok(Ok(Err((documents, errors))));
  }
  while !unprocessed_imports.is_empty() {
    let import_path = unprocessed_imports.remove(0);
    let easl_subsource = lookup(&import_path)?;
    let subdocument = parse_easl_without_comments(&easl_subsource);
    documents.add_document(
      subdocument.clone(),
      import_path.to_str().unwrap().to_string(),
      easl_subsource,
    );
    for ast in documents
      .sources
      .last_mut()
      .unwrap()
      .0
      .syntax_trees
      .iter_mut()
    {
      check_as_import_statement(
        ast,
        &import_path,
        &resolve,
        &mut unprocessed_imports,
        &mut encountered_imports,
        &mut import_forms,
        &mut errors,
      )?;
    }
    if !subdocument.parsing_failures.is_empty() {
      record_imports(&mut documents, import_forms);
      return Ok(Err(documents));
    }
    if !errors.is_empty() {
      record_imports(&mut documents, import_forms);
      return Ok(Ok(Err((documents, errors))));
    }
  }
  record_imports(&mut documents, import_forms);
  Ok(Ok(Ok(documents)))
}

/// Records which loaded document each import form loads, in
/// `documents.imports`. Forms naming documents that weren't loaded (loading
/// stopped at an error first) are left out.
fn record_imports(
  documents: &mut EaslMultiDocument,
  import_forms: Vec<(DocumentPosition, PathBuf)>,
) {
  let document_indices: HashMap<PathBuf, usize> = documents
    .sources
    .iter()
    .enumerate()
    .map(|(i, (_, path, _))| (PathBuf::from(path), i))
    .collect();
  documents.imports = import_forms
    .into_iter()
    .filter_map(|(position, path)| {
      document_indices.get(&path).map(|index| (position, *index))
    })
    .collect();
}

pub fn load_and_parse_easl_multidocument(
  primary_easl_file_path: &Path,
) -> std::io::Result<
  Result<
    Result<EaslMultiDocument, (EaslMultiDocument, ErrorLog)>,
    EaslMultiDocument,
  >,
> {
  load_and_parse_easl_multidocument_with_lookup_function(
    primary_easl_file_path,
    |path| std::fs::read_to_string(path),
  )
}
