use std::{
  collections::HashSet,
  path::{Path, PathBuf},
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
}
impl SSEOperator for Operator {
  fn left_args(&self) -> usize {
    match self {
      Operator::Annotation => 0,
      Operator::TypeAscription => 1,
      Operator::ExpressionComment => 0,
    }
  }

  fn right_args(&self) -> usize {
    match self {
      Operator::Annotation => 2,
      Operator::TypeAscription => 1,
      Operator::ExpressionComment => 1,
    }
  }

  fn op_str(&self) -> &str {
    match self {
      Operator::Annotation => "@",
      Operator::TypeAscription => ":",
      Operator::ExpressionComment => "#_",
    }
  }
}

static DEFAULT_CTX: LazyLock<SSEContext<Encloser, Operator>> =
  LazyLock::new(|| {
    SSEContext::new(
      vec![
        Encloser::Parens,
        Encloser::Square,
        Encloser::Curly,
        Encloser::LineComment,
        Encloser::BlockComment,
        Encloser::Quote,
      ],
      vec![
        Operator::Annotation,
        Operator::TypeAscription,
        Operator::ExpressionComment,
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
}

impl EaslMultiDocument {
  fn empty() -> Self {
    Self { sources: vec![] }
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
  pub fn describe_parse_error(&self, err: ParseError) -> String {
    let (source_document, _, source_text) = self.sources.last().unwrap();
    err.describe(source_document, source_text)
  }
}

pub fn load_and_parse_easl_multidocument_with_lookup_function(
  primary_easl_file_path: &Path,
  mut lookup: impl FnMut(&Path) -> std::io::Result<String>,
) -> std::io::Result<
  Result<
    Result<EaslMultiDocument, (EaslMultiDocument, ErrorLog)>,
    EaslMultiDocument,
  >,
> {
  let primary_easl_file_path = primary_easl_file_path.canonicalize()?;
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
  let check_as_import_statement = |ast: &EaslTree,
                                   current_file_path: &Path,
                                   unprocessed_imports: &mut Vec<PathBuf>,
                                   encountered_imports: &mut HashSet<
    PathBuf,
  >,
                                   errors: &mut ErrorLog|
   -> Result<(), std::io::Error> {
    if let EaslTree::Inner(
      (_, EncloserOrOperator::Encloser(Encloser::Parens)),
      children,
    ) = ast
      && let Some(first_child) = children.get(0)
      && let EaslTree::Leaf(_, leaf) = first_child
      && leaf == "import"
    {
      if children.len() == 2
        && let EaslTree::Inner(
          (_, EncloserOrOperator::Encloser(Encloser::Quote)),
          string_children,
        ) = &children[1]
        && string_children.len() == 1
        && let EaslTree::Leaf(_, import_path_string) =
          string_children[0].clone()
      {
        let canonicalized_import_path_string =
          if import_path_string.starts_with("/") {
            PathBuf::from(import_path_string).canonicalize()?
          } else {
            current_file_path
              .parent()
              .unwrap()
              .join(import_path_string)
              .canonicalize()?
          };

        if !encountered_imports.contains(&canonicalized_import_path_string) {
          encountered_imports.insert(canonicalized_import_path_string.clone());
          unprocessed_imports.push(canonicalized_import_path_string);
        }
      } else {
        errors.log(CompileError::new(
          CompileErrorKind::InvalidImportStatement,
          ast.position().into(),
        ));
      }
    }
    Ok(())
  };
  let mut errors = ErrorLog::new();
  for ast in documents.sources[0].0.syntax_trees.iter() {
    check_as_import_statement(
      ast,
      &primary_easl_file_path,
      &mut unprocessed_imports,
      &mut encountered_imports,
      &mut errors,
    )?;
  }
  if !errors.is_empty() {
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
        &mut unprocessed_imports,
        &mut encountered_imports,
        &mut errors,
      )?;
    }
    if !subdocument.parsing_failures.is_empty() {
      return Ok(Err(documents));
    }
    if !errors.is_empty() {
      return Ok(Ok(Err((documents, errors))));
    }
  }
  Ok(Ok(Ok(documents)))
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
