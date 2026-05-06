use std::path::Path;

use fsexp::ParseError;

use crate::{
  compiler::{info::ProgramInfo, program::EaslDocument},
  parse::{
    EaslMultiDocument, load_and_parse_easl_multidocument,
    parse_easl_without_comments,
  },
};

use super::{builtins::built_in_macros, error::ErrorLog, program::Program};

pub fn compile_easl_documents_to_wgsl(
  documents: EaslMultiDocument,
) -> Result<String, (EaslMultiDocument, ErrorLog)> {
  let (mut program, errors) =
    Program::from_easl_documents(&documents, built_in_macros());
  if !errors.is_empty() {
    return Err((documents, errors));
  }
  let errors = program.validate_raw_program();
  if !errors.is_empty() {
    return Err((documents, errors));
  }

  program
    .compile_to_wgsl()
    .map_err(|e| (documents, ErrorLog::from(e)))
}

pub fn compile_easl_document_to_wgsl(
  document: EaslDocument,
) -> Result<String, (EaslMultiDocument, ErrorLog)> {
  compile_easl_documents_to_wgsl(
    EaslMultiDocument::from_singular_document_sourceless(document),
  )
}

pub fn compile_easl_source_to_wgsl(
  easl_source: &str,
) -> Result<Result<String, (EaslMultiDocument, ErrorLog)>, EaslMultiDocument> {
  let document = parse_easl_without_comments(easl_source);
  if !document.parsing_failures.is_empty() {
    return Err(EaslMultiDocument::from_singular_document(
      document,
      String::new(),
      easl_source.to_string(),
    ));
  }
  Ok(compile_easl_documents_to_wgsl(
    EaslMultiDocument::from_singular_document(
      document,
      String::new(),
      easl_source.to_string(),
    ),
  ))
}

pub fn compile_easl_file_to_wgsl(
  primary_easl_file_path: &Path,
) -> std::io::Result<
  Result<Result<String, (EaslMultiDocument, ErrorLog)>, EaslMultiDocument>,
> {
  match load_easl_program_from_file(primary_easl_file_path)? {
    Ok((documents, Ok(mut program))) => {
      let errors = program.validate_raw_program();
      if !errors.is_empty() {
        return Ok(Ok(Err((documents, errors))));
      }
      Ok(Ok(
        program
          .compile_to_wgsl()
          .map_err(|e| (documents, ErrorLog::from(e))),
      ))
    }
    Ok((documents, Err(errors))) => Ok(Ok(Err((documents, errors)))),
    Err(e) => Ok(Err(e)),
  }
}

pub fn load_easl_program_from_file(
  primary_easl_file_path: &Path,
) -> std::io::Result<
  Result<(EaslMultiDocument, Result<Program, ErrorLog>), EaslMultiDocument>,
> {
  match load_and_parse_easl_multidocument(primary_easl_file_path)? {
    Ok(Ok(docs)) => {
      let (program, errors) =
        Program::from_easl_documents(&docs, built_in_macros());
      Ok(Ok((
        docs,
        if errors.is_empty() {
          Ok(program)
        } else {
          Err(errors)
        },
      )))
    }
    Ok(Err((doc, e))) => Ok(Ok((doc, Err(e)))),
    Err(e) => Ok(Err(e)),
  }
}

pub fn get_easl_program_info(
  easl_source: &str,
) -> Result<Result<ProgramInfo, ErrorLog>, Vec<ParseError>> {
  let document = parse_easl_without_comments(easl_source);
  if !document.parsing_failures.is_empty() {
    return Err(document.parsing_failures);
  }
  let (mut program, _) = Program::from_easl_documents(
    &EaslMultiDocument::from_singular_document(
      document,
      String::new(),
      easl_source.to_string(),
    ),
    built_in_macros(),
  );
  let errors = program.validate_raw_program();
  Ok(if errors.is_empty() {
    Ok(ProgramInfo::from(&program))
  } else {
    Err(errors)
  })
}
