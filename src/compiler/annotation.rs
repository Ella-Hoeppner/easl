use std::fmt::Display;
use std::rc::Rc;
use std::str::FromStr;

use fsexp::syntax::EncloserOrOperator::{self, *};

use crate::compiler::entry::EntryPoint;
use crate::compiler::error::CompileErrorKind;
use crate::compiler::util::compile_word;
use crate::compiler::vars::{GroupAndBinding, VariableAddressSpace};
use crate::parse::EaslTree;
use crate::parse::{Encloser::*, Operator};

use super::error::{
  CompileError, CompileErrorKind::*, CompileResult, ErrorLog, SourceTrace, err,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnnotationKind {
  Singular(Rc<str>, SourceTrace),
  Map(Vec<(Rc<str>, SourceTrace, Rc<str>, SourceTrace)>),
  Multiple(Vec<Annotation>),
}

impl Display for AnnotationKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      AnnotationKind::Singular(s, _) => write!(f, "{}", s),
      AnnotationKind::Map(items) => {
        write!(f, "{{\n")?;
        for (key, _, val, _) in items {
          write!(f, "  {} {}\n", key, val)?;
        }
        write!(f, "\n}}")
      }
      AnnotationKind::Multiple(sub_annotations) => {
        for annotation in sub_annotations.iter() {
          annotation.fmt(f).unwrap();
          write!(f, "\n")?
        }
        Ok(())
      }
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annotation {
  pub kind: AnnotationKind,
  pub source_trace: SourceTrace,
}

impl Display for Annotation {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.kind.fmt(f)
  }
}

impl Annotation {
  pub fn from_annotation_tree(ast: EaslTree) -> CompileResult<Self> {
    match ast {
      EaslTree::Leaf(pos, singular) => {
        let source_trace: SourceTrace = pos.into();
        Ok(Self {
          kind: AnnotationKind::Singular(singular.into(), source_trace.clone()),
          source_trace: source_trace,
        })
      }
      EaslTree::Inner((pos, Encloser(Curly)), map_fields) => {
        let source_trace: SourceTrace = pos.into();
        if map_fields.len() % 2 == 0 {
          Ok(Self {
            kind: AnnotationKind::Map(
              map_fields
                .into_iter()
                .map(|field| {
                  if let EaslTree::Leaf(pos, field_string) = field {
                    Ok((field_string.into(), pos.into()))
                  } else {
                    err(
                      InvalidAnnotation("fields must all be leaves".into()),
                      source_trace.clone(),
                    )
                  }
                })
                .collect::<CompileResult<Vec<(Rc<str>, SourceTrace)>>>()?
                .chunks(2)
                .map(|arr| {
                  (
                    arr[0].0.clone(),
                    arr[0].1.clone(),
                    arr[1].0.clone(),
                    arr[1].1.clone(),
                  )
                })
                .collect(),
            ),
            source_trace,
          })
        } else {
          err(
            InvalidAnnotation("fields must all be leaves".into()),
            source_trace,
          )
        }
      }
      _ => err(
        InvalidAnnotation("fields must all be leaves".into()),
        ast.position().clone().into(),
      ),
    }
  }
  pub fn compile(self) -> String {
    self
      .properties()
      .into_iter()
      .fold(String::new(), |s, (name, _, value)| {
        if let Some((value, _)) = value {
          s + &format!("@{}({})", compile_word(name), compile_word(value))
        } else {
          match &*name {
            "vertex" | "fragment" | "compute" => s + &format!("@{name} "),
            _ => s,
          }
        }
      })
  }
  pub fn compile_optional(maybe_self: Option<Self>) -> String {
    if let Some(annotation) = maybe_self {
      annotation.compile()
    } else {
      String::new()
    }
  }
  pub fn properties(
    &self,
  ) -> Vec<(Rc<str>, SourceTrace, Option<(Rc<str>, SourceTrace)>)> {
    match &self.kind {
      AnnotationKind::Singular(name, source_trace) => {
        vec![(name.clone(), source_trace.clone(), None)]
      }
      AnnotationKind::Map(items) => items
        .iter()
        .cloned()
        .map(|(name, name_source, value, value_source)| {
          (name, name_source, Some((value, value_source)))
        })
        .collect(),
      AnnotationKind::Multiple(sub_annotations) => sub_annotations
        .into_iter()
        .map(Self::properties)
        .reduce(|mut a, mut b| {
          a.append(&mut b);
          a
        })
        .unwrap_or(vec![]),
    }
  }
  pub fn validate_as_top_level_var_data(
    &self,
  ) -> CompileResult<(Option<GroupAndBinding>, Option<VariableAddressSpace>)>
  {
    let mut group = None;
    let mut binding = None;
    let mut address_space = None;
    for (name, name_source, value) in self.properties().into_iter() {
      match (&*name, value) {
        ("group", Some((value, value_source))) => match u8::from_str(&*value) {
          Ok(value) => group = Some(value),
          Err(_) => {
            return err(
              InvalidVariableAnnotation(self.clone().into()),
              value_source,
            );
          }
        },
        ("binding", Some((value, value_source))) => match u8::from_str(&*value)
        {
          Ok(value) => binding = Some(value),
          Err(_) => {
            return err(
              InvalidVariableAnnotation(self.clone().into()),
              value_source,
            );
          }
        },
        ("address", Some((value, value_source))) => {
          match VariableAddressSpace::from_str(&*value) {
            Some(a) => address_space = Some(a),
            None => {
              return err(
                InvalidVariableAnnotation(self.clone().into()),
                value_source,
              );
            }
          }
        }
        _ => {
          return err(
            InvalidVariableAnnotation(self.clone().into()),
            name_source,
          );
        }
      }
    }
    Ok((
      match (group, binding) {
        (Some(_), None) => {
          return err(
            CompileErrorKind::GroupMissingBinding,
            self.source_trace.clone(),
          );
        }
        (None, Some(_)) => {
          return err(
            CompileErrorKind::BindingMissingGroup,
            self.source_trace.clone(),
          );
        }
        (None, None) => None,
        (Some(group), Some(binding)) => {
          Some(GroupAndBinding { group, binding })
        }
      },
      address_space,
    ))
  }
  pub(crate) fn validate_as_function_annotation(
    &self,
  ) -> CompileResult<FunctionAnnotation> {
    let mut parsed_annotation = FunctionAnnotation {
      associative: false,
      entry: None,
    };
    let mut workgroup_size: Option<usize> = None;
    for (name, name_source, value) in self.properties().into_iter() {
      match (&*name, value) {
        ("associative", None) => {
          parsed_annotation.associative = true;
        }
        ("workgroup-size", Some((size_str, size_source))) => {
          if let Ok(size) = size_str.parse::<usize>() {
            workgroup_size = Some(size);
          } else {
            return Err(CompileError::new(
              InvalidWorkgroupSize(size_str.to_string()),
              size_source,
            ));
          }
        }
        ("fragment" | "vertex" | "compute" | "cpu", None) => {
          if parsed_annotation.entry.is_none() {
            parsed_annotation.entry = Some(match &*name {
              "fragment" => EntryPoint::Fragment,
              "vertex" => EntryPoint::Vertex,
              "compute" => EntryPoint::Compute(0),
              "cpu" => EntryPoint::Cpu,
              _ => unreachable!(),
            })
          } else {
            return Err(CompileError::new(
              ConflictingEntryPointAnnotations,
              name_source,
            ));
          }
        }
        _ => {
          return Err(CompileError::new(
            InvalidFunctionAnnotation(self.clone().into()),
            name_source,
          ));
        }
      }
    }
    if let Some(workgroup_size) = workgroup_size {
      match parsed_annotation.entry {
        Some(EntryPoint::Compute(_)) | None => {
          parsed_annotation.entry = Some(EntryPoint::Compute(workgroup_size))
        }
        _ => {
          return Err(CompileError::new(
            InvalidWorkgroupSizeAnnotation,
            self.source_trace.clone(),
          ));
        }
      }
    } else {
      if let Some(EntryPoint::Compute(_)) = parsed_annotation.entry {
        return Err(CompileError::new(
          ComputeEntryMissingWorkgroupSize,
          self.source_trace.clone(),
        ));
      }
    }
    Ok(parsed_annotation)
  }
}

pub(crate) struct FunctionAnnotation {
  pub(crate) associative: bool,
  pub(crate) entry: Option<EntryPoint>,
}
impl Default for FunctionAnnotation {
  fn default() -> Self {
    Self {
      associative: false,
      entry: None,
    }
  }
}

pub fn extract_annotation(
  exp: EaslTree,
  errors: &mut ErrorLog,
) -> (EaslTree, Option<Annotation>) {
  if let EaslTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::Annotation)),
    mut children,
  ) = exp
  {
    let annotation_tree = children.remove(0);
    let exp = children.remove(0);
    match Annotation::from_annotation_tree(annotation_tree) {
      Ok(annotation) => {
        let (exp, inner_annotation) = extract_annotation(exp, errors);
        if let Some(inner_annotation) = inner_annotation {
          (
            exp,
            Some(Annotation {
              source_trace: annotation
                .source_trace
                .clone()
                .insert_as_secondary(inner_annotation.source_trace.clone()),
              kind: AnnotationKind::Multiple(vec![
                annotation,
                inner_annotation,
              ]),
            }),
          )
        } else {
          (exp, Some(annotation))
        }
      }
      Err(err) => {
        errors.log(err);
        (exp, None)
      }
    }
  } else {
    (exp, None)
  }
}
