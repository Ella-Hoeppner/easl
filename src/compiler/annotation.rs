use std::fmt::Display;
use std::rc::Rc;
use std::str::FromStr;

use sse::syntax::EncloserOrOperator::{self, *};

use crate::compiler::error::CompileErrorKind;
use crate::compiler::util::compile_word;
use crate::compiler::vars::{GroupAndBinding, VariableAddressSpace};
use crate::parse::EaslTree;
use crate::parse::{Encloser::*, Operator};

use super::error::{
  CompileError, CompileErrorKind::*, CompileResult, ErrorLog, SourceTrace, err,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Annotation {
  Singular(Rc<str>),
  Map(Vec<(Rc<str>, Rc<str>)>),
  Multiple(Vec<Self>),
}

impl Display for Annotation {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Annotation::Singular(s) => write!(f, "{}", s),
      Annotation::Map(items) => {
        write!(f, "{{\n")?;
        for (key, val) in items {
          write!(f, "  {} {}\n", key, val)?;
        }
        write!(f, "\n}}")
      }
      Annotation::Multiple(sub_annotations) => {
        for annotation in sub_annotations.iter() {
          annotation.fmt(f).unwrap();
          write!(f, "\n")?
        }
        Ok(())
      }
    }
  }
}

impl Annotation {
  pub fn from_annotation_tree(ast: EaslTree) -> CompileResult<Self> {
    match ast {
      EaslTree::Leaf(_, singular) => Ok(Self::Singular(singular.into())),
      EaslTree::Inner((position, Encloser(Curly)), map_fields) => {
        let source_trace: SourceTrace = position.into();
        if map_fields.len() % 2 == 0 {
          Ok(Self::Map(
            map_fields
              .into_iter()
              .map(|field| {
                if let EaslTree::Leaf(_, field_string) = field {
                  Ok(field_string.into())
                } else {
                  err(
                    InvalidAnnotation("fields must all be leaves".into()),
                    source_trace.clone(),
                  )
                }
              })
              .collect::<CompileResult<Vec<Rc<str>>>>()?
              .chunks(2)
              .map(|arr| (arr[0].clone(), arr[1].clone()))
              .collect(),
          ))
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
      .fold(String::new(), |s, (name, value)| {
        if let Some(value) = value {
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
  pub fn properties(&self) -> Vec<(Rc<str>, Option<Rc<str>>)> {
    match self {
      Annotation::Singular(s) => vec![(s.clone(), None)],
      Annotation::Map(items) => items
        .iter()
        .cloned()
        .map(|(property, value)| (property, Some(value)))
        .collect(),
      Annotation::Multiple(sub_annotations) => sub_annotations
        .into_iter()
        .map(Self::properties)
        .reduce(|mut a, mut b| {
          a.append(&mut b);
          a
        })
        .unwrap_or(vec![]),
    }
  }
  pub fn into_top_level_var_data(
    &self,
    source_trace: &SourceTrace,
  ) -> CompileResult<(Option<GroupAndBinding>, Option<VariableAddressSpace>)>
  {
    let mut group = None;
    let mut binding = None;
    let mut address_space = None;
    for (property, value) in self.properties().into_iter() {
      match (&*property, value) {
        ("group", Some(value)) => match u8::from_str(&*value) {
          Ok(value) => group = Some(value),
          Err(_) => {
            return err(
              InvalidVariableAnnotation(self.clone().into()),
              source_trace.clone(),
            );
          }
        },
        ("binding", Some(value)) => match u8::from_str(&*value) {
          Ok(value) => binding = Some(value),
          Err(_) => {
            return err(
              InvalidVariableAnnotation(self.clone().into()),
              source_trace.clone(),
            );
          }
        },
        ("address", Some(value)) => {
          match VariableAddressSpace::from_str(&*value) {
            Some(a) => address_space = Some(a),
            None => {
              return err(
                InvalidVariableAnnotation(self.clone().into()),
                source_trace.clone(),
              );
            }
          }
        }
        _ => {
          return err(
            InvalidVariableAnnotation(self.clone().into()),
            source_trace.clone(),
          );
        }
      }
    }
    Ok((
      match (group, binding) {
        (Some(_), None) => {
          return err(
            CompileErrorKind::GroupMissingBinding,
            source_trace.clone(),
          );
        }
        (None, Some(_)) => {
          return err(
            CompileErrorKind::BindingMissingGroup,
            source_trace.clone(),
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
  pub fn validate_for_top_level_function(
    &self,
    source_trace: &SourceTrace,
  ) -> CompileResult<bool> {
    let mut is_associative = false;
    for (property, value) in self.properties().into_iter() {
      match (&*property, value) {
        ("associative", None) => {
          is_associative = true;
        }
        ("workgroup-size", _) => {}
        ("fragment" | "vertex" | "compute", None) => {}
        ("entry", Some(entry_type))
          if (*entry_type == *"fragment"
            || *entry_type == *"vertex"
            || *entry_type == *"compute") => {}
        _ => {
          return Err(CompileError::new(
            InvalidFunctionAnnotation(self.clone().into()),
            source_trace.clone(),
          ));
        }
      }
    }
    Ok(is_associative)
  }
}

pub fn extract_annotation(
  exp: EaslTree,
  errors: &mut ErrorLog,
) -> (EaslTree, Option<(Annotation, SourceTrace)>) {
  if let EaslTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::Annotation)),
    mut children,
  ) = exp
  {
    let annotation_tree = children.remove(0);
    let annotation_source_trace: SourceTrace =
      annotation_tree.position().into();
    let exp = children.remove(0);
    match Annotation::from_annotation_tree(annotation_tree) {
      Ok(annotation) => {
        let (exp, inner_annotation) = extract_annotation(exp, errors);
        if let Some((inner_annotation, inner_annotation_source_trace)) =
          inner_annotation
        {
          (
            exp,
            Some((
              Annotation::Multiple(vec![annotation, inner_annotation]),
              annotation_source_trace
                .insert_as_secondary(inner_annotation_source_trace),
            )),
          )
        } else {
          (exp, Some((annotation, annotation_source_trace)))
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
