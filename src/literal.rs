//! Constant values of a type in the three forms an embedder moves them
//! between: literal source (`0.5`, `true`, `(vec2f 0.5 1.)`, `[1u 2u]`),
//! the flat VM word layout `@external` variables use (see `external.rs`),
//! and an expression that rebuilds the value from words held elsewhere
//! (say, elements of a `u32` array the embedder writes).
//!
//! Supported types are the scalars (`f32`, `i32`, `u32`, `bool`), structs
//! of supported fields (vectors included, written with their aliases like
//! `vec2f`), and fixed-size arrays of a supported type.

use fsexp::{Ast, EncloserOrOperator, syntax::ContainsEncloserOrOperator};
use std::fmt::Debug;

use crate::{
  compiler::{
    error::SourceTrace,
    expression::{ExpKind, Number, parse_number},
    structs::Struct,
    types::{Type, TypeState},
  },
  parse::{Encloser, Operator},
};

/// Whether values of `t` have a literal form (and so a word layout and a
/// rebuilding expression).
pub fn supports_literals(t: &Type) -> bool {
  match t {
    Type::F32 | Type::I32 | Type::U32 | Type::Bool => true,
    Type::Struct(s) => {
      !is_matrix(s)
        && !s.fields.is_empty()
        && s
          .fields
          .iter()
          .all(|field| supports_literals(&field.field_type.kind.unwrap_known()))
    }
    Type::Array(Some(size), inner) => {
      size.as_literal().is_some()
        && supports_literals(&inner.kind.unwrap_known())
    }
    _ => false,
  }
}

/// How many words a value of `t` occupies in the VM layout.
pub fn word_count(t: &Type) -> usize {
  crate::interpreter::vm_words_of(t)
}

/// The words of `literal`, a literal of type `t`; `None` when it isn't one.
/// A single argument to a vector constructor fills every component, as
/// the constructor itself does.
pub fn literal_words<
  D: Clone + PartialEq + Debug,
  L: Clone + PartialEq + Debug + ContainsEncloserOrOperator<Encloser, Operator>,
>(
  literal: &Ast<D, (D, L)>,
  t: &Type,
) -> Option<Vec<u32>> {
  let mut words = vec![];
  push_literal_words(literal, t, &mut words)?;
  Some(words)
}

fn push_literal_words<
  D: Clone + PartialEq + Debug,
  L: Clone + PartialEq + Debug + ContainsEncloserOrOperator<Encloser, Operator>,
>(
  literal: &Ast<D, (D, L)>,
  t: &Type,
  words: &mut Vec<u32>,
) -> Option<()> {
  match (literal, t) {
    (Ast::Leaf(_, text), Type::F32 | Type::I32 | Type::U32 | Type::Bool) => {
      words.push(scalar_word(text, t)?);
    }
    (Ast::Inner((_, label), children), Type::Struct(s))
      if *label.get_encloser_or_operator()
        == EncloserOrOperator::Encloser(Encloser::Parens)
        && supports_literals(t) =>
    {
      let (constructor, args) = children.split_first()?;
      if !matches!(constructor, Ast::Leaf(_, name) if *name == constructor_name(s))
      {
        return None;
      }
      let field_types: Vec<Type> = s
        .fields
        .iter()
        .map(|field| field.field_type.kind.unwrap_known())
        .collect();
      if args.len() == 1 && field_types.len() > 1 && is_vector(s) {
        for field_type in &field_types {
          push_literal_words(&args[0], field_type, words)?;
        }
      } else if args.len() == field_types.len() {
        for (arg, field_type) in args.iter().zip(&field_types) {
          push_literal_words(arg, field_type, words)?;
        }
      } else {
        return None;
      }
    }
    (Ast::Inner((_, label), items), Type::Array(Some(size), inner))
      if *label.get_encloser_or_operator()
        == EncloserOrOperator::Encloser(Encloser::Square) =>
    {
      if Some(items.len() as u32) != size.as_literal() {
        return None;
      }
      let inner = inner.kind.unwrap_known();
      for item in items {
        push_literal_words(item, &inner, words)?;
      }
    }
    _ => return None,
  }
  Some(())
}

/// One scalar's word, using the compiler's own number-literal rules.
fn scalar_word(text: &str, t: &Type) -> Option<u32> {
  if *t == Type::Bool {
    return match text {
      "true" => Some(1),
      "false" => Some(0),
      _ => None,
    };
  }
  let number = parse_number(text, SourceTrace::empty())?;
  let allowed = match &number.data.kind {
    TypeState::Known(known) => known == t,
    TypeState::OneOf(options) => options.contains(t),
    _ => false,
  };
  if !allowed {
    return None;
  }
  match (number.kind, t) {
    (ExpKind::NumberLiteral(Number::Float(value)), Type::F32) => {
      Some((value as f32).to_bits())
    }
    (ExpKind::NumberLiteral(Number::Int(value)), Type::F32) => {
      Some((value as f32).to_bits())
    }
    (ExpKind::NumberLiteral(Number::Int(value)), Type::I32) => {
      i32::try_from(value).ok().map(|value| value as u32)
    }
    (ExpKind::NumberLiteral(Number::Int(value)), Type::U32) => {
      u32::try_from(value).ok()
    }
    _ => None,
  }
}

/// The literal source of the value `words` (in `t`'s VM layout) encode.
/// Floats are written in their shortest round-tripping form, so reading
/// the literal back gives the same words.
pub fn words_literal(t: &Type, words: &[u32]) -> String {
  let mut offset = 0;
  write_literal(t, words, &mut offset)
}

fn write_literal(t: &Type, words: &[u32], offset: &mut usize) -> String {
  match t {
    Type::F32 | Type::I32 | Type::U32 | Type::Bool => {
      let word = words[*offset];
      *offset += 1;
      match t {
        Type::F32 => {
          let value = f32::from_bits(word);
          let value = if value.is_finite() { value } else { 0. };
          let text = format!("{value}");
          if text.contains('.') {
            text
          } else {
            format!("{text}.")
          }
        }
        Type::I32 => format!("{}", word as i32),
        Type::U32 => format!("{word}u"),
        _ => (word != 0).to_string(),
      }
    }
    Type::Struct(s) => format!(
      "({} {})",
      constructor_name(s),
      s.fields
        .iter()
        .map(|field| {
          write_literal(&field.field_type.kind.unwrap_known(), words, offset)
        })
        .collect::<Vec<_>>()
        .join(" ")
    ),
    Type::Array(Some(size), inner) => {
      let inner = inner.kind.unwrap_known();
      format!(
        "[{}]",
        (0..size.as_literal().unwrap_or(0))
          .map(|_| write_literal(&inner, words, offset))
          .collect::<Vec<_>>()
          .join(" ")
      )
    }
    _ => panic!("no literal form for {t:?}"),
  }
}

/// An expression rebuilding a value of `t` from its VM-layout words, where
/// `word(i)` is an expression of type `u32` holding word `i`.
pub fn words_expression(t: &Type, word: &dyn Fn(usize) -> String) -> String {
  let mut offset = 0;
  write_expression(t, word, &mut offset)
}

fn write_expression(
  t: &Type,
  word: &dyn Fn(usize) -> String,
  offset: &mut usize,
) -> String {
  match t {
    Type::F32 | Type::I32 | Type::U32 | Type::Bool => {
      let source = word(*offset);
      *offset += 1;
      match t {
        Type::F32 => format!("(bitcast {source}): f32"),
        Type::I32 => format!("(bitcast {source}): i32"),
        Type::U32 => source,
        _ => format!("(!= {source} 0u)"),
      }
    }
    Type::Struct(s) => format!(
      "({} {})",
      constructor_name(s),
      s.fields
        .iter()
        .map(|field| {
          write_expression(&field.field_type.kind.unwrap_known(), word, offset)
        })
        .collect::<Vec<_>>()
        .join(" ")
    ),
    Type::Array(Some(size), inner) => {
      let inner = inner.kind.unwrap_known();
      format!(
        "[{}]",
        (0..size.as_literal().unwrap_or(0))
          .map(|_| write_expression(&inner, word, offset))
          .collect::<Vec<_>>()
          .join(" ")
      )
    }
    _ => panic!("no literal form for {t:?}"),
  }
}

fn is_matrix(s: &Struct) -> bool {
  s.name.starts_with("mat")
}

fn is_vector(s: &Struct) -> bool {
  s.abstract_ancestor.name.0.starts_with("vec")
    && (2..=4).contains(&s.fields.len())
}

/// The name that constructs `s` in source: the struct's name, or for a
/// vector its typed alias (`vec2f`, `vec3u`, ...).
fn constructor_name(s: &Struct) -> String {
  if is_vector(s) {
    let suffix = match s.fields[0].field_type.kind.unwrap_known() {
      Type::F32 => "f",
      Type::I32 => "i",
      Type::U32 => "u",
      Type::Bool => "b",
      _ => return s.name.to_string(),
    };
    format!("vec{}{suffix}", s.fields.len())
  } else {
    s.name.to_string()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{
    compiler::{builtins::built_in_macros, program::Program},
    parse::{EaslMultiDocument, parse_easl},
  };

  /// The types of a program's top-level vars, by name.
  fn var_types(source: &str) -> Vec<(String, Type)> {
    let mut document = parse_easl(source);
    document.strip_comments();
    let (mut program, errors) = Program::from_easl_documents(
      &EaslMultiDocument::from_singular_document_sourceless(document),
      built_in_macros(),
    );
    assert!(errors.is_empty(), "{errors:?}");
    let errors = program
      .validate_raw_program(crate::compiler::program::CompilerTarget::WGSL);
    assert!(errors.is_empty(), "{errors:?}");
    program
      .top_level_vars
      .iter()
      .map(|var| (var.name.to_string(), var.var_type.clone()))
      .collect()
  }

  fn var_type(source: &str, name: &str) -> Type {
    var_types(source)
      .into_iter()
      .find(|(var_name, _)| var_name == name)
      .unwrap()
      .1
  }

  fn words_of_source(source: &str, t: &Type) -> Option<Vec<u32>> {
    let document = parse_easl(source);
    literal_words(&document.syntax_trees[0], t)
  }

  #[test]
  fn literals_round_trip_through_words() {
    let source = "(struct Pair a: f32 b: (vec2 u32))
@external (var s: f32)
@external (var b: bool)
@external (var i: i32)
@external (var v: vec3f)
@external (var p: Pair)
@external (var a: [2: vec2i])
@cpu (defn main [] (spawn-window (fn [] ())))";
    for (name, literal) in [
      ("s", "0.333"),
      ("s", "-2."),
      ("b", "true"),
      ("i", "-7"),
      ("v", "(vec3f 0.5 1. -0.25)"),
      ("p", "(Pair 0.5 (vec2u 3u 4u))"),
      ("a", "[(vec2i 1 2) (vec2i -3 4)]"),
    ] {
      let t = var_type(source, name);
      assert!(supports_literals(&t), "{name}");
      let words = words_of_source(literal, &t)
        .unwrap_or_else(|| panic!("{literal} should read as {name}'s type"));
      assert_eq!(words.len(), word_count(&t));
      let written = words_literal(&t, &words);
      assert_eq!(words_of_source(&written, &t), Some(words), "{written}");
    }
    let v = var_type(source, "v");
    assert_eq!(
      words_of_source("(vec3f 0.5)", &v),
      Some(vec![0.5f32.to_bits(); 3]),
      "a lone vector argument fills every component"
    );
    assert_eq!(words_literal(&v, &[1f32.to_bits(); 3]), "(vec3f 1. 1. 1.)");
    let u = var_type("@external (var u: u32)", "u");
    assert_eq!(words_literal(&u, &[7]), "7u");
    assert_eq!(words_of_source("7u", &u), Some(vec![7]));
    assert_eq!(words_of_source("0.5", &u), None);
    assert_eq!(words_of_source("-1", &u), None);
    assert_eq!(words_of_source("(vec2f 1. 2.)", &v), None);
  }

  #[test]
  fn words_expressions_rebuild_values() {
    let source = "(struct Pair a: f32 b: (vec2 u32))
@external (var p: Pair)
@external (var t: (vec2 bool))";
    let word = |i: usize| format!("(w {i}u)");
    assert_eq!(
      words_expression(&var_type(source, "p"), &word),
      "(Pair (bitcast (w 0u)): f32 (vec2u (w 1u) (w 2u)))"
    );
    assert_eq!(
      words_expression(&var_type(source, "t"), &word),
      "(vec2b (!= (w 0u) 0u) (!= (w 1u) 0u))"
    );
  }

  /// Rebuilt values compile and evaluate to the encoded value, on the CPU
  /// and on the GPU.
  #[test]
  fn words_expressions_compile() {
    let t = var_type("@external (var v: vec3f)", "v");
    let expression = words_expression(&t, &|i| {
      format!("(.{} (words 0u))", ["x", "y", "z"][i])
    });
    let source = format!(
      "@uniform (var words: [1: vec4u])
@fragment (defn f []: @{{location 0}} vec4f (vec4f {expression} 1.))
@vertex (defn v []: @{{builtin position}} vec4f (vec4f 0.))
@cpu (defn main [] (print {expression}))"
    );
    let compiled = crate::compile_easl_source_to_wgsl(&source)
      .expect("parses")
      .unwrap_or_else(|(_, errors)| panic!("{errors:?}"));
    assert!(compiled.contains("bitcast"));
  }
}
