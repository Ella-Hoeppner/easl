use std::rc::Rc;

use fsexp::syntax::EncloserOrOperator;
use unicode_segmentation::UnicodeSegmentation;

use crate::{
  compiler::wgsl::is_wgsl_reserved_word,
  parse::{EaslTree, Operator},
};

use super::error::{CompileErrorKind::*, CompileResult, err};

pub fn indent(s: String) -> String {
  s.replace("\n", "\n  ")
}

pub fn read_leaf(ast: EaslTree) -> CompileResult<Rc<str>> {
  if let EaslTree::Leaf(_, word) = ast {
    Ok(word.into())
  } else {
    err(ExpectedLeaf, ast.position().into())
  }
}

pub fn read_type_annotated_name(
  ast: EaslTree,
) -> CompileResult<(Rc<str>, EaslTree)> {
  if let EaslTree::Inner(
    (position, EncloserOrOperator::Operator(Operator::TypeAscription)),
    mut children,
  ) = ast
  {
    let type_ast = children.remove(1);
    let name_ast = children.remove(0);
    if let EaslTree::Leaf(_, name) = name_ast {
      Ok((name.into(), type_ast))
    } else {
      err(ExpectedTypeAnnotatedName, position.into())
    }
  } else {
    err(ExpectedTypeAnnotatedName, ast.position().clone().into())
  }
}

pub fn compile_word(word: Rc<str>) -> String {
  let compiled_word = match &*word {
    "vec2<bool>"
    | "vec3<bool>"
    | "vec4<bool>"
    | "bitcast<f32>"
    | "bitcast<i32>"
    | "bitcast<u32>"
    | "bitcast<vec2f>"
    | "bitcast<vec3f>"
    | "bitcast<vec4f>"
    | "bitcast<vec2i>"
    | "bitcast<vec3i>"
    | "bitcast<vec4i>"
    | "bitcast<vec2u>"
    | "bitcast<vec3u>"
    | "bitcast<vec4u>"
    | "bitcast<vec2<bool>>"
    | "bitcast<vec3<bool>>"
    | "bitcast<vec4<bool>>" => word.to_string(),
    _ => word
      .replace("-", "_")
      .replace("+", "PLUS")
      .replace("*", "STAR")
      .replace(">", "ABRACKET_RIGHT")
      .replace("<", "ABRACKET_LEFT")
      .replace("?", "QMARK")
      .replace("!", "EMARK")
      .replace("=", "EQUAL_SIGN"),
  };
  if is_wgsl_reserved_word(&compiled_word) {
    compiled_word + "_"
  } else {
    compiled_word
  }
}

pub fn is_valid_name(word: &Rc<str>) -> bool {
  match &**word {
    "defn" | "def" | "struct" | "enum" | "let" | "return" | "if" | "when"
    | "match" | "break" | "continue" => false,
    other => {
      for (i, grapheme) in other.grapheme_indices(true) {
        if i == 0
          && grapheme.len() == 1
          && grapheme.chars().next().unwrap().is_ascii_digit()
        {
          return false;
        }
        match grapheme {
          "'" | " " | "|" | "~" | "." | "@" | ":" | ";" | "/" | "#" | "("
          | ")" | "[" | "]" | "{" | "}" => return false,
          _ => {}
        }
      }
      true
    }
  }
}
