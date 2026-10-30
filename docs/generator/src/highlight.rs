//! Small hand-rolled syntax highlighters for easl and GLSL code blocks.
//!
//! Emits `<span class="...">` wrappers with single-letter classes:
//!   c comment · s string · n number/literal · k keyword · t type
//!   f call-position symbol · a annotation · p delimiter

/// Special forms and reserved words that get keyword coloring.
const KEYWORDS: &[&str] = &[
  "defn", "def", "var", "override", "struct", "enum", "import", "let", "fn",
  "if", "when", "do", "match", "for", "while", "break", "continue", "return",
  "discard", "->", "<>", "_",
];

pub fn highlight_easl(code: &str) -> String {
  let chars: Vec<char> = code.chars().collect();
  let mut out = String::new();
  let mut i = 0;
  // true right after an opening paren: the next symbol is in call position
  let mut call_position = false;

  while i < chars.len() {
    let c = chars[i];
    match c {
      ';' => {
        // line comment, or block comment `;* ... *;`
        let start = i;
        if i + 1 < chars.len() && chars[i + 1] == '*' {
          i += 2;
          while i < chars.len()
            && !(chars[i] == '*' && i + 1 < chars.len() && chars[i + 1] == ';')
          {
            i += 1;
          }
          i = (i + 2).min(chars.len());
        } else {
          while i < chars.len() && chars[i] != '\n' {
            i += 1;
          }
        }
        span(&mut out, "c", &collect(&chars[start..i]));
      }
      '"' => {
        let start = i;
        i += 1;
        while i < chars.len() && chars[i] != '"' {
          i += 1;
        }
        i = (i + 1).min(chars.len());
        span(&mut out, "s", &collect(&chars[start..i]));
      }
      '#' if i + 1 < chars.len() && chars[i + 1] == '_' => {
        span(&mut out, "c", "#_");
        i += 2;
      }
      '(' => {
        span(&mut out, "p", "(");
        call_position = true;
        i += 1;
      }
      ')' | '[' | ']' | '{' | '}' => {
        span(&mut out, "p", &c.to_string());
        call_position = false;
        i += 1;
      }
      ':' => {
        span(&mut out, "p", ":");
        i += 1;
      }
      '@' => {
        // an annotation: `@word`, or the `@` opening `@{...}` / `@[...]`
        let start = i;
        i += 1;
        while i < chars.len() && !is_token_break(chars[i]) && chars[i] != '@' {
          i += 1;
        }
        span(&mut out, "a", &collect(&chars[start..i]));
      }
      c if c.is_whitespace() => {
        out.push(c);
        i += 1;
      }
      _ => {
        let start = i;
        while i < chars.len() && !is_token_break(chars[i]) {
          i += 1;
        }
        let token = collect(&chars[start..i]);
        let class = classify(&token, call_position);
        match class {
          Some(class) => span(&mut out, class, &token),
          None => out.push_str(&escape(&token)),
        }
        call_position = false;
      }
    }
  }
  out
}

fn classify(token: &str, call_position: bool) -> Option<&'static str> {
  if token == "true" || token == "false" || is_number(token) {
    Some("n")
  } else if KEYWORDS.contains(&token) {
    Some("k")
  } else if is_type_name(token) {
    Some("t")
  } else if call_position {
    Some("f")
  } else {
    None
  }
}

fn is_token_break(c: char) -> bool {
  c.is_whitespace()
    || matches!(c, '(' | ')' | '[' | ']' | '{' | '}' | ':' | ';' | '"')
}

fn is_number(token: &str) -> bool {
  let digits = token.strip_prefix('-').unwrap_or(token);
  if !digits.chars().next().is_some_and(|c| c.is_ascii_digit()) {
    return false;
  }
  // strip a single type-suffix character (5u, -3i, 6f, and b for vec suffixes)
  let digits = digits
    .strip_suffix(['f', 'i', 'u', 'b'])
    .unwrap_or(digits);
  let mut seen_dot = false;
  digits.chars().all(|c| {
    if c == '.' {
      !std::mem::replace(&mut seen_dot, true)
    } else {
      c.is_ascii_digit()
    }
  })
}

fn is_type_name(token: &str) -> bool {
  if matches!(token, "f32" | "i32" | "u32" | "bool") {
    return true;
  }
  // vec2, vec3f, vec4b, ...
  if let Some(rest) = token.strip_prefix("vec") {
    let rest = rest.strip_suffix(['f', 'i', 'u', 'b']).unwrap_or(rest);
    if matches!(rest, "2" | "3" | "4") {
      return true;
    }
  }
  // mat4x4, mat2x3f, ...
  if let Some(rest) = token.strip_prefix("mat") {
    let rest = rest
      .strip_suffix(['f', 'i', 'u'])
      .unwrap_or(rest);
    let bytes = rest.as_bytes();
    if bytes.len() == 3
      && bytes[0].is_ascii_digit()
      && bytes[1] == b'x'
      && bytes[2].is_ascii_digit()
    {
      return true;
    }
  }
  // PascalCase user/builtin types (but not the wildcard `_` etc.)
  token.chars().next().is_some_and(|c| c.is_ascii_uppercase())
}

fn span(out: &mut String, class: &str, text: &str) {
  out.push_str("<span class=\"");
  out.push_str(class);
  out.push_str("\">");
  out.push_str(&escape(text));
  out.push_str("</span>");
}

fn collect(chars: &[char]) -> String {
  chars.iter().collect()
}

fn escape(text: &str) -> String {
  text
    .replace('&', "&amp;")
    .replace('<', "&lt;")
    .replace('>', "&gt;")
}

/// GLSL keywords and qualifiers that get keyword coloring.
const GLSL_KEYWORDS: &[&str] = &[
  "if", "else", "for", "while", "do", "break", "continue", "return",
  "discard", "switch", "case", "default", "struct", "const", "in", "out",
  "inout", "uniform", "layout", "precision", "highp", "mediump", "lowp",
  "flat", "smooth",
];

/// Highlights a GLSL code block with the same span classes as easl: a name
/// followed by `(` is a call, unless it's a keyword or a type (constructors
/// keep type coloring), and `#` lines are preprocessor directives.
pub fn highlight_glsl(code: &str) -> String {
  let chars: Vec<char> = code.chars().collect();
  let mut out = String::new();
  let mut i = 0;

  while i < chars.len() {
    let c = chars[i];
    let next = chars.get(i + 1).copied();
    if c == '/' && next == Some('/') {
      let start = i;
      while i < chars.len() && chars[i] != '\n' {
        i += 1;
      }
      span(&mut out, "c", &collect(&chars[start..i]));
    } else if c == '/' && next == Some('*') {
      let start = i;
      i += 2;
      while i < chars.len() && !(chars[i] == '*' && chars.get(i + 1) == Some(&'/')) {
        i += 1;
      }
      i = (i + 2).min(chars.len());
      span(&mut out, "c", &collect(&chars[start..i]));
    } else if c == '#' {
      let start = i;
      while i < chars.len() && chars[i] != '\n' {
        i += 1;
      }
      span(&mut out, "a", &collect(&chars[start..i]));
    } else if c.is_ascii_digit() || (c == '.' && next.is_some_and(|n| n.is_ascii_digit())) {
      let start = i;
      while i < chars.len() {
        let d = chars[i];
        if (d == 'e' || d == 'E') && matches!(chars.get(i + 1), Some('+' | '-')) {
          i += 2;
        } else if d.is_ascii_alphanumeric() || d == '.' {
          i += 1;
        } else {
          break;
        }
      }
      span(&mut out, "n", &collect(&chars[start..i]));
    } else if c.is_ascii_alphabetic() || c == '_' {
      let start = i;
      while i < chars.len() && (chars[i].is_ascii_alphanumeric() || chars[i] == '_') {
        i += 1;
      }
      let token = collect(&chars[start..i]);
      let mut lookahead = i;
      while lookahead < chars.len() && chars[lookahead].is_whitespace() {
        lookahead += 1;
      }
      let called = chars.get(lookahead) == Some(&'(');
      let class = if token == "true" || token == "false" {
        Some("n")
      } else if GLSL_KEYWORDS.contains(&token.as_str()) {
        Some("k")
      } else if is_glsl_type(&token) {
        Some("t")
      } else if called {
        Some("f")
      } else {
        None
      };
      match class {
        Some(class) => span(&mut out, class, &token),
        None => out.push_str(&escape(&token)),
      }
    } else if matches!(c, '(' | ')' | '[' | ']' | '{' | '}' | ';' | ',') {
      span(&mut out, "p", &c.to_string());
      i += 1;
    } else {
      out.push_str(&escape(&c.to_string()));
      i += 1;
    }
  }
  out
}

fn is_glsl_type(token: &str) -> bool {
  if matches!(token, "void" | "bool" | "int" | "uint" | "float" | "double") {
    return true;
  }
  // vec3, ivec2, uvec4, bvec3, dvec2, mat4, mat2x3, ...
  let vector = token
    .strip_prefix(['i', 'u', 'b', 'd'])
    .unwrap_or(token)
    .strip_prefix("vec");
  if vector.is_some_and(|n| matches!(n, "2" | "3" | "4")) {
    return true;
  }
  if let Some(rest) = token.strip_prefix("dmat").or_else(|| token.strip_prefix("mat")) {
    let bytes = rest.as_bytes();
    let dim = |b: u8| (b'2'..=b'4').contains(&b);
    if (bytes.len() == 1 && dim(bytes[0]))
      || (bytes.len() == 3 && dim(bytes[0]) && bytes[1] == b'x' && dim(bytes[2]))
    {
      return true;
    }
  }
  token.starts_with("sampler") || token.starts_with("image") || token.starts_with("texture")
}
