//! A small hand-rolled syntax highlighter for easl code blocks.
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
