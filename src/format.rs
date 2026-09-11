use fsexp::{
  Encloser as SSEEncloser, EncloserOrOperator, Operator as SSEOperator,
  ParseError,
};

use crate::{
  compiler::program::EaslDocument,
  parse::{EaslTree, Encloser, Operator, parse_easl},
};

/// Below this width (measured only from a form's own start, indentation
/// ignored), the construction pass prefers a single-line "horizontal"
/// layout. This is the horizontal-greedy default.
const MAX_EXPRESSION_WIDTH: usize = 30;

/// Hard upper bound on line width. When a layout chosen by construction
/// would place any character past this column (accounting for actual
/// indentation), the `refine` pass restructures the outermost offending
/// form into a more vertical layout.
const MAX_WIDTH: usize = 80;

/// When an application has to break across lines, a name no wider than this
/// keeps the "aligned" layout (args aligned under the first, on the
/// function-name line). A longer name would shift every argument too far
/// right, so its arguments go entirely below the name instead. (Single-
/// argument breaks always go below — there's nothing to align under.)
const MAX_INLINE_APPLICATION_NAME_WIDTH: usize = 16;

fn indented_newline(indentation: usize) -> String {
  "\n".to_string() + &" ".repeat(indentation)
}

/// Joins already-printed sibling lines, each starting at `indentation`.
/// A part that already ends in a newline is a line comment (its `\n`
/// terminator is mandatory — it protects whatever follows from being
/// commented out), so the separator between it and the next part is just
/// the indentation, never another newline. This is what keeps line
/// comments from leaving a blank line after themselves.
fn join_vertical(parts: Vec<String>, indentation: usize) -> String {
  let mut result = String::new();
  for (i, part) in parts.into_iter().enumerate() {
    if i == 0 {
      result = part;
    } else if result.ends_with('\n') {
      result.push_str(&" ".repeat(indentation));
      result.push_str(&part);
    } else {
      result.push_str(&indented_newline(indentation));
      result.push_str(&part);
    }
  }
  result
}

fn is_tree_comment(tree: &EaslTree) -> bool {
  match &tree {
    fsexp::Ast::Leaf(_, _) => false,
    fsexp::Ast::Inner((_, encloser_or_operator), _) => {
      match encloser_or_operator {
        EncloserOrOperator::Encloser(
          Encloser::BlockComment | Encloser::LineComment,
        ) => true,
        EncloserOrOperator::Operator(Operator::ExpressionComment) => true,
        _ => false,
      }
    }
  }
}

fn is_line_comment_block(block: &Block) -> bool {
  matches!(block, Enclosed(Encloser::LineComment, _))
}

#[derive(Debug, Clone)]
pub enum Block {
  Leaf(String),
  Horizontal(Vec<Self>),
  Vertical(Vec<Self>),
  Enclosed(Encloser, Box<Self>),
  Prefixed(Operator, Box<Self>),
  AnnotationHorizontal(Box<Self>, Option<Box<Self>>),
  AnnotationVertical(Box<Self>, Option<Box<Self>>),
  Typed(Box<Self>, Option<Box<Self>>),
  /// `term:` on the first line with the type on the next, indented one
  /// column past the term. The vertical fallback for a `:` annotation whose
  /// type won't fit on the term's line — `refine` switches a `Typed` to
  /// this only when the horizontal form overflows.
  TypedVertical(Box<Self>, Box<Self>),
  /// `f arg0` on the first line, remaining args on following lines aligned
  /// under `arg0` (column = `f.len() + 1` from the form's start). The
  /// "aligned" application strategy, and also the header shape of `defn`.
  ApplicationIndentation(String, Box<Self>),
  /// `f` alone on the first line, every arg on a following line indented
  /// one column past the form's start. The most vertical application
  /// strategy — reclaims the horizontal space a long function name would
  /// otherwise cost every argument.
  ApplicationBelow(String, Vec<Self>),
  /// A single-line `fn` literal: `elements` is `[params, body...]`. If it
  /// no longer fits, `refine` turns it into an `IndentedBody` (body one
  /// column past `fn`).
  Lambda(Vec<Self>),
  Bindings(Vec<(Self, Option<Self>)>),
  IndentedBody {
    opener: String,
    prefix: Box<Self>,
    bodies: Vec<Self>,
  },
}
use Block::*;

impl Block {
  fn last_line_width(&self) -> usize {
    match self {
      Vertical(blocks) => blocks
        .iter()
        .last()
        .map(|block| block.last_line_width())
        .unwrap_or(0),
      Enclosed(encloser, inner) => {
        encloser.opening_encloser_str().len()
          + encloser.closing_encloser_str().len()
          + inner.last_line_width()
      }
      Prefixed(operator, inner) => {
        operator.op_str().len() + inner.last_line_width()
      }
      Typed(term, ty) => {
        term.last_line_width()
          + 2
          + ty.as_ref().map(|ty| ty.last_line_width()).unwrap_or(0)
      }
      TypedVertical(_, ty) => 1 + ty.last_line_width(),
      AnnotationHorizontal(data, term) => {
        data.width() + term.as_ref().map(|term| term.width()).unwrap_or(0) + 2
      }
      AnnotationVertical(data, term) => term
        .as_ref()
        .map(|term| term.last_line_width())
        .unwrap_or_else(|| data.last_line_width()),
      ApplicationIndentation(f, inner) => f.len() + 1 + inner.last_line_width(),
      ApplicationBelow(f, args) => args
        .last()
        .map(|a| 1 + a.last_line_width())
        .unwrap_or_else(|| f.len()),
      Bindings(bindings) => bindings
        .last()
        .map(|(name, value)| {
          if let Some(value) = value {
            name.last_line_width() + 1 + value.last_line_width()
          } else {
            name.last_line_width()
          }
        })
        .unwrap_or(0),
      IndentedBody { bodies, .. } => {
        bodies
          .last()
          .map(|body_exp| body_exp.last_line_width())
          .unwrap_or(0)
          + 1
      }
      _ => self.width(),
    }
  }
  fn width(&self) -> usize {
    match self {
      Leaf(s) => s.len(),
      Horizontal(blocks) => {
        if blocks.is_empty() {
          0
        } else {
          blocks
            .iter()
            .fold(blocks.len() - 1, |sum, leaf| sum + leaf.width())
        }
      }
      Vertical(blocks) => {
        blocks.iter().map(|block| block.width()).max().unwrap_or(0)
      }
      Enclosed(encloser, inner) => {
        encloser.opening_encloser_str().len()
          + inner.width().max(
            inner.last_line_width() + encloser.closing_encloser_str().len(),
          )
      }
      Prefixed(operator, inner) => operator.op_str().len() + inner.width(),
      Typed(term, ty) => {
        term.last_line_width()
          + 2
          + ty.as_ref().map(|ty| ty.width()).unwrap_or(0)
      }
      TypedVertical(term, ty) => term
        .width()
        .max(term.last_line_width() + 1)
        .max(1 + ty.width()),
      AnnotationHorizontal(data, term) => {
        data.width() + 2 + term.as_ref().map(|term| term.width()).unwrap_or(0)
      }
      AnnotationVertical(data, term) => (data.width() + 1)
        .max(term.as_ref().map(|term| term.width()).unwrap_or(0)),
      ApplicationIndentation(f, inner) => f.len() + 1 + inner.width(),
      ApplicationBelow(f, args) => f
        .len()
        .max(1 + args.iter().map(|a| a.width()).max().unwrap_or(0)),
      Lambda(elements) => {
        2 + elements.iter().map(|e| 1 + e.width()).sum::<usize>()
      }
      Bindings(bindings) => bindings
        .iter()
        .map(|(name, value)| {
          if let Some(value) = value {
            name.width().max(name.last_line_width() + value.width())
          } else {
            name.width()
          }
        })
        .max()
        .unwrap_or(0),
      IndentedBody {
        opener,
        prefix,
        bodies,
      } => (bodies
        .iter()
        .map(|body_exp| body_exp.width())
        .max()
        .unwrap_or(0)
        + 1)
        .max(opener.len() + 1 + prefix.width()),
    }
  }
  fn height(&self) -> usize {
    match self {
      Leaf(_) => 1,
      Horizontal(_) => 1,
      Lambda(_) => 1,
      Vertical(blocks) => blocks.iter().map(|block| block.height()).sum(),
      Typed(term, ty) => {
        term.height() + ty.as_ref().map(|ty| ty.width()).unwrap_or(1) - 1
      }
      TypedVertical(term, ty) => term.height() + ty.height(),
      AnnotationHorizontal(_, _) => 1,
      AnnotationVertical(data, term) => {
        data.height() + term.as_ref().map(|term| term.width()).unwrap_or(0)
      }
      ApplicationIndentation(_, block) => block.height(),
      ApplicationBelow(_, args) => {
        1 + args.iter().map(|a| a.height()).sum::<usize>()
      }
      Enclosed(_, inner) | Prefixed(_, inner) => inner.height(),
      Bindings(bindings) => bindings
        .iter()
        .map(|(name, value)| {
          if let Some(value) = value {
            name.height() + value.height() - 1
          } else {
            name.height()
          }
        })
        .sum(),
      IndentedBody { prefix, bodies, .. } => {
        prefix.height() + bodies.iter().map(|body| body.height()).sum::<usize>()
      }
    }
  }
  /// Whether this block actually renders across more than one line. Unlike
  /// `height`, this ignores the deliberate over-counting in `Typed` /
  /// `AnnotationVertical` (which forces typed sequences to lay out
  /// vertically) — a `Typed` of single-line parts prints on one line.
  fn prints_multiline(&self) -> bool {
    match self {
      Leaf(_) | Lambda(_) | Horizontal(_) | AnnotationHorizontal(_, _) => false,
      Vertical(blocks) => {
        blocks.len() > 1 || blocks.iter().any(|b| b.prints_multiline())
      }
      Enclosed(_, inner) | Prefixed(_, inner) => inner.prints_multiline(),
      Typed(term, ty) => {
        term.prints_multiline()
          || ty.as_ref().map(|t| t.prints_multiline()).unwrap_or(false)
      }
      AnnotationVertical(_, _)
      | ApplicationBelow(_, _)
      | TypedVertical(_, _) => true,
      ApplicationIndentation(_, inner) => inner.prints_multiline(),
      Bindings(bindings) => {
        bindings.len() > 1
          || bindings.iter().any(|(name, value)| {
            name.prints_multiline()
              || value
                .as_ref()
                .map(|v| v.prints_multiline())
                .unwrap_or(false)
          })
      }
      IndentedBody { .. } => true,
    }
  }
  /// The rightmost column this block reaches when placed at `indent`, with
  /// `trailing` characters (ancestor closing enclosers) following its last
  /// line. `width()` already covers every interior line; `trailing` only
  /// extends the last one.
  fn effective_width(&self, trailing: usize) -> usize {
    self.width().max(self.last_line_width() + trailing)
  }
  /// Top-down width-limit pass. Any subtree that already fits within
  /// `MAX_WIDTH` at its actual position is left exactly as construction
  /// produced it. A subtree that overflows is switched to a more vertical
  /// layout — the outermost offending form first — and its children are
  /// then given a fresh chance to fit at their reduced indentation.
  fn refine(self, indent: usize, trailing: usize) -> Self {
    if indent + self.effective_width(trailing) <= MAX_WIDTH {
      return self;
    }
    match self {
      Leaf(s) => Leaf(s),
      Horizontal(blocks) => Vertical(refine_vertical(blocks, indent, trailing)),
      Vertical(blocks) => Vertical(refine_vertical(blocks, indent, trailing)),
      Enclosed(encloser, inner) => {
        let opening = encloser.opening_encloser_str().len();
        let closing = encloser.closing_encloser_str().len();
        Enclosed(
          encloser,
          Box::new(inner.refine(indent + opening, trailing + closing)),
        )
      }
      Prefixed(operator, inner) => {
        let len = operator.op_str().len();
        Prefixed(operator, Box::new(inner.refine(indent + len, trailing)))
      }
      Typed(term, None) => Typed(Box::new(term.refine(indent, trailing)), None),
      Typed(term, Some(ty)) => {
        // The term is followed by ": ty" on its last line, so it must break
        // for that whole suffix, not just for itself.
        let term = term.refine(indent, 2 + ty.width() + trailing);
        let offset = term.last_line_width();
        // Keep the type on the term's line when it fits there as a single
        // line; otherwise drop it below, one column past the term.
        if !ty.prints_multiline()
          && indent + offset + 2 + ty.width() + trailing <= MAX_WIDTH
        {
          Typed(
            Box::new(term),
            Some(Box::new(ty.refine(offset + 2, trailing))),
          )
        } else {
          TypedVertical(
            Box::new(term),
            Box::new(ty.refine(indent + 1, trailing)),
          )
        }
      }
      TypedVertical(term, ty) => TypedVertical(
        Box::new(term.refine(indent, 1)),
        Box::new(ty.refine(indent + 1, trailing)),
      ),
      AnnotationHorizontal(data, term) | AnnotationVertical(data, term) => {
        let data = data.refine(indent + 1, 0);
        let term = term.map(|term| Box::new(term.refine(indent, trailing)));
        AnnotationVertical(Box::new(data), term)
      }
      ApplicationIndentation(f, inner) => match *inner {
        // An application whose flat/aligned layout overflowed breaks
        // further. A short name keeps args aligned under the first (if that
        // now fits); a long name — or a lone argument — puts every argument
        // below the name to reclaim the horizontal space.
        Horizontal(args) | Vertical(args) => {
          if args.len() == 1 || f.len() > MAX_INLINE_APPLICATION_NAME_WIDTH {
            ApplicationBelow(f, refine_vertical(args, indent + 1, trailing))
          } else {
            let aligned_indent = indent + f.len() + 1;
            let aligned = ApplicationIndentation(
              f.clone(),
              Box::new(Vertical(refine_vertical(
                args.clone(),
                aligned_indent,
                trailing,
              ))),
            );
            if indent + aligned.effective_width(trailing) <= MAX_WIDTH {
              aligned
            } else {
              ApplicationBelow(f, refine_vertical(args, indent + 1, trailing))
            }
          }
        }
        other => {
          let offset = f.len() + 1;
          ApplicationIndentation(
            f,
            Box::new(other.refine(indent + offset, trailing)),
          )
        }
      },
      ApplicationBelow(f, args) => {
        ApplicationBelow(f, refine_vertical(args, indent + 1, trailing))
      }
      Lambda(mut elements) => {
        let prefix = elements.remove(0);
        IndentedBody {
          opener: "fn".to_string(),
          prefix: Box::new(prefix),
          bodies: elements,
        }
        .refine(indent, trailing)
      }
      Bindings(bindings) => {
        let count = bindings.len();
        Bindings(
          bindings
            .into_iter()
            .enumerate()
            .map(|(i, (name, value))| {
              let last = i + 1 == count;
              let name_trailing = if value.is_some() {
                0
              } else if last {
                trailing
              } else {
                0
              };
              let name = name.refine(indent, name_trailing);
              let value = value.map(|value| {
                let offset = name.last_line_width();
                value
                  .refine(indent + offset + 1, if last { trailing } else { 0 })
              });
              (name, value)
            })
            .collect(),
        )
      }
      IndentedBody {
        opener,
        prefix,
        bodies,
      } => {
        let prefix = prefix.refine(indent + opener.len() + 1, 0);
        let bodies = refine_vertical(bodies, indent + 1, trailing);
        IndentedBody {
          opener,
          prefix: Box::new(prefix),
          bodies,
        }
      }
    }
  }
  fn print(self, indentation: usize) -> String {
    match self {
      Leaf(s) => s,
      Horizontal(blocks) => blocks
        .into_iter()
        .map(|block| block.print(indentation))
        .reduce(|acc, s| acc + " " + &s)
        .unwrap_or_else(|| String::new()),
      Vertical(blocks) => join_vertical(
        blocks
          .into_iter()
          .map(|block| block.print(indentation))
          .collect(),
        indentation,
      ),
      Enclosed(encloser, inner) => {
        encloser.opening_encloser_str().to_string()
          + &inner.print(encloser.opening_encloser_str().len() + indentation)
          + encloser.closing_encloser_str()
      }
      Prefixed(operator, inner) => {
        operator.op_str().to_string()
          + &inner.print(indentation + operator.op_str().len())
      }
      Typed(term, ty) => {
        let offset = term.last_line_width();
        term.print(indentation)
          + ": "
          + &ty
            .map(|ty| ty.print(offset + 2))
            .unwrap_or_else(|| String::new())
      }
      TypedVertical(term, ty) => {
        term.print(indentation)
          + ":"
          + &indented_newline(indentation + 1)
          + &ty.print(indentation + 1)
      }
      AnnotationHorizontal(data, term) => {
        "@".to_string()
          + &data.print(indentation)
          + " "
          + &term
            .map(|term| term.print(indentation))
            .unwrap_or_else(|| String::new())
      }
      AnnotationVertical(data, term) => {
        "@".to_string()
          + &data.print(indentation + 1)
          + &indented_newline(indentation)
          + &term
            .map(|term| term.print(indentation))
            .unwrap_or_else(|| String::new())
      }
      ApplicationIndentation(f, inner) => {
        let f_len = f.len();
        f + " " + &inner.print(indentation + f_len + 1)
      }
      ApplicationBelow(f, args) => {
        let inner_indentation = indentation + 1;
        let mut result = f;
        for arg in args {
          let part = arg.print(inner_indentation);
          if result.ends_with('\n') {
            result.push_str(&" ".repeat(inner_indentation));
          } else {
            result.push_str(&indented_newline(inner_indentation));
          }
          result.push_str(&part);
        }
        result
      }
      Lambda(elements) => {
        let mut result = "fn".to_string();
        for element in elements {
          result.push(' ');
          result.push_str(&element.print(indentation));
        }
        result
      }
      Bindings(bindings) => join_vertical(
        bindings
          .into_iter()
          .map(|(name, value)| {
            if let Some(value) = value {
              let name_last_line_width = name.last_line_width();
              name.print(indentation)
                + " "
                + &value.print(indentation + name_last_line_width + 1)
            } else {
              name.print(indentation)
            }
          })
          .collect(),
        indentation,
      ),
      IndentedBody {
        opener,
        prefix,
        bodies,
      } => {
        let opener_len = opener.len();
        let head = opener + " " + &prefix.print(indentation + opener_len + 1);
        if bodies.is_empty() {
          return head;
        }
        let bodies_str = join_vertical(
          bodies
            .into_iter()
            .map(|body_exp| body_exp.print(indentation + 1))
            .collect(),
          indentation + 1,
        );
        if head.ends_with('\n') {
          head + &" ".repeat(indentation + 1) + &bodies_str
        } else {
          head + &indented_newline(indentation + 1) + &bodies_str
        }
      }
    }
  }
  fn from_sub_blocks(blocks: Vec<Self>) -> Self {
    if blocks
      .iter()
      .any(|b| b.height() > 1 || is_line_comment_block(b))
    {
      Vertical(blocks)
    } else {
      let total_width = if blocks.is_empty() {
        0
      } else {
        (blocks.len() - 1) + blocks.iter().map(|b| b.width()).sum::<usize>()
      };
      if total_width < MAX_EXPRESSION_WIDTH {
        Horizontal(blocks)
      } else {
        Vertical(blocks)
      }
    }
  }
  fn bindings_from_trees(trees: Vec<EaslTree>) -> Vec<(Self, Option<Self>)> {
    let mut trees = trees.into_iter();
    let mut bindings = vec![];
    while let Some(name) = trees.next() {
      if is_tree_comment(&name) {
        bindings.push((Self::from_tree(name), None));
      } else {
        bindings.push((
          Self::from_tree(name),
          Some({
            let mut comments_and_values = vec![];
            while let Some(comment_or_value) = trees.next() {
              let is_comment = is_tree_comment(&comment_or_value);
              comments_and_values.push(comment_or_value);
              if !is_comment {
                break;
              };
            }
            if comments_and_values.len() == 1 {
              Self::from_tree(comments_and_values.remove(0))
            } else {
              Self::from_trees(comments_and_values, false)
            }
          }),
        ));
      };
    }
    bindings
  }
  fn from_trees(mut trees: Vec<EaslTree>, application: bool) -> Self {
    if trees.len() == 1 {
      Self::from_tree(trees.remove(0))
    } else {
      let mut trees = trees.into_iter().peekable();
      if let Some(EaslTree::Leaf(_, _)) = &trees.peek() {
        if application {
          let Some(EaslTree::Leaf(_, s)) = trees.next() else {
            unreachable!()
          };
          return match s.as_str() {
            "struct" | "enum" | "when" | "while" | "for"
              if trees.len() >= 1 =>
            {
              let mut blocks = trees.map(Self::from_tree);
              IndentedBody {
                opener: s,
                prefix: blocks.next().unwrap().into(),
                bodies: blocks.collect(),
              }
            }
            "fn" if trees.len() >= 1 => {
              let params = Self::from_tree(trees.next().unwrap());
              let bodies: Vec<Self> = trees.map(Self::from_tree).collect();
              let all_single_line = params.height() == 1
                && bodies
                  .iter()
                  .all(|b| b.height() == 1 && !is_line_comment_block(b));
              let flat_width = 2
                + 1
                + params.width()
                + bodies.iter().map(|b| 1 + b.width()).sum::<usize>();
              if all_single_line && flat_width < MAX_EXPRESSION_WIDTH {
                let mut elements = vec![params];
                elements.extend(bodies);
                Lambda(elements)
              } else {
                IndentedBody {
                  opener: s,
                  prefix: Box::new(params),
                  bodies,
                }
              }
            }
            "match" if trees.len() >= 1 => IndentedBody {
              opener: s,
              prefix: Self::from_tree(trees.next().unwrap()).into(),
              bodies: vec![Bindings(Self::bindings_from_trees(
                trees.collect(),
              ))],
            },
            "if" if trees.len() >= 1 => {
              let mut blocks: Vec<Block> = trees.map(Self::from_tree).collect();
              if blocks.iter().find(|b| b.height() > 1).is_some()
                && blocks.iter().map(|b| b.width()).sum::<usize>()
                  + blocks.len()
                  + 2
                  < MAX_EXPRESSION_WIDTH
              {
                Horizontal(
                  std::iter::once(Leaf(s)).chain(blocks.into_iter()).collect(),
                )
              } else {
                IndentedBody {
                  opener: s,
                  prefix: Box::new(blocks.remove(0)),
                  bodies: blocks,
                }
              }
            }
            "defn"
              if trees.len() >= 2
                && matches!(trees.peek().unwrap(), EaslTree::Leaf(_, _)) =>
            {
              let mut blocks = trees.map(Self::from_tree);
              let Leaf(fn_name) = blocks.next().unwrap() else {
                unreachable!()
              };
              IndentedBody {
                opener: s,
                prefix: Box::new(ApplicationIndentation(
                  fn_name,
                  Box::new(blocks.next().unwrap()),
                )),
                bodies: blocks.collect(),
              }
            }
            "let"
              if trees.len() >= 1
                && matches!(
                  trees.peek().unwrap(),
                  EaslTree::Inner(
                    (_, EncloserOrOperator::Encloser(Encloser::Square)),
                    _
                  )
                ) =>
            {
              let EaslTree::Inner(
                (_, EncloserOrOperator::Encloser(Encloser::Square)),
                subtrees,
              ) = trees.next().unwrap()
              else {
                unreachable!()
              };

              IndentedBody {
                opener: s,
                prefix: Box::new(Enclosed(
                  Encloser::Square,
                  Box::new(Bindings(Self::bindings_from_trees(subtrees))),
                )),
                bodies: trees.map(Self::from_tree).collect(),
              }
            }
            _ => {
              let long_name = s.len() > MAX_INLINE_APPLICATION_NAME_WIDTH;
              match Self::from_sub_blocks(trees.map(Self::from_tree).collect())
              {
                // A genuinely multi-line broken application places its
                // arguments below the name when the name is long enough that
                // aligning would shift them too far right. Otherwise they
                // align under the first argument. (A `Vertical` that still
                // prints on one line — a lone typed argument the height
                // quirk flagged — stays aligned, i.e. inline.)
                Vertical(args)
                  if long_name
                    && (args.len() > 1
                      || args.iter().any(|a| a.prints_multiline())) =>
                {
                  ApplicationBelow(s, args)
                }
                inner => ApplicationIndentation(s, Box::new(inner)),
              }
            }
          };
        }
      }
      Self::from_sub_blocks(trees.map(Self::from_tree).collect())
    }
  }
  fn from_tree(tree: EaslTree) -> Self {
    match tree {
      EaslTree::Leaf(_, s) => {
        Leaf(if s.is_empty() { "_".to_string() } else { s })
      }
      EaslTree::Inner((_, encloser_or_operator), mut asts) => {
        match encloser_or_operator {
          EncloserOrOperator::Operator(Operator::TypeAscription) => {
            let mut asts_iter = asts.into_iter();
            Typed(
              Box::new(Self::from_tree(asts_iter.next().unwrap())),
              asts_iter.next().map(|ast| Box::new(Self::from_tree(ast))),
            )
          }
          EncloserOrOperator::Operator(Operator::Annotation) => {
            let data = Self::from_tree(asts.remove(0));
            let term = Self::from_tree(asts.remove(0));
            if data.height() == 1
              && term.height() == 1
              && data.width() + 1 + term.width() < MAX_EXPRESSION_WIDTH
            {
              AnnotationHorizontal(Box::new(data), Some(Box::new(term)))
            } else {
              AnnotationVertical(Box::new(data), Some(Box::new(term)))
            }
          }
          EncloserOrOperator::Operator(operator) => {
            Prefixed(operator, Box::new(Self::from_trees(asts, false)))
          }
          EncloserOrOperator::Encloser(encloser) => Enclosed(encloser, {
            Box::new(match encloser {
              Encloser::Curly => Bindings(Self::bindings_from_trees(asts)),
              Encloser::Parens => Self::from_trees(asts, true),
              _ => Self::from_trees(asts, false),
            })
          }),
        }
      }
    }
  }
}

/// Refines a run of sibling blocks laid out one-per-line at `indent`. Only
/// the last sibling's line is followed by the ancestor `trailing`; the
/// others are followed by a newline.
fn refine_vertical(
  blocks: Vec<Block>,
  indent: usize,
  trailing: usize,
) -> Vec<Block> {
  let count = blocks.len();
  blocks
    .into_iter()
    .enumerate()
    .map(|(i, block)| {
      block.refine(indent, if i + 1 == count { trailing } else { 0 })
    })
    .collect()
}

pub fn format_easl_tree(ast: EaslTree) -> String {
  Block::from_tree(ast).refine(0, 0).print(0)
}

pub fn format_easl_trees(asts: Vec<EaslTree>) -> String {
  let mut s = String::new();
  let mut last_ast_was_line_comment = false;
  for (i, ast) in asts.into_iter().enumerate() {
    if i > 0 && !last_ast_was_line_comment {
      s += "\n";
    }
    if let EaslTree::Inner(
      (_, EncloserOrOperator::Encloser(Encloser::LineComment)),
      _,
    ) = ast
    {
      s += &format_easl_tree(ast);
      last_ast_was_line_comment = true;
    } else {
      s += &format_easl_tree(ast);
      s += "\n";
      last_ast_was_line_comment = false;
    }
  }
  s
}

pub fn format_document(
  document: EaslDocument,
) -> Result<String, Vec<ParseError>> {
  if document.parsing_failures.is_empty() {
    Ok(format_easl_trees(document.syntax_trees))
  } else {
    Err(document.parsing_failures.clone())
  }
}

pub fn format_easl_source(
  easl_source: &str,
) -> Result<String, Vec<ParseError>> {
  format_document(parse_easl(easl_source))
}
