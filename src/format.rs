use sse::{
  Encloser as SSEEncloser, EncloserOrOperator, Operator as SSEOperator,
};

use crate::{
  compiler::program::EaslDocument,
  parse::{EaslTree, Encloser, Operator},
};

const MAX_EXPRESSION_WIDTH: usize = 30;
const MAX_SINGLE_LINE_STRUCT_WIDTH: usize = 20;

fn indented_newline(indentation: usize) -> String {
  "\n".to_string() + &" ".repeat(indentation)
}

fn is_tree_comment(tree: &EaslTree) -> bool {
  match &tree {
    sse::Ast::Leaf(_, _) => false,
    sse::Ast::Inner((_, encloser_or_operator), _) => match encloser_or_operator
    {
      EncloserOrOperator::Encloser(
        Encloser::BlockComment | Encloser::LineComment,
      ) => true,
      EncloserOrOperator::Operator(Operator::ExpressionComment) => true,
      _ => false,
    },
  }
}

pub enum Block {
  Leaf(String),
  Horizontal(Vec<Self>),
  Vertical(Vec<Self>),
  Enclosed(Encloser, Box<Self>),
  Prefixed(Operator, Box<Self>),
  MetadataHorizontal(Box<Self>, Box<Self>),
  MetadataVertical(Box<Self>, Box<Self>),
  Typed(Box<Self>, Box<Self>),
  ApplicationIndentation(String, Box<Self>),
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
      Typed(term, ty) => term.last_line_width() + 2 + ty.last_line_width(),
      MetadataHorizontal(data, term) => data.width() + term.width() + 2,
      MetadataVertical(_, term) => term.last_line_width(),
      ApplicationIndentation(f, inner) => f.len() + 1 + inner.last_line_width(),
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
      Horizontal(blocks) => blocks
        .iter()
        .fold(blocks.len() - 1, |sum, leaf| sum + leaf.width()),
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
      Typed(term, ty) => term.last_line_width() + 2 + ty.width(),
      MetadataHorizontal(data, term) => data.width() + 2 + term.width(),
      MetadataVertical(data, term) => (data.width() + 1).max(term.width()),
      ApplicationIndentation(f, inner) => f.len() + 1 + inner.width(),
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
      Vertical(blocks) => blocks.iter().map(|block| block.height()).sum(),
      Typed(term, ty) => term.height() + ty.height() - 1,
      MetadataHorizontal(_, _) => 1,
      MetadataVertical(data, term) => data.height() + term.width(),
      ApplicationIndentation(_, block) => block.height(),
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
  fn print(self, indentation: usize) -> String {
    match self {
      Leaf(s) => s,
      Horizontal(blocks) => blocks
        .into_iter()
        .map(|block| block.print(indentation))
        .reduce(|acc, s| acc + " " + &s)
        .unwrap_or_else(|| String::new()),
      Vertical(blocks) => blocks
        .into_iter()
        .map(|block| block.print(indentation))
        .reduce(|acc, s| acc + &indented_newline(indentation) + &s)
        .unwrap_or_else(|| String::new()),
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
        term.print(indentation) + ": " + &ty.print(offset + 2)
      }
      MetadataHorizontal(data, term) => {
        "@".to_string()
          + &data.print(indentation)
          + " "
          + &term.print(indentation)
      }
      MetadataVertical(data, term) => {
        "@".to_string()
          + &data.print(indentation + 1)
          + &indented_newline(indentation)
          + &term.print(indentation)
      }
      ApplicationIndentation(f, inner) => {
        let f_len = f.len();
        f + " " + &inner.print(indentation + f_len + 1)
      }
      Bindings(bindings) => bindings
        .into_iter()
        .map(|(name, value)| {
          if let Some(value) = value {
            let name_last_line_width = name.last_line_width();
            name.print(indentation)
              + " "
              + &value.print(indentation + name_last_line_width)
          } else {
            name.print(indentation)
          }
        })
        .collect::<Vec<String>>()
        .join(&indented_newline(indentation)),
      IndentedBody {
        opener,
        prefix,
        bodies,
      } => {
        let opener_len = opener.len();
        opener
          + " "
          + &prefix.print(indentation + opener_len + 1)
          + &indented_newline(indentation + 1)
          + &bodies
            .into_iter()
            .map(|body_exp| body_exp.print(indentation + 1))
            .reduce(|a, b| a + &indented_newline(indentation + 1) + &b)
            .unwrap_or_else(|| String::new())
      }
    }
  }
  fn from_sub_blocks(blocks: Vec<Self>) -> Self {
    if blocks.iter().find(|b| b.height() > 1).is_some() {
      Vertical(blocks)
    } else {
      let total_width =
        (blocks.len() - 1) + blocks.iter().map(|b| b.width()).sum::<usize>();
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
            'outer: loop {
              while let Some(comment_or_value) = trees.next() {
                let is_comment = is_tree_comment(&comment_or_value);
                comments_and_values.push(comment_or_value);
                if !is_comment {
                  break 'outer;
                };
              }
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
      if let EaslTree::Leaf(_, _) = &trees
        .peek()
        .expect("Block::from_trees called with empty vec")
      {
        if application {
          let Some(EaslTree::Leaf(_, s)) = trees.next() else {
            unreachable!()
          };
          return match s.as_str() {
            "struct" | "when" | "while" | "for" if trees.len() >= 1 => {
              let mut blocks = trees.map(Self::from_tree);
              IndentedBody {
                opener: s,
                prefix: blocks.next().unwrap().into(),
                bodies: blocks.collect(),
              }
            }
            "match" if trees.len() >= 1 => {
              //let mut blocks = trees.map(Self::from_tree);
              IndentedBody {
                opener: s,
                prefix: Self::from_tree(trees.next().unwrap()).into(),
                bodies: vec![Bindings(Self::bindings_from_trees(
                  trees.collect(),
                ))],
              }
            }
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
            _ => ApplicationIndentation(
              s,
              Box::new(Self::from_sub_blocks(
                trees.map(Self::from_tree).collect(),
              )),
            ),
          };
        }
      }
      Self::from_sub_blocks(trees.map(Self::from_tree).collect())
    }
  }
  fn from_tree(tree: EaslTree) -> Self {
    match tree {
      EaslTree::Leaf(_, s) => Leaf(s),
      EaslTree::Inner((_, encloser_or_operator), mut asts) => {
        match encloser_or_operator {
          EncloserOrOperator::Operator(Operator::TypeAnnotation) => Typed(
            Box::new(Self::from_tree(asts.remove(0))),
            Box::new(Self::from_tree(asts.remove(0))),
          ),
          EncloserOrOperator::Operator(Operator::MetadataAnnotation) => {
            let data = Self::from_tree(asts.remove(0));
            let term = Self::from_tree(asts.remove(0));
            if data.height() == 1
              && term.height() == 1
              && data.width() + 1 + term.width() < MAX_EXPRESSION_WIDTH
            {
              MetadataHorizontal(Box::new(data), Box::new(term))
            } else {
              MetadataVertical(Box::new(data), Box::new(term))
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

pub fn format_document(document: EaslDocument) -> String {
  let mut s = String::new();
  for (i, ast) in document.syntax_trees.into_iter().enumerate() {
    if i > 0 {
      s += "\n";
    }
    s += &Block::from_tree(ast).print(0);
    s += "\n";
  }
  s
}
