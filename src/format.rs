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
  Bindings(Vec<Self>),
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
      Bindings(blocks) => {
        if blocks.len() % 2 == 1 {
          blocks.last().unwrap().last_line_width()
        } else {
          if blocks.is_empty() {
            0
          } else {
            let last_binding = &blocks[blocks.len() - 2];
            if last_binding.height() == 1 {
              last_binding.width()
                + 1
                + blocks.last().unwrap().last_line_width()
            } else {
              blocks.last().unwrap().last_line_width()
            }
          }
        }
      }
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
      Bindings(blocks) => blocks
        .chunks(2)
        .map(|subblocks| {
          if subblocks.len() == 2 {
            let binding = &subblocks[0];
            if binding.height() == 1 {
              binding.width() + 1 + subblocks[1].width()
            } else {
              binding.width().max(subblocks[1].width())
            }
          } else {
            subblocks[0].width()
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
      Bindings(blocks) => blocks
        .chunks(2)
        .map(|subblocks| {
          if subblocks.len() == 2 {
            let binding = &subblocks[0];
            if binding.height() == 1 {
              subblocks[1].height()
            } else {
              binding.height() + subblocks[1].height()
            }
          } else {
            subblocks[0].height()
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
          + "\n"
          + &term.print(indentation)
      }
      ApplicationIndentation(f, inner) => {
        let f_len = f.len();
        f + " " + &inner.print(indentation + f_len + 1)
      }
      Bindings(blocks) => {
        let mut s = String::new();
        let mut blocks_iter = blocks.into_iter();
        let mut first_loop = true;
        while let Some(binding) = blocks_iter.next() {
          if first_loop {
            first_loop = false;
          } else {
            s += &indented_newline(indentation);
          }
          if let Some(value) = blocks_iter.next() {
            if binding.height() == 1 {
              s += &binding.print(indentation);
              s += " ";
              s += &value.print(indentation);
            } else {
              s += &binding.print(indentation);
              s += &indented_newline(indentation);
              s += &value.print(indentation);
            }
          } else {
            s += &binding.print(indentation);
          }
        }
        s
      }
      IndentedBody {
        opener,
        prefix,
        bodies,
      } => {
        let opener_len = opener.len();
        opener
          + " "
          + &prefix.print(indentation + opener_len)
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
  fn from_trees(mut trees: Vec<EaslTree>, application: bool) -> Self {
    if trees.len() == 1 {
      Self::from_tree(trees.remove(0))
    } else {
      let mut trees = trees.into_iter().peekable();
      //let mut blocks = trees.into_iter().map(Self::from_tree).peekable();
      if let EaslTree::Leaf(_, _) = &trees
        .peek()
        .expect("Block::from_trees called with empty vec")
      {
        if application {
          let Some(EaslTree::Leaf(_, s)) = trees.next() else {
            unreachable!()
          };
          return match s.as_str() {
            "struct" if trees.len() >= 1 => {
              let mut blocks = trees.map(Self::from_tree);
              IndentedBody {
                opener: s,
                prefix: blocks.next().unwrap().into(),
                bodies: blocks.collect(),
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
              let subtree_blocks =
                subtrees.into_iter().map(Self::from_tree).collect();
              IndentedBody {
                opener: s,
                prefix: Box::new(Enclosed(
                  Encloser::Square,
                  Box::new(Bindings(subtree_blocks)),
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
              Encloser::Curly => {
                let blocks = asts
                  .into_iter()
                  .map(Self::from_tree)
                  .collect::<Vec<Block>>();
                if (blocks.iter().map(|block| block.width()).sum::<usize>()
                  + blocks.len()
                  - 1)
                  < MAX_SINGLE_LINE_STRUCT_WIDTH
                {
                  Self::from_sub_blocks(blocks)
                } else {
                  Bindings(blocks)
                }
              }
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
