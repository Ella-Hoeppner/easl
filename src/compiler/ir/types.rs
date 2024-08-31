use crate::{compiler::ir::structs::UntypedStruct, parse::TyntTree};

use super::{
  environment::Environment,
  expression::{
    Exp::{self, *},
    ExpNode, Number,
  },
  structs::Struct,
};
use core::fmt::Debug;

pub enum CompileError {
  TypeError(TypeError),
  InvalidMetadata,
  InvalidStructField,
  InvalidStructName,
}

pub enum TypeError {
  CouldntInfer,
  IncompatibleConstraints,
  UnknownFunction,
  InvalidArity,
  UnrecognizedTypeName,
}

impl From<TypeError> for CompileError {
  fn from(err: TypeError) -> Self {
    Self::TypeError(err)
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyntType {
  None,
  F32,
  I32,
  U32,
  Bool,
  Struct(String),
}
impl TyntType {
  pub fn from_name(
    name: String,
    struct_names: &Vec<String>,
  ) -> Result<Self, CompileError> {
    use TyntType::*;
    Ok(match name.as_str() {
      "F32" => F32,
      "I32" => I32,
      "U32" => U32,
      "Bool" => Bool,
      _ => {
        if struct_names.contains(&name) {
          Struct(name)
        } else {
          return Err(TypeError::UnrecognizedTypeName.into());
        }
      }
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
enum TypeConstraints {
  Anything,
  OneOf(Vec<TyntType>),
  Definitely(TyntType),
}
use TypeConstraints::*;

impl TypeConstraints {
  fn restrict(&mut self, constraints: Self) -> Result<bool, TypeError> {
    match (self.clone(), constraints) {
      (Anything, Anything) => Ok(false),
      (_, Anything) => Ok(true),
      (Anything, mut constraints) => {
        std::mem::swap(self, &mut constraints);
        Ok(true)
      }
      (Definitely(a), Definitely(b)) => {
        if a == b {
          Ok(false)
        } else {
          Err(TypeError::IncompatibleConstraints)
        }
      }
      (OneOf(a), OneOf(b)) => {
        if a == b {
          return Ok(false);
        }
        let narrowed: Vec<_> =
          a.into_iter().filter(|a| b.contains(&a)).collect();
        if narrowed.is_empty() {
          Err(TypeError::IncompatibleConstraints)
        } else {
          std::mem::swap(
            self,
            &mut if narrowed.len() == 1 {
              Definitely(narrowed.into_iter().next().unwrap())
            } else {
              OneOf(narrowed)
            },
          );
          Ok(true)
        }
      }
      (OneOf(a), Definitely(b)) => {
        if a.contains(&b) {
          std::mem::swap(self, &mut Definitely(b));
          Ok(true)
        } else {
          Err(TypeError::IncompatibleConstraints)
        }
      }
      (Definitely(a), OneOf(b)) => {
        if b.contains(&a) {
          std::mem::swap(self, &mut Definitely(a));
          Ok(false)
        } else {
          Err(TypeError::IncompatibleConstraints)
        }
      }
    }
  }
}

enum SharedTypeConstraints {
  Representative(TypeConstraints),
  Shared(usize),
}

struct TypeMap {
  types: Vec<SharedTypeConstraints>,
}

impl TypeMap {
  pub fn new(types: Vec<SharedTypeConstraints>) -> Self {
    Self { types }
  }
  pub fn get_type_representative_index(&self, i: usize) -> usize {
    match self.types[i] {
      SharedTypeConstraints::Representative(_) => i,
      SharedTypeConstraints::Shared(parent) => {
        self.get_type_representative_index(parent)
      }
    }
  }
  pub fn get_type(&self, i: usize) -> TypeConstraints {
    if let SharedTypeConstraints::Representative(t) =
      &self.types[self.get_type_representative_index(i)]
    {
      t.clone()
    } else {
      unreachable!()
    }
  }
  pub fn join(&mut self, a: usize, b: usize) -> Result<bool, TypeError> {
    let a_representative = self.get_type_representative_index(a);
    let b_representative = self.get_type_representative_index(b);
    if a_representative == b_representative {
      return Ok(false);
    }
    let mut b_constraints = SharedTypeConstraints::Shared(a);
    std::mem::swap(&mut self.types[b_representative], &mut b_constraints);
    Ok(
      if let SharedTypeConstraints::Representative(a_type_ref) =
        &mut self.types[a_representative]
      {
        if let SharedTypeConstraints::Representative(b_type) = b_constraints {
          a_type_ref.restrict(b_type)?
        } else {
          unreachable!()
        }
      } else {
        unreachable!()
      },
    )
  }
  pub fn constrain(
    &mut self,
    index: usize,
    constraints: TypeConstraints,
  ) -> Result<bool, TypeError> {
    let representative_index = self.get_type_representative_index(index);
    if let SharedTypeConstraints::Representative(t) =
      &mut self.types[representative_index]
    {
      t.restrict(constraints)
    } else {
      unreachable!()
    }
  }
}

impl ExpNode<(usize, Environment)> {
  fn build_from_tynt_tree(
    tree: TyntTree,
    parent_env: &Environment,
    tree_type_constraints: &mut Vec<SharedTypeConstraints>,
  ) -> Self {
    let mut build_type_constraints =
      |exp, (constraints, env)| -> Result<ExpNode<(usize, Environment)>, !> {
        let constraint_index = tree_type_constraints.len();
        tree_type_constraints
          .push(SharedTypeConstraints::Representative(constraints));
        Ok(ExpNode {
          exp: exp,
          data: (constraint_index, env),
        })
      };
    match tree {
      TyntTree::Leaf(_, leaf) => {
        if leaf == "true" || leaf == "false" {
          ExpNode {
            exp: Box::new(Exp::BooleanLiteral(leaf == "true")),
            data: (Definitely(TyntType::Bool), parent_env.clone()),
          }
        } else if let Ok(i) = leaf.parse::<i64>() {
          ExpNode {
            exp: Box::new(Exp::NumberLiteral(Number::Int(i))),
            data: if i < 0 {
              (
                OneOf(vec![TyntType::I32, TyntType::F32]),
                parent_env.clone(),
              )
            } else {
              (
                OneOf(vec![TyntType::I32, TyntType::U32, TyntType::U32]),
                parent_env.clone(),
              )
            },
          }
        } else if let Ok(f) = leaf.parse::<f64>() {
          ExpNode {
            exp: Box::new(Exp::NumberLiteral(Number::Float(f))),
            data: (Definitely(TyntType::F32), parent_env.clone()),
          }
        } else {
          ExpNode {
            exp: Box::new(Exp::Name(leaf)),
            data: (Anything, parent_env.clone()),
          }
        }
      }
      TyntTree::Inner((_, encloser_or_operator), children) => {
        use crate::parse::Encloser::*;
        use crate::parse::Operator::*;
        use sse::syntax::EncloserOrOperator::*;
        match encloser_or_operator {
          Encloser(e) => match e {
            Parens => todo!(),
            Square => todo!(),
            Curly => todo!("I haven't implemented anonymous structs yet!!"),
          },
          Operator(o) => match o {
            Metadata => todo!(),
            TypeAnnotation => todo!(),
          },
        }
      }
    }
    .walk(&mut |node| Ok(node), &mut build_type_constraints)
    .unwrap()
  }
}

struct Program {
  structs: Vec<(String, Struct)>,
  expressions: Vec<ExpNode<(usize, Environment)>>,
  types: TypeMap,
  global_env: Environment,
}

impl Program {
  pub fn new(
    trees: Vec<TyntTree>,
    global_env: Environment,
  ) -> Result<Self, CompileError> {
    Ok(
      Self::unfinished_from_tynt_trees(trees, global_env)?
        .infer_initial_constraints()?
        .infer_types_until_fixed_point()?
        .ensure_fully_typed()?,
    )
  }
  fn unfinished_from_tynt_trees(
    trees: Vec<TyntTree>,
    global_env: Environment,
  ) -> Result<Self, CompileError> {
    let mut types = vec![];
    let mut expressions = vec![];
    let mut structs = vec![];
    for tree in trees.into_iter() {
      use crate::parse::Encloser::*;
      use crate::parse::Operator::*;
      use sse::syntax::EncloserOrOperator::*;
      let mut matched_struct = false;
      if let TyntTree::Inner((_, Encloser(Parens)), children) = &tree {
        if let Some(first_child) = children.first() {
          if let TyntTree::Leaf(_, leaf) = first_child {
            if leaf == "struct" {
              matched_struct = true;
              let mut children_iter = children.iter().skip(1).cloned();
              if let Some(TyntTree::Leaf(_, struct_name)) = children_iter.next()
              {
                let fields: Vec<_> = children.iter().skip(1).cloned().collect();
                structs.push((
                  struct_name,
                  UntypedStruct::from_field_expressions(fields)?,
                ));
              } else {
                return Err(CompileError::InvalidStructName);
              }
            }
          }
        }
      }
      if !matched_struct {
        expressions.push(ExpNode::build_from_tynt_tree(
          tree,
          &global_env,
          &mut types,
        ));
      }
    }
    /*let initial_trees = trees
    .into_iter()
    .map(|tree| {
      ExpNode::build_from_tynt_tree(tree, &global_env, &mut types, true)
    })
    .collect();*/
    let struct_names: Vec<String> =
      structs.iter().map(|(name, _)| name.clone()).collect();
    Ok(Self {
      structs: structs
        .into_iter()
        .map(|(name, untyped_struct)| {
          untyped_struct
            .assign_types(&struct_names)
            .map(|s| (name, s))
        })
        .collect::<Result<Vec<(String, Struct)>, CompileError>>()?,
      expressions,
      types: TypeMap::new(types),
      global_env,
    })
  }
  fn infer_initial_constraints(mut self) -> Result<Self, TypeError> {
    let new_expressions = self
      .expressions
      .into_iter()
      .map(|tree| {
        tree.walk(
          &mut |node| Ok(node),
          &mut |exp: Box<Exp<(usize, Environment)>>, (type_index, env)| {
            let new_env = match &*exp {
              Application(f, _) => match &*f.exp {
                Name(name) => {
                  let fn_output_type = env
                    .fn_output_type(name)
                    .ok_or(TypeError::UnknownFunction)?;
                  self
                    .types
                    .constrain(type_index, Definitely(fn_output_type))?;
                  env
                }
                _ => {
                  todo!("I haven't implemented higher order functions yet!!")
                }
              },
              Let(binding_pairs, body) => {
                self.types.join(type_index, body.data.0)?;
                let body_env = binding_pairs.into_iter().fold(
                  env,
                  |env, (name, value_scope)| {
                    env.bind(name.clone(), value_scope.data.0)
                  },
                );
                body_env
              }
              Match(scrutinee, cases) => {
                for (pattern_exp, body_exp) in cases {
                  self.types.join(scrutinee.data.0, pattern_exp.data.0)?;
                  self.types.join(type_index, body_exp.data.0)?;
                }
                env
                // if the match block is non-exhaustive, it's type should be
                // restricted to bottom. I think at first match blocks only
                // need to be able to match to literals, so the only exhaustive
                // case will be the case of 2 bools
              }
              _ => env,
            };
            Ok(ExpNode {
              exp,
              data: (type_index, new_env),
            })
          },
        )
      })
      .collect::<Result<_, TypeError>>()?;
    Ok(Self {
      expressions: new_expressions,
      structs: self.structs,
      types: self.types,
      global_env: self.global_env,
    })
  }
  fn propagate_constraints(mut self) -> Result<(Self, bool), TypeError> {
    let mut changed = false;
    let new_expressions = self
      .expressions
      .into_iter()
      .map(|tree| {
        tree.walk(
          &mut |node| {
            let (type_index, env) = node.data;
            let new_exp = match *node.exp {
              Application(f, args) => match *f.exp {
                Name(name) => {
                  let arg_types = env
                    .fn_input_types(&name)
                    .ok_or(TypeError::UnknownFunction)?;
                  if arg_types.len() != args.len() {
                    return Err(TypeError::InvalidArity);
                  }
                  Application(
                    ExpNode {
                      data: f.data,
                      exp: Box::new(Name(name)),
                    },
                    args
                      .into_iter()
                      .zip(arg_types.into_iter())
                      .map(|(arg, arg_type)| {
                        let constrain_caused_change = self.types.constrain(
                          type_index,
                          TypeConstraints::Definitely(arg_type),
                        )?;
                        changed |= constrain_caused_change;
                        Ok(arg)
                      })
                      .collect::<Result<_, TypeError>>()?,
                  )
                }
                _ => {
                  todo!("I haven't implemented higher order functions yet!!")
                }
              },
              other => other,
            };
            Ok(ExpNode {
              exp: Box::new(new_exp),
              data: (type_index, env),
            })
          },
          &mut |exp, data| Ok(ExpNode { exp, data }),
        )
      })
      .collect::<Result<_, TypeError>>()?;
    Ok((
      Self {
        expressions: new_expressions,
        structs: self.structs,
        types: self.types,
        global_env: self.global_env,
      },
      changed,
    ))
  }
  fn infer_types_until_fixed_point(mut self) -> Result<Self, TypeError> {
    loop {
      let (new_program, changed) = self.propagate_constraints()?;
      self = new_program;
      if !changed {
        break;
      }
    }
    Ok(self)
  }
  fn ensure_fully_typed(self) -> Result<Self, TypeError> {
    Ok(Self {
      expressions: self
        .expressions
        .into_iter()
        .map(|tree| {
          tree.walk(&mut |exp| Ok(exp), &mut |exp, (type_index, env)| {
            Ok(ExpNode {
              exp,
              data: match self.types.get_type(type_index) {
                TypeConstraints::Definitely(t) => Ok((type_index, env)),
                _ => Err(TypeError::CouldntInfer),
              }?,
            })
          })
        })
        .collect::<Result<_, TypeError>>()?,
      structs: self.structs,
      types: self.types,
      global_env: self.global_env,
    })
  }
}
