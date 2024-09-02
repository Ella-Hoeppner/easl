use std::env::Args;

use sse::syntax::EncloserOrOperator;

use crate::{
  compiler::ir::structs::UntypedStruct,
  parse::{Operator, TyntTree},
};

use super::{
  environment::Environment,
  expression::{
    Exp::{self, *},
    ExpNode,
  },
  metadata::Metadata,
  structs::Struct,
  types::{CompileError, TyntType, TypeMap},
};

struct TopLevelVar {
  name: String,
  metadata: Option<Metadata>,
  attributes: Vec<String>,
  var_type: TyntType,
}

struct TopLevelFunction {
  name: String,
  metadata: Option<Metadata>,
  arg_metadata: Vec<Option<Metadata>>,
  return_metadata: Option<Metadata>,
  exp: ExpNode<(usize, Environment)>,
}

struct Program {
  structs: Vec<Struct>,
  top_level_vars: Vec<TopLevelVar>,
  top_level_functions: Vec<TopLevelFunction>,
  types: TypeMap,
  global_env: Environment,
}

pub fn read_type_annotated_name(
  exp: TyntTree,
) -> Result<(String, String), CompileError> {
  if let TyntTree::Inner(
    (_, EncloserOrOperator::Operator(Operator::TypeAnnotation)),
    mut children,
  ) = exp
  {
    if let TyntTree::Leaf(_, type_name) = children.remove(1) {
      if let TyntTree::Leaf(_, name) = children.remove(0) {
        Ok((name, type_name))
      } else {
        Err(CompileError::ExpectedTypeAnnotatedName)
      }
    } else {
      Err(CompileError::ExpectedTypeAnnotatedName)
    }
  } else {
    Err(CompileError::ExpectedTypeAnnotatedName)
  }
}

fn extract_metadata(
  exp: TyntTree,
) -> Result<(Option<Metadata>, TyntTree), CompileError> {
  Ok(
    if let TyntTree::Inner(
      (_, EncloserOrOperator::Operator(Operator::MetadataAnnotation)),
      mut children,
    ) = exp
    {
      let exp = children.remove(1);
      (
        Some(Metadata::from_metadata_expression(children.remove(0))?),
        exp,
      )
    } else {
      (None, exp)
    },
  )
}

impl Program {
  fn init_from_tynt_trees(
    trees: Vec<TyntTree>,
    global_env: Environment,
  ) -> Result<Self, CompileError> {
    let mut non_struct_trees = vec![];
    let mut untyped_structs = vec![];
    for tree in trees.into_iter() {
      use crate::parse::Encloser::*;
      use crate::parse::Operator::*;
      use sse::syntax::EncloserOrOperator::*;
      let (metadata, tree_body) = extract_metadata(tree.clone())?;
      if let TyntTree::Inner((_, Encloser(Parens)), children) = tree_body {
        let mut children_iter = children.into_iter();
        if let Some(TyntTree::Leaf(_, first_child)) = children_iter.next() {
          if &first_child == "struct" {
            if let Some(TyntTree::Leaf(_, struct_name)) = children_iter.next() {
              untyped_structs.push(UntypedStruct::from_field_expressions(
                struct_name,
                children_iter.collect(),
              )?);
            } else {
              return Err(CompileError::InvalidStructName);
            }
          } else {
            non_struct_trees.push(tree)
          }
        } else {
          return Err(CompileError::UnrecognizedTopLevelForm);
        }
      } else {
        return Err(CompileError::UnrecognizedTopLevelForm);
      }
    }
    let struct_names: Vec<String> =
      untyped_structs.iter().map(|s| s.name.clone()).collect();
    let structs = untyped_structs
      .into_iter()
      .map(|untyped_struct| untyped_struct.assign_types(&struct_names))
      .collect::<Result<Vec<Struct>, CompileError>>()?;
    let mut top_level_vars = vec![];
    let mut top_level_functions = vec![];
    for tree in non_struct_trees.into_iter() {
      use crate::parse::Encloser::*;
      use crate::parse::Operator::*;
      use sse::syntax::EncloserOrOperator::*;
      let (metadata, tree_body) = extract_metadata(tree.clone())?;
      if let TyntTree::Inner((_, Encloser(Parens)), children) = tree_body {
        let mut children_iter = children.into_iter();
        if let Some(TyntTree::Leaf(_, first_child)) = children_iter.next() {
          match first_child.as_str() {
            "var" => {
              let (attributes, name_and_type_exp) = match children_iter.len() {
                1 => (vec![], children_iter.next().unwrap()),
                2 => {
                  let attributes_exp = children_iter.next().unwrap();
                  if let TyntTree::Inner(
                    (_, Encloser(Square)),
                    attribute_exps,
                  ) = attributes_exp
                  {
                    let attributes: Vec<String> = attribute_exps
                      .into_iter()
                      .map(|attribute_exp| {
                        if let TyntTree::Leaf(_, attribute_string) =
                          attribute_exp
                        {
                          Ok(attribute_string)
                        } else {
                          Err(CompileError::InvalidTopLevelVar(
                            "Expected leaf for attribute, found inner form"
                              .to_string(),
                          ))
                        }
                      })
                      .collect::<Result<_, CompileError>>()?;
                    (attributes, children_iter.next().unwrap())
                  } else {
                    return Err(CompileError::InvalidTopLevelVar(
                      "Expected square-bracket enclosed attributes".to_string(),
                    ));
                  }
                }
                _ => {
                  return Err(CompileError::InvalidTopLevelVar(
                    "Invalid number of inner forms".to_string(),
                  ))
                }
              };
              let (name, type_name) =
                read_type_annotated_name(name_and_type_exp)?;
              top_level_vars.push(TopLevelVar {
                name,
                metadata,
                attributes,
                var_type: TyntType::from_name(type_name, &struct_names)?,
              })
            }
            "def" => {
              let name_exp = children_iter
                .next()
                .ok_or(CompileError::InvalidDef("Missing Name".to_string()))?;
              if let TyntTree::Leaf(_, name) = name_exp {
                let value_exp = children_iter.next().ok_or(
                  CompileError::InvalidDef("Missing Value".to_string()),
                )?;
                match value_exp {
                  TyntTree::Inner(
                    (fn_range, Encloser(Parens)),
                    value_children,
                  ) => {
                    let mut value_children_iter = value_children.into_iter();
                    if let Some(TyntTree::Leaf(_, first_leaf)) =
                      value_children_iter.next()
                    {
                      if first_leaf == "fn".to_string() {
                        let signature_exp = value_children_iter.next().ok_or(
                          CompileError::InvalidFunction(
                            "Missing Argument List".to_string(),
                          ),
                        )?;
                        if let TyntTree::Inner(
                          (signature_range, Operator(TypeAnnotation)),
                          args_and_return_type,
                        ) = signature_exp
                        {
                          let (return_metadata, return_type) =
                            extract_metadata(args_and_return_type[1].clone())?;
                          let arg_list = args_and_return_type[0].clone();
                          if let TyntTree::Inner(
                            (_, Encloser(Square)),
                            arg_children,
                          ) = arg_list
                          {
                            let (arg_metadata, arg_expressions): (
                              Vec<Option<Metadata>>,
                              Vec<TyntTree>,
                            ) = arg_children
                              .into_iter()
                              .map(|arg| extract_metadata(arg))
                              .collect::<Result<_, CompileError>>()?;
                            let metadata_stripped_fn_expression =
                              TyntTree::Inner(
                                (fn_range, Encloser(Parens)),
                                std::iter::once(TyntTree::Inner(
                                  (signature_range, Operator(TypeAnnotation)),
                                  args_and_return_type,
                                ))
                                .chain(value_children_iter)
                                .collect(),
                              );
                            top_level_functions.push(TopLevelFunction {
                              name,
                              metadata,
                              arg_metadata,
                              return_metadata,
                              exp: todo!(),
                            })
                          } else {
                            return Err(CompileError::InvalidFunction(
                              "Invalid Argument List".to_string(),
                            ));
                          }
                        } else {
                          return Err(CompileError::InvalidFunction(
                            "Invalid Signature".to_string(),
                          ));
                        }
                      } else {
                        panic!("only fns are supported in def for now!!")
                      }
                    } else {
                      panic!("only fns are supported in def for now!!")
                    }
                  }
                  _ => panic!("only fns are supported in def for now!!"),
                }
              } else {
                return Err(CompileError::InvalidDef(
                  "Invalid Name".to_string(),
                ));
              }
            }
            _ => return Err(CompileError::UnrecognizedTopLevelForm),
          }
        } else {
          return Err(CompileError::UnrecognizedTopLevelForm);
        }
      } else {
        return Err(CompileError::UnrecognizedTopLevelForm);
      }
    }
    Ok(Self {
      structs,
      top_level_vars,
      top_level_functions,
      types: TypeMap::empty(),
      global_env,
    })
  }
  /*pub fn new(
    trees: Vec<TyntTree>,
    global_env: Environment,
  ) -> Result<Self, CompileError> {
    Ok(
      Self::unfinished_from_tynt_trees(trees, global_env)?
        .infer_initial_constraints()?
        .infer_types_until_fixed_point()?
        .ensure_fully_typed()?,
    )
  }*/
  /*fn infer_initial_constraints(mut self) -> Result<Self, TypeError> {
    let new_top_level_bindings = self
      .top_level_bindings
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
                  self.types.constrain(
                    type_index,
                    TypeConstraints::Definitely(fn_output_type),
                  )?;
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
      top_level_bindings: new_top_level_bindings,
      structs: self.structs,
      types: self.types,
      global_env: self.global_env,
    })
  }
  fn propagate_constraints(mut self) -> Result<(Self, bool), TypeError> {
    let mut changed = false;
    let new_top_level_bindings = self
      .top_level_bindings
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
        top_level_bindings: new_top_level_bindings,
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
      top_level_bindings: self
        .top_level_bindings
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
  }*/
}
