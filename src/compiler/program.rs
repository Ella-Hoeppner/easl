use std::{
  cell::RefCell,
  cmp::Ordering,
  collections::{HashMap, HashSet},
  rc::Rc,
};

use sse::{Ast, document::Document, syntax::EncloserOrOperator};
use take_mut::take;

use crate::{
  Never,
  compiler::{
    annotation::{FunctionAnnotation, extract_annotation},
    builtins::built_in_functions,
    effects::Effect,
    enums::{AbstractEnum, UntypedEnum},
    error::{CompileError, SourceTrace},
    expression::{
      Exp, ExpKind, ExpressionCompilationPosition,
      arg_list_and_return_type_from_easl_tree,
    },
    functions::{
      AbstractFunctionSignature, BuiltinArgumentAnnotation, EntryPoint,
      FunctionArgumentAnnotation,
    },
    structs::UntypedStruct,
    types::{
      AbstractType, Type, TypeConstraint, TypeState, UntypedType, Variable,
      VariableKind, parse_generic_argument,
    },
    util::compile_word,
  },
  parse::{EaslSyntax, EaslTree, Encloser, Operator, parse_easl},
};

use super::{
  builtins::{
    ABNORMAL_CONSTRUCTOR_STRUCTS, built_in_structs, built_in_type_aliases,
  },
  error::{
    CompileErrorKind::{self, *},
    CompileResult, ErrorLog,
  },
  expression::TypedExp,
  functions::{FunctionImplementationKind, TopLevelFunction},
  macros::{Macro, macroexpand},
  structs::AbstractStruct,
  vars::TopLevelVar,
};

pub type EaslDocument<'s> = Document<'s, EaslSyntax>;

pub trait EaslDocumentMethods {
  fn override_def(&mut self, def_name: &str, new_def_value: &str) -> bool;
}
impl<'s> EaslDocumentMethods for EaslDocument<'s> {
  fn override_def(&mut self, def_name: &str, new_def_value: &str) -> bool {
    if let Ok(parsed_document) = parse_easl(new_def_value)
      && let Some(new_value_ast) = parsed_document.syntax_trees.first()
    {
      for ast in self.syntax_trees.iter_mut() {
        if let Ast::Inner(
          (_, EncloserOrOperator::Encloser(Encloser::Parens)),
          children,
        ) = ast
          && let Some(Ast::Leaf(_, first_leaf)) = children.first()
          && first_leaf == "def"
          && let Some(Ast::Inner(
            (_, EncloserOrOperator::Operator(Operator::TypeAscription)),
            name_children,
          )) = children.get(2)
          && let Some(Ast::Leaf(_, binding_name)) = name_children.first()
          && binding_name == def_name
          && let Some(value) = children.get_mut(2)
        {
          *value = new_value_ast.clone();
          return true;
        }
      }
    }
    false
  }
}

thread_local! {
  pub static DEFAULT_PROGRAM: RefCell<Program> =
    RefCell::new(
      Program::empty()
        .with_functions(built_in_functions())
        .with_structs(
          built_in_structs().into_iter().map(|s| Rc::new(s)).collect(),
        )
        .with_type_aliases(built_in_type_aliases()));
}

#[derive(Debug, Clone)]
pub struct NameContext {
  user_names: HashSet<Rc<str>>,
  generated_names: HashSet<Rc<str>>,
  monomorphized_names: HashMap<(Rc<str>, Vec<Rc<str>>), Rc<str>>,
}

impl NameContext {
  fn empty() -> Self {
    Self {
      user_names: HashSet::new(),
      generated_names: HashSet::new(),
      monomorphized_names: HashMap::new(),
    }
  }
  fn track_all_ast_names(&mut self, ast: &EaslTree) {
    ast.walk(&mut |ast| {
      if let EaslTree::Leaf(_, name) = ast {
        self.track_user_name(name);
      }
    });
  }
  fn track_user_name(&mut self, name: &str) {
    self.user_names.insert(name.into());
    self.user_names.insert(compile_word(name.into()).into());
  }
  fn is_taken(&self, name: &str) -> bool {
    self.user_names.contains(name) || self.generated_names.contains(name)
  }
  pub fn gensym(&mut self, base_name: &str) -> Rc<str> {
    if self.is_taken(base_name) {
      let mut i = 0;
      let final_name: Rc<str> = loop {
        let modified_name = base_name.to_string() + &format!("_{i}");
        if !self.is_taken(&modified_name) {
          break modified_name.into();
        }
        i += 1;
      };
      self.generated_names.insert(final_name.clone());
      final_name
    } else {
      self.generated_names.insert(base_name.into());
      base_name.into()
    }
  }
  pub(crate) fn get_monomorphized_name(
    &mut self,
    base_type_name: Rc<str>,
    generic_arg_names: Vec<Rc<str>>,
  ) -> Rc<str> {
    let monomorphization_id = (base_type_name, generic_arg_names);
    self
      .monomorphized_names
      .get(&monomorphization_id)
      .map(|name| name.clone())
      .unwrap_or_else(|| {
        let full_name: Rc<str> = monomorphization_id
          .1
          .clone()
          .into_iter()
          .fold(
            monomorphization_id.0.to_string(),
            |full_name, generic_arg_name| full_name + "_" + &generic_arg_name,
          )
          .into();
        let final_name = self.gensym(&full_name);
        self.generated_names.insert(final_name.clone());
        self
          .monomorphized_names
          .insert(monomorphization_id, final_name.clone());
        final_name
      })
  }
}

#[derive(Debug, Clone)]
pub struct TypeDefs {
  pub structs: Vec<Rc<AbstractStruct>>,
  pub enums: Vec<Rc<AbstractEnum>>,
  pub type_aliases: Vec<(Rc<str>, Rc<AbstractStruct>)>,
}

impl TypeDefs {
  pub fn empty() -> Self {
    Self {
      structs: vec![],
      enums: vec![],
      type_aliases: vec![],
    }
  }
}

#[derive(Debug, Clone)]
pub struct Program {
  pub names: RefCell<NameContext>,
  pub typedefs: TypeDefs,
  pub abstract_functions:
    HashMap<Rc<str>, Vec<Rc<RefCell<AbstractFunctionSignature>>>>,
  pub top_level_vars: Vec<TopLevelVar>,
}

impl Default for Program {
  fn default() -> Self {
    DEFAULT_PROGRAM.with_borrow(|ctx| ctx.clone())
  }
}

impl Program {
  pub fn empty() -> Self {
    Self {
      names: NameContext::empty().into(),
      typedefs: TypeDefs::empty(),
      abstract_functions: HashMap::new(),
      top_level_vars: vec![],
    }
  }
  pub fn add_top_level_var(&mut self, var: TopLevelVar, errors: &mut ErrorLog) {
    if let Some(previous_var) = self
      .top_level_vars
      .iter()
      .find(|old_var| old_var.name == var.name)
    {
      errors.log(CompileError {
        kind: VariableNameCollision(var.name.to_string()),
        source_trace: var
          .source_trace
          .clone()
          .insert_as_secondary(previous_var.source_trace.clone()),
      })
    }
    self.names.borrow_mut().track_user_name(&var.name);
    self.top_level_vars.push(var);
  }
  pub fn add_abstract_function(
    &mut self,
    signature: Rc<RefCell<AbstractFunctionSignature>>,
  ) {
    let name = Rc::clone(&signature.borrow().name);
    self.names.borrow_mut().track_user_name(&name);
    if let FunctionImplementationKind::Composite(f) =
      &signature.borrow().implementation
    {
      let f = f.borrow();
      for arg_name in f.arg_names.iter() {
        self.names.borrow_mut().track_user_name(&arg_name);
      }
      f.body
        .walk(&mut |exp| {
          if let ExpKind::Name(name) = &exp.kind {
            self.names.borrow_mut().track_user_name(&name);
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
    if let Some(bucket) = self.abstract_functions.get_mut(&name) {
      let mut novel = true;
      for existing_signature in bucket.iter() {
        if existing_signature == &signature {
          novel = false;
          break;
        }
      }
      if novel {
        bucket.push(signature.into());
      }
    } else {
      self.abstract_functions.insert(name, vec![signature.into()]);
    }
  }
  pub fn with_functions(
    mut self,
    functions: Vec<AbstractFunctionSignature>,
  ) -> Self {
    for f in functions {
      self.add_abstract_function(Rc::new(RefCell::new(f)));
    }
    self
  }
  pub fn with_struct(mut self, s: Rc<AbstractStruct>) -> Self {
    if !self.typedefs.structs.contains(&s) {
      if !ABNORMAL_CONSTRUCTOR_STRUCTS.contains(&&*s.name) {
        self.add_abstract_function(Rc::new(RefCell::new(
          AbstractFunctionSignature {
            name: s.name.clone(),
            generic_args: s
              .generic_args
              .iter()
              .map(|name| (name.clone(), vec![]))
              .collect(),
            arg_types: s
              .fields
              .iter()
              .map(|field| field.field_type.clone())
              .collect(),
            mutated_args: vec![],
            return_type: AbstractType::AbstractStruct(s.clone()),
            implementation: FunctionImplementationKind::StructConstructor,
            associative: false,
          },
        )));
      }
      self.typedefs.structs.push(s);
      self.typedefs.structs.dedup();
    }
    self
  }
  pub fn with_enum(mut self, e: Rc<AbstractEnum>) -> Self {
    if !self.typedefs.enums.contains(&e) {
      if !ABNORMAL_CONSTRUCTOR_STRUCTS.contains(&&*e.name) {
        for variant in e.variants.iter() {
          if variant.inner_type != AbstractType::Type(Type::Unit) {
            self.add_abstract_function(Rc::new(RefCell::new(
              AbstractFunctionSignature {
                name: variant.name.clone(),
                generic_args: e
                  .generic_args
                  .iter()
                  .map(|name| (name.clone(), vec![]))
                  .collect(),
                arg_types: vec![variant.inner_type.clone()],
                mutated_args: vec![],
                return_type: AbstractType::AbstractEnum(e.clone()),
                implementation: FunctionImplementationKind::EnumConstructor(
                  variant.name.clone(),
                ),
                associative: false,
              },
            )));
          }
        }
      }
      self.typedefs.enums.push(e);
      self.typedefs.enums.dedup();
    }
    self
  }
  pub fn with_structs(self, structs: Vec<Rc<AbstractStruct>>) -> Self {
    structs.into_iter().fold(self, |ctx, s| ctx.with_struct(s))
  }
  pub fn with_type_aliases(
    mut self,
    mut aliases: Vec<(Rc<str>, Rc<AbstractStruct>)>,
  ) -> Self {
    self.typedefs.type_aliases.append(&mut aliases);
    self
  }
  pub fn add_monomorphized_struct(&mut self, s: Rc<AbstractStruct>) {
    if self
      .typedefs
      .structs
      .iter()
      .find(|existing_struct| {
        existing_struct.name == s.name
          && existing_struct.filled_generics == s.filled_generics
      })
      .is_none()
    {
      self.typedefs.structs.push(s);
    }
  }
  pub fn add_monomorphized_enum(&mut self, e: Rc<AbstractEnum>) {
    if self
      .typedefs
      .enums
      .iter()
      .find(|existing_struct| {
        existing_struct.name == e.name
          && existing_struct.filled_generics == e.filled_generics
      })
      .is_none()
    {
      self.typedefs.enums.push(e);
    }
  }
  pub fn concrete_signatures(
    &mut self,
    fn_name: &Rc<str>,
    source_trace: SourceTrace,
  ) -> CompileResult<Option<Vec<Type>>> {
    if let Some(signatures) = self.abstract_functions.get(fn_name) {
      signatures
        .into_iter()
        .map(|signature| {
          Ok(Type::Function(Box::new(
            AbstractFunctionSignature::concretize(
              Rc::new(RefCell::new(signature.borrow().clone())),
              &self.typedefs,
              source_trace.clone(),
            )?,
          )))
        })
        .collect::<CompileResult<Vec<_>>>()
        .map(|x| Some(x))
    } else {
      Ok(None)
    }
  }
  pub fn abstract_functions_iter(
    &self,
  ) -> impl Iterator<Item = &Rc<RefCell<AbstractFunctionSignature>>> {
    self
      .abstract_functions
      .values()
      .map(|fs| fs.iter())
      .flatten()
  }
  pub fn abstract_functions_iter_mut(
    &mut self,
  ) -> impl Iterator<Item = &mut Rc<RefCell<AbstractFunctionSignature>>> {
    self
      .abstract_functions
      .values_mut()
      .map(|fs| fs.iter_mut())
      .flatten()
  }
  pub fn from_easl_document(
    document: &'_ EaslDocument,
    macros: Vec<Macro>,
  ) -> (Self, ErrorLog) {
    let mut errors = ErrorLog::new();
    let mut names = NameContext::empty();
    for tree in document.syntax_trees.iter() {
      names.track_all_ast_names(tree);
    }
    let trees = document
      .syntax_trees
      .iter()
      .cloned()
      .map(|tree| macroexpand(tree, &macros, &mut names, &mut errors))
      .collect::<Vec<EaslTree>>();

    let mut non_typedef_trees = vec![];
    let mut untyped_types = vec![];

    for tree in trees.into_iter() {
      use crate::parse::Encloser::*;
      use sse::syntax::EncloserOrOperator::*;
      let (tree_body, annotation) =
        extract_annotation(tree.clone(), &mut errors);
      if let EaslTree::Inner((position, Encloser(Parens)), children) =
        &tree_body
      {
        let source_trace: SourceTrace = position.clone().into();
        let mut children_iter = children.into_iter();
        if let Some(EaslTree::Leaf(position, first_child)) =
          children_iter.next()
        {
          let source_trace: SourceTrace = position.clone().into();
          match first_child.as_str() {
            "struct" | "enum" => {
              if let Some(struct_name) = children_iter.next() {
                match struct_name {
                  EaslTree::Leaf(_, name) => match first_child.as_str() {
                    "struct" => match UntypedStruct::from_field_trees(
                      name.clone().into(),
                      vec![],
                      children_iter.cloned().collect(),
                      source_trace,
                    ) {
                      Ok(s) => untyped_types.push(UntypedType::Struct(s)),
                      Err(e) => errors.log(e),
                    },
                    "enum" => match UntypedEnum::from_field_trees(
                      name.clone().into(),
                      vec![],
                      children_iter.cloned().collect(),
                      source_trace,
                    ) {
                      Ok(e) => untyped_types.push(UntypedType::Enum(e)),
                      Err(e) => errors.log(e),
                    },
                    _ => unreachable!(),
                  },
                  EaslTree::Inner(
                    (position, Encloser(Parens)),
                    signature_children,
                  ) => {
                    let source_trace: SourceTrace = position.clone().into();
                    let (signature_leaves, signature_errors) = signature_children
                    .into_iter()
                    .map(|child| match child {
                      EaslTree::Leaf(_, name) => {
                        (Some(name.clone().into()), None)
                      }
                      _ => (
                        None,
                        Some(CompileError::new(
                          InvalidTypeName,
                          child.position().clone().into(),
                        )),
                      ),
                    })
                    .collect::<(Vec<Option<Rc<str>>>, Vec<Option<CompileError>>)>();
                    let filtered_signature_errors: Vec<CompileError> =
                      signature_errors.into_iter().filter_map(|x| x).collect();
                    if filtered_signature_errors.is_empty() {
                      let mut filtered_signature_leaves = signature_leaves
                        .into_iter()
                        .filter_map(|x| x)
                        .collect::<Vec<_>>()
                        .into_iter();
                      if let Some(type_name) = filtered_signature_leaves.next()
                      {
                        if filtered_signature_leaves.len() == 0 {
                          errors.log(CompileError::new(
                            InvalidTypeName,
                            source_trace,
                          ));
                        } else {
                          match first_child.as_str() {
                            "struct" => match UntypedStruct::from_field_trees(
                              type_name,
                              filtered_signature_leaves.collect(),
                              children_iter.cloned().collect(),
                              source_trace,
                            ) {
                              Ok(s) => {
                                untyped_types.push(UntypedType::Struct(s))
                              }
                              Err(e) => errors.log(e),
                            },
                            "enum" => match UntypedEnum::from_field_trees(
                              type_name,
                              filtered_signature_leaves.collect(),
                              children_iter.cloned().collect(),
                              source_trace,
                            ) {
                              Ok(e) => untyped_types.push(UntypedType::Enum(e)),
                              Err(e) => errors.log(e),
                            },
                            _ => unreachable!(),
                          }
                        }
                      } else {
                        errors.log(CompileError::new(
                          InvalidTypeName,
                          source_trace,
                        ));
                      }
                    } else {
                      errors.log_all(filtered_signature_errors);
                    }
                  }
                  EaslTree::Inner((position, _), _) => {
                    errors.log(CompileError::new(
                      InvalidTypeName,
                      position.clone().into(),
                    ));
                  }
                }
              } else {
                errors
                  .log(CompileError::new(InvalidTypeDefinition, source_trace));
              }
            }
            _ => non_typedef_trees.push((annotation, tree_body)),
          }
        } else {
          errors.log(CompileError::new(
            UnrecognizedTopLevelForm(tree_body.clone()),
            source_trace,
          ));
        }
      } else {
        errors.log(CompileError::new(
          UnrecognizedTopLevelForm(tree_body),
          tree.position().clone().into(),
        ));
      }
    }
    untyped_types.sort_by(|a, b| {
      if a.references_type_name(b.name()) {
        Ordering::Greater
      } else if b.references_type_name(a.name()) {
        Ordering::Less
      } else {
        Ordering::Equal
      }
    });
    for name in macros.iter().flat_map(|m| m.reserved_names.iter().cloned()) {
      names.user_names.insert(name);
    }
    let mut program = Program::default();
    program.names = names.into();
    for untyped_type in untyped_types {
      match untyped_type {
        UntypedType::Struct(untyped_struct) => {
          match untyped_struct.assign_types(&program.typedefs) {
            Ok(s) => program = program.with_struct(s.into()),
            Err(e) => errors.log(e),
          }
        }
        UntypedType::Enum(untyped_enum) => {
          match untyped_enum.assign_types(&program.typedefs) {
            Ok(e) => program = program.with_enum(e.into()),
            Err(e) => errors.log(e),
          }
        }
      }
    }

    for (annotation, tree) in non_typedef_trees.into_iter() {
      use crate::parse::Encloser::*;
      use sse::syntax::EncloserOrOperator::*;
      if let EaslTree::Inner((parens_position, Encloser(Parens)), children) =
        tree
      {
        let parens_source_trace: SourceTrace = parens_position.clone().into();
        let mut children_iter = children.into_iter();
        let first_child = children_iter.next();
        if let Some(EaslTree::Leaf(first_child_position, first_child)) =
          first_child
        {
          let first_child_source_trace: SourceTrace =
            first_child_position.clone().into();
          match first_child.as_str() {
            "var" | "def" | "override" => {
              if let Some(var) = TopLevelVar::from_ast(
                first_child.as_str(),
                &parens_source_trace,
                children_iter,
                &program,
                document,
                annotation,
                &mut errors,
              ) {
                program.add_top_level_var(var, &mut errors);
              }
            }
            "defn" => match children_iter.next() {
              Some(name_ast) => {
                let fn_and_generic_names: Option<(Rc<str>, Vec<_>)> =
                  match name_ast {
                    EaslTree::Leaf(_, name) => Some((name.into(), vec![])),
                    EaslTree::Inner((_, Encloser(Parens)), subtrees) => {
                      let mut subtrees_iter = subtrees.into_iter();
                      if let Some(EaslTree::Leaf(_, name)) =
                        subtrees_iter.next()
                      {
                        match subtrees_iter
                          .map(|subtree| {
                            parse_generic_argument(
                              subtree,
                              &program.typedefs,
                              &vec![],
                            )
                          })
                          .collect::<CompileResult<Vec<_>>>()
                        {
                          Ok(generic_args) => Some((name.into(), generic_args)),
                          Err(e) => {
                            errors.log(e);
                            None
                          }
                        }
                      } else {
                        errors.log(CompileError::new(
                          InvalidDefn("Invalid name".into()),
                          first_child_source_trace,
                        ));
                        None
                      }
                    }
                    _ => {
                      errors.log(CompileError::new(
                      InvalidDefn(
                        "Expected name or parens with name and generic arguments"
                          .into(),
                      ),
                      first_child_source_trace,
                    ));
                      None
                    }
                  };
                if let Some((fn_name, generic_args)) = fn_and_generic_names {
                  match children_iter.next() {
                    Some(arg_list_ast) => {
                      let generic_arg_names: Vec<Rc<str>> = generic_args
                        .iter()
                        .map(|(name, _)| name.clone())
                        .collect();
                      match arg_list_and_return_type_from_easl_tree(
                        arg_list_ast,
                        &program.typedefs,
                        &generic_arg_names,
                      ) {
                        Ok((
                          source_path,
                          arg_names,
                          arg_types,
                          arg_annotations,
                          return_type,
                          return_annotation,
                        )) => {
                          match return_type.concretize(
                            &generic_arg_names,
                            &program.typedefs,
                            source_path.clone().into(),
                          ) {
                            Ok(concrete_return_type) => {
                              match arg_types
                                .iter()
                                .zip(arg_annotations.iter())
                                .map(|(t, annotation)| {
                                  Ok((
                                    Variable {
                                      var_type: t
                                        .concretize(
                                          &generic_arg_names,
                                          &program.typedefs,
                                          source_path.clone().into(),
                                        )?
                                        .known()
                                        .into(),
                                      kind: if let Some(annotation) = annotation
                                        && annotation.var
                                      {
                                        VariableKind::Var
                                      } else {
                                        VariableKind::Let
                                      },
                                    },
                                    if let AbstractType::Generic(generic_name) =
                                      t
                                    {
                                      generic_args
                                        .iter()
                                        .find_map(|(name, constraints)| {
                                          (generic_name == name)
                                            .then(|| constraints.clone())
                                        })
                                        .unwrap_or(vec![])
                                    } else {
                                      vec![]
                                    },
                                  ))
                                })
                                .collect::<CompileResult<
                                  Vec<(Variable, Vec<TypeConstraint>)>,
                                >>() {
                                Ok(concrete_args) => {
                                  match TypedExp::function_from_body_tree(
                                    source_path.clone(),
                                    children_iter.collect(),
                                    concrete_return_type.known().into(),
                                    arg_names.clone(),
                                    concrete_args,
                                    &program.typedefs,
                                    &generic_arg_names,
                                  ) {
                                    Ok(body) => {
                                      let parsed_annotation = if let Some((
                                        annotation,
                                        annotation_source_trace,
                                      )) =
                                        &annotation
                                      {
                                        match annotation
                                          .validate_as_function_annotation(
                                            annotation_source_trace,
                                          ) {
                                          Ok(is_associative) => is_associative,
                                          Err(e) => {
                                            errors.log(e);
                                            FunctionAnnotation::default()
                                          }
                                        }
                                      } else {
                                        FunctionAnnotation::default()
                                      };
                                      let all_arg_annotations: Vec<
                                        FunctionArgumentAnnotation,
                                      > = arg_annotations
                                        .iter()
                                        .cloned()
                                        .filter_map(|x| x)
                                        .collect();
                                      for annotation in
                                        all_arg_annotations.iter()
                                      {
                                        if let Some(builtin) =
                                          &annotation.builtin
                                        {
                                          if let Some(entry) =
                                            parsed_annotation.entry
                                          {
                                            if !builtin.allowed_for_entry(entry)
                                            {
                                              errors.log(CompileError {
                                                kind:
                                                  BuiltinArgumentsOnWrongEntry(
                                                    builtin.name().to_string(),
                                                    entry.name().to_string(),
                                                  ),
                                                source_trace: source_path
                                                  .clone(),
                                              });
                                            }
                                          } else {
                                            errors.log(CompileError {
                                              kind: BuiltinArgumentsOnlyAllowedOnEntry,
                                              source_trace: source_path.clone(),
                                            });
                                          }
                                        }
                                      }
                                      let all_builtins: Vec<
                                        BuiltinArgumentAnnotation,
                                      > = all_arg_annotations
                                        .iter()
                                        .filter_map(|a| a.builtin.clone())
                                        .collect();
                                      if all_builtins
                                        .iter()
                                        .collect::<HashSet<_>>()
                                        .len()
                                        < all_builtins.len()
                                      {
                                        errors.log(CompileError {
                                          kind: DuplicateBuiltinArgument,
                                          source_trace: source_path,
                                        });
                                      }
                                      let implementation =
                                        FunctionImplementationKind::Composite(
                                          Rc::new(RefCell::new(
                                            TopLevelFunction {
                                              arg_names,
                                              arg_annotations,
                                              return_annotation,
                                              entry_point: parsed_annotation
                                                .entry,
                                              body,
                                            },
                                          )),
                                        );
                                      program.add_abstract_function(Rc::new(
                                        RefCell::new(
                                          AbstractFunctionSignature {
                                            name: fn_name,
                                            generic_args,
                                            arg_types,
                                            mutated_args: vec![],
                                            return_type,
                                            implementation,
                                            associative: parsed_annotation
                                              .associative,
                                          },
                                        ),
                                      ));
                                    }
                                    Err(e) => errors.log(e),
                                  }
                                }
                                Err(e) => {
                                  errors.log(e);
                                }
                              }
                            }
                            Err(e) => {
                              errors.log(e);
                            }
                          }
                        }
                        Err(e) => errors.log(e),
                      }
                    }
                    None => errors.log(CompileError::new(
                      InvalidDefn("Missing Argument List".into()),
                      parens_source_trace.clone(),
                    )),
                  }
                }
              }
              None => errors.log(CompileError::new(
                InvalidDefn("Missing Name".into()),
                parens_source_trace.clone(),
              )),
            },
            _ => {
              errors.log(CompileError::new(
                UnrecognizedTopLevelForm(EaslTree::Leaf(
                  first_child_position.clone(),
                  first_child,
                )),
                first_child_source_trace,
              ));
            }
          }
        } else {
          errors.log(CompileError::new(
            UnrecognizedTopLevelForm(first_child.unwrap_or(EaslTree::Inner(
              (
                parens_position.clone(),
                EncloserOrOperator::Encloser(Parens),
              ),
              vec![],
            ))),
            parens_source_trace,
          ));
        }
      } else {
        errors.log(CompileError::new(
          UnrecognizedTopLevelForm(tree.clone()),
          tree.position().clone().into(),
        ));
      }
    }
    (program, errors)
  }
  fn propagate_types(&mut self, errors: &mut ErrorLog) -> bool {
    let mut base_context = self.clone();
    let mut anything_changed = false;
    for var in self.top_level_vars.iter_mut() {
      if let Some(value_expression) = &mut var.value {
        let changed = value_expression.data.constrain(
          var.var_type.clone().known(),
          &var.source_trace,
          errors,
        );
        anything_changed |= changed;
        let changed =
          value_expression.propagate_types(&mut base_context, errors);
        anything_changed |= changed;
      }
    }
    for f in self.abstract_functions_iter_mut() {
      if let FunctionImplementationKind::Composite(implementation) =
        &f.borrow().implementation
      {
        let changed = implementation
          .borrow_mut()
          .body
          .propagate_types(&mut base_context, errors);
        anything_changed |= changed;
      }
    }
    anything_changed
  }
  fn find_untyped(&mut self) -> Vec<SourceTrace> {
    self
      .abstract_functions_iter()
      .map(|f| {
        if let FunctionImplementationKind::Composite(implementation) =
          &f.borrow().implementation
        {
          implementation.borrow_mut().body.find_untyped()
        } else {
          vec![]
        }
      })
      .collect::<Vec<_>>()
      .into_iter()
      .chain(self.top_level_vars.iter_mut().map(|v| {
        if let Some(value) = &mut v.value {
          value.find_untyped()
        } else {
          vec![]
        }
        .into_iter()
        .chain(
          (!v.var_type.check_is_fully_known())
            .then(|| v.source_trace.clone())
            .into_iter(),
        )
        .collect()
      }))
      .flatten()
      .collect()
  }
  pub fn validate_match_blocks(&self, errors: &mut ErrorLog) {
    for abstract_function in self.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(implementation) =
        &abstract_function.borrow().implementation
      {
        (**implementation)
          .borrow_mut()
          .body
          .validate_match_blocks(errors);
      }
    }
  }
  pub fn fully_infer_types(&mut self, errors: &mut ErrorLog) {
    loop {
      let did_type_states_change = self.propagate_types(errors);
      if !did_type_states_change {
        let untyped_expressions = self.find_untyped();
        return if untyped_expressions.is_empty() {
          break;
        } else {
          for source_trace in untyped_expressions {
            let source_trace = source_trace;
            errors.log(CompileError::new(CouldntInferTypes, source_trace));
          }
        };
      }
    }
  }
  pub fn validate_assignments(&mut self, errors: &mut ErrorLog) {
    for abstract_f in self.abstract_functions_iter() {
      let abstract_f = abstract_f.borrow();
      if let FunctionImplementationKind::Composite(implementation) =
        &abstract_f.implementation
      {
        let implementation = implementation.borrow_mut();
        if let Err(e) = implementation.body.validate_assignments(self) {
          errors.log(e);
        }
      }
    }
  }
  pub fn monomorphize(&mut self, errors: &mut ErrorLog) {
    let mut monomorphized_ctx = Program::default();
    monomorphized_ctx.names = self.names.clone();
    for f in self.abstract_functions_iter() {
      if f.borrow().generic_args.is_empty() {
        if let FunctionImplementationKind::Composite(implementation) =
          &f.borrow().implementation
        {
          let mut borrowed_implementation = implementation.borrow_mut();
          match borrowed_implementation
            .body
            .monomorphize(&self, &mut monomorphized_ctx)
          {
            Ok(_) => {
              let mut new_f = (**f).borrow().clone();
              new_f.implementation =
                FunctionImplementationKind::Composite(implementation.clone());
              drop(borrowed_implementation);
              monomorphized_ctx
                .add_abstract_function(Rc::new(RefCell::new(new_f)));
            }
            Err(e) => errors.log(e),
          }
        } else {
          monomorphized_ctx.add_abstract_function(Rc::clone(f));
        }
      }
    }
    for s in self.typedefs.structs.iter() {
      if s.generic_args.is_empty() {
        monomorphized_ctx.add_monomorphized_struct(s.clone());
      }
    }
    take(self, |old_ctx| {
      monomorphized_ctx.top_level_vars = old_ctx.top_level_vars;
      monomorphized_ctx
    });
  }
  pub fn inline_all_higher_order_arguments(&mut self, errors: &mut ErrorLog) {
    loop {
      let changed = self.inline_higher_order_arguments(errors);
      if !errors.is_empty() || !changed {
        break;
      }
    }
  }
  pub fn inline_higher_order_arguments(
    &mut self,
    errors: &mut ErrorLog,
  ) -> bool {
    let mut changed = false;
    let mut inlined_ctx = Program::default();
    inlined_ctx.names = self.names.clone();
    for f in self.abstract_functions_iter() {
      if f.borrow().generic_args.is_empty()
        && f
          .borrow()
          .arg_types
          .iter()
          .find(|t| {
            if let AbstractType::Type(Type::Function(_)) = t {
              true
            } else {
              false
            }
          })
          .is_none()
      {
        match &f.borrow().implementation {
          FunctionImplementationKind::Composite(implementation) => {
            let mut borrowed_implementation = implementation.borrow_mut();
            match borrowed_implementation
              .body
              .inline_higher_order_arguments(&mut inlined_ctx)
            {
              Ok(added_new_function) => {
                changed |= added_new_function;
                let mut new_f = (**f).borrow().clone();
                new_f.implementation =
                  FunctionImplementationKind::Composite(implementation.clone());
                drop(borrowed_implementation);
                inlined_ctx.add_abstract_function(Rc::new(RefCell::new(new_f)));
              }
              Err(e) => errors.log(e),
            }
          }
          FunctionImplementationKind::EnumConstructor(_) => {
            inlined_ctx.add_abstract_function(Rc::clone(f));
          }
          _ => {}
        }
      }
    }
    take(self, |old_ctx| {
      inlined_ctx.typedefs = old_ctx.typedefs;
      inlined_ctx.top_level_vars = old_ctx.top_level_vars;
      inlined_ctx
    });
    changed
  }
  pub fn compile_to_wgsl(self) -> CompileResult<String> {
    let mut names = self.names.borrow_mut();
    let mut wgsl = String::new();
    for v in self.top_level_vars.iter() {
      wgsl += &v.clone().compile(&mut names);
      wgsl += ";\n";
    }
    wgsl += "\n";
    let default_structs = built_in_structs();
    for s in self
      .typedefs
      .structs
      .iter()
      .cloned()
      .filter(|s| !default_structs.contains(s))
    {
      if let Some(compiled_struct) = Rc::unwrap_or_clone(s)
        .compile_if_non_generic(&self.typedefs, &mut names)?
      {
        wgsl += &compiled_struct;
        wgsl += "\n\n";
      }
    }
    for e in self.typedefs.enums.iter().cloned() {
      if let Some(compiled_enum) = Rc::unwrap_or_clone(e)
        .compile_if_non_generic(&self.typedefs, &mut names)?
      {
        wgsl += &compiled_enum;
        wgsl += "\n\n";
      }
    }
    for f in self.abstract_functions_iter() {
      let f = f.borrow().clone();
      if f.generic_args.is_empty() {
        match f.implementation {
          FunctionImplementationKind::EnumConstructor(
            original_variant_name,
          ) => {
            let variant_name = f.name;
            let AbstractType::AbstractEnum(e) = f.return_type else {
              unreachable!("EnumConstructor fn had a non-enum type")
            };
            let (discriminant, variant) = e
              .variants
              .iter()
              .enumerate()
              .find(|(_, v)| v.name == original_variant_name)
              .expect("EnumConstructor fn name didn't match any variant");
            let AbstractType::Type(inner_type) = &variant.inner_type else {
              unreachable!()
            };
            let args_str = if *inner_type == Type::Unit {
              String::new()
            } else {
              let inner_type_name = inner_type.monomorphized_name(&mut names);
              format!("value: {inner_type_name}")
            };
            let bitcast_inner_values = inner_type
              .bitcastable_chunk_accessors("value".into())
              .into_iter()
              .map(|exp| {
                format!(
                  "bitcast<u32>({})",
                  exp.compile(
                    ExpressionCompilationPosition::InnerExpression,
                    &mut names
                  )
                )
              })
              .chain(std::iter::repeat("0u".into()))
              .take(e.inner_data_size_in_u32s()?)
              .collect::<Vec<String>>()
              .join(", ");
            let enum_name = e.original_ancestor().monomorphized_name(
              &e.variants
                .iter()
                .map(|variant| {
                  let AbstractType::Type(t) = &variant.inner_type else {
                    unreachable!()
                  };
                  t.clone()
                })
                .collect(),
              &mut names,
            );
            wgsl += &format!(
              "fn {variant_name}({args_str}) -> {enum_name} {{\n  \
              return {enum_name}({discriminant}u, array({bitcast_inner_values}));\n\
              }}"
            );
            wgsl += "\n\n";
          }
          _ => {}
        }
      }
    }
    for f in self.abstract_functions_iter() {
      let f = f.borrow().clone();
      if f.generic_args.is_empty() {
        match f.implementation {
          FunctionImplementationKind::Composite(implementation) => {
            wgsl += &implementation
              .borrow()
              .clone()
              .compile(&f.name, &mut names)?;
            wgsl += "\n\n";
          }
          _ => {}
        }
      }
    }
    Ok(wgsl)
  }
  pub fn expand_associative_applications(&mut self) {
    for f in self
      .abstract_functions
      .iter_mut()
      .map(|(_, fns)| fns.into_iter())
      .flatten()
    {
      if let FunctionImplementationKind::Composite(f) =
        &f.borrow().implementation
      {
        f.borrow_mut()
          .body
          .walk_mut::<()>(&mut |exp| {
            take(&mut exp.kind, |exp_kind| {
              if let ExpKind::Application(f, args) = exp_kind {
                if let ExpKind::Name(_) = &f.kind
                  && let Type::Function(x) = f.data.kind.unwrap_known()
                  && let Some(abstract_ancestor) = &x.abstract_ancestor
                  && abstract_ancestor.associative
                  && args.len() != 2
                {
                  let mut args_iter = args.into_iter();
                  let mut new_exp = args_iter.next().unwrap();
                  while let Some(next_arg) = args_iter.next() {
                    new_exp = Exp {
                      kind: ExpKind::Application(
                        f.clone(),
                        vec![new_exp, next_arg],
                      ),
                      data: exp.data.clone(),
                      source_trace: exp.source_trace.clone(),
                    };
                  }
                  new_exp.kind
                } else {
                  ExpKind::Application(f, args)
                }
              } else {
                exp_kind
              }
            });
            Ok(true)
          })
          .unwrap();
      }
    }
  }
  fn deshadow(&mut self, errors: &mut ErrorLog) {
    let globally_bound_names: Vec<Rc<str>> = self
      .top_level_vars
      .iter()
      .map(|v| Rc::clone(&v.name))
      .chain(
        self
          .abstract_functions
          .iter()
          .map(|(name, _)| Rc::clone(name)),
      )
      .collect();
    for (_, signatures) in self.abstract_functions.iter_mut() {
      for signature in signatures.iter_mut() {
        let mut signature = signature.borrow_mut();
        if let FunctionImplementationKind::Composite(f) =
          &mut signature.implementation
        {
          f.borrow_mut().body.deshadow(
            &globally_bound_names,
            errors,
            &mut self.names.borrow_mut(),
          );
        }
      }
    }
  }
  fn wrap_mutable_function_args(&mut self) {
    for signature in self.abstract_functions_iter() {
      if let FunctionImplementationKind::Composite(implementation) =
        &signature.borrow().implementation
      {
        let mut implementation = implementation.borrow_mut();
        if let Type::Function(f) = &mut implementation.body.data.unwrap_known()
          && let ExpKind::Function(arg_names, body) =
            &mut implementation.body.kind
        {
          let mutable_args: Vec<_> = f
            .args
            .iter()
            .zip(arg_names.iter())
            .filter_map(|((var, _), arg_name)| {
              if var.kind == VariableKind::Var {
                Some((arg_name.clone(), var.var_type.clone()))
              } else {
                None
              }
            })
            .collect();
          if mutable_args.len() > 0 {
            take(body, |body| {
              TypedExp {
                data: body.data.clone(),
                source_trace: body.source_trace.clone(),
                kind: ExpKind::Let(
                  mutable_args
                    .into_iter()
                    .map(|(arg_name, arg_type)| {
                      (
                        arg_name.clone(),
                        VariableKind::Var,
                        TypedExp {
                          data: arg_type.clone(),
                          kind: ExpKind::Name(arg_name),
                          source_trace: body.source_trace.clone(),
                        },
                      )
                    })
                    .collect(),
                  body,
                ),
              }
              .into()
            });
          }
        }
      }
    }
  }
  fn validate_associative_signatures(&self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if signature.associative
        && (signature.arg_types.len() != 2
          || signature.arg_types[0] != signature.arg_types[1]
          || signature.arg_types[0] != signature.return_type)
      {
        if let FunctionImplementationKind::Composite(implementation) =
          &signature.implementation
        {
          errors.log(CompileError {
            kind: CompileErrorKind::InvalidAssociativeSignature,
            source_trace: implementation.borrow().body.source_trace.clone(),
          });
        }
      }
    }
  }
  fn catch_duplicate_signatures(&self, errors: &mut ErrorLog) {
    for (name, signatures) in self.abstract_functions.iter() {
      let mut normalized_signatures: Vec<(Option<SourceTrace>, _)> = vec![];
      for signature in signatures {
        if let FunctionImplementationKind::Builtin(_)
        | FunctionImplementationKind::StructConstructor =
          signature.borrow().implementation
        {
          let normalized = signature.borrow().normalized_signature();
          normalized_signatures.push((None, normalized));
        }
      }
      for signature in signatures {
        if let FunctionImplementationKind::Composite(f) =
          &signature.borrow().implementation
        {
          let source = f.borrow().body.source_trace.clone();
          let normalized = signature.borrow().normalized_signature();
          for (previous_signature, previous_normalized) in
            normalized_signatures.iter()
          {
            if *previous_normalized == normalized {
              if let Some(previous_source) = previous_signature {
                errors.log(CompileError {
                  kind: CompileErrorKind::DuplicateFunctionSignature(
                    name.to_string(),
                  ),
                  source_trace: source
                    .clone()
                    .insert_as_secondary(previous_source.clone()),
                });
              } else {
                errors.log(CompileError {
                  kind: CompileErrorKind::FunctionSignatureConflictsWithBuiltin(
                    name.to_string(),
                  ),
                  source_trace: source.clone(),
                });
              }
            }
          }
          normalized_signatures.push((Some(source), normalized));
        }
      }
    }
  }
  fn catch_globally_shadowing_fn_args(&self, errors: &mut ErrorLog) {
    for (_, signatures) in self.abstract_functions.iter() {
      for signature in signatures {
        if let FunctionImplementationKind::Composite(f) =
          &signature.borrow().implementation
        {
          let f = f.borrow();
          for arg_name in f.arg_names.iter() {
            if self.abstract_functions.get(arg_name).is_some()
              || self
                .top_level_vars
                .iter()
                .find(|v| v.name == *arg_name)
                .is_some()
            {
              errors.log(CompileError::new(
                CantShadowTopLevelBinding(arg_name.to_string()),
                f.body.source_trace.clone(),
              ))
            }
          }
        }
      }
    }
  }
  fn catch_top_level_function_and_var_name_collisions(
    &self,
    errors: &mut ErrorLog,
  ) {
    for var in self.top_level_vars.iter() {
      if self.abstract_functions.get(&var.name).is_some() {
        errors.log(CompileError {
          kind: VariableFunctionNameCollision(var.name.to_string()),
          source_trace: var.source_trace.clone(),
        })
      }
    }
  }
  fn ensure_no_typeless_bindings(&self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(implementation) =
        &signature.implementation
      {
        implementation
          .borrow()
          .body
          .walk(&mut |exp| {
            match &exp.kind {
              ExpKind::Let(items, _) => {
                for (_, _, value) in items.iter() {
                  if Type::Unit.known() == value.data.kind {
                    errors.log(CompileError {
                      kind: CompileErrorKind::TypelessBinding,
                      source_trace: value.source_trace.clone(),
                    });
                  }
                }
              }
              _ => {}
            }
            Ok::<_, Never>(true)
          })
          .unwrap();
      }
    }
  }
  pub fn validate_control_flow(&mut self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        f.borrow().body.validate_control_flow(errors, 0);
      }
    }
  }
  pub fn deexpressionify(&mut self) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        f.borrow_mut().body.deexpressionify(self);
      }
    }
  }
  pub fn separate_overloaded_fns(&mut self) {
    let mut renames = HashMap::new();
    for (_, signatures) in self.abstract_functions.iter() {
      if signatures.len() > 1 {
        for s in signatures.iter() {
          let mut s = s.borrow_mut();
          let base_name = s.name.clone();
          let type_signature = if s.generic_args.is_empty()
            && let FunctionImplementationKind::Composite(f) =
              &mut s.implementation
            && let Type::Function(f) = f.borrow().body.data.unwrap_known()
          {
            f.unwrap_type_signature()
          } else {
            continue;
          };
          let new_name = base_name.to_string()
            + "_"
            + &type_signature
              .iter()
              .map(|t| t.monomorphized_name(&mut self.names.borrow_mut()))
              .collect::<Vec<String>>()
              .join("_");
          let new_name: Rc<str> = new_name.into();
          s.name = new_name.clone();
          if !renames.contains_key(&base_name) {
            renames.insert(base_name.clone(), vec![]);
          }
          renames
            .get_mut(&base_name)
            .unwrap()
            .push((type_signature, new_name));
        }
      }
    }
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        f.borrow_mut()
          .body
          .walk_mut(&mut |exp| {
            if let ExpKind::Name(name) = &mut exp.kind {
              if let Some(renames) = renames.get(name)
                && let Type::Function(f) = exp.data.unwrap_known()
              {
                let f_signature = f.unwrap_type_signature();
                for (signature, rename) in renames.iter() {
                  if signature == &f_signature {
                    *name = rename.clone();
                  }
                }
              }
              Ok::<bool, Never>(false)
            } else {
              Ok(true)
            }
          })
          .unwrap();
      }
    }
  }
  pub fn desugar_swizzle_assignments(&mut self) {
    let mut names = self.names.borrow_mut();
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        f.borrow_mut()
          .body
          .walk_mut(&mut |exp| {
            exp.desugar_swizzle_assignments(&mut names);
            Ok::<_, Never>(true)
          })
          .unwrap();
      }
    }
  }
  pub fn validate_top_level_fn_effects(&mut self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        let f = f.borrow();
        if let Some(entry_point) = f.entry_point
          && let EntryPoint::Vertex | EntryPoint::Compute(_) = entry_point
        {
          let ExpKind::Function(_, body) = &f.body.kind else {
            unreachable!()
          };
          let effects = body.effects(self);
          for e in effects.0.iter() {
            match e {
              Effect::Discard => {
                errors.log(CompileError {
                  kind: DiscardOutsideFragment,
                  source_trace: f.body.source_trace.clone(),
                });
              }
              Effect::FragmentExclusiveFunction(name) => {
                errors.log(CompileError {
                  kind: FragmentExclusiveFunctionOutsideFragment(
                    name.to_string(),
                  ),
                  source_trace: f.body.source_trace.clone(),
                });
              }
              _ => {}
            }
          }
        }
      }
    }
  }
  pub fn validate_raw_program(&mut self) -> ErrorLog {
    let mut errors = ErrorLog::new();
    self.validate_associative_signatures(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.wrap_mutable_function_args();
    self.deshadow(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_globally_shadowing_fn_args(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_top_level_function_and_var_name_collisions(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.fully_infer_types(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_control_flow(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.ensure_no_typeless_bindings(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.expand_associative_applications();
    self.validate_assignments(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_duplicate_signatures(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_match_blocks(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.monomorphize(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.inline_all_higher_order_arguments(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_top_level_fn_effects(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.desugar_swizzle_assignments();
    self.deexpressionify();
    self.separate_overloaded_fns();
    errors
  }
  pub fn gather_type_annotations(&self) -> Vec<(SourceTrace, TypeState)> {
    let mut type_annotations = vec![];
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      let FunctionImplementationKind::Composite(implementation) =
        &signature.implementation
      else {
        continue;
      };
      implementation
        .borrow()
        .body
        .walk(&mut |exp: &TypedExp| {
          type_annotations
            .push((exp.source_trace.clone(), exp.data.kind.clone()));
          Ok::<_, Never>(true)
        })
        .unwrap();
    }
    type_annotations
  }
  pub fn find_fn_names_by_entry_point(
    &self,
    entry_kind_predicate: impl Fn(EntryPoint) -> bool,
  ) -> Vec<String> {
    self
      .abstract_functions_iter()
      .filter_map(|abstract_f| {
        let abstract_f = abstract_f.borrow();
        if let FunctionImplementationKind::Composite(f) =
          &abstract_f.implementation
          && let Some(entry_point) = f.borrow().entry_point
          && entry_kind_predicate(entry_point)
        {
          Some(abstract_f.name.to_string())
        } else {
          None
        }
      })
      .collect()
  }
}
