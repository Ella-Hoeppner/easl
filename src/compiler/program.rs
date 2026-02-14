use std::{
  cell::RefCell,
  collections::{HashMap, HashSet},
  rc::Rc,
};

use fsexp::{Ast, document::Document, syntax::EncloserOrOperator};
use take_mut::take;

use crate::{
  Never,
  compiler::{
    annotation::extract_annotation,
    builtins::built_in_functions,
    effects::Effect,
    entry::{
      BuiltinIOAttribute, EntryPoint, IOAttribute, IOAttributeKind,
      IOAttributes, InputOrOutput,
    },
    enums::{AbstractEnum, UntypedEnum},
    error::{CompileError, SourceTrace, err},
    expression::{
      Accessor, Exp, ExpKind, ExpressionCompilationPosition, Number,
    },
    functions::{
      AbstractFunctionSignature, FunctionArgumentAnnotation, FunctionSignature,
      Ownership, TopLevelFunction,
    },
    structs::{AbstractStructField, UntypedStruct},
    types::{
      AbstractType, ConcreteArraySize, GenericArgument,
      ImmutableProgramLocalContext, NameDefinitionSource, Type, TypeState,
      UntypedType, Variable, VariableKind,
    },
    util::{compile_word, is_valid_name},
    vars::TopLevelVariableKind,
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
  functions::FunctionImplementationKind,
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
    let new_def_document = parse_easl(new_def_value);
    if let Some(new_value_ast) = new_def_document.syntax_trees.first() {
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
    if generic_arg_names.is_empty() {
      return base_type_name;
    }
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
  pub structs: Vec<AbstractStruct>,
  pub enums: Vec<AbstractEnum>,
  pub type_aliases: Vec<(Rc<str>, Rc<AbstractStruct>)>,
}

impl TypeDefs {
  pub fn get_attributable_components(
    &self,
    t: Type,
    input_or_output: InputOrOutput,
    source_trace: SourceTrace,
    errors: &mut ErrorLog,
  ) -> Vec<(Rc<AbstractStruct>, Rc<str>, IOAttributes)> {
    match t {
      Type::Struct(s) => s
        .fields
        .iter()
        .filter_map(|f| {
          if f.field_type.unwrap_known().is_attributable() {
            Some((
              s.abstract_ancestor.clone(),
              f.name.clone(),
              f.attributes.clone(),
            ))
          } else {
            errors.log(CompileError::new(
              CantAssignAttributesToFieldOfType(f.name.to_string()),
              source_trace
                .clone()
                .insert_as_secondary(s.abstract_ancestor.source_trace.clone()),
            ));
            None
          }
        })
        .collect(),
      Type::Unit => vec![],
      _ => {
        errors.log(CompileError::new(
          EntryInputOrOutputMustBeScalarOrStruct(input_or_output),
          source_trace,
        ));
        vec![]
      }
    }
  }
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
      for (arg_name, _) in f.arg_names.iter() {
        self.names.borrow_mut().track_user_name(&arg_name);
      }
      f.expression
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
      if !ABNORMAL_CONSTRUCTOR_STRUCTS.contains(&&*s.name.0) {
        self.add_abstract_function(Rc::new(RefCell::new(
          AbstractFunctionSignature {
            name: s.name.0.clone(),
            generic_args: s.generic_args.clone(),
            arg_types: s
              .fields
              .iter()
              .map(|field| (field.field_type.clone(), Ownership::Owned))
              .collect(),
            return_type: AbstractType::AbstractStruct(s.clone()),
            implementation: FunctionImplementationKind::StructConstructor,
            associative: false,
            captured_scope: None,
          },
        )));
      }
      self.typedefs.structs.push(s.as_ref().clone());
      self.typedefs.structs.dedup();
    }
    self
  }
  pub fn with_enum(mut self, e: AbstractEnum) -> Self {
    if !self.typedefs.enums.contains(&e) {
      if !ABNORMAL_CONSTRUCTOR_STRUCTS.contains(&&*e.name.0) {
        for variant in e.variants.iter() {
          if variant.inner_type != AbstractType::Type(Type::Unit) {
            self.add_abstract_function(Rc::new(RefCell::new(
              AbstractFunctionSignature {
                name: variant.name.clone(),
                generic_args: e.generic_args.clone(),
                arg_types: vec![(variant.inner_type.clone(), Ownership::Owned)],
                return_type: AbstractType::AbstractEnum(e.clone().into()),
                implementation: FunctionImplementationKind::EnumConstructor(
                  variant.name.clone(),
                ),
                associative: false,
                captured_scope: None,
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
  pub fn add_monomorphized_struct(&mut self, s: AbstractStruct) {
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
  pub fn add_monomorphized_enum(&mut self, e: AbstractEnum) {
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
      use fsexp::syntax::EncloserOrOperator::*;
      let (tree_body, annotation) =
        extract_annotation(tree.clone(), &mut errors);
      let EaslTree::Inner((position, Encloser(Parens)), children) = &tree_body
      else {
        errors.log(CompileError::new(
          UnrecognizedTopLevelForm(tree_body),
          tree.position().clone().into(),
        ));
        continue;
      };
      let source_trace: SourceTrace = position.clone().into();
      let mut children_iter = children.into_iter();
      let Some(EaslTree::Leaf(position, first_child)) = children_iter.next()
      else {
        errors.log(CompileError::new(
          UnrecognizedTopLevelForm(tree_body.clone()),
          source_trace,
        ));
        continue;
      };
      let source_trace: SourceTrace = position.clone().into();
      match first_child.as_str() {
        "struct" | "enum" => {
          if annotation.is_some() {
            errors.log(CompileError {
              kind: AnnotationNotAllowedOnType,
              source_trace: source_trace.clone(),
            });
          }
          if let Some(struct_name) = children_iter.next() {
            match struct_name {
              EaslTree::Leaf(pos, name) => match first_child.as_str() {
                "struct" => untyped_types.push(UntypedType::Struct(
                  UntypedStruct::from_field_trees(
                    (name.clone().into(), pos.into()),
                    vec![],
                    children_iter.cloned().collect(),
                    source_trace,
                    &mut errors,
                  ),
                )),
                "enum" => match UntypedEnum::from_field_trees(
                  (name.clone().into(), pos.into()),
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
                    EaslTree::Leaf(pos, name) => {
                      (Some((name.clone().into(), pos.into())), None)
                    }
                    _ => (
                      None,
                      Some(CompileError::new(
                        InvalidTypeName,
                        child.position().clone().into(),
                      )),
                    ),
                  })
                  .collect::<(
                    Vec<Option<(Rc<str>, SourceTrace)>>,
                    Vec<Option<CompileError>>,
                  )>();
                let filtered_signature_errors: Vec<CompileError> =
                  signature_errors.into_iter().filter_map(|x| x).collect();
                if filtered_signature_errors.is_empty() {
                  let mut filtered_signature_leaves = signature_leaves
                    .into_iter()
                    .filter_map(|x| x)
                    .collect::<Vec<_>>()
                    .into_iter();
                  if let Some((type_name, type_name_source)) =
                    filtered_signature_leaves.next()
                  {
                    if filtered_signature_leaves.len() == 0 {
                      errors
                        .log(CompileError::new(InvalidTypeName, source_trace));
                    } else {
                      match first_child.as_str() {
                        "struct" => untyped_types.push(UntypedType::Struct(
                          UntypedStruct::from_field_trees(
                            (type_name, type_name_source),
                            filtered_signature_leaves
                              .into_iter()
                              .map(|(name, source)| {
                                (name, GenericArgument::Type(vec![]), source)
                              })
                              .collect(),
                            children_iter.cloned().collect(),
                            source_trace,
                            &mut errors,
                          ),
                        )),
                        "enum" => match UntypedEnum::from_field_trees(
                          (type_name, type_name_source),
                          filtered_signature_leaves
                            .into_iter()
                            .map(|(name, source)| {
                              (name, GenericArgument::Type(vec![]), source)
                            })
                            .collect(),
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
                    errors
                      .log(CompileError::new(InvalidTypeName, source_trace));
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
            errors.log(CompileError::new(InvalidTypeDefinition, source_trace));
          }
        }
        _ => non_typedef_trees.push((annotation, tree_body)),
      }
    }
    let mut program = Program::default();
    program.names = names.into();
    match UntypedType::sort_by_references(&untyped_types) {
      Ok(sorted_untyped_types) => {
        for name in macros.iter().flat_map(|m| m.reserved_names.iter().cloned())
        {
          program.names.borrow_mut().user_names.insert(name);
        }
        for untyped_type in sorted_untyped_types {
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
      }
      Err(e) => {
        let source_trace = if let Some(first_name) = e.get(0)
          && let Some(primary_type) =
            untyped_types.iter().find(|t| t.name() == first_name)
        {
          let mut source_trace = primary_type.source_trace().clone();
          for i in 1..e.len() {
            if let Some(secondary_type) =
              untyped_types.iter().find(|t| t.name() == &e[i])
            {
              source_trace = source_trace
                .insert_as_secondary(secondary_type.source_trace().clone());
            }
          }
          source_trace
        } else {
          SourceTrace::empty()
        };
        errors.log(CompileError::new(
          TypeDependencyCycle(
            e.into_iter().map(|name| name.to_string()).collect(),
          ),
          source_trace,
        ));
      }
    }

    for (annotation, tree) in non_typedef_trees.into_iter() {
      use crate::parse::Encloser::*;
      use fsexp::syntax::EncloserOrOperator::*;
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
            "defn" => {
              if let Some(f) = AbstractFunctionSignature::from_defn_ast(
                children_iter,
                first_child_source_trace,
                parens_source_trace,
                annotation,
                &program,
                &mut errors,
              ) {
                program.add_abstract_function(Rc::new(RefCell::new(f)));
              }
            }
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
          &var.var_type.clone().known(),
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
          .expression
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
          implementation.borrow_mut().expression.find_untyped()
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
          .expression
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
        if let Err(e) = implementation.expression.validate_assignments(self) {
          errors.log(e);
        }
      }
    }
  }
  pub fn monomorphize(&mut self, errors: &mut ErrorLog) {
    let mut monomorphized_ctx = Program::default();
    monomorphized_ctx.names = self.names.clone();
    for f in self.abstract_functions_iter() {
      if f.borrow().generic_args.is_empty()
        && let FunctionImplementationKind::Composite(implementation) =
          &f.borrow().implementation
      {
        let mut borrowed_implementation = implementation.borrow_mut();
        match borrowed_implementation
          .expression
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
    for s in self.typedefs.structs.iter() {
      if s.generic_args.is_empty() {
        monomorphized_ctx.add_monomorphized_struct(s.clone());
      }
    }
    for e in self.typedefs.enums.iter() {
      if e.generic_args.is_empty() {
        monomorphized_ctx.add_monomorphized_enum(e.clone());
      }
    }
    take(self, |old_ctx| {
      monomorphized_ctx.top_level_vars = old_ctx.top_level_vars;
      monomorphized_ctx
    });
  }
  pub fn validate_argument_ownership(&mut self, errors: &mut ErrorLog) {
    for f in self.abstract_functions_iter() {
      let mut borrowed_f = f.borrow_mut();
      if let FunctionImplementationKind::Composite(implementation) =
        &mut borrowed_f.implementation
      {
        let mut implementation = implementation.borrow_mut();
        implementation
          .expression
          .walk_mut_with_ctx::<Never>(
            &mut |exp, ctx| {
              match &exp.kind {
                ExpKind::Application(f, args) => {
                  if let Type::Function(f) = f.data.unwrap_known()
                    && let Some(abstract_f) = f.abstract_ancestor
                    && let abstract_f = abstract_f.borrow()
                    && let FunctionImplementationKind::Composite(_) =
                      abstract_f.implementation
                  {
                    for (i, (_, expected_ownership)) in
                      abstract_f.arg_types.iter().enumerate()
                    {
                      let arg = &args[i];
                      match expected_ownership {
                        Ownership::Owned => {
                          if arg.data.ownership != Ownership::Owned {
                            errors.log(CompileError::new(
                              ArgumentMustBeOwnedValue,
                              arg.source_trace.clone(),
                            ));
                          }
                        }
                        Ownership::Reference | Ownership::MutableReference => {
                          if let ExpKind::Name(name) = &arg.kind {
                            let top_level_var = self
                              .top_level_vars
                              .iter()
                              .find(|v| v.name == *name);
                            if let Some(TopLevelVar {
                              kind:
                                TopLevelVariableKind::Var {
                                  address_space, ..
                                },
                              ..
                            }) = top_level_var
                              && !address_space.may_be_passed_as_reference()
                            {
                              errors.log(CompileError::new(
                                PassedReferenceFromInvalidAddressSpace(
                                  *address_space,
                                ),
                                arg.source_trace.clone(),
                              ));
                            }
                            if *expected_ownership
                              == Ownership::MutableReference
                            {
                              match arg.data.ownership {
                                Ownership::Reference => {
                                  errors.log(CompileError::new(
                                    ReferenceMustBeMutable,
                                    arg.source_trace.clone(),
                                  ));
                                }
                                Ownership::Owned => {
                                  if ctx
                                    .variables
                                    .get(&**name)
                                    .map(|(v, _)| v.kind)
                                    .or_else(|| {
                                      top_level_var
                                        .map(TopLevelVar::variable_kind)
                                    })
                                    .unwrap()
                                    != VariableKind::Var
                                  {
                                    errors.log(CompileError::new(
                                      ImmutableOwnedPassedAsMutableReference,
                                      arg.source_trace.clone(),
                                    ));
                                  }
                                }
                                _ => {}
                              }
                            }
                          } else {
                            errors.log(CompileError::new(
                              ReferenceArgumentMustBeName,
                              arg.source_trace.clone(),
                            ));
                          }
                        }
                        Ownership::Pointer(_) => {
                          unreachable!(
                            "unexpected Ownership::Pointer encountered"
                          )
                        }
                      }
                    }
                  }
                }
                _ => {}
              }
              Ok(true)
            },
            &mut ImmutableProgramLocalContext::empty(self),
          )
          .unwrap();
      }
    }
  }
  pub fn monomorphize_reference_address_spaces(&mut self) {
    loop {
      let mut monomorphized_ctx = Program::default();
      monomorphized_ctx.names = self.names.clone();
      monomorphized_ctx.typedefs = self.typedefs.clone();
      let mut changed = false;
      for f in self.abstract_functions_iter() {
        let borrowed_f = f.borrow();
        if let FunctionImplementationKind::Composite(implementation) =
          &f.borrow().implementation
        {
          if borrowed_f.reference_arg_positions().is_empty() {
            let mut borrowed_implementation = implementation.borrow_mut();
            changed |= borrowed_implementation
              .expression
              .monomorphize_reference_address_spaces(
                &self,
                &mut monomorphized_ctx,
              );
            let mut new_f = (**f).borrow().clone();
            new_f.implementation =
              FunctionImplementationKind::Composite(implementation.clone());
            drop(borrowed_implementation);
            monomorphized_ctx
              .add_abstract_function(Rc::new(RefCell::new(new_f)));
          }
        } else {
          monomorphized_ctx.add_abstract_function(f.clone());
        }
      }
      take(self, |old_ctx| {
        monomorphized_ctx.top_level_vars = old_ctx.top_level_vars;
        monomorphized_ctx
      });
      if !changed {
        break;
      }
    }
  }
  pub fn catch_illegal_match_functions(&mut self, _errors: &mut ErrorLog) {
    // todo!()
  }
  pub fn propagate_abstract_function_signatures(&mut self) {
    loop {
      let mut changed = false;
      let copy_program = self.clone();
      for top_level_f in self.abstract_functions_iter() {
        let mut borrowed_f = top_level_f.borrow_mut();
        match &borrowed_f.implementation {
          FunctionImplementationKind::Composite(implementation) => {
            let mut borrowed_implementation = implementation.borrow_mut();
            borrowed_implementation
              .expression
              .walk_mut_with_ctx(
                &mut |exp, ctx| {
                  exp.data.as_known_mut(|t| {
                    if let Type::Function(f) = t
                      && f.abstract_ancestor.is_none()
                    {
                      match &exp.kind {
                        ExpKind::Name(name) => {
                          if let Some((v, _)) = ctx.variables.get(name)
                            && let Type::Function(bound_f) =
                              v.var_type.unwrap_known()
                            && let Some(abstract_ancestor) =
                              bound_f.abstract_ancestor
                          {
                            f.abstract_ancestor = Some(abstract_ancestor);
                            changed = true;
                          }
                        }
                        ExpKind::Application(applied_f, _) => {
                          if let Type::Function(applied_f) =
                            applied_f.data.unwrap_known()
                            && let Some(abstract_applied_f) =
                              applied_f.abstract_ancestor
                            && let Type::Function(_) =
                              applied_f.return_type.unwrap_known()
                            && let Some(signatures) = self
                              .abstract_functions
                              .get(&abstract_applied_f.borrow().name)
                            && let Some(signature) = signatures.get(0)
                            && let AbstractType::Type(Type::Function(
                              returned_f,
                            )) = &signature.borrow().return_type
                            && let Some(returned_abstract_ancestor) =
                              &returned_f.abstract_ancestor
                          {
                            f.abstract_ancestor =
                              Some(returned_abstract_ancestor.clone());
                            changed = true;
                          }
                        }
                        _ => {}
                      }
                    }
                    Ok::<bool, Never>(true)
                  })
                },
                &mut ImmutableProgramLocalContext::empty(&copy_program),
              )
              .unwrap();
            borrowed_implementation
              .expression
              .walk_mut(&mut |exp| {
                exp.data.as_known_mut(|t| {
                  if let Type::Function(f) = t
                    && f.abstract_ancestor.is_none()
                    && let Some(inner_exp) = match &exp.kind {
                      ExpKind::Let(_, body) => Some(body.as_ref()),
                      ExpKind::Block(exps) => exps.last(),
                      _ => None,
                    }
                    && let Type::Function(inner_f) =
                      inner_exp.data.unwrap_known()
                    && let Some(inner_abstract_ancestor) =
                      inner_f.abstract_ancestor
                  {
                    f.abstract_ancestor = Some(inner_abstract_ancestor.clone());
                    changed = true;
                  }
                });
                Ok::<bool, Never>(true)
              })
              .unwrap();
            let inner_abstract_ancestor = if let ExpKind::Function(_, body) =
              &borrowed_implementation.expression.kind
              && let Type::Function(inner_f) = body.data.unwrap_known()
              && let Some(ancestor) = inner_f.abstract_ancestor
            {
              Some(ancestor)
            } else {
              None
            };
            if let TypeState::Known(Type::Function(signature)) =
              &mut borrowed_implementation.expression.data.kind
              && let TypeState::Known(Type::Function(return_signature)) =
                &mut signature.return_type.kind
            {
              if let Some(inner_abstract_ancestor) =
                inner_abstract_ancestor.clone()
                && return_signature.abstract_ancestor.is_none()
              {
                return_signature.abstract_ancestor =
                  Some(inner_abstract_ancestor);
                changed = true;
              }
            }
            drop(borrowed_implementation);
            if let AbstractType::Type(Type::Function(f)) =
              &mut borrowed_f.return_type
              && f.abstract_ancestor.is_none()
            {
              if let Some(inner_abstract_ancestor) = inner_abstract_ancestor {
                f.abstract_ancestor = Some(inner_abstract_ancestor);
                changed = true;
              }
            }
          }
          _ => {}
        }
      }
      if !changed {
        break;
      }
    }
  }
  pub fn inline_local_bound_function_applications(&mut self) {
    let mut representative_structs = vec![];
    for f in self.abstract_functions_iter() {
      let borrowed_f = f.borrow();
      match &borrowed_f.implementation {
        FunctionImplementationKind::Composite(implementation) => implementation
          .borrow_mut()
          .expression
          .walk_mut_with_ctx(
            &mut |exp, ctx| match &mut exp.kind {
              ExpKind::Application(f, args) => {
                let ExpKind::Name(original_name) = &mut f.kind else {
                  panic!("non-name fn being applied")
                };
                if let Type::Function(_) = f.data.unwrap_known() {
                  match ctx.get_name_definition_source(&original_name) {
                    Some(source) => match source {
                      NameDefinitionSource::LocalBinding(_) => {
                        let Type::Function(bound_signature) = ctx
                          .variables
                          .get(original_name)
                          .unwrap()
                          .0
                          .var_type
                          .unwrap_known()
                        else {
                          panic!()
                        };
                        if let Some(abstract_fn) =
                          bound_signature.abstract_ancestor
                        {
                          let abstract_fn = abstract_fn.borrow();
                          let new_name = abstract_fn.name.clone();
                          if let Some(captured_scope) =
                            abstract_fn.captured_scope.as_ref()
                          {
                            args.push(Exp {
                              data: Type::Struct(
                                AbstractStruct::concretize(
                                  Rc::new(captured_scope.clone()),
                                  &self.typedefs,
                                  &vec![],
                                  f.source_trace.clone(),
                                )
                                .unwrap(),
                              )
                              .known()
                              .into(),
                              kind: ExpKind::Name(original_name.clone()),
                              source_trace: f.source_trace.clone(),
                            });
                            representative_structs.push(captured_scope.clone());
                          }
                          *original_name = new_name;
                        }
                      }
                      _ => {}
                    },
                    None => {}
                  }
                }
                Ok(true)
              }
              _ => Ok::<bool, Never>(true),
            },
            &mut ImmutableProgramLocalContext::empty(self),
          )
          .unwrap(),
        _ => {}
      }
    }
    for s in representative_structs {
      self.add_monomorphized_struct(s);
    }
  }
  pub fn extract_inner_functions(&mut self, errors: &mut ErrorLog) {
    let mut new_signatures: Vec<AbstractFunctionSignature> = vec![];
    for f in self.abstract_functions_iter() {
      let borrowed_f = f.borrow();
      if !borrowed_f.generic_args.is_empty() {
        continue;
      }
      match &borrowed_f.implementation {
        FunctionImplementationKind::Composite(implementation) => {
          let mut root_encountered = false;
          implementation
            .borrow_mut()
            .expression
            .walk_mut_with_ctx(
              &mut |exp, ctx| {
                if !root_encountered {
                  root_encountered = true;
                  return Ok(true);
                }
                let effects = exp.effects(self);
                if let ExpKind::Function(arg_names, body) = &mut exp.kind {
                  let name = self.names.borrow_mut().gensym("inner_fn");
                  let Type::Function(f_signature) = exp.data.unwrap_known()
                  else {
                    panic!()
                  };
                  let captured_vars = effects
                    .0
                    .iter()
                    .map(|e| match e {
                      Effect::ReadsVar(var_name) => {
                        Ok(match ctx.variables.get(var_name) {
                          Some((var, _)) => {
                            Some((var_name, var.var_type.unwrap_known()))
                          }
                          None => None,
                        })
                      }
                      _ => {
                        err(IllegalEffectsInClosure, body.source_trace.clone())
                      }
                    })
                    .collect::<CompileResult<Vec<_>>>();
                  let captured_vars: Vec<(&Rc<str>, Type)> = captured_vars
                    .unwrap_or_else(|e| {
                      errors.log(e);
                      vec![]
                    })
                    .into_iter()
                    .filter_map(|x| x)
                    .collect();
                  let captured_scope = if captured_vars.is_empty() {
                    None
                  } else {
                    Some(AbstractStruct {
                      name: (
                        self
                          .names
                          .borrow_mut()
                          .gensym(&format!("{name}_scope"))
                          .into(),
                        exp.source_trace.clone(),
                      ),
                      filled_generics: HashMap::new(),
                      fields: captured_vars
                        .iter()
                        .map(|(name, t)| AbstractStructField {
                          attributes: IOAttributes::empty(
                            exp.source_trace.clone(),
                          ),
                          name: (**name).clone(),
                          field_type: AbstractType::Type(t.clone()),
                          source_trace: exp.source_trace.clone(),
                        })
                        .collect(),
                      generic_args: vec![],
                      abstract_ancestor: None,
                      source_trace: exp.source_trace.clone(),
                      opaque: false,
                    })
                  };
                  let mut arg_types: Vec<(AbstractType, Ownership)> =
                    f_signature
                      .args
                      .iter()
                      .map(|(arg, _)| {
                        (
                          AbstractType::Type(arg.var_type.unwrap_known()),
                          Ownership::Owned,
                        )
                      })
                      .collect();
                  let captured_scope = captured_scope.map(|captured_scope| {
                    (
                      captured_scope.clone(),
                      AbstractType::AbstractStruct(Rc::new(captured_scope))
                        .concretize(
                          &vec![],
                          &self.typedefs,
                          exp.source_trace.clone(),
                        )
                        .unwrap(),
                      self.names.borrow_mut().gensym("scope"),
                    )
                  });
                  if let Some((
                    captured_scope,
                    concrete_captured_scope_type,
                    scope_name,
                  )) = &captured_scope
                  {
                    arg_names
                      .push((scope_name.clone(), exp.source_trace.clone()));
                    exp.data.as_known_mut(|t| {
                      let Type::Function(f) = t else {
                        panic!();
                      };
                      f.args.push((
                        Variable {
                          kind: VariableKind::Let,
                          var_type: concrete_captured_scope_type
                            .clone()
                            .known()
                            .into(),
                        },
                        vec![],
                      ));
                    });
                    arg_types.push((
                      AbstractType::AbstractStruct(Rc::new(
                        captured_scope.clone(),
                      )),
                      Ownership::Owned,
                    ));
                  }
                  let signature = AbstractFunctionSignature {
                    name: name.clone(),
                    generic_args: vec![],
                    associative: false,
                    arg_types,
                    return_type: AbstractType::Type(
                      f_signature.return_type.unwrap_known(),
                    ),
                    implementation: FunctionImplementationKind::Composite(
                      Rc::new(RefCell::new(TopLevelFunction {
                        name_source_trace: exp.source_trace.clone(),
                        arg_names: arg_names.clone(),
                        arg_annotations: arg_names
                          .iter()
                          .map(|(_, arg_source_trace)| {
                            FunctionArgumentAnnotation::empty(
                              arg_source_trace.clone(),
                            )
                          })
                          .collect(),
                        return_attributes: IOAttributes::empty(
                          exp.source_trace.clone(),
                        ),
                        entry_point: None,
                        expression: {
                          let mut new_exp = exp.clone();
                          if let Some((
                            _,
                            concrete_captured_scope_type,
                            scope_name,
                          )) = &captured_scope
                          {
                            let ExpKind::Function(_, body) = &mut new_exp.kind
                            else {
                              panic!()
                            };
                            take(&mut **body, |body| Exp {
                              data: body.data.clone(),
                              kind: ExpKind::Let(
                                captured_vars
                                  .iter()
                                  .map(|(arg_name, t)| {
                                    (
                                      (**arg_name).clone(),
                                      exp.source_trace.clone(),
                                      VariableKind::Let,
                                      Exp {
                                        data: t.clone().known().into(),
                                        kind: ExpKind::Access(
                                          Accessor::Field((**arg_name).clone()),
                                          Box::new(Exp {
                                            data: concrete_captured_scope_type
                                              .clone()
                                              .known()
                                              .into(),
                                            kind: ExpKind::Name(
                                              scope_name.clone(),
                                            ),
                                            source_trace: exp
                                              .source_trace
                                              .clone(),
                                          }),
                                        ),
                                        source_trace: exp.source_trace.clone(),
                                      },
                                    )
                                  })
                                  .collect(),
                                Box::new(body),
                              ),
                              source_trace: exp.source_trace.clone(),
                            })
                          }
                          new_exp
                        },
                      })),
                    ),
                    captured_scope: captured_scope
                      .as_ref()
                      .map(|(s, _, _)| s.clone()),
                  };
                  new_signatures.push(signature.clone());
                  *exp = Exp {
                    data: Type::Function(Box::new(FunctionSignature {
                      abstract_ancestor: Some(Rc::new(RefCell::new(signature))),
                      args: f_signature.args,
                      return_type: f_signature.return_type,
                    }))
                    .known()
                    .into(),
                    kind: if let Some((captured_scope, _, _)) = captured_scope {
                      ExpKind::Application(
                        Box::new(Exp {
                          data: Type::Function(Box::new(FunctionSignature {
                            abstract_ancestor: None,
                            args: captured_vars
                              .iter()
                              .map(|(_, t)| {
                                (
                                  Variable {
                                    kind: VariableKind::Let,
                                    var_type: t.clone().known().into(),
                                  },
                                  vec![],
                                )
                              })
                              .collect(),
                            return_type: exp.data.clone(),
                          }))
                          .known()
                          .into(),
                          kind: ExpKind::Name(captured_scope.name.0.clone()),
                          source_trace: exp.source_trace.clone(),
                        }),
                        captured_vars
                          .into_iter()
                          .map(|(name, t)| Exp {
                            data: t.known().into(),
                            kind: ExpKind::Name(name.clone()),
                            source_trace: exp.source_trace.clone(),
                          })
                          .collect(),
                      )
                    } else {
                      ExpKind::Name(name)
                    },
                    source_trace: exp.source_trace.clone(),
                  };

                  Ok(true)
                } else {
                  Ok::<bool, Never>(true)
                }
              },
              &mut ImmutableProgramLocalContext::empty(self),
            )
            .unwrap();
        }
        _ => {}
      }
    }
    for s in new_signatures {
      self.add_abstract_function(Rc::new(RefCell::new(s)));
    }
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
    inlined_ctx.typedefs = self.typedefs.clone();
    for f in self.abstract_functions_iter() {
      let borrowed_f = f.borrow();
      if !borrowed_f.has_uninlined_higher_order_arguments() {
        match &borrowed_f.implementation {
          FunctionImplementationKind::Composite(implementation) => {
            let mut borrowed_implementation = implementation.borrow_mut();
            match borrowed_implementation
              .expression
              .inline_higher_order_arguments(&mut inlined_ctx)
            {
              Ok(added_new_function) => {
                changed |= added_new_function;
                let mut new_f = borrowed_f.clone();
                drop(borrowed_implementation);
                new_f.implementation =
                  FunctionImplementationKind::Composite(implementation.clone());
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
      inlined_ctx.top_level_vars = old_ctx.top_level_vars;
      inlined_ctx
    });
    changed
  }
  pub fn remove_unitlike_values(&mut self) {
    let mut names = NameContext::empty();
    std::mem::swap(&mut names, &mut self.names.borrow_mut());
    take(&mut self.typedefs.structs, |structs| {
      structs
        .into_iter()
        .filter(|s| !s.is_unitlike(&mut names))
        .collect()
    });
    for f in self.abstract_functions_iter_mut() {
      let f = f.borrow_mut();
      if let FunctionImplementationKind::Composite(implementation) =
        &f.implementation
      {
        let mut implementation = implementation.borrow_mut();
        implementation
          .expression
          .walk_mut(&mut |exp| match &mut exp.kind {
            ExpKind::Application(applied_f, args) => {
              applied_f.data.with_dereferenced_mut(|t| match t {
                TypeState::Known(t) => match t {
                  Type::Function(applied_f_signature) => {
                    if let Some(applied_f_abstract_signature) =
                      &mut applied_f_signature.abstract_ancestor
                    {
                      let applied_f_abstract_signature =
                        (**applied_f_abstract_signature).clone();
                      applied_f_abstract_signature
                        .borrow_mut()
                        .remove_unitlike_arguments(&mut names);
                      applied_f_signature.abstract_ancestor =
                        Some(Rc::new(applied_f_abstract_signature));
                    }
                    let args_to_remove: Vec<usize> = (0..args.len())
                      .rev()
                      .filter(|i| {
                        args[*i].data.unwrap_known().is_unitlike(&mut names)
                      })
                      .collect();
                    for i in args_to_remove {
                      args.remove(i);
                      applied_f_signature.args.remove(i);
                    }
                  }
                  _ => {}
                },
                _ => {}
              });

              Ok(true)
            }
            ExpKind::Let(bindings, _) => {
              take(bindings, |bindings| {
                bindings
                  .into_iter()
                  .filter(|(_, _, _, t)| {
                    !t.data.unwrap_known().is_unitlike(&mut names)
                  })
                  .collect()
              });
              Ok(true)
            }
            _ => Ok::<bool, Never>(true),
          })
          .unwrap();
      }
    }
    std::mem::swap(&mut names, &mut self.names.borrow_mut());
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
      if let Some(compiled_struct) =
        s.compile_if_non_generic(&self.typedefs, &mut names)?
      {
        wgsl += &compiled_struct;
        wgsl += "\n\n";
      }
    }
    for e in self.typedefs.enums.iter().cloned() {
      if let Some(compiled_enum) =
        e.compile_if_non_generic(&self.typedefs, &mut names)?
      {
        wgsl += &compiled_enum;
        wgsl += "\n\n";
      }
    }
    for f in self.abstract_functions_iter() {
      let f = f.borrow().clone();
      if f.generic_args.is_empty() && !f.has_uninlined_higher_order_arguments()
      {
        match f.implementation {
          FunctionImplementationKind::EnumConstructor(
            original_variant_name,
          ) => {
            let variant_name = compile_word(f.name);
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
            let enum_name = compile_word(
              e.original_ancestor().monomorphized_name(
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
              ),
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
      if f.generic_args.is_empty() && !f.has_uninlined_higher_order_arguments()
      {
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
          .expression
          .walk_mut::<()>(&mut |exp| {
            take(&mut exp.kind, |exp_kind| {
              if let ExpKind::Application(f, args) = exp_kind {
                if let ExpKind::Name(_) = &f.kind
                  && let Type::Function(x) = f.data.kind.unwrap_known()
                  && let Some(abstract_ancestor) = &x.abstract_ancestor
                  && abstract_ancestor.borrow().associative
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
          f.borrow_mut().expression.deshadow(
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
        if let Type::Function(f) =
          &mut implementation.expression.data.unwrap_known()
          && let ExpKind::Function(arg_names, body) =
            &mut implementation.expression.kind
        {
          let mutable_args: Vec<_> = f
            .args
            .iter()
            .zip(arg_names.iter())
            .filter_map(|((var, _), arg_name)| {
              if var.kind == VariableKind::Var
                && var.var_type.ownership == Ownership::Owned
              {
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
                    .map(|((arg_name, _), arg_type)| {
                      (
                        arg_name.clone(),
                        SourceTrace::empty(),
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
  fn validate_names(&self, errors: &mut ErrorLog) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(implementation) =
        &signature.implementation
      {
        let implementation = implementation.borrow();
        if !is_valid_name(&signature.name) {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidName,
            implementation.name_source_trace.clone(),
          ))
        }
        for (generic_name, _, source_trace) in signature.generic_args.iter() {
          if !is_valid_name(generic_name) {
            errors.log(CompileError::new(
              CompileErrorKind::InvalidName,
              source_trace.clone(),
            ))
          }
        }
        implementation
          .expression
          .walk(&mut |exp| {
            let names: Vec<_> = match &exp.kind {
              ExpKind::Let(items, _) => items
                .iter()
                .map(|(name, source, _, _)| (name, source))
                .collect(),
              ExpKind::Match(_, arms) => arms
                .iter()
                .flat_map(|(pattern, _)| {
                  if let ExpKind::Application(_, args) = &pattern.kind {
                    args
                      .iter()
                      .filter_map(|arg| {
                        if let ExpKind::Name(name) = &arg.kind {
                          Some((name, &arg.source_trace))
                        } else {
                          None
                        }
                      })
                      .collect()
                  } else {
                    vec![]
                  }
                })
                .collect(),
              ExpKind::ForLoop {
                increment_variable_name,
                ..
              } => {
                vec![(&increment_variable_name.0, &increment_variable_name.1)]
              }
              _ => vec![],
            };
            for (name, source) in names {
              if !is_valid_name(name) {
                errors.log(CompileError::new(
                  CompileErrorKind::InvalidName,
                  source.clone(),
                ));
              }
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
    for e in self.typedefs.enums.iter() {
      if !is_valid_name(&e.name.0) {
        errors.log(CompileError::new(
          CompileErrorKind::InvalidName,
          e.name.1.clone(),
        ));
      }
      for (name, _, source) in e.generic_args.iter() {
        if !is_valid_name(name) {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidName,
            source.clone(),
          ));
        }
      }
      for variant in e.variants.iter() {
        if !is_valid_name(&variant.name) {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidName,
            variant.source.clone(),
          ));
        }
      }
    }
    for s in self.typedefs.structs.iter() {
      if !is_valid_name(&s.name.0) {
        errors.log(CompileError::new(
          CompileErrorKind::InvalidName,
          s.name.1.clone(),
        ));
      }
      for (name, _, source) in s.generic_args.iter() {
        if !is_valid_name(name) {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidName,
            source.clone(),
          ));
        }
      }
      for field in s.fields.iter() {
        if !is_valid_name(&field.name) {
          errors.log(CompileError::new(
            CompileErrorKind::InvalidName,
            field.source_trace.clone(),
          ));
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
          || signature.arg_types[0].0 != signature.return_type)
      {
        if let FunctionImplementationKind::Composite(implementation) =
          &signature.implementation
        {
          errors.log(CompileError {
            kind: CompileErrorKind::InvalidAssociativeSignature,
            source_trace: implementation
              .borrow()
              .expression
              .source_trace
              .clone(),
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
          let source = f.borrow().expression.source_trace.clone();
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
          for (arg_name, _) in f.arg_names.iter() {
            if self.abstract_functions.get(arg_name).is_some()
              || self
                .top_level_vars
                .iter()
                .find(|v| v.name == *arg_name)
                .is_some()
            {
              errors.log(CompileError::new(
                CantShadowTopLevelBinding(arg_name.to_string()),
                f.expression.source_trace.clone(),
              ))
            }
          }
        }
      }
    }
  }
  fn catch_duplicate_struct_fields(&self, errors: &mut ErrorLog) {
    for s in self.typedefs.structs.iter() {
      let mut names_so_far = HashSet::new();
      for field in s.fields.iter() {
        let name = &field.name;
        if names_so_far.contains(name) {
          errors.log(CompileError::new(
            CompileErrorKind::DuplicateStructFieldName,
            field.source_trace.clone(),
          ));
        } else {
          names_so_far.insert(name);
        }
      }
    }
  }
  fn catch_duplicate_enum_variants(&self, errors: &mut ErrorLog) {
    for e in self.typedefs.enums.iter() {
      let mut names_so_far = HashSet::new();
      for variant in e.variants.iter() {
        let name = &variant.name;
        if names_so_far.contains(name) {
          errors.log(CompileError::new(
            CompileErrorKind::DuplicateEnumVariantName,
            variant.source.clone(),
          ));
        } else {
          names_so_far.insert(name);
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
          .expression
          .walk(&mut |exp| {
            match &exp.kind {
              ExpKind::Let(items, _) => {
                for (_, source_trace, _, value) in items.iter() {
                  if Type::Unit.known() == value.data.kind {
                    errors.log(CompileError {
                      kind: CompileErrorKind::TypelessBinding,
                      source_trace: source_trace.clone(),
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
        f.borrow().expression.validate_control_flow(errors, 0);
      }
    }
  }
  pub fn deexpressionify(&mut self) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        let mut f = f.borrow_mut();
        f.expression.throw_away_inner_values_in_blocks(self);
        f.expression.deexpressionify(self);
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
            && let Type::Function(f) = f.borrow().expression.data.unwrap_known()
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
          .expression
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
  pub fn inline_def_array_sizes(&mut self) {
    let u32_constants: HashMap<Rc<str>, u32> = self
      .top_level_vars
      .iter()
      .filter_map(|v| {
        if (v.var_type == Type::U32 || v.var_type == Type::I32)
          && v.kind == TopLevelVariableKind::Const
          && let Some(TypedExp {
            kind: ExpKind::NumberLiteral(Number::Int(n)),
            ..
          }) = v.value
          && let Ok(n) = n.try_into()
        {
          Some((v.name.clone(), n))
        } else {
          None
        }
      })
      .collect();
    for f in self.abstract_functions_iter_mut() {
      if let FunctionImplementationKind::Composite(f) =
        &f.borrow_mut().implementation
      {
        f.borrow_mut()
          .expression
          .walk_mut(&mut |exp| {
            if let TypeState::Known(t) = &mut exp.data.kind {
              t.walk_mut(&|t| {
                if let Type::Array(Some(size), _) = t
                  && let ConcreteArraySize::Constant(constant_name) = size
                  && let Some(n) = u32_constants.get(constant_name)
                {
                  *size = ConcreteArraySize::Literal(*n);
                }
              });
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
  }
  pub fn inline_static_array_length_calls(&mut self) {
    for f in self.abstract_functions_iter_mut() {
      if let FunctionImplementationKind::Composite(f) =
        &f.borrow_mut().implementation
      {
        f.borrow_mut()
          .expression
          .walk_mut(&mut |exp| {
            if let ExpKind::Application(f, args) = &exp.kind
              && let ExpKind::Name(f_name) = &f.kind
              && &**f_name == "array-length"
              && let Type::Array(Some(size), _) = args[0].data.unwrap_known()
              && size != ConcreteArraySize::Unsized
            {
              let size = match size {
                ConcreteArraySize::Literal(x) => x,
                ConcreteArraySize::UnificationVariable(const_generic_value) => {
                  const_generic_value.value.borrow().unwrap()
                }
                _ => panic!("can't handle this kind of ConcreteArraySize here"),
              };
              *exp = TypedExp {
                data: Type::U32.known().into(),
                kind: ExpKind::NumberLiteral(Number::Int(size as i64)),
                source_trace: exp.source_trace.clone(),
              }
            }
            Ok::<bool, Never>(true)
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
          .expression
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
          let ExpKind::Function(_, body) = &f.expression.kind else {
            unreachable!()
          };
          let effects = body.effects(self);
          for e in effects.0.iter() {
            match e {
              Effect::Discard => {
                errors.log(CompileError {
                  kind: DiscardOutsideFragment,
                  source_trace: f.expression.source_trace.clone(),
                });
              }
              Effect::FragmentExclusiveFunction(name) => {
                errors.log(CompileError {
                  kind: FragmentExclusiveFunctionOutsideFragment(
                    name.to_string(),
                  ),
                  source_trace: f.expression.source_trace.clone(),
                });
              }
              _ => {}
            }
          }
        }
      }
    }
  }
  pub fn validate_entry_points(&mut self, errors: &mut ErrorLog) {
    let mut inferred_struct_field_locations: Vec<(
      Rc<AbstractStruct>,
      Rc<str>,
      usize,
    )> = vec![];
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(f) =
        &signature.implementation
      {
        let mut f = f.borrow_mut();
        let f_source = f.expression.source_trace.clone();
        if let Some(entry) = f.entry_point {
          let Type::Function(signature) = f.expression.data.unwrap_known()
          else {
            unreachable!()
          };
          match entry {
            EntryPoint::Vertex => {
              if signature.return_type.unwrap_known().is_vec4f()
                && f.return_attributes.is_empty()
              {
                let return_source =
                  f.return_attributes.attributed_source.clone();
                f.return_attributes.try_add_attribute(
                  IOAttribute {
                    kind: IOAttributeKind::Builtin(
                      BuiltinIOAttribute::Position,
                    ),
                    source_trace: return_source,
                  },
                  errors,
                );
              }
            }
            EntryPoint::Compute(_) => {
              let mut errored = false;
              if Type::Unit != signature.return_type.unwrap_known() {
                errors.log(CompileError::new(
                  ComputeEntryReturnType,
                  f.expression.source_trace.clone(),
                ));
                errored = true;
              }
              if !f.return_attributes.is_empty() {
                errors.log(CompileError::new(
                  ComputeEntryReturnType,
                  f.expression.source_trace.clone(),
                ));
                errored = true;
              }
              if errored {
                continue;
              }
            }
            EntryPoint::Fragment => {}
          }

          let check_for_duplicate_builtins =
            |attributables: &Vec<(
              Type,
              Result<
                &mut IOAttributes,
                (Rc<AbstractStruct>, Rc<str>, IOAttributes),
              >,
            )>|
             -> Vec<(String, SourceTrace)> {
              let mut duplicates = HashSet::new();
              let mut builtins = HashSet::new();
              for (_, attributable) in attributables.iter() {
                let attribute = match attributable {
                  Ok(a) => &*a,
                  Err((_, _, a)) => a,
                };
                if let Some((builtin, source_trace)) = attribute.builtin() {
                  if builtins.contains(builtin) {
                    duplicates.insert((
                      builtin.name().to_string(),
                      source_trace.clone(),
                    ));
                  } else {
                    builtins.insert(builtin.clone());
                  }
                }
              }
              duplicates.into_iter().collect()
            };

          let mut handle_inout_attributables =
            |attributables: Vec<(
              Type,
              Result<
                &mut IOAttributes,
                (Rc<AbstractStruct>, Rc<str>, IOAttributes),
              >,
            )>,
             input_or_output: InputOrOutput,
             errors: &mut ErrorLog|
             -> (
              HashMap<usize, (SourceTrace, Result<Type, Rc<AbstractStruct>>)>,
              HashMap<BuiltinIOAttribute, Result<Type, AbstractType>>,
            ) {
              for (name, source) in check_for_duplicate_builtins(&attributables)
              {
                errors.log(CompileError::new(
                  DuplicateBuiltinAttribute(input_or_output, name),
                  source,
                ))
              }
              let mut used_locations: HashMap<
                usize,
                (SourceTrace, Result<Type, Rc<AbstractStruct>>),
              > = HashMap::new();
              let mut used_builtins: HashMap<
                BuiltinIOAttribute,
                Result<Type, AbstractType>,
              > = HashMap::new();
              for (t, attributable) in attributables.iter() {
                let attributes = match attributable {
                  Ok(a) => &*a,
                  Err((_, _, a)) => a,
                };
                if let Some((builtin, source)) = attributes.builtin() {
                  if match input_or_output {
                    InputOrOutput::Input => {
                      !builtin.is_valid_input_for_stage(&entry)
                    }
                    InputOrOutput::Output => {
                      !builtin.is_valid_output_for_stage(&entry)
                    }
                  } {
                    errors.log(CompileError::new(
                      InvalidBuiltinForEntryPoint(
                        builtin.name().to_string(),
                        input_or_output,
                        entry.name().to_string(),
                      ),
                      source.clone(),
                    ));
                  } else {
                    used_builtins.insert(
                      *builtin,
                      match attributable {
                        Ok(_) => Ok(t.clone()),
                        Err((s, field_name, _)) => Err(
                          s.fields
                            .iter()
                            .find_map(|f| {
                              (f.name == *field_name)
                                .then(|| f.field_type.clone())
                            })
                            .unwrap()
                            .clone(),
                        ),
                      },
                    );
                  }
                  let t = match attributable {
                    Ok(_) => t,
                    Err((s, field_name, _)) => &s
                      .fields
                      .iter()
                      .find(|f| f.name == *field_name)
                      .unwrap()
                      .field_type
                      .concretize(&vec![], &self.typedefs, SourceTrace::empty())
                      .unwrap(),
                  };
                  if !builtin.is_type_compatible(t) {
                    errors.log(CompileError::new(
                      InvalidBuiltinType(builtin.name().to_string()),
                      attributes.attributed_source.clone(),
                    ))
                  }
                } else {
                  if let Some((location, source)) = attributes.location() {
                    used_locations.insert(location, (source, Ok(t.clone())));
                  }
                }
              }
              for (t, attributable) in attributables {
                let attributes = match &attributable {
                  Ok(a) => &*a,
                  Err((_, _, a)) => a,
                };
                if attributes.builtin().is_none()
                  && attributes.location().is_none()
                {
                  let untaken_location =
                    (0..).find(|i| !used_locations.contains_key(i)).unwrap();
                  match attributable {
                    Ok(a) => {
                      if t.is_location_attributable() {
                        let source_trace = a.attributed_source.clone();
                        a.try_add_attribute(
                          IOAttribute {
                            kind: IOAttributeKind::Location(untaken_location),
                            source_trace: source_trace.clone(),
                          },
                          errors,
                        );
                        used_locations
                          .insert(untaken_location, (source_trace, Ok(t)));
                      } else {
                        errors.log(CompileError::new(
                          InvalidTypeForEntryPoint(t.into(), input_or_output),
                          f_source.clone(),
                        ));
                      }
                    }
                    Err((t, field_name, a)) => {
                      inferred_struct_field_locations.push((
                        t.clone(),
                        field_name.clone(),
                        untaken_location,
                      ));
                      used_locations.insert(
                        untaken_location,
                        (a.attributed_source, Err(t.clone())),
                      );
                    }
                  }
                }
              }
              (used_locations, used_builtins)
            };

          let input_attributables: Vec<(
            Type,
            Result<
              &mut IOAttributes,
              (Rc<AbstractStruct>, Rc<str>, IOAttributes),
            >,
          )> = f
            .arg_annotations
            .iter_mut()
            .enumerate()
            .flat_map(|(i, annotation)| {
              let arg = &signature.args[i];
              let arg_type = arg.0.var_type.unwrap_known();
              if arg_type.is_attributable() {
                vec![(arg_type, Ok(&mut annotation.attributes))]
              } else {
                if let Some(source) =
                  annotation.attributes.source_trace_if_not_empty()
                {
                  errors
                    .log(CompileError::new(CantAssignAttributesToType, source));
                  vec![]
                } else {
                  self
                    .typedefs
                    .get_attributable_components(
                      arg_type.clone(),
                      InputOrOutput::Input,
                      f_source.clone(),
                      errors,
                    )
                    .into_iter()
                    .map(|(t, field_name, attributes)| {
                      (arg_type.clone(), Err((t, field_name, attributes)))
                    })
                    .collect()
                }
              }
            })
            .collect();
          handle_inout_attributables(
            input_attributables,
            InputOrOutput::Input,
            errors,
          );

          let return_type = signature.return_type.unwrap_known();
          let output_attributables: Vec<(
            Type,
            Result<
              &mut IOAttributes,
              (Rc<AbstractStruct>, Rc<str>, IOAttributes),
            >,
          )> = if return_type.is_attributable() {
            vec![(return_type, Ok(&mut f.return_attributes))]
          } else {
            if let Some(source) =
              f.return_attributes.source_trace_if_not_empty()
            {
              errors.log(CompileError::new(CantAssignAttributesToType, source));
              vec![]
            } else {
              self
                .typedefs
                .get_attributable_components(
                  return_type.clone(),
                  InputOrOutput::Output,
                  f_source.clone(),
                  errors,
                )
                .into_iter()
                .map(|(t, field_name, attributes)| {
                  (return_type.clone(), Err((t, field_name, attributes)))
                })
                .collect()
            }
          };
          let (used_output_locations, used_output_builtins) =
            handle_inout_attributables(
              output_attributables,
              InputOrOutput::Output,
              errors,
            );

          match entry {
            EntryPoint::Vertex => {
              if let Some(t) =
                used_output_builtins.get(&BuiltinIOAttribute::Position)
              {
                if match t {
                  Ok(t) => !t.is_vec4f(),
                  Err(t) => !t.is_vec4f(),
                } {
                  errors.log(CompileError::new(
                    VertexPositionOutputInvalidType,
                    f_source,
                  ));
                }
              } else {
                errors.log(CompileError::new(
                  VertexMustHavePositionOutput,
                  f_source,
                ));
              }
            }
            EntryPoint::Fragment => {
              if let Some((_, t)) = used_output_locations.get(&0) {
                if let Ok(Type::Struct(s)) = t
                  && &*s.name == "vec4"
                  && {
                    let field_type = s.fields[0].field_type.unwrap_known();
                    field_type == Type::F32
                      || field_type == Type::U32
                      || field_type == Type::I32
                  }
                {
                } else if let Err(s) = t
                  && &*s.name.0 == "vec4"
                  && {
                    match s.fields[0].field_type {
                      AbstractType::Type(Type::F32 | Type::U32 | Type::I32) => {
                        true
                      }
                      _ => false,
                    }
                  }
                {
                } else {
                  errors.log(CompileError::new(
                    Fragment0OutputInvalidType,
                    f_source,
                  ));
                }
              } else {
                errors.log(CompileError::new(
                  FragmentMustHaveLocation0Output,
                  f_source,
                ));
              }
            }
            EntryPoint::Compute(_) => {}
          }
        } else {
          for attributes in f
            .arg_annotations
            .iter()
            .map(|a| &a.attributes)
            .chain(std::iter::once(&f.return_attributes))
          {
            if let Some(source_trace) = attributes.source_trace_if_not_empty() {
              errors
                .log(CompileError::new(IOAttributesOnNonEntry, source_trace));
            }
          }
        }
      }
    }
    while let Some((s, field_name, location)) =
      inferred_struct_field_locations.pop()
    {
      let struct_source_trace = s.source_trace.clone();
      let mut field_locations = vec![(field_name, location)];
      let mut remaining_inferred_struct_field_locations = vec![];
      for (other_s, field_name, location) in inferred_struct_field_locations {
        if s == other_s {
          let location = (field_name, location);
          if !field_locations.contains(&location) {
            field_locations.push(location);
          }
        } else {
          remaining_inferred_struct_field_locations
            .push((other_s, field_name, location));
        }
      }
      let s = self
        .typedefs
        .structs
        .iter_mut()
        .find(|existing_s| *s == **existing_s)
        .unwrap();
      for (field_name, location) in field_locations {
        s.fields
          .iter_mut()
          .find(|f| f.name == field_name)
          .unwrap()
          .attributes
          .try_add_attribute(
            IOAttribute {
              kind: IOAttributeKind::Location(location),
              source_trace: struct_source_trace.clone(),
            },
            errors,
          );
      }
      inferred_struct_field_locations =
        remaining_inferred_struct_field_locations;
    }
  }
  pub fn catch_expressions_after_control_flow(
    &mut self,
    errors: &mut ErrorLog,
  ) {
    for signature in self.abstract_functions_iter() {
      let signature = signature.borrow();
      if let FunctionImplementationKind::Composite(implementation) =
        &signature.implementation
      {
        implementation
          .borrow()
          .expression
          .walk(&mut |exp| {
            match &exp.kind {
              ExpKind::Block(children) => {
                let mut encountered_control_flow_operator = None;
                for child in children.iter() {
                  match child.kind {
                    ExpKind::Break
                    | ExpKind::Continue
                    | ExpKind::Discard
                    | ExpKind::Return(_) => {
                      encountered_control_flow_operator =
                        Some(match child.kind {
                          ExpKind::Break => "break".to_string(),
                          ExpKind::Continue => "continue".to_string(),
                          ExpKind::Discard => "discard".to_string(),
                          ExpKind::Return(_) => "return".to_string(),
                          _ => unreachable!(),
                        });
                    }
                    _ => {
                      if let Some(name) = &encountered_control_flow_operator {
                        errors.log(CompileError::new(
                          ExpressionAfterControlFlow(name.clone()),
                          child.source_trace.clone(),
                        ))
                      }
                    }
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
  pub fn validate_raw_program(&mut self) -> ErrorLog {
    let mut errors = ErrorLog::new();
    self.validate_names(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
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
    self.catch_duplicate_struct_fields(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_duplicate_enum_variants(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.inline_def_array_sizes();
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
    self.desugar_swizzle_assignments();
    self.deexpressionify();
    self.deshadow(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.monomorphize(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_illegal_match_functions(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.extract_inner_functions(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.separate_overloaded_fns();
    self.propagate_abstract_function_signatures();
    self.inline_local_bound_function_applications();
    self.inline_all_higher_order_arguments(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.remove_unitlike_values();
    self.validate_top_level_fn_effects(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_entry_points(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.catch_expressions_after_control_flow(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.validate_argument_ownership(&mut errors);
    if !errors.is_empty() {
      return errors;
    }
    self.inline_static_array_length_calls();
    self.monomorphize_reference_address_spaces();
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
        .expression
        .walk(&mut |exp: &TypedExp| {
          type_annotations
            .push((exp.source_trace.clone(), exp.data.kind.clone()));
          if let ExpKind::Let(bindings, _) = &exp.kind {
            for (_, source_trace, _, bound_exp) in bindings.iter() {
              type_annotations
                .push((source_trace.clone(), bound_exp.data.kind.clone()))
            }
          }
          Ok::<_, Never>(true)
        })
        .unwrap();
    }
    type_annotations
  }
  pub fn gather_name_definition_sites(
    &self,
  ) -> HashMap<Vec<usize>, NameDefinitionSource> {
    let mut top_level_name_definitions = HashMap::new();
    for e in self.typedefs.enums.iter() {
      let e = e.original_ancestor();
      top_level_name_definitions.insert(
        e.name.0.clone(),
        NameDefinitionSource::Enum(e.name.1.primary_path()),
      );
    }
    for t in self.typedefs.enums.iter() {
      let t = t.original_ancestor();
      top_level_name_definitions.insert(
        t.name.0.clone(),
        NameDefinitionSource::Enum(t.name.1.primary_path()),
      );
    }
    let mut defn_locations: HashMap<Rc<str>, Vec<Vec<usize>>> = HashMap::new();
    for f in self.abstract_functions_iter() {
      let f = f.borrow();
      if let FunctionImplementationKind::Composite(implementation) =
        &f.implementation
      {
        if !defn_locations.contains_key(&f.name) {
          defn_locations.insert(f.name.clone(), vec![]);
        }
        defn_locations
          .get_mut(&f.name)
          .unwrap()
          .push(implementation.borrow().name_source_trace.primary_path());
      }
    }
    for (name, sources) in defn_locations {
      top_level_name_definitions
        .insert(name, NameDefinitionSource::Defn(sources));
    }
    let mut sites = HashMap::new();
    for f in self.abstract_functions_iter() {
      let f = f.borrow();
      if let FunctionImplementationKind::Composite(f) = &f.implementation {
        f.borrow()
          .expression
          .walk_with_ctx(
            &mut |exp, ctx| {
              match &exp.kind {
                ExpKind::Name(name) => {
                  if let Some(definition_source) = top_level_name_definitions
                    .get(name)
                    .cloned()
                    .or_else(|| ctx.get_name_definition_source(name))
                  {
                    sites.insert(
                      exp.source_trace.primary_path(),
                      definition_source,
                    );
                  }
                }
                _ => {}
              }
              Ok::<bool, Never>(true)
            },
            &mut ImmutableProgramLocalContext::empty(self),
          )
          .unwrap();
      }
    }
    sites
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
  pub fn main_fn(&self) -> Option<Rc<RefCell<AbstractFunctionSignature>>> {
    if let Some(main_fns) = self.abstract_functions.get("main".into()) {
      let main_fn = main_fns.iter().find(|f| {
        let f = f.borrow();
        f.arg_types.is_empty() && f.return_type == AbstractType::Unit
      });
      main_fn.cloned()
    } else {
      None
    }
  }
  pub fn find_definition(
    &self,
    name: &str,
    path: &Vec<usize>,
  ) -> Option<NameDefinitionSource> {
    for e in self.typedefs.enums.iter() {
      let e = e.original_ancestor();
      if &*e.name.0 == name {
        return Some(NameDefinitionSource::Enum(e.name.1.primary_path()));
      }
    }
    for t in self.typedefs.enums.iter() {
      let t = t.original_ancestor();
      if &*t.name.0 == name {
        return Some(NameDefinitionSource::Enum(t.name.1.primary_path()));
      }
    }
    let mut defn_locations: HashSet<Vec<usize>> = HashSet::new();
    for f in self.abstract_functions_iter() {
      let f = f.borrow();
      if &*f.name == name {
        if let FunctionImplementationKind::Composite(f) = &f.implementation {
          defn_locations.insert(f.borrow().name_source_trace.primary_path());
        }
      }
    }
    if !defn_locations.is_empty() {
      return Some(NameDefinitionSource::Defn(
        defn_locations.into_iter().collect(),
      ));
    }
    for f in self.abstract_functions_iter() {
      let f = f.borrow();
      if let FunctionImplementationKind::Composite(f) = &f.implementation {
        let mut definition_source: Option<NameDefinitionSource> = None;
        fn is_prefix(a: &Vec<usize>, b: &Vec<usize>) -> bool {
          a.len() < b.len()
            && a.iter().zip(b.iter()).find(|(a, b)| a != b).is_none()
        }
        f.borrow()
          .expression
          .walk(&mut |exp| {
            let exp_path = exp.source_trace.primary_path();
            if is_prefix(&exp_path, path) {
              return Ok(false);
            }
            match &exp.kind {
              ExpKind::ForLoop {
                increment_variable_name,
                ..
              } => {
                if &*increment_variable_name.0 == name {
                  definition_source = Some(NameDefinitionSource::LocalBinding(
                    increment_variable_name.1.primary_path(),
                  ))
                }
              }
              ExpKind::Let(bindings, _) => {
                let bindings_to_consider = if path[exp_path.len()] == 1 {
                  // path being searched for is inside bindings
                  if let Some(internal_binding_index) =
                    path.get(exp_path.len() + 1)
                    && internal_binding_index % 2 == 1
                  {
                    let internal_binding_index = internal_binding_index / 2;
                    internal_binding_index.checked_sub(1).unwrap_or(0)
                  } else {
                    0
                  }
                } else {
                  // path being searched for is inside body
                  bindings.len()
                };
                for (binding_name, binding_source_trace, _, _) in
                  bindings.iter().take(bindings_to_consider).rev()
                {
                  if &**binding_name == name {
                    definition_source =
                      Some(NameDefinitionSource::LocalBinding(
                        binding_source_trace.primary_path(),
                      ));
                    break;
                  }
                }
              }
              ExpKind::Match(_, arms) => {
                for (pattern, arm_body) in arms.iter() {
                  if is_prefix(&arm_body.source_trace.primary_path(), path) {
                    match &pattern.kind {
                      ExpKind::Name(pattern_name) => {
                        if &**pattern_name == name {
                          definition_source =
                            Some(NameDefinitionSource::LocalBinding(
                              pattern.source_trace.primary_path(),
                            ));
                        }
                      }
                      ExpKind::Application(_, args) => {
                        for arg in args.iter() {
                          if let ExpKind::Name(pattern_name) = &arg.kind {
                            if &**pattern_name == name {
                              Some(NameDefinitionSource::LocalBinding(
                                arg.source_trace.primary_path(),
                              ));
                            }
                          }
                        }
                      }
                      _ => {}
                    }
                  }
                }
              }
              _ => {}
            }
            Ok::<bool, Never>(true)
          })
          .unwrap();
      }
    }
    None
  }
}
