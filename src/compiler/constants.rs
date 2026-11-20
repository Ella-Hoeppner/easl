//! Values computed at compile time, from `def`s and the expressions in
//! them. Two things need them:
//! - an array size naming a `def` (`Program::inline_def_array_sizes`),
//!   computed before type inference, since the size is part of a type: an
//!   unsuffixed literal takes the type its context gives;
//! - an enum construction in a top-level initializer, which WGSL emission
//!   writes with its payload's words computed here
//!   (`TypedExp::compile_enum_value`): a WGSL initializer must be a constant
//!   expression, and only `bitcast` (which naga doesn't evaluate in one)
//!   could make the words from a float. An initializer holding a
//!   construction this can't compute isn't a constant expression
//!   (`Program::non_constant_initializers`).

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::compiler::{
  expression::{Accessor, ExpKind, Number, TypedExp},
  functions::{FunctionImplementationKind, extract_vec_size},
  program::Program,
  types::{Type, TypeState},
  vars::TopLevelVariableKind,
};

/// A value computed at compile time.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ConstantValue {
  /// A number or bool: its type and its bits (a bool's are 0 or 1).
  Scalar(Type, u32),
  /// A vector's components, a struct's fields, an array's elements, or an
  /// enum's discriminant and data words, in order.
  Composite(Vec<ConstantValue>),
}

/// Why an expression has no value `ConstantFolder` computes.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum FoldError {
  /// It isn't built only from what the folder computes.
  NotConstant,
  /// Computing it divides an integer by zero (an error at runtime too).
  DividesByZero,
}

type FoldResult = Result<ConstantValue, FoldError>;

impl ConstantValue {
  /// The words an enum payload holds this in: its leaves' bits, in order,
  /// as `Type::bitcastable_chunk_accessors` lays a payload out.
  pub(crate) fn words(&self) -> Vec<u32> {
    match self {
      ConstantValue::Scalar(_, bits) => vec![*bits],
      ConstantValue::Composite(parts) => {
        parts.iter().flat_map(ConstantValue::words).collect()
      }
    }
  }
  /// The integer this is, if it's a `u32` or an `i32`.
  pub(crate) fn integer(&self) -> Option<i64> {
    match self {
      ConstantValue::Scalar(Type::U32, bits) => Some(*bits as i64),
      ConstantValue::Scalar(Type::I32, bits) => Some(*bits as i32 as i64),
      _ => None,
    }
  }
  /// The type of this value's first scalar, which an unsuffixed literal
  /// combined with it takes.
  fn scalar_type(&self) -> Option<Type> {
    match self {
      ConstantValue::Scalar(t, _) => Some(t.clone()),
      ConstantValue::Composite(parts) => parts.first()?.scalar_type(),
    }
  }
  /// Whether this can be a value of `t` (scalars and vectors are checked;
  /// anything else is left to type inference).
  fn fits(&self, t: &Type) -> bool {
    match self {
      ConstantValue::Scalar(scalar_type, _) => scalar_type == t,
      ConstantValue::Composite(parts) => match t.vector_element_type() {
        Some(element) => {
          let Type::Struct(s) = t else { unreachable!() };
          parts.len() == s.fields.len()
            && parts.iter().all(|part| part.fits(&element))
        }
        None => !matches!(t, Type::F32 | Type::I32 | Type::U32 | Type::Bool),
      },
    }
  }
  fn components(self) -> Option<Vec<ConstantValue>> {
    match self {
      ConstantValue::Composite(parts) => Some(parts),
      ConstantValue::Scalar(..) => None,
    }
  }
  /// This scalar converted to `to`, as the runtimes convert: an integer's
  /// bits are kept, and a float to an integer saturates (as Rust's `as`
  /// does).
  fn convert(&self, to: &Type) -> Option<ConstantValue> {
    let ConstantValue::Scalar(from, bits) = self else {
      return None;
    };
    let bits = *bits;
    let converted = match (from, to) {
      (from, to) if from == to => bits,
      (Type::F32, Type::I32) => f32::from_bits(bits) as i32 as u32,
      (Type::F32, Type::U32) => f32::from_bits(bits) as u32,
      (Type::I32, Type::F32) => (bits as i32 as f32).to_bits(),
      (Type::U32, Type::F32) => (bits as f32).to_bits(),
      (Type::I32, Type::U32) | (Type::U32, Type::I32) => bits,
      _ => return None,
    };
    Some(ConstantValue::Scalar(to.clone(), converted))
  }
  /// `self op other`, as the runtimes compute it: componentwise, a scalar
  /// applying to each of a composite's components; integer `+`, `-`, and
  /// `*` wrap, and `/` and `%` truncate toward zero (so `%` takes the sign
  /// of the dividend).
  fn arithmetic(self, op: &str, other: ConstantValue) -> FoldResult {
    use ConstantValue::*;
    match (self, other) {
      (Scalar(t, a), Scalar(other_type, b)) if t == other_type => {
        if matches!(t, Type::I32 | Type::U32)
          && matches!(op, "/" | "%")
          && b == 0
        {
          return Err(FoldError::DividesByZero);
        }
        let bits = match t {
          Type::F32 => {
            let (a, b) = (f32::from_bits(a), f32::from_bits(b));
            match op {
              "+" => a + b,
              "-" => a - b,
              "*" => a * b,
              "/" => a / b,
              _ => a % b,
            }
            .to_bits()
          }
          Type::I32 => {
            let (a, b) = (a as i32, b as i32);
            (match op {
              "+" => a.wrapping_add(b),
              "-" => a.wrapping_sub(b),
              "*" => a.wrapping_mul(b),
              "/" => a.wrapping_div(b),
              _ => a.wrapping_rem(b),
            }) as u32
          }
          Type::U32 => match op {
            "+" => a.wrapping_add(b),
            "-" => a.wrapping_sub(b),
            "*" => a.wrapping_mul(b),
            "/" => a / b,
            _ => a % b,
          },
          _ => return Err(FoldError::NotConstant),
        };
        Ok(Scalar(t, bits))
      }
      (Composite(a), Composite(b)) if a.len() == b.len() => a
        .into_iter()
        .zip(b)
        .map(|(a, b)| a.arithmetic(op, b))
        .collect::<Result<_, _>>()
        .map(Composite),
      (Composite(a), b @ Scalar(..)) => a
        .into_iter()
        .map(|a| a.arithmetic(op, b.clone()))
        .collect::<Result<_, _>>()
        .map(Composite),
      (a @ Scalar(..), Composite(b)) => b
        .into_iter()
        .map(|b| a.clone().arithmetic(op, b))
        .collect::<Result<_, _>>()
        .map(Composite),
      _ => Err(FoldError::NotConstant),
    }
  }
  /// The negation of this signed number, or of each component.
  fn negated(self) -> FoldResult {
    match self {
      ConstantValue::Scalar(Type::F32, bits) => Ok(ConstantValue::Scalar(
        Type::F32,
        (-f32::from_bits(bits)).to_bits(),
      )),
      ConstantValue::Scalar(Type::I32, bits) => Ok(ConstantValue::Scalar(
        Type::I32,
        (bits as i32).wrapping_neg() as u32,
      )),
      ConstantValue::Composite(parts) => parts
        .into_iter()
        .map(ConstantValue::negated)
        .collect::<Result<_, _>>()
        .map(ConstantValue::Composite),
      _ => Err(FoldError::NotConstant),
    }
  }
}

/// Computes the values of expressions built from literals; vector
/// constructions; the scalar conversions; `+`, `-`, `*`, `/`, and `%`;
/// field, swizzle, and constant-index accesses; and the `def`s computed from
/// those. In a typed expression, it also computes struct, array, and enum
/// constructions and unit variants.
#[derive(Debug, Clone)]
pub(crate) struct ConstantFolder {
  /// Each `def`'s declared type and initializer.
  defs: HashMap<Arc<str>, (Type, TypedExp)>,
  /// The base name of each monomorphized name, which a generic enum's unit
  /// variants are referred to by.
  base_names: HashMap<Arc<str>, Arc<str>>,
}

impl Program {
  /// A folder for this program's `def`s and the expressions in them.
  pub(crate) fn constant_folder(&self) -> ConstantFolder {
    ConstantFolder {
      defs: self
        .top_level_vars
        .iter()
        .filter(|v| v.kind == TopLevelVariableKind::Const)
        .filter_map(|v| {
          Some((v.name.clone(), (v.var_type.clone(), v.value.clone()?)))
        })
        .collect(),
      base_names: self.names.read().unwrap().monomorphized_to_base_names(),
    }
  }
}

impl ConstantFolder {
  /// A folding of several expressions, sharing the `def`s it computes.
  /// With `integers_only`, a value involving anything but integers isn't
  /// computed (an array size is computed from integers alone).
  pub(crate) fn folding(&self, integers_only: bool) -> Folding<'_> {
    Folding {
      folder: self,
      integers_only,
      folded: HashMap::new(),
      visiting: HashSet::new(),
    }
  }
  /// The value of the typed expression `exp`.
  pub(crate) fn fold(&self, exp: &TypedExp) -> FoldResult {
    self.folding(false).exp(exp, None)
  }
}

/// A run of `ConstantFolder`, remembering the `def`s it has computed.
pub(crate) struct Folding<'a> {
  folder: &'a ConstantFolder,
  integers_only: bool,
  folded: HashMap<Arc<str>, FoldResult>,
  /// The `def`s being computed, to stop at a cycle (which is reported as
  /// `CyclicTopLevelDefinitions`).
  visiting: HashSet<Arc<str>>,
}

impl Folding<'_> {
  /// The value of the `def` `name`, of its declared type.
  pub(crate) fn def(&mut self, name: &Arc<str>) -> FoldResult {
    if let Some(result) = self.folded.get(name) {
      return result.clone();
    }
    let Some((t, value)) = self.folder.defs.get(name) else {
      return Err(FoldError::NotConstant);
    };
    if !self.visiting.insert(name.clone()) {
      return Err(FoldError::NotConstant);
    }
    let result = self.exp(value, Some(t)).and_then(|value| {
      if value.fits(t) {
        Ok(value)
      } else {
        Err(FoldError::NotConstant)
      }
    });
    self.visiting.remove(name);
    self.folded.insert(name.clone(), result.clone());
    result
  }
  /// The value of `exp`. `expected` is the type its context gives it,
  /// which an unsuffixed integer literal takes when it has no type yet.
  pub(crate) fn exp(
    &mut self,
    exp: &TypedExp,
    expected: Option<&Type>,
  ) -> FoldResult {
    let not_constant = Err(FoldError::NotConstant);
    let known = exp.data.kind.try_unwrap_known();
    match &exp.kind {
      ExpKind::NumberLiteral(number) => {
        let t = known.or_else(|| match number {
          Number::Int(_) => expected.cloned(),
          Number::Float(_) => Some(Type::F32),
        });
        let bits = match (number, &t) {
          (Number::Int(i), Some(Type::F32)) => (*i as f32).to_bits(),
          (Number::Int(i), Some(Type::U32)) => {
            u32::try_from(*i).map_err(|_| FoldError::NotConstant)?
          }
          (Number::Int(i), Some(Type::I32)) => {
            i32::try_from(*i).map_err(|_| FoldError::NotConstant)? as u32
          }
          (Number::Float(f), Some(Type::F32)) => (*f as f32).to_bits(),
          _ => return not_constant,
        };
        let t = t.unwrap();
        if self.integers_only && !matches!(t, Type::U32 | Type::I32) {
          return not_constant;
        }
        Ok(ConstantValue::Scalar(t, bits))
      }
      ExpKind::BooleanLiteral(b) if !self.integers_only => {
        Ok(ConstantValue::Scalar(Type::Bool, *b as u32))
      }
      ExpKind::Name(name) => {
        if self.folder.defs.contains_key(name) {
          return self.def(name);
        }
        let Some(t @ Type::Enum(e)) = &known else {
          return not_constant;
        };
        let base = self.folder.base_names.get(name).unwrap_or(name);
        let Some(variant) = e.variants.iter().position(|v| {
          v.name == *base && v.inner_type.with_known(|t| *t == Type::Unit)
        }) else {
          return not_constant;
        };
        Ok(enum_value(t, variant, None))
      }
      ExpKind::ArrayLiteral(elements) if known.is_some() => elements
        .iter()
        .map(|element| self.exp(element, None))
        .collect::<Result<_, _>>()
        .map(ConstantValue::Composite),
      ExpKind::Access(accessor, inner) => {
        let index = match accessor {
          Accessor::ArrayIndex(index) => Some(self.index(index)?),
          _ => None,
        };
        let parts = self
          .exp(inner, None)?
          .components()
          .ok_or(FoldError::NotConstant)?;
        let part =
          |i: usize| parts.get(i).cloned().ok_or(FoldError::NotConstant);
        match accessor {
          Accessor::Field(field) => {
            let index = match inner.data.kind.try_unwrap_known() {
              Some(Type::Struct(s)) => {
                s.fields.iter().position(|f| f.name == *field)
              }
              _ => ["x", "y", "z", "w"].iter().position(|c| **c == **field),
            };
            part(index.ok_or(FoldError::NotConstant)?)
          }
          Accessor::Swizzle(fields) => fields
            .iter()
            .map(|field| part(field.index()))
            .collect::<Result<_, _>>()
            .map(ConstantValue::Composite),
          Accessor::ArrayIndex(_) => part(index.unwrap()),
        }
      }
      ExpKind::Application(callee, args) => {
        self.application(exp, callee, args, known, expected)
      }
      _ => not_constant,
    }
  }
  fn index(&mut self, index: &TypedExp) -> Result<usize, FoldError> {
    self
      .exp(index, Some(&Type::U32))?
      .integer()
      .and_then(|i| usize::try_from(i).ok())
      .ok_or(FoldError::NotConstant)
  }
  fn application(
    &mut self,
    exp: &TypedExp,
    callee: &TypedExp,
    args: &[TypedExp],
    known: Option<Type>,
    expected: Option<&Type>,
  ) -> FoldResult {
    let not_constant = Err(FoldError::NotConstant);
    if let Some(variant) = exp.constructed_enum_variant() {
      let Some(t @ Type::Enum(e)) = &known else {
        return not_constant;
      };
      let Some(variant) = e.variants.iter().position(|v| v.name == variant)
      else {
        return not_constant;
      };
      let payload = match args.first() {
        Some(payload) => Some(self.exp(payload, None)?),
        None => None,
      };
      return Ok(enum_value(t, variant, payload));
    }
    // An index into an array value.
    if callee.data.is_known_and(|t| matches!(t, Type::Array(..))) {
      let index = self.index(&args[0])?;
      return self
        .exp(callee, None)?
        .components()
        .and_then(|parts| parts.into_iter().nth(index))
        .ok_or(FoldError::NotConstant);
    }
    if let TypeState::Known(Type::Function(signature)) = &callee.data.kind
      && let Some(ancestor) = &signature.abstract_ancestor
      && let FunctionImplementationKind::StructConstructor =
        ancestor.read().unwrap().implementation
      && known
        .as_ref()
        .is_some_and(|t| t.vector_element_type().is_none())
    {
      return args
        .iter()
        .map(|arg| self.exp(arg, None))
        .collect::<Result<_, _>>()
        .map(ConstantValue::Composite);
    }
    let ExpKind::Name(op) = &callee.kind else {
      return not_constant;
    };
    if let Some(width) = extract_vec_size(op) {
      return self.vector(op, width, args, known.or(expected.cloned()));
    }
    match (&**op, args) {
      ("f32" | "i32" | "u32", [arg]) => {
        let to = match &**op {
          "f32" => Type::F32,
          "i32" => Type::I32,
          _ => Type::U32,
        };
        self
          .exp(arg, None)?
          .convert(&to)
          .ok_or(FoldError::NotConstant)
      }
      ("-", [arg]) => self.exp(arg, expected)?.negated(),
      ("+" | "-" | "*" | "/" | "%", [_, _, ..]) => {
        // The operands' scalars share a type: the one the context gives,
        // or else the first operand's own.
        let scalar = expected
          .and_then(|t| t.vector_element_type().or(Some(t.clone())))
          .or_else(|| {
            args.iter().find_map(|arg| {
              self.exp(arg, None).ok().and_then(|v| v.scalar_type())
            })
          });
        let mut operands = args
          .iter()
          .map(|arg| self.exp(arg, scalar.as_ref()))
          .collect::<Result<Vec<_>, _>>()?
          .into_iter();
        let first = operands.next().unwrap();
        // `+` and `*` take any number of operands until associative
        // expansion; the others take two.
        if !matches!(&**op, "+" | "*") && operands.len() != 1 {
          return not_constant;
        }
        operands.try_fold(first, |a, b| a.arithmetic(op, b))
      }
      _ => not_constant,
    }
  }
  /// The vector of `width` the constructor `name` builds from `args`: its
  /// components in order, or one repeated, each converted to its element
  /// type (named by the constructor's suffix, or else `t`'s).
  fn vector(
    &mut self,
    name: &str,
    width: usize,
    args: &[TypedExp],
    t: Option<Type>,
  ) -> FoldResult {
    let element = match name.chars().last() {
      Some('f') => Some(Type::F32),
      Some('i') => Some(Type::I32),
      Some('u') => Some(Type::U32),
      Some('b') => Some(Type::Bool),
      _ => t.and_then(|t| t.vector_element_type()),
    }
    .ok_or(FoldError::NotConstant)?;
    let components = args
      .iter()
      .map(|arg| self.exp(arg, Some(&element)))
      .collect::<Result<Vec<_>, _>>()?
      .into_iter()
      .flat_map(|value| match value {
        ConstantValue::Composite(parts) => parts,
        scalar => vec![scalar],
      })
      .map(|scalar| scalar.convert(&element).ok_or(FoldError::NotConstant))
      .collect::<Result<Vec<_>, _>>()?;
    match components.len() {
      1 => Ok(ConstantValue::Composite(vec![components[0].clone(); width])),
      n if n == width => Ok(ConstantValue::Composite(components)),
      _ => Err(FoldError::NotConstant),
    }
  }
}

/// The value of the enum `t`'s variant `variant` holding `payload`: its
/// discriminant, then its data words, the payload's words followed by
/// zeros.
fn enum_value(
  t: &Type,
  variant: usize,
  payload: Option<ConstantValue>,
) -> ConstantValue {
  let Type::Enum(e) = t else {
    unreachable!("only enums have variants")
  };
  let word_count = e
    .inner_flat_data_size_in_u32s()
    .expect("a concrete enum has a flat size");
  let mut words = payload.map(|p| p.words()).unwrap_or_default();
  words.resize(word_count, 0);
  ConstantValue::Composite(
    std::iter::once(ConstantValue::Scalar(Type::U32, variant as u32))
      .chain(
        words
          .into_iter()
          .map(|w| ConstantValue::Scalar(Type::U32, w)),
      )
      .collect(),
  )
}
