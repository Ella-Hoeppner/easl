//! Concrete types in a serializable form, so a precompiled program (see
//! `crate::precompiled`) can carry the types its runtime metadata names
//! without the compiler. A type is written as its structure, plus what the
//! runtime reads of a struct's or enum's lineage (its original name, and
//! whether it's opaque), and rebuilt from that when read. A function type
//! is written as the data its values are laid out as (its closure scope, or
//! unit), as everything host-side treats it (`Type::closure_data_type`).

use std::{collections::HashMap, sync::Arc};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use super::{
  entry::IOAttributes,
  enums::{AbstractEnum, AbstractEnumVariant, Enum, EnumVariant},
  error::SourceTrace,
  structs::{AbstractStruct, AbstractStructField, Struct, StructField},
  types::{AbstractType, ConcreteArraySize, ExpTypeInfo, Type, TypeState},
};

#[derive(Serialize, Deserialize)]
enum SerializedType {
  Unit,
  F32,
  I32,
  U32,
  Bool,
  String,
  Struct {
    name: Arc<str>,
    original_name: Arc<str>,
    opaque: bool,
    fields: Vec<(Arc<str>, SerializedType)>,
  },
  Enum {
    name: Arc<str>,
    original_name: Arc<str>,
    variants: Vec<(Arc<str>, SerializedType)>,
  },
  /// `None` for a runtime-sized array.
  Array(Option<u32>, Box<SerializedType>),
}

impl SerializedType {
  fn of(t: &Type) -> Self {
    let known = |t: &ExpTypeInfo| t.kind.with_known(Self::of);
    match t {
      Type::Unit => Self::Unit,
      Type::F32 => Self::F32,
      Type::I32 => Self::I32,
      Type::U32 => Self::U32,
      Type::Bool => Self::Bool,
      Type::String => Self::String,
      Type::Struct(s) => Self::Struct {
        name: s.name.clone(),
        original_name: s.abstract_ancestor.original_ancestor().name.0.clone(),
        opaque: s.abstract_ancestor.opaque,
        fields: s
          .fields
          .iter()
          .map(|field| (field.name.clone(), known(&field.field_type)))
          .collect(),
      },
      Type::Enum(e) => Self::Enum {
        name: e.name.clone(),
        original_name: original_enum(&e.abstract_ancestor).name.0.clone(),
        variants: e
          .variants
          .iter()
          .map(|variant| (variant.name.clone(), known(&variant.inner_type)))
          .collect(),
      },
      Type::Array(size, element) => Self::Array(
        match size {
          Some(ConcreteArraySize::Unsized) | None => None,
          Some(size) => Some(size.as_literal().unwrap_or_else(|| {
            panic!("an unresolved array size reached the runtime: {size:?}")
          })),
        },
        Box::new(known(element)),
      ),
      Type::Function(_) => Self::of(
        &t.closure_data_type()
          .expect("a function type reached the runtime without its function"),
      ),
      Type::BoxedFunction(_) | Type::Skolem(..) => {
        panic!("a type that never reaches the runtime was serialized: {t:?}")
      }
    }
  }

  fn into_type(self) -> Type {
    match self {
      Self::Unit => Type::Unit,
      Self::F32 => Type::F32,
      Self::I32 => Type::I32,
      Self::U32 => Type::U32,
      Self::Bool => Type::Bool,
      Self::String => Type::String,
      Self::Struct {
        name,
        original_name,
        opaque,
        fields,
      } => {
        let fields: Vec<(Arc<str>, Type)> = fields
          .into_iter()
          .map(|(name, t)| (name, t.into_type()))
          .collect();
        let definition =
          |name: Arc<str>, ancestor: Option<Arc<AbstractStruct>>| {
            Arc::new(AbstractStruct {
              name: (name, SourceTrace::empty()),
              filled_generics: HashMap::new(),
              fields: fields
                .iter()
                .map(|(name, t)| AbstractStructField {
                  attributes: IOAttributes::empty(SourceTrace::empty()),
                  name: name.clone(),
                  field_type: AbstractType::Type(t.clone()),
                  source_trace: SourceTrace::empty(),
                })
                .collect(),
              generic_args: vec![],
              abstract_ancestor: ancestor,
              source_trace: SourceTrace::empty(),
              opaque,
            })
          };
        let original = definition(original_name.clone(), None);
        let abstract_ancestor = if name == original_name {
          original
        } else {
          definition(name.clone(), Some(original))
        };
        Type::Struct(Struct {
          name,
          fields: fields
            .into_iter()
            .map(|(name, t)| StructField {
              attributes: IOAttributes::empty(SourceTrace::empty()),
              name,
              field_type: TypeState::Known(t).into(),
            })
            .collect(),
          abstract_ancestor,
        })
      }
      Self::Enum {
        name,
        original_name,
        variants,
      } => {
        let variants: Vec<(Arc<str>, Type)> = variants
          .into_iter()
          .map(|(name, t)| (name, t.into_type()))
          .collect();
        let definition =
          |name: Arc<str>, ancestor: Option<Arc<AbstractEnum>>| {
            Arc::new(AbstractEnum {
              name: (name, SourceTrace::empty()),
              filled_generics: HashMap::new(),
              generic_args: vec![],
              variants: variants
                .iter()
                .map(|(name, t)| AbstractEnumVariant {
                  name: name.clone(),
                  source: SourceTrace::empty(),
                  inner_type: AbstractType::Type(t.clone()),
                })
                .collect(),
              abstract_ancestor: ancestor,
              source_trace: SourceTrace::empty(),
            })
          };
        let original = definition(original_name.clone(), None);
        let abstract_ancestor = if name == original_name {
          original
        } else {
          definition(name.clone(), Some(original))
        };
        Type::Enum(Enum {
          name,
          variants: variants
            .into_iter()
            .map(|(name, t)| EnumVariant {
              name,
              inner_type: TypeState::Known(t).into(),
            })
            .collect(),
          abstract_ancestor,
        })
      }
      Self::Array(size, element) => Type::Array(
        Some(
          size.map_or(ConcreteArraySize::Unsized, ConcreteArraySize::Literal),
        ),
        Box::new(TypeState::Known(element.into_type()).into()),
      ),
    }
  }
}

fn original_enum(e: &AbstractEnum) -> &AbstractEnum {
  match &e.abstract_ancestor {
    Some(parent) => original_enum(parent),
    None => e,
  }
}

impl Serialize for Type {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    SerializedType::of(self).serialize(serializer)
  }
}

impl<'de> Deserialize<'de> for Type {
  fn deserialize<D: Deserializer<'de>>(
    deserializer: D,
  ) -> Result<Self, D::Error> {
    Ok(SerializedType::deserialize(deserializer)?.into_type())
  }
}
