//! Types WGSL can't hold in a binding as they are. A `bool` isn't
//! host-shareable, so a uniform or storage binding whose type involves one
//! is declared with its *host type*: every `bool` stored as a `u32` and
//! every bool vector as a `u32` vector (0 or 1 per bool, which is how both
//! CPU runtimes already lay a bool out), and every struct holding one as a
//! generated copy of the struct with those fields. An enum's payload is
//! already `u32` words, but a uniform's arrays need a 16-byte stride, so in
//! a uniform an enum with a payload is held as a generated struct with its
//! data words in `vec4u`s (`BufferLayout::Uniform`, which the CPU runtimes
//! upload in the same layout).
//!
//! Only emission sees this. Shader code reads and writes such a binding
//! through places rooted at it (`TypedExp::host_encoded_place`), which
//! compile to their raw access, converted where the value leaves or enters
//! the binding: a read of a place whose type involves a bool (or, in a
//! uniform, an enum) is unpacked, and a value assigned to one is packed. Everything inside a place (field
//! accesses, indexing, swizzles) reads the host type as it is, so `(.count
//! (layers i))` costs nothing and `(.on (layers i))` one comparison.
//!
//! Values passed between a vertex and a fragment shader can't be bools
//! either, so a struct whose located fields include one has an IO copy too
//! (`IoStruct`: the same attributes, a flat `u32` per bool), which the
//! entries pass in its place: an entry with such an input or output is
//! emitted as a plain function holding its body and an entry wrapping it,
//! which unpacks its inputs and packs its result
//! (`TopLevelFunction::compile_io_wrapper`).

use std::collections::HashMap;
use std::sync::Arc;

use crate::compiler::{
  entry::IOAttributes,
  program::{CompilerTarget, NameContext},
  types::{BufferLayout, ConcreteArraySize, Type},
  util::compile_word,
};

/// The host-encoded bindings of one WGSL emission, and the definitions
/// their conversions need.
#[derive(Debug, Clone, Default)]
pub(crate) struct HostEncoding {
  /// Each global declared with its host type, by name (storage read views
  /// included), with its binding's layout.
  pub(crate) globals: HashMap<Arc<str>, BufferLayout>,
  /// Each host struct's name, by its struct's (or enum's) WGSL name and
  /// layout.
  host_structs: HashMap<(String, BufferLayout), String>,
  /// Each struct's, enum's, or fixed array's conversion functions (unpack,
  /// pack), by its WGSL name and layout.
  conversions: HashMap<(String, BufferLayout), (String, String)>,
  /// Each IO copy of a struct with located bools, by the struct's WGSL
  /// name.
  io_structs: HashMap<String, IoStruct>,
  /// The host structs and conversion functions used so far. WGSL resolves
  /// module-scope names in any order, so they're emitted after everything
  /// else.
  pub(crate) definitions: Vec<String>,
}

/// The IO copy of a struct whose located fields include a bool, and the
/// functions converting the struct to and from it.
#[derive(Debug, Clone)]
pub(crate) struct IoStruct {
  pub(crate) name: String,
  pub(crate) unpack: String,
  pub(crate) pack: String,
}

impl Type {
  /// Whether a binding of this type with `layout` has to be host-encoded:
  /// whether it holds a `bool` outside an enum payload, or, in a uniform, an
  /// enum with a payload (see the module docs).
  pub(crate) fn needs_host_encoding(&self, layout: BufferLayout) -> bool {
    match self {
      Type::Bool => true,
      Type::Enum(e) => {
        layout == BufferLayout::Uniform
          && e.inner_flat_data_size_in_u32s().unwrap_or(0) > 0
      }
      Type::Array(_, inner) => {
        inner.with_known(|inner| inner.needs_host_encoding(layout))
      }
      Type::Struct(s) => s.fields.iter().any(|f| {
        f.field_type
          .with_known(|field_type| field_type.needs_host_encoding(layout))
      }),
      Type::Function(_) => self
        .closure_data_type()
        .is_some_and(|data| data.needs_host_encoding(layout)),
      _ => false,
    }
  }
}

/// The vector width of a bool vector type.
fn bool_vector_width(t: &Type) -> Option<usize> {
  (t.vector_element_type() == Some(Type::Bool)).then(|| {
    let Type::Struct(s) = t else { unreachable!() };
    match &*s.name {
      "vec2" => 2,
      "vec3" => 3,
      _ => 4,
    }
  })
}

impl NameContext {
  /// Starts a WGSL emission whose host-encoded globals are `globals`.
  pub(crate) fn begin_host_encoding(
    &mut self,
    globals: HashMap<Arc<str>, BufferLayout>,
  ) {
    self.host_encoding = HostEncoding {
      globals,
      ..Default::default()
    };
  }
  /// Defines the IO copy of the struct `name` (with `fields`: each one's
  /// compiled name, attributes, and type) if a located field holds a bool,
  /// returning whether it did.
  pub(crate) fn register_io_struct(
    &mut self,
    name: String,
    fields: Vec<(String, IOAttributes, Type)>,
  ) -> bool {
    if !fields.iter().any(|(_, attributes, t)| {
      attributes.location().is_some() && t.is_bool_scalar_or_vector()
    }) {
      return false;
    }
    let io = IoStruct {
      name: self.gensym(&format!("{name}_io")).to_string(),
      unpack: self.gensym(&format!("easl_unpack_{name}_io")).to_string(),
      pack: self.gensym(&format!("easl_pack_{name}_io")).to_string(),
    };
    let mut definition = vec![];
    let mut unpacked = vec![];
    let mut packed = vec![];
    for (field, attributes, t) in &fields {
      definition.push(format!(
        "  {}{field}: {}",
        attributes.compile(t),
        self.host_type_name(t, BufferLayout::Storage)
      ));
      unpacked.push(self.host_unpack(
        t,
        BufferLayout::Storage,
        format!("h.{field}"),
      ));
      packed.push(self.host_pack(
        t,
        BufferLayout::Storage,
        format!("v.{field}"),
      ));
    }
    let IoStruct {
      name: io_name,
      unpack,
      pack,
    } = &io;
    self.host_encoding.definitions.extend([
      format!("struct {io_name} {{\n{}\n}}", definition.join(",\n")),
      format!(
        "fn {unpack}(h: {io_name}) -> {name} {{\n  return {name}({});\n}}",
        unpacked.join(", ")
      ),
      format!(
        "fn {pack}(v: {name}) -> {io_name} {{\n  return {io_name}({});\n}}",
        packed.join(", ")
      ),
    ]);
    self.host_encoding.io_structs.insert(name, io);
    true
  }
  /// The IO copy of the struct type `t`, if it has one.
  pub(crate) fn io_struct(&mut self, t: &Type) -> Option<IoStruct> {
    if !matches!(t, Type::Struct(_)) {
      return None;
    }
    let name = t.monomorphized_name(self, CompilerTarget::WGSL);
    self.host_encoding.io_structs.get(&name).cloned()
  }
  /// The WGSL host type standing for `t` in a binding with `layout`.
  pub(crate) fn host_type_name(
    &mut self,
    t: &Type,
    layout: BufferLayout,
  ) -> String {
    if !t.needs_host_encoding(layout) {
      return t.monomorphized_name(self, CompilerTarget::WGSL);
    }
    if let Some(width) = bool_vector_width(t) {
      return format!("vec{width}u");
    }
    match t {
      Type::Bool => "u32".to_string(),
      Type::Array(size, inner) => {
        let inner = self.host_type_name(&inner.unwrap_known(), layout);
        match size.clone().map(|size| size.compile_type()) {
          Some(size) if !size.is_empty() => format!("array<{inner}, {size}>"),
          _ => format!("array<{inner}>"),
        }
      }
      Type::Function(_) => {
        self.host_type_name(&t.closure_data_type().unwrap(), layout)
      }
      Type::Struct(_) | Type::Enum(_) => {
        let name = t.monomorphized_name(self, CompilerTarget::WGSL);
        let key = (name.clone(), layout);
        if let Some(host_name) = self.host_encoding.host_structs.get(&key) {
          return host_name.clone();
        }
        let host_name = self
          .gensym(&match t {
            Type::Enum(_) => format!("{name}_uniform"),
            _ => format!("{name}_host"),
          })
          .to_string();
        self
          .host_encoding
          .host_structs
          .insert(key, host_name.clone());
        let fields = match t {
          Type::Struct(s) => s
            .fields
            .iter()
            .map(|field| {
              let field_type =
                self.host_type_name(&field.field_type.unwrap_known(), layout);
              format!("  {}: {field_type}", s.compiled_field_name(&field.name))
            })
            .collect::<Vec<_>>()
            .join(",\n"),
          _ => format!(
            "  discriminant: u32,\n  data: array<vec4u, {}>",
            uniform_enum_data_vectors(t)
          ),
        };
        self
          .host_encoding
          .definitions
          .push(format!("struct {host_name} {{\n{fields}\n}}"));
        host_name
      }
      _ => unreachable!(
        "only bools, vectors, enums, structs, and arrays are host-encoded"
      ),
    }
  }
  /// `raw`, a value read from a binding with `layout` as its host type,
  /// converted to `t`.
  pub(crate) fn host_unpack(
    &mut self,
    t: &Type,
    layout: BufferLayout,
    raw: String,
  ) -> String {
    if !t.needs_host_encoding(layout) {
      return raw;
    }
    if let Some(width) = bool_vector_width(t) {
      return format!("({raw} != vec{width}u(0u))");
    }
    match t {
      Type::Bool => format!("({raw} != 0u)"),
      Type::Function(_) => {
        self.host_unpack(&t.closure_data_type().unwrap(), layout, raw)
      }
      _ => {
        let (unpack, _) = self.host_conversions(t, layout);
        format!("{unpack}({raw})")
      }
    }
  }
  /// `value`, of type `t`, converted to its host type to be stored in a
  /// binding with `layout`.
  pub(crate) fn host_pack(
    &mut self,
    t: &Type,
    layout: BufferLayout,
    value: String,
  ) -> String {
    if !t.needs_host_encoding(layout) {
      return value;
    }
    if let Some(width) = bool_vector_width(t) {
      return format!("vec{width}u({value})");
    }
    match t {
      Type::Bool => format!("u32({value})"),
      Type::Function(_) => {
        self.host_pack(&t.closure_data_type().unwrap(), layout, value)
      }
      _ => {
        let (_, pack) = self.host_conversions(t, layout);
        format!("{pack}({value})")
      }
    }
  }
  /// The functions converting a struct, enum, or fixed-size array between
  /// `t` and its host type in `layout`, defining them the first time.
  fn host_conversions(
    &mut self,
    t: &Type,
    layout: BufferLayout,
  ) -> (String, String) {
    let name = t.monomorphized_name(self, CompilerTarget::WGSL);
    let key = (name.clone(), layout);
    if let Some(conversions) = self.host_encoding.conversions.get(&key) {
      return conversions.clone();
    }
    let host_name = self.host_type_name(t, layout);
    let base = compile_word(match t {
      Type::Struct(_) | Type::Enum(_) => name.clone().into(),
      _ => Arc::<str>::from("array"),
    });
    let unpack = self.gensym(&format!("easl_unpack_{base}")).to_string();
    let pack = self.gensym(&format!("easl_pack_{base}")).to_string();
    self
      .host_encoding
      .conversions
      .insert(key, (unpack.clone(), pack.clone()));
    let (unpacked, packed) = match t {
      Type::Struct(s) => {
        let mut unpacked = vec![];
        let mut packed = vec![];
        for field in &s.fields {
          let field_type = field.field_type.unwrap_known();
          let access = format!(".{}", s.compiled_field_name(&field.name));
          unpacked.push(self.host_unpack(
            &field_type,
            layout,
            format!("h{access}"),
          ));
          packed.push(self.host_pack(
            &field_type,
            layout,
            format!("v{access}"),
          ));
        }
        (
          format!("  return {name}({});", unpacked.join(", ")),
          format!("  return {host_name}({});", packed.join(", ")),
        )
      }
      Type::Enum(e) => {
        let words = e.inner_flat_data_size_in_u32s().unwrap();
        let vectors = uniform_enum_data_vectors(t);
        let convert = |source: &str, result_type: &str, data_type, word| {
          format!(
            "  var data: {data_type};\n  \
             for (var i = 0u; i < {words}u; i++) {{\n    \
             {word};\n  \
             }}\n  \
             return {result_type}({source}.discriminant, data);"
          )
        };
        (
          convert(
            "h",
            &name,
            format!("array<u32, {words}>"),
            "data[i] = h.data[i / 4u][i % 4u]",
          ),
          convert(
            "v",
            &host_name,
            format!("array<vec4u, {vectors}>"),
            "data[i / 4u][i % 4u] = v.data[i]",
          ),
        )
      }
      Type::Array(Some(ConcreteArraySize::Literal(length)), element) => {
        let element = element.unwrap_known();
        let unpack_element =
          self.host_unpack(&element, layout, "h[i]".to_string());
        let pack_element = self.host_pack(&element, layout, "v[i]".to_string());
        let convert = |result_type: &str, element: String| {
          format!(
            "  var result: {result_type};\n  \
             for (var i = 0u; i < {length}u; i++) {{\n    \
             result[i] = {element};\n  \
             }}\n  \
             return result;"
          )
        };
        (
          convert(&name, unpack_element),
          convert(&host_name, pack_element),
        )
      }
      _ => unreachable!(
        "only structs, enums, and fixed-size arrays are converted whole: a \
         runtime-sized array is never a value on the GPU"
      ),
    };
    self.host_encoding.definitions.push(format!(
      "fn {unpack}(h: {host_name}) -> {name} {{\n{unpacked}\n}}"
    ));
    self.host_encoding.definitions.push(format!(
      "fn {pack}(v: {name}) -> {host_name} {{\n{packed}\n}}"
    ));
    (unpack, pack)
  }
}

/// How many `vec4u`s a uniform holds the enum `t`'s data words in.
fn uniform_enum_data_vectors(t: &Type) -> usize {
  let Type::Enum(e) = t else { unreachable!() };
  e.inner_flat_data_size_in_u32s().unwrap().div_ceil(4)
}
