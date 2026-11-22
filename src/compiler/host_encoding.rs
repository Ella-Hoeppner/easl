//! Types WGSL can't hold in a binding as they are. A `bool` isn't
//! host-shareable, so a uniform or storage binding whose type involves one
//! is declared with its *host type*: every `bool` stored as a `u32` and
//! every bool vector as a `u32` vector (0 or 1 per bool, which is how both
//! CPU runtimes already lay a bool out), and every struct holding one as a
//! generated copy of the struct with those fields.
//!
//! A uniform also has stricter layout rules (`BufferLayout::Uniform`, which
//! the CPU runtimes upload in): every array's stride is a multiple of 16
//! bytes, and every struct, enum, or array member of a struct starts on a
//! 16-byte boundary, with a struct or enum member taking a multiple of 16
//! bytes. A type whose plain WGSL layout breaks them is declared in a
//! uniform with a host type that follows them: an array's elements wrapped
//! in a struct padded to the stride (`@size`), a struct's such members
//! given `@align(16)` and `@size`, and an enum with a payload holding its
//! data words in `vec4u`s. A type whose plain layout follows them is
//! declared as it is.
//!
//! Only emission sees this. Shader code reads and writes such a binding
//! through places rooted at it (`TypedExp::host_encoded_place`), which
//! compile to their raw access (an index into a wrapped array reading the
//! element's `value`), converted where the value leaves or enters the
//! binding: a read of a place whose type is host-encoded is unpacked, and a
//! value assigned to one is packed. Everything inside a place (field
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
  types::{BufferLayout, ConcreteArraySize, Type, round_up_to},
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
  /// The struct wrapping each element type of a uniform's arrays whose
  /// stride needs padding, by the element type's WGSL name.
  element_wrappers: HashMap<String, String>,
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
  /// enum with a payload or a type whose plain layout breaks a uniform's
  /// rules (see the module docs).
  pub(crate) fn needs_host_encoding(&self, layout: BufferLayout) -> bool {
    let uniform = layout == BufferLayout::Uniform;
    match self {
      Type::Bool => true,
      Type::Enum(e) => {
        uniform && e.inner_flat_data_size_in_u32s().unwrap_or(0) > 0
      }
      Type::Array(_, inner) => {
        inner.with_known(|inner| inner.needs_host_encoding(layout))
          || (uniform && self.wraps_uniform_elements())
      }
      Type::Struct(s) => {
        s.fields.iter().any(|f| {
          f.field_type
            .with_known(|field_type| field_type.needs_host_encoding(layout))
        }) || (uniform && self.breaks_uniform_layout())
      }
      Type::Function(_) => self
        .closure_data_type()
        .is_some_and(|data| data.needs_host_encoding(layout)),
      _ => false,
    }
  }
  /// Whether this array type's elements, as a uniform declares them, have a
  /// stride that isn't a multiple of 16 bytes, so a uniform wraps each one
  /// in a struct padded to its stride.
  pub(crate) fn wraps_uniform_elements(&self) -> bool {
    let Type::Array(_, element) = self else {
      return false;
    };
    let (align, size) = element.unwrap_known().uniform_host_shape();
    round_up_to(align, size) % 4 != 0
  }
  /// Whether this struct's plain layout, with its fields as a uniform
  /// declares them, breaks a uniform's rules: a struct, enum, or array
  /// field off a 16-byte boundary, or a field less than a multiple of 16
  /// bytes after a struct or enum field's start.
  fn breaks_uniform_layout(&self) -> bool {
    let Type::Struct(s) = self else {
      return false;
    };
    if !self.is_wgsl_composite() {
      return false;
    }
    let mut offset = 0;
    let mut next_allowed = 0;
    for field in &s.fields {
      let field_type = field.field_type.unwrap_known();
      let (align, size) = field_type.uniform_host_shape();
      offset = round_up_to(align, offset);
      if offset < next_allowed
        || (field_type.is_wgsl_composite() && offset % 4 != 0)
      {
        return true;
      }
      next_allowed = if field_type.is_wgsl_composite()
        && !matches!(field_type, Type::Array(..))
      {
        offset + round_up_to(4, size)
      } else {
        0
      };
      offset += size;
    }
    false
  }
  /// The alignment and size, in u32s, WGSL gives the type a uniform
  /// declares this as: its host type, or else itself.
  fn uniform_host_shape(&self) -> (usize, usize) {
    let uniform = BufferLayout::Uniform;
    if !self.needs_host_encoding(uniform) {
      return (
        self.wgsl_alignment_in_u32s_in(BufferLayout::Storage),
        self.wgsl_flat_data_size_in_u32s_in(BufferLayout::Storage),
      );
    }
    match self {
      Type::Array(size, element) => {
        let element = element.unwrap_known();
        let (align, size_of_element) = element.uniform_host_shape();
        let stride = if self.wraps_uniform_elements() {
          element.wgsl_array_stride_in_u32s_in(uniform)
        } else {
          round_up_to(align, size_of_element)
        };
        let length = size.as_ref().and_then(|s| s.as_literal()).unwrap_or(0);
        (align, stride * length as usize)
      }
      Type::Struct(s) if self.is_wgsl_composite() => {
        let mut offset = 0;
        let mut struct_align = 1;
        for field in &s.fields {
          let (align, size) =
            field.field_type.unwrap_known().uniform_member_shape();
          struct_align = struct_align.max(align);
          offset = round_up_to(align, offset) + size;
        }
        (struct_align, round_up_to(struct_align, offset))
      }
      Type::Function(_) => {
        self.closure_data_type().unwrap().uniform_host_shape()
      }
      // A bool and a bool vector are `u32`s, and an enum's host type holds
      // its data in `vec4u`s, as the uniform layout does.
      _ => (
        self.wgsl_alignment_in_u32s_in(uniform),
        self.wgsl_flat_data_size_in_u32s_in(uniform),
      ),
    }
  }
  /// The alignment and size, in u32s, of a struct field of this type in a
  /// uniform's host struct: its host type's, or for a struct, enum, or array
  /// the uniform layout's, which the field's attributes give it.
  fn uniform_member_shape(&self) -> (usize, usize) {
    if self.is_wgsl_composite() {
      (
        4,
        self.wgsl_flat_data_size_in_u32s_in(BufferLayout::Uniform),
      )
    } else {
      self.uniform_host_shape()
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
        let inner =
          if layout == BufferLayout::Uniform && t.wraps_uniform_elements() {
            self.uniform_element_wrapper(&inner.unwrap_known())
          } else {
            self.host_type_name(&inner.unwrap_known(), layout)
          };
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
              let field_type = field.field_type.unwrap_known();
              let attributes = match layout {
                BufferLayout::Uniform => uniform_member_attributes(&field_type),
                BufferLayout::Storage => String::new(),
              };
              let field_type = self.host_type_name(&field_type, layout);
              format!(
                "  {attributes}{}: {field_type}",
                s.compiled_field_name(&field.name)
              )
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
        let wrapper = (layout == BufferLayout::Uniform
          && t.wraps_uniform_elements())
        .then(|| self.uniform_element_wrapper(&element));
        let raw_element = match wrapper {
          Some(_) => "h[i].value",
          None => "h[i]",
        };
        let unpack_element =
          self.host_unpack(&element, layout, raw_element.to_string());
        let pack_element = self.host_pack(&element, layout, "v[i]".to_string());
        let pack_element = match &wrapper {
          Some(wrapper) => format!("{wrapper}({pack_element})"),
          None => pack_element,
        };
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

impl NameContext {
  /// The struct a uniform's array wraps each `element` in, padded to the
  /// array's stride, defining it the first time.
  fn uniform_element_wrapper(&mut self, element: &Type) -> String {
    let name = element.monomorphized_name(self, CompilerTarget::WGSL);
    if let Some(wrapper) = self.host_encoding.element_wrappers.get(&name) {
      return wrapper.clone();
    }
    let wrapper = self
      .gensym(&format!(
        "{}_uniform_element",
        compile_word(name.clone().into())
      ))
      .to_string();
    self
      .host_encoding
      .element_wrappers
      .insert(name, wrapper.clone());
    let stride = element.wgsl_array_stride_in_u32s_in(BufferLayout::Uniform);
    let element_type = self.host_type_name(element, BufferLayout::Uniform);
    self.host_encoding.definitions.push(format!(
      "struct {wrapper} {{\n  @size({}) value: {element_type}\n}}",
      stride * 4
    ));
    wrapper
  }
}

/// The attributes giving a uniform host struct's field of type `t` the
/// uniform layout: a struct, enum, or array field is aligned to 16 bytes
/// and takes its uniform size, where its host type's own layout differs.
fn uniform_member_attributes(t: &Type) -> String {
  if !t.is_wgsl_composite() {
    return String::new();
  }
  let (align, size) = t.uniform_host_shape();
  let (uniform_align, uniform_size) = t.uniform_member_shape();
  let mut attributes = String::new();
  if align != uniform_align {
    attributes += "@align(16) ";
  }
  if size != uniform_size {
    attributes += &format!("@size({}) ", uniform_size * 4);
  }
  attributes
}

/// How many `vec4u`s a uniform holds the enum `t`'s data words in.
fn uniform_enum_data_vectors(t: &Type) -> usize {
  let Type::Enum(e) = t else { unreachable!() };
  e.inner_flat_data_size_in_u32s().unwrap().div_ceil(4)
}
