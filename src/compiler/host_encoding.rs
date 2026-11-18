//! Bools in GPU bindings. WGSL can't share a `bool` between the host and
//! the GPU (it isn't host-shareable), so a uniform or storage binding whose
//! type involves one is declared with its *host type*: every `bool` stored
//! as a `u32` and every bool vector as a `u32` vector (0 or 1 per bool,
//! which is how both CPU runtimes already lay a bool out), and every struct
//! holding one as a generated copy of the struct with those fields. Enums
//! need nothing: their payloads are already `u32` words.
//!
//! Only emission sees this. Shader code reads and writes such a binding
//! through places rooted at it (`TypedExp::is_host_encoded_place`), which
//! compile to their raw access, converted where the value leaves or enters
//! the binding: a read of a place whose type involves a bool is unpacked,
//! and a value assigned to one is packed. Everything inside a place (field
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
  types::{ConcreteArraySize, Type},
  util::compile_word,
};

/// The host-encoded bindings of one WGSL emission, and the definitions
/// their conversions need.
#[derive(Debug, Clone, Default)]
pub(crate) struct HostEncoding {
  /// Each global declared with its host type, by name (storage read views
  /// included), with its declared type.
  pub(crate) globals: HashMap<Arc<str>, Type>,
  /// Each host struct's name, by its struct's WGSL name.
  host_structs: HashMap<String, String>,
  /// Each struct's or fixed array's conversion functions (unpack, pack),
  /// by its WGSL name.
  conversions: HashMap<String, (String, String)>,
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
  /// Whether a binding of this type has to be host-encoded: whether it
  /// holds a `bool` outside an enum payload (see the module docs).
  pub(crate) fn needs_host_encoding(&self) -> bool {
    match self {
      Type::Bool => true,
      Type::Array(_, inner) => inner.with_known(Type::needs_host_encoding),
      Type::Struct(s) => s
        .fields
        .iter()
        .any(|f| f.field_type.with_known(Type::needs_host_encoding)),
      Type::Function(_) => self
        .closure_data_type()
        .is_some_and(|data| data.needs_host_encoding()),
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
    globals: HashMap<Arc<str>, Type>,
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
        self.host_type_name(t)
      ));
      unpacked.push(self.host_unpack(t, format!("h.{field}")));
      packed.push(self.host_pack(t, format!("v.{field}")));
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
  /// The WGSL host type standing for `t` in a binding.
  pub(crate) fn host_type_name(&mut self, t: &Type) -> String {
    if !t.needs_host_encoding() {
      return t.monomorphized_name(self, CompilerTarget::WGSL);
    }
    if let Some(width) = bool_vector_width(t) {
      return format!("vec{width}u");
    }
    match t {
      Type::Bool => "u32".to_string(),
      Type::Array(size, inner) => {
        let inner = self.host_type_name(&inner.unwrap_known());
        match size.clone().map(|size| size.compile_type()) {
          Some(size) if !size.is_empty() => format!("array<{inner}, {size}>"),
          _ => format!("array<{inner}>"),
        }
      }
      Type::Function(_) => self.host_type_name(&t.closure_data_type().unwrap()),
      Type::Struct(s) => {
        let name = t.monomorphized_name(self, CompilerTarget::WGSL);
        if let Some(host_name) = self.host_encoding.host_structs.get(&name) {
          return host_name.clone();
        }
        let host_name = self.gensym(&format!("{name}_host")).to_string();
        self
          .host_encoding
          .host_structs
          .insert(name, host_name.clone());
        let fields = s
          .fields
          .iter()
          .map(|field| {
            let field_type =
              self.host_type_name(&field.field_type.unwrap_known());
            format!("  {}: {field_type}", s.compiled_field_name(&field.name))
          })
          .collect::<Vec<_>>()
          .join(",\n");
        self
          .host_encoding
          .definitions
          .push(format!("struct {host_name} {{\n{fields}\n}}"));
        host_name
      }
      _ => unreachable!("only bools, vectors, structs, and arrays hold bools"),
    }
  }
  /// `raw`, a value read from a binding as its host type, converted to `t`.
  pub(crate) fn host_unpack(&mut self, t: &Type, raw: String) -> String {
    if !t.needs_host_encoding() {
      return raw;
    }
    if let Some(width) = bool_vector_width(t) {
      return format!("({raw} != vec{width}u(0u))");
    }
    match t {
      Type::Bool => format!("({raw} != 0u)"),
      Type::Function(_) => {
        self.host_unpack(&t.closure_data_type().unwrap(), raw)
      }
      _ => {
        let (unpack, _) = self.host_conversions(t);
        format!("{unpack}({raw})")
      }
    }
  }
  /// `value`, of type `t`, converted to its host type to be stored in a
  /// binding.
  pub(crate) fn host_pack(&mut self, t: &Type, value: String) -> String {
    if !t.needs_host_encoding() {
      return value;
    }
    if let Some(width) = bool_vector_width(t) {
      return format!("vec{width}u({value})");
    }
    match t {
      Type::Bool => format!("u32({value})"),
      Type::Function(_) => {
        self.host_pack(&t.closure_data_type().unwrap(), value)
      }
      _ => {
        let (_, pack) = self.host_conversions(t);
        format!("{pack}({value})")
      }
    }
  }
  /// The functions converting a struct or fixed-size array between `t` and
  /// its host type, defining them the first time.
  fn host_conversions(&mut self, t: &Type) -> (String, String) {
    let name = t.monomorphized_name(self, CompilerTarget::WGSL);
    if let Some(conversions) = self.host_encoding.conversions.get(&name) {
      return conversions.clone();
    }
    let host_name = self.host_type_name(t);
    let base = compile_word(match t {
      Type::Struct(_) => name.clone().into(),
      _ => Arc::<str>::from("array"),
    });
    let unpack = self.gensym(&format!("easl_unpack_{base}")).to_string();
    let pack = self.gensym(&format!("easl_pack_{base}")).to_string();
    self
      .host_encoding
      .conversions
      .insert(name.clone(), (unpack.clone(), pack.clone()));
    let (unpacked, packed) = match t {
      Type::Struct(s) => {
        let mut unpacked = vec![];
        let mut packed = vec![];
        for field in &s.fields {
          let field_type = field.field_type.unwrap_known();
          let access = format!(".{}", s.compiled_field_name(&field.name));
          unpacked.push(self.host_unpack(&field_type, format!("h{access}")));
          packed.push(self.host_pack(&field_type, format!("v{access}")));
        }
        (
          format!("  return {name}({});", unpacked.join(", ")),
          format!("  return {host_name}({});", packed.join(", ")),
        )
      }
      Type::Array(Some(ConcreteArraySize::Literal(length)), element) => {
        let element = element.unwrap_known();
        let unpack_element = self.host_unpack(&element, "h[i]".to_string());
        let pack_element = self.host_pack(&element, "v[i]".to_string());
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
        "only structs and fixed-size arrays are converted whole: a \
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
