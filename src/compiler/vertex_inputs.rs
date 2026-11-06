//! Vertex data: the arrays a draw passes feed its vertex shader's data
//! parameters, as vertex buffers.
//!
//! A vertex entry's data parameters are the ones that aren't builtins (a
//! struct can mix both: its builtin fields come from the GPU, the rest from
//! the array). A draw passes one array per data parameter, matched in order:
//! the per-vertex array (if any) feeds the first, then the per-instance
//! array. Each value's attributes sit at their offsets in the WGSL storage
//! layout, so the array's own GPU buffer is the vertex buffer as it is:
//! nothing is converted or copied.
//!
//! A parameter WGSL can take as an entry input (a number, a vector, or a
//! struct of those) is emitted as written, with its `@{location}`s, written
//! or inferred, so hosts drawing the WGSL with their own vertex buffers see
//! exactly what the program says. Any other type (a matrix, or a struct or
//! fixed-size array of other types) is flattened: the entry takes its
//! attributes as inputs and rebuilds the value at the top of its body
//! (`vertex_input_expression`). A flattened value's attributes take the
//! `@{location}`s written on it or its fields, counting up across a
//! composite, and otherwise the lowest free locations.

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::sync::{Arc, RwLock};

use crate::Never;
use crate::compiler::{
  entry::EntryPoint,
  error::{CompileError, CompileErrorKind, ErrorLog},
  expression::{ExpKind, TypedExp},
  functions::{FunctionImplementationKind, TopLevelFunction, extract_vec_size},
  modules::display_name,
  program::{CompilerTarget, NameContext, Program},
  structs::AbstractStruct,
  types::{Type, wgsl_matrix_column_stride_in_u32s},
  util::compile_word,
  vars::{TopLevelVariableKind, VariableAddressSpace},
};

/// WebGPU's default limit on a pipeline's vertex attributes, and on their
/// locations. Its limit on a vertex buffer's stride (2048 bytes) needs no
/// check of its own: a layout is its attributes, each at most 16 bytes, and
/// padding before each smaller than that.
pub const MAX_VERTEX_ATTRIBUTES: usize = 16;

/// The scalar type of a vertex attribute's components.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum VertexScalar {
  F32,
  U32,
  I32,
}

/// One vertex attribute: a scalar or vector at `offset` bytes into each
/// element, read at the shader input `location`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VertexAttribute {
  pub scalar: VertexScalar,
  /// 1 to 4.
  pub components: u8,
  pub offset: u32,
  pub location: u32,
}

/// How one data parameter's values sit in its array's buffer.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VertexInputLayout {
  /// Bytes from one element to the next.
  pub stride: u32,
  pub attributes: Vec<VertexAttribute>,
}

/// A vertex attribute of a type, before locations are resolved.
#[derive(Debug, Clone, Copy)]
struct VertexLeaf {
  scalar: VertexScalar,
  components: u8,
  /// In u32s.
  offset: usize,
  /// The `@{location}` written on it, if any.
  location: Option<usize>,
}

/// A location inferred for a struct field of an entry's input or output.
pub struct InferredFieldLocation {
  pub definition: Arc<AbstractStruct>,
  pub field_name: Arc<str>,
  pub location: usize,
  /// The vertex entry taking the struct as an input, when one is.
  pub vertex_entry: Option<(Arc<str>, Arc<RwLock<TopLevelFunction>>)>,
}

/// A struct definition holds one location per field, but vertex entries can
/// infer different ones for a struct they share: one might take it first
/// and another after a vector. Each such struct's first vertex entry by name
/// (or any other entry using it) keeps it as written; the entries that
/// disagree flatten it, so the attributes take locations of their own.
/// Returns the locations left to write on the definitions.
pub fn flatten_conflicting_vertex_inputs(
  inferred: Vec<InferredFieldLocation>,
) -> Vec<InferredFieldLocation> {
  // (struct name, vertex entry name) -> that entry's locations
  let mut assignments: BTreeMap<
    (Arc<str>, Option<Arc<str>>),
    BTreeSet<(Arc<str>, usize)>,
  > = BTreeMap::new();
  for location in inferred.iter() {
    assignments
      .entry((
        location.definition.name.0.clone(),
        location.vertex_entry.as_ref().map(|(name, _)| name.clone()),
      ))
      .or_default()
      .insert((location.field_name.clone(), location.location));
  }
  let conflicts = |entry: &(Arc<str>, Option<Arc<str>>)| -> bool {
    let (definition, _) = entry;
    let keeper = assignments
      .iter()
      .find(|((name, _), _)| name == definition)
      .map(|(_, locations)| locations)
      .expect("every location has an assignment");
    let mine = &assignments[entry];
    let repeats_a_field = mine
      .iter()
      .map(|(field, _)| field)
      .collect::<BTreeSet<_>>()
      .len()
      != mine.len();
    mine != keeper || repeats_a_field
  };
  inferred
    .into_iter()
    .filter(|location| {
      let Some((name, implementation)) = &location.vertex_entry else {
        return true;
      };
      let flattenable = !location
        .definition
        .fields
        .iter()
        .any(|field| field.attributes.builtin().is_some());
      let key = (location.definition.name.0.clone(), Some(name.clone()));
      if !flattenable || !conflicts(&key) {
        return true;
      }
      let mut implementation = implementation.write().unwrap();
      let params: Vec<usize> = implementation
        .param_types()
        .iter()
        .enumerate()
        .filter(|(_, t)| {
          matches!(t, Type::Struct(s)
            if *s.abstract_ancestor == *location.definition)
        })
        .map(|(i, _)| i)
        .collect();
      for param in params {
        if !implementation.flattened_vertex_params.contains(&param) {
          implementation.flattened_vertex_params.push(param);
        }
      }
      false
    })
    .collect()
}

/// Gives `leaves` the consecutive locations from `first`.
fn locate_leaves(leaves: &mut [VertexLeaf], first: usize) {
  for (i, leaf) in leaves.iter_mut().enumerate() {
    leaf.location = Some(first + i);
  }
}

fn round_up(align: usize, size: usize) -> usize {
  size.div_ceil(align) * align
}

/// The scalar of a vertex attribute of type `t`.
fn vertex_scalar(t: &Type) -> Option<VertexScalar> {
  match t {
    Type::F32 => Some(VertexScalar::F32),
    Type::U32 => Some(VertexScalar::U32),
    Type::I32 => Some(VertexScalar::I32),
    _ => None,
  }
}

impl Type {
  /// The WGSL array stride of an array of this type, in u32s.
  fn wgsl_array_stride_in_u32s(&self) -> usize {
    round_up(
      self.wgsl_alignment_in_u32s(),
      self.wgsl_flat_data_size_in_u32s(),
    )
  }

  /// Pushes this type's attributes, in order, for a value `offset` u32s
  /// into an element, skipping builtin fields. `None` when part of the type
  /// can't be vertex data.
  fn push_vertex_leaves(
    &self,
    offset: usize,
    leaves: &mut Vec<VertexLeaf>,
  ) -> Option<()> {
    let mut leaf = |scalar, components, offset| {
      leaves.push(VertexLeaf {
        scalar,
        components,
        offset,
        location: None,
      })
    };
    if let Some(scalar) = vertex_scalar(self) {
      leaf(scalar, 1, offset);
      return Some(());
    }
    match self {
      Type::Struct(s) if self.is_vector() => {
        let scalar = vertex_scalar(&self.vector_element_type()?)?;
        leaf(scalar, extract_vec_size(&s.name)? as u8, offset);
        Some(())
      }
      Type::Struct(s) if let Some((cols, rows)) = self.matrix_dimensions() => {
        let scalar = vertex_scalar(&s.fields[0].field_type.unwrap_known())?;
        let column_stride = wgsl_matrix_column_stride_in_u32s(rows);
        for column in 0..cols {
          leaf(scalar, rows as u8, offset + column * column_stride);
        }
        Some(())
      }
      Type::Struct(s) => {
        let mut field_offset = 0;
        for field in s.fields.iter() {
          let field_type = field.field_type.unwrap_known();
          field_offset =
            round_up(field_type.wgsl_alignment_in_u32s(), field_offset);
          if field.attributes.builtin().is_none() {
            let first = leaves.len();
            field_type.push_vertex_leaves(offset + field_offset, leaves)?;
            if let Some((location, _)) = field.attributes.location() {
              locate_leaves(&mut leaves[first..], location);
            }
          }
          field_offset += field_type.wgsl_flat_data_size_in_u32s();
        }
        Some(())
      }
      Type::Array(size, element) => {
        let length = size.as_ref()?.as_literal()?;
        let element = element.unwrap_known();
        let stride = element.wgsl_array_stride_in_u32s();
        for i in 0..length as usize {
          element.push_vertex_leaves(offset + i * stride, leaves)?;
        }
        Some(())
      }
      _ => None,
    }
  }

  /// Whether this is a struct (not a vector or matrix) with only builtin
  /// fields: an entry input with nothing for a vertex buffer to feed.
  fn is_builtin_struct(&self) -> bool {
    match self {
      Type::Struct(s) if !self.is_vector() && !self.is_matrix() => {
        !s.fields.is_empty()
          && s.fields.iter().all(|f| f.attributes.builtin().is_some())
      }
      _ => false,
    }
  }

  /// Whether a builtin-attributed field sits anywhere in this type.
  fn holds_builtins(&self) -> bool {
    match self {
      Type::Struct(s) if !self.is_vector() && !self.is_matrix() => {
        s.fields.iter().any(|field| {
          field.attributes.builtin().is_some()
            || field.field_type.unwrap_known().holds_builtins()
        })
      }
      Type::Array(_, element) => element.unwrap_known().holds_builtins(),
      _ => false,
    }
  }

  /// Whether WGSL can take this type as an entry input as it is.
  fn is_wgsl_entry_input(&self) -> bool {
    self.is_attributable()
      || match self {
        Type::Struct(s) if !self.is_matrix() => s
          .fields
          .iter()
          .all(|f| f.field_type.unwrap_known().is_attributable()),
        _ => false,
      }
  }

  /// Whether a vertex entry takes a value of this type as attributes it
  /// rebuilds. Other types either are WGSL entry inputs already or can't be
  /// entry inputs at all.
  fn flattens_as_vertex_input(&self) -> bool {
    !self.is_wgsl_entry_input()
      && !self.holds_builtins()
      && self.push_vertex_leaves(0, &mut vec![]).is_some()
  }

  /// The WGSL expression rebuilding a value of this type from its vertex
  /// attributes, named in order by `attributes`.
  pub fn vertex_input_expression(
    &self,
    attributes: &mut impl Iterator<Item = String>,
    names: &mut NameContext,
  ) -> String {
    let constructor = |parts: Vec<String>, names: &mut NameContext| {
      format!(
        "{}({})",
        self.monomorphized_name(names, CompilerTarget::WGSL),
        parts.join(", ")
      )
    };
    if vertex_scalar(self).is_some() || self.is_vector() {
      return attributes.next().expect("a vertex attribute per leaf");
    }
    match self {
      Type::Struct(_) if let Some((cols, _)) = self.matrix_dimensions() => {
        let columns = (0..cols)
          .map(|_| attributes.next().expect("a vertex attribute per column"))
          .collect();
        constructor(columns, names)
      }
      Type::Struct(s) => {
        let fields = s
          .fields
          .iter()
          .map(|field| {
            field
              .field_type
              .unwrap_known()
              .vertex_input_expression(attributes, names)
          })
          .collect();
        constructor(fields, names)
      }
      Type::Array(size, element) => {
        let length = size
          .as_ref()
          .and_then(|size| size.as_literal())
          .expect("vertex data arrays are sized");
        let element = element.unwrap_known();
        let elements = (0..length)
          .map(|_| element.vertex_input_expression(attributes, names))
          .collect();
        constructor(elements, names)
      }
      _ => unreachable!("only flattened vertex inputs are rebuilt"),
    }
  }
}

impl TopLevelFunction {
  /// This function's parameter types.
  pub(crate) fn param_types(&self) -> Vec<Type> {
    let Type::Function(signature) = self.expression.data.unwrap_known() else {
      panic!("a top-level function without a function type")
    };
    signature
      .args
      .iter()
      .map(|(arg, _)| arg.var_type.unwrap_known())
      .collect()
  }

  /// The indices of this vertex entry's data parameters: those that
  /// aren't builtins or structs of only builtins.
  pub fn vertex_data_params(&self) -> Vec<usize> {
    self
      .param_types()
      .iter()
      .zip(self.arg_annotations.iter())
      .enumerate()
      .filter(|(_, (t, annotation))| {
        annotation.attributes.builtin().is_none() && !t.is_builtin_struct()
      })
      .map(|(i, _)| i)
      .collect()
  }

  /// Whether this vertex entry's `param`th parameter is a flattened data
  /// parameter.
  pub fn flattens_vertex_param(&self, param: usize) -> bool {
    self.flattened_vertex_params.contains(&param)
      || (self.arg_annotations[param].attributes.builtin().is_none()
        && self.param_types()[param].flattens_as_vertex_input())
  }

  /// The attributes of parameter `param`, with their written locations.
  /// `None` when it can't be vertex data.
  fn vertex_leaves(&self, param: usize) -> Option<Vec<VertexLeaf>> {
    let mut leaves = vec![];
    self.param_types()[param].push_vertex_leaves(0, &mut leaves)?;
    if let Some((location, _)) =
      self.arg_annotations[param].attributes.location()
    {
      locate_leaves(&mut leaves, location);
    }
    Some(leaves)
  }

  /// Rejects `@{location}`s written on this vertex entry's data parameters
  /// that land on the same location, which WGSL can't express. A written
  /// location counts up across a composite, so a matrix's can overlap
  /// another field's. Runs before any location is inferred.
  pub fn validate_vertex_input_locations(&self, errors: &mut ErrorLog) {
    let mut used = HashSet::new();
    for param in self.vertex_data_params() {
      for leaf in self.vertex_leaves(param).unwrap_or_default() {
        if let Some(location) = leaf.location
          && !used.insert(location)
        {
          errors.log(CompileError::new(
            CompileErrorKind::OverlappingVertexLocations(location),
            self.arg_annotations[param]
              .attributes
              .attributed_source
              .clone(),
          ));
        }
      }
    }
  }

  /// Sets `vertex_inputs`, the layouts of this vertex entry's data
  /// parameters, once every location is known: `structs` hold the ones
  /// inferred for struct fields, and flattened attributes take the lowest
  /// locations left.
  fn resolve_vertex_inputs(&mut self, structs: &[AbstractStruct]) {
    let types = self.param_types();
    let params = self.vertex_data_params();
    let mut all_leaves: Vec<Vec<VertexLeaf>> = vec![];
    for param in params.iter().copied() {
      let mut leaves = self.vertex_leaves(param).unwrap_or_default();
      if !self.flattens_vertex_param(param)
        && let Type::Struct(s) = &types[param]
        && let Some(definition) =
          structs.iter().find(|d| **d == *s.abstract_ancestor)
      {
        let inferred = definition
          .fields
          .iter()
          .filter(|field| field.attributes.builtin().is_none())
          .map(|field| field.attributes.location());
        for (leaf, location) in leaves.iter_mut().zip(inferred) {
          leaf.location = leaf.location.or(location.map(|(l, _)| l));
        }
      }
      all_leaves.push(leaves);
    }
    let mut used: HashSet<usize> = all_leaves
      .iter()
      .flatten()
      .filter_map(|leaf| leaf.location)
      .collect();
    self.vertex_inputs = params
      .iter()
      .zip(all_leaves)
      .map(|(param, leaves)| VertexInputLayout {
        stride: 4 * types[*param].wgsl_array_stride_in_u32s() as u32,
        attributes: leaves
          .into_iter()
          .map(|leaf| {
            let location = leaf.location.unwrap_or_else(|| {
              let free = (0..).find(|l| !used.contains(l)).unwrap();
              used.insert(free);
              free
            });
            VertexAttribute {
              scalar: leaf.scalar,
              components: leaf.components,
              offset: 4 * leaf.offset as u32,
              location: location as u32,
            }
          })
          .collect(),
      })
      .collect();
  }
}

impl VertexAttribute {
  /// The WGSL name of the vertex entry input this attribute is read into.
  pub fn input_name(&self, names: &mut NameContext) -> String {
    compile_word(
      names.local_name(&format!("easl-vertex-input-{}", self.location)),
    )
  }

  /// The WGSL type of this attribute's input.
  pub fn wgsl_type(&self) -> String {
    let scalar = match self.scalar {
      VertexScalar::F32 => "f32",
      VertexScalar::U32 => "u32",
      VertexScalar::I32 => "i32",
    };
    match self.components {
      1 => scalar.to_string(),
      n => format!("vec{n}<{scalar}>"),
    }
  }
}

/// The positions of a draw application's arguments after its shaders, by
/// role.
#[derive(Debug, Clone, Copy)]
pub struct DrawPositions {
  /// The index array of `dispatch-indexed-render-shaders`.
  pub indices: Option<usize>,
  /// The vertex count or vertex data array.
  pub vertices: Option<usize>,
  /// The instance count or instance data array.
  pub instances: Option<usize>,
}

impl DrawPositions {
  /// The roles of the arguments of an application of `f_name`, when it's a
  /// draw builtin. After `dispatch-indexed-render-shaders`' indices, an
  /// array is vertex data and a count is the instance count.
  pub fn of(f_name: &str, args: &[TypedExp]) -> Option<Self> {
    let present = |i: usize| (i < args.len()).then_some(i);
    match f_name {
      "dispatch-render-shaders" => Some(Self {
        indices: None,
        vertices: present(2),
        instances: present(3),
      }),
      "dispatch-indexed-render-shaders" => {
        let (vertices, instances) = match args.len() {
          3 => (None, None),
          4 if !is_draw_array(&args[3]) => (None, Some(3)),
          4 => (Some(3), None),
          _ => (Some(3), Some(4)),
        };
        Some(Self {
          indices: Some(2),
          vertices,
          instances,
        })
      }
      _ => None,
    }
  }
}

/// A draw application's arguments after its shaders, by role.
pub struct DrawArgs<'a> {
  /// The index array of `dispatch-indexed-render-shaders`.
  pub indices: Option<&'a TypedExp>,
  /// The vertex count or vertex data array.
  pub vertices: Option<&'a TypedExp>,
  /// The instance count or instance data array.
  pub instances: Option<&'a TypedExp>,
}

/// Whether `arg` is a draw's data array rather than a count.
pub fn is_draw_array(arg: &TypedExp) -> bool {
  arg.data.with_known(|t| matches!(t, Type::Array(..)))
}

impl<'a> DrawArgs<'a> {
  /// The roles of the arguments of an application of `f_name`, when it's a
  /// draw builtin (see `DrawPositions::of`).
  pub fn of(f_name: &str, args: &'a [TypedExp]) -> Option<Self> {
    let positions = DrawPositions::of(f_name, args)?;
    let arg = |position: Option<usize>| position.map(|i| &args[i]);
    Some(Self {
      indices: arg(positions.indices),
      vertices: arg(positions.vertices),
      instances: arg(positions.instances),
    })
  }

  /// The arrays feeding the vertex shader's data parameters, in order.
  pub fn data_arrays(&self) -> Vec<&'a TypedExp> {
    [self.vertices, self.instances]
      .into_iter()
      .flatten()
      .filter(|arg| is_draw_array(arg))
      .collect()
  }

  /// Every array the draw reads: its indices and data arrays.
  pub fn arrays(&self) -> Vec<&'a TypedExp> {
    self.indices.into_iter().chain(self.data_arrays()).collect()
  }
}

impl Program {
  /// Whether `name` is a global held in a GPU buffer, so a draw can read
  /// it as vertex data or indices.
  fn is_gpu_buffer_var(&self, name: &str) -> bool {
    self.top_level_vars.iter().any(|v| {
      &*v.name == name
        && matches!(
          v.kind,
          TopLevelVariableKind::Var {
            address_space: VariableAddressSpace::Uniform
              | VariableAddressSpace::StorageRead
              | VariableAddressSpace::StorageReadWrite,
            ..
          }
        )
    })
  }

  /// Checks a draw's index and data arrays: each is a GPU buffer global,
  /// the data arrays match the vertex shader's data parameters in number
  /// and type, and the fragment shader writes none of them.
  pub(crate) fn validate_draw_data(
    &self,
    exp: &TypedExp,
    args: &DrawArgs,
    vertex_name: &str,
    vertex: &TopLevelFunction,
    fragment: &TopLevelFunction,
    errors: &mut ErrorLog,
  ) {
    let mut names = vec![];
    for array in args.arrays() {
      match &array.kind {
        ExpKind::Name(name) if self.is_gpu_buffer_var(name) => {
          names.push(name.clone())
        }
        _ => errors.log(CompileError::new(
          CompileErrorKind::DrawDataNotGpuArray,
          array.source_trace.clone(),
        )),
      }
    }
    let data_arrays = args.data_arrays();
    let params = vertex.vertex_data_params();
    let leaves: Vec<VertexLeaf> = params
      .iter()
      .filter_map(|param| vertex.vertex_leaves(*param))
      .flatten()
      .collect();
    if leaves.len() > MAX_VERTEX_ATTRIBUTES {
      errors.log(CompileError::new(
        CompileErrorKind::TooManyVertexAttributes(leaves.len()),
        exp.source_trace.clone(),
      ));
    }
    if let Some(location) = leaves
      .iter()
      .filter_map(|leaf| leaf.location)
      .find(|location| *location >= MAX_VERTEX_ATTRIBUTES)
    {
      errors.log(CompileError::new(
        CompileErrorKind::VertexLocationOutOfRange(location),
        exp.source_trace.clone(),
      ));
    }
    if params.len() != data_arrays.len() {
      errors.log(CompileError::new(
        CompileErrorKind::VertexDataCountMismatch(
          display_name(vertex_name).to_string(),
          params.len(),
          data_arrays.len(),
        ),
        exp.source_trace.clone(),
      ));
    } else {
      let types = vertex.param_types();
      for (param, array) in params.into_iter().zip(data_arrays) {
        let Type::Array(_, element) = array.data.unwrap_known() else {
          unreachable!("a draw data array has an array type")
        };
        let element = element.unwrap_known();
        if element != types[param] {
          errors.log(CompileError::new(
            CompileErrorKind::VertexDataTypeMismatch(
              types[param].clone().into(),
              element.into(),
            ),
            array.source_trace.clone(),
          ));
        }
      }
    }
    let (_, writes) = fragment.effects().gpu_read_and_written_globals();
    let mut written: Vec<&Arc<str>> =
      names.iter().filter(|name| writes.contains(name)).collect();
    written.sort();
    written.dedup();
    for name in written {
      errors.log(CompileError::new(
        CompileErrorKind::DrawWritesItsData(display_name(name).to_string()),
        exp.source_trace.clone(),
      ));
    }
  }

  /// Resolves every vertex entry's vertex input layouts. Runs after
  /// `validate_entry_points`, which infers the locations they read.
  pub fn resolve_vertex_inputs(&mut self) {
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      if f.entry_point == Some(EntryPoint::Vertex)
        && let FunctionImplementationKind::Composite(implementation) =
          &f.implementation
      {
        implementation
          .write()
          .unwrap()
          .resolve_vertex_inputs(&self.typedefs.structs);
      }
    }
  }

  /// The globals draws read as vertex data or indices.
  pub(crate) fn draw_data_vars(&self) -> HashSet<Arc<str>> {
    let mut vars = HashSet::new();
    for f in self.abstract_functions_iter() {
      let f = f.read().unwrap();
      let FunctionImplementationKind::Composite(implementation) =
        &f.implementation
      else {
        continue;
      };
      implementation
        .read()
        .unwrap()
        .expression
        .walk(&mut |exp| {
          if let ExpKind::Application(applied, args) = &exp.kind
            && let ExpKind::Name(f_name) = &applied.kind
            && let Some(draw) = DrawArgs::of(f_name, args)
          {
            for array in draw.arrays() {
              if let ExpKind::Name(name) = &array.kind {
                vars.insert(name.clone());
              }
            }
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
    vars
  }
}
