use std::fmt::Display;

use std::sync::Arc;

use crate::{
  CompilerTarget,
  compiler::{
    annotation::Annotation,
    builtins::{vec3, vec4},
    error::{
      CompileError, CompileErrorKind::*, CompileResult, ErrorLog, SourceTrace,
      err,
    },
    program::TypeDefs,
    structs::AbstractStruct,
    types::{AbstractType, Type},
  },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryPoint {
  Vertex,
  Fragment,
  Compute(usize),
  Cpu,
  Audio,
}
impl EntryPoint {
  pub fn compile(&self) -> String {
    match self {
      EntryPoint::Vertex => "@vertex\n".to_string(),
      EntryPoint::Fragment => "@fragment\n".to_string(),
      EntryPoint::Compute(size) => {
        format!("@compute\n@workgroup_size({size})\n")
      }
      EntryPoint::Cpu => "@cpu\n".to_string(),
      EntryPoint::Audio => "@audio\n".to_string(),
    }
  }
  pub fn name(&self) -> &'static str {
    match self {
      EntryPoint::Vertex => "vertex",
      EntryPoint::Fragment => "fragment",
      EntryPoint::Compute(_) => "compute",
      EntryPoint::Cpu => "cpu",
      EntryPoint::Audio => "audio",
    }
  }
  pub fn should_compile_to_target(&self, target: CompilerTarget) -> bool {
    use EntryPoint::*;
    match target {
      CompilerTarget::WGSL => {
        matches!(self, Vertex | Fragment | Compute(_))
      }
      CompilerTarget::C => {
        matches!(self, Vertex | Fragment | Compute(_) | Audio)
      }
      CompilerTarget::VM => matches!(self, Audio),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputOrOutput {
  Input,
  Output,
}

impl Display for InputOrOutput {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        InputOrOutput::Input => "input",
        InputOrOutput::Output => "output",
      }
    )
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinIOAttribute {
  VertexIndex,
  InstanceIndex,
  Position,
  FrontFacing,
  FragDepth,
  SampleIndex,
  SampleMask,
  LocalInvocationId,
  LocalInvocationIndex,
  GlobalInvocationId,
  WorkgroupId,
  NumWorkgroups,
}
use BuiltinIOAttribute::*;

impl BuiltinIOAttribute {
  pub fn all() -> Vec<Self> {
    vec![
      VertexIndex,
      InstanceIndex,
      Position,
      FrontFacing,
      FragDepth,
      SampleIndex,
      SampleMask,
      LocalInvocationId,
      LocalInvocationIndex,
      GlobalInvocationId,
      WorkgroupId,
      NumWorkgroups,
    ]
  }
  pub fn value_type(&self) -> Type {
    match self {
      VertexIndex | InstanceIndex | LocalInvocationIndex | SampleIndex
      | SampleMask => Type::U32,
      FrontFacing => Type::Bool,
      FragDepth => Type::F32,
      LocalInvocationId | GlobalInvocationId | WorkgroupId | NumWorkgroups => {
        Type::Struct(
          AbstractStruct::concretize(
            vec3()
              .fill_abstract_generics(vec![AbstractType::Type(Type::U32)])
              .into(),
            &TypeDefs::empty(),
            &vec![],
            SourceTrace::empty(),
          )
          .unwrap(),
        )
      }
      Position => Type::Struct(
        AbstractStruct::concretize(
          vec4()
            .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
            .into(),
          &TypeDefs::empty(),
          &vec![],
          SourceTrace::empty(),
        )
        .unwrap(),
      ),
    }
  }
  pub fn abstract_type(&self) -> AbstractType {
    match self {
      VertexIndex | InstanceIndex | LocalInvocationIndex | SampleIndex
      | SampleMask => AbstractType::Type(Type::U32),
      FrontFacing => AbstractType::Type(Type::Bool),
      FragDepth => AbstractType::Type(Type::F32),
      LocalInvocationId | GlobalInvocationId | WorkgroupId | NumWorkgroups => {
        AbstractType::AbstractStruct(
          vec3()
            .fill_abstract_generics(vec![AbstractType::Type(Type::U32)])
            .into(),
        )
      }
      Position => AbstractType::AbstractStruct(
        vec4()
          .fill_abstract_generics(vec![AbstractType::Type(Type::F32)])
          .into(),
      ),
    }
  }
  pub fn is_type_compatible(&self, t: &Type) -> bool {
    match self {
      VertexIndex | InstanceIndex | LocalInvocationIndex | SampleIndex
      | SampleMask => *t == Type::U32,
      FrontFacing => *t == Type::Bool,
      LocalInvocationId | GlobalInvocationId | WorkgroupId | NumWorkgroups => {
        t.is_vec3u()
      }
      Position => t.is_vec4f(),
      FragDepth => *t == Type::F32,
    }
  }
  pub fn from_name(name: &str) -> Option<Self> {
    Some(match name {
      "vertex-index" => VertexIndex,
      "instance-index" => InstanceIndex,
      "front-facing" => FrontFacing,
      "sample-index" => SampleIndex,
      "local-invocation-id" => LocalInvocationId,
      "local-invocation-index" => LocalInvocationIndex,
      "global-invocation-id" => GlobalInvocationId,
      "position" => Position,
      "frag-depth" => FragDepth,
      "sample-mask" => SampleMask,
      "workgroup-id" => WorkgroupId,
      "num-workgroups" => NumWorkgroups,
      _ => return None,
    })
  }
  pub fn name(&self) -> &'static str {
    use BuiltinIOAttribute::*;
    match self {
      VertexIndex => "vertex-index",
      InstanceIndex => "instance-index",
      FrontFacing => "front-facing",
      SampleIndex => "sample-index",
      LocalInvocationId => "local-invocation-id",
      LocalInvocationIndex => "local-invocation-index",
      GlobalInvocationId => "global-invocation-id",
      Position => "position",
      FragDepth => "frag-depth",
      SampleMask => "sample-mask",
      WorkgroupId => "workgroup-id",
      NumWorkgroups => "num-workgroups",
    }
  }
  pub fn compiled_name(&self) -> &'static str {
    match self {
      VertexIndex => "vertex_index",
      InstanceIndex => "instance_index",
      FrontFacing => "front_facing",
      SampleIndex => "sample_index",
      LocalInvocationId => "local_invocation_id",
      LocalInvocationIndex => "local_invocation_index",
      GlobalInvocationId => "global_invocation_id",
      Position => "position",
      FragDepth => "frag_depth",
      SampleMask => "sample_mask",
      WorkgroupId => "workgroup_id",
      NumWorkgroups => "num_workgroups",
    }
  }
  pub fn is_valid_input_for_stage(&self, entry: &EntryPoint) -> bool {
    use EntryPoint::*;
    match (entry, self) {
      (Fragment, Position | FrontFacing | SampleIndex | SampleMask) => true,
      (Vertex, VertexIndex | InstanceIndex) => true,
      (
        Compute(_),
        LocalInvocationId | LocalInvocationIndex | GlobalInvocationId
        | WorkgroupId | NumWorkgroups,
      ) => true,
      _ => false,
    }
  }
  pub fn is_valid_output_for_stage(&self, entry: &EntryPoint) -> bool {
    use EntryPoint::*;
    match (entry, self) {
      (Fragment, FragDepth | SampleMask) => true,
      (Vertex, Position) => true,
      _ => false,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InterpolationKind {
  Perspective,
  Linear,
  Flat,
}
use InterpolationKind::*;
impl InterpolationKind {
  fn name(&self) -> &str {
    match self {
      Perspective => "perspective",
      Linear => "linear",
      Flat => "flat",
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InterpolationSampling {
  Center,
  Centroid,
  Sample,
  First,
  Either,
}
use InterpolationSampling::*;
impl InterpolationSampling {
  fn name(&self) -> &str {
    match self {
      Center => "center",
      Centroid => "centroid",
      Sample => "sample",
      First => "first",
      Either => "either",
    }
  }
}

impl InterpolationKind {
  fn is_sampling_allowed(&self, sampling: InterpolationSampling) -> bool {
    match (self, sampling) {
      (Flat, First | Either) => true,
      (Linear | Perspective, Center | Centroid | Sample) => true,
      _ => false,
    }
  }
  fn default_sampling(&self) -> InterpolationSampling {
    match self {
      Perspective | Linear => Center,
      Flat => First,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Interpolation {
  kind: InterpolationKind,
  sampling: InterpolationSampling,
}

impl Interpolation {
  /// What integer values passed between stages always get.
  const FLAT: Self = Self {
    kind: Flat,
    sampling: First,
  };
  pub fn is_flat(&self) -> bool {
    self.kind == Flat
  }
  /// The setting as written in easl: `kind-sampling`.
  pub fn describe(&self) -> String {
    format!("{}-{}", self.kind.name(), self.sampling.name())
  }
  /// Parses an interpolation setting: a kind (`perspective`, `linear`,
  /// `flat`), optionally followed by `-` and a sampling (`center`,
  /// `centroid`, `sample` for the first two; `first`, `either` for `flat`).
  fn parse(s: &str, source_trace: SourceTrace) -> CompileResult<Self> {
    let (kind_name, sampling_name) = match s.split_once('-') {
      Some((kind_name, sampling_name)) => (kind_name, Some(sampling_name)),
      None => (s, None),
    };
    let Some(kind) = [Perspective, Linear, Flat]
      .into_iter()
      .find(|kind| kind.name() == kind_name)
    else {
      return err(InvalidInterpolation, source_trace);
    };
    let Some(sampling_name) = sampling_name else {
      return Ok(Self {
        sampling: kind.default_sampling(),
        kind,
      });
    };
    let Some(sampling) = [Center, Centroid, Sample, First, Either]
      .into_iter()
      .find(|sampling| sampling.name() == sampling_name)
    else {
      return err(InvalidInterpolation, source_trace);
    };
    if !kind.is_sampling_allowed(sampling) {
      return err(
        InvalidInterpolationSampling(
          kind_name.to_string(),
          sampling_name.to_string(),
        ),
        source_trace,
      );
    }
    Ok(Self { kind, sampling })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IOAttributeKind {
  Builtin(BuiltinIOAttribute),
  Location(usize),
  Interpolate(Interpolation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IOAttribute {
  pub kind: IOAttributeKind,
  pub source_trace: SourceTrace,
}

impl IOAttribute {
  fn try_parse_from_annotation_property(
    name: &Arc<str>,
    name_source: SourceTrace,
    value: Option<(Arc<str>, SourceTrace)>,
    _arg_or_field_name: Option<(Arc<str>, SourceTrace)>,
  ) -> CompileResult<Option<Self>> {
    match (&**name, value) {
      ("builtin", value) => {
        let Some((builtin_name, value_source)) = value else {
          return Ok(None);
        };
        if let Some(builtin) = BuiltinIOAttribute::from_name(&builtin_name) {
          Ok(Some(Self {
            kind: IOAttributeKind::Builtin(builtin),
            source_trace: name_source.insert_as_secondary(value_source),
          }))
        } else {
          return Err(CompileError {
            kind: InvalidBuiltinAttributeName(builtin_name.to_string()),
            source_trace: value_source,
          });
        }
      }
      ("location", Some((value, value_source))) => match value.parse::<usize>()
      {
        Ok(location) => Ok(Some(Self {
          kind: IOAttributeKind::Location(location),
          source_trace: name_source.insert_as_secondary(value_source),
        })),
        Err(_) => Err(CompileError {
          kind: InvalidIOLocation,
          source_trace: value_source.clone(),
        }),
      },
      ("interpolate", Some((value, value_source))) => Ok(Some(Self {
        kind: IOAttributeKind::Interpolate(Interpolation::parse(
          &*value,
          value_source.clone(),
        )?),
        source_trace: name_source.insert_as_secondary(value_source),
      })),
      _ => Ok(None),
    }
  }
  /// Whether `self` and `other` can't be on the same value: each kind can
  /// appear once, and a builtin takes no `location` or `interpolate` (those
  /// are for user-defined values passed between stages).
  fn conflicts(&self, other: &Self) -> bool {
    use IOAttributeKind::*;
    match (&self.kind, &other.kind) {
      (Builtin(_), _) | (_, Builtin(_)) => true,
      (Location(_), Location(_)) => true,
      (Interpolate(_), Interpolate(_)) => true,
      _ => false,
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IOAttributes {
  pub attributes: Vec<IOAttribute>,
  pub attributed_source: SourceTrace,
}

impl IOAttributes {
  pub fn empty(attributed_source: SourceTrace) -> Self {
    Self {
      attributes: vec![],
      attributed_source,
    }
  }
  pub fn is_empty(&self) -> bool {
    self.attributes.is_empty()
  }
  pub fn has_builtin_io_attribute(
    &self,
    attribute: BuiltinIOAttribute,
  ) -> bool {
    self.attributes.iter().any(|arg_attribute| {
      arg_attribute.kind == IOAttributeKind::Builtin(attribute)
    })
  }
  pub fn source_trace_if_not_empty(&self) -> Option<SourceTrace> {
    if !self.attributes.is_empty() {
      let mut source_trace = self.attributes[0].source_trace.clone();
      for attribute in &self.attributes[1..] {
        source_trace =
          source_trace.insert_as_secondary(attribute.source_trace.clone());
      }
      Some(source_trace)
    } else {
      None
    }
  }
  /// The WGSL attributes for a value of type `value_type`. An integer value
  /// with a location and no interpolation gets `flat` (WGSL requires it for
  /// integers passed between stages).
  pub fn compile(&self, value_type: &Type) -> String {
    let implicit_interpolation = (self.location().is_some()
      && self.interpolation().is_none()
      && value_type.is_integer_scalar_or_vector())
    .then(|| IOAttributeKind::Interpolate(Interpolation::FLAT));
    let mut s = String::new();
    for kind in self
      .attributes
      .iter()
      .map(|a| &a.kind)
      .chain(implicit_interpolation.as_ref())
    {
      let (name, value) = match kind {
        IOAttributeKind::Builtin(builtin) => (
          "builtin",
          match builtin {
            VertexIndex => "vertex_index",
            InstanceIndex => "instance_index",
            FrontFacing => "front_facing",
            SampleIndex => "sample_index",
            LocalInvocationId => "local_invocation_id",
            LocalInvocationIndex => "local_invocation_index",
            GlobalInvocationId => "global_invocation_id",
            Position => "position",
            FragDepth => "frag_depth",
            SampleMask => "sample_mask",
            WorkgroupId => "workgroup_id",
            NumWorkgroups => "num_workgroups",
          }
          .to_string(),
        ),
        IOAttributeKind::Location(location) => {
          ("location", format!("{location}"))
        }
        IOAttributeKind::Interpolate(interpolation) => (
          "interpolate",
          format!(
            "{}, {}",
            interpolation.kind.name(),
            interpolation.sampling.name()
          ),
        ),
      };
      s += &format!("@{name}({value}) ");
    }
    s
  }
  pub fn try_add_attribute(
    &mut self,
    attribute: IOAttribute,
    errors: &mut ErrorLog,
  ) {
    for a in self.attributes.iter() {
      if a.conflicts(&attribute) {
        errors.log(CompileError::new(
          ConflictingAttributes,
          attribute
            .source_trace
            .insert_as_secondary(a.source_trace.clone()),
        ));
        return;
      }
    }
    self.attributes.push(attribute)
  }
  pub fn parse_from_annotation(
    annotation: Annotation,
    arg_or_field_name: Option<(Arc<str>, SourceTrace)>,
    errors: &mut ErrorLog,
  ) -> (
    Self,
    Vec<(Arc<str>, SourceTrace, Option<(Arc<str>, SourceTrace)>)>,
  ) {
    let mut attributes = Self::empty(
      arg_or_field_name
        .as_ref()
        .map(|(_, source)| source.clone())
        .unwrap_or_else(|| annotation.source_trace.clone()),
    );
    let mut leftover_properties = vec![];
    for (name, name_source, value) in annotation.properties() {
      match IOAttribute::try_parse_from_annotation_property(
        &name,
        name_source.clone(),
        value.clone(),
        arg_or_field_name.clone(),
      ) {
        Ok(Some(attribute)) => attributes.try_add_attribute(attribute, errors),
        Ok(None) => leftover_properties.push((name, name_source, value)),
        Err(e) => errors.log(e),
      }
    }
    (attributes, leftover_properties)
  }
  pub fn builtin(&self) -> Option<(&BuiltinIOAttribute, &SourceTrace)> {
    self.attributes.iter().find_map(|attribute| {
      if let IOAttributeKind::Builtin(b) = &attribute.kind {
        Some((b, &attribute.source_trace))
      } else {
        None
      }
    })
  }
  pub fn interpolation(&self) -> Option<(&Interpolation, &SourceTrace)> {
    self.attributes.iter().find_map(|attribute| {
      if let IOAttributeKind::Interpolate(i) = &attribute.kind {
        Some((i, &attribute.source_trace))
      } else {
        None
      }
    })
  }
  pub fn location(&self) -> Option<(usize, SourceTrace)> {
    self.attributes.iter().find_map(|attribute| {
      if let IOAttributeKind::Location(l) = &attribute.kind {
        Some((*l, attribute.source_trace.clone()))
      } else {
        None
      }
    })
  }
}
