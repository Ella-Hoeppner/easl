//! Every location a draw's fragment shader reads must be one its vertex
//! shader outputs, with a compatible type and the same interpolation (the
//! vertex shader may output more, which goes unused). The GPU checks this
//! when it creates the draw's pipeline, so a program that breaks it is
//! rejected here, at the draw, instead.

use std::collections::BTreeMap;
use std::fmt;

use crate::Never;
use crate::compiler::{
  entry::{IOAttributes, Interpolation},
  error::{CompileError, CompileErrorKind, ErrorLog},
  expression::{ExpKind, TypedExp},
  functions::FunctionImplementationKind,
  modules::display_name,
  program::Program,
  types::{Type, TypeDescription},
  vertex_inputs::DrawArgs,
};

/// A value passed between a draw's shaders, at its location.
struct StageValue {
  /// The struct field or parameter holding it (not the return value).
  name: Option<String>,
  value_type: Type,
  interpolation: Interpolation,
}

/// How a draw's fragment shader's input at one location doesn't fit its
/// vertex shader's outputs.
#[derive(Debug, Clone, PartialEq)]
pub enum RenderInterfaceMismatch {
  /// The fragment shader reads a location the vertex shader doesn't output
  /// (into the named field or parameter).
  NotOutput {
    location: usize,
    name: Option<String>,
    input: TypeDescription,
  },
  Types {
    location: usize,
    output: TypeDescription,
    input: TypeDescription,
  },
  /// Interpolation settings, as written in easl (`perspective-center`).
  Interpolations {
    location: usize,
    output: String,
    input: String,
  },
}

/// A value at a location, for a message: `` (`start`, a `vec2f`) ``.
fn describe_value(
  name: &Option<String>,
  value_type: &TypeDescription,
) -> String {
  match name {
    Some(name) => format!("(`{name}`, a `{value_type}`)"),
    None => format!("(a `{value_type}`)"),
  }
}

impl fmt::Display for RenderInterfaceMismatch {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::NotOutput {
        location,
        name,
        input,
      } => write!(
        f,
        "the fragment shader reads location {location} {}, which the \
         vertex shader doesn't output",
        describe_value(name, input)
      ),
      Self::Types {
        location,
        output,
        input,
      } => write!(
        f,
        "location {location} is a `{output}` in the vertex shader's output \
         but a `{input}` in the fragment shader's input"
      ),
      Self::Interpolations {
        location,
        output,
        input,
      } => write!(
        f,
        "location {location} is interpolated `{output}` in the vertex \
         shader's output but `{input}` in the fragment shader's input"
      ),
    }
  }
}

impl RenderInterfaceMismatch {
  /// How each of `inputs` that doesn't fit `outputs` doesn't, by location.
  fn between(
    outputs: &BTreeMap<usize, StageValue>,
    inputs: &BTreeMap<usize, StageValue>,
  ) -> Vec<Self> {
    inputs
      .iter()
      .filter_map(|(location, input)| {
        let location = *location;
        match outputs.get(&location) {
          None => Some(Self::NotOutput {
            location,
            name: input.name.clone(),
            input: input.value_type.clone().into(),
          }),
          Some(output) if !input.value_type.compatible(&output.value_type) => {
            Some(Self::Types {
              location,
              output: output.value_type.clone().into(),
              input: input.value_type.clone().into(),
            })
          }
          Some(output) if input.interpolation != output.interpolation => {
            Some(Self::Interpolations {
              location,
              output: output.interpolation.describe(),
              input: input.interpolation.describe(),
            })
          }
          Some(_) => None,
        }
      })
      .collect()
  }
}

impl Program {
  /// Checks that every draw's vertex shader outputs what its fragment
  /// shader reads. Runs after `validate_entry_points`, which infers
  /// the locations neither shader writes.
  pub fn validate_render_interfaces(&self, errors: &mut ErrorLog) {
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
            && DrawArgs::of(f_name, args).is_some()
            && let Some(outputs) = self.stage_interface(&args[0], false)
            && let Some(inputs) = self.stage_interface(&args[1], true)
          {
            for mismatch in RenderInterfaceMismatch::between(&outputs, &inputs)
            {
              errors.log(CompileError::new(
                CompileErrorKind::IncompatibleRenderEntryPoints(mismatch),
                exp.source_trace.clone(),
              ));
            }
          }
          Ok::<bool, Never>(true)
        })
        .unwrap();
    }
  }

  /// The values `shader` (a draw's vertex or fragment shader argument)
  /// passes to or takes from the other stage, by location: its inputs if
  /// `inputs`, else its outputs. Builtins aren't passed between stages.
  fn stage_interface(
    &self,
    shader: &TypedExp,
    inputs: bool,
  ) -> Option<BTreeMap<usize, StageValue>> {
    let Type::Function(signature) = shader.data.unwrap_known() else {
      return None;
    };
    let ancestor = signature.abstract_ancestor.as_ref()?.read().unwrap();
    let FunctionImplementationKind::Composite(implementation) =
      &ancestor.implementation
    else {
      return None;
    };
    let implementation = implementation.read().unwrap();
    let Type::Function(signature) =
      implementation.expression.data.unwrap_known()
    else {
      unreachable!("a function's expression has a function type")
    };
    let values: Vec<(Option<String>, Type, &IOAttributes)> = if inputs {
      signature
        .args
        .iter()
        .zip(implementation.arg_annotations.iter())
        .zip(implementation.arg_names.iter())
        .map(|(((arg, _), annotation), (name, _))| {
          (
            Some(display_name(name).to_string()),
            arg.var_type.unwrap_known(),
            &annotation.attributes,
          )
        })
        .collect()
    } else {
      vec![(
        None,
        signature.return_type.unwrap_known(),
        &implementation.return_attributes,
      )]
    };
    let mut interface = BTreeMap::new();
    let mut add =
      |name: Option<String>, value_type: Type, attributes: &IOAttributes| {
        if let Some((location, _)) = attributes.location() {
          interface.insert(
            location,
            StageValue {
              name,
              interpolation: Interpolation::between_stages(
                attributes.interpolation().map(|(i, _)| i),
                &value_type,
              ),
              value_type,
            },
          );
        }
      };
    for (name, value_type, attributes) in values {
      if value_type.is_attributable() {
        add(name, value_type, attributes);
      } else if let Type::Struct(s) = &value_type
        // The definition holds the locations inferred for its fields (which
        // the copy in a type predates, so it's found by name).
        && let Some(definition) = self
          .typedefs
          .structs
          .iter()
          .find(|d| d.name.0 == s.abstract_ancestor.name.0)
      {
        for field in s.fields.iter() {
          if let Some(defined) =
            definition.fields.iter().find(|d| d.name == field.name)
          {
            add(
              Some(field.name.to_string()),
              field.field_type.unwrap_known(),
              &defined.attributes,
            );
          }
        }
      }
    }
    Some(interface)
  }
}
