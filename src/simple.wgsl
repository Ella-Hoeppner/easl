@group(0) @binding(0) var<uniform> dimensions: vec2f;
@group(0) @binding(1) var<uniform> time: f32;

struct VertexInput {
  @location(0) corner_position: vec2f,
}

struct VertexOutput {
  @builtin(position) vertex_pos: vec4f,
};

@vertex
fn vertex(in: VertexInput) -> VertexOutput {
  return VertexOutput(vec4f(in.corner_position, 0.0, 1.0));
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4f {
  let pos = in.vertex_pos.xy/dimensions;
  let osc = sin(time*9.)*0.5+0.5;
  return vec4f(pow(vec3f(pos, osc),vec3f(2.2)), 1.);
}
