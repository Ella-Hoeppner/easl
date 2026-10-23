#[cfg(not(feature = "window"))]
fn main() {}

#[cfg(feature = "window")]
mod window_region {
  use easl::compiler::builtins::built_in_macros;
  use easl::compiler::program::{CompilerTarget, Program, ValidationOptions};
  use easl::interpreter::{WindowEvent, derive_gpu_interface};
  use easl::parse::{EaslMultiDocument, parse_easl_without_comments};
  use easl::render_settings::RenderSettings;
  use easl::window::{GpuCore, WindowRegion, create_headless_gpu_core};

  /// A triangle covering part of the window, colored by its fragments'
  /// window positions: both the layout (the viewport) and the fragment
  /// positions have to agree with a full-window render. The fragments read
  /// the position as an argument, through `(position)` in a helper they
  /// call, and both at once.
  const SOURCE: &str = "
@vertex
(defn vert [@{builtin vertex-index} i: u32]: @{builtin position} vec4f
  (vec4f (match i
           0u (vec2f -0.9 -0.7)
           1u (vec2f 0.8 -0.2)
           _ (vec2f -0.3 0.9))
         0.
         1.))

@fragment
(defn frag [@{builtin position} pos: vec4f]: @{location 0} vec4f
  (vec4f (/ pos.x 64.) (/ pos.y 48.) 0.5 1.))

(defn position-color []: vec4f
  (vec4f (/ (.x (position)) 64.) (/ (.y (position)) 48.) 0.5 1.))

@fragment
(defn helper-frag []: @{location 0} vec4f
  (position-color))

@fragment
(defn arg-and-helper-frag [@{builtin position} pos: vec4f]: @{location 0} vec4f
  (vec4f (.x (position-color)) (/ pos.y 48.) 0.5 1.))

@cpu
(defn main []
  (spawn-window (fn []
                  (dispatch-render-shaders vert frag 3)
                  (dispatch-render-shaders vert helper-frag 3)
                  (dispatch-render-shaders vert arg-and-helper-frag 3))))
";

  const FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Rgba8Unorm;

  fn render(
    gpu: &mut GpuCore,
    events: &[WindowEvent],
    size: (u32, u32),
    region: WindowRegion,
  ) -> Vec<u8> {
    let texture = gpu.device.create_texture(&wgpu::TextureDescriptor {
      label: None,
      size: wgpu::Extent3d {
        width: size.0,
        height: size.1,
        depth_or_array_layers: 1,
      },
      mip_level_count: 1,
      sample_count: 1,
      dimension: wgpu::TextureDimension::D2,
      format: FORMAT,
      usage: wgpu::TextureUsages::RENDER_ATTACHMENT
        | wgpu::TextureUsages::COPY_SRC,
      view_formats: &[],
    });
    let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
    gpu.execute_render_batch_to_window_region(
      events, &view, FORMAT, size, region,
    );
    let bytes_per_row = (size.0 * 4).div_ceil(256) * 256;
    let buffer = gpu.device.create_buffer(&wgpu::BufferDescriptor {
      label: None,
      size: (bytes_per_row * size.1) as u64,
      usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
      mapped_at_creation: false,
    });
    let mut encoder = gpu
      .device
      .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
    encoder.copy_texture_to_buffer(
      texture.as_image_copy(),
      wgpu::TexelCopyBufferInfo {
        buffer: &buffer,
        layout: wgpu::TexelCopyBufferLayout {
          offset: 0,
          bytes_per_row: Some(bytes_per_row),
          rows_per_image: Some(size.1),
        },
      },
      texture.size(),
    );
    gpu.queue.submit(std::iter::once(encoder.finish()));
    buffer.slice(..).map_async(wgpu::MapMode::Read, |_| {});
    gpu
      .device
      .poll(wgpu::PollType::wait_indefinitely())
      .unwrap();
    let data = buffer.slice(..).get_mapped_range();
    (0..size.1)
      .flat_map(|row| {
        let start = (row * bytes_per_row) as usize;
        data[start..start + (size.0 * 4) as usize].to_vec()
      })
      .collect()
  }

  /// A region render with `frag`, of a program compiled for window
  /// regions, matches the same pixels of a full-window render.
  fn assert_region_render_matches_the_whole_window(frag: &str) {
    let (mut program, errors) = Program::from_easl_documents(
      &EaslMultiDocument::from_singular_document(
        parse_easl_without_comments(SOURCE),
        String::new(),
        SOURCE.to_string(),
      ),
      built_in_macros(),
    );
    assert!(errors.is_empty(), "{errors:?}");
    let errors = program.validate_raw_program_with(
      CompilerTarget::WGSL,
      ValidationOptions {
        window_regions: true,
      },
    );
    assert!(errors.is_empty(), "{errors:?}");
    let interface = derive_gpu_interface(&program);
    let wgsl = program
      .clone()
      .compile_to_target(CompilerTarget::WGSL)
      .unwrap();
    let entry = |name: &str| {
      interface
        .gpu_entries
        .iter()
        .position(|entry| entry.name == name)
        .unwrap() as u16
    };
    let events = [WindowEvent::RenderShaders {
      vert: entry("vert"),
      frag: entry(frag),
      vert_count: 3,
      pre_upload: vec![],
      settings: RenderSettings::default(),
      render_target: None,
    }];
    let gpu = create_headless_gpu_core(
      &wgsl,
      &interface.binding_infos(),
      &interface.gpu_entries,
    );
    let mut gpu = gpu.write().unwrap();
    let window = (64, 48);
    let whole = render(
      &mut gpu,
      &events,
      window,
      WindowRegion {
        window_size: window,
        origin: (0, 0),
      },
    );
    let (origin, size) = ((20, 9), (30, 33));
    let part = render(
      &mut gpu,
      &events,
      size,
      WindowRegion {
        window_size: window,
        origin,
      },
    );
    let mut covered = 0;
    for y in 0..size.1 {
      for x in 0..size.0 {
        let in_part = ((y * size.0 + x) * 4) as usize;
        let in_whole =
          (((y + origin.1) * window.0 + x + origin.0) * 4) as usize;
        let (expected, got) =
          (&whole[in_whole..in_whole + 4], &part[in_part..in_part + 4]);
        assert!(
          expected.iter().zip(got).all(|(a, b)| a.abs_diff(*b) <= 1),
          "{frag}: pixel ({x}, {y}) of the region: {got:?}, the window has \
           {expected:?}"
        );
        covered += (got[2] > 0) as usize;
      }
    }
    assert!(
      covered > 0 && covered < (size.0 * size.1) as usize,
      "the region should hold part of the triangle"
    );
  }

  #[test]
  fn region_renders_match_the_whole_window() {
    assert_region_render_matches_the_whole_window("frag");
  }

  /// A helper's `(position)` reads the entry's offset position, through
  /// the private global the lookup extraction assigns.
  #[test]
  fn helper_position_lookups_are_window_relative() {
    assert_region_render_matches_the_whole_window("helper_frag");
  }

  /// The same with the entry also taking the position as an argument.
  #[test]
  fn helper_and_argument_positions_agree() {
    assert_region_render_matches_the_whole_window("arg_and_helper_frag");
  }
}
