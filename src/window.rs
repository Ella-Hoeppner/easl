use std::{cell::RefCell, collections::HashMap, sync::Arc, time::Instant};

use winit::{
  application::ApplicationHandler,
  event::WindowEvent as WinitWindowEvent,
  event_loop::{ActiveEventLoop, ControlFlow, EventLoop},
  platform::run_on_demand::EventLoopExtRunOnDemand,
  window::{Window, WindowId},
};

use crate::{
  compiler::{expression::Exp, types::ExpTypeInfo},
  interpreter::{
    EvalError, EvalException, EvaluationEnvironment, GpuBufferKind, IOManager,
    WindowEvent, eval,
  },
};

// winit forbids creating more than one EventLoop per process. We keep one alive
// in a thread-local and reuse it across multiple spawn-window calls via
// run_app_on_demand, which takes &mut self instead of consuming the loop.
thread_local! {
  static EVENT_LOOP: RefCell<Option<EventLoop<()>>> = RefCell::new(None);
}

pub fn run_window_loop<IO: IOManager>(
  body: Exp<ExpTypeInfo>,
  env: EvaluationEnvironment<IO>,
) -> Result<EvaluationEnvironment<IO>, (EvaluationEnvironment<IO>, EvalError)> {
  EVENT_LOOP.with(|cell| {
    let mut opt = cell.borrow_mut();
    let event_loop = opt.get_or_insert_with(|| EventLoop::new().unwrap());
    event_loop.set_control_flow(ControlFlow::Poll);
    let mut app = App {
      body,
      env: Some(env),
      state: None,
      error: None,
      closed: false,
      last_frame_time: None,
    };
    event_loop.run_app_on_demand(&mut app).unwrap();
    match app.error {
      None => Ok(app.env.unwrap()),
      Some(e) => Err((app.env.unwrap(), e)),
    }
  })
}

struct App<IO: IOManager> {
  body: Exp<ExpTypeInfo>,
  env: Option<EvaluationEnvironment<IO>>,
  state: Option<RenderState>,
  error: Option<EvalError>,
  closed: bool,
  last_frame_time: Option<Instant>,
}

/// Metadata about a single GPU buffer binding, kept so we can recreate
/// buffers with the correct usage flags when the size changes at runtime.
struct BindingSlot {
  group: u8,
  binding: u8,
  kind: GpuBufferKind,
}

struct RenderState {
  window: Arc<Window>,
  device: wgpu::Device,
  queue: wgpu::Queue,
  surface: wgpu::Surface<'static>,
  surface_config: wgpu::SurfaceConfiguration,
  shader: wgpu::ShaderModule,
  pipeline_layout: wgpu::PipelineLayout,
  pipelines: HashMap<(String, String), wgpu::RenderPipeline>,
  compute_pipelines: HashMap<String, wgpu::ComputePipeline>,
  binding_slots: Vec<BindingSlot>,
  binding_buffers: HashMap<(u8, u8), wgpu::Buffer>,
  /// Tracks the byte-length of each buffer so we can detect when a dynamic
  /// array is resized and the buffer needs to be recreated.
  binding_buffer_sizes: HashMap<(u8, u8), u64>,
  /// Kept alive so `rebuild_bind_groups` can recreate bind groups without
  /// recreating the layouts (which would invalidate the pipeline layout).
  bind_group_layouts: Vec<wgpu::BindGroupLayout>,
  bind_groups: Vec<wgpu::BindGroup>,
}

impl<IO: IOManager> ApplicationHandler for App<IO> {
  fn resumed(&mut self, event_loop: &ActiveEventLoop) {
    let window = Arc::new(
      event_loop
        .create_window(Window::default_attributes().with_title("easl"))
        .unwrap(),
    );
    let env = self.env.as_ref().unwrap();
    let wgsl = env.wgsl().to_string();
    let binding_infos = env.binding_infos();
    let state =
      pollster::block_on(RenderState::new(window, &wgsl, &binding_infos))
        .unwrap();
    self.state = Some(state);
  }

  fn window_event(
    &mut self,
    event_loop: &ActiveEventLoop,
    _id: WindowId,
    event: WinitWindowEvent,
  ) {
    match event {
      WinitWindowEvent::CloseRequested => event_loop.exit(),
      WinitWindowEvent::Resized(new_size) => {
        if let Some(state) = &mut self.state {
          state.resize(new_size.width, new_size.height);
          state.window.request_redraw();
        }
      }
      WinitWindowEvent::RedrawRequested => {
        if self.closed {
          return;
        }
        let now = Instant::now();
        // if let Some(last) = self.last_frame_time {
        //   let fps = 1.0 / last.elapsed().as_secs_f64();
        //   println!("fps: {fps:.1}"); // fps logging
        // }
        self.last_frame_time = Some(now);
        let env = self.env.as_mut().unwrap();
        match eval(self.body.clone(), env) {
          Ok(_) => {}
          Err(EvalException::Error(EvalError::CloseWindow)) => {
            self.closed = true;
            event_loop.exit();
            return;
          }
          Err(e) => {
            self.closed = true;
            self.error = Some(e.into());
            event_loop.exit();
            return;
          }
        }
        let draw_calls = env.io.take_frame_draw_calls();
        if let Some(state) = &mut self.state {
          state.render(&draw_calls);
          state.window.request_redraw();
        }
      }
      _ => {}
    }
  }
}

fn gpu_buffer_usage(kind: GpuBufferKind) -> wgpu::BufferUsages {
  match kind {
    GpuBufferKind::Uniform => {
      wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST
    }
    GpuBufferKind::StorageReadOnly | GpuBufferKind::StorageReadWrite => {
      wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST
    }
  }
}

fn gpu_binding_type(kind: GpuBufferKind) -> wgpu::BufferBindingType {
  match kind {
    GpuBufferKind::Uniform => wgpu::BufferBindingType::Uniform,
    GpuBufferKind::StorageReadOnly => {
      wgpu::BufferBindingType::Storage { read_only: true }
    }
    GpuBufferKind::StorageReadWrite => {
      wgpu::BufferBindingType::Storage { read_only: false }
    }
  }
}

fn gpu_binding_visibility(kind: GpuBufferKind) -> wgpu::ShaderStages {
  match kind {
    // Read-only storage and uniforms are accessible from vertex, fragment, and compute shaders.
    GpuBufferKind::Uniform | GpuBufferKind::StorageReadOnly => {
      wgpu::ShaderStages::VERTEX
        | wgpu::ShaderStages::FRAGMENT
        | wgpu::ShaderStages::COMPUTE
    }
    // Read-write storage is not permitted in vertex shaders, but is in compute.
    GpuBufferKind::StorageReadWrite => {
      wgpu::ShaderStages::FRAGMENT | wgpu::ShaderStages::COMPUTE
    }
  }
}

impl RenderState {
  async fn new(
    window: Arc<Window>,
    wgsl: &str,
    binding_infos: &[((u8, u8), GpuBufferKind, u64)],
  ) -> Result<Self, String> {
    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
      backends: wgpu::Backends::all(),
      ..Default::default()
    });

    let surface = instance
      .create_surface(window.clone())
      .map_err(|e| e.to_string())?;

    let adapter = instance
      .request_adapter(&wgpu::RequestAdapterOptions {
        power_preference: wgpu::PowerPreference::default(),
        compatible_surface: Some(&surface),
        force_fallback_adapter: false,
      })
      .await
      .map_err(|e| e.to_string())?;

    let (device, queue) = adapter
      .request_device(&wgpu::DeviceDescriptor {
        label: None,
        required_features: wgpu::Features::empty(),
        required_limits: wgpu::Limits::default(),
        memory_hints: wgpu::MemoryHints::default(),
        ..Default::default()
      })
      .await
      .map_err(|e| e.to_string())?;

    let size = window.inner_size();
    let surface_caps = surface.get_capabilities(&adapter);
    let surface_format = surface_caps
      .formats
      .iter()
      .find(|f| f.is_srgb())
      .copied()
      .unwrap_or(surface_caps.formats[0]);

    let surface_config = wgpu::SurfaceConfiguration {
      usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
      format: surface_format,
      width: size.width.max(1),
      height: size.height.max(1),
      present_mode: wgpu::PresentMode::Fifo,
      alpha_mode: surface_caps.alpha_modes[0],
      view_formats: vec![],
      desired_maximum_frame_latency: 2,
    };
    surface.configure(&device, &surface_config);

    let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
      label: Some("easl shader"),
      source: wgpu::ShaderSource::Wgsl(wgsl.into()),
    });

    // Collect binding metadata.
    let binding_slots: Vec<BindingSlot> = binding_infos
      .iter()
      .map(|&((group, binding), kind, _)| BindingSlot {
        group,
        binding,
        kind,
      })
      .collect();

    let max_group = binding_infos
      .iter()
      .map(|((g, _), _, _)| *g as usize)
      .max()
      .map(|g| g + 1)
      .unwrap_or(0);

    // Create one buffer per binding.  Dynamic bindings (size == 0) get a
    // 16-byte placeholder; they will be resized on the first upload.
    let mut binding_buffers: HashMap<(u8, u8), wgpu::Buffer> = HashMap::new();
    let mut binding_buffer_sizes: HashMap<(u8, u8), u64> = HashMap::new();
    for &((group, binding), kind, size) in binding_infos {
      let alloc_size = size.max(16);
      let buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some(&format!("binding g{group}b{binding}")),
        size: alloc_size,
        usage: gpu_buffer_usage(kind),
        mapped_at_creation: false,
      });
      binding_buffers.insert((group, binding), buffer);
      binding_buffer_sizes.insert((group, binding), alloc_size);
    }

    // Create bind group layouts (one per group number, 0-indexed).
    let bind_group_layouts: Vec<wgpu::BindGroupLayout> = (0..max_group)
      .map(|group_idx| {
        let entries: Vec<wgpu::BindGroupLayoutEntry> = binding_infos
          .iter()
          .filter(|((g, _), _, _)| *g as usize == group_idx)
          .map(|((_, b), kind, _)| wgpu::BindGroupLayoutEntry {
            binding: *b as u32,
            visibility: gpu_binding_visibility(*kind),
            ty: wgpu::BindingType::Buffer {
              ty: gpu_binding_type(*kind),
              has_dynamic_offset: false,
              min_binding_size: None,
            },
            count: None,
          })
          .collect();
        device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
          label: Some(&format!("bind group layout {group_idx}")),
          entries: &entries,
        })
      })
      .collect();

    // Create initial bind groups.
    let bind_groups: Vec<wgpu::BindGroup> = bind_group_layouts
      .iter()
      .enumerate()
      .map(|(group_idx, layout)| {
        let entries: Vec<wgpu::BindGroupEntry> = binding_slots
          .iter()
          .filter(|s| s.group as usize == group_idx)
          .map(|s| wgpu::BindGroupEntry {
            binding: s.binding as u32,
            resource: binding_buffers[&(s.group, s.binding)]
              .as_entire_binding(),
          })
          .collect();
        device.create_bind_group(&wgpu::BindGroupDescriptor {
          label: Some(&format!("bind group {group_idx}")),
          layout,
          entries: &entries,
        })
      })
      .collect();

    let bind_group_layout_refs: Vec<&wgpu::BindGroupLayout> =
      bind_group_layouts.iter().collect();
    let pipeline_layout =
      device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: None,
        bind_group_layouts: &bind_group_layout_refs,
        immediate_size: 0,
      });

    Ok(Self {
      window,
      device,
      queue,
      surface,
      surface_config,
      shader,
      pipeline_layout,
      pipelines: HashMap::new(),
      compute_pipelines: HashMap::new(),
      binding_slots,
      binding_buffers,
      binding_buffer_sizes,
      bind_group_layouts,
      bind_groups,
    })
  }

  /// Recreates all bind groups to pick up any buffers that were just replaced.
  fn rebuild_bind_groups(&mut self) {
    self.bind_groups = self
      .bind_group_layouts
      .iter()
      .enumerate()
      .map(|(group_idx, layout)| {
        let entries: Vec<wgpu::BindGroupEntry> = self
          .binding_slots
          .iter()
          .filter(|s| s.group as usize == group_idx)
          .map(|s| wgpu::BindGroupEntry {
            binding: s.binding as u32,
            resource: self.binding_buffers[&(s.group, s.binding)]
              .as_entire_binding(),
          })
          .collect();
        self.device.create_bind_group(&wgpu::BindGroupDescriptor {
          label: Some(&format!("bind group {group_idx}")),
          layout,
          entries: &entries,
        })
      })
      .collect();
  }

  fn get_or_create_pipeline(
    &mut self,
    vert_entry: &str,
    frag_entry: &str,
  ) -> &wgpu::RenderPipeline {
    let key = (vert_entry.to_string(), frag_entry.to_string());
    if !self.pipelines.contains_key(&key) {
      let pipeline =
        self
          .device
          .create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("easl pipeline"),
            layout: Some(&self.pipeline_layout),
            vertex: wgpu::VertexState {
              module: &self.shader,
              entry_point: Some(vert_entry),
              buffers: &[],
              compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
              module: &self.shader,
              entry_point: Some(frag_entry),
              targets: &[Some(wgpu::ColorTargetState {
                format: self.surface_config.format,
                blend: Some(wgpu::BlendState::REPLACE),
                write_mask: wgpu::ColorWrites::ALL,
              })],
              compilation_options: Default::default(),
            }),
            primitive: wgpu::PrimitiveState {
              topology: wgpu::PrimitiveTopology::TriangleList,
              strip_index_format: None,
              front_face: wgpu::FrontFace::Ccw,
              cull_mode: None,
              polygon_mode: wgpu::PolygonMode::Fill,
              unclipped_depth: false,
              conservative: false,
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState {
              count: 1,
              mask: !0,
              alpha_to_coverage_enabled: false,
            },
            multiview_mask: None,
            cache: None,
          });
      self.pipelines.insert(key.clone(), pipeline);
    }
    &self.pipelines[&key]
  }

  fn get_or_create_compute_pipeline(&mut self, entry: &str) {
    if self.compute_pipelines.contains_key(entry) {
      return;
    }
    let pipeline =
      self
        .device
        .create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
          label: Some(&format!("easl compute pipeline {entry}")),
          layout: Some(&self.pipeline_layout),
          module: &self.shader,
          entry_point: Some(entry),
          compilation_options: Default::default(),
          cache: None,
        });
    self.compute_pipelines.insert(entry.to_string(), pipeline);
  }

  /// Uploads binding data to the GPU.  If a buffer's size has changed (e.g.
  /// a dynamic array was reassigned), the buffer is recreated and all bind
  /// groups for the affected groups are rebuilt.
  fn upload_bindings(&mut self, data: &[((u8, u8), Vec<u8>)]) {
    let mut needs_rebuild = false;

    for ((group, binding), bytes) in data {
      let key = (*group, *binding);
      let incoming_size = bytes.len() as u64;
      let stored_size = *self.binding_buffer_sizes.get(&key).unwrap_or(&0);

      if incoming_size != stored_size {
        let kind = self
          .binding_slots
          .iter()
          .find(|s| s.group == *group && s.binding == *binding)
          .map(|s| s.kind)
          .unwrap_or(GpuBufferKind::Uniform);
        let buffer = self.device.create_buffer(&wgpu::BufferDescriptor {
          label: Some(&format!("binding g{group}b{binding}")),
          size: incoming_size.max(16),
          usage: gpu_buffer_usage(kind),
          mapped_at_creation: false,
        });
        self.binding_buffers.insert(key, buffer);
        self.binding_buffer_sizes.insert(key, incoming_size);
        needs_rebuild = true;
      }

      if let Some(buffer) = self.binding_buffers.get(&key) {
        // println!(
        //   "cpu->gpu upload: g{group}b{binding} ({} bytes)",
        //   bytes.len()
        // );
        self.queue.write_buffer(buffer, 0, bytes);
      }
    }

    if needs_rebuild {
      self.rebuild_bind_groups();
    }
  }

  fn resize(&mut self, width: u32, height: u32) {
    if width > 0 && height > 0 {
      self.surface_config.width = width;
      self.surface_config.height = height;
      self.surface.configure(&self.device, &self.surface_config);
    }
  }

  fn render(&mut self, draw_calls: &[WindowEvent]) {
    if draw_calls.is_empty() {
      return;
    }

    // Upload any CPU-modified buffers needed by each dispatch, then
    // pre-create all pipelines before recording.
    for draw_call in draw_calls {
      match draw_call {
        WindowEvent::RenderShaders {
          vert,
          frag,
          pre_upload,
          ..
        } => {
          self.upload_bindings(pre_upload);
          let vert = vert.replace('-', "_");
          let frag = frag.replace('-', "_");
          self.get_or_create_pipeline(&vert, &frag);
        }
        WindowEvent::ComputeShader {
          entry, pre_upload, ..
        } => {
          self.upload_bindings(pre_upload);
          let entry = entry.replace('-', "_");
          self.get_or_create_compute_pipeline(&entry);
        }
      }
    }

    let has_render = draw_calls
      .iter()
      .any(|c| matches!(c, WindowEvent::RenderShaders { .. }));

    let output = if has_render {
      match self.surface.get_current_texture() {
        Ok(texture) => Some(texture),
        Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => return,
        Err(e) => {
          eprintln!("Surface error: {e:?}");
          return;
        }
      }
    } else {
      None
    };

    let view = output.as_ref().map(|o| {
      o.texture
        .create_view(&wgpu::TextureViewDescriptor::default())
    });

    let mut encoder =
      self
        .device
        .create_command_encoder(&wgpu::CommandEncoderDescriptor {
          label: Some("render encoder"),
        });

    // Compute dispatches first (in order).
    for draw_call in draw_calls {
      if let WindowEvent::ComputeShader {
        entry,
        workgroup_count: (x, y, z),
        ..
      } = draw_call
      {
        let entry = entry.replace('-', "_");
        let mut compute_pass =
          encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
            label: Some("compute pass"),
            timestamp_writes: None,
          });
        for (group_idx, bind_group) in self.bind_groups.iter().enumerate() {
          compute_pass.set_bind_group(group_idx as u32, bind_group, &[]);
        }
        compute_pass.set_pipeline(&self.compute_pipelines[&entry]);
        compute_pass.dispatch_workgroups(*x, *y, *z);
      }
    }

    // One render pass for all render calls.
    if let Some(view) = &view {
      let mut render_pass =
        encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
          label: Some("render pass"),
          color_attachments: &[Some(wgpu::RenderPassColorAttachment {
            view,
            resolve_target: None,
            depth_slice: None,
            ops: wgpu::Operations {
              load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
              store: wgpu::StoreOp::Store,
            },
          })],
          depth_stencil_attachment: None,
          occlusion_query_set: None,
          timestamp_writes: None,
          multiview_mask: None,
        });
      for (group_idx, bind_group) in self.bind_groups.iter().enumerate() {
        render_pass.set_bind_group(group_idx as u32, bind_group, &[]);
      }
      for draw_call in draw_calls {
        if let WindowEvent::RenderShaders {
          vert,
          frag,
          vert_count,
          ..
        } = draw_call
        {
          let vert = vert.replace('-', "_");
          let frag = frag.replace('-', "_");
          render_pass.set_pipeline(&self.pipelines[&(vert, frag)]);
          render_pass.draw(0..*vert_count, 0..1);
        }
      }
    }

    self.queue.submit(std::iter::once(encoder.finish()));
    if let Some(output) = output {
      output.present();
    }
  }
}
