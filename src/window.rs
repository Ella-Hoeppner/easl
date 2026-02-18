use std::sync::Arc;

use winit::{
  application::ApplicationHandler,
  event::WindowEvent,
  event_loop::{ActiveEventLoop, ControlFlow, EventLoop},
  window::{Window, WindowId},
};

pub struct DispatchConfig {
  pub wgsl: String,
  pub vert_entry: String,
  pub frag_entry: String,
  pub vert_count: u32,
}

pub fn dispatch_shader(config: DispatchConfig) {
  let event_loop = EventLoop::new().unwrap();
  event_loop.set_control_flow(ControlFlow::Poll);
  let mut app = App {
    config,
    state: None,
  };
  event_loop.run_app(&mut app).unwrap();
}

struct App {
  config: DispatchConfig,
  state: Option<RenderState>,
}

struct RenderState {
  window: Arc<Window>,
  device: wgpu::Device,
  queue: wgpu::Queue,
  surface: wgpu::Surface<'static>,
  surface_config: wgpu::SurfaceConfiguration,
  pipeline: wgpu::RenderPipeline,
  vert_count: u32,
}

impl ApplicationHandler for App {
  fn resumed(&mut self, event_loop: &ActiveEventLoop) {
    let window = Arc::new(
      event_loop
        .create_window(Window::default_attributes().with_title("easl"))
        .unwrap(),
    );
    let state =
      pollster::block_on(RenderState::new(window, &self.config)).unwrap();
    self.state = Some(state);
  }

  fn window_event(
    &mut self,
    event_loop: &ActiveEventLoop,
    _id: WindowId,
    event: WindowEvent,
  ) {
    match event {
      WindowEvent::CloseRequested => event_loop.exit(),
      WindowEvent::Resized(new_size) => {
        if let Some(state) = &mut self.state {
          state.resize(new_size.width, new_size.height);
          state.window.request_redraw();
        }
      }
      WindowEvent::RedrawRequested => {
        if let Some(state) = &mut self.state {
          state.render();
          state.window.request_redraw();
        }
      }
      _ => {}
    }
  }
}

impl RenderState {
  async fn new(
    window: Arc<Window>,
    config: &DispatchConfig,
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

    // Convert easl kebab-case names to WGSL snake_case
    let vert_entry = config.vert_entry.replace('-', "_");
    let frag_entry = config.frag_entry.replace('-', "_");

    let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
      label: Some("easl shader"),
      source: wgpu::ShaderSource::Wgsl(config.wgsl.as_str().into()),
    });

    let pipeline_layout =
      device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: None,
        bind_group_layouts: &[],
        immediate_size: 0,
      });

    let pipeline =
      device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: Some("easl pipeline"),
        layout: Some(&pipeline_layout),
        vertex: wgpu::VertexState {
          module: &shader,
          entry_point: Some(&vert_entry),
          buffers: &[],
          compilation_options: Default::default(),
        },
        fragment: Some(wgpu::FragmentState {
          module: &shader,
          entry_point: Some(&frag_entry),
          targets: &[Some(wgpu::ColorTargetState {
            format: surface_config.format,
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

    Ok(Self {
      window,
      device,
      queue,
      surface,
      surface_config,
      pipeline,
      vert_count: config.vert_count,
    })
  }

  fn resize(&mut self, width: u32, height: u32) {
    if width > 0 && height > 0 {
      self.surface_config.width = width;
      self.surface_config.height = height;
      self.surface.configure(&self.device, &self.surface_config);
    }
  }

  fn render(&self) {
    let output = match self.surface.get_current_texture() {
      Ok(texture) => texture,
      Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => return,
      Err(e) => {
        eprintln!("Surface error: {e:?}");
        return;
      }
    };
    let view = output
      .texture
      .create_view(&wgpu::TextureViewDescriptor::default());
    let mut encoder =
      self
        .device
        .create_command_encoder(&wgpu::CommandEncoderDescriptor {
          label: Some("render encoder"),
        });
    {
      let mut render_pass =
        encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
          label: Some("render pass"),
          color_attachments: &[Some(wgpu::RenderPassColorAttachment {
            view: &view,
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
      render_pass.set_pipeline(&self.pipeline);
      // vert_count vertices in 1 instance; TriangleList assembles every 3
      // consecutive vertex outputs into a triangle. The vertex shader uses
      // @builtin(vertex_index) to select each vertex's position.
      render_pass.draw(0..self.vert_count, 0..1);
    }
    self.queue.submit(std::iter::once(encoder.finish()));
    output.present();
  }
}
