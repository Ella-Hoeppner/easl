use std::{cell::RefCell, collections::HashMap, sync::Arc};

use winit::{
  application::ApplicationHandler,
  event::WindowEvent as WinitWindowEvent,
  event_loop::{ActiveEventLoop, ControlFlow, EventLoop},
  platform::run_on_demand::EventLoopExtRunOnDemand,
  window::{Window, WindowId},
};

use crate::{
  compiler::{expression::Exp, types::ExpTypeInfo},
  interpreter::{eval, EvalError, EvaluationEnvironment, IOManager, WindowEvent},
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
}

impl<IO: IOManager> ApplicationHandler for App<IO> {
  fn resumed(&mut self, event_loop: &ActiveEventLoop) {
    let window = Arc::new(
      event_loop
        .create_window(Window::default_attributes().with_title("easl"))
        .unwrap(),
    );
    let wgsl = self.env.as_ref().unwrap().wgsl().to_string();
    let state =
      pollster::block_on(RenderState::new(window, &wgsl)).unwrap();
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
        let env = self.env.as_mut().unwrap();
        match eval(self.body.clone(), env) {
          Ok(_) => {}
          Err(e) => {
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

impl RenderState {
  async fn new(window: Arc<Window>, wgsl: &str) -> Result<Self, String> {
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

    let pipeline_layout =
      device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: None,
        bind_group_layouts: &[],
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
    })
  }

  fn get_or_create_pipeline(
    &mut self,
    vert_entry: &str,
    frag_entry: &str,
  ) -> &wgpu::RenderPipeline {
    let key = (vert_entry.to_string(), frag_entry.to_string());
    if !self.pipelines.contains_key(&key) {
      let pipeline = self.device.create_render_pipeline(
        &wgpu::RenderPipelineDescriptor {
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
        },
      );
      self.pipelines.insert(key.clone(), pipeline);
    }
    &self.pipelines[&key]
  }

  fn resize(&mut self, width: u32, height: u32) {
    if width > 0 && height > 0 {
      self.surface_config.width = width;
      self.surface_config.height = height;
      self.surface.configure(&self.device, &self.surface_config);
    }
  }

  fn render(&mut self, draw_calls: &[WindowEvent]) {
    // Ensure all needed pipelines exist before borrowing for render pass.
    for draw_call in draw_calls {
      let vert = draw_call.vert.replace('-', "_");
      let frag = draw_call.frag.replace('-', "_");
      self.get_or_create_pipeline(&vert, &frag);
    }

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
      for draw_call in draw_calls {
        let vert = draw_call.vert.replace('-', "_");
        let frag = draw_call.frag.replace('-', "_");
        render_pass.set_pipeline(&self.pipelines[&(vert, frag)]);
        render_pass.draw(0..draw_call.vert_count, 0..1);
      }
    }
    self.queue.submit(std::iter::once(encoder.finish()));
    output.present();
  }
}
