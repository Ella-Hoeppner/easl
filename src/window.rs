use std::{collections::HashMap, sync::Arc, time::Instant};

use std::sync::RwLock;

use winit::{
  application::ApplicationHandler,
  dpi::{PhysicalPosition, PhysicalSize},
  event::WindowEvent as WinitWindowEvent,
  event_loop::{ActiveEventLoop, ControlFlow, EventLoop},
  platform::run_on_demand::EventLoopExtRunOnDemand,
  window::{Window, WindowId},
};

use crate::{
  compiler::{expression::Exp, types::ExpTypeInfo},
  interpreter::{
    BufferUpload, EvalError, EvalException, EvaluationEnvironment,
    GpuBufferKind, IOManager, WindowEvent, eval,
  },
};

// winit forbids creating more than one EventLoop per process. We keep one alive
// in a thread-local and reuse it across multiple spawn-window calls via
// run_app_on_demand, which takes &mut self instead of consuming the loop.
thread_local! {
  static EVENT_LOOP: RwLock<Option<EventLoop<()>>> = RwLock::new(None);
  /// On hot-reload, the RenderState (including the wgpu surface and window) is
  /// moved here before the event loop exits.  The next run picks it up and
  /// updates only the shader and pipeline layout in-place, so the OS window and
  /// Metal layer are never torn down — no flash, no z-order change.
  static PERSISTENT_RELOAD_STATE: std::cell::RefCell<Option<RenderState>> =
    std::cell::RefCell::new(None);
  /// Fallback: last known window geometry, used when the hot-reload path is
  /// unavailable and a fresh window must be opened at the same position/size.
  static PREV_GEOMETRY: std::cell::RefCell<
    Option<(PhysicalPosition<i32>, PhysicalSize<u32>)>,
  > = std::cell::RefCell::new(None);
}

/// Drop the persistent render state (if any). Called by the CLI watch loop
/// when the reloaded program exits without opening a window.
pub fn close_persistent_window() {
  PERSISTENT_RELOAD_STATE.with(|cell| cell.borrow_mut().take());
}

/// Returns the GPU core from the persistent reload state (if any), without
/// consuming it. Used by `ensure_gpu_ready` so that pre-spawn-window compute
/// dispatches (e.g. one-shot initialisation shaders) run on the same GPU that
/// the window will later reuse, rather than on a freshly-created headless GPU
/// that gets thrown away when `setup_window` takes `PERSISTENT_RELOAD_STATE`.
pub fn persistent_gpu() -> Option<Arc<RwLock<GpuCore>>> {
  PERSISTENT_RELOAD_STATE
    .with(|c| c.borrow().as_ref().map(|s| Arc::clone(&s.gpu)))
}

pub fn run_window_loop<IO: IOManager>(
  body: Exp<ExpTypeInfo>,
  env: &mut EvaluationEnvironment<IO>,
) -> Result<bool, EvalError> {
  EVENT_LOOP.with(|cell| {
    let mut opt = cell.write().unwrap();
    let event_loop = opt.get_or_insert_with(|| EventLoop::new().unwrap());
    event_loop.set_control_flow(ControlFlow::Poll);
    let mut app = App {
      body,
      env,
      state: None,
      error: None,
      closed: false,
      last_frame_time: None,
      window_start_time: None,
      reload: false,
    };
    event_loop.run_app_on_demand(&mut app).unwrap();
    // If a reload is pending, re-show the window immediately so it stays
    // visible while the main thread recompiles.  macOS orders the window
    // out when the RunLoop stops; showing it here (before the next
    // run_app_on_demand) minimises the flicker.
    if app.reload {
      PERSISTENT_RELOAD_STATE.with(|c| {
        if let Some(state) = c.borrow().as_ref() {
          state.window.set_visible(true);
        }
      });
    }
    match app.error {
      None => Ok(app.reload),
      Some(e) => Err(e),
    }
  })
}

struct App<'a, IO: IOManager> {
  body: Exp<ExpTypeInfo>,
  env: &'a mut EvaluationEnvironment<IO>,
  state: Option<RenderState>,
  error: Option<EvalError>,
  closed: bool,
  last_frame_time: Option<Instant>,
  window_start_time: Option<Instant>,
  /// Set to true when the loop exits because a hot-reload was requested.
  reload: bool,
}

/// Metadata about a single GPU buffer binding, kept so we can recreate
/// buffers with the correct usage flags when the size changes at runtime.
#[derive(Clone)]
pub struct BindingSlot {
  pub group: u8,
  pub binding: u8,
  pub kind: GpuBufferKind,
}

/// All GPU resources needed for compute dispatch and buffer management.
/// Shared between `RenderState` and `StdoutIO` via `Arc<RwLock<GpuCore>>`.
pub struct GpuCore {
  /// The wgpu instance this device was created from. Kept alive so that
  /// a window surface can be created on the same instance later (e.g. when
  /// transitioning from a headless compute context to a windowed one).
  pub instance: wgpu::Instance,
  pub device: wgpu::Device,
  pub queue: wgpu::Queue,
  shader: wgpu::ShaderModule,
  pipeline_layout: wgpu::PipelineLayout,
  pub compute_pipelines: HashMap<String, wgpu::ComputePipeline>,
  /// Cached render pipelines. Keyed by (vert_entry, frag_entry, additive, format)
  /// so the same cache covers both surface-format and offscreen-format pipelines.
  render_pipelines: HashMap<(String, String, bool, wgpu::TextureFormat), wgpu::RenderPipeline>,
  pub binding_slots: Vec<BindingSlot>,
  pub binding_buffers: HashMap<(u8, u8), wgpu::Buffer>,
  /// Tracks the byte-length of each buffer so we can detect when a dynamic
  /// array is resized and the buffer needs to be recreated.
  pub binding_buffer_sizes: HashMap<(u8, u8), u64>,
  /// Kept alive so `rebuild_bind_groups` can recreate bind groups without
  /// recreating the layouts (which would invalidate the pipeline layout).
  pub bind_group_layouts: Vec<wgpu::BindGroupLayout>,
  pub bind_groups: Vec<wgpu::BindGroup>,
  /// Current window dimensions in pixels.
  pub window_size: (u32, u32),
  /// Time in seconds since the window was opened, updated at the start of each frame.
  pub window_time: f32,
  /// Time in seconds between the previous frame and the current frame.
  pub window_delta_time: f32,
  /// The window surface, if a window is open. Set by `RenderState::new` /
  /// `from_existing_gpu`. Used by `execute_render_batch` to render directly to
  /// the real surface instead of an offscreen texture.
  pub surface: Option<wgpu::Surface<'static>>,
  /// Surface configuration (format, size, present mode, …). Present iff `surface` is Some.
  pub surface_config: Option<wgpu::SurfaceConfiguration>,
  /// A surface texture acquired mid-frame by `execute_render_batch`. At
  /// end-of-frame `RenderState::render` calls `present()` on this rather than
  /// re-rendering.
  pub pending_present: Option<wgpu::SurfaceTexture>,
}

impl GpuCore {
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

  /// Hot-reload: swap the shader module and rebuild everything that depends on
  /// it (compute pipelines, bind group layouts, bind groups, pipeline layout).
  /// Buffers whose size is unchanged are reused so GPU-written data survives.
  pub fn update_for_reload(
    &mut self,
    wgsl: &str,
    binding_infos: &[((u8, u8), GpuBufferKind, u64)],
  ) {
    // 1. New shader module.
    self.shader =
      self
        .device
        .create_shader_module(wgpu::ShaderModuleDescriptor {
          label: Some("easl shader"),
          source: wgpu::ShaderSource::Wgsl(wgsl.into()),
        });

    // 2. Clear compute and render pipelines (they reference the old shader module).
    self.compute_pipelines.clear();
    self.render_pipelines.clear();

    // 3. Update binding slots.
    self.binding_slots = binding_infos
      .iter()
      .map(|&((group, binding), kind, _)| BindingSlot {
        group,
        binding,
        kind,
      })
      .collect();

    // 4. Rebuild buffers.  Reuse existing buffers whose size didn't change so
    // that GPU-written storage data (e.g. compute results) survives the reload.
    let mut new_buffers: HashMap<(u8, u8), wgpu::Buffer> = HashMap::new();
    let mut new_sizes: HashMap<(u8, u8), u64> = HashMap::new();
    for &((group, binding), kind, size) in binding_infos {
      let key = (group, binding);
      let alloc_size = size.max(16);
      let old_size = self.binding_buffer_sizes.get(&key).copied().unwrap_or(0);
      if old_size == alloc_size {
        if let Some(buf) = self.binding_buffers.remove(&key) {
          new_buffers.insert(key, buf);
          new_sizes.insert(key, alloc_size);
          continue;
        }
      }
      new_buffers.insert(
        key,
        self.device.create_buffer(&wgpu::BufferDescriptor {
          label: Some(&format!("binding g{group}b{binding}")),
          size: alloc_size,
          usage: gpu_buffer_usage(kind),
          mapped_at_creation: false,
        }),
      );
      new_sizes.insert(key, alloc_size);
    }
    self.binding_buffers = new_buffers;
    self.binding_buffer_sizes = new_sizes;

    // 5. Rebuild bind group layouts.
    let max_group = binding_infos
      .iter()
      .map(|((g, _), _, _)| *g as usize)
      .max()
      .map(|g| g + 1)
      .unwrap_or(0);
    self.bind_group_layouts = (0..max_group)
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
        self
          .device
          .create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some(&format!("bind group layout {group_idx}")),
            entries: &entries,
          })
      })
      .collect();

    // 6. Rebuild bind groups.
    self.rebuild_bind_groups();

    // 7. Rebuild pipeline layout (references the new bind group layouts).
    let refs: Vec<&wgpu::BindGroupLayout> =
      self.bind_group_layouts.iter().collect();
    self.pipeline_layout =
      self
        .device
        .create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
          label: None,
          bind_group_layouts: &refs,
          immediate_size: 0,
        });
  }

  pub fn get_or_create_compute_pipeline(&mut self, entry: &str) {
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

  /// Uploads binding data to the GPU. If a buffer's size has changed (e.g.
  /// a dynamic array was reassigned), the buffer is recreated and all bind
  /// groups are rebuilt.
  ///
  /// `BufferUpload::Clear` buffers are zeroed via `encoder.clear_buffer` (a
  /// fast GPU-side zero-fill with no CPU allocation) rather than copying a
  /// zeroed slice from the CPU.
  pub fn upload_bindings(&mut self, data: &[((u8, u8), BufferUpload)]) {
    let mut needs_rebuild = false;

    // First pass: recreate any buffers whose size changed.
    for ((group, binding), upload) in data {
      let key = (*group, *binding);
      let incoming_size = match upload {
        BufferUpload::Data(bytes) => bytes.len() as u64,
        BufferUpload::Clear { byte_count } => *byte_count,
      };
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
    }

    if needs_rebuild {
      self.rebuild_bind_groups();
    }

    // Second pass: write data or issue GPU clears.
    let mut encoder: Option<wgpu::CommandEncoder> = None;
    for ((group, binding), upload) in data {
      let key = (*group, *binding);
      match upload {
        BufferUpload::Data(bytes) => {
          if let Some(buffer) = self.binding_buffers.get(&key) {
            // eprintln!(
            //   "[GPU-XFER] CPU→GPU upload: g{}b{}, {} bytes",
            //   group,
            //   binding,
            //   bytes.len()
            // );
            self.queue.write_buffer(buffer, 0, bytes);
          }
        }
        BufferUpload::Clear { byte_count } => {
          if let Some(buffer) = self.binding_buffers.get(&key) {
            // eprintln!(
            //   "[GPU-XFER] CPU→GPU clear: g{}b{}, {} bytes (GPU-side zero-fill)",
            //   group,
            //   binding,
            //   byte_count
            // );
            let enc = encoder.get_or_insert_with(|| {
              self.device.create_command_encoder(
                &wgpu::CommandEncoderDescriptor {
                  label: Some("clear encoder"),
                },
              )
            });
            enc.clear_buffer(buffer, 0, None);
          }
        }
      }
    }
    if let Some(enc) = encoder {
      self.queue.submit(std::iter::once(enc.finish()));
    }
  }

  /// Immediately executes a compute shader and blocks until GPU completes.
  /// Uploads `pre_upload` buffers first, then dispatches and polls.
  pub fn execute_compute(
    &mut self,
    entry: &str,
    workgroup_count: (u32, u32, u32),
    pre_upload: Vec<((u8, u8), BufferUpload)>,
  ) {
    let entry = entry.replace('-', "_");
    self.upload_bindings(&pre_upload);
    self.get_or_create_compute_pipeline(&entry);

    let mut encoder =
      self
        .device
        .create_command_encoder(&wgpu::CommandEncoderDescriptor {
          label: Some("compute encoder"),
        });
    {
      let mut compute_pass =
        encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
          label: Some("compute pass"),
          timestamp_writes: None,
        });
      for (group_idx, bind_group) in self.bind_groups.iter().enumerate() {
        compute_pass.set_bind_group(group_idx as u32, bind_group, &[]);
      }
      compute_pass.set_pipeline(&self.compute_pipelines[&entry]);
      let (x, y, z) = workgroup_count;
      compute_pass.dispatch_workgroups(x, y, z);
    }
    self.queue.submit(std::iter::once(encoder.finish()));
    self
      .device
      .poll(wgpu::PollType::wait_indefinitely())
      .unwrap();
  }

  /// Batches multiple compute dispatches into one encoder, one submit, one
  /// poll. Much cheaper than calling `execute_compute` N times (which creates
  /// N encoders, N submits, and N blocking polls).
  pub fn execute_compute_batch(
    &mut self,
    calls: Vec<(String, (u32, u32, u32), Vec<((u8, u8), BufferUpload)>)>,
  ) {
    if calls.is_empty() {
      return;
    }
    let all_uploads: Vec<_> =
      calls.iter().flat_map(|(_, _, u)| u.iter().cloned()).collect();
    self.upload_bindings(&all_uploads);

    let mut encoder =
      self
        .device
        .create_command_encoder(&wgpu::CommandEncoderDescriptor {
          label: Some("compute encoder"),
        });
    for (entry, (x, y, z), _) in &calls {
      let entry = entry.replace('-', "_");
      self.get_or_create_compute_pipeline(&entry);
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
    self.queue.submit(std::iter::once(encoder.finish()));
    self
      .device
      .poll(wgpu::PollType::wait_indefinitely())
      .unwrap();
  }

  /// Texture format used for the offscreen render target (headless fallback).
  const OFFSCREEN_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Rgba8Unorm;

  /// Creates and caches a render pipeline for the given format (if not already cached).
  pub fn get_or_create_render_pipeline(
    &mut self,
    vert_entry: &str,
    frag_entry: &str,
    additive: bool,
    format: wgpu::TextureFormat,
  ) {
    let key = (vert_entry.to_string(), frag_entry.to_string(), additive, format);
    if self.render_pipelines.contains_key(&key) {
      return;
    }
    let blend = if additive {
      wgpu::BlendState {
        color: wgpu::BlendComponent {
          src_factor: wgpu::BlendFactor::One,
          dst_factor: wgpu::BlendFactor::One,
          operation: wgpu::BlendOperation::Add,
        },
        alpha: wgpu::BlendComponent {
          src_factor: wgpu::BlendFactor::One,
          dst_factor: wgpu::BlendFactor::One,
          operation: wgpu::BlendOperation::Add,
        },
      }
    } else {
      wgpu::BlendState::REPLACE
    };
    let pipeline =
      self
        .device
        .create_render_pipeline(&wgpu::RenderPipelineDescriptor {
          label: Some("easl render pipeline"),
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
              format,
              blend: Some(blend),
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
    self.render_pipelines.insert(key, pipeline);
  }

  /// Executes a batch of render shader calls, blocking until the GPU finishes.
  /// Used by `flush_queued_compute` so that CPU reads after
  /// `dispatch-render-shaders` see the shader's storage writes.
  ///
  /// If `self.surface` is present (a window is open), renders to the real
  /// surface texture and stores it in `self.pending_present` so that
  /// `RenderState::render` can just call `present()` at end-of-frame without
  /// re-running the shaders. Falls back to a 1×1 offscreen texture otherwise
  /// (headless / test mode).
  pub fn execute_render_batch(
    &mut self,
    calls: Vec<(String, String, u32, Vec<((u8, u8), BufferUpload)>, bool)>,
  ) {
    if calls.is_empty() {
      return;
    }
    let all_uploads: Vec<_> = calls
      .iter()
      .flat_map(|(_, _, _, u, _)| u.iter().cloned())
      .collect();
    self.upload_bindings(&all_uploads);

    // Try to acquire the real surface texture; fall back to offscreen if
    // unavailable or if no surface exists (headless mode).
    let surface_texture: Option<wgpu::SurfaceTexture> =
      self.surface.as_ref().and_then(|s| match s.get_current_texture() {
        Ok(t) => Some(t),
        Err(_) => None,
      });

    let format = if surface_texture.is_some() {
      self.surface_config.as_ref().unwrap().format
    } else {
      Self::OFFSCREEN_FORMAT
    };

    // Pre-create all pipelines before the render pass borrow begins.
    for (vert, frag, _, _, additive) in &calls {
      let vert = vert.replace('-', "_");
      let frag = frag.replace('-', "_");
      self.get_or_create_render_pipeline(&vert, &frag, *additive, format);
    }

    // Create the view: real surface or offscreen 1×1.
    // `offscreen_texture` is declared here so it lives long enough.
    let offscreen_texture;
    let view = if let Some(st) = &surface_texture {
      st.texture
        .create_view(&wgpu::TextureViewDescriptor::default())
    } else {
      offscreen_texture = self.device.create_texture(&wgpu::TextureDescriptor {
        label: Some("offscreen render target"),
        size: wgpu::Extent3d {
          width: 1,
          height: 1,
          depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: Self::OFFSCREEN_FORMAT,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
        view_formats: &[],
      });
      offscreen_texture.create_view(&wgpu::TextureViewDescriptor::default())
    };

    let mut encoder =
      self
        .device
        .create_command_encoder(&wgpu::CommandEncoderDescriptor {
          label: Some("render encoder"),
        });
    {
      let mut render_pass =
        encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
          label: Some("flush render pass"),
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
      for (vert, frag, vert_count, _, additive) in &calls {
        let vert = vert.replace('-', "_");
        let frag = frag.replace('-', "_");
        let key = (vert, frag, *additive, format);
        for (group_idx, bind_group) in self.bind_groups.iter().enumerate() {
          render_pass.set_bind_group(group_idx as u32, bind_group, &[]);
        }
        render_pass.set_pipeline(&self.render_pipelines[&key]);
        render_pass.draw(0..*vert_count, 0..1);
      }
    }
    self.queue.submit(std::iter::once(encoder.finish()));
    self
      .device
      .poll(wgpu::PollType::wait_indefinitely())
      .unwrap();

    // If we rendered to the real surface, save the texture so end-of-frame
    // can just call present() rather than re-rendering.
    self.pending_present = surface_texture;
  }

  /// Reads a GPU buffer back to CPU, blocking until done. Returns raw bytes.
  pub fn read_buffer(&self, group: u8, binding: u8, size: u64) -> Vec<u8> {
    // eprintln!(
    //   "[GPU-XFER] GPU→CPU readback: g{}b{}, {} bytes (BLOCKING)",
    //   group, binding, size
    // );
    let source = &self.binding_buffers[&(group, binding)];
    let staging = self.device.create_buffer(&wgpu::BufferDescriptor {
      label: Some("staging readback buffer"),
      size,
      usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
      mapped_at_creation: false,
    });

    let mut encoder =
      self
        .device
        .create_command_encoder(&wgpu::CommandEncoderDescriptor {
          label: Some("readback encoder"),
        });
    encoder.copy_buffer_to_buffer(source, 0, &staging, 0, size);
    self.queue.submit(std::iter::once(encoder.finish()));

    let (sender, receiver) = std::sync::mpsc::channel();
    staging
      .slice(..)
      .map_async(wgpu::MapMode::Read, move |result| {
        sender.send(result).unwrap();
      });
    self
      .device
      .poll(wgpu::PollType::wait_indefinitely())
      .unwrap();
    receiver.recv().unwrap().unwrap();

    let data = staging.slice(..).get_mapped_range();
    let bytes = data.to_vec();
    drop(data);
    staging.unmap();
    bytes
  }
}

/// Creates a headless wgpu GPU core without a window or surface, suitable for
/// running compute shaders outside of a windowed context (e.g. in tests).
pub(crate) fn create_headless_gpu_core(
  wgsl: &str,
  binding_infos: &[((u8, u8), GpuBufferKind, u64)],
) -> Arc<RwLock<GpuCore>> {
  pollster::block_on(async {
    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
      backends: wgpu::Backends::all(),
      ..Default::default()
    });

    let adapter = instance
      .request_adapter(&wgpu::RequestAdapterOptions {
        power_preference: wgpu::PowerPreference::default(),
        compatible_surface: None,
        force_fallback_adapter: false,
      })
      .await
      .expect("No wgpu adapter found for headless GPU core");

    let (device, queue) = adapter
      .request_device(&wgpu::DeviceDescriptor {
        label: None,
        required_features: wgpu::Features::VERTEX_WRITABLE_STORAGE,
        required_limits: adapter.limits(),
        memory_hints: wgpu::MemoryHints::default(),
        ..Default::default()
      })
      .await
      .expect("Failed to create headless wgpu device");

    let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
      label: Some("easl shader"),
      source: wgpu::ShaderSource::Wgsl(wgsl.into()),
    });

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

    Arc::new(RwLock::new(GpuCore {
      instance,
      device,
      queue,
      shader,
      pipeline_layout,
      compute_pipelines: HashMap::new(),
      render_pipelines: HashMap::new(),
      binding_slots,
      binding_buffers,
      binding_buffer_sizes,
      bind_group_layouts,
      bind_groups,
      window_size: (1, 1),
      window_time: 0.0,
      window_delta_time: 0.0,
      surface: None,
      surface_config: None,
      pending_present: None,
    }))
  })
}

struct RenderState {
  window: Arc<Window>,
  pub gpu: Arc<RwLock<GpuCore>>,
}

impl<'a, IO: IOManager> App<'a, IO> {
  /// Creates or reuses a window and builds the initial `RenderState`.
  ///
  /// On first run this always creates a fresh window. On hot-reload, the same
  /// `Arc<Window>` is taken from `PERSISTENT_WINDOW` so the OS window stays
  /// visible without flashing or focus changes.
  fn setup_window(&mut self, event_loop: &ActiveEventLoop) {
    // Hot-reload path: reuse the existing render state (window + surface +
    // device) from the previous run, updating only the shader and pipelines.
    // This keeps the Metal layer alive so the window never flashes.
    if let Some(mut state) =
      PERSISTENT_RELOAD_STATE.with(|cell| cell.borrow_mut().take())
    {
      // Only update the GPU's shader/layouts if ensure_gpu_ready hasn't already
      // done so (which happens when dispatch-compute-shader is called before
      // spawn-window). Calling update_for_reload a second time would recreate
      // unsized-array buffers (type_size=0 → alloc_size=16) regardless of their
      // actual size, wiping any data the pre-spawn-window compute just wrote.
      if self.env.io.get_gpu().is_none() {
        let wgsl = self.env.wgsl().to_string();
        let binding_infos = self.env.binding_infos();
        state
          .gpu
          .write()
          .unwrap()
          .update_for_reload(&wgsl, &binding_infos);
      }
      // Reconfigure the surface in case it became outdated during the gap
      // between event loop runs (e.g. the window was resized or the display
      // changed while the event loop was not active).
      // Render pipelines were already cleared by update_for_reload above.
      {
        let gpu = state.gpu.read().unwrap();
        if let (Some(surface), Some(config)) = (&gpu.surface, &gpu.surface_config) {
          surface.configure(&gpu.device, config);
        }
      }
      // Re-show the window: macOS may have ordered it out during the
      // resign-active → become-active lifecycle that happens while the main
      // thread was busy recompiling between run_app_on_demand calls.
      state.window.set_visible(true);
      self.env.io.set_gpu(Arc::clone(&state.gpu));
      self.state = Some(state);
      return;
    }

    // Fresh-start path: create a new window and full GPU state.
    // If we have a previous geometry, open the window there instead of the
    // OS default position so reloads don't move the window.
    let attrs = PREV_GEOMETRY
      .with(|c| *c.borrow())
      .map(|(pos, size)| {
        Window::default_attributes()
          .with_title("easl")
          .with_position(pos)
          .with_inner_size(size)
          .with_active(false)
      })
      .unwrap_or_else(|| Window::default_attributes().with_title("easl"));
    let window = Arc::new(event_loop.create_window(attrs).unwrap());

    // If a headless GPU already exists (from a prior dispatch-compute-shader),
    // reuse it by adding a render surface on top. This avoids the expensive
    // GPU→CPU→GPU round-trip that would otherwise be needed to preserve
    // GPU-written buffer contents (e.g. large compute output arrays).
    let state = if let Some(existing_gpu) = self.env.io.get_gpu() {
      pollster::block_on(RenderState::from_existing_gpu(window, existing_gpu))
        .unwrap()
    } else {
      let wgsl = self.env.wgsl().to_string();
      let binding_infos = self.env.binding_infos();
      pollster::block_on(RenderState::new(window, &wgsl, &binding_infos))
        .unwrap()
    };
    // Give the interpreter's IO manager direct access to GPU resources so
    // compute dispatches can execute synchronously within eval().
    self.env.io.set_gpu(Arc::clone(&state.gpu));
    self.state = Some(state);
  }
}

impl<'a, IO: IOManager> ApplicationHandler for App<'a, IO> {
  fn resumed(&mut self, event_loop: &ActiveEventLoop) {
    // Only run setup if we haven't already (about_to_wait may have beaten us
    // to it on platforms where resumed fires late or not at all after reload).
    if self.state.is_none() {
      self.setup_window(event_loop);
    }
  }

  /// Fallback for platforms (macOS) where `resumed` is not re-fired on the
  /// second `run_app_on_demand` call because the app never went inactive.
  /// With `ControlFlow::Poll` this fires every iteration, so we guard with
  /// `self.state.is_none()` to run setup at most once.
  fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
    if self.state.is_none() && !self.closed && !self.reload {
      self.setup_window(event_loop);
      if let Some(state) = &self.state {
        state.window.request_redraw();
      }
    }
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
        if self.window_start_time.is_none() {
          self.window_start_time = Some(now);
        }
        let window_time = (now - self.window_start_time.unwrap()).as_secs_f32();
        let window_delta_time = self
          .last_frame_time
          .map_or(0.0, |last| (now - last).as_secs_f32());
        self.last_frame_time = Some(now);
        if let Some(state) = &self.state {
          let mut gpu = state.gpu.write().unwrap();
          gpu.window_time = window_time;
          gpu.window_delta_time = window_delta_time;
        }
        match eval(self.body.clone(), self.env) {
          Ok(_) => {}
          Err(EvalException::CloseWindow) => {
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
        let draw_calls = self.env.io.take_frame_draw_calls();
        if let Some(state) = &mut self.state {
          state.render(&draw_calls);
          state.window.request_redraw();
        }
        // Check for hot-reload after every successful frame.
        if self.env.io.reload_requested() {
          // Save current window geometry as a fallback for the fresh-start path.
          if let Some(state) = &self.state {
            let pos = state
              .window
              .outer_position()
              .unwrap_or(PhysicalPosition::new(100, 100));
            let size = state.window.inner_size();
            PREV_GEOMETRY.with(|c| *c.borrow_mut() = Some((pos, size)));
          }
          // Move the whole RenderState into PERSISTENT_RELOAD_STATE so that
          // the window and Metal surface stay alive while the event loop is
          // between runs. setup_window() will pick it up and update only the
          // shader and pipelines, never touching the window or surface.
          PERSISTENT_RELOAD_STATE
            .with(|cell| *cell.borrow_mut() = self.state.take());
          self.reload = true;
          event_loop.exit();
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
    // Include COPY_SRC so we can stage GPU→CPU readbacks.
    GpuBufferKind::StorageReadOnly | GpuBufferKind::StorageReadWrite => {
      wgpu::BufferUsages::STORAGE
        | wgpu::BufferUsages::COPY_DST
        | wgpu::BufferUsages::COPY_SRC
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
      wgpu::ShaderStages::FRAGMENT
        | wgpu::ShaderStages::COMPUTE
        | wgpu::ShaderStages::VERTEX
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
        // todo! right now this feature is necessary to make `delaunay.easl`,
        // but eventually this should be `Features::empty()`. The delaunay
        // example should be refactored once it's possible to have a
        // global var like `particles` be treated as storage-write from inside
        // compute shaders, but just storage from inside render shaders
        required_features: wgpu::Features::VERTEX_WRITABLE_STORAGE,
        required_limits: adapter.limits(),
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

    let gpu = Arc::new(RwLock::new(GpuCore {
      instance,
      device,
      queue,
      shader,
      pipeline_layout,
      compute_pipelines: HashMap::new(),
      render_pipelines: HashMap::new(),
      binding_slots,
      binding_buffers,
      binding_buffer_sizes,
      bind_group_layouts,
      bind_groups,
      window_size: (surface_config.width, surface_config.height),
      window_time: 0.0,
      window_delta_time: 0.0,
      surface: Some(surface),
      surface_config: Some(surface_config),
      pending_present: None,
    }));

    Ok(Self { window, gpu })
  }

  /// Builds a `RenderState` by adding a render surface to an already-running
  /// headless `GpuCore`. Reuses the existing device, buffers, and bind groups —
  /// no GPU↔CPU data transfer needed.
  async fn from_existing_gpu(
    window: Arc<Window>,
    gpu: Arc<RwLock<GpuCore>>,
  ) -> Result<Self, String> {
    let gpu_read = gpu.read().unwrap();

    let surface = gpu_read
      .instance
      .create_surface(window.clone())
      .map_err(|e| e.to_string())?;

    // Request an adapter compatible with the surface (for capability queries
    // only — we reuse the existing device for all actual GPU work).
    let adapter = gpu_read
      .instance
      .request_adapter(&wgpu::RequestAdapterOptions {
        power_preference: wgpu::PowerPreference::default(),
        compatible_surface: Some(&surface),
        force_fallback_adapter: false,
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
    surface.configure(&gpu_read.device, &surface_config);
    drop(gpu_read);

    {
      let mut gpu_write = gpu.write().unwrap();
      gpu_write.window_size = (surface_config.width, surface_config.height);
      gpu_write.surface = Some(surface);
      gpu_write.surface_config = Some(surface_config);
    }

    Ok(Self { window, gpu })
  }

  fn resize(&mut self, width: u32, height: u32) {
    if width > 0 && height > 0 {
      let mut gpu = self.gpu.write().unwrap();
      gpu.window_size = (width, height);
      if let Some(config) = &mut gpu.surface_config {
        config.width = width;
        config.height = height;
      }
      if let (Some(surface), Some(config)) = (&gpu.surface, &gpu.surface_config) {
        surface.configure(&gpu.device, config);
      }
    }
  }

  fn render(&mut self, draw_calls: &[WindowEvent]) {
    // Fast path: if flush_queued_compute already rendered to the real surface
    // mid-frame, just present that pre-rendered texture. Render events were
    // drained by flush_queued_compute so draw_calls should be empty here.
    {
      let mut gpu = self.gpu.write().unwrap();
      if let Some(pending) = gpu.pending_present.take() {
        if draw_calls.is_empty() {
          pending.present();
          return;
        }
        // New draw calls arrived after the flush (unusual). The first render's
        // storage writes are already committed; discard its visual output and
        // fall through to re-render below.
        drop(pending);
      }
    }

    if draw_calls.is_empty() {
      return;
    }

    let mut gpu = self.gpu.write().unwrap();

    let format = gpu
      .surface_config
      .as_ref()
      .map(|c| c.format)
      .unwrap_or(GpuCore::OFFSCREEN_FORMAT);

    // Pre-pass: ensure all pipeline objects exist. No buffer uploads here —
    // uploads are done per-draw-call below so each draw sees its own values.
    for draw_call in draw_calls {
      match draw_call {
        WindowEvent::RenderShaders {
          vert,
          frag,
          additive,
          ..
        } => {
          let vert = vert.replace('-', "_");
          let frag = frag.replace('-', "_");
          gpu.get_or_create_render_pipeline(&vert, &frag, *additive, format);
        }
        WindowEvent::ComputeShader { entry, .. } => {
          let entry = entry.replace('-', "_");
          gpu.get_or_create_compute_pipeline(&entry);
        }
      }
    }

    let has_render = draw_calls
      .iter()
      .any(|c| matches!(c, WindowEvent::RenderShaders { .. }));

    let output = if has_render {
      match gpu.surface.as_ref().expect("RenderState has no surface").get_current_texture() {
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

    // Batch all compute dispatches into one encoder (multiple passes) and all
    // render draws into one encoder (one render pass, multiple draw calls).
    // This reduces queue.submit() calls from N to at most 2 per frame.
    //
    // Uploads are aggregated across all calls of each kind — if the same
    // binding appears in multiple pre_uploads, the last write wins, which is
    // correct because collect_dirty_uploads marks each binding Synced on first
    // upload so only the first dispatch in the frame will carry each binding.

    // --- Compute pass (one encoder, one submit) ---
    let compute_calls: Vec<_> = draw_calls
      .iter()
      .filter_map(|c| {
        if let WindowEvent::ComputeShader {
          entry,
          workgroup_count,
          pre_upload,
        } = c
        {
          Some((entry, workgroup_count, pre_upload))
        } else {
          None
        }
      })
      .collect();

    if !compute_calls.is_empty() {
      let all_uploads: Vec<_> = compute_calls
        .iter()
        .flat_map(|(_, _, u)| u.iter().cloned())
        .collect();
      gpu.upload_bindings(&all_uploads);

      let mut encoder =
        gpu.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
          label: Some("compute encoder"),
        });
      for (entry, (x, y, z), _) in &compute_calls {
        let entry = entry.replace('-', "_");
        let mut compute_pass =
          encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
            label: Some("compute pass"),
            timestamp_writes: None,
          });
        for (group_idx, bind_group) in gpu.bind_groups.iter().enumerate() {
          compute_pass.set_bind_group(group_idx as u32, bind_group, &[]);
        }
        compute_pass.set_pipeline(&gpu.compute_pipelines[&entry]);
        compute_pass.dispatch_workgroups(*x, *y, *z);
      }
      gpu.queue.submit(std::iter::once(encoder.finish()));
    }

    // --- Render pass (one encoder, one submit) ---
    if let Some(view) = &view {
      let render_calls: Vec<_> = draw_calls
        .iter()
        .filter_map(|c| {
          if let WindowEvent::RenderShaders {
            vert,
            frag,
            vert_count,
            pre_upload,
            additive,
          } = c
          {
            Some((vert, frag, vert_count, pre_upload, additive))
          } else {
            None
          }
        })
        .collect();

      if !render_calls.is_empty() {
        let all_uploads: Vec<_> = render_calls
          .iter()
          .flat_map(|(_, _, _, u, _)| u.iter().cloned())
          .collect();
        gpu.upload_bindings(&all_uploads);

        let mut encoder = gpu.device.create_command_encoder(
          &wgpu::CommandEncoderDescriptor {
            label: Some("render encoder"),
          },
        );
        {
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
          for (vert, frag, vert_count, _, additive) in &render_calls {
            let vert = vert.replace('-', "_");
            let frag = frag.replace('-', "_");
            let key = (vert, frag, **additive, format);
            for (group_idx, bind_group) in gpu.bind_groups.iter().enumerate() {
              render_pass.set_bind_group(group_idx as u32, bind_group, &[]);
            }
            render_pass.set_pipeline(&gpu.render_pipelines[&key]);
            render_pass.draw(0..**vert_count, 0..1);
          }
        }
        gpu.queue.submit(std::iter::once(encoder.finish()));
      }
    }

    if let Some(output) = output {
      output.present();
    }
  }
}
