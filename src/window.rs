#[cfg(not(target_arch = "wasm32"))]
use std::time::{Duration, Instant};
use std::{
  collections::{HashMap, HashSet},
  future::Future,
  pin::Pin,
  sync::{Arc, Mutex},
  task::{Context, Poll, Waker},
};

use std::sync::RwLock;

// The winit window loop is native-only: in a browser, the web runtime drives
// frames from `requestAnimationFrame` and renders to a canvas it provides.
#[cfg(not(target_arch = "wasm32"))]
use winit::{
  application::ApplicationHandler,
  dpi::{PhysicalPosition, PhysicalSize},
  event::{
    DeviceEvent, DeviceId, ElementState, MouseButton, StartCause,
    WindowEvent as WinitWindowEvent,
  },
  event_loop::{ActiveEventLoop, ControlFlow, EventLoop},
  keyboard::{Key, NamedKey},
  window::{CursorGrabMode, Window, WindowId},
};
// winit only supports run-on-demand event loops on desktop platforms
#[cfg(not(any(target_os = "ios", target_arch = "wasm32")))]
use winit::platform::run_on_demand::EventLoopExtRunOnDemand;

use crate::interpreter::{
  BufferUpload, EvalError, FrameDriver, GpuBindingInfo, GpuBufferKind,
  GpuEntryInfo, SamplerAddress, SamplerFilter, SamplerSettings, WindowEvent,
};
#[cfg(not(target_arch = "wasm32"))]
use crate::interpreter::{EvalException, IOManager};

// winit forbids creating more than one EventLoop per process. We keep one alive
// in a thread-local and reuse it across multiple spawn-window calls via
// run_app_on_demand, which takes &mut self instead of consuming the loop.
#[cfg(not(target_arch = "wasm32"))]
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
#[cfg(not(target_arch = "wasm32"))]
pub fn close_persistent_window() {
  PERSISTENT_RELOAD_STATE.with(|cell| cell.borrow_mut().take());
}
#[cfg(target_arch = "wasm32")]
pub fn close_persistent_window() {}

/// Returns the GPU core from the persistent reload state (if any), without
/// consuming it. Used by `ensure_gpu_ready` so that pre-spawn-window compute
/// dispatches (e.g. one-shot initialisation shaders) run on the same GPU that
/// the window will later reuse, rather than on a freshly-created headless GPU
/// that gets thrown away when `setup_window` takes `PERSISTENT_RELOAD_STATE`.
#[cfg(not(target_arch = "wasm32"))]
pub fn persistent_gpu() -> Option<Arc<RwLock<GpuCore>>> {
  PERSISTENT_RELOAD_STATE
    .with(|c| c.borrow().as_ref().map(|s| Arc::clone(&s.gpu)))
}
#[cfg(target_arch = "wasm32")]
pub fn persistent_gpu() -> Option<Arc<RwLock<GpuCore>>> {
  None
}

/// The texture format used for binding textures that can serve as render
/// targets, and for the headless offscreen fallback.  Rgba16Float gives 16-bit
/// float precision per channel, avoiding the colour banding / distortion that
/// Rgba8Unorm produces when many semi-transparent layers are blended.
const BINDING_TEXTURE_FORMAT: wgpu::TextureFormat =
  wgpu::TextureFormat::Rgba16Float;

/// Bytes per texel for [`BINDING_TEXTURE_FORMAT`] (Rgba16Float = 8).
const BINDING_TEXTURE_BPP: u32 = 8;

/// Convert an f32 in [0, 1] to an IEEE 754 half-precision (f16) value.
fn f32_to_f16(value: f32) -> u16 {
  let bits = value.to_bits();
  let sign = (bits >> 16) & 0x8000;
  let exp = ((bits >> 23) & 0xFF) as i32;
  let man = bits & 0x7F_FFFF;
  if exp == 0 {
    sign as u16
  } else if exp == 0xFF {
    (sign | 0x7C00 | if man != 0 { 0x200 } else { 0 }) as u16
  } else {
    let new_exp = exp - 127 + 15;
    if new_exp >= 31 {
      (sign | 0x7C00) as u16
    } else if new_exp <= 0 {
      sign as u16
    } else {
      (sign | ((new_exp as u32) << 10) | (man >> 13)) as u16
    }
  }
}

/// Convert an IEEE 754 half-precision (f16) value to f32. Inverse of
/// [`f32_to_f16`]; used when reading textures back for `save-png`.
fn f16_to_f32(half: u16) -> f32 {
  let sign = ((half as u32) & 0x8000) << 16;
  let exp = ((half >> 10) & 0x1F) as u32;
  let man = (half & 0x3FF) as u32;
  let bits = if exp == 0 {
    if man == 0 {
      sign
    } else {
      // subnormal: normalize
      let mut exp = 127 - 15 + 1;
      let mut man = man;
      while man & 0x400 == 0 {
        man <<= 1;
        exp -= 1;
      }
      sign | ((exp as u32) << 23) | ((man & 0x3FF) << 13)
    }
  } else if exp == 0x1F {
    sign | 0x7F80_0000 | (man << 13)
  } else {
    sign | ((exp + 127 - 15) << 23) | (man << 13)
  };
  f32::from_bits(bits)
}

/// Convert RGBA16Float texel bytes (8 bytes/pixel) to RGBA8 (4
/// bytes/pixel), clamping each channel to [0, 1]. Inverse of
/// [`rgba8_to_rgba16float`] (exact for all u8 values, since every k/255 is
/// within f16 rounding distance of its byte).
fn rgba16float_to_rgba8(data: &[u8]) -> Vec<u8> {
  let mut out = Vec::with_capacity(data.len() / 2);
  for pair in data.chunks_exact(2) {
    let half = u16::from_le_bytes([pair[0], pair[1]]);
    let f = f16_to_f32(half).clamp(0., 1.);
    out.push((f * 255.).round() as u8);
  }
  out
}

/// Convert RGBA8 (`&[u8]`, 4 bytes/pixel) to RGBA16Float (`Vec<u8>`, 8
/// bytes/pixel).  Each u8 channel is normalised to [0, 1] and stored as f16.
fn rgba8_to_rgba16float(data: &[u8]) -> Vec<u8> {
  let mut out = Vec::with_capacity(data.len() * 2);
  for &byte in data {
    let f = byte as f32 / 255.0;
    out.extend_from_slice(&f32_to_f16(f).to_le_bytes());
  }
  out
}

/// A sampler with `settings`, filtering and addressing the same way on both
/// axes.
fn create_sampler(
  device: &wgpu::Device,
  settings: SamplerSettings,
) -> wgpu::Sampler {
  let filter = match settings.filter {
    SamplerFilter::Nearest => wgpu::FilterMode::Nearest,
    SamplerFilter::Linear => wgpu::FilterMode::Linear,
  };
  let address = match settings.address {
    SamplerAddress::ClampToEdge => wgpu::AddressMode::ClampToEdge,
    SamplerAddress::Repeat => wgpu::AddressMode::Repeat,
    SamplerAddress::MirrorRepeat => wgpu::AddressMode::MirrorRepeat,
  };
  device.create_sampler(&wgpu::SamplerDescriptor {
    label: Some("sampler"),
    address_mode_u: address,
    address_mode_v: address,
    address_mode_w: address,
    mag_filter: filter,
    min_filter: filter,
    ..Default::default()
  })
}

fn create_texture_and_view(
  device: &wgpu::Device,
  label: &str,
  width: u32,
  height: u32,
  format: wgpu::TextureFormat,
  usage: wgpu::TextureUsages,
) -> (wgpu::Texture, wgpu::TextureView) {
  let texture = device.create_texture(&wgpu::TextureDescriptor {
    label: Some(label),
    size: wgpu::Extent3d {
      width,
      height,
      depth_or_array_layers: 1,
    },
    mip_level_count: 1,
    sample_count: 1,
    dimension: wgpu::TextureDimension::D2,
    format,
    usage,
    view_formats: &[],
  });
  let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
  (texture, view)
}

/// iOS and the web have no run-on-demand event loop (the OS or browser owns
/// the main loop), so the built-in window loop is unavailable there;
/// embedding applications drive rendering themselves (the web runtime from
/// `requestAnimationFrame`).
#[cfg(any(target_os = "ios", target_arch = "wasm32"))]
pub fn run_window_loop<D: FrameDriver>(
  _driver: &mut D,
) -> Result<bool, EvalError> {
  unimplemented!(
    "easl's built-in window loop isn't supported on this platform; the \
     embedding application must drive frames itself"
  )
}

#[cfg(not(any(target_os = "ios", target_arch = "wasm32")))]
pub fn run_window_loop<D: FrameDriver>(
  driver: &mut D,
) -> Result<bool, EvalError> {
  EVENT_LOOP.with(|cell| {
    let mut opt = cell.write().unwrap();
    let event_loop = opt.get_or_insert_with(|| EventLoop::new().unwrap());
    event_loop.set_control_flow(ControlFlow::Poll);
    let mut app = App {
      driver,
      state: None,
      error: None,
      closed: false,
      last_frame_time: None,
      window_start_time: None,
      last_frame_presented: false,
      observed_presented_interval: None,
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

#[cfg(not(target_arch = "wasm32"))]
struct App<'a, D: FrameDriver> {
  driver: &'a mut D,
  state: Option<RenderState>,
  error: Option<EvalError>,
  closed: bool,
  last_frame_time: Option<Instant>,
  window_start_time: Option<Instant>,
  /// Whether the previous frame presented to the screen. Presented frames
  /// are paced by drawable acquisition (vsync); unpresented frames must be
  /// paced explicitly (see the RedrawRequested handler).
  last_frame_presented: bool,
  /// The inter-frame interval last observed between consecutive presented
  /// frames — the display's real cadence, used to pace unpresented frames
  /// when the monitor's refresh rate can't be queried.
  observed_presented_interval: Option<Duration>,
  /// Set to true when the loop exits because a hot-reload was requested.
  reload: bool,
}

/// The interval at which to run frames that produce no present (occluded
/// window, or a frame with no screen-targeted draws): the display's refresh
/// rate when the monitor reports one, else the cadence observed while
/// frames were presenting, else 60Hz.
#[cfg(not(target_arch = "wasm32"))]
fn unpresented_frame_interval(
  refresh_rate_millihertz: Option<u32>,
  observed_presented_interval: Option<Duration>,
) -> Duration {
  match refresh_rate_millihertz {
    Some(millihertz) if millihertz > 0 => {
      Duration::from_secs_f64(1000. / millihertz as f64)
    }
    _ => observed_presented_interval.unwrap_or(Duration::from_micros(16_667)),
  }
}

/// Metadata about a single GPU buffer binding, kept so we can recreate
/// buffers with the correct usage flags when the size changes at runtime.
#[derive(Clone)]
pub struct BindingSlot {
  pub group: u8,
  pub binding: u8,
  /// Source-level variable name, for diagnostics.
  pub name: std::sync::Arc<str>,
  pub kind: GpuBufferKind,
}

/// Where a `GpuCore` renders screen-targeted draws. Every target runs them
/// in program order with the frame's other GPU work.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScreenTarget {
  /// Directly into the surface's image for the frame, acquired at the
  /// frame's first screen draw and presented by `finish_frame`. When the
  /// surface has no image to give (occluded, lost), the frame's screen draws
  /// run into a 1×1 stand-in, so their storage writes still happen.
  Surface,
  /// Into a window-sized texture, copied to the surface (when there is one)
  /// and presented by `finish_frame`. The web runtime draws this way: a CPU
  /// read of GPU results yields to the browser mid-frame, which would
  /// present a canvas image that is still being drawn. Tests use it without
  /// a surface to read the screen back (`read_screen`).
  Texture,
  /// No screen (headless): screen draws run into a 1×1 stand-in.
  StandIn,
  /// The embedder draws the screen itself (`execute_render_batch_to_view`),
  /// so the frame path skips screen draws.
  Embedder,
}

/// The screen image of a frame in progress.
struct FrameScreen {
  /// The surface image being drawn, under `ScreenTarget::Surface`.
  surface_texture: Option<wgpu::SurfaceTexture>,
  view: wgpu::TextureView,
  /// Whether a pass has drawn into `view` yet this frame; the first one
  /// clears it.
  drawn: bool,
}

/// Identity of a cached render pipeline: (vert entry id, frag entry id,
/// additive blending, target format).
type RenderPipelineKey = (u16, u16, bool, wgpu::TextureFormat);

/// Descriptor objects owned by one cached pipeline. Buffers stay global and
/// shared across all pipelines; these are only the layout/bind-group
/// objects covering the subset of bindings the pipeline's entry points use.
struct PipelineBindings {
  /// (group, binding) -> the visibility its layout entry declares. Only
  /// bindings used by this pipeline's entries (plus all textures) appear.
  used: HashMap<(u8, u8), wgpu::ShaderStages>,
  /// One layout per group index (empty layouts for unused group indices).
  layouts: Vec<wgpu::BindGroupLayout>,
  bind_groups: Vec<wgpu::BindGroup>,
}

struct CachedComputePipeline {
  pipeline: wgpu::ComputePipeline,
  bindings: PipelineBindings,
}

struct CachedRenderPipeline {
  pipeline: wgpu::RenderPipeline,
  bindings: PipelineBindings,
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
  /// Dense GPU entry table (indexed by the entry ids `WindowEvent` carries):
  /// compiled entry name + the buffer bindings the entry's code references.
  /// Each pipeline's bind group layouts cover exactly its entries' bindings
  /// (plus all textures), so per-stage binding budgets only pay for genuine
  /// usage.
  gpu_entries: Vec<GpuEntryInfo>,
  /// Backend of the device, when known (None for embedder-owned devices).
  /// Used for backend-specific binding-budget validation.
  backend: Option<wgpu::Backend>,
  /// Cached compute pipelines, indexed by dense entry id.
  compute_pipelines: Vec<Option<CachedComputePipeline>>,
  /// Cached render pipelines with their per-pipeline binding objects.
  /// Linear scan keyed by (vert_id, frag_id, additive, format) — pipeline
  /// counts are small and integer-compare scans beat hashing here.
  render_pipelines: Vec<(RenderPipelineKey, CachedRenderPipeline)>,
  pub binding_slots: Vec<BindingSlot>,
  pub binding_buffers: HashMap<(u8, u8), wgpu::Buffer>,
  /// Tracks the byte-length of each buffer (or width*height*4 for textures)
  /// so we can detect when a binding's size changes and needs to be recreated.
  pub binding_buffer_sizes: HashMap<(u8, u8), u64>,
  /// GPU textures for `Texture2D` bindings (keyed by group, binding).
  pub textures: HashMap<(u8, u8), wgpu::Texture>,
  /// Texture views for `Texture2D` bindings, used in bind groups.
  pub texture_views: HashMap<(u8, u8), wgpu::TextureView>,
  /// Samplers for `Sampler` bindings, with the settings each was made from.
  samplers: HashMap<(u8, u8), (SamplerSettings, wgpu::Sampler)>,
  /// Current window dimensions in pixels.
  pub window_size: (u32, u32),
  /// Time in seconds since the window was opened, updated at the start of each frame.
  pub window_time: f32,
  /// Time in seconds between the previous frame and the current frame.
  pub window_delta_time: f32,
  /// Number of frames rendered since the window opened (0 on the first frame).
  pub window_frame_index: u32,
  /// Set of lowercase character keys currently held down.
  pub keys_down: HashSet<String>,
  /// Set of lowercase character keys pressed this frame (not held from a previous frame).
  /// Cleared after each frame's eval completes.
  pub keys_just_down: HashSet<String>,
  /// Pixel position of the mouse cursor relative to the window, in physical pixels.
  pub mouse_coords: (u32, u32),
  /// True if the mouse cursor is currently inside the window.
  pub mouse_present: bool,
  /// True if the left mouse button is currently held down.
  pub mouse_down: bool,
  /// True if the left mouse button was pressed this frame (not held from a previous frame).
  /// Cleared after each frame's eval completes.
  pub mouse_just_down: bool,
  /// True if the right mouse button is currently held down.
  pub mouse_right_down: bool,
  /// True if the right mouse button was pressed this frame. Cleared after
  /// each frame's eval completes.
  pub mouse_right_just_down: bool,
  /// Raw mouse motion accumulated since the previous frame, in device units
  /// (+y down). Cleared after each frame's eval completes.
  pub mouse_delta: (f32, f32),
  /// True while the cursor is captured (hidden and locked to the window).
  pub mouse_captured: bool,
  /// A pending `capture-mouse` (`Some(true)`) or `release-mouse`
  /// (`Some(false)`) request, applied to the window at the end of the frame
  /// that made it.
  pub mouse_capture_request: Option<bool>,
  /// The window surface, if a window is open (`attach_surface`).
  pub surface: Option<wgpu::Surface<'static>>,
  /// Surface configuration (format, size, present mode, …). Present iff `surface` is Some.
  pub surface_config: Option<wgpu::SurfaceConfiguration>,
  /// Where screen-targeted draws render.
  pub screen_target: ScreenTarget,
  /// The window-sized texture `ScreenTarget::Texture` draws into.
  screen_texture: Option<wgpu::Texture>,
  /// The screen image of the frame in progress, from the frame's first
  /// screen draw until `finish_frame`.
  frame_screen: Option<FrameScreen>,
  /// A 1×1 placeholder texture used as a stand-in in bind groups when a real
  /// texture is simultaneously the render target (COLOR_TARGET + RESOURCE is
  /// forbidden by wgpu within the same render pass).
  placeholder_texture_view: wgpu::TextureView,
}

impl GpuCore {
  /// Bind group entries for one group index of a pipeline's used-binding
  /// set. `placeholder_for` swaps that texture slot for the 1x1 placeholder
  /// (used when a texture is simultaneously the render target).
  fn bind_group_entries_for(
    &self,
    used: &HashMap<(u8, u8), wgpu::ShaderStages>,
    group_idx: usize,
    placeholder_for: Option<(u8, u8)>,
  ) -> Vec<wgpu::BindGroupEntry<'_>> {
    let mut slots: Vec<&BindingSlot> = self
      .binding_slots
      .iter()
      .filter(|slot| {
        slot.group as usize == group_idx
          && used.contains_key(&(slot.group, slot.binding))
      })
      .collect();
    slots.sort_by_key(|slot| slot.binding);
    slots
      .into_iter()
      .map(|slot| {
        let key = (slot.group, slot.binding);
        wgpu::BindGroupEntry {
          binding: slot.binding as u32,
          resource: match slot.kind {
            GpuBufferKind::Texture2D => wgpu::BindingResource::TextureView(
              if placeholder_for == Some(key) {
                &self.placeholder_texture_view
              } else {
                self.texture_views.get(&key).expect("texture view missing")
              },
            ),
            GpuBufferKind::Sampler => {
              wgpu::BindingResource::Sampler(&self.samplers[&key].1)
            }
            _ => self.binding_buffers[&key].as_entire_binding(),
          },
        }
      })
      .collect()
  }

  /// Creates the bind groups for a pipeline's layouts. With
  /// `placeholder_for` set, the given texture slot is replaced by the
  /// placeholder view (avoids wgpu's COLOR_TARGET + RESOURCE conflict when
  /// rendering to a bound texture).
  fn create_bind_groups(
    &self,
    used: &HashMap<(u8, u8), wgpu::ShaderStages>,
    layouts: &[wgpu::BindGroupLayout],
    placeholder_for: Option<(u8, u8)>,
  ) -> Vec<wgpu::BindGroup> {
    layouts
      .iter()
      .enumerate()
      .map(|(group_idx, layout)| {
        let entries =
          self.bind_group_entries_for(used, group_idx, placeholder_for);
        self.device.create_bind_group(&wgpu::BindGroupDescriptor {
          label: Some(&format!("bind group {group_idx}")),
          layout,
          entries: &entries,
        })
      })
      .collect()
  }

  /// Builds the layout and bind-group objects for a pipeline whose stages
  /// are the given (entry id, stage) pairs. The used-binding set is exactly
  /// the union of the entries' effect-derived binding sets, plus every
  /// texture binding (texture usage isn't effect-tracked yet). Validates
  /// the set against the device's per-stage binding budgets first, so limit
  /// violations surface as a clear easl error naming the bindings rather
  /// than an opaque wgpu failure.
  fn build_pipeline_bindings(
    &self,
    label: &str,
    stage_entries: &[(u16, wgpu::ShaderStages)],
  ) -> PipelineBindings {
    let mut used: HashMap<(u8, u8), wgpu::ShaderStages> = HashMap::new();
    let mut all_stages = wgpu::ShaderStages::NONE;
    for (entry_id, stage) in stage_entries {
      all_stages |= *stage;
      let entry =
        self.gpu_entries.get(*entry_id as usize).unwrap_or_else(|| {
          panic!(
            "easl internal error: entry id {entry_id} out of range for the \
           GPU entry table ({label})"
          )
        });
      for key in &entry.used_bindings {
        *used.entry(*key).or_insert(wgpu::ShaderStages::NONE) |= *stage;
      }
    }
    for slot in &self.binding_slots {
      if matches!(slot.kind, GpuBufferKind::Texture2D | GpuBufferKind::Sampler)
      {
        *used
          .entry((slot.group, slot.binding))
          .or_insert(wgpu::ShaderStages::NONE) |= all_stages;
      }
    }
    if let Err(message) = self.validate_pipeline_bindings(label, &used) {
      panic!("easl: {message}");
    }
    let group_count = used
      .keys()
      .map(|(group, _)| *group as usize + 1)
      .max()
      .unwrap_or(0);
    let layouts: Vec<wgpu::BindGroupLayout> = (0..group_count)
      .map(|group_idx| {
        let mut slots: Vec<&BindingSlot> = self
          .binding_slots
          .iter()
          .filter(|slot| {
            slot.group as usize == group_idx
              && used.contains_key(&(slot.group, slot.binding))
          })
          .collect();
        slots.sort_by_key(|slot| slot.binding);
        let entries: Vec<wgpu::BindGroupLayoutEntry> = slots
          .into_iter()
          .map(|slot| wgpu::BindGroupLayoutEntry {
            binding: slot.binding as u32,
            visibility: used[&(slot.group, slot.binding)],
            ty: match slot.kind {
              GpuBufferKind::Texture2D => wgpu::BindingType::Texture {
                multisampled: false,
                view_dimension: wgpu::TextureViewDimension::D2,
                sample_type: wgpu::TextureSampleType::Float {
                  filterable: true,
                },
              },
              GpuBufferKind::Sampler => {
                wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering)
              }
              _ => wgpu::BindingType::Buffer {
                ty: gpu_binding_type(slot.kind),
                has_dynamic_offset: false,
                min_binding_size: None,
              },
            },
            count: None,
          })
          .collect();
        self
          .device
          .create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some(&format!("{label} bind group layout {group_idx}")),
            entries: &entries,
          })
      })
      .collect();
    let bind_groups = self.create_bind_groups(&used, &layouts, None);
    PipelineBindings {
      used,
      layouts,
      bind_groups,
    }
  }

  /// Validates one pipeline's binding set against the device's per-stage
  /// limits and Metal's vertex-stage buffer budget.
  fn validate_pipeline_bindings(
    &self,
    label: &str,
    used: &HashMap<(u8, u8), wgpu::ShaderStages>,
  ) -> Result<(), String> {
    let limits = self.device.limits();
    let name_of = |key: &(u8, u8)| -> String {
      self
        .binding_slots
        .iter()
        .find(|slot| (slot.group, slot.binding) == *key)
        .map(|slot| {
          format!("{} (group {}, binding {})", slot.name, key.0, key.1)
        })
        .unwrap_or_else(|| format!("group {}, binding {}", key.0, key.1))
    };
    let buffer_kind_of = |key: &(u8, u8)| -> Option<GpuBufferKind> {
      self
        .binding_slots
        .iter()
        .find(|slot| (slot.group, slot.binding) == *key)
        .map(|slot| slot.kind)
        .filter(|kind| {
          !matches!(kind, GpuBufferKind::Texture2D | GpuBufferKind::Sampler)
        })
    };
    for (stage_name, stage) in [
      ("vertex", wgpu::ShaderStages::VERTEX),
      ("fragment", wgpu::ShaderStages::FRAGMENT),
      ("compute", wgpu::ShaderStages::COMPUTE),
    ] {
      let mut storage: Vec<(u8, u8)> = vec![];
      let mut uniforms: Vec<(u8, u8)> = vec![];
      for (key, visibility) in used {
        if !visibility.contains(stage) {
          continue;
        }
        match buffer_kind_of(key) {
          Some(GpuBufferKind::Uniform) => uniforms.push(*key),
          Some(_) => storage.push(*key),
          None => {}
        }
      }
      storage.sort();
      uniforms.sort();
      let describe = |keys: &[(u8, u8)]| -> String {
        keys.iter().map(name_of).collect::<Vec<_>>().join(", ")
      };
      if storage.len() as u32 > limits.max_storage_buffers_per_shader_stage {
        return Err(format!(
          "the {stage_name} stage of {label} references {} storage \
           buffers, but this device supports at most {} per stage. Storage \
           bindings used: {}",
          storage.len(),
          limits.max_storage_buffers_per_shader_stage,
          describe(&storage),
        ));
      }
      if uniforms.len() as u32 > limits.max_uniform_buffers_per_shader_stage {
        return Err(format!(
          "the {stage_name} stage of {label} references {} uniform \
           buffers, but this device supports at most {} per stage. Uniform \
           bindings used: {}",
          uniforms.len(),
          limits.max_uniform_buffers_per_shader_stage,
          describe(&uniforms),
        ));
      }
      if stage == wgpu::ShaderStages::VERTEX
        && self.backend == Some(wgpu::Backend::Metal)
        && storage.len() as u32
          + uniforms.len() as u32
          + METAL_RESERVED_VERTEX_BUFFER_SLOTS
          > METAL_MAX_VERTEX_STAGE_BUFFERS
      {
        let mut all: Vec<(u8, u8)> = storage;
        all.extend(uniforms);
        all.sort();
        return Err(format!(
          "{label} needs too many GPU buffer bindings in the vertex stage: \
           {} bindings are referenced from its vertex shader, plus {} slot \
           wgpu reserves internally, but Metal supports at most {} \
           vertex-stage buffers. Vertex-visible bindings: {}",
          all.len(),
          METAL_RESERVED_VERTEX_BUFFER_SLOTS,
          METAL_MAX_VERTEX_STAGE_BUFFERS,
          describe(&all),
        ));
      }
    }
    Ok(())
  }

  /// Recreates the bind groups of every cached pipeline that references any
  /// of the changed bindings (buffers/textures are recreated on resize, and
  /// bind groups are immutable snapshots of buffer references).
  fn rebuild_bind_groups_for(&mut self, changed: &[(u8, u8)]) {
    let mut rebuilt_compute: Vec<(usize, Vec<wgpu::BindGroup>)> = vec![];
    for (id, cached) in self.compute_pipelines.iter().enumerate() {
      if let Some(cached) = cached
        && changed
          .iter()
          .any(|key| cached.bindings.used.contains_key(key))
      {
        rebuilt_compute.push((
          id,
          self.create_bind_groups(
            &cached.bindings.used,
            &cached.bindings.layouts,
            None,
          ),
        ));
      }
    }
    for (id, groups) in rebuilt_compute {
      self.compute_pipelines[id]
        .as_mut()
        .unwrap()
        .bindings
        .bind_groups = groups;
    }
    let mut rebuilt_render: Vec<(usize, Vec<wgpu::BindGroup>)> = vec![];
    for (i, (_, cached)) in self.render_pipelines.iter().enumerate() {
      if changed
        .iter()
        .any(|key| cached.bindings.used.contains_key(key))
      {
        rebuilt_render.push((
          i,
          self.create_bind_groups(
            &cached.bindings.used,
            &cached.bindings.layouts,
            None,
          ),
        ));
      }
    }
    for (i, groups) in rebuilt_render {
      self.render_pipelines[i].1.bindings.bind_groups = groups;
    }
  }

  /// Hot-reload: swap the shader module and rebuild everything that depends on
  /// it (compute pipelines, bind group layouts, bind groups, pipeline layout).
  /// Buffers whose size is unchanged are reused so GPU-written data survives.
  pub fn update_for_reload(
    &mut self,
    wgsl: &str,
    binding_infos: &[GpuBindingInfo],
    gpu_entries: &[GpuEntryInfo],
  ) {
    self.gpu_entries = gpu_entries.to_vec();
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
      .map(|info| BindingSlot {
        group: info.group,
        binding: info.binding,
        name: info.name.clone(),
        kind: info.kind,
      })
      .collect();

    // 4. Rebuild buffers.  Reuse existing buffers whose size didn't change so
    // that GPU-written storage data (e.g. compute results) survives the reload.
    // For Texture2D slots, keep existing textures/views (no buffer).
    let mut new_buffers: HashMap<(u8, u8), wgpu::Buffer> = HashMap::new();
    let mut new_sizes: HashMap<(u8, u8), u64> = HashMap::new();
    for info in binding_infos {
      let (group, binding, kind, size) =
        (info.group, info.binding, info.kind, info.byte_size);
      let key = (group, binding);
      if kind == GpuBufferKind::Sampler {
        // Keep an existing sampler; a new one starts at the defaults.
        if !self.samplers.contains_key(&key) {
          let settings = SamplerSettings::default();
          self
            .samplers
            .insert(key, (settings, create_sampler(&self.device, settings)));
        }
        continue;
      }
      if kind == GpuBufferKind::Texture2D {
        // Keep existing texture if one exists; create a placeholder if not.
        if !self.textures.contains_key(&key) {
          let (texture, view) = create_texture_and_view(
            &self.device,
            &format!("texture g{group}b{binding}"),
            1,
            1,
            BINDING_TEXTURE_FORMAT,
            wgpu::TextureUsages::TEXTURE_BINDING
              | wgpu::TextureUsages::COPY_SRC
              | wgpu::TextureUsages::COPY_DST
              | wgpu::TextureUsages::RENDER_ATTACHMENT,
          );
          self.textures.insert(key, texture);
          self.texture_views.insert(key, view);
        }
        continue;
      }
      // Minimum one word — wgpu forbids zero-size buffers. Never larger:
      // WGSL derives `arrayLength` from the buffer's byte size, so
      // over-allocating a runtime-sized storage binding fabricates
      // phantom elements (uploads arrive pre-padded per binding kind).
      let alloc_size = size.max(4);
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

    // Per-pipeline layouts and bind groups are rebuilt lazily: the pipeline
    // caches were cleared above, so the next get_or_create_* recreates them
    // against the new shader module, buffers, and entry table.
  }

  pub fn get_or_create_compute_pipeline(&mut self, entry: u16) {
    if self.compute_pipelines.len() <= entry as usize {
      self
        .compute_pipelines
        .resize_with(entry as usize + 1, || None);
    }
    if self.compute_pipelines[entry as usize].is_some() {
      return;
    }
    let entry_name = self.gpu_entries[entry as usize].name.clone();
    let label = format!("compute pipeline {entry_name}");
    let bindings = self
      .build_pipeline_bindings(&label, &[(entry, wgpu::ShaderStages::COMPUTE)]);
    let layout_refs: Vec<Option<&wgpu::BindGroupLayout>> =
      bindings.layouts.iter().map(Some).collect();
    let pipeline_layout =
      self
        .device
        .create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
          label: Some(&label),
          bind_group_layouts: &layout_refs,
          immediate_size: 0,
        });
    let pipeline =
      self
        .device
        .create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
          label: Some(&format!("easl {label}")),
          layout: Some(&pipeline_layout),
          module: &self.shader,
          entry_point: Some(&entry_name),
          compilation_options: Default::default(),
          cache: None,
        });
    self.compute_pipelines[entry as usize] =
      Some(CachedComputePipeline { pipeline, bindings });
  }

  /// Uploads binding data to the GPU. If a buffer's size has changed (e.g.
  /// a dynamic array was reassigned), the buffer is recreated and all bind
  /// groups are rebuilt.
  ///
  /// `BufferUpload::Clear` buffers are zeroed via `encoder.clear_buffer` (a
  /// fast GPU-side zero-fill with no CPU allocation) rather than copying a
  /// zeroed slice from the CPU.
  pub fn upload_bindings(&mut self, data: &[((u8, u8), BufferUpload)]) {
    let mut changed_keys: Vec<(u8, u8)> = vec![];

    // First pass: recreate any buffers/textures whose size changed.
    for ((group, binding), upload) in data {
      let key = (*group, *binding);
      match upload {
        BufferUpload::TextureData { width, height, .. } => {
          let incoming_size =
            *width as u64 * *height as u64 * BINDING_TEXTURE_BPP as u64;
          // Compare extents, not byte counts: a 1x4 and a 2x2 image are the
          // same size but need different textures.
          let stored_extent = self
            .textures
            .get(&key)
            .map(|texture| (texture.width(), texture.height()));
          if stored_extent != Some((*width, *height)) {
            let (texture, view) = create_texture_and_view(
              &self.device,
              &format!("texture g{group}b{binding}"),
              *width,
              *height,
              BINDING_TEXTURE_FORMAT,
              wgpu::TextureUsages::TEXTURE_BINDING
                | wgpu::TextureUsages::COPY_SRC
                | wgpu::TextureUsages::COPY_DST
                | wgpu::TextureUsages::RENDER_ATTACHMENT,
            );
            self.textures.insert(key, texture);
            self.texture_views.insert(key, view);
            self.binding_buffer_sizes.insert(key, incoming_size);
            changed_keys.push(key);
          }
        }
        BufferUpload::Sampler(settings) => {
          if self.samplers.get(&key).map(|(stored, _)| stored) != Some(settings)
          {
            self.samplers.insert(
              key,
              (*settings, create_sampler(&self.device, *settings)),
            );
            changed_keys.push(key);
          }
        }
        _ => {
          let incoming_size = match upload {
            BufferUpload::Data(bytes) => bytes.len() as u64,
            BufferUpload::Clear { byte_count } => *byte_count,
            BufferUpload::TextureData { .. } | BufferUpload::Sampler(_) => {
              unreachable!()
            }
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
              // Minimum one word, never more — see the comment on the
              // initial allocations: extra bytes fabricate phantom
              // `arrayLength` elements.
              size: incoming_size.max(4),
              usage: gpu_buffer_usage(kind),
              mapped_at_creation: false,
            });
            self.binding_buffers.insert(key, buffer);
            self.binding_buffer_sizes.insert(key, incoming_size);
            changed_keys.push(key);
          }
        }
      }
    }

    if !changed_keys.is_empty() {
      self.rebuild_bind_groups_for(&changed_keys);
    }

    // Second pass: write data, issue GPU clears, or upload texture pixels.
    // (Sampler settings are complete after the first pass.)
    let mut encoder: Option<wgpu::CommandEncoder> = None;
    for ((group, binding), upload) in data {
      let key = (*group, *binding);
      match upload {
        BufferUpload::Sampler(_) => {}
        BufferUpload::Data(bytes) => {
          if let Some(buffer) = self.binding_buffers.get(&key) {
            self.queue.write_buffer(buffer, 0, bytes);
          }
        }
        BufferUpload::Clear { .. } => {
          if let Some(buffer) = self.binding_buffers.get(&key) {
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
        BufferUpload::TextureData {
          width,
          height,
          data,
        } => {
          if let Some(texture) = self.textures.get(&key) {
            let f16_data = rgba8_to_rgba16float(data);
            self.queue.write_texture(
              wgpu::TexelCopyTextureInfo {
                texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
              },
              &f16_data,
              wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(*width * BINDING_TEXTURE_BPP),
                rows_per_image: Some(*height),
              },
              wgpu::Extent3d {
                width: *width,
                height: *height,
                depth_or_array_layers: 1,
              },
            );
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
    entry: u16,
    workgroup_count: (u32, u32, u32),
    pre_upload: Vec<((u8, u8), BufferUpload)>,
  ) {
    self.upload_bindings(&pre_upload);
    self.get_or_create_compute_pipeline(entry);

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
      let cached = self.compute_pipelines[entry as usize].as_ref().unwrap();
      for (group_idx, bind_group) in
        cached.bindings.bind_groups.iter().enumerate()
      {
        compute_pass.set_bind_group(group_idx as u32, bind_group, &[]);
      }
      compute_pass.set_pipeline(&cached.pipeline);
      let (x, y, z) = workgroup_count;
      compute_pass.dispatch_workgroups(x, y, z);
    }
    self.queue.submit(std::iter::once(encoder.finish()));
    self
      .device
      .poll(wgpu::PollType::wait_indefinitely())
      .unwrap();
  }

  /// Batches multiple compute dispatches into the minimum number of submits,
  /// splitting only when a call's pre_upload would overwrite a binding already
  /// uploaded for the current in-flight encoder. Consecutive calls with
  /// non-conflicting uploads are merged into a single encoder and submitted
  /// together. Always uses a single final poll, saving N-1 blocking waits vs.
  /// calling `execute_compute` N times.
  pub fn execute_compute_batch(
    &mut self,
    calls: Vec<(u16, (u32, u32, u32), Vec<((u8, u8), BufferUpload)>)>,
  ) {
    if calls.is_empty() {
      return;
    }
    self.encode_compute_batch(calls);
    self
      .device
      .poll(wgpu::PollType::wait_indefinitely())
      .unwrap();
  }

  /// Encodes and submits a batch of compute dispatches without waiting for
  /// completion. Each dispatch's pre_uploads are applied just before it is
  /// encoded; when an upload would overwrite a binding that already-encoded
  /// (but not yet submitted) dispatches depend on — e.g. the same entry
  /// dispatched twice in one frame with different captured-scope values —
  /// the current encoder is submitted first so those dispatches see their
  /// own values. In the common case of no conflicting uploads every
  /// dispatch is encoded into a single submit.
  fn encode_compute_batch(
    &mut self,
    calls: Vec<(u16, (u32, u32, u32), Vec<((u8, u8), BufferUpload)>)>,
  ) {
    // Tracks bindings the dispatches already encoded into the current
    // encoder (not yet submitted) upload or access: a later upload of any of
    // them must wait for that encoder's submit, or they'd see the new value.
    let mut pending_bindings: std::collections::HashSet<(u8, u8)> =
      std::collections::HashSet::new();
    let mut current_encoder: Option<wgpu::CommandEncoder> = None;

    for (entry, (x, y, z), pre_upload) in calls {
      // If this call would overwrite a binding already uploaded for the
      // current encoder's dispatches, submit that encoder first so those
      // dispatches see the old values, then start a fresh encoder.
      let has_conflict = pre_upload
        .iter()
        .any(|(gb, _)| pending_bindings.contains(gb));
      if has_conflict {
        if let Some(enc) = current_encoder.take() {
          self.queue.submit(std::iter::once(enc.finish()));
        }
        pending_bindings.clear();
      }

      self.upload_bindings(&pre_upload);
      for (gb, _) in &pre_upload {
        pending_bindings.insert(*gb);
      }
      pending_bindings.extend(
        self.gpu_entries[entry as usize]
          .used_bindings
          .iter()
          .copied(),
      );
      self.get_or_create_compute_pipeline(entry);

      let encoder = current_encoder.get_or_insert_with(|| {
        self
          .device
          .create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("compute encoder"),
          })
      });
      {
        let mut compute_pass =
          encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
            label: Some("compute pass"),
            timestamp_writes: None,
          });
        let cached = self.compute_pipelines[entry as usize].as_ref().unwrap();
        for (group_idx, bind_group) in
          cached.bindings.bind_groups.iter().enumerate()
        {
          compute_pass.set_bind_group(group_idx as u32, bind_group, &[]);
        }
        compute_pass.set_pipeline(&cached.pipeline);
        compute_pass.dispatch_workgroups(x, y, z);
      }
    }
    if let Some(enc) = current_encoder.take() {
      self.queue.submit(std::iter::once(enc.finish()));
    }
  }

  /// Encodes and submits a sequence of render calls: one render pass per
  /// consecutive same-render-target group, all in one encoder/submit.
  /// Screen-targeted calls draw into the frame's screen image
  /// (`begin_screen_pass`), or are skipped under `ScreenTarget::Embedder`.
  /// Does NOT apply pre_uploads or wait for completion — callers own upload
  /// ordering and synchronization. Pipelines must already exist in the
  /// cache.
  fn encode_render_groups(
    &mut self,
    calls: &[(u16, u16, u32, bool, Option<(u8, u8)>)],
  ) {
    if calls.is_empty() {
      return;
    }
    let screen_format = self.screen_format();

    let mut encoder =
      self
        .device
        .create_command_encoder(&wgpu::CommandEncoderDescriptor {
          label: Some("render encoder"),
        });
    let mut any_pass = false;

    // Group consecutive calls by render target, one render pass per group.
    let mut i = 0;
    while i < calls.len() {
      let current_rt = calls[i].4;
      let end = calls[i..]
        .iter()
        .position(|c| c.4 != current_rt)
        .map_or(calls.len(), |j| i + j);
      let group = &calls[i..end];

      // Texture targets are cleared by each pass; the screen only by the
      // frame's first. The views are owned, so no borrow of `self` outlives
      // this point.
      let (view, load) = match current_rt {
        Some(rt) => (
          self.textures[&rt]
            .create_view(&wgpu::TextureViewDescriptor::default()),
          wgpu::LoadOp::Clear(wgpu::Color::BLACK),
        ),
        None if self.screen_target == ScreenTarget::Embedder => {
          i = end;
          continue;
        }
        None => self.begin_screen_pass(),
      };
      let format = if current_rt.is_none() {
        screen_format
      } else {
        GpuCore::OFFSCREEN_FORMAT
      };
      // Resolve each draw's pipeline before the pass borrow begins. For
      // pipelines that bind the pass's render target as a texture, build a
      // variant bind-group set with a placeholder in that slot (avoids
      // wgpu's COLOR_TARGET + RESOURCE conflict).
      let prepared: Vec<(usize, Option<Vec<wgpu::BindGroup>>)> = group
        .iter()
        .map(|(vert, frag, _, additive, _)| {
          let key = (*vert, *frag, *additive, format);
          let index = self
            .render_pipelines
            .iter()
            .position(|(k, _)| *k == key)
            .expect("render pipeline missing from cache");
          let bindings = &self.render_pipelines[index].1.bindings;
          let placeholder = current_rt
            .filter(|rt| bindings.used.contains_key(rt))
            .map(|rt| {
              self.create_bind_groups(
                &bindings.used,
                &bindings.layouts,
                Some(rt),
              )
            });
          (index, placeholder)
        })
        .collect();

      {
        let mut render_pass =
          encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("render pass"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
              view: &view,
              resolve_target: None,
              depth_slice: None,
              ops: wgpu::Operations {
                load,
                store: wgpu::StoreOp::Store,
              },
            })],
            depth_stencil_attachment: None,
            occlusion_query_set: None,
            timestamp_writes: None,
            multiview_mask: None,
          });
        for ((_, _, vert_count, _, _), (pipeline_index, placeholder)) in
          group.iter().zip(prepared.iter())
        {
          let cached = &self.render_pipelines[*pipeline_index].1;
          let bind_groups = placeholder
            .as_deref()
            .unwrap_or(&cached.bindings.bind_groups);
          for (group_idx, bind_group) in bind_groups.iter().enumerate() {
            render_pass.set_bind_group(group_idx as u32, bind_group, &[]);
          }
          render_pass.set_pipeline(&cached.pipeline);
          render_pass.draw(0..*vert_count, 0..1);
        }
      }
      any_pass = true;

      i = end;
    }

    if any_pass {
      self.queue.submit(std::iter::once(encoder.finish()));
    }
  }

  /// Executes a frame's queued GPU work — compute dispatches and render
  /// passes, to textures and to the screen — strictly in program order, so
  /// GPU write→read dependencies hold regardless of the kinds involved: a
  /// compute dispatched after a draw sees what it wrote, a draw dispatched
  /// after a compute sees its results, and each call's pre_uploads (the CPU
  /// writes made before it) apply at its place in the order. This is a hard
  /// language requirement.
  ///
  /// Screen draws go into the frame's screen image (see `ScreenTarget`),
  /// which stays open across calls until `finish_frame` presents it, so a
  /// mid-frame flush (`StdoutIO::flush_queued_compute`) can run the work
  /// queued so far and later work still draws into the same frame.
  ///
  /// Shared by the winit frame loop (`render_frame`), the headless test
  /// frame loop (`CaptureIO::run_spawn_window`), the web runtime, and the
  /// mid-frame flush path, so all of them exercise one ordering
  /// implementation rather than parallel ones.
  pub fn execute_frame_gpu_work(&mut self, draw_calls: &[WindowEvent]) {
    let screen_format = self.screen_format();

    // Pre-pass: ensure all pipeline objects exist. No buffer uploads here —
    // uploads are applied below.
    for draw_call in draw_calls {
      match draw_call {
        WindowEvent::RenderShaders {
          vert,
          frag,
          additive,
          render_target,
          ..
        } => {
          let format = if render_target.is_none() {
            screen_format
          } else {
            GpuCore::OFFSCREEN_FORMAT
          };
          self.get_or_create_render_pipeline(*vert, *frag, *additive, format);
        }
        WindowEvent::ComputeShader { entry, .. } => {
          self.get_or_create_compute_pipeline(*entry);
        }
      }
    }

    // Walk the events in program order, batching consecutive runs of the
    // same kind: compute runs go through the conflict-splitting compute
    // encoder, render runs through the render-pass grouper (which merges
    // consecutive same-target draws into one pass). A render run's uploads
    // are applied before it is encoded, so a draw with uploads starts a new
    // run: they must not reach the draws before it.
    let mut pending_compute: Vec<(
      u16,
      (u32, u32, u32),
      Vec<((u8, u8), BufferUpload)>,
    )> = vec![];
    let mut pending_renders: Vec<(u16, u16, u32, bool, Option<(u8, u8)>)> =
      vec![];
    let mut pending_render_uploads: Vec<((u8, u8), BufferUpload)> = vec![];

    macro_rules! flush_runs {
      ($self:ident) => {
        if !pending_compute.is_empty() {
          $self.encode_compute_batch(std::mem::take(&mut pending_compute));
        }
        if !pending_renders.is_empty() {
          $self.upload_bindings(&std::mem::take(&mut pending_render_uploads));
          $self.encode_render_groups(&std::mem::take(&mut pending_renders));
        }
      };
    }

    for call in draw_calls {
      match call {
        WindowEvent::ComputeShader {
          entry,
          workgroup_count,
          pre_upload,
        } => {
          if !pending_renders.is_empty() {
            flush_runs!(self);
          }
          pending_compute.push((*entry, *workgroup_count, pre_upload.clone()));
        }
        WindowEvent::RenderShaders {
          vert,
          frag,
          vert_count,
          pre_upload,
          additive,
          render_target,
        } => {
          if !pending_compute.is_empty()
            || (!pending_renders.is_empty() && !pre_upload.is_empty())
          {
            flush_runs!(self);
          }
          pending_render_uploads.extend(pre_upload.iter().cloned());
          pending_renders.push((
            *vert,
            *frag,
            *vert_count,
            *additive,
            *render_target,
          ));
        }
      }
    }
    flush_runs!(self);
  }

  /// The format screen-targeted pipelines render to: the surface's when
  /// there is one.
  fn screen_format(&self) -> wgpu::TextureFormat {
    self
      .surface_config
      .as_ref()
      .map(|c| c.format)
      .unwrap_or(GpuCore::OFFSCREEN_FORMAT)
  }

  /// The view a screen render pass draws into, and how the pass loads it:
  /// the frame's first screen pass acquires the frame's screen image and
  /// clears it, later ones draw over what is there.
  fn begin_screen_pass(
    &mut self,
  ) -> (wgpu::TextureView, wgpu::LoadOp<wgpu::Color>) {
    if self.frame_screen.is_none() {
      self.frame_screen = Some(self.acquire_frame_screen());
    }
    let screen = self.frame_screen.as_mut().unwrap();
    let load = if screen.drawn {
      wgpu::LoadOp::Load
    } else {
      wgpu::LoadOp::Clear(wgpu::Color::BLACK)
    };
    screen.drawn = true;
    (screen.view.clone(), load)
  }

  fn acquire_frame_screen(&mut self) -> FrameScreen {
    match self.screen_target {
      ScreenTarget::Surface => {
        match self
          .surface
          .as_ref()
          .expect("ScreenTarget::Surface without a surface")
          .get_current_texture()
        {
          wgpu::CurrentSurfaceTexture::Success(texture)
          | wgpu::CurrentSurfaceTexture::Suboptimal(texture) => FrameScreen {
            view: texture
              .texture
              .create_view(&wgpu::TextureViewDescriptor::default()),
            surface_texture: Some(texture),
            drawn: false,
          },
          wgpu::CurrentSurfaceTexture::Occluded
          | wgpu::CurrentSurfaceTexture::Lost
          | wgpu::CurrentSurfaceTexture::Outdated => self.stand_in_screen(),
          other => {
            eprintln!("Surface error: {other:?}");
            self.stand_in_screen()
          }
        }
      }
      ScreenTarget::Texture => {
        let (width, height) = self.window_size;
        let format = self.screen_format();
        let current = self.screen_texture.as_ref().is_some_and(|texture| {
          texture.width() == width
            && texture.height() == height
            && texture.format() == format
        });
        if !current {
          let (texture, _) = create_texture_and_view(
            &self.device,
            "screen texture",
            width,
            height,
            format,
            wgpu::TextureUsages::RENDER_ATTACHMENT
              | wgpu::TextureUsages::COPY_SRC,
          );
          self.screen_texture = Some(texture);
        }
        FrameScreen {
          surface_texture: None,
          view: self
            .screen_texture
            .as_ref()
            .unwrap()
            .create_view(&wgpu::TextureViewDescriptor::default()),
          drawn: false,
        }
      }
      ScreenTarget::StandIn => self.stand_in_screen(),
      ScreenTarget::Embedder => {
        unreachable!("the frame path skips screen draws for embedders")
      }
    }
  }

  /// A 1×1 screen image in the screen format, for frames whose screen draws
  /// have nowhere visible to go.
  fn stand_in_screen(&self) -> FrameScreen {
    let (_, view) = create_texture_and_view(
      &self.device,
      "stand-in screen",
      1,
      1,
      self.screen_format(),
      wgpu::TextureUsages::RENDER_ATTACHMENT,
    );
    FrameScreen {
      surface_texture: None,
      view,
      drawn: false,
    }
  }

  /// Ends the frame: presents its screen image, if a screen draw opened one.
  /// Returns whether the frame presented: a present means drawable
  /// acquisition (vsync) throttled the caller's loop, while an unpresented
  /// frame has no natural backpressure and must be paced explicitly.
  pub fn finish_frame(&mut self) -> bool {
    let Some(screen) = self.frame_screen.take() else {
      return false;
    };
    if let Some(surface_texture) = screen.surface_texture {
      surface_texture.present();
      return true;
    }
    if self.screen_target != ScreenTarget::Texture {
      return false;
    }
    let (Some(surface), Some(texture)) = (&self.surface, &self.screen_texture)
    else {
      return false;
    };
    let output = match surface.get_current_texture() {
      wgpu::CurrentSurfaceTexture::Success(output)
      | wgpu::CurrentSurfaceTexture::Suboptimal(output) => output,
      _ => return false,
    };
    let mut encoder =
      self
        .device
        .create_command_encoder(&wgpu::CommandEncoderDescriptor {
          label: Some("screen present encoder"),
        });
    encoder.copy_texture_to_texture(
      texture.as_image_copy(),
      output.texture.as_image_copy(),
      wgpu::Extent3d {
        width: texture.width().min(output.texture.width()),
        height: texture.height().min(output.texture.height()),
        depth_or_array_layers: 1,
      },
    );
    self.queue.submit(std::iter::once(encoder.finish()));
    output.present();
    true
  }

  /// Adds a window surface, rendering the screen to `target`.
  pub fn attach_surface(
    &mut self,
    surface: wgpu::Surface<'static>,
    config: wgpu::SurfaceConfiguration,
    target: ScreenTarget,
  ) {
    self.window_size = (config.width, config.height);
    self.surface = Some(surface);
    self.surface_config = Some(config);
    self.screen_target = target;
  }

  /// Resizes the surface (and the window size the screen texture follows).
  /// Ends any frame in progress unpresented: a surface can't be configured
  /// while one of its images is held.
  pub fn resize_surface(&mut self, width: u32, height: u32) {
    self.frame_screen = None;
    self.window_size = (width, height);
    if let Some(config) = &mut self.surface_config {
      config.width = width;
      config.height = height;
    }
    self.reconfigure_surface();
  }

  /// Reconfigures the surface with its current configuration, ending any
  /// frame in progress unpresented.
  pub fn reconfigure_surface(&mut self) {
    self.frame_screen = None;
    if let (Some(surface), Some(config)) = (&self.surface, &self.surface_config)
    {
      surface.configure(&self.device, config);
    }
  }

  /// Reads the screen texture back as RGBA8 bytes, returning `(width,
  /// height, pixels)` (`ScreenTarget::Texture` without a surface only).
  pub fn read_screen(&self) -> Option<(u32, u32, Vec<u8>)> {
    let texture = self.screen_texture.as_ref()?;
    (texture.format() == BINDING_TEXTURE_FORMAT)
      .then(|| self.read_texture_rgba8(texture))
  }

  /// Blocks until all submitted GPU work has completed.
  pub fn wait_idle(&self) {
    self
      .device
      .poll(wgpu::PollType::wait_indefinitely())
      .unwrap();
  }

  /// Texture format used for the offscreen render target and render-to-texture.
  const OFFSCREEN_FORMAT: wgpu::TextureFormat = BINDING_TEXTURE_FORMAT;

  /// Creates and caches a render pipeline for the given format (if not already cached).
  pub fn get_or_create_render_pipeline(
    &mut self,
    vert_entry: u16,
    frag_entry: u16,
    additive: bool,
    format: wgpu::TextureFormat,
  ) {
    let key = (vert_entry, frag_entry, additive, format);
    if self.render_pipelines.iter().any(|(k, _)| *k == key) {
      return;
    }
    let vert_name = self.gpu_entries[vert_entry as usize].name.clone();
    let frag_name = self.gpu_entries[frag_entry as usize].name.clone();
    let label = format!("render pipeline {vert_name}+{frag_name}");
    let bindings = self.build_pipeline_bindings(
      &label,
      &[
        (vert_entry, wgpu::ShaderStages::VERTEX),
        (frag_entry, wgpu::ShaderStages::FRAGMENT),
      ],
    );
    let layout_refs: Vec<Option<&wgpu::BindGroupLayout>> =
      bindings.layouts.iter().map(Some).collect();
    let pipeline_layout =
      self
        .device
        .create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
          label: Some(&label),
          bind_group_layouts: &layout_refs,
          immediate_size: 0,
        });
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
          label: Some(&format!("easl {label}")),
          layout: Some(&pipeline_layout),
          vertex: wgpu::VertexState {
            module: &self.shader,
            entry_point: Some(&vert_name),
            buffers: &[],
            compilation_options: Default::default(),
          },
          fragment: Some(wgpu::FragmentState {
            module: &self.shader,
            entry_point: Some(&frag_name),
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
    self
      .render_pipelines
      .push((key, CachedRenderPipeline { pipeline, bindings }));
  }

  /// Creates a new `GpuCore` from an existing wgpu device and queue.
  ///
  /// This allows sharing the device/queue with an external renderer (e.g.
  /// easl_studio's `WGPUController`) so that textures produced here can be
  /// directly consumed in the caller's render passes without cross-device
  /// copies.  A dummy `wgpu::Instance` is created internally; it is only used
  /// for hot-reload surface creation, which is not needed in the studio.
  pub fn new_from_parts(
    device: wgpu::Device,
    queue: wgpu::Queue,
    wgsl: &str,
    binding_infos: &[GpuBindingInfo],
    gpu_entries: &[GpuEntryInfo],
  ) -> Arc<RwLock<Self>> {
    Self::new_from_parts_with_backend(
      device,
      queue,
      wgsl,
      binding_infos,
      gpu_entries,
      // Backend unknown here (the embedder owns the device), so the
      // Metal-specific vertex-stage budget check is skipped; the general
      // per-pipeline limit checks still apply.
      None,
    )
  }

  fn new_from_parts_with_backend(
    device: wgpu::Device,
    queue: wgpu::Queue,
    wgsl: &str,
    binding_infos: &[GpuBindingInfo],
    gpu_entries: &[GpuEntryInfo],
    backend: Option<wgpu::Backend>,
  ) -> Arc<RwLock<Self>> {
    if let Err(message) =
      validate_binding_limits(&device.limits(), binding_infos)
    {
      panic!("easl: {message}");
    }
    // Dummy instance — the field is only used for hot-reload surface creation.
    let instance = wgpu::Instance::new(
      wgpu::InstanceDescriptor::new_without_display_handle(),
    );

    let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
      label: Some("easl shader"),
      source: wgpu::ShaderSource::Wgsl(wgsl.into()),
    });

    let binding_slots: Vec<BindingSlot> = binding_infos
      .iter()
      .map(|info| BindingSlot {
        group: info.group,
        binding: info.binding,
        name: info.name.clone(),
        kind: info.kind,
      })
      .collect();

    let mut binding_buffers: HashMap<(u8, u8), wgpu::Buffer> = HashMap::new();
    let mut binding_buffer_sizes: HashMap<(u8, u8), u64> = HashMap::new();
    let mut textures: HashMap<(u8, u8), wgpu::Texture> = HashMap::new();
    let mut texture_views: HashMap<(u8, u8), wgpu::TextureView> =
      HashMap::new();
    let mut samplers: HashMap<(u8, u8), (SamplerSettings, wgpu::Sampler)> =
      HashMap::new();
    for info in binding_infos {
      let (group, binding, kind, size) =
        (info.group, info.binding, info.kind, info.byte_size);
      let key = (group, binding);
      if kind == GpuBufferKind::Sampler {
        let settings = SamplerSettings::default();
        samplers.insert(key, (settings, create_sampler(&device, settings)));
      } else if kind == GpuBufferKind::Texture2D {
        let (texture, view) = create_texture_and_view(
          &device,
          &format!("texture g{group}b{binding}"),
          1,
          1,
          BINDING_TEXTURE_FORMAT,
          wgpu::TextureUsages::TEXTURE_BINDING
            | wgpu::TextureUsages::COPY_SRC
            | wgpu::TextureUsages::COPY_DST
            | wgpu::TextureUsages::RENDER_ATTACHMENT,
        );
        textures.insert(key, texture);
        texture_views.insert(key, view);
      } else {
        // Minimum one word — wgpu forbids zero-size buffers. Never larger:
        // WGSL derives `arrayLength` from the buffer's byte size, so
        // over-allocating a runtime-sized storage binding fabricates
        // phantom elements (uploads arrive pre-padded per binding kind).
        let alloc_size = size.max(4);
        let buffer = device.create_buffer(&wgpu::BufferDescriptor {
          label: Some(&format!("binding g{group}b{binding}")),
          size: alloc_size,
          usage: gpu_buffer_usage(kind),
          mapped_at_creation: false,
        });
        binding_buffers.insert(key, buffer);
        binding_buffer_sizes.insert(key, alloc_size);
      }
    }

    let placeholder_texture_view = {
      let (_, view) = create_texture_and_view(
        &device,
        "placeholder texture",
        1,
        1,
        wgpu::TextureFormat::Rgba8Unorm,
        wgpu::TextureUsages::TEXTURE_BINDING,
      );
      view
    };

    Arc::new(RwLock::new(GpuCore {
      instance,
      device,
      queue,
      shader,
      gpu_entries: gpu_entries.to_vec(),
      backend,
      compute_pipelines: vec![],
      render_pipelines: vec![],
      binding_slots,
      binding_buffers,
      binding_buffer_sizes,
      textures,
      texture_views,
      samplers,
      window_size: (1, 1),
      window_time: 0.0,
      window_delta_time: 0.0,
      window_frame_index: 0,
      keys_down: HashSet::new(),
      keys_just_down: HashSet::new(),
      mouse_coords: (0, 0),
      mouse_present: false,
      mouse_down: false,
      mouse_just_down: false,
      mouse_right_down: false,
      mouse_right_just_down: false,
      mouse_delta: (0., 0.),
      mouse_captured: false,
      mouse_capture_request: None,
      surface: None,
      surface_config: None,
      screen_target: ScreenTarget::Embedder,
      screen_texture: None,
      frame_screen: None,
      placeholder_texture_view,
    }))
  }

  /// Executes a batch of render shader calls, rendering screen-targeted calls
  /// (`render_target == None`) into the provided `screen_view` with
  /// `screen_format` instead of acquiring from `self.surface`.  Offscreen
  /// render targets (`render_target == Some(...)`) work as in
  /// [`execute_frame_gpu_work`].  Blocks until the GPU finishes.
  pub fn execute_render_batch_to_view(
    &mut self,
    calls: Vec<(
      u16,
      u16,
      u32,
      Vec<((u8, u8), BufferUpload)>,
      bool,
      Option<(u8, u8)>,
    )>,
    screen_view: &wgpu::TextureView,
    screen_format: wgpu::TextureFormat,
  ) {
    if calls.is_empty() {
      return;
    }
    let all_uploads: Vec<_> = calls
      .iter()
      .flat_map(|(_, _, _, u, _, _)| u.iter().cloned())
      .collect();
    self.upload_bindings(&all_uploads);

    // Pre-create all pipelines before render-pass borrows begin.
    for (vert, frag, _, _, additive, rt) in &calls {
      let format = if rt.is_none() {
        screen_format
      } else {
        Self::OFFSCREEN_FORMAT
      };
      self.get_or_create_render_pipeline(*vert, *frag, *additive, format);
    }

    let mut encoder =
      self
        .device
        .create_command_encoder(&wgpu::CommandEncoderDescriptor {
          label: Some("render-to-view encoder"),
        });

    // Group consecutive calls by render target; one render pass per group.
    let mut i = 0;
    while i < calls.len() {
      let current_rt = calls[i].5;
      let end = calls[i..]
        .iter()
        .position(|c| c.5 != current_rt)
        .map_or(calls.len(), |j| i + j);
      let group = &calls[i..end];

      let texture_target_view: Option<wgpu::TextureView> =
        current_rt.map(|rt| {
          self.textures[&rt]
            .create_view(&wgpu::TextureViewDescriptor::default())
        });

      let view = match &texture_target_view {
        Some(v) => v,
        None => screen_view,
      };
      let format = if current_rt.is_none() {
        screen_format
      } else {
        Self::OFFSCREEN_FORMAT
      };
      // Resolve each draw's pipeline before the pass borrow begins; build
      // placeholder bind-group variants for pipelines that bind the pass's
      // render target as a texture.
      let prepared: Vec<(usize, Option<Vec<wgpu::BindGroup>>)> = group
        .iter()
        .map(|(vert, frag, _, _, additive, _)| {
          let key = (*vert, *frag, *additive, format);
          let index = self
            .render_pipelines
            .iter()
            .position(|(k, _)| *k == key)
            .expect("render pipeline missing from cache");
          let bindings = &self.render_pipelines[index].1.bindings;
          let placeholder = current_rt
            .filter(|rt| bindings.used.contains_key(rt))
            .map(|rt| {
              self.create_bind_groups(
                &bindings.used,
                &bindings.layouts,
                Some(rt),
              )
            });
          (index, placeholder)
        })
        .collect();

      {
        let mut render_pass =
          encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("render-to-view pass"),
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
        for ((_, _, vert_count, _, _, _), (pipeline_index, placeholder)) in
          group.iter().zip(prepared.iter())
        {
          let cached = &self.render_pipelines[*pipeline_index].1;
          let bind_groups = placeholder
            .as_deref()
            .unwrap_or(&cached.bindings.bind_groups);
          for (group_idx, bind_group) in bind_groups.iter().enumerate() {
            render_pass.set_bind_group(group_idx as u32, bind_group, &[]);
          }
          render_pass.set_pipeline(&cached.pipeline);
          render_pass.draw(0..*vert_count, 0..1);
        }
      }

      i = end;
    }

    self.queue.submit(std::iter::once(encoder.finish()));
    self
      .device
      .poll(wgpu::PollType::wait_indefinitely())
      .unwrap();
  }

  /// Reads a GPU buffer back to CPU, blocking until done. Returns raw bytes.
  /// Reads a binding texture back from the GPU as RGBA8 bytes, returning
  /// `(width, height, pixels)`. Blocking, like `read_buffer`; used by the
  /// `save-png` builtin for textures the GPU has rendered into. Returns
  /// `None` when no texture exists at the binding.
  pub fn read_texture(
    &self,
    group: u8,
    binding: u8,
  ) -> Option<(u32, u32, Vec<u8>)> {
    Some(self.read_texture_rgba8(self.textures.get(&(group, binding))?))
  }

  /// Reads a texture in `BINDING_TEXTURE_FORMAT` back as RGBA8 bytes,
  /// returning `(width, height, pixels)`. Blocking.
  fn read_texture_rgba8(&self, texture: &wgpu::Texture) -> (u32, u32, Vec<u8>) {
    let width = texture.width();
    let height = texture.height();
    // wgpu requires bytes_per_row aligned to 256 for texture→buffer copies.
    let unpadded_bytes_per_row = width * BINDING_TEXTURE_BPP;
    let align = wgpu::COPY_BYTES_PER_ROW_ALIGNMENT;
    let padded_bytes_per_row = unpadded_bytes_per_row.div_ceil(align) * align;
    let staging = self.device.create_buffer(&wgpu::BufferDescriptor {
      label: Some("texture readback buffer"),
      size: padded_bytes_per_row as u64 * height as u64,
      usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
      mapped_at_creation: false,
    });
    let mut encoder =
      self
        .device
        .create_command_encoder(&wgpu::CommandEncoderDescriptor {
          label: Some("texture readback encoder"),
        });
    encoder.copy_texture_to_buffer(
      wgpu::TexelCopyTextureInfo {
        texture,
        mip_level: 0,
        origin: wgpu::Origin3d::ZERO,
        aspect: wgpu::TextureAspect::All,
      },
      wgpu::TexelCopyBufferInfo {
        buffer: &staging,
        layout: wgpu::TexelCopyBufferLayout {
          offset: 0,
          bytes_per_row: Some(padded_bytes_per_row),
          rows_per_image: Some(height),
        },
      },
      wgpu::Extent3d {
        width,
        height,
        depth_or_array_layers: 1,
      },
    );
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
    let mut texel_bytes =
      Vec::with_capacity((unpadded_bytes_per_row * height) as usize);
    for row in 0..height {
      let start = (row * padded_bytes_per_row) as usize;
      texel_bytes.extend_from_slice(
        &data[start..start + unpadded_bytes_per_row as usize],
      );
    }
    drop(data);
    staging.unmap();
    (width, height, rgba16float_to_rgba8(&texel_bytes))
  }

  pub fn read_buffer(&self, group: u8, binding: u8, size: u64) -> Vec<u8> {
    let staging = self.copy_binding_to_staging(group, binding, size);
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
    read_mapped_staging(staging)
  }

  /// [`Self::read_buffer`] without blocking, for platforms that can't wait
  /// on the GPU (browsers). The returned future completes once the platform
  /// runs the buffer-map callback, which a browser does on its own; native
  /// callers would have to poll the device.
  pub fn read_buffer_async(
    &self,
    group: u8,
    binding: u8,
    size: u64,
  ) -> BufferRead {
    let staging = self.copy_binding_to_staging(group, binding, size);
    let state = Arc::new(Mutex::new(BufferReadState::default()));
    let callback_state = Arc::clone(&state);
    staging
      .slice(..)
      .map_async(wgpu::MapMode::Read, move |result| {
        let mut state = callback_state.lock().unwrap();
        state.result = Some(result);
        if let Some(waker) = state.waker.take() {
          waker.wake();
        }
      });
    BufferRead {
      staging: Some(staging),
      state,
    }
  }

  /// Copies `size` bytes of a binding's buffer into a fresh mappable
  /// staging buffer.
  fn copy_binding_to_staging(
    &self,
    group: u8,
    binding: u8,
    size: u64,
  ) -> wgpu::Buffer {
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
    staging
  }

  /// Executes the rest of a frame's GPU work and ends the frame
  /// (`finish_frame`), returning whether it presented.
  pub fn render_frame(&mut self, draw_calls: &[WindowEvent]) -> bool {
    self.execute_frame_gpu_work(draw_calls);
    self.finish_frame()
  }
}

/// Reads a mapped staging buffer's contents and unmaps it.
fn read_mapped_staging(staging: wgpu::Buffer) -> Vec<u8> {
  let data = staging.slice(..).get_mapped_range();
  let bytes = data.to_vec();
  drop(data);
  staging.unmap();
  bytes
}

/// A pending [`GpuCore::read_buffer_async`]: resolves to the buffer's bytes.
pub struct BufferRead {
  staging: Option<wgpu::Buffer>,
  state: Arc<Mutex<BufferReadState>>,
}

#[derive(Default)]
struct BufferReadState {
  result: Option<Result<(), wgpu::BufferAsyncError>>,
  waker: Option<Waker>,
}

impl Future for BufferRead {
  type Output = Vec<u8>;

  fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Vec<u8>> {
    let mut state = self.state.lock().unwrap();
    match state.result.take() {
      Some(result) => {
        drop(state);
        result.expect("easl: mapping a GPU readback buffer failed");
        Poll::Ready(read_mapped_staging(
          self
            .staging
            .take()
            .expect("BufferRead polled after completion"),
        ))
      }
      None => {
        state.waker = Some(cx.waker().clone());
        Poll::Pending
      }
    }
  }
}

/// Creates a headless wgpu GPU core without a window or surface, suitable for
/// running compute shaders outside of a windowed context (e.g. in tests).
pub fn create_headless_gpu_core(
  wgsl: &str,
  binding_infos: &[GpuBindingInfo],
  gpu_entries: &[GpuEntryInfo],
) -> Arc<RwLock<GpuCore>> {
  pollster::block_on(async {
    let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
      backends: wgpu::Backends::all(),
      ..wgpu::InstanceDescriptor::new_without_display_handle()
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
    install_gpu_error_handler(&device);

    let backend = Some(adapter.get_info().backend);
    let gpu = GpuCore::new_from_parts_with_backend(
      device,
      queue,
      wgsl,
      binding_infos,
      gpu_entries,
      backend,
    );
    {
      let mut gpu = gpu.write().unwrap();
      gpu.instance = instance;
      gpu.screen_target = ScreenTarget::StandIn;
    }
    gpu
  })
}

#[cfg(not(target_arch = "wasm32"))]
struct RenderState {
  window: Arc<Window>,
  pub gpu: Arc<RwLock<GpuCore>>,
}

#[cfg(not(target_arch = "wasm32"))]
impl<'a, D: FrameDriver> App<'a, D> {
  /// Creates or reuses a window and builds the initial `RenderState`.
  ///
  /// On first run this always creates a fresh window. On hot-reload, the same
  /// `Arc<Window>` is taken from `PERSISTENT_WINDOW` so the OS window stays
  /// visible without flashing or focus changes.
  fn setup_window(&mut self, event_loop: &ActiveEventLoop) {
    // Hot-reload path: reuse the existing render state (window + surface +
    // device) from the previous run, updating only the shader and pipelines.
    // This keeps the Metal layer alive so the window never flashes.
    if let Some(state) =
      PERSISTENT_RELOAD_STATE.with(|cell| cell.borrow_mut().take())
    {
      // Only update the GPU's shader/layouts if ensure_gpu_ready hasn't already
      // done so (which happens when dispatch-compute-shader is called before
      // spawn-window). Calling update_for_reload a second time would recreate
      // unsized-array buffers (type_size=0 → alloc_size=16) regardless of their
      // actual size, wiping any data the pre-spawn-window compute just wrote.
      if self.driver.io_mut().get_gpu().is_none() {
        let wgsl = self.driver.wgsl().to_string();
        let binding_infos = self.driver.binding_infos();
        let gpu_entries = self.driver.gpu_entries();
        state.gpu.write().unwrap().update_for_reload(
          &wgsl,
          &binding_infos,
          &gpu_entries,
        );
      }
      // Reconfigure the surface in case it became outdated during the gap
      // between event loop runs (e.g. the window was resized or the display
      // changed while the event loop was not active).
      // Render pipelines were already cleared by update_for_reload above.
      state.gpu.write().unwrap().reconfigure_surface();
      // Reset per-run GPU state so the new program starts from a clean slate.
      {
        let mut gpu = state.gpu.write().unwrap();
        gpu.window_frame_index = 0;
        gpu.keys_down.clear();
        gpu.keys_just_down.clear();
        gpu.mouse_coords = (0, 0);
        gpu.mouse_present = false;
        gpu.mouse_down = false;
        gpu.mouse_just_down = false;
        gpu.mouse_right_down = false;
        gpu.mouse_right_just_down = false;
        gpu.mouse_delta = (0., 0.);
        gpu.mouse_capture_request = None;
        if gpu.mouse_captured {
          set_cursor_captured(&state.window, false);
          gpu.mouse_captured = false;
        }
      }
      // Re-show the window: macOS may have ordered it out during the
      // resign-active → become-active lifecycle that happens while the main
      // thread was busy recompiling between run_app_on_demand calls.
      state.window.set_visible(true);
      self.driver.io_mut().set_gpu(Arc::clone(&state.gpu));
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
      .or_else(|| {
        self.driver.io_mut().preferred_window_hints().map(
          |((w, h), activate)| {
            Window::default_attributes()
              .with_title("easl")
              .with_inner_size(PhysicalSize::new(w, h))
              .with_active(activate)
          },
        )
      })
      .unwrap_or_else(|| Window::default_attributes().with_title("easl"));
    let window = Arc::new(event_loop.create_window(attrs).unwrap());

    // If a headless GPU already exists (from a prior dispatch-compute-shader),
    // reuse it by adding a render surface on top. This avoids the expensive
    // GPU→CPU→GPU round-trip that would otherwise be needed to preserve
    // GPU-written buffer contents (e.g. large compute output arrays).
    let state = if let Some(existing_gpu) = self.driver.io_mut().get_gpu() {
      pollster::block_on(RenderState::from_existing_gpu(window, existing_gpu))
        .unwrap()
    } else {
      let wgsl = self.driver.wgsl().to_string();
      let binding_infos = self.driver.binding_infos();
      let gpu_entries = self.driver.gpu_entries();
      pollster::block_on(RenderState::new(
        window,
        &wgsl,
        &binding_infos,
        &gpu_entries,
      ))
      .unwrap()
    };
    // Give the interpreter's IO manager direct access to GPU resources so
    // compute dispatches can execute synchronously within eval().
    self.driver.io_mut().set_gpu(Arc::clone(&state.gpu));
    self.state = Some(state);
  }
}

#[cfg(not(target_arch = "wasm32"))]
impl<'a, D: FrameDriver> ApplicationHandler for App<'a, D> {
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

  /// The wake half of unpresented-frame pacing: when the `WaitUntil`
  /// deadline scheduled by an unpresented frame fires, request the next
  /// redraw. This is the only place that may request it — see the
  /// unpresented branch of the `RedrawRequested` handler for why
  /// requesting at schedule time would defeat the pacing.
  fn new_events(&mut self, _event_loop: &ActiveEventLoop, cause: StartCause) {
    if matches!(cause, StartCause::ResumeTimeReached { .. })
      && !self.closed
      && !self.reload
      && let Some(state) = &self.state
    {
      state.window.request_redraw();
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
          // window_frame_index is already the correct value for this frame
          // (0 on first frame); it is incremented after eval below.
        }
        match self.driver.run_frame() {
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
        if let Some(state) = &self.state {
          let mut gpu = state.gpu.write().unwrap();
          gpu.window_frame_index += 1;
          // keys_just_down / mouse_just_down track inputs since the last frame's eval.
          // Clear them now so only newly-triggered inputs show up next frame.
          gpu.keys_just_down.clear();
          gpu.mouse_just_down = false;
          gpu.mouse_right_just_down = false;
          gpu.mouse_delta = (0., 0.);
          if let Some(captured) = gpu.mouse_capture_request.take() {
            gpu.mouse_captured = set_cursor_captured(&state.window, captured);
          }
        }
        let draw_calls = self.driver.io_mut().take_frame_draw_calls();
        if let Some(state) = &mut self.state {
          let presented = state.render(&draw_calls);
          if presented {
            // A presented frame was paced by drawable acquisition (vsync
            // blocks in get_current_texture once the swapchain's drawables
            // are in flight), so the loop can rerun immediately.
            if self.last_frame_presented
              && window_delta_time > 0.002
              && window_delta_time < 0.1
            {
              self.observed_presented_interval =
                Some(Duration::from_secs_f32(window_delta_time));
            }
            event_loop.set_control_flow(ControlFlow::Poll);
            state.window.request_redraw();
          } else {
            // Nothing paced this frame: the window is occluded (wgpu's
            // Metal backend reports Occluded without touching
            // nextDrawable), or the frame had no screen-targeted draws.
            // Left unpaced, this loop spins at CPU speed, flooding the
            // GPU queue with thousands of unpresented submissions per
            // second — a standing multi-thousand command-buffer backlog
            // that starves the compositor on refocus (whole-system
            // watchdog crashes on macOS). Two explicit substitutes:
            // backpressure — wait for this frame's GPU work so at most
            // one unpresented frame is ever in flight — and cadence —
            // schedule the next frame at the display's own rate, so
            // background programs (e.g. frame loops feeding the audio
            // thread) keep running exactly as they would when visible.
            // The next redraw is requested from `new_events` when the
            // deadline fires, NOT here: `request_redraw` latches a
            // runloop wakeup (winit's `queue_redraw` calls
            // `run_loop.wakeup()`), so requesting now would make the
            // post-cycle sleep return immediately and deliver the
            // redraw at spin rate, silently defeating `WaitUntil`
            // (measured: ~90k redraws/sec vs the intended display
            // cadence).
            state.gpu.read().unwrap().wait_idle();
            let interval = unpresented_frame_interval(
              state
                .window
                .current_monitor()
                .and_then(|m| m.refresh_rate_millihertz()),
              self.observed_presented_interval,
            );
            event_loop.set_control_flow(ControlFlow::WaitUntil(now + interval));
          }
          self.last_frame_presented = presented;
        }
        // Check for hot-reload after every successful frame.
        if self.driver.io_mut().reload_requested() {
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
      WinitWindowEvent::KeyboardInput { event, .. } => {
        if let Some(state) = &self.state {
          let mut gpu = state.gpu.write().unwrap();
          // Escape always releases a captured cursor (as browsers do for
          // pointer lock), so a program can never trap the mouse.
          if event.logical_key == Key::Named(NamedKey::Escape)
            && event.state == ElementState::Pressed
            && gpu.mouse_captured
          {
            gpu.mouse_captured = set_cursor_captured(&state.window, false);
            gpu.mouse_capture_request = None;
          }
          for key in key_names(&event) {
            match event.state {
              ElementState::Pressed if !event.repeat => {
                gpu.keys_down.insert(key.clone());
                gpu.keys_just_down.insert(key);
              }
              ElementState::Released => {
                gpu.keys_down.remove(&key);
              }
              _ => {}
            }
          }
        }
      }
      WinitWindowEvent::ModifiersChanged(modifiers) => {
        // Modifier presses arrive as `KeyboardInput` events; this only
        // catches releases that happened while another window had focus.
        if let Some(state) = &self.state {
          let modifiers = modifiers.state();
          let mut gpu = state.gpu.write().unwrap();
          for (held, name) in [
            (modifiers.shift_key(), "shift"),
            (modifiers.control_key(), "ctrl"),
            (modifiers.alt_key(), "alt"),
            (modifiers.super_key(), "super"),
          ] {
            if !held {
              gpu.keys_down.remove(name);
            }
          }
        }
      }
      WinitWindowEvent::Focused(focused) => {
        if !focused && let Some(state) = &self.state {
          let mut gpu = state.gpu.write().unwrap();
          // Key releases that happen while unfocused are never delivered,
          // so drop held state rather than leave keys stuck down.
          gpu.keys_down.clear();
          gpu.mouse_down = false;
          gpu.mouse_right_down = false;
          if gpu.mouse_captured {
            gpu.mouse_captured = set_cursor_captured(&state.window, false);
          }
        }
      }
      WinitWindowEvent::CursorMoved { position, .. } => {
        if let Some(state) = &self.state {
          let mut gpu = state.gpu.write().unwrap();
          gpu.mouse_coords = (position.x as u32, position.y as u32);
        }
      }
      WinitWindowEvent::CursorEntered { .. } => {
        if let Some(state) = &self.state {
          state.gpu.write().unwrap().mouse_present = true;
        }
      }
      WinitWindowEvent::CursorLeft { .. } => {
        if let Some(state) = &self.state {
          state.gpu.write().unwrap().mouse_present = false;
        }
      }
      WinitWindowEvent::MouseInput {
        state: btn_state,
        button: MouseButton::Left,
        ..
      } => {
        if let Some(state) = &self.state {
          let mut gpu = state.gpu.write().unwrap();
          match btn_state {
            ElementState::Pressed => {
              gpu.mouse_down = true;
              gpu.mouse_just_down = true;
            }
            ElementState::Released => {
              gpu.mouse_down = false;
            }
          }
        }
      }
      WinitWindowEvent::MouseInput {
        state: btn_state,
        button: MouseButton::Right,
        ..
      } => {
        if let Some(state) = &self.state {
          let mut gpu = state.gpu.write().unwrap();
          match btn_state {
            ElementState::Pressed => {
              gpu.mouse_right_down = true;
              gpu.mouse_right_just_down = true;
            }
            ElementState::Released => {
              gpu.mouse_right_down = false;
            }
          }
        }
      }
      _ => {}
    }
  }

  fn device_event(
    &mut self,
    _event_loop: &ActiveEventLoop,
    _device_id: DeviceId,
    event: DeviceEvent,
  ) {
    // Raw motion keeps arriving while the cursor is locked in place, which
    // is what makes `mouse-delta` usable for first-person camera control.
    if let DeviceEvent::MouseMotion { delta: (dx, dy) } = event
      && let Some(state) = &self.state
      && state.window.has_focus()
    {
      let mut gpu = state.gpu.write().unwrap();
      gpu.mouse_delta.0 += dx as f32;
      gpu.mouse_delta.1 += dy as f32;
    }
  }
}

/// Hides the cursor and locks it in place (or confines it to the window on
/// platforms without locking), or undoes that. Returns whether the cursor
/// is now captured.
#[cfg(not(target_arch = "wasm32"))]
fn set_cursor_captured(window: &Window, captured: bool) -> bool {
  if captured {
    let grabbed = window
      .set_cursor_grab(CursorGrabMode::Locked)
      .or_else(|_| window.set_cursor_grab(CursorGrabMode::Confined))
      .is_ok();
    if grabbed {
      window.set_cursor_visible(false);
    }
    grabbed
  } else {
    let _ = window.set_cursor_grab(CursorGrabMode::None);
    window.set_cursor_visible(true);
    false
  }
}

/// The `key-down?` names a key event affects: the lowercase character for
/// character keys (ignoring modifiers, so shift+1 is still `"1"`), and
/// fixed names for the supported named keys.
#[cfg(not(target_arch = "wasm32"))]
fn key_names(event: &winit::event::KeyEvent) -> Vec<String> {
  #[cfg(any(
    target_os = "macos",
    target_os = "windows",
    target_os = "linux",
    target_os = "freebsd",
    target_os = "openbsd",
    target_os = "netbsd",
    target_os = "dragonfly",
  ))]
  let key = {
    use winit::platform::modifier_supplement::KeyEventExtModifierSupplement;
    event.key_without_modifiers()
  };
  #[cfg(not(any(
    target_os = "macos",
    target_os = "windows",
    target_os = "linux",
    target_os = "freebsd",
    target_os = "openbsd",
    target_os = "netbsd",
    target_os = "dragonfly",
  )))]
  let key = event.logical_key.clone();
  match key {
    Key::Character(c) => vec![c.to_lowercase()],
    Key::Named(named) => named_key_names(named)
      .iter()
      .map(|name| name.to_string())
      .collect(),
    _ => vec![],
  }
}

/// The names a named (non-character) key answers to in `key-down?`. The
/// web runtime's `KeyboardEvent.key` mapping (`web/src/lib.rs`) mirrors
/// this, so both report the same names.
#[cfg(not(target_arch = "wasm32"))]
fn named_key_names(key: NamedKey) -> &'static [&'static str] {
  match key {
    NamedKey::Space => &[" ", "space"],
    NamedKey::Shift => &["shift"],
    NamedKey::Control => &["ctrl"],
    NamedKey::Alt => &["alt"],
    NamedKey::Super | NamedKey::Meta => &["super"],
    NamedKey::Escape => &["escape"],
    NamedKey::Enter => &["enter"],
    NamedKey::Tab => &["tab"],
    NamedKey::Backspace => &["backspace"],
    NamedKey::Delete => &["delete"],
    NamedKey::ArrowUp => &["up"],
    NamedKey::ArrowDown => &["down"],
    NamedKey::ArrowLeft => &["left"],
    NamedKey::ArrowRight => &["right"],
    _ => &[],
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
    GpuBufferKind::Texture2D | GpuBufferKind::Sampler => {
      unreachable!("gpu_buffer_usage called for a {kind:?} binding")
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
    GpuBufferKind::Texture2D | GpuBufferKind::Sampler => {
      unreachable!("gpu_binding_type called for a {kind:?} binding")
    }
  }
}

/// wgpu-hal clamps Metal's vertex stage to this many buffer-argument slots
/// (`wgpu_hal::MAX_VERTEX_BUFFERS`), counting every vertex-visible layout
/// buffer — whether or not the shader uses it — plus one slot it always
/// reserves for the buffer-sizes table. This isn't exposed through
/// `wgpu::Limits`, so we encode it here; exceeding it fails at render
/// pipeline creation with an opaque internal error.
const METAL_MAX_VERTEX_STAGE_BUFFERS: u32 = 16;
const METAL_RESERVED_VERTEX_BUFFER_SLOTS: u32 = 1;

/// Validates program-wide binding-table properties against the device's
/// limits before any wgpu objects are created. Per-stage budgets are
/// per-pipeline concerns, checked in `validate_pipeline_bindings` when each
/// pipeline's (much smaller) used-binding set is known; only globally-fixed
/// properties are checked here.
fn validate_binding_limits(
  limits: &wgpu::Limits,
  binding_infos: &[GpuBindingInfo],
) -> Result<(), String> {
  let group_count = binding_infos
    .iter()
    .map(|info| info.group as u32 + 1)
    .max()
    .unwrap_or(0);
  if group_count > limits.max_bind_groups {
    return Err(format!(
      "this program's GPU bindings exceed a device limit: bind groups \
       0..{} are used, but this device supports at most {} bind groups",
      group_count - 1,
      limits.max_bind_groups,
    ));
  }
  Ok(())
}

/// Installs a panic-with-context handler for GPU errors easl failed to
/// pre-validate, so that nothing ever surfaces as a raw wgpu panic.
pub fn install_gpu_error_handler(device: &wgpu::Device) {
  device.on_uncaptured_error(std::sync::Arc::new(|error: wgpu::Error| {
    panic!(
      "easl: the GPU rejected an operation that easl did not pre-validate. \
       This is an easl bug — please report it, including the details below \
       and the program that triggered it.\n\n{error}"
    );
  }));
}

#[cfg(not(target_arch = "wasm32"))]
impl RenderState {
  async fn new(
    window: Arc<Window>,
    wgsl: &str,
    binding_infos: &[GpuBindingInfo],
    gpu_entries: &[GpuEntryInfo],
  ) -> Result<Self, String> {
    let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
      backends: wgpu::Backends::all(),
      ..wgpu::InstanceDescriptor::new_without_display_handle()
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
    install_gpu_error_handler(&device);

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

    let gpu = GpuCore::new_from_parts_with_backend(
      device,
      queue,
      wgsl,
      binding_infos,
      gpu_entries,
      Some(adapter.get_info().backend),
    );
    {
      let mut gpu = gpu.write().unwrap();
      gpu.instance = instance;
      gpu.attach_surface(surface, surface_config, ScreenTarget::Surface);
    }

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

    gpu.write().unwrap().attach_surface(
      surface,
      surface_config,
      ScreenTarget::Surface,
    );

    Ok(Self { window, gpu })
  }

  fn resize(&mut self, width: u32, height: u32) {
    if width > 0 && height > 0 {
      self.gpu.write().unwrap().resize_surface(width, height);
    }
  }

  /// Executes a frame's GPU work; see [`GpuCore::render_frame`].
  fn render(&mut self, draw_calls: &[WindowEvent]) -> bool {
    self.gpu.write().unwrap().render_frame(draw_calls)
  }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
  use super::*;

  /// Every u8 channel value must survive the upload/readback conversion
  /// pair exactly — this is what makes `save-png` of a loaded-then-uploaded
  /// image byte-identical to its source.
  #[test]
  fn rgba8_f16_roundtrip_is_exact() {
    for byte in 0..=255u8 {
      let half = f32_to_f16(byte as f32 / 255.);
      let back = (f16_to_f32(half).clamp(0., 1.) * 255.).round() as u8;
      assert_eq!(back, byte);
    }
  }

  /// The unpresented-frame pacer prefers the monitor's reported refresh
  /// rate, falls back to the cadence observed between presented frames,
  /// then to 60Hz.
  #[test]
  fn unpresented_frame_interval_fallback_chain() {
    assert_eq!(
      unpresented_frame_interval(Some(120_000), None),
      Duration::from_secs_f64(1000. / 120_000.)
    );
    assert_eq!(
      unpresented_frame_interval(Some(60_000), Some(Duration::from_millis(4))),
      Duration::from_secs_f64(1000. / 60_000.)
    );
    assert_eq!(
      unpresented_frame_interval(Some(0), Some(Duration::from_millis(4))),
      Duration::from_millis(4)
    );
    assert_eq!(
      unpresented_frame_interval(None, Some(Duration::from_millis(4))),
      Duration::from_millis(4)
    );
    assert_eq!(
      unpresented_frame_interval(None, None),
      Duration::from_micros(16_667)
    );
  }

  /// Spot-check the f16 decoder against known bit patterns.
  #[test]
  fn f16_to_f32_known_values() {
    assert_eq!(f16_to_f32(0x0000), 0.);
    assert_eq!(f16_to_f32(0x3C00), 1.);
    assert_eq!(f16_to_f32(0x3800), 0.5);
    assert_eq!(f16_to_f32(0x3400), 0.25);
    assert_eq!(f16_to_f32(0xBC00), -1.);
    // subnormal: smallest positive f16
    assert_eq!(f16_to_f32(0x0001), 5.960464477539063e-8);
    assert!(f16_to_f32(0x7C00).is_infinite());
  }
}
