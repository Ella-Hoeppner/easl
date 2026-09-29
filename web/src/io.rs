use std::sync::{Arc, RwLock};

use easl::{
  interpreter::{
    BufferUpload, EvalError, FrameDriver, IOManager, StdoutIO, WindowEvent,
  },
  window::GpuCore,
};

/// The web runtime's IO manager. GPU work and window/input queries go
/// through a `StdoutIO` holding the canvas's `GpuCore` (the page's event
/// listeners write input state into the same core the native window loop
/// does); printing goes to the browser console.
///
/// A browser can't block on the GPU, so CPU reads of GPU-written data
/// suspend the VM instead, and the runtime reads them back asynchronously
/// (see `VmRunState::AwaitingReadback`).
pub struct WebIO {
  inner: StdoutIO,
  /// Screen draws queued before a readback's flush, already past their
  /// uploads; they render with the rest of the frame's screen draws.
  deferred_screen_draws: Vec<WindowEvent>,
}

impl WebIO {
  pub fn new() -> Self {
    Self {
      inner: StdoutIO::new(),
      deferred_screen_draws: vec![],
    }
  }

  /// Executes the GPU work queued so far, so a readback sees its results.
  /// Screen draws are deferred to the end of the frame: presenting happens
  /// when the browser regains control, which awaiting the readback gives
  /// it, so drawing them now would show a partial frame.
  pub fn flush_queued_gpu_work(&mut self) {
    let events = self.inner.take_frame_draw_calls();
    let gpu = self.inner.get_gpu().expect("the web runtime has a GPU");
    gpu.write().unwrap().execute_frame_gpu_work(&events);
    self
      .deferred_screen_draws
      .extend(events.into_iter().filter_map(|event| match event {
        WindowEvent::RenderShaders {
          vert,
          frag,
          vert_count,
          additive,
          render_target: None,
          ..
        } => Some(WindowEvent::RenderShaders {
          vert,
          frag,
          vert_count,
          pre_upload: vec![],
          additive,
          render_target: None,
        }),
        _ => None,
      }));
  }
}

fn blocking_readback() -> ! {
  unreachable!("the bytecode VM suspends for readbacks on the web")
}

impl IOManager for WebIO {
  fn println(&mut self, s: &str) {
    web_sys::console::log_1(&s.into());
  }

  fn record_draw(
    &mut self,
    vert: u16,
    frag: u16,
    vert_name: &str,
    frag_name: &str,
    vert_count: u32,
    pre_upload: Vec<((u8, u8), BufferUpload)>,
    additive: bool,
    render_target: Option<(u8, u8)>,
  ) -> Result<(), EvalError> {
    self.inner.record_draw(
      vert,
      frag,
      vert_name,
      frag_name,
      vert_count,
      pre_upload,
      additive,
      render_target,
    )
  }

  fn record_compute(
    &mut self,
    entry: u16,
    entry_name: &str,
    workgroup_count: (u32, u32, u32),
    pre_upload: Vec<((u8, u8), BufferUpload)>,
  ) -> Result<(), EvalError> {
    self
      .inner
      .record_compute(entry, entry_name, workgroup_count, pre_upload)
  }

  fn take_frame_draw_calls(&mut self) -> Vec<WindowEvent> {
    let mut calls = std::mem::take(&mut self.deferred_screen_draws);
    calls.extend(self.inner.take_frame_draw_calls());
    calls
  }

  fn record_close_window(&mut self) {}

  fn sync_gpu_to_cpu(
    &mut self,
    _group: u8,
    _binding: u8,
    _size: u64,
  ) -> Option<Vec<u8>> {
    blocking_readback()
  }

  fn can_block_on_gpu(&self) -> bool {
    false
  }

  fn flush_queued_compute(&mut self) {
    blocking_readback()
  }

  fn run_spawn_window_driver<D: FrameDriver<IO = Self>>(
    _driver: &mut D,
  ) -> Result<bool, EvalError> {
    unreachable!("the web runtime drives frames from requestAnimationFrame")
  }

  fn window_size(&self) -> (u32, u32) {
    self.inner.window_size()
  }

  fn window_time(&self) -> f32 {
    self.inner.window_time()
  }

  fn window_delta_time(&self) -> f32 {
    self.inner.window_delta_time()
  }

  fn window_frame_index(&self) -> u32 {
    self.inner.window_frame_index()
  }

  fn key_down(&self, key: &str) -> bool {
    self.inner.key_down(key)
  }

  fn key_just_down(&self, key: &str) -> bool {
    self.inner.key_just_down(key)
  }

  fn mouse_coords(&self) -> (u32, u32) {
    self.inner.mouse_coords()
  }

  fn mouse_present(&self) -> bool {
    self.inner.mouse_present()
  }

  fn mouse_down(&self) -> bool {
    self.inner.mouse_down()
  }

  fn mouse_just_down(&self) -> bool {
    self.inner.mouse_just_down()
  }

  fn get_gpu(&self) -> Option<Arc<RwLock<GpuCore>>> {
    self.inner.get_gpu()
  }

  fn set_gpu(&mut self, gpu: Arc<RwLock<GpuCore>>) {
    self.inner.set_gpu(gpu)
  }

  fn get_buffer_byte_size(&self, group: u8, binding: u8) -> Option<u64> {
    self.inner.get_buffer_byte_size(group, binding)
  }
}
