//! Host-side video-frame decoding for the `load-video` / `get-video-frame-
//! texture` builtins. Decoding is done by driving the `ffmpeg` *binary* as a
//! subprocess (`ffmpeg-sidecar`) and reading raw RGBA frames from its stdout
//! — no linking against libav*, so it works with whatever `ffmpeg` is on
//! PATH. Requires the `video` feature (and the `ffmpeg` binary at runtime).
//!
//! An easl `Video` value is just plain data — `(source-index, frame,
//! frame-count)` — so it has ordinary value semantics for free. The heavy,
//! non-copyable decoder lives here in a per-source registry keyed by the
//! source index; it's a *semantically transparent cache* (it only affects
//! how fast a frame decodes, never which frame you get). `decode_frame` is a
//! pure function of `(source, frame)`: sequential access advances the running
//! decoder; a backward jump restarts it from the start (frame-accurate, at
//! the cost of re-decoding — a keyframe-seek optimization is future work).

/// Metadata about an opened video source.
pub struct VideoInfo {
  pub width: u32,
  pub height: u32,
  /// Total frame count (estimated from duration × fps, so approximate for
  /// containers that don't store an exact count).
  pub frame_count: u32,
}

/// A decoded frame's RGBA8 pixels.
pub struct DecodedFrame {
  pub width: u32,
  pub height: u32,
  pub rgba: Vec<u8>,
}

#[cfg(feature = "video")]
mod backend {
  use super::{DecodedFrame, VideoInfo};
  use ffmpeg_sidecar::child::FfmpegChild;
  use ffmpeg_sidecar::command::FfmpegCommand;
  use ffmpeg_sidecar::event::{OutputVideoFrame, StreamTypeSpecificData};

  /// A running decoder positioned partway through a source: the `ffmpeg`
  /// child (kept for teardown) and the frame iterator (which owns the moved-
  /// out stdout/stderr, so this isn't self-referential), plus the index of
  /// the next frame the iterator will yield.
  struct Decoder {
    child: FfmpegChild,
    frames: Box<dyn Iterator<Item = OutputVideoFrame>>,
    next_index: u32,
  }

  impl Drop for Decoder {
    fn drop(&mut self) {
      let _ = self.child.kill();
    }
  }

  pub struct VideoSource {
    path: String,
    width: u32,
    height: u32,
    frame_count: u32,
    decoder: Option<Decoder>,
    /// The most recently decoded (frame index, RGBA pixels), so re-requesting
    /// the same frame is free.
    cached: Option<(u32, Vec<u8>)>,
  }

  fn spawn_decoder(path: &str) -> Result<Decoder, String> {
    let mut child = FfmpegCommand::new()
      .hide_banner()
      .input(path)
      .format("rawvideo")
      .pix_fmt("rgba")
      .arg("-")
      .spawn()
      .map_err(|e| format!("failed to spawn ffmpeg: {e}"))?;
    let frames = Box::new(
      child
        .iter()
        .map_err(|e| format!("failed to read ffmpeg output: {e}"))?
        .filter_frames(),
    );
    Ok(Decoder {
      child,
      frames,
      next_index: 0,
    })
  }

  pub fn open(path: &str) -> Result<(VideoSource, VideoInfo), String> {
    // Probe metadata (dimensions, fps, duration) with a short-lived process.
    let mut probe = FfmpegCommand::new()
      .hide_banner()
      .input(path)
      .format("null")
      .arg("-")
      .spawn()
      .map_err(|e| {
        format!("failed to spawn ffmpeg to probe \"{path}\": {e}")
      })?;
    let mut iter = probe
      .iter()
      .map_err(|e| format!("failed to probe \"{path}\": {e}"))?;
    let meta = iter
      .collect_metadata()
      .map_err(|e| format!("failed to read metadata for \"{path}\": {e}"))?;
    let duration = meta.duration().unwrap_or(0.0);
    let video_stream = meta
      .input_streams
      .iter()
      .find_map(|s| match &s.type_specific_data {
        StreamTypeSpecificData::Video(v) => Some(v.clone()),
        _ => None,
      })
      .ok_or_else(|| format!("\"{path}\" has no video stream"))?;
    let _ = iter.count(); // drain so ffmpeg exits cleanly
    let _ = probe.kill();
    let frame_count =
      ((duration * video_stream.fps as f64).round() as i64).max(1) as u32;
    Ok((
      VideoSource {
        path: path.to_string(),
        width: video_stream.width,
        height: video_stream.height,
        frame_count,
        decoder: None,
        cached: None,
      },
      VideoInfo {
        width: video_stream.width,
        height: video_stream.height,
        frame_count,
      },
    ))
  }

  impl VideoSource {
    pub fn decode_frame(&mut self, frame: u32) -> Result<DecodedFrame, String> {
      let target = frame.min(self.frame_count.saturating_sub(1));
      if let Some((cached_index, pixels)) = &self.cached
        && *cached_index == target
      {
        return Ok(DecodedFrame {
          width: self.width,
          height: self.height,
          rgba: pixels.clone(),
        });
      }
      // Restart from the beginning for a backward seek (frame-accurate; the
      // running decoder only moves forward).
      if self.decoder.as_ref().is_none_or(|d| d.next_index > target) {
        self.decoder = Some(spawn_decoder(&self.path)?);
      }
      let decoder = self.decoder.as_mut().unwrap();
      let mut pixels: Option<Vec<u8>> = None;
      while decoder.next_index <= target {
        match decoder.frames.next() {
          Some(f) => {
            let index = decoder.next_index;
            decoder.next_index += 1;
            if index == target {
              pixels = Some(f.data);
            }
          }
          // End of stream before reaching the target: fall back to the last
          // frame we did decode (or a black frame if none), and drop the
          // exhausted decoder so the next request restarts cleanly.
          None => {
            self.decoder = None;
            break;
          }
        }
      }
      let pixels = pixels
        .or_else(|| self.cached.as_ref().map(|(_, p)| p.clone()))
        .unwrap_or_else(|| vec![0u8; (self.width * self.height * 4) as usize]);
      self.cached = Some((target, pixels.clone()));
      Ok(DecodedFrame {
        width: self.width,
        height: self.height,
        rgba: pixels,
      })
    }
  }
}

/// Host-side registry of opened video sources, indexed by the `source-index`
/// stored in a `Video` value. Lives on the `EvaluationEnvironment` (main
/// thread only — a decoder can't cross to the audio thread). Present on every
/// build; the decoding methods error without the `video` feature.
#[derive(Default)]
pub struct VideoRegistry {
  #[cfg(feature = "video")]
  sources: Vec<backend::VideoSource>,
}

impl VideoRegistry {
  pub fn new() -> Self {
    Self::default()
  }

  /// Opens `path` (already resolved to an absolute/source-relative path by
  /// the caller) and returns its `(source-index, info)`.
  #[cfg(feature = "video")]
  pub fn open(&mut self, path: &str) -> Result<(u32, VideoInfo), String> {
    let (source, info) = backend::open(path)?;
    let index = self.sources.len() as u32;
    self.sources.push(source);
    Ok((index, info))
  }

  #[cfg(not(feature = "video"))]
  pub fn open(&mut self, _path: &str) -> Result<(u32, VideoInfo), String> {
    Err(
      "video support is not compiled in — rebuild with the `video` feature"
        .to_string(),
    )
  }

  #[cfg(feature = "video")]
  pub fn decode_frame(
    &mut self,
    source_index: u32,
    frame: u32,
  ) -> Result<DecodedFrame, String> {
    let source = self
      .sources
      .get_mut(source_index as usize)
      .ok_or_else(|| format!("invalid video source index {source_index}"))?;
    source.decode_frame(frame)
  }

  #[cfg(not(feature = "video"))]
  pub fn decode_frame(
    &mut self,
    _source_index: u32,
    _frame: u32,
  ) -> Result<DecodedFrame, String> {
    Err(
      "video support is not compiled in — rebuild with the `video` feature"
        .to_string(),
    )
  }
}
