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
  /// Total frame count, counted exactly when the source is opened (by
  /// decoding it the way frames are later read).
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
  use ffmpeg_sidecar::event::{
    FfmpegEvent, OutputVideoFrame, StreamTypeSpecificData,
  };

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

  /// Every decoded frame, exactly once: no frames duplicated or dropped to
  /// reach a constant rate (which `rawvideo` output would otherwise do for
  /// variable-frame-rate sources). The frame count and the decoder both use
  /// it, so frame indices agree.
  const PASSTHROUGH_FRAMES: [&str; 2] = ["-fps_mode", "passthrough"];

  fn spawn_decoder(path: &str) -> Result<Decoder, String> {
    let mut child = FfmpegCommand::new()
      .hide_banner()
      .input(path)
      .format("rawvideo")
      .pix_fmt("rgba")
      .args(PASSTHROUGH_FRAMES)
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
    // Probe metadata (dimensions) and count the frames exactly, by decoding
    // the video stream to nowhere the way the decoder reads it.
    let mut probe = FfmpegCommand::new()
      .hide_banner()
      .input(path)
      .arg("-an")
      .args(PASSTHROUGH_FRAMES)
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
    let video_stream = meta
      .input_streams
      .iter()
      .find_map(|s| match &s.type_specific_data {
        StreamTypeSpecificData::Video(v) => Some(v.clone()),
        _ => None,
      })
      .ok_or_else(|| format!("\"{path}\" has no video stream"))?;
    // The last progress report counts every frame.
    let frame_count = iter
      .filter_map(|event| match event {
        FfmpegEvent::Progress(progress) => Some(progress.frame),
        _ => None,
      })
      .last();
    let _ = probe.kill();
    let frame_count = frame_count
      .ok_or_else(|| format!("failed to count the frames of \"{path}\""))?;
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
    /// How many frames the stream actually has, by decoding it (tests).
    #[cfg(test)]
    pub fn decoded_frame_count(&self) -> u32 {
      let mut decoder = spawn_decoder(&self.path).unwrap();
      decoder.frames.by_ref().count() as u32
    }

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
        let Some(f) = decoder.frames.next() else {
          // The frame count is exact, so this is ffmpeg disagreeing with
          // itself, not a short video.
          let decoded = decoder.next_index;
          self.decoder = None;
          return Err(format!(
            "\"{}\" ended after {decoded} frames, before frame {target} of \
             the {} it was counted to have",
            self.path, self.frame_count
          ));
        };
        if decoder.next_index == target {
          pixels = Some(f.data);
        }
        decoder.next_index += 1;
      }
      let pixels = pixels.expect("decoding stopped at the target frame");
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

#[cfg(all(test, feature = "video"))]
mod tests {
  use super::backend::open;

  /// The frame count is exact: it's the number of frames the decoder
  /// produces, and a request past the end gets the last of them.
  #[test]
  fn frame_count_is_exact() {
    let path = "data/video/scrub.mp4";
    let (mut source, info) = open(path).unwrap();
    assert_eq!(info.frame_count, source.decoded_frame_count());
    let last = open(path)
      .unwrap()
      .0
      .decode_frame(info.frame_count - 1)
      .unwrap();
    let first = source.decode_frame(0).unwrap();
    assert!(
      first.rgba != last.rgba,
      "the fixture's first and last differ"
    );
    let past_end = source.decode_frame(info.frame_count + 10).unwrap();
    assert!(
      past_end.rgba == last.rgba,
      "past the end gave the wrong frame"
    );
  }
}
