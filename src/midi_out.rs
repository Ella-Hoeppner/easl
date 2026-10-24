//! MIDI output messages: what each `send-midi-*` builtin sends, shared by
//! both CPU runtimes and the audio thread so they build identical bytes
//! and fail with identical errors. Where the messages go is up to the IO
//! manager (the main thread) or the audio driver (the audio thread); see
//! `crate::midi` for the native output.
//!
//! Values follow the input builtins' conventions: velocities, controller
//! values, and pressures are `0..1` (clamped), pitch bend is `-1..1`
//! (clamped), and note numbers, controller numbers, programs, and raw data
//! bytes are `0..127`, channels `0..15` — an integer out of range is a
//! runtime error rather than a silently masked value.

use std::fmt::Display;

/// Which message a `send-midi-*` call sends, and so how its (up to three)
/// argument words are read. Each message's arguments come in its
/// builtin's order, with a missing trailing channel read as channel 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum MidiOutKind {
  /// `[note, velocity, channel]`.
  NoteOn,
  /// `[note, channel]`.
  NoteOff,
  /// `[controller, value, channel]`.
  ControlChange,
  /// `[value, channel]`.
  PitchBend,
  /// Channel pressure: `[value, channel]`.
  Aftertouch,
  /// Polyphonic key pressure: `[note, value, channel]`.
  NoteAftertouch,
  /// `[program, channel]`.
  ProgramChange,
  /// A raw message of the given number of bytes (status first).
  Raw1,
  Raw2,
  Raw3,
}

impl MidiOutKind {
  const ALL: [MidiOutKind; 10] = [
    MidiOutKind::NoteOn,
    MidiOutKind::NoteOff,
    MidiOutKind::ControlChange,
    MidiOutKind::PitchBend,
    MidiOutKind::Aftertouch,
    MidiOutKind::NoteAftertouch,
    MidiOutKind::ProgramChange,
    MidiOutKind::Raw1,
    MidiOutKind::Raw2,
    MidiOutKind::Raw3,
  ];

  /// The message a call of builtin `name` with `arg_count` arguments sends,
  /// if `name` is a `send-midi-*` builtin.
  pub fn of_builtin(name: &str, arg_count: usize) -> Option<Self> {
    Some(match name {
      "send-midi-note-on" => MidiOutKind::NoteOn,
      "send-midi-note-off" => MidiOutKind::NoteOff,
      "send-midi-cc" => MidiOutKind::ControlChange,
      "send-midi-pitch-bend" => MidiOutKind::PitchBend,
      "send-midi-aftertouch" => MidiOutKind::Aftertouch,
      "send-midi-note-aftertouch" => MidiOutKind::NoteAftertouch,
      "send-midi-program-change" => MidiOutKind::ProgramChange,
      "send-midi-message" => match arg_count {
        1 => MidiOutKind::Raw1,
        2 => MidiOutKind::Raw2,
        _ => MidiOutKind::Raw3,
      },
      _ => return None,
    })
  }

  /// Every `send-midi-*` builtin's name.
  pub const BUILTIN_NAMES: [&'static str; 8] = [
    "send-midi-note-on",
    "send-midi-note-off",
    "send-midi-cc",
    "send-midi-pitch-bend",
    "send-midi-aftertouch",
    "send-midi-note-aftertouch",
    "send-midi-program-change",
    "send-midi-message",
  ];

  pub fn from_u16(value: u16) -> Self {
    Self::ALL[value as usize]
  }
}

/// One MIDI message: one to three bytes, status first.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MidiMessage {
  bytes: [u8; 3],
  len: u8,
}

/// Why a `send-midi-*` call couldn't build its message.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MidiOutError {
  /// An integer argument outside its range (`what` names it).
  OutOfRange {
    what: &'static str,
    value: u32,
    max: u32,
  },
  /// A raw message's status byte isn't one (below `0x80`, or above
  /// `0xFF`), or starts or ends a system-exclusive message, which can't be
  /// sent a few bytes at a time.
  InvalidStatus(u32),
  /// A raw message given a different number of data bytes than its status
  /// takes.
  WrongDataLength { status: u8, expected: u8, got: u8 },
}

impl Display for MidiOutError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      MidiOutError::OutOfRange { what, value, max } => write!(
        f,
        "MIDI {what} {value} is out of range (it must be at most {max})"
      ),
      MidiOutError::InvalidStatus(status)
        if *status == 0xF0 || *status == 0xF7 =>
      {
        write!(
          f,
          "MIDI status {status} starts or ends a system-exclusive message, \
           which `send-midi-message` can't send"
        )
      }
      MidiOutError::InvalidStatus(status) => write!(
        f,
        "{status} isn't a MIDI status byte (it must be between 128 and 255)"
      ),
      MidiOutError::WrongDataLength {
        status,
        expected,
        got,
      } => write!(
        f,
        "MIDI status {status} takes {expected} data byte{}, but {got} {} given",
        if *expected == 1 { "" } else { "s" },
        if *got == 1 { "was" } else { "were" }
      ),
    }
  }
}

/// A `0..=127` integer argument as a data byte.
fn data_byte(what: &'static str, value: u32) -> Result<u8, MidiOutError> {
  if value > 127 {
    return Err(MidiOutError::OutOfRange {
      what,
      value,
      max: 127,
    });
  }
  Ok(value as u8)
}

/// A `0..1` value as a data byte (`NaN` reads as 0).
fn unit_byte(value: f32) -> u8 {
  let value = if value.is_nan() { 0. } else { value };
  (value.clamp(0., 1.) * 127.).round() as u8
}

/// The status byte of a channel message of kind `high_nibble`.
fn channel_status(high_nibble: u8, channel: u32) -> Result<u8, MidiOutError> {
  if channel > 15 {
    return Err(MidiOutError::OutOfRange {
      what: "channel",
      value: channel,
      max: 15,
    });
  }
  Ok(high_nibble | channel as u8)
}

/// How many data bytes a message with this status takes.
fn data_length(status: u8) -> u8 {
  match status {
    0xC0..=0xDF | 0xF1 | 0xF3 => 1,
    0x80..=0xEF | 0xF2 => 2,
    _ => 0,
  }
}

impl MidiMessage {
  fn new(bytes: &[u8]) -> Self {
    let mut array = [0; 3];
    array[..bytes.len()].copy_from_slice(bytes);
    Self {
      bytes: array,
      len: bytes.len() as u8,
    }
  }

  /// The message bytes, status first.
  pub fn as_bytes(&self) -> &[u8] {
    &self.bytes[..self.len as usize]
  }

  /// The message a `kind` call with argument words `args` sends (see
  /// `MidiOutKind` for each kind's arguments; `f32` arguments are their
  /// bits).
  pub fn build(
    kind: MidiOutKind,
    args: [u32; 3],
  ) -> Result<Self, MidiOutError> {
    let float = |i: usize| f32::from_bits(args[i]);
    Ok(match kind {
      MidiOutKind::NoteOn => {
        let velocity = float(1);
        // A nonzero velocity never rounds down to 0, which would make the
        // message a note-off.
        let velocity_byte = match unit_byte(velocity) {
          0 if velocity > 0. => 1,
          byte => byte,
        };
        Self::new(&[
          channel_status(0x90, args[2])?,
          data_byte("note", args[0])?,
          velocity_byte,
        ])
      }
      // Release velocity 64: the value the MIDI spec recommends for
      // senders without release velocity.
      MidiOutKind::NoteOff => Self::new(&[
        channel_status(0x80, args[1])?,
        data_byte("note", args[0])?,
        64,
      ]),
      MidiOutKind::ControlChange => Self::new(&[
        channel_status(0xB0, args[2])?,
        data_byte("controller", args[0])?,
        unit_byte(float(1)),
      ]),
      MidiOutKind::PitchBend => {
        let value = if float(0).is_nan() { 0. } else { float(0) };
        let raw = ((value.clamp(-1., 1.) * 8192.).round() as i32 + 8192)
          .clamp(0, 16383) as u16;
        Self::new(&[
          channel_status(0xE0, args[1])?,
          (raw & 0x7F) as u8,
          (raw >> 7) as u8,
        ])
      }
      MidiOutKind::Aftertouch => {
        Self::new(&[channel_status(0xD0, args[1])?, unit_byte(float(0))])
      }
      MidiOutKind::NoteAftertouch => Self::new(&[
        channel_status(0xA0, args[2])?,
        data_byte("note", args[0])?,
        unit_byte(float(1)),
      ]),
      MidiOutKind::ProgramChange => Self::new(&[
        channel_status(0xC0, args[1])?,
        data_byte("program", args[0])?,
      ]),
      MidiOutKind::Raw1 | MidiOutKind::Raw2 | MidiOutKind::Raw3 => {
        let given = match kind {
          MidiOutKind::Raw1 => 0,
          MidiOutKind::Raw2 => 1,
          _ => 2,
        };
        let status = args[0];
        if !(0x80..=0xFF).contains(&status) || status == 0xF0 || status == 0xF7
        {
          return Err(MidiOutError::InvalidStatus(status));
        }
        let status = status as u8;
        let expected = data_length(status);
        if expected != given {
          return Err(MidiOutError::WrongDataLength {
            status,
            expected,
            got: given,
          });
        }
        let mut bytes = [status, 0, 0];
        for i in 1..=given as usize {
          bytes[i] = data_byte("data byte", args[i])?;
        }
        Self::new(&bytes[..=given as usize])
      }
    })
  }
}

/// A system-exclusive message carrying `data` (the bytes between its
/// `0xF0` and `0xF7`, each `0..127`): `send-midi-sysex`.
pub fn sysex_message(data: &[u32]) -> Result<Vec<u8>, MidiOutError> {
  let mut bytes = Vec::with_capacity(data.len() + 2);
  bytes.push(0xF0);
  for byte in data {
    bytes.push(data_byte("system-exclusive data byte", *byte)?);
  }
  bytes.push(0xF7);
  Ok(bytes)
}

impl Display for MidiMessage {
  /// The bytes in hex, e.g. `90 3c 64`.
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let bytes: Vec<String> =
      self.as_bytes().iter().map(|b| format!("{b:02x}")).collect();
    write!(f, "{}", bytes.join(" "))
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn build(kind: MidiOutKind, args: [u32; 3]) -> Vec<u8> {
    MidiMessage::build(kind, args).unwrap().as_bytes().to_vec()
  }

  #[test]
  fn channel_messages() {
    assert_eq!(
      build(MidiOutKind::NoteOn, [60, 1f32.to_bits(), 2]),
      [0x92, 60, 127]
    );
    // A tiny velocity still sounds; zero is a note-off by convention.
    assert_eq!(
      build(MidiOutKind::NoteOn, [60, 0.001f32.to_bits(), 0]),
      [0x90, 60, 1]
    );
    assert_eq!(
      build(MidiOutKind::NoteOn, [60, 0f32.to_bits(), 0]),
      [0x90, 60, 0]
    );
    assert_eq!(build(MidiOutKind::NoteOff, [60, 15, 0]), [0x8F, 60, 64]);
    assert_eq!(
      build(MidiOutKind::ControlChange, [74, 2f32.to_bits(), 0]),
      [0xB0, 74, 127]
    );
    assert_eq!(
      build(MidiOutKind::PitchBend, [0f32.to_bits(), 0, 0]),
      [0xE0, 0, 64]
    );
    assert_eq!(
      build(MidiOutKind::PitchBend, [1f32.to_bits(), 0, 0]),
      [0xE0, 0x7F, 0x7F]
    );
    assert_eq!(
      build(MidiOutKind::PitchBend, [(-1f32).to_bits(), 0, 0]),
      [0xE0, 0, 0]
    );
    assert_eq!(
      build(MidiOutKind::Aftertouch, [0.5f32.to_bits(), 1, 0]),
      [0xD1, 64]
    );
    assert_eq!(build(MidiOutKind::ProgramChange, [5, 0, 0]), [0xC0, 5]);
  }

  #[test]
  fn raw_messages() {
    assert_eq!(build(MidiOutKind::Raw1, [0xF8, 0, 0]), [0xF8]);
    assert_eq!(build(MidiOutKind::Raw3, [0x90, 60, 100]), [0x90, 60, 100]);
    assert_eq!(
      MidiMessage::build(MidiOutKind::Raw1, [0x90, 0, 0]),
      Err(MidiOutError::WrongDataLength {
        status: 0x90,
        expected: 2,
        got: 0
      })
    );
    assert_eq!(
      MidiMessage::build(MidiOutKind::Raw1, [0xF0, 0, 0]),
      Err(MidiOutError::InvalidStatus(0xF0))
    );
    assert_eq!(
      MidiMessage::build(MidiOutKind::Raw1, [60, 0, 0]),
      Err(MidiOutError::InvalidStatus(60))
    );
  }

  #[test]
  fn sysex() {
    assert_eq!(
      sysex_message(&[0, 32, 41, 2, 12, 14, 1]),
      Ok(vec![0xF0, 0, 32, 41, 2, 12, 14, 1, 0xF7])
    );
    assert_eq!(
      sysex_message(&[0xF7]),
      Err(MidiOutError::OutOfRange {
        what: "system-exclusive data byte",
        value: 0xF7,
        max: 127
      })
    );
  }

  #[test]
  fn out_of_range_integers() {
    assert_eq!(
      MidiMessage::build(MidiOutKind::NoteOff, [128, 0, 0]),
      Err(MidiOutError::OutOfRange {
        what: "note",
        value: 128,
        max: 127
      })
    );
    assert_eq!(
      MidiMessage::build(MidiOutKind::ProgramChange, [0, 16, 0]),
      Err(MidiOutError::OutOfRange {
        what: "channel",
        value: 16,
        max: 15
      })
    );
  }
}
