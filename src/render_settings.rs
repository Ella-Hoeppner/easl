//! Render settings: how a draw is rasterized and how its output combines
//! with its target. Programs change them with `set-blend` and its
//! siblings; each draw records the settings current when it's dispatched.
//! These types are the one definition of each setting's variants: the
//! builtin enums are built from `NAMES` (so a value's discriminant indexes
//! them), and both runtimes decode discriminants with `from_discriminant`.

/// Defines a render-setting enum whose first variant is its default, with
/// its variants' names in declaration (discriminant) order.
macro_rules! setting_enum {
  ($(#[$doc:meta])* $name:ident { $first:ident $(, $variant:ident)* $(,)? }) => {
    $(#[$doc])*
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
    pub enum $name {
      #[default]
      $first,
      $($variant),*
    }

    impl $name {
      /// The variants' names, in discriminant order.
      pub const NAMES: &'static [&'static str] =
        &[stringify!($first) $(, stringify!($variant))*];
      const VARIANTS: &'static [Self] = &[Self::$first $(, Self::$variant)*];

      /// The variant with declaration-order index `discriminant`.
      pub fn from_discriminant(discriminant: u32) -> Self {
        *Self::VARIANTS.get(discriminant as usize).unwrap_or_else(|| {
          panic!(
            "{} discriminant {discriminant} out of range",
            stringify!($name)
          )
        })
      }

      /// The variant's name, as easl writes it after `Enum/`.
      pub fn name(self) -> &'static str {
        Self::NAMES[Self::VARIANTS.iter().position(|v| *v == self).unwrap()]
      }
    }
  };
}

setting_enum!(
  /// What a blend component multiplies a color (the fragment's output, the
  /// "source", or the target's current color, the "destination") by before
  /// combining them. `Src`/`Dst` multiply each channel by the matching
  /// channel, `Constant` by the blend constant (`set-blend-constant`), and
  /// `SrcAlphaSaturated` by `min(src.a, 1 - dst.a)` (1 for alpha).
  BlendFactor {
    Zero,
    One,
    Src,
    OneMinusSrc,
    SrcAlpha,
    OneMinusSrcAlpha,
    Dst,
    OneMinusDst,
    DstAlpha,
    OneMinusDstAlpha,
    SrcAlphaSaturated,
    Constant,
    OneMinusConstant,
  }
);

setting_enum!(
  /// How a blend component combines the weighted source and destination.
  /// `Min` and `Max` ignore the factors (they compare the unweighted
  /// colors), as every graphics API does.
  BlendOperation {
    Add,
    Subtract,
    ReverseSubtract,
    Min,
    Max,
  }
);

/// How one part of a draw's output (its color channels, or its alpha)
/// combines with the target's: `operation(src * src_factor, dst *
/// dst_factor)`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BlendComponent {
  pub src_factor: BlendFactor,
  pub dst_factor: BlendFactor,
  pub operation: BlendOperation,
}

impl BlendComponent {
  /// Writes the source over the destination.
  pub const REPLACE: Self = Self {
    src_factor: BlendFactor::One,
    dst_factor: BlendFactor::Zero,
    operation: BlendOperation::Add,
  };

  /// The component from its easl arguments' discriminants.
  fn from_discriminants(words: &[u32]) -> Self {
    Self {
      src_factor: BlendFactor::from_discriminant(words[0]),
      dst_factor: BlendFactor::from_discriminant(words[1]),
      operation: BlendOperation::from_discriminant(words[2]),
    }
  }

  /// This component with factors that can't affect it reset: `Min` and
  /// `Max` ignore their factors (and WebGPU requires them to be `One`).
  fn normalized(self) -> Self {
    match self.operation {
      BlendOperation::Min | BlendOperation::Max => Self {
        src_factor: BlendFactor::One,
        dst_factor: BlendFactor::One,
        ..self
      },
      _ => self,
    }
  }
}

/// How a draw's output combines with what its target already holds.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Blend {
  pub color: BlendComponent,
  pub alpha: BlendComponent,
}

impl Default for Blend {
  /// No blending: a draw overwrites what the target holds. Blending costs
  /// fill rate (and on tile-based GPUs, hidden-surface removal), so it's
  /// opt-in.
  fn default() -> Self {
    Self {
      color: BlendComponent::REPLACE,
      alpha: BlendComponent::REPLACE,
    }
  }
}

impl Blend {
  /// Whether this blend writes the source over the destination unchanged,
  /// so a pipeline can skip blending altogether.
  pub fn is_replace(&self) -> bool {
    self.color.normalized() == BlendComponent::REPLACE
      && self.alpha.normalized() == BlendComponent::REPLACE
  }
}

setting_enum!(
  /// Whether a draw tests fragments against the target's depth buffer, and
  /// the comparison a fragment's depth must pass against the stored one.
  DepthTest {
    Off,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    Always,
  }
);

setting_enum!(
  /// Which triangles a draw discards by the way they face.
  CullMode { Off, Back, Front }
);

setting_enum!(
  /// The winding order of a front-facing triangle.
  FrontFace {
    CounterClockwise,
    Clockwise,
  }
);

setting_enum!(
  /// How a draw's vertices form primitives.
  Topology {
    TriangleList,
    TriangleStrip,
    LineList,
    LineStrip,
    PointList,
  }
);

/// All of a draw's render settings.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RenderSettings {
  pub blend: Blend,
  /// The color the `Constant` blend factors use, as `f32` bits.
  pub blend_constant: [u32; 4],
  pub depth_test: DepthTest,
  /// Whether fragments that pass the depth test write their depth. Only
  /// meaningful when `depth_test` isn't `Off`.
  pub depth_write: bool,
  pub cull: CullMode,
  pub front_face: FrontFace,
  pub topology: Topology,
}

impl Default for RenderSettings {
  fn default() -> Self {
    Self {
      blend: Blend::default(),
      blend_constant: [0; 4],
      depth_test: DepthTest::default(),
      depth_write: true,
      cull: CullMode::default(),
      front_face: FrontFace::default(),
      topology: Topology::default(),
    }
  }
}

impl RenderSettings {
  /// The settings a draw's pipeline is made from: these with fields that
  /// can't affect the pipeline reset, so equivalent settings compare equal
  /// (`depth_write` without a depth test, factors `Min` and `Max` ignore,
  /// and the blend constant, which is set per draw).
  pub fn normalized(self) -> Self {
    Self {
      blend: Blend {
        color: self.blend.color.normalized(),
        alpha: self.blend.alpha.normalized(),
      },
      blend_constant: [0; 4],
      depth_write: self.depth_write || self.depth_test == DepthTest::Off,
      ..self
    }
  }

  /// The blend constant as a color.
  pub fn blend_constant(&self) -> [f32; 4] {
    self.blend_constant.map(f32::from_bits)
  }

  /// Whether a draw with these settings uses its target's depth buffer.
  pub fn uses_depth(&self) -> bool {
    self.depth_test != DepthTest::Off
  }

  /// Sets one setting from its setter's arguments, as words: an enum
  /// variant's discriminant, a bool as 0 or 1, a `vec4f` as four `f32`s'
  /// bits.
  pub fn set(&mut self, setting: RenderSetting, words: &[u32]) {
    let value = words[0];
    match setting {
      RenderSetting::Blend => {
        let color = BlendComponent::from_discriminants(&words[0..3]);
        let alpha = match words.len() {
          3 => color,
          _ => BlendComponent::from_discriminants(&words[3..6]),
        };
        self.blend = Blend { color, alpha }
      }
      RenderSetting::BlendConstant => {
        self.blend_constant = words[0..4].try_into().unwrap()
      }
      RenderSetting::DepthTest => {
        self.depth_test = DepthTest::from_discriminant(value)
      }
      RenderSetting::DepthWrite => self.depth_write = value != 0,
      RenderSetting::Cull => self.cull = CullMode::from_discriminant(value),
      RenderSetting::FrontFace => {
        self.front_face = FrontFace::from_discriminant(value)
      }
      RenderSetting::Topology => {
        self.topology = Topology::from_discriminant(value)
      }
    }
  }
}

/// One render setting, as its setter names it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RenderSetting {
  Blend,
  BlendConstant,
  DepthTest,
  DepthWrite,
  Cull,
  FrontFace,
  Topology,
}

/// The type of one of a setter's arguments.
#[derive(Clone, Copy, Debug)]
pub enum SettingArgument {
  /// A builtin enum, with its variants' names.
  Enum(&'static str, &'static [&'static str]),
  Bool,
  /// A `vec4f`.
  Color,
}

const FACTOR: SettingArgument =
  SettingArgument::Enum("BlendFactor", BlendFactor::NAMES);
const OPERATION: SettingArgument =
  SettingArgument::Enum("BlendOperation", BlendOperation::NAMES);

impl RenderSetting {
  /// The setter builtins, with the setting each changes.
  pub const SETTERS: &'static [(&'static str, RenderSetting)] = &[
    ("set-blend", RenderSetting::Blend),
    ("set-blend-constant", RenderSetting::BlendConstant),
    ("set-depth-test", RenderSetting::DepthTest),
    ("set-depth-write", RenderSetting::DepthWrite),
    ("set-cull-mode", RenderSetting::Cull),
    ("set-front-face", RenderSetting::FrontFace),
    ("set-topology", RenderSetting::Topology),
  ];

  /// The builtin enums the setters take, with their variants' names in
  /// discriminant order.
  pub const ENUMS: &'static [(&'static str, &'static [&'static str])] = &[
    ("BlendFactor", BlendFactor::NAMES),
    ("BlendOperation", BlendOperation::NAMES),
    ("DepthTest", DepthTest::NAMES),
    ("CullMode", CullMode::NAMES),
    ("FrontFace", FrontFace::NAMES),
    ("Topology", Topology::NAMES),
  ];

  /// The setting the setter builtin `name` changes.
  pub fn of_setter(name: &str) -> Option<Self> {
    Self::SETTERS
      .iter()
      .find_map(|(setter, setting)| (*setter == name).then_some(*setting))
  }

  /// The argument lists the setter accepts: `set-blend` takes one blend
  /// component for both color and alpha, or one for each.
  pub fn overloads(self) -> &'static [&'static [SettingArgument]] {
    match self {
      RenderSetting::Blend => &[
        &[FACTOR, FACTOR, OPERATION],
        &[FACTOR, FACTOR, OPERATION, FACTOR, FACTOR, OPERATION],
      ],
      RenderSetting::BlendConstant => &[&[SettingArgument::Color]],
      RenderSetting::DepthWrite => &[&[SettingArgument::Bool]],
      RenderSetting::DepthTest => {
        &[&[SettingArgument::Enum("DepthTest", DepthTest::NAMES)]]
      }
      RenderSetting::Cull => {
        &[&[SettingArgument::Enum("CullMode", CullMode::NAMES)]]
      }
      RenderSetting::FrontFace => {
        &[&[SettingArgument::Enum("FrontFace", FrontFace::NAMES)]]
      }
      RenderSetting::Topology => {
        &[&[SettingArgument::Enum("Topology", Topology::NAMES)]]
      }
    }
  }

  /// How many words each of the setter's arguments takes.
  pub fn words_per_argument(self) -> usize {
    match self {
      RenderSetting::BlendConstant => 4,
      _ => 1,
    }
  }
}
