#[cfg(feature = "window")]
pub mod audio;
pub mod compiler;
pub mod external;
pub mod format;
pub mod interpreter;
#[cfg(all(feature = "window", not(target_arch = "wasm32")))]
pub mod midi;
#[cfg(all(feature = "window", target_arch = "wasm32"))]
#[path = "midi_web.rs"]
pub mod midi;
pub mod parse;
pub mod thread_sync;
pub mod video;
pub mod vm;
pub mod web_bundle;
#[cfg(feature = "window")]
pub mod window;

#[derive(Debug)]
pub(crate) enum Never {}

pub use compiler::core::compile_easl_file_to_target;
pub use compiler::core::compile_easl_file_to_wgsl;
pub use compiler::core::compile_easl_source_to_target;
pub use compiler::core::compile_easl_source_to_wgsl;
pub use compiler::core::get_easl_program_info;
pub use compiler::core::load_easl_program_from_file;
pub use compiler::core::load_easl_program_from_sources;
pub use compiler::program::CompilerTarget;
pub use format::format_easl_source;
