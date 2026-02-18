pub mod compiler;
pub mod format;
pub mod interpreter;
pub mod parse;
#[cfg(feature = "window")]
pub(crate) mod window;

#[derive(Debug)]
pub(crate) enum Never {}

pub use compiler::core::compile_easl_source_to_wgsl;
pub use compiler::core::get_easl_program_info;
pub use format::format_easl_source;
