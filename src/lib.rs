pub mod compiler;
pub mod format;
pub mod interpreter;
pub mod parse;
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
pub use compiler::program::CompilerTarget;
pub use format::format_easl_source;
