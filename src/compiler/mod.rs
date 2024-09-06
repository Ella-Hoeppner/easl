pub mod core;
pub mod error;
pub mod expression;
pub mod functions;
pub mod metadata;
pub mod program;
pub mod structs;
pub mod types;
pub mod util;
pub mod vars;

pub use core::compile_tynt_to_wgsl;
