pub mod core;
pub mod error;
pub mod expression;
pub mod functions;
pub mod ir;
pub mod metadata;
pub mod structs;
pub mod types;
pub mod var;
pub mod word;

pub use core::compile_tynt;
