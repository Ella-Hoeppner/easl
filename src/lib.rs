#![feature(never_type)]
#![feature(pattern)]
#![feature(exact_size_is_empty)]
#![feature(result_flattening)]

pub mod compiler;
pub mod format;
pub mod parse;

#[cfg(test)]
mod core_tests {}
