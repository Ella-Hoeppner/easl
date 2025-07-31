pub mod compiler;
pub mod format;
pub mod interpreter;
pub mod parse;

#[cfg(test)]
mod core_tests {}

#[derive(Debug)]
pub(crate) enum Never {}
