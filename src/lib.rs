#[macro_use]
extern crate log;

mod ast;
mod generator;
mod parser;
mod typechecker;

pub use generator::generate_skeleton_datafrog;
