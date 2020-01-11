#[macro_use]
extern crate log;

mod ast;
mod data_structures;
mod generator;
mod generator_new;
mod parser;
mod typechecker;

pub use generator::generate_skeleton_datafrog;
pub use generator_new::generate_datafrog;
