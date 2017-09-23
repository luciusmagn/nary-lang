//! Nary - an embedded scripting language for Rust

mod engine;
mod fn_register;
mod parser;

#[cfg(test)]
mod tests;

pub use engine::{Engine, Scope};
pub use fn_register::FnRegister;
