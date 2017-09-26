//! Nary - an embedded scripting language for Rust

#![allow(unknown_lints)]
#![allow(type_complexity)]
#![allow(too_many_arguments)]
#![allow(new_without_default)]
#![allow(needless_pass_by_value)]

mod engine;
mod fn_register;
mod parser;

#[cfg(test)]
mod tests;

pub use engine::{Engine, Scope};
pub use fn_register::FnRegister;
