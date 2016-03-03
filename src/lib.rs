//! Rhai - embedded scripting for Rust


// Todo (in no particular order):
// * Doc some examples
// *   Maintaining state
// *   Overloading
// *   How it works
// * Vectors
// * Tighten parser?
// * Errors with positions?

mod engine;

mod fn_register;
mod parser;

pub use engine::Engine;
pub use fn_register::FnRegister;
