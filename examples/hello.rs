extern crate rhai;
use rhai::Engine;

fn main() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i32>("40 + 2") {
        println!("Answer: {}", result);  // prints 42
    }
}