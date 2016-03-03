extern crate rhai;
use rhai::Engine;

fn main() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval("40 + 2".to_string()).unwrap().downcast::<i32>() {
        println!("Answer: {}", *result);  // prints 42
    }
}