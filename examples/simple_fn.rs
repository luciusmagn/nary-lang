extern crate rhai;
use rhai::{Engine, FnRegister};

fn add(x: i32, y: i32) -> i32 {
    x + y
}

fn main() {
    let mut engine = Engine::new();

    engine.register_fn("add", add);
 
    if let Ok(result) = engine.eval::<i32>("add(40, 2)") {
       println!("Answer: {}", result);  // prints 42
    }
}
