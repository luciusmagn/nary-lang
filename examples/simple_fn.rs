extern crate rhai;
use rhai::{Engine, FnRegister};

fn add(x: i32, y: i32) -> i32 {
    x + y
}

fn main() {
    let mut engine = Engine::new();

    engine.register_fn("add", add);
 
    if let Ok(result) = engine.eval("add(40, 2)").unwrap().downcast::<i32>() {
       println!("Answer: {}", *result);  // prints 42
    }
}
