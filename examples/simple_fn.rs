extern crate rhai;
use rhai::{Engine, FnRegister};

fn add(x: i32, y: i32) -> i32 {
    x + y
}

fn main() {
    let mut engine = Engine::new();

    &(add as fn(x: i32, y: i32)->i32).register(&mut engine, "add");
 
    if let Ok(result) = engine.eval("add(40, 2)".to_string()).unwrap().downcast::<i32>() {
       println!("Answer: {}", *result);  // prints 42
    }
}
