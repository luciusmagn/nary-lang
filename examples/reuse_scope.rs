extern crate rhai;
use rhai::{Engine, Scope};

fn main() {
    let mut engine = Engine::new();
    let mut scope: Scope = Vec::new();

    if let Ok(_) = engine.eval_with_scope(&mut scope, "var x = 4 + 5") { } else { assert!(false); }    

    if let Ok(result) = engine.eval_with_scope(&mut scope, "x").unwrap().downcast::<i32>() {
       println!("result: {}", result);
    }
}

