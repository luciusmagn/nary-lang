use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::fmt::Display;

extern crate rhai;
use rhai::{Engine, FnRegister};

fn showit<T: Display>(x: &mut T) -> () {
    println!("{}", x)
}

fn main() {
    for fname in env::args().skip(1) {
        let mut engine = Engine::new();

        engine.register_fn("print", showit as fn(x: &mut i32)->());
        engine.register_fn("print", showit as fn(x: &mut i64)->());
        engine.register_fn("print", showit as fn(x: &mut u32)->());
        engine.register_fn("print", showit as fn(x: &mut u64)->());
        engine.register_fn("print", showit as fn(x: &mut f32)->());
        engine.register_fn("print", showit as fn(x: &mut f64)->());
        engine.register_fn("print", showit as fn(x: &mut bool)->());
        engine.register_fn("print", showit as fn(x: &mut String)->());

        if let Ok(mut f) = File::open(fname.clone()) {
            let mut contents = String::new();
            
            if let Ok(_) = f.read_to_string(&mut contents) {
                match engine.eval::<()>(&contents) {
                    Ok(_) => (),
                    Err(e) => {println!("Error: {:?}", e)}
                }
            }
        }
    }
}

