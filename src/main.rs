use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::fmt::Display;

mod engine;
use engine::Engine;

mod fn_register;
use fn_register::FnRegister;

mod parser;

// Todo (in no particular order):
// * Doc some examples
// * String constants
// * Remove empty box values?

fn showit<T: Display>(x: &mut T) -> () {
    println!("{}", x)
}

fn main() {
    for fname in env::args().skip(1) {
        let mut engine = Engine::new();

        &(showit as fn(x: &mut i32)->()).register(&mut engine, "print");
        &(showit as fn(x: &mut i64)->()).register(&mut engine, "print");
        &(showit as fn(x: &mut u32)->()).register(&mut engine, "print");
        &(showit as fn(x: &mut u64)->()).register(&mut engine, "print");
        &(showit as fn(x: &mut f32)->()).register(&mut engine, "print");
        &(showit as fn(x: &mut f64)->()).register(&mut engine, "print");
        &(showit as fn(x: &mut bool)->()).register(&mut engine, "print");
        &(showit as fn(x: &mut String)->()).register(&mut engine, "print");

        if let Ok(mut f) = File::open(fname.clone()) {
            let mut contents = String::new();
            if let Ok(_) = f.read_to_string(&mut contents) {

                match engine.eval(contents) {
                    Ok(_) => (),
                    Err(e) => {println!("Error: {:?}", e)}
                }
            }
        }
    }
}
