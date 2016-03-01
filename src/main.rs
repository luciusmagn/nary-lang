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
// * Function in script
// * Doc some examples
// * Refactor identifer to not require inlining of clone lookup in engine
// * Remove empty box values?
// * Methods

/*
fn simple_fn(x: i32) -> bool { x == 1 }
fn simple_fn2(x: &mut i32) -> bool { x.clone() == 2 }

#[derive(Debug)]
struct TestStruct {
    x: i32
}

impl TestStruct {
    fn update(&mut self) {
        self.x += 1000;
    }

    fn new() -> TestStruct {
        TestStruct { x: 1 }
    }
}

#[derive(Debug)]
struct TestStruct2 {
    x: bool
}

impl TestStruct2 {
    fn update(&mut self) {
        self.x = true;
    }
}

fn engine_test() {
    let mut engine = Engine::new();

    &(simple_fn as fn(i32)->bool).register(&mut engine, "simple_fn"); 
    &(simple_fn2 as fn(&mut i32)->bool).register(&mut engine, "simple_fn2");
    &(TestStruct::update as fn(&mut TestStruct)->()).register(&mut engine, "update");
    &(TestStruct::new as fn()->TestStruct).register(&mut engine, "newteststruct");
    &(showit as fn(x: &mut Box<Debug>)->()).register(&mut engine, "showit");
    &(TestStruct2::update as fn(&mut TestStruct2)->()).register(&mut engine, "update");

    let mut arg : Box<Any> = Box::new(2);

    println!("Result: {:?}", engine.call_fn_1_arg("simple_fn" , &mut arg).unwrap().downcast::<bool>());
    println!("Result: {:?}", engine.call_fn_1_arg("simple_fn2", &mut arg).unwrap().downcast::<bool>());
    println!("Intentional errors: ");
    println!("  Result: {:?}", engine.call_fn_1_arg("simple_fn3", &mut arg));
    arg = Box::new("foo");    
    println!("  Result: {:?}", engine.call_fn_1_arg("simple_fn", &mut arg));

    let mut ts : Box<Any> = Box::new(TestStruct { x: 6 });
    engine.call_fn_1_arg("update" , &mut ts);
    
    let result : Result<Box<TestStruct>, Box<Any>> = ts.downcast();
    println!("TS: {:?}", result);

    let myts = engine.call_fn_0_arg("newteststruct").unwrap().downcast::<TestStruct>();
    println!("MyTS: {:?}", myts);

    let mut mybox = Box::new(Box::new(56) as Box<Debug>) as Box<Any>;
    engine.call_fn_1_arg("showit", &mut mybox);

    let mut ts2 : Box<Any> = Box::new(TestStruct2 { x: false });
    engine.call_fn_1_arg("update" , &mut ts2);

    let result2 : Result<Box<TestStruct2>, Box<Any>> = ts2.downcast();
    println!("TS2: {:?}", result2);
}
*/

fn showit<T: Display>(x: &mut T) -> () {
    println!("{}", x)
}

fn main() {
    let mut engine = Engine::new();
    &(showit as fn(x: &mut i32)->()).register(&mut engine, "print");
    &(showit as fn(x: &mut i64)->()).register(&mut engine, "print");
    &(showit as fn(x: &mut u32)->()).register(&mut engine, "print");
    &(showit as fn(x: &mut u64)->()).register(&mut engine, "print");
    &(showit as fn(x: &mut f32)->()).register(&mut engine, "print");
    &(showit as fn(x: &mut f64)->()).register(&mut engine, "print");
    &(showit as fn(x: &mut bool)->()).register(&mut engine, "print");
    &(showit as fn(x: &mut String)->()).register(&mut engine, "print");

    for fname in env::args().skip(1) {
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
