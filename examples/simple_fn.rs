extern crate nary;
use nary::{Engine, FnRegister};

fn add(x: i64, y: i64) -> i64
{
	x + y
}

fn main()
{
	let mut engine = Engine::new();

	engine.register_fn("add", add);

	if let Ok(result) = engine.eval::<i64>("add(40, 2)")
	{
		println!("Answer: {}", result); // prints 42
	}

	let mut engine = Engine::new();

	if let Ok(result) = engine.eval::<i64>("fn add(a, b) {a+b} add(40, 2)")
	{
		println!("Answer: {}", result); // prints 42
	}
}
