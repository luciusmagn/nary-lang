extern crate nary;
use nary::{Engine, FnRegister};

#[derive(Clone)]
struct TestStruct
{
	x: i64,
}

impl TestStruct
{
	fn update(&mut self)
	{
		self.x += 1000;
	}

	fn new() -> TestStruct
	{
		TestStruct { x: 1 }
	}
}

fn main()
{
	let mut engine = Engine::new();

	engine.register_type::<TestStruct>();

	engine.register_fn("update", TestStruct::update);
	engine.register_fn("new_ts", TestStruct::new);

	if let Ok(result) = engine.eval::<TestStruct>("let x = new_ts(); x.update(); x")
	{
		println!("result: {}", result.x); // prints 1001
	}
}
