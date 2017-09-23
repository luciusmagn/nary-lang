extern crate nary;
use nary::{Engine, FnRegister};

#[derive(Clone, Debug)]
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

	println!("{:?}",
	         engine.eval::<TestStruct>("let x = [new_ts()]; x[0].update(); x[0]"));
}
