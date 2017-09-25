extern crate nary;
use nary::{Engine, Scope};


fn main()
{
	let mut engine = Engine::new();
	let mut scope: Scope = Scope::new();

	if engine.eval_with_scope::<()>(&mut scope, "var x = 4 + 5").is_err()
	{
		assert!(false);
	}

	if let Ok(result) = engine.eval_with_scope::<i64>(&mut scope, "x")
	{
		println!("result: {}", result);
	}
}
