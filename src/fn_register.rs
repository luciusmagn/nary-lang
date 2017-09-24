// TODO figure out the mess with getting function name in closure

use std::mem;
use std::any::Any;
use std::boxed::Box;

use engine::{EvalAltResult, Engine, FnType};

pub trait FnRegister<A, RetVal, Args>
{
	fn register_fn(&mut self, name: &str, f: A);
}

impl<'a, A, T, U, V, W, X, Y, Z> FnRegister<A, Z, (&'a mut T, U, V, W, X, Y)> for Engine
	where A: 'static + Fn(&mut T, U, V, W, X, Y) -> Z,
	      T: Any + Sync,
	      U: Clone + Any + Sync,
	      V: Clone + Any + Sync,
	      W: Clone + Any + Sync,
	      X: Clone + Any + Sync,
	      Y: Clone + Any + Sync,
	      Z: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn(&mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>)
		                    -> Result<Box<(Any + Sync)>, EvalAltResult>> = Box::new(move |arg1: &mut Box<(Any + Sync)>,
		                                                                   arg2: &mut Box<(Any + Sync)>,
		                                                                   arg3: &mut Box<(Any + Sync)>,
		                                                                   arg4: &mut Box<(Any + Sync)>,
		                                                                   arg5: &mut Box<(Any + Sync)>,
		                                                                   arg6: &mut Box<(Any + Sync)>| {
			let mut arg1_b: Box<Any> = unsafe { mem::transmute_copy(arg1) };
			let mut arg2_b: Box<Any> = unsafe { mem::transmute_copy(arg2) };
			let mut arg3_b: Box<Any> = unsafe { mem::transmute_copy(arg3) };
			let mut arg4_b: Box<Any> = unsafe { mem::transmute_copy(arg4) };
			let mut arg5_b: Box<Any> = unsafe { mem::transmute_copy(arg5) };
			let mut arg6_b: Box<Any> = unsafe { mem::transmute_copy(arg6) }; 

			let inside1 = (*arg1_b).downcast_mut() as Option<&mut T>;
			let inside2 = (*arg2_b).downcast_mut() as Option<&mut U>;
			let inside3 = (*arg3_b).downcast_mut() as Option<&mut V>;
			let inside4 = (*arg4_b).downcast_mut() as Option<&mut W>;
			let inside5 = (*arg5_b).downcast_mut() as Option<&mut X>;
			let inside6 = (*arg6_b).downcast_mut() as Option<&mut Y>;

			match (inside1, inside2, inside3, inside4, inside5, inside6)
			{
				(Some(b), Some(c), Some(d), Some(e), Some(f), Some(g)) =>
				{
					Ok(Box::new(fun(b, c.clone(), d.clone(), e.clone(), f.clone(), g.clone())) as Box<(Any + Sync)>)
				},
				_ => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
			}
		});

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn6(wrapped));
	}
}

impl<'a, A, T, U, V, W, X, Y, Z> FnRegister<A, Z, (&'a T, U, V, W, X, Y)> for Engine
	where A: 'static + Fn(T, U, V, W, X, Y) -> Z,
	      T: Clone + Any + Sync,
	      U: Clone + Any + Sync,
	      V: Clone + Any + Sync,
	      W: Clone + Any + Sync,
	      X: Clone + Any + Sync,
	      Y: Clone + Any + Sync,
	      Z: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn(&mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>)
		                    -> Result<Box<(Any + Sync)>, EvalAltResult>> = Box::new(move |arg1: &mut Box<(Any + Sync)>,
		                                                                   arg2: &mut Box<(Any + Sync)>,
		                                                                   arg3: &mut Box<(Any + Sync)>,
		                                                                   arg4: &mut Box<(Any + Sync)>,
		                                                                   arg5: &mut Box<(Any + Sync)>,
		                                                                   arg6: &mut Box<(Any + Sync)>| {

			let mut arg1_b: Box<Any> = unsafe { mem::transmute_copy(arg1) };
			let mut arg2_b: Box<Any> = unsafe { mem::transmute_copy(arg2) };
			let mut arg3_b: Box<Any> = unsafe { mem::transmute_copy(arg3) };
			let mut arg4_b: Box<Any> = unsafe { mem::transmute_copy(arg4) };
			let mut arg5_b: Box<Any> = unsafe { mem::transmute_copy(arg5) };
			let mut arg6_b: Box<Any> = unsafe { mem::transmute_copy(arg6) }; 
			
			let inside1 = (*arg1_b).downcast_mut() as Option<&mut T>;
			let inside2 = (*arg2_b).downcast_mut() as Option<&mut U>;
			let inside3 = (*arg3_b).downcast_mut() as Option<&mut V>;
			let inside4 = (*arg4_b).downcast_mut() as Option<&mut W>;
			let inside5 = (*arg5_b).downcast_mut() as Option<&mut X>;
			let inside6 = (*arg6_b).downcast_mut() as Option<&mut Y>;

			match (inside1, inside2, inside3, inside4, inside5, inside6)
			{
				(Some(b), Some(c), Some(d), Some(e), Some(f), Some(g)) =>
				{
					Ok(Box::new(fun(b.clone(),
					                c.clone(),
					                d.clone(),
					                e.clone(),
					                f.clone(),
					                g.clone())) as Box<(Any + Sync)>)
				},
				_ => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
			}
		});

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn6(wrapped));
	}
}

impl<'a, A, T, U, V, W, X, Y> FnRegister<A, Y, (&'a mut T, U, V, W, X)> for Engine
	where A: 'static + Fn(&mut T, U, V, W, X) -> Y,
	      T: Any + Sync,
	      U: Clone + Any + Sync,
	      V: Clone + Any + Sync,
	      W: Clone + Any + Sync,
	      X: Clone + Any + Sync,
	      Y: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn(&mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>)
		                    -> Result<Box<(Any + Sync)>, EvalAltResult>> = Box::new(move |arg1: &mut Box<(Any + Sync)>,
		                                                                   arg2: &mut Box<(Any + Sync)>,
		                                                                   arg3: &mut Box<(Any + Sync)>,
		                                                                   arg4: &mut Box<(Any + Sync)>,
		                                                                   arg5: &mut Box<(Any + Sync)>| {
			let mut arg1_b: Box<Any> = unsafe { mem::transmute_copy(arg1) };
			let mut arg2_b: Box<Any> = unsafe { mem::transmute_copy(arg2) };
			let mut arg3_b: Box<Any> = unsafe { mem::transmute_copy(arg3) };
			let mut arg4_b: Box<Any> = unsafe { mem::transmute_copy(arg4) };
			let mut arg5_b: Box<Any> = unsafe { mem::transmute_copy(arg5) };
			
			let inside1 = (*arg1_b).downcast_mut() as Option<&mut T>;
			let inside2 = (*arg2_b).downcast_mut() as Option<&mut U>;
			let inside3 = (*arg3_b).downcast_mut() as Option<&mut V>;
			let inside4 = (*arg4_b).downcast_mut() as Option<&mut W>;
			let inside5 = (*arg5_b).downcast_mut() as Option<&mut X>;

			match (inside1, inside2, inside3, inside4, inside5)
			{
				(Some(b), Some(c), Some(d), Some(e), Some(f)) =>
				{
					Ok(Box::new(fun(b, c.clone(), d.clone(), e.clone(), f.clone())) as Box<(Any + Sync)>)
				},
				_ => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
			}
		});

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn5(wrapped));
	}
}

impl<'a, A, T, U, V, W, X, Y> FnRegister<A, Y, (&'a T, U, V, W, X)> for Engine
	where A: 'static + Fn(T, U, V, W, X) -> Y,
	      T: Clone + Any + Sync,
	      U: Clone + Any + Sync,
	      V: Clone + Any + Sync,
	      W: Clone + Any + Sync,
	      X: Clone + Any + Sync,
	      Y: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn(&mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>,
		                    &mut Box<(Any + Sync)>)
		                    -> Result<Box<(Any + Sync)>, EvalAltResult>> = Box::new(move |arg1: &mut Box<(Any + Sync)>,
		                                                                   arg2: &mut Box<(Any + Sync)>,
		                                                                   arg3: &mut Box<(Any + Sync)>,
		                                                                   arg4: &mut Box<(Any + Sync)>,
		                                                                   arg5: &mut Box<(Any + Sync)>| {
			let mut arg1_b: Box<Any> = unsafe { mem::transmute_copy(arg1) };
			let mut arg2_b: Box<Any> = unsafe { mem::transmute_copy(arg2) };
			let mut arg3_b: Box<Any> = unsafe { mem::transmute_copy(arg3) };
			let mut arg4_b: Box<Any> = unsafe { mem::transmute_copy(arg4) };
			let mut arg5_b: Box<Any> = unsafe { mem::transmute_copy(arg5) };

			let inside1 = (*arg1_b).downcast_mut() as Option<&mut T>;
			let inside2 = (*arg2_b).downcast_mut() as Option<&mut U>;
			let inside3 = (*arg3_b).downcast_mut() as Option<&mut V>;
			let inside4 = (*arg4_b).downcast_mut() as Option<&mut W>;
			let inside5 = (*arg5_b).downcast_mut() as Option<&mut X>;

			match (inside1, inside2, inside3, inside4, inside5)
			{
				(Some(b), Some(c), Some(d), Some(e), Some(f)) =>
				{
					Ok(Box::new(fun(b.clone(), c.clone(), d.clone(), e.clone(), f.clone())) as Box<(Any + Sync)>)
				},
				_ => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
			}
		});

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn5(wrapped));
	}
}

impl<'a, A, T, U, V, W, X> FnRegister<A, X, (&'a mut T, U, V, W)> for Engine
	where A: 'static + Fn(&mut T, U, V, W) -> X,
	      T: Any + Sync,
	      U: Clone + Any + Sync,
	      V: Clone + Any + Sync,
	      W: Clone + Any + Sync,
	      X: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn(&mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>) -> Result<Box<(Any + Sync)>, EvalAltResult>> =
			Box::new(move |arg1: &mut Box<(Any + Sync)>, arg2: &mut Box<(Any + Sync)>, arg3: &mut Box<(Any + Sync)>, arg4: &mut Box<(Any + Sync)>| {
				let mut arg1_b: Box<Any> = unsafe { mem::transmute_copy(arg1) };
				let mut arg2_b: Box<Any> = unsafe { mem::transmute_copy(arg2) };
				let mut arg3_b: Box<Any> = unsafe { mem::transmute_copy(arg3) };
				let mut arg4_b: Box<Any> = unsafe { mem::transmute_copy(arg4) };
			
				let inside1 = (*arg1_b).downcast_mut() as Option<&mut T>;
				let inside2 = (*arg2_b).downcast_mut() as Option<&mut U>;
				let inside3 = (*arg3_b).downcast_mut() as Option<&mut V>;
				let inside4 = (*arg4_b).downcast_mut() as Option<&mut W>;

				match (inside1, inside2, inside3, inside4)
				{
					(Some(b), Some(c), Some(d), Some(e)) => Ok(Box::new(fun(b, c.clone(), d.clone(), e.clone())) as Box<(Any + Sync)>),
					_ => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
				}
			});

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn4(wrapped));
	}
}

impl<'a, A, T, U, V, W, X> FnRegister<A, X, (&'a T, U, V, W)> for Engine
	where A: 'static + Fn(T, U, V, W) -> X,
	      T: Clone + Any + Sync,
	      U: Clone + Any + Sync,
	      V: Clone + Any + Sync,
	      W: Clone + Any + Sync,
	      X: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn(&mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>) -> Result<Box<(Any + Sync)>, EvalAltResult>> =
			Box::new(move |arg1: &mut Box<(Any + Sync)>, arg2: &mut Box<(Any + Sync)>, arg3: &mut Box<(Any + Sync)>, arg4: &mut Box<(Any + Sync)>| {
				let mut arg1_b: Box<Any> = unsafe { mem::transmute_copy(arg1) };
				let mut arg2_b: Box<Any> = unsafe { mem::transmute_copy(arg2) };
				let mut arg3_b: Box<Any> = unsafe { mem::transmute_copy(arg3) };
				let mut arg4_b: Box<Any> = unsafe { mem::transmute_copy(arg4) };

				let inside1 = (*arg1_b).downcast_mut() as Option<&mut T>;
				let inside2 = (*arg2_b).downcast_mut() as Option<&mut U>;
				let inside3 = (*arg3_b).downcast_mut() as Option<&mut V>;
				let inside4 = (*arg4_b).downcast_mut() as Option<&mut W>;

				match (inside1, inside2, inside3, inside4)
				{
					(Some(b), Some(c), Some(d), Some(e)) =>
					{
						Ok(Box::new(fun(b.clone(), c.clone(), d.clone(), e.clone())) as Box<(Any + Sync)>)
					},
					_ => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
				}
			});

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn4(wrapped));
	}
}

impl<'a, A, T, U, V, W> FnRegister<A, W, (&'a mut T, U, V)> for Engine
	where A: 'static + Fn(&mut T, U, V) -> W,
	      T: Any + Sync,
	      U: Clone + Any + Sync,
	      V: Clone + Any + Sync,
	      W: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn(&mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>) -> Result<Box<(Any + Sync)>, EvalAltResult>> =
			Box::new(move |arg1: &mut Box<(Any + Sync)>, arg2: &mut Box<(Any + Sync)>, arg3: &mut Box<(Any + Sync)>| {
				let mut arg1_b: Box<Any> = unsafe { mem::transmute_copy(arg1) };
				let mut arg2_b: Box<Any> = unsafe { mem::transmute_copy(arg2) };
				let mut arg3_b: Box<Any> = unsafe { mem::transmute_copy(arg3) };

				let inside1 = (*arg1_b).downcast_mut() as Option<&mut T>;
				let inside2 = (*arg2_b).downcast_mut() as Option<&mut U>;
				let inside3 = (*arg3_b).downcast_mut() as Option<&mut V>;

				match (inside1, inside2, inside3)
				{
					(Some(b), Some(c), Some(d)) => Ok(Box::new(fun(b, c.clone(), d.clone())) as Box<(Any + Sync)>),
					_ => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
				}
			});

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn3(wrapped));
	}
}

impl<'a, A, T, U, V, W> FnRegister<A, W, (&'a T, U, V)> for Engine
	where A: 'static + Fn(T, U, V) -> W,
	      T: Clone + Any + Sync,
	      U: Clone + Any + Sync,
	      V: Clone + Any + Sync,
	      W: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn(&mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>) -> Result<Box<(Any + Sync)>, EvalAltResult>> =
			Box::new(move |arg1: &mut Box<(Any + Sync)>, arg2: &mut Box<(Any + Sync)>, arg3: &mut Box<(Any + Sync)>| {
				let mut arg1_b: Box<Any> = unsafe { mem::transmute_copy(arg1) };
				let mut arg2_b: Box<Any> = unsafe { mem::transmute_copy(arg2) };
				let mut arg3_b: Box<Any> = unsafe { mem::transmute_copy(arg3) };

				let inside1 = (*arg1_b).downcast_mut() as Option<&mut T>;
				let inside2 = (*arg2_b).downcast_mut() as Option<&mut U>;
				let inside3 = (*arg3_b).downcast_mut() as Option<&mut V>;

				match (inside1, inside2, inside3)
				{
					(Some(b), Some(c), Some(d)) => Ok(Box::new(fun(b.clone(), c.clone(), d.clone())) as Box<(Any + Sync)>),
					_ => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
				}
			});

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn3(wrapped));
	}
}

impl<'a, A, T, U, V> FnRegister<A, V, (&'a mut T, U)> for Engine
	where A: 'static + Fn(&mut T, U) -> V,
	      T: Any + Sync,
	      U: Clone + Any + Sync,
	      V: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn(&mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>) -> Result<Box<(Any + Sync)>, EvalAltResult>> =
			Box::new(move |arg1: &mut Box<(Any + Sync)>, arg2: &mut Box<(Any + Sync)>| {
				let mut arg1_b: Box<Any> = unsafe { mem::transmute_copy(arg1) };
				let mut arg2_b: Box<Any> = unsafe { mem::transmute_copy(arg2) };
				
				let inside1 = (*arg1_b).downcast_mut() as Option<&mut T>;
				let inside2 = (*arg2_b).downcast_mut() as Option<&mut U>;

				match (inside1, inside2)
				{
					(Some(b), Some(c)) => Ok(Box::new(fun(b, c.clone())) as Box<(Any + Sync)>),
					_ => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
				}
			});

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn2(wrapped));
	}
}

impl<'a, A, T, U, V> FnRegister<A, V, (&'a T, U)> for Engine
	where A: 'static + Fn(T, U) -> V,
	      T: Clone + Any + Sync,
	      U: Clone + Any + Sync,
	      V: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn(&mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>) -> Result<Box<(Any + Sync)>, EvalAltResult>> =
			Box::new(move |arg1: &mut Box<(Any + Sync)>, arg2: &mut Box<(Any + Sync)>| {
				let mut arg1_b: Box<Any> = unsafe { mem::transmute_copy(arg1) };
				let mut arg2_b: Box<Any> = unsafe { mem::transmute_copy(arg2) };

				let inside1 = (*arg1_b).downcast_mut() as Option<&mut T>;
				let inside2 = (*arg2_b).downcast_mut() as Option<&mut U>;

				match (inside1, inside2)
				{
					(Some(b), Some(c)) => Ok(Box::new(fun(b.clone(), c.clone())) as Box<(Any + Sync)>),
					_ => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
				}
			});

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn2(wrapped));
	}
}

impl<'a, A, T, U> FnRegister<A, U, (&'a mut T)> for Engine
	where A: 'static + Fn(&mut T) -> U,
	      T: Any + Sync,
	      U: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn(&mut Box<(Any + Sync)>) -> Result<Box<(Any + Sync)>, EvalAltResult>> = Box::new(move |arg: &mut Box<(Any + Sync)>| {
			let mut arg_b: Box<Any> = unsafe { mem::transmute_copy(arg) };
			let inside = (*arg_b).downcast_mut() as Option<&mut T>;
			match inside
			{
				Some(b) => Ok(Box::new(fun(b)) as Box<(Any + Sync)>),
				None => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
			}
		});

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn1(wrapped));
	}
}


impl<'a, A, T, U> FnRegister<A, U, (&'a T)> for Engine
	where A: 'static + Fn(T) -> U,
	      T: Clone + Any + Sync,
	      U: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn(&mut Box<(Any + Sync)>) -> Result<Box<(Any + Sync)>, EvalAltResult>> = Box::new(move |arg: &mut Box<(Any + Sync)>| {
			let mut arg_b: Box<Any> = unsafe { mem::transmute_copy(arg) };
			let inside = (*arg_b).downcast_mut() as Option<&mut T>;
			match inside
			{
				Some(b) => Ok(Box::new(fun(b.clone())) as Box<(Any + Sync)>),
				None => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
			}
		});

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn1(wrapped));
	}
}

impl<A, T> FnRegister<A, T, ()> for Engine
	where A: 'static + Fn() -> T,
	      T: Any + Sync
{
	fn register_fn(&mut self, name: &str, fun: A)
	{
		let wrapped: Box<Fn() -> Result<Box<(Any + Sync)>, EvalAltResult>> = Box::new(move || Ok(Box::new(fun()) as Box<(Any + Sync)>));

		let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
		(*ent).push(FnType::ExternalFn0(wrapped));
	}
}
