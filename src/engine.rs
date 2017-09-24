use std::collections::HashMap;
use std::error::Error;
use std::any::Any;
use std::boxed::Box;
use std::io::Write;
use std::thread;
use std::fmt;

use parser::{lex, parse, Expr, Stmt, FnDef};
use fn_register::FnRegister;

use std::ops::{Add, Sub, Mul, Div, Rem};
use std::cmp::{Ord, Eq};

/* EVIL MAGIC HERE */

/* END OF EVIL MAGIC */

pub enum EvalAltResult
{
	ErrorFunctionNotFound(String),
	ErrorFunctionArgMismatch(String),
	ErrorFunctionCallNotSupported,
	ErrorIndexMismatch,
	ErrorIfGuardMismatch,
	ErrorVariableNotFound(String),
	ErrorFunctionArityNotSupported,
	ErrorAssignmentToUnknownLHS,
	ErrorMismatchOutputType,
	ErrorCantOpenScriptFile,
	InternalErrorMalformedDotExpression,
	LoopBreak,
	Return(Box<(Any + Sync)>),
}

// TODO
impl fmt::Debug for EvalAltResult
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
	{
		write!(f, "pain and suffering ;-;")
	}
}

impl Error for EvalAltResult
{
	fn description(&self) -> &str
	{
		"Use the Display trait implemented for EvalAltResult"
	}

	fn cause(&self) -> Option<&Error>
	{
		None
	}
}

impl fmt::Display for EvalAltResult
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
	{
		match *self
		{
			EvalAltResult::ErrorFunctionNotFound(ref n) => write!(f, "Function not found: {}", n),
			EvalAltResult::ErrorFunctionArgMismatch(ref n) => write!(f, "Function argument types do not match: {}", n),
			EvalAltResult::ErrorFunctionCallNotSupported => write!(f, "Function call with > 2 argument not supported"),
			EvalAltResult::ErrorIndexMismatch => write!(f, "Index does not match array"),
			EvalAltResult::ErrorIfGuardMismatch => write!(f, "If guards expect boolean expression"),
			EvalAltResult::ErrorVariableNotFound(ref x) => write!(f, "Variable not found: {}", x),
			EvalAltResult::ErrorFunctionArityNotSupported => write!(f, "Functions of more than 3 parameters are not yet supported"),
			EvalAltResult::ErrorAssignmentToUnknownLHS => write!(f, "Assignment to an unsupported left-hand side"),
			EvalAltResult::ErrorMismatchOutputType => write!(f, "Cast of output failed"),
			EvalAltResult::ErrorCantOpenScriptFile => write!(f, "Cannot open script file"),
			EvalAltResult::InternalErrorMalformedDotExpression => write!(f, "[Internal error] Unexpected expression in dot expression"),
			EvalAltResult::LoopBreak => write!(f, "Loop broken before completion (not an error)"),
			EvalAltResult::Return(_) => write!(f, "Function returned value (not an error)"),
		}
	}
}

pub enum FnType
{
	ExternalFn0(Box<Fn() -> Result<Box<(Any + Sync)>, EvalAltResult>>),
	ExternalFn1(Box<Fn(&mut Box<(Any + Sync)>) -> Result<Box<(Any + Sync)>, EvalAltResult>>),
	ExternalFn2(Box<Fn(&mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>) -> Result<Box<(Any + Sync)>, EvalAltResult>>),
	ExternalFn3(Box<Fn(&mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>) -> Result<Box<(Any + Sync)>, EvalAltResult>>),
	ExternalFn4(Box<Fn(&mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>, &mut Box<(Any + Sync)>) -> Result<Box<(Any + Sync)>, EvalAltResult>>),
	ExternalFn5(Box<Fn(&mut Box<(Any + Sync)>,
	                    &mut Box<(Any + Sync)>,
	                    &mut Box<(Any + Sync)>,
	                    &mut Box<(Any + Sync)>,
	                    &mut Box<(Any + Sync)>)
	                    -> Result<Box<(Any + Sync)>, EvalAltResult>>),
	ExternalFn6(Box<Fn(&mut Box<(Any + Sync)>,
	                    &mut Box<(Any + Sync)>,
	                    &mut Box<(Any + Sync)>,
	                    &mut Box<(Any + Sync)>,
	                    &mut Box<(Any + Sync)>,
	                    &mut Box<(Any + Sync)>)
	                    -> Result<Box<(Any + Sync)>, EvalAltResult>>),

	InternalFn(FnDef),
}

pub struct Engine
{
	pub fns: HashMap<String, Vec<FnType>>,
	pub threads: u64,
}

// TODO split threads from other stuff
pub type Scope = Vec<(String, Box<(Any + Sync)>)>;

impl Engine
{
	fn call_fn(&self,
	           name: &str,
	           arg1: Option<&mut Box<(Any + Sync)>>,
	           arg2: Option<&mut Box<(Any + Sync)>>,
	           arg3: Option<&mut Box<(Any + Sync)>>,
	           arg4: Option<&mut Box<(Any + Sync)>>,
	           arg5: Option<&mut Box<(Any + Sync)>>,
	           arg6: Option<&mut Box<(Any + Sync)>>)
	           -> Result<Box<(Any + Sync)>, EvalAltResult>
	{

		match self.fns.get(name)
		{
			Some(ref vf) =>
			{
				match (arg1, arg2, arg3, arg4, arg5, arg6)
				{
					(Some(ref mut a1),
					 Some(ref mut a2),
					 Some(ref mut a3),
					 Some(ref mut a4),
					 Some(ref mut a5),
					 Some(ref mut a6)) =>
					{
						for arr_f in *vf
						{
							match arr_f
							{
								&FnType::ExternalFn6(ref f) =>
								{
									match f(*a1, *a2, *a3, *a4, *a5, *a6)
									{
										Ok(v) => return Ok(v),
										_ => (),
									}
								},
								&FnType::InternalFn(ref f) =>
								{
									if f.params.len() != 6
									{
										return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
									}

									let mut new_scope: Scope = Vec::new();
									let result1 = self.call_fn("clone", Some(a1), None, None, None, None, None);
									let result2 = self.call_fn("clone", Some(a2), None, None, None, None, None);
									let result3 = self.call_fn("clone", Some(a3), None, None, None, None, None);
									let result4 = self.call_fn("clone", Some(a4), None, None, None, None, None);
									let result5 = self.call_fn("clone", Some(a5), None, None, None, None, None);
									let result6 = self.call_fn("clone", Some(a6), None, None, None, None, None);

									match (result1, result2, result3, result4, result5, result6)
									{
										(Ok(r1), Ok(r2), Ok(r3), Ok(r4), Ok(r5), Ok(r6)) =>
										{
											new_scope.push((f.params[0].clone(), r1));
											new_scope.push((f.params[1].clone(), r2));
											new_scope.push((f.params[2].clone(), r3));
											new_scope.push((f.params[3].clone(), r4));
											new_scope.push((f.params[4].clone(), r5));
											new_scope.push((f.params[5].clone(), r6));
										},
										_ => return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string())),
									}
									match self.eval_stmt(&mut new_scope, &*f.body)
									{
										Err(EvalAltResult::Return(x)) => return Ok(x),
										x => return x,
									}
								},
								_ => (),
							}
						}
						return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
					},
					(Some(ref mut a1), Some(ref mut a2), Some(ref mut a3), Some(ref mut a4), Some(ref mut a5), None) =>
					{
						for arr_f in *vf
						{
							match arr_f
							{
								&FnType::ExternalFn5(ref f) =>
								{
									match f(*a1, *a2, *a3, *a4, *a5)
									{
										Ok(v) => return Ok(v),
										_ => (),
									}
								},
								&FnType::InternalFn(ref f) =>
								{
									if f.params.len() != 5
									{
										return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
									}

									let mut new_scope: Scope = Vec::new();
									let result1 = self.call_fn("clone", Some(a1), None, None, None, None, None);
									let result2 = self.call_fn("clone", Some(a2), None, None, None, None, None);
									let result3 = self.call_fn("clone", Some(a3), None, None, None, None, None);
									let result4 = self.call_fn("clone", Some(a4), None, None, None, None, None);
									let result5 = self.call_fn("clone", Some(a5), None, None, None, None, None);

									match (result1, result2, result3, result4, result5)
									{
										(Ok(r1), Ok(r2), Ok(r3), Ok(r4), Ok(r5)) =>
										{
											new_scope.push((f.params[0].clone(), r1));
											new_scope.push((f.params[1].clone(), r2));
											new_scope.push((f.params[2].clone(), r3));
											new_scope.push((f.params[3].clone(), r4));
											new_scope.push((f.params[4].clone(), r5));
										},
										_ => return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string())),
									}
									match self.eval_stmt(&mut new_scope, &*f.body)
									{
										Err(EvalAltResult::Return(x)) => return Ok(x),
										x => return x,
									}
								},
								_ => (),
							}
						}
						return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
					},
					(Some(ref mut a1), Some(ref mut a2), Some(ref mut a3), Some(ref mut a4), None, None) =>
					{
						for arr_f in *vf
						{
							match arr_f
							{
								&FnType::ExternalFn4(ref f) =>
								{
									match f(*a1, *a2, *a3, *a4)
									{
										Ok(v) => return Ok(v),
										_ => (),
									}
								},
								&FnType::InternalFn(ref f) =>
								{
									if f.params.len() != 4
									{
										return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
									}

									let mut new_scope: Scope = Vec::new();
									let result1 = self.call_fn("clone", Some(a1), None, None, None, None, None);
									let result2 = self.call_fn("clone", Some(a2), None, None, None, None, None);
									let result3 = self.call_fn("clone", Some(a3), None, None, None, None, None);
									let result4 = self.call_fn("clone", Some(a4), None, None, None, None, None);
									match (result1, result2, result3, result4)
									{
										(Ok(r1), Ok(r2), Ok(r3), Ok(r4)) =>
										{
											new_scope.push((f.params[0].clone(), r1));
											new_scope.push((f.params[1].clone(), r2));
											new_scope.push((f.params[2].clone(), r3));
											new_scope.push((f.params[3].clone(), r4));
										},
										_ => return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string())),
									}
									match self.eval_stmt(&mut new_scope, &*f.body)
									{
										Err(EvalAltResult::Return(x)) => return Ok(x),
										x => return x,
									}
								},
								_ => (),
							}
						}
						return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
					},
					(Some(ref mut a1), Some(ref mut a2), Some(ref mut a3), None, None, None) =>
					{
						for arr_f in *vf
						{
							match arr_f
							{
								&FnType::ExternalFn3(ref f) =>
								{
									match f(*a1, *a2, *a3)
									{
										Ok(v) => return Ok(v),
										_ => (),
									}
								},
								&FnType::InternalFn(ref f) =>
								{
									if f.params.len() != 3
									{
										return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
									}

									let mut new_scope: Scope = Vec::new();
									let result1 = self.call_fn("clone", Some(a1), None, None, None, None, None);
									let result2 = self.call_fn("clone", Some(a2), None, None, None, None, None);
									let result3 = self.call_fn("clone", Some(a3), None, None, None, None, None);
									match (result1, result2, result3)
									{
										(Ok(r1), Ok(r2), Ok(r3)) =>
										{
											new_scope.push((f.params[0].clone(), r1));
											new_scope.push((f.params[1].clone(), r2));
											new_scope.push((f.params[2].clone(), r3));
										},
										_ => return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string())),
									}
									match self.eval_stmt(&mut new_scope, &*f.body)
									{
										Err(EvalAltResult::Return(x)) => return Ok(x),
										x => return x,
									}
								},
								_ => (),
							}
						}
						return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
					},
					(Some(ref mut a1), Some(ref mut a2), None, None, None, None) =>
					{
						for arr_f in *vf
						{
							match arr_f
							{
								&FnType::ExternalFn2(ref f) =>
								{
									match f(*a1, *a2)
									{
										Ok(v) => return Ok(v),
										_ => (),
									}
								},
								&FnType::InternalFn(ref f) =>
								{
									if f.params.len() != 2
									{
										return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
									}

									let mut new_scope: Scope = Vec::new();
									let result1 = self.call_fn("clone", Some(a1), None, None, None, None, None);
									let result2 = self.call_fn("clone", Some(a2), None, None, None, None, None);
									match (result1, result2)
									{
										(Ok(r1), Ok(r2)) =>
										{
											new_scope.push((f.params[0].clone(), r1));
											new_scope.push((f.params[1].clone(), r2));
										},
										_ => return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string())),
									}
									match self.eval_stmt(&mut new_scope, &*f.body)
									{
										Err(EvalAltResult::Return(x)) => return Ok(x),
										x => return x,
									}
								},
								_ => (),
							}
						}
						return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
					},
					(Some(ref mut a1), None, None, None, None, None) =>
					{
						for arr_f in *vf
						{
							match arr_f
							{
								&FnType::ExternalFn1(ref f) =>
								{
									match f(*a1)
									{
										Ok(v) => return Ok(v),
										_ => (),
									}
								},
								&FnType::InternalFn(ref f) =>
								{
									if f.params.len() != 1
									{
										return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
									}

									let mut new_scope: Scope = Vec::new();
									let result1 = self.call_fn("clone", Some(a1), None, None, None, None, None);
									match result1
									{
										Ok(r1) =>
										{
											new_scope.push((f.params[0].clone(), r1));
										},
										_ => return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string())),
									}
									match self.eval_stmt(&mut new_scope, &*f.body)
									{
										Err(EvalAltResult::Return(x)) => return Ok(x),
										x => return x,
									}
								},
								_ => (),
							}
						}
						return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
					},
					_ =>
					{
						for arr_f in *vf
						{
							match arr_f
							{
								&FnType::ExternalFn0(ref f) =>
								{
									match f()
									{
										Ok(v) => return Ok(v),
										_ => (),
									}
								},
								&FnType::InternalFn(ref f) =>
								{
									if f.params.len() != 0
									{
										return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
									}

									let mut new_scope: Scope = Vec::new();
									match self.eval_stmt(&mut new_scope, &*f.body)
									{
										Err(EvalAltResult::Return(x)) => return Ok(x),
										x => return x,
									}
								},
								_ => (),
							}
						}
						return Err(EvalAltResult::ErrorFunctionArgMismatch(name.to_string()));
					},
				}
			},
			None => Err(EvalAltResult::ErrorFunctionNotFound(name.to_string())),
		}
	}

	pub fn register_type<T: Clone + Any + Sync>(&mut self)
	{
		fn clone_helper<T: Clone>(t: T) -> T
		{
			t.clone()
		};

		self.register_fn("clone", clone_helper as fn(T) -> T);
	}

	pub fn register_get<T: Clone + Any + Sync, U: Clone + Any + Sync, F>(&mut self, name: &str, get_fn: F)
		where F: 'static + Fn(&mut T) -> U
	{
		let get_name = "get$".to_string() + name;
		self.register_fn(&get_name, get_fn);
	}

	pub fn register_set<T: Clone + Any + Sync, U: Clone + Any + Sync, F>(&mut self, name: &str, set_fn: F)
		where F: 'static + Fn(&mut T, U) -> ()
	{
		let set_name = "set$".to_string() + name;
		self.register_fn(&set_name, set_fn);
	}

	pub fn register_get_set<T: Clone + Any + Sync, U: Clone + Any + Sync, F, G>(&mut self, name: &str, get_fn: F, set_fn: G)
		where F: 'static + Fn(&mut T) -> U,
		      G: 'static + Fn(&mut T, U) -> ()
	{
		self.register_get(name, get_fn);
		self.register_set(name, set_fn);
	}

	fn get_dot_val_helper(&self, scope: &mut Scope, this_ptr: &mut Box<(Any + Sync)>, dot_rhs: &Expr) -> Result<Box<(Any + Sync)>, EvalAltResult>
	{
		match *dot_rhs
		{
			Expr::FnCall(ref fn_name, ref args) =>
			{
				if args.len() == 0
				{
					return self.call_fn(&fn_name, Some(this_ptr), None, None, None, None, None);
				}
				else if args.len() == 1
				{
					let mut arg = self.eval_expr(scope, &args[0])?;

					return self.call_fn(&fn_name,
					                    Some(this_ptr),
					                    Some(&mut arg),
					                    None,
					                    None,
					                    None,
					                    None);
				}
				else if args.len() == 2
				{
					let mut arg1 = self.eval_expr(scope, &args[0])?;
					let mut arg2 = self.eval_expr(scope, &args[1])?;

					return self.call_fn(&fn_name,
					                    Some(this_ptr),
					                    Some(&mut arg1),
					                    Some(&mut arg2),
					                    None,
					                    None,
					                    None);
				}
				else if args.len() == 3
				{
					let mut arg1 = self.eval_expr(scope, &args[0])?;
					let mut arg2 = self.eval_expr(scope, &args[1])?;
					let mut arg3 = self.eval_expr(scope, &args[2])?;

					return self.call_fn(&fn_name,
					                    Some(this_ptr),
					                    Some(&mut arg1),
					                    Some(&mut arg2),
					                    Some(&mut arg3),
					                    None,
					                    None);
				}
				else if args.len() == 4
				{
					let mut arg1 = self.eval_expr(scope, &args[0])?;
					let mut arg2 = self.eval_expr(scope, &args[1])?;
					let mut arg3 = self.eval_expr(scope, &args[2])?;
					let mut arg4 = self.eval_expr(scope, &args[3])?;

					return self.call_fn(&fn_name,
					                    Some(this_ptr),
					                    Some(&mut arg1),
					                    Some(&mut arg2),
					                    Some(&mut arg3),
					                    Some(&mut arg4),
					                    None);
				}
				else if args.len() == 5
				{
					let mut arg1 = self.eval_expr(scope, &args[0])?;
					let mut arg2 = self.eval_expr(scope, &args[1])?;
					let mut arg3 = self.eval_expr(scope, &args[2])?;
					let mut arg4 = self.eval_expr(scope, &args[3])?;
					let mut arg5 = self.eval_expr(scope, &args[4])?;

					return self.call_fn(&fn_name,
					                    Some(this_ptr),
					                    Some(&mut arg1),
					                    Some(&mut arg2),
					                    Some(&mut arg3),
					                    Some(&mut arg4),
					                    Some(&mut arg5));
				}
				else
				{
					Err(EvalAltResult::ErrorFunctionCallNotSupported)
				}
			},
			Expr::Identifier(ref id) =>
			{
				let get_fn_name = "get$".to_string() + id;
				return self.call_fn(&get_fn_name, Some(this_ptr), None, None, None, None, None);
			},
			Expr::Index(ref id, ref idx_raw) =>
			{
				let idx = self.eval_expr(scope, idx_raw)?;

				let get_fn_name = "get$".to_string() + id;

				if let Ok(mut val) = self.call_fn(&get_fn_name, Some(this_ptr), None, None, None, None, None)
				{
					let d: Box<Any> = idx;
					if let Ok(i) = d.downcast::<i64>()
					{
						let val_b: Box<Any> = val;
						if let Some(arr_typed) = (*val_b).downcast_mut() as Option<&mut Vec<Box<(Any + Sync)>>>
						{
							return self.call_fn("clone",
							                    Some(&mut arr_typed[*i as usize]),
							                    None,
							                    None,
							                    None,
							                    None,
							                    None);
						}
						else
						{
							return Err(EvalAltResult::ErrorIndexMismatch);
						}
					}
					else
					{
						return Err(EvalAltResult::ErrorIndexMismatch);
					}
				}
				else
				{
					return Err(EvalAltResult::ErrorIndexMismatch);
				}
			},
			Expr::Dot(ref inner_lhs, ref inner_rhs) =>
			{
				match **inner_lhs
				{
					Expr::Identifier(ref id) =>
					{
						let get_fn_name = "get$".to_string() + id;
						let result = self.call_fn(&get_fn_name, Some(this_ptr), None, None, None, None, None);

						match result
						{
							Ok(mut v) => return self.get_dot_val_helper(scope, &mut v, inner_rhs),
							e => return e,
						}
					},
					_ => Err(EvalAltResult::InternalErrorMalformedDotExpression),
				}
			},
			_ => Err(EvalAltResult::InternalErrorMalformedDotExpression),
		}
	}

	fn get_dot_val(&self, scope: &mut Scope, dot_lhs: &Expr, dot_rhs: &Expr) -> Result<Box<(Any + Sync)>, EvalAltResult>
	{
		match *dot_lhs
		{
			Expr::Identifier(ref id) =>
			{
				let mut target: Option<Box<(Any + Sync)>> = None;

				for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev()
				{
					if *id == *name
					{
						let result = self.call_fn("clone", Some(val), None, None, None, None, None);

						if let Ok(clone) = result
						{
							target = Some(clone);
							break;
						}
						else
						{
							return result;
						}
					}
				}

				if let Some(mut t) = target
				{
					let result = self.get_dot_val_helper(scope, &mut t, dot_rhs);

					for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev()
					{
						if *id == *name
						{
							*val = t;
							break;
						}
					}
					return result;
				}

				return Err(EvalAltResult::ErrorVariableNotFound(id.clone()));
			},
			Expr::Index(ref id, ref idx_raw) =>
			{
				let idx_boxed = self.eval_expr(scope, idx_raw)?;
				let idx_b: Box<Any> = idx_boxed;
				let idx = if let Ok(i) = idx_b.downcast::<i64>()
				{
					i
				}
				else
				{
					return Err(EvalAltResult::ErrorIndexMismatch);
				};

				let mut target: Option<Box<(Any + Sync)>> = None;

				for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev()
				{
					if *id == *name
					{
						let mut val_b: Box<Any> = *val;
						if let Some(arr_typed) = (*val_b).downcast_mut() as Option<&mut Vec<Box<(Any + Sync)>>>
						{
							let result = self.call_fn("clone",
							                          Some(&mut arr_typed[*idx as usize]),
							                          None,
							                          None,
							                          None,
							                          None,
							                          None);

							if let Ok(clone) = result
							{
								target = Some(clone);
								break;
							}
							else
							{
								return result;
							}
						}
						else
						{
							return Err(EvalAltResult::ErrorIndexMismatch);
						}
					}
				}

				if let Some(mut t) = target
				{
					let result = self.get_dot_val_helper(scope, &mut t, dot_rhs);
					for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev()
					{
						if *id == *name
						{
							let mut val_b: Box<Any> = *val;
							if let Some(arr_typed) = (*val_b).downcast_mut() as Option<&mut Vec<Box<(Any + Sync)>>>
							{
								arr_typed[*idx as usize] = t;
								break;
							}
						}
					}
					return result;
				}

				Err(EvalAltResult::ErrorVariableNotFound(id.clone()))
			},
			_ => Err(EvalAltResult::InternalErrorMalformedDotExpression),
		}
	}

	fn set_dot_val_helper(&self,
	                      this_ptr: &mut Box<(Any + Sync)>,
	                      dot_rhs: &Expr,
	                      mut source_val: Box<(Any + Sync)>)
	                      -> Result<Box<(Any + Sync)>, EvalAltResult>
	{
		match *dot_rhs
		{
			Expr::Identifier(ref id) =>
			{
				let set_fn_name = "set$".to_string() + id;
				self.call_fn(&set_fn_name,
				             Some(this_ptr),
				             Some(&mut source_val),
				             None,
				             None,
				             None,
				             None)
			},
			Expr::Dot(ref inner_lhs, ref inner_rhs) =>
			{
				match **inner_lhs
				{
					Expr::Identifier(ref id) =>
					{
						let get_fn_name = "get$".to_string() + id;
						let result = self.call_fn(&get_fn_name, Some(this_ptr), None, None, None, None, None);

						match result
						{
							Ok(mut v) =>
							{
								match self.set_dot_val_helper(&mut v, inner_rhs, source_val)
								{
									Ok(_) =>
									{
										let set_fn_name = "set$".to_string() + id;

										self.call_fn(&set_fn_name,
										             Some(this_ptr),
										             Some(&mut v),
										             None,
										             None,
										             None,
										             None)
									},
									e => e,
								}
							},
							e => e,
						}

					},
					_ => Err(EvalAltResult::InternalErrorMalformedDotExpression),
				}
			},
			_ => Err(EvalAltResult::InternalErrorMalformedDotExpression),
		}
	}

	fn set_dot_val(&self,
	               scope: &mut Scope,
	               dot_lhs: &Expr,
	               dot_rhs: &Expr,
	               source_val: Box<(Any + Sync)>)
	               -> Result<Box<(Any + Sync)>, EvalAltResult>
	{
		match *dot_lhs
		{
			Expr::Identifier(ref id) =>
			{
				let mut target: Option<Box<(Any + Sync)>> = None;

				for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev()
				{
					if *id == *name
					{
						if let Ok(clone) = self.call_fn("clone", Some(val), None, None, None, None, None)
						{
							target = Some(clone);
							break;
						}
						else
						{
							return Err(EvalAltResult::ErrorVariableNotFound(id.clone()));
						}
					}
				}

				if let Some(mut t) = target
				{
					let result = self.set_dot_val_helper(&mut t, dot_rhs, source_val);

					for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev()
					{
						if *id == *name
						{
							*val = t;
							break;
						}
					}
					return result;
				}

				return Err(EvalAltResult::ErrorAssignmentToUnknownLHS);
			},
			Expr::Index(ref id, ref idx_raw) =>
			{
				let idx_boxed = self.eval_expr(scope, idx_raw)?;
				let mut idx_b: Box<Any> = idx_boxed;
				let idx = if let Ok(i) = idx_b.downcast::<i64>()
				{
					i
				}
				else
				{
					return Err(EvalAltResult::ErrorIndexMismatch);
				};

				let mut target: Option<Box<(Any + Sync)>> = None;

				for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev()
				{
					if *id == *name
					{
						let mut val_b: Box<Any> = *val;
						if let Some(arr_typed) = (*val_b).downcast_mut() as Option<&mut Vec<Box<(Any + Sync)>>>
						{
							let result = self.call_fn("clone",
							                          Some(&mut arr_typed[*idx as usize]),
							                          None,
							                          None,
							                          None,
							                          None,
							                          None);

							if let Ok(clone) = result
							{
								target = Some(clone);
								break;
							}
							else
							{
								return result;
							}
						}
						else
						{
							return Err(EvalAltResult::ErrorIndexMismatch);
						}
					}
				}

				if let Some(mut t) = target
				{
					let result = self.set_dot_val_helper(&mut t, dot_rhs, source_val);
					for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev()
					{
						if *id == *name
						{
							let mut val_b: Box<Any> = *val;
							if let Some(arr_typed) = (*val_b).downcast_mut() as Option<&mut Vec<Box<(Any + Sync)>>>
							{
								arr_typed[*idx as usize] = t;
								break;
							}
						}
					}
					return result;
				}

				Err(EvalAltResult::ErrorVariableNotFound(id.clone()))
			},
			_ => Err(EvalAltResult::InternalErrorMalformedDotExpression),
		}
	}

	fn eval_expr(&self, scope: &mut Scope, expr: &Expr) -> Result<Box<(Any + Sync)>, EvalAltResult>
	{
		match *expr
		{
			Expr::IntConst(i) => Ok(Box::new(i)),
			Expr::StringConst(ref s) => Ok(Box::new(s.clone())),
			Expr::CharConst(ref c) => Ok(Box::new(c.clone())),
			Expr::Identifier(ref id) =>
			{
				for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev()
				{
					if *id == *name
					{
						return self.call_fn("clone", Some(val), None, None, None, None, None);
					}
				}
				Err(EvalAltResult::ErrorVariableNotFound(id.clone()))
			},
			Expr::Index(ref id, ref idx_raw) =>
			{
				let idx = self.eval_expr(scope, idx_raw)?;

				for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev()
				{
					if *id == *name
					{
						let mut idx_b: Box<Any> = idx;
						if let Ok(i) = idx_b.downcast::<i64>()
						{
							let mut val_b: Box<Any> = *val;
							if let Some(arr_typed) = (*val_b).downcast_mut() as Option<&mut Vec<Box<(Any + Sync)>>>
							{
								return self.call_fn("clone",
								                    Some(&mut arr_typed[*i as usize]),
								                    None,
								                    None,
								                    None,
								                    None,
								                    None);
							}
							else
							{
								return Err(EvalAltResult::ErrorIndexMismatch);
							}
						}
						else
						{
							return Err(EvalAltResult::ErrorIndexMismatch);
						}
					}
				}

				Err(EvalAltResult::ErrorVariableNotFound(id.clone()))
			},
			Expr::Assignment(ref id, ref rhs) =>
			{
				let rhs_val = self.eval_expr(scope, rhs)?;

				match **id
				{
					Expr::Identifier(ref n) =>
					{
						for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev()
						{
							if *n == *name
							{

								*val = rhs_val;

								return Ok(Box::new(()));
							}
						}
						Err(EvalAltResult::ErrorVariableNotFound(n.clone()))
					},
					Expr::Index(ref id, ref idx_raw) =>
					{
						let idx = self.eval_expr(scope, idx_raw)?;

						for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev()
						{
							if *id == *name
							{
								let mut idx_b: Box<Any> = idx;
								if let Ok(i) = idx_b.downcast::<i64>()
								{
									let mut val_b: Box<Any> = *val;
									if let Some(arr_typed) = (*val_b).downcast_mut() as Option<&mut Vec<Box<(Any + Sync)>>>
									{
										arr_typed[*i as usize] = rhs_val;
										return Ok(Box::new(()));
									}
									else
									{
										return Err(EvalAltResult::ErrorIndexMismatch);
									}
								}
								else
								{
									return Err(EvalAltResult::ErrorIndexMismatch);
								}
							}
						}

						Err(EvalAltResult::ErrorVariableNotFound(id.clone()))
					},
					Expr::Dot(ref dot_lhs, ref dot_rhs) => self.set_dot_val(scope, dot_lhs, dot_rhs, rhs_val),
					_ => Err(EvalAltResult::ErrorAssignmentToUnknownLHS),
				}
			},
			Expr::Dot(ref lhs, ref rhs) => self.get_dot_val(scope, lhs, rhs),
			Expr::Array(ref contents) =>
			{
				let mut arr = Vec::new();

				for item in (*contents).iter()
				{
					let arg = self.eval_expr(scope, item)?;
					arr.push(arg);
				}

				Ok(Box::new(arr))
			},
			Expr::FnCall(ref fn_name, ref args) =>
			{
				if args.len() == 0
				{
					self.call_fn(&fn_name, None, None, None, None, None, None)
				}
				else if args.len() == 1
				{
					let mut arg = self.eval_expr(scope, &args[0])?;

					self.call_fn(&fn_name, Some(&mut arg), None, None, None, None, None)
				}
				else if args.len() == 2
				{
					let mut arg1 = self.eval_expr(scope, &args[0])?;
					let mut arg2 = self.eval_expr(scope, &args[1])?;

					self.call_fn(&fn_name,
					             Some(&mut arg1),
					             Some(&mut arg2),
					             None,
					             None,
					             None,
					             None)
				}
				else if args.len() == 3
				{
					let mut arg1 = self.eval_expr(scope, &args[0])?;
					let mut arg2 = self.eval_expr(scope, &args[1])?;
					let mut arg3 = self.eval_expr(scope, &args[2])?;

					self.call_fn(&fn_name,
					             Some(&mut arg1),
					             Some(&mut arg2),
					             Some(&mut arg3),
					             None,
					             None,
					             None)
				}
				else if args.len() == 4
				{
					let mut arg1 = self.eval_expr(scope, &args[0])?;
					let mut arg2 = self.eval_expr(scope, &args[1])?;
					let mut arg3 = self.eval_expr(scope, &args[2])?;
					let mut arg4 = self.eval_expr(scope, &args[3])?;

					self.call_fn(&fn_name,
					             Some(&mut arg1),
					             Some(&mut arg2),
					             Some(&mut arg3),
					             Some(&mut arg4),
					             None,
					             None)
				}
				else if args.len() == 5
				{
					let mut arg1 = self.eval_expr(scope, &args[0])?;
					let mut arg2 = self.eval_expr(scope, &args[1])?;
					let mut arg3 = self.eval_expr(scope, &args[2])?;
					let mut arg4 = self.eval_expr(scope, &args[3])?;
					let mut arg5 = self.eval_expr(scope, &args[4])?;

					self.call_fn(&fn_name,
					             Some(&mut arg1),
					             Some(&mut arg2),
					             Some(&mut arg3),
					             Some(&mut arg4),
					             Some(&mut arg5),
					             None)
				}
				else if args.len() == 6
				{
					let mut arg1 = self.eval_expr(scope, &args[0])?;
					let mut arg2 = self.eval_expr(scope, &args[1])?;
					let mut arg3 = self.eval_expr(scope, &args[2])?;
					let mut arg4 = self.eval_expr(scope, &args[3])?;
					let mut arg5 = self.eval_expr(scope, &args[4])?;
					let mut arg6 = self.eval_expr(scope, &args[5])?;

					self.call_fn(&fn_name,
					             Some(&mut arg1),
					             Some(&mut arg2),
					             Some(&mut arg3),
					             Some(&mut arg4),
					             Some(&mut arg5),
					             Some(&mut arg6))
				}
				else
				{
					Err(EvalAltResult::ErrorFunctionCallNotSupported)
				}
			},
			Expr::True => Ok(Box::new(true)),
			Expr::False => Ok(Box::new(false)),
		}
	}

	fn eval_stmt(&self, scope: &mut Scope, stmt: &Stmt) -> Result<Box<(Any + Sync)>, EvalAltResult>
	{
		match *stmt
		{
			Stmt::Expr(ref e) => self.eval_expr(scope, e),
			Stmt::Block(ref b) =>
			{
				let prev_len = scope.len();
				let mut last_result: Result<Box<(Any + Sync)>, EvalAltResult> = Ok(Box::new(()));

				for s in b.iter()
				{
					last_result = self.eval_stmt(scope, s);
					match last_result
					{
						Err(x) =>
						{
							last_result = Err(x);
							break;
						},
						_ => (),
					}
				}

				while scope.len() > prev_len
				{
					scope.pop();
				}

				return last_result;
			},
			Stmt::If(ref guard, ref body) =>
			{
				let guard_result = self.eval_expr(scope, guard)?;
				let mut guard_result_b: Box<Any> = guard_result;
				match guard_result_b.downcast::<bool>()
				{
					Ok(g) =>
					{
						if *g
						{
							self.eval_stmt(scope, body)
						}
						else
						{
							Ok(Box::new(()))
						}
					},
					Err(_) => Err(EvalAltResult::ErrorIfGuardMismatch),
				}
			},
			Stmt::IfElse(ref guard, ref body, ref else_body) =>
			{
				let guard_result = self.eval_expr(scope, guard)?;
				let guard_result_b: Box<Any> = guard_result;
				match guard_result_b.downcast::<bool>()
				{
					Ok(g) =>
					{
						if *g
						{
							self.eval_stmt(scope, body)
						}
						else
						{
							self.eval_stmt(scope, else_body)
						}
					},
					Err(_) => Err(EvalAltResult::ErrorIfGuardMismatch),
				}
			},
			Stmt::Thread(ref name, ref body) =>
			{
				let name = if let &Some(ref n) = name {n.clone()} else {
				    "nameless".to_string() + &self.threads.to_string()
				};
				let body = body.clone();
				let tr = thread::spawn(
					||
					{
						self.eval_stmt(&mut Scope::new(), &Stmt::Block(Box::new(Vec::new())));
					}
				);

				scope.push(("_thread_".to_string() + &name, Box::new(tr)));
				Ok(Box::new(()))
			}
			Stmt::While(ref guard, ref body) =>
			{
				loop
				{
					let guard_result = self.eval_expr(scope, guard)?;
					let guard_result_b: Box<Any> = guard_result;
					match guard_result_b.downcast::<bool>()
					{
						Ok(g) =>
						{
							if *g
							{
								match self.eval_stmt(scope, body)
								{
									Err(EvalAltResult::LoopBreak) =>
									{
										return Ok(Box::new(()));
									},
									Err(x) =>
									{
										return Err(x);
									},
									_ => (),
								}
							}
							else
							{
								return Ok(Box::new(()));
							}
						},
						Err(_) => return Err(EvalAltResult::ErrorIfGuardMismatch),
					}
				}
			},
			Stmt::Break => return Err(EvalAltResult::LoopBreak),
			Stmt::Return => return Err(EvalAltResult::Return(Box::new(()))),
			Stmt::ReturnWithVal(ref a) =>
			{
				let result = self.eval_expr(scope, a)?;
				return Err(EvalAltResult::Return(result));
			},
			Stmt::Var(ref name, ref init) =>
			{
				match init
				{
					&Some(ref v) =>
					{
						let i = self.eval_expr(scope, v)?;
						scope.push((name.clone(), i));
					},
					&None =>
					{
						scope.push((name.clone(), Box::new(())));
					},
				};
				Ok(Box::new(()))
			},
		}
	}

	pub fn eval_file<T: Any + Clone>(&mut self, fname: &str) -> Result<T, EvalAltResult>
	{
		use std::fs::File;
		use std::io::prelude::*;

		if let Ok(mut f) = File::open(fname.clone())
		{
			let mut contents = String::new();

			if let Ok(_) = f.read_to_string(&mut contents)
			{
				self.eval::<T>(&contents)
			}
			else
			{
				Err(EvalAltResult::ErrorCantOpenScriptFile)
			}
		}
		else
		{
			Err(EvalAltResult::ErrorCantOpenScriptFile)
		}
	}

	pub fn eval<T: Any + Clone>(&mut self, input: &str) -> Result<T, EvalAltResult>
	{
		let mut scope: Scope = Vec::new();

		self.eval_with_scope(&mut scope, input)
	}

	pub fn eval_with_scope<T: Any + Clone>(&mut self, scope: &mut Scope, input: &str) -> Result<T, EvalAltResult>
	{
		let tokens = lex(input);

		let mut peekables = tokens.peekable();
		let tree = parse(&mut peekables);

		match tree
		{
			Ok((ref os, ref fns)) =>
			{
				let mut x: Result<Box<(Any + Sync)>, EvalAltResult> = Ok(Box::new(()));

				for f in fns
				{
					if f.params.len() > 6
					{
						return Err(EvalAltResult::ErrorFunctionArityNotSupported);
					}
					let name = f.name.clone();
					let local_f = f.clone();
					let ent = self.fns.entry(name).or_insert(Vec::new());
					(*ent).push(FnType::InternalFn(local_f));
				}

				for o in os
				{
					x = match self.eval_stmt(scope, &o)
					{
						Ok(v) => Ok(v),
						Err(e) => return Err(e),
					}
				}

				match x
				{
					Ok(v) =>
					{
						let v_b: Box<Any> = v;
						match v_b.downcast::<T>()
						{
							Ok(out) => Ok(*out),
							Err(_) => Err(EvalAltResult::ErrorMismatchOutputType),
						}
					},
					Err(e) => Err(e),
				}
			},
			Err(_) => Err(EvalAltResult::ErrorFunctionArgMismatch(String::new())),
		}
	}

	pub fn register_default_lib(engine: &mut Engine)
	{
		engine.register_type::<i32>();
		engine.register_type::<u32>();
		engine.register_type::<i64>();
		engine.register_type::<u64>();
		engine.register_type::<f32>();
		engine.register_type::<f64>();
		engine.register_type::<String>();
		engine.register_type::<char>();
		engine.register_type::<bool>();

		macro_rules! reg_op {
            ($engine:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
                $(
                    $engine.register_fn($x, ($op as fn(x: $y, y: $y)->$y));
                )*
            )
        }

		macro_rules! reg_cmp {
            ($engine:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
                $(
                    $engine.register_fn($x, ($op as fn(x: $y, y: $y)->bool));
                )*
            )
        }

		fn add<T: Add>(x: T, y: T) -> <T as Add>::Output {x + y}
		fn sub<T: Sub>(x: T, y: T) -> <T as Sub>::Output {x - y}
		fn mul<T: Mul>(x: T, y: T) -> <T as Mul>::Output {x * y}
		fn div<T: Div>(x: T, y: T) -> <T as Div>::Output {x / y}
		fn rem<T: Rem>(x: T, y: T) -> <T as Rem>::Output {x % y}
		fn lt<T: Ord>(x: T, y: T) ->  bool {x < y}
		fn gt<T: Ord>(x: T, y: T) ->  bool {x > y}
		fn lte<T: Ord>(x: T, y: T) -> bool {x <= y}
		fn gte<T: Ord>(x: T, y: T) -> bool {x >= y}
		fn eq<T: Eq>(x: T, y: T) ->   bool {x == y}
		fn ne<T: Eq>(x: T, y: T) ->   bool {x != y}
		fn and(x: bool, y: bool) ->   bool {x && y}
		fn or(x: bool, y: bool) ->    bool {x || y}
		fn concat(x: String, y: String) -> String {x + &y}

		reg_op!(engine, "+", add, i32, i64, u32, u64, f32, f64);
		reg_op!(engine, "-", sub, i32, i64, u32, u64, f32, f64);
		reg_op!(engine, "*", mul, i32, i64, u32, u64, f32, f64);
		reg_op!(engine, "/", div, i32, i64, u32, u64, f32, f64);
		reg_op!(engine, "%", rem, i32, i64, u32, u64, f32, f64);

		reg_cmp!(engine, "<", lt, i32, i64, u32, u64, String);
		reg_cmp!(engine, "<=", lte, i32, i64, u32, u64, String);
		reg_cmp!(engine, ">", gt, i32, i64, u32, u64, String);
		reg_cmp!(engine, ">=", gte, i32, i64, u32, u64, String);
		reg_cmp!(engine, "==", eq, i32, i64, u32, u64, bool, String);
		reg_cmp!(engine, "!=", ne, i32, i64, u32, u64, bool, String);

		reg_op!(engine, "||", or, bool);
		reg_op!(engine, "&&", and, bool);

		engine.register_fn("+", concat);

		// engine.register_fn("[]", idx);
		// FIXME?  Registering array lookups are a special case because we want to return boxes
		// directly let ent = engine.fns.entry("[]".to_string()).or_insert(Vec::new());
		// (*ent).push(FnType::ExternalFn2(Box::new(idx)));

	}

	pub fn new() -> Engine
	{
		let mut engine = Engine { fns: HashMap::new(), threads: 0 };

		Engine::register_default_lib(&mut engine);

		engine
	}
}
