use std::any::Any;
use std::boxed::Box;

use engine::{EvalError, Engine};

pub trait FnRegister {
    fn register(self, engine: &mut Engine, name: &str);
}

impl<T: Any, U: Any> FnRegister for fn(&mut T)->U {
    fn register(self, engine: &mut Engine, name: &str) {
        let wrapped : Box<Fn(&mut Box<Any>)->Result<Box<Any>, EvalError>> = 
            Box::new(
                move |x: &mut Box<Any>| {
                    let inside = (*x).downcast_mut() as Option<&mut T>;
                    
                    match inside {
                        Some(b) => Ok(Box::new(self(b)) as Box<Any>),
                        None => Err(EvalError::FunctionArgMismatch)
                    }
                }
            );

        let ent = engine.fns_arity_1.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(wrapped);
    }
}

impl<T: Any+Clone, U: Any> FnRegister for fn(T)->U {
    fn register(self, engine: &mut Engine, name: &str) {        
        let wrapped : Box<Fn(&mut Box<Any>)->Result<Box<Any>, EvalError>> = 
            Box::new(
                move |x: &mut Box<Any>| {
                    let inside = (*x).downcast_mut() as Option<&mut T>;
                    match inside {
                        Some(b) => Ok(Box::new(self(b.clone())) as Box<Any>),
                        None => Err(EvalError::FunctionArgMismatch)
                    }
                }
            );

        let ent = engine.fns_arity_1.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(wrapped);
    }
}

impl<T: Any> FnRegister for fn()->T {
    fn register(self, engine: &mut Engine, name: &str) {
        let wrapped : Box<Fn()->Result<Box<Any>, EvalError>> = 
            Box::new(
                move || { Ok(Box::new(self()) as Box<Any>) }
            );

        let ent = engine.fns_arity_0.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(wrapped);
    }
}
