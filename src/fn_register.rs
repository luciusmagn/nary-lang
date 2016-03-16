use std::any::Any;
use std::boxed::Box;

use engine::{EvalAltResult, Engine, FnType};

pub trait FnRegister<A, RetVal, Args> {
    fn register_fn(&mut self, name: &str, f: A);
}

impl<'a, A, T, U, V, W, X, Y, Z> FnRegister<A, Z, (&'a mut T, U, V, W, X, Y)> for Engine
    where A: 'static+Fn(&mut T, U, V, W, X, Y) -> Z, T: Clone+Any, U: Clone+Any, V: Clone+Any, W: Clone+Any, 
        X: Clone+Any, Y: Clone+Any, Z: Clone+Any
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>, 
            &mut Box<Any>)->Result<Box<Any>, EvalAltResult>> = 
            
            Box::new(
                move |arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>, arg4: &mut Box<Any>,
                    arg5: &mut Box<Any>, arg6: &mut Box<Any>| {
                    
                    let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                    let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                    let inside3 = (*arg3).downcast_mut() as Option<&mut V>;
                    let inside4 = (*arg4).downcast_mut() as Option<&mut W>;
                    let inside5 = (*arg5).downcast_mut() as Option<&mut X>;
                    let inside6 = (*arg6).downcast_mut() as Option<&mut Y>;
                    
                    match (inside1, inside2, inside3, inside4, inside5, inside6) {
                        (Some(b), Some(c), Some(d), Some(e), Some(f), Some(g)) => Ok(Box::new(fun(b, c.clone(), d.clone(), 
                            e.clone(), f.clone(), g.clone())) as Box<Any>),
                        _ => Err(EvalAltResult::ErrorFunctionArgMismatch)
                    }
                }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn6(wrapped));
    }
}

impl<'a, A, T, U, V, W, X, Y, Z> FnRegister<A, Z, (&'a T, U, V, W, X, Y)> for Engine
    where A: 'static+Fn(T, U, V, W, X, Y) -> Z, T: Clone+Any, U: Clone+Any, V: Clone+Any, W: Clone+Any, 
        X: Clone+Any, Y: Clone+Any, Z: Clone+Any
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>, 
            &mut Box<Any>)->Result<Box<Any>, EvalAltResult>> = 
            
            Box::new(
                move |arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>, arg4: &mut Box<Any>,
                    arg5: &mut Box<Any>, arg6: &mut Box<Any>| {

                    let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                    let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                    let inside3 = (*arg3).downcast_mut() as Option<&mut V>;
                    let inside4 = (*arg4).downcast_mut() as Option<&mut W>;
                    let inside5 = (*arg5).downcast_mut() as Option<&mut X>;
                    let inside6 = (*arg6).downcast_mut() as Option<&mut Y>;
                    
                    match (inside1, inside2, inside3, inside4, inside5, inside6) {
                        (Some(b), Some(c), Some(d), Some(e), Some(f), Some(g)) => Ok(Box::new(fun(b.clone(), c.clone(), d.clone(), 
                            e.clone(), f.clone(), g.clone())) as Box<Any>),
                        _ => Err(EvalAltResult::ErrorFunctionArgMismatch)
                    }
                }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn6(wrapped));
    }
}

impl<'a, A, T, U, V, W, X, Y> FnRegister<A, Y, (&'a mut T, U, V, W, X)> for Engine
    where A: 'static+Fn(&mut T, U, V, W, X) -> Y, T: Clone+Any, U: Clone+Any, V: Clone+Any, W: Clone+Any, X: Clone+Any, Y: Clone+Any
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalAltResult>> = 
            Box::new(
                move |arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>, arg4: &mut Box<Any>,
                    arg5: &mut Box<Any>| {

                    let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                    let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                    let inside3 = (*arg3).downcast_mut() as Option<&mut V>;
                    let inside4 = (*arg4).downcast_mut() as Option<&mut W>;
                    let inside5 = (*arg5).downcast_mut() as Option<&mut X>;
                    
                    match (inside1, inside2, inside3, inside4, inside5) {
                        (Some(b), Some(c), Some(d), Some(e), Some(f)) => Ok(Box::new(fun(b, c.clone(), d.clone(), 
                            e.clone(), f.clone())) as Box<Any>),
                        _ => Err(EvalAltResult::ErrorFunctionArgMismatch)
                    }
                }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn5(wrapped));
    }
}

impl<'a, A, T, U, V, W, X, Y> FnRegister<A, Y, (&'a T, U, V, W, X)> for Engine
    where A: 'static+Fn(T, U, V, W, X) -> Y, T: Clone+Any, U: Clone+Any, V: Clone+Any, W: Clone+Any, X: Clone+Any, Y: Clone+Any
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalAltResult>> = 
            Box::new(
                move |arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>, arg4: &mut Box<Any>,
                    arg5: &mut Box<Any>| {

                    let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                    let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                    let inside3 = (*arg3).downcast_mut() as Option<&mut V>;
                    let inside4 = (*arg4).downcast_mut() as Option<&mut W>;
                    let inside5 = (*arg5).downcast_mut() as Option<&mut X>;
                    
                    match (inside1, inside2, inside3, inside4, inside5) {
                        (Some(b), Some(c), Some(d), Some(e), Some(f)) => Ok(Box::new(fun(b.clone(), c.clone(), d.clone(), 
                            e.clone(), f.clone())) as Box<Any>),
                        _ => Err(EvalAltResult::ErrorFunctionArgMismatch)
                    }
                }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn5(wrapped));
    }
}

impl<'a, A, T, U, V, W, X> FnRegister<A, X, (&'a mut T, U, V, W)> for Engine
    where A: 'static+Fn(&mut T, U, V, W) -> X, T: Clone+Any, U: Clone+Any, V: Clone+Any, W: Clone+Any, X: Clone+Any
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalAltResult>> = 
            Box::new(
                move |arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>, arg4: &mut Box<Any>| {
                    let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                    let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                    let inside3 = (*arg3).downcast_mut() as Option<&mut V>;
                    let inside4 = (*arg4).downcast_mut() as Option<&mut W>;
                    
                    match (inside1, inside2, inside3, inside4) {
                        (Some(b), Some(c), Some(d), Some(e)) => Ok(Box::new(fun(b, c.clone(), d.clone(), e.clone())) as Box<Any>),
                        _ => Err(EvalAltResult::ErrorFunctionArgMismatch)
                    }
                }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn4(wrapped));
    }
}

impl<'a, A, T, U, V, W, X> FnRegister<A, X, (&'a T, U, V, W)> for Engine
    where A: 'static+Fn(T, U, V, W) -> X, T: Clone+Any, U: Clone+Any, V: Clone+Any, W: Clone+Any, X: Clone+Any
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalAltResult>> = 
            Box::new(
                move |arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>, arg4: &mut Box<Any>| {
                    let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                    let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                    let inside3 = (*arg3).downcast_mut() as Option<&mut V>;
                    let inside4 = (*arg4).downcast_mut() as Option<&mut W>;
                    
                    match (inside1, inside2, inside3, inside4) {
                        (Some(b), Some(c), Some(d), Some(e)) => Ok(Box::new(fun(b.clone(), c.clone(), d.clone(), e.clone())) as Box<Any>),
                        _ => Err(EvalAltResult::ErrorFunctionArgMismatch)
                    }
                }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn4(wrapped));
    }
}

impl<'a, A, T, U, V, W> FnRegister<A, W, (&'a mut T, U, V)> for Engine
    where A: 'static+Fn(&mut T, U, V) -> W, T: Clone+Any, U: Clone+Any, V: Clone+Any, W: Clone+Any
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalAltResult>> = 
            Box::new(
                move |arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>| {
                    let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                    let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                    let inside3 = (*arg3).downcast_mut() as Option<&mut V>;
                    
                    match (inside1, inside2, inside3) {
                        (Some(b), Some(c), Some(d)) => Ok(Box::new(fun(b, c.clone(), d.clone())) as Box<Any>),
                        _ => Err(EvalAltResult::ErrorFunctionArgMismatch)
                    }
                }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn3(wrapped));
    }
}

impl<'a, A, T, U, V, W> FnRegister<A, W, (&'a T, U, V)> for Engine
    where A: 'static+Fn(T, U, V) -> W, T: Clone+Any, U: Clone+Any, V: Clone+Any, W: Clone+Any
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalAltResult>> = 
            Box::new(
                move |arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>| {
                    let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                    let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                    let inside3 = (*arg3).downcast_mut() as Option<&mut V>;
                    
                    match (inside1, inside2, inside3) {
                        (Some(b), Some(c), Some(d)) => Ok(Box::new(fun(b.clone(), c.clone(), d.clone())) as Box<Any>),
                        _ => Err(EvalAltResult::ErrorFunctionArgMismatch)
                    }
                }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn3(wrapped));
    }
}

impl<'a, A, T, U, V> FnRegister<A, V, (&'a mut T, U)> for Engine
    where A: 'static+Fn(&mut T, U) -> V, T: Clone+Any, U: Clone+Any, V: Clone+Any
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn(&mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalAltResult>> = 
            Box::new(
                move |arg1: &mut Box<Any>, arg2: &mut Box<Any>| {
                    let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                    let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                    
                    match (inside1, inside2) {
                        (Some(b), Some(c)) => Ok(Box::new(fun(b, c.clone())) as Box<Any>),
                        _ => Err(EvalAltResult::ErrorFunctionArgMismatch)
                    }
                }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn2(wrapped));
    }
}

impl<'a, A, T, U, V> FnRegister<A, V, (&'a T, U)> for Engine
    where A: 'static+Fn(T, U) -> V, T: Clone+Any, U: Clone+Any, V: Clone+Any
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn(&mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalAltResult>> = 
            Box::new(
                move |arg1: &mut Box<Any>, arg2: &mut Box<Any>| {
                    let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                    let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                    
                    match (inside1, inside2) {
                        (Some(b), Some(c)) => Ok(Box::new(fun(b.clone(), c.clone())) as Box<Any>),
                        _ => Err(EvalAltResult::ErrorFunctionArgMismatch)
                    }
                }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn2(wrapped));
    }
}

impl<'a, A, T, U> FnRegister<A, U, (&'a mut T)> for Engine
    where A: 'static+Fn(&mut T) -> U, T: Clone+Any, U: Clone+Any 
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn(&mut Box<Any>)->Result<Box<Any>, EvalAltResult>> = 
            Box::new(
                move |arg: &mut Box<Any>| {
                    let inside = (*arg).downcast_mut() as Option<&mut T>;
                    
                    match inside {
                        Some(b) => Ok(Box::new(fun(b)) as Box<Any>),
                        None => Err(EvalAltResult::ErrorFunctionArgMismatch)
                    }
                }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn1(wrapped));
    }
}


impl<'a, A, T, U> FnRegister<A, U, (&'a T)> for Engine
    where A: 'static+Fn(T) -> U, T: Clone+Any, U: Clone+Any
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn(&mut Box<Any>)->Result<Box<Any>, EvalAltResult>> = 
            Box::new(
                move |arg: &mut Box<Any>| {
                    let inside = (*arg).downcast_mut() as Option<&mut T>;
                    match inside {
                        Some(b) => Ok(Box::new(fun(b.clone())) as Box<Any>),
                        None => Err(EvalAltResult::ErrorFunctionArgMismatch)
                    }
                }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn1(wrapped));
    }
}

impl<A, T> FnRegister<A, T, ()> for Engine
    where A: 'static+Fn() -> T, T: Clone+Any
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped : Box<Fn()->Result<Box<Any>, EvalAltResult>> = 
            Box::new(
                move || { Ok(Box::new(fun()) as Box<Any>) }
            );

        let ent = self.fns.entry(name.to_string()).or_insert(Vec::new());
        (*ent).push(FnType::ExternalFn0(wrapped));
    }
}
