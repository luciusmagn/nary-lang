use std::collections::HashMap;
use std::error::Error;
use std::any::Any;
use std::boxed::Box;
use std::fmt;

use parser::{lex, parse, Expr, Stmt, FnDef };
use fn_register::FnRegister;

use std::ops::{Add, Sub, Mul, Div};
use std::cmp::{Ord, Eq};

#[derive(Debug)]
pub enum EvalError {
    FunctionNotFound,
    FunctionArgMismatch,
    FunctionCallNotSupported,
    IfGuardMismatch,
    VariableNotFound,
    FunctionArityNotSupported
}

impl Error for EvalError {
    fn description(&self) -> &str {
        match *self {
            EvalError::FunctionNotFound => "Function not found",
            EvalError::FunctionArgMismatch => "Function argument types do not match",
            EvalError::FunctionCallNotSupported => "Function call with > 2 argument not supported",
            EvalError::IfGuardMismatch => "If guards expect boolean expression",
            EvalError::VariableNotFound => "Variable not found",
            EvalError::FunctionArityNotSupported => "Functions of more than 3 parameters are not yet supported"
        }
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

pub enum FnType {
    ExternalFn0(Box<Fn()->Result<Box<Any>, EvalError>>),
    ExternalFn1(Box<Fn(&mut Box<Any>)->Result<Box<Any>, EvalError>>),
    ExternalFn2(Box<Fn(&mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>),
    ExternalFn3(Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>),
    ExternalFn4(Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>),
    ExternalFn5(Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>),
    ExternalFn6(Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>),

    InternalFn(FnDef)
}

pub struct Engine {
    pub fns: HashMap<String, Vec<FnType>>
}

pub type Scope = Vec<(String, Box<Any>)>;

impl Engine {
    fn call_fn(&self, name: &str, arg1: Option<&mut Box<Any>>, arg2: Option<&mut Box<Any>>, arg3: Option<&mut Box<Any>>,
        arg4: Option<&mut Box<Any>>, arg5: Option<&mut Box<Any>>, arg6: Option<&mut Box<Any>>) -> Result<Box<Any>, EvalError> {

        match self.fns.get(name) {
            Some(ref vf) => {
                match (arg1, arg2, arg3, arg4, arg5, arg6) {
                    (Some(ref mut a1), Some(ref mut a2), Some(ref mut a3), Some(ref mut a4), Some(ref mut a5), Some(ref mut a6)) => {
                        for arr_f in *vf {
                            match arr_f {
                                & FnType::ExternalFn6(ref f) => {
                                    match f(*a1, *a2, *a3, *a4, *a5, *a6) {
                                        Ok(v) => return Ok(v),
                                        _ => ()
                                    }
                                }
                                & FnType::InternalFn(ref f) => {
                                    let mut new_scope: Scope = Vec::new();
                                    let result1 = self.call_fn("clone", Some(a1), None, None, None, None, None);
                                    let result2 = self.call_fn("clone", Some(a2), None, None, None, None, None);
                                    let result3 = self.call_fn("clone", Some(a3), None, None, None, None, None);
                                    let result4 = self.call_fn("clone", Some(a4), None, None, None, None, None);
                                    let result5 = self.call_fn("clone", Some(a5), None, None, None, None, None);
                                    let result6 = self.call_fn("clone", Some(a6), None, None, None, None, None);

                                    match (result1, result2, result3, result4, result5, result6) {
                                        (Ok(r1), Ok(r2), Ok(r3), Ok(r4), Ok(r5), Ok(r6)) => {
                                            new_scope.push((f.params[0].clone(), r1));
                                            new_scope.push((f.params[1].clone(), r2));
                                            new_scope.push((f.params[2].clone(), r3));
                                            new_scope.push((f.params[3].clone(), r4));
                                            new_scope.push((f.params[4].clone(), r5));
                                            new_scope.push((f.params[5].clone(), r6));
                                        },
                                        _ => return Err(EvalError::FunctionArgMismatch)
                                    }                                
                                    return self.eval_stmt(&mut new_scope, &*f.body);                                    
                                }
                                _ => ()
                            }
                        }
                        return Err(EvalError::FunctionArgMismatch);
                    }
                    (Some(ref mut a1), Some(ref mut a2), Some(ref mut a3), Some(ref mut a4), Some(ref mut a5), None) => {
                        for arr_f in *vf {
                            match arr_f {
                                & FnType::ExternalFn5(ref f) => {
                                    match f(*a1, *a2, *a3, *a4, *a5) {
                                        Ok(v) => return Ok(v),
                                        _ => ()
                                    }
                                }
                                & FnType::InternalFn(ref f) => {
                                    let mut new_scope: Scope = Vec::new();
                                    let result1 = self.call_fn("clone", Some(a1), None, None, None, None, None);
                                    let result2 = self.call_fn("clone", Some(a2), None, None, None, None, None);
                                    let result3 = self.call_fn("clone", Some(a3), None, None, None, None, None);
                                    let result4 = self.call_fn("clone", Some(a4), None, None, None, None, None);
                                    let result5 = self.call_fn("clone", Some(a5), None, None, None, None, None);

                                    match (result1, result2, result3, result4, result5) {
                                        (Ok(r1), Ok(r2), Ok(r3), Ok(r4), Ok(r5)) => {
                                            new_scope.push((f.params[0].clone(), r1));
                                            new_scope.push((f.params[1].clone(), r2));
                                            new_scope.push((f.params[2].clone(), r3));
                                            new_scope.push((f.params[3].clone(), r4));
                                            new_scope.push((f.params[4].clone(), r5));
                                        },
                                        _ => return Err(EvalError::FunctionArgMismatch)
                                    }                                
                                    return self.eval_stmt(&mut new_scope, &*f.body);                                    
                                }
                                _ => ()
                            }
                        }
                        return Err(EvalError::FunctionArgMismatch);
                    }
                    (Some(ref mut a1), Some(ref mut a2), Some(ref mut a3), Some(ref mut a4), None, None) => {
                        for arr_f in *vf {
                            match arr_f {
                                & FnType::ExternalFn4(ref f) => {
                                    match f(*a1, *a2, *a3, *a4) {
                                        Ok(v) => return Ok(v),
                                        _ => ()
                                    }
                                }
                                & FnType::InternalFn(ref f) => {
                                    let mut new_scope: Scope = Vec::new();
                                    let result1 = self.call_fn("clone", Some(a1), None, None, None, None, None);
                                    let result2 = self.call_fn("clone", Some(a2), None, None, None, None, None);
                                    let result3 = self.call_fn("clone", Some(a3), None, None, None, None, None);
                                    let result4 = self.call_fn("clone", Some(a4), None, None, None, None, None);
                                    match (result1, result2, result3, result4) {
                                        (Ok(r1), Ok(r2), Ok(r3), Ok(r4)) => {
                                            new_scope.push((f.params[0].clone(), r1));
                                            new_scope.push((f.params[1].clone(), r2));
                                            new_scope.push((f.params[2].clone(), r3));
                                            new_scope.push((f.params[3].clone(), r4));
                                        },
                                        _ => return Err(EvalError::FunctionArgMismatch)
                                    }                                
                                    return self.eval_stmt(&mut new_scope, &*f.body);                                    
                                }
                                _ => ()
                            }
                        }
                        return Err(EvalError::FunctionArgMismatch);
                    }
                    (Some(ref mut a1), Some(ref mut a2), Some(ref mut a3), None, None, None) => {
                        for arr_f in *vf {
                            match arr_f {
                                & FnType::ExternalFn3(ref f) => {
                                    match f(*a1, *a2, *a3) {
                                        Ok(v) => return Ok(v),
                                        _ => ()
                                    }
                                }
                                & FnType::InternalFn(ref f) => {
                                    let mut new_scope: Scope = Vec::new();
                                    let result1 = self.call_fn("clone", Some(a1), None, None, None, None, None);
                                    let result2 = self.call_fn("clone", Some(a2), None, None, None, None, None);
                                    let result3 = self.call_fn("clone", Some(a3), None, None, None, None, None);
                                    match (result1, result2, result3) {
                                        (Ok(r1), Ok(r2), Ok(r3)) => {
                                            new_scope.push((f.params[0].clone(), r1));
                                            new_scope.push((f.params[1].clone(), r2));
                                            new_scope.push((f.params[2].clone(), r3));
                                        },
                                        _ => return Err(EvalError::FunctionArgMismatch)
                                    }                                
                                    return self.eval_stmt(&mut new_scope, &*f.body);                                    
                                }
                                _ => ()
                            }
                        }
                        return Err(EvalError::FunctionArgMismatch);
                    }
                    (Some(ref mut a1), Some(ref mut a2), None, None, None, None) => {
                        for arr_f in *vf {
                            match arr_f {
                                & FnType::ExternalFn2(ref f) => {
                                    match f(*a1, *a2) {
                                        Ok(v) => return Ok(v),
                                        _ => ()
                                    }
                                }
                                & FnType::InternalFn(ref f) => {
                                    let mut new_scope: Scope = Vec::new();
                                    let result1 = self.call_fn("clone", Some(a1), None, None, None, None, None);
                                    let result2 = self.call_fn("clone", Some(a2), None, None, None, None, None);
                                    match (result1, result2) {
                                        (Ok(r1), Ok(r2)) => {
                                            new_scope.push((f.params[0].clone(), r1));
                                            new_scope.push((f.params[1].clone(), r2));
                                        },
                                        _ => return Err(EvalError::FunctionArgMismatch)
                                    }                                
                                    return self.eval_stmt(&mut new_scope, &*f.body);                                    
                                }
                                _ => ()
                            }
                        }
                        return Err(EvalError::FunctionArgMismatch);
                    }
                    (Some(ref mut a1), None, None, None, None, None) => {
                        for arr_f in *vf {
                            match arr_f {
                                & FnType::ExternalFn1(ref f) => {
                                    match f(*a1) {
                                        Ok(v) => return Ok(v),
                                        _ => ()
                                    }
                                }
                                & FnType::InternalFn(ref f) => {
                                    let mut new_scope: Scope = Vec::new();
                                    let result1 = self.call_fn("clone", Some(a1), None, None, None, None, None);
                                    match result1 {
                                        Ok(r1) => {
                                            new_scope.push((f.params[0].clone(), r1));
                                        },
                                        _ => return Err(EvalError::FunctionArgMismatch)
                                    }                                
                                    return self.eval_stmt(&mut new_scope, &*f.body);                                    
                                }
                                _ => ()
                            }
                        }
                        return Err(EvalError::FunctionArgMismatch);
                    }
                    _ => {
                        for arr_f in *vf {
                            match arr_f {
                                & FnType::ExternalFn0(ref f) => {
                                    match f() {
                                        Ok(v) => return Ok(v),
                                        _ => ()
                                    }
                                }
                                & FnType::InternalFn(ref f) => {
                                    let mut new_scope: Scope = Vec::new();
                                    return self.eval_stmt(&mut new_scope, &*f.body);                                    
                                }
                                _ => ()
                            }
                        }
                        return Err(EvalError::FunctionArgMismatch);
                    }
                }
            }
            None => Err(EvalError::FunctionNotFound)
        }
    }

    fn register_type<T: Clone+Any>(&mut self) {
        fn clone_helper<T: Clone>(t:T)->T { t.clone() };

        &(clone_helper as fn(T)->T).register(self, "clone");
    }

    fn eval_expr(&self, scope: &mut Scope, expr: &Expr) -> Result<Box<Any>, EvalError> {
        match *expr {
            Expr::IntConst(i) => Ok(Box::new(i)),
            Expr::StringConst(ref s) => Ok(Box::new(s.clone())),
            Expr::Identifier(ref id) => {
                for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                    if *id == *name {                        
                        return self.call_fn("clone", Some(val), None, None, None, None, None);
                    }
                }
                Err(EvalError::VariableNotFound)
            }
            Expr::Assignment(ref id, ref rhs) => {
                match **id {
                    Expr::Identifier(ref n) => {
                        let rhs_val = try!(self.eval_expr(scope, rhs));
                        for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                            if *n == *name {

                                *val = rhs_val;

                                return Ok(Box::new(()));
                            }
                        }
                        Err(EvalError::VariableNotFound)
                    }
                    _ => Err(EvalError::VariableNotFound)
                }
            }
            Expr::FnCall(ref fn_name, ref args) => {
                if args.len() == 0 {
                    self.call_fn(&fn_name, None, None, None, None, None, None)
                }
                else if args.len() == 1 {
                    let mut arg = try!(self.eval_expr(scope, &args[0]));

                    self.call_fn(&fn_name, Some(&mut arg), None, None, None, None, None)
                }
                else if args.len() == 2 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));

                    self.call_fn(&fn_name, Some(&mut arg1), Some(&mut arg2), None, None, None, None)
                }
                else if args.len() == 3 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));
                    let mut arg3 = try!(self.eval_expr(scope, &args[2]));

                    self.call_fn(&fn_name, Some(&mut arg1), Some(&mut arg2), Some(&mut arg3), None, None, None)
                }
                else if args.len() == 4 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));
                    let mut arg3 = try!(self.eval_expr(scope, &args[2]));
                    let mut arg4 = try!(self.eval_expr(scope, &args[3]));

                    self.call_fn(&fn_name, Some(&mut arg1), Some(&mut arg2), Some(&mut arg3), Some(&mut arg4), None, None)
                }
                else if args.len() == 5 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));
                    let mut arg3 = try!(self.eval_expr(scope, &args[2]));
                    let mut arg4 = try!(self.eval_expr(scope, &args[3]));
                    let mut arg5 = try!(self.eval_expr(scope, &args[4]));

                    self.call_fn(&fn_name, Some(&mut arg1), Some(&mut arg2), Some(&mut arg3), Some(&mut arg4), 
                        Some(&mut arg5), None)
                }
                else if args.len() == 6 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));
                    let mut arg3 = try!(self.eval_expr(scope, &args[2]));
                    let mut arg4 = try!(self.eval_expr(scope, &args[3]));
                    let mut arg5 = try!(self.eval_expr(scope, &args[4]));
                    let mut arg6 = try!(self.eval_expr(scope, &args[5]));

                    self.call_fn(&fn_name, Some(&mut arg1), Some(&mut arg2), Some(&mut arg3), Some(&mut arg4), 
                        Some(&mut arg5), Some(&mut arg6))
                }
                else {
                    Err(EvalError::FunctionCallNotSupported)
                }
            }
            Expr::MethodCall(ref target, ref fn_name, ref args) => {                
                if args.len() == 0 {
                    for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                        if *target == *name {
                            return self.call_fn(&fn_name, Some(val), None, None, None, None, None);
                        }
                    }
                    Err(EvalError::VariableNotFound)
                }
                else if args.len() == 1 {
                    let mut arg = try!(self.eval_expr(scope, &args[0]));

                    for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                        if *target == *name {
                            return self.call_fn(&fn_name, Some(val), Some(&mut arg), None, None, None, None);
                        }
                    }
                    Err(EvalError::VariableNotFound)
                }
                else if args.len() == 2 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));

                    for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                        if *target == *name {
                            return self.call_fn(&fn_name, Some(val), Some(&mut arg1), Some(&mut arg2), None, None, None);
                        }
                    }
                    Err(EvalError::VariableNotFound)
                }
                else if args.len() == 3 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));
                    let mut arg3 = try!(self.eval_expr(scope, &args[2]));

                    for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                        if *target == *name {
                            return self.call_fn(&fn_name, Some(val), Some(&mut arg1), Some(&mut arg2), Some(&mut arg3), None, None);
                        }
                    }
                    Err(EvalError::VariableNotFound)
                }
                else if args.len() == 4 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));
                    let mut arg3 = try!(self.eval_expr(scope, &args[2]));
                    let mut arg4 = try!(self.eval_expr(scope, &args[3]));

                    for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                        if *target == *name {
                            return self.call_fn(&fn_name, Some(val), Some(&mut arg1), Some(&mut arg2), Some(&mut arg3), 
                                Some(&mut arg4), None);
                        }
                    }
                    Err(EvalError::VariableNotFound)
                }
                else if args.len() == 5 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));
                    let mut arg3 = try!(self.eval_expr(scope, &args[2]));
                    let mut arg4 = try!(self.eval_expr(scope, &args[3]));
                    let mut arg5 = try!(self.eval_expr(scope, &args[4]));

                    for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                        if *target == *name {
                            return self.call_fn(&fn_name, Some(val), Some(&mut arg1), Some(&mut arg2), Some(&mut arg3), 
                                Some(&mut arg4), Some(&mut arg5));
                        }
                    }
                    Err(EvalError::VariableNotFound)
                }
                else {
                    Err(EvalError::FunctionCallNotSupported)
                }
            }
            Expr::True => {
                Ok(Box::new(true))
            }
            Expr::False => {
                Ok(Box::new(false))
            }            
        }
    }

    fn eval_stmt(&self, scope: &mut Scope, stmt: &Stmt) -> Result<Box<Any>, EvalError> {
        match *stmt {
            Stmt::Expr(ref e) => {
                self.eval_expr(scope, e)
            }
            Stmt::Block(ref b) => {
                let prev_len = scope.len();
                let mut last_result : Result<Box<Any>, EvalError> = Ok(Box::new(0));

                for s in b.iter() {
                    last_result = self.eval_stmt(scope, s)
                }

                while scope.len() > prev_len {
                    scope.pop();
                }

                return last_result;
            }
            Stmt::If(ref guard, ref body) => {
                let guard_result = try!(self.eval_expr(scope, guard));
                match guard_result.downcast::<bool>() {
                    Ok(g) => {
                        if *g {
                            self.eval_stmt(scope, body)
                        }
                        else {
                            Ok(Box::new(()))
                        }
                    }
                    Err(_) => Err(EvalError::IfGuardMismatch)
                }
            }
            Stmt::IfElse(ref guard, ref body, ref else_body) => {
                let guard_result = try!(self.eval_expr(scope, guard));
                match guard_result.downcast::<bool>() {
                    Ok(g) => {
                        if *g {
                            self.eval_stmt(scope, body)
                        }
                        else {
                            self.eval_stmt(scope, else_body)
                        }
                    }
                    Err(_) => Err(EvalError::IfGuardMismatch)
                }
            }
            Stmt::While(ref guard, ref body) => {
                loop {
                    let guard_result = try!(self.eval_expr(scope, guard));
                    match guard_result.downcast::<bool>() {
                        Ok(g) => {
                            if *g {
                                try!(self.eval_stmt(scope, body));
                            }
                            else {
                                return Ok(Box::new(()));
                            }
                        }
                        Err(_) => return Err(EvalError::IfGuardMismatch)
                    }                    
                }
            }
            Stmt::Var(ref name, ref init) => {
                match init {
                    & Some(ref v) => {
                        let i = try!(self.eval_expr(scope, v)); 
                        scope.push((name.clone(), i));
                    },
                    & None => {
                        scope.push((name.clone(), Box::new(())));
                    }
                };
                Ok(Box::new(()))
            }
        }
    }

    pub fn eval(&mut self, input: String) -> Result<Box<Any>, EvalError> {
        let mut scope: Scope = Vec::new();

        self.eval_with_scope(&mut scope, input)
    }

    pub fn eval_with_scope(&mut self, scope: &mut Scope, input: String) -> Result<Box<Any>, EvalError> {
        let tokens = lex(&input);

        let mut peekables = tokens.peekable();
        let tree = parse(&mut peekables);

        match tree {
            Ok((ref os, ref fns)) => {
                let mut x: Result<Box<Any>, EvalError> = Ok(Box::new(()));
                
                for f in fns {
                    if f.params.len() > 6 {
                        return Err(EvalError::FunctionArityNotSupported);
                    }
                    let name = f.name.clone();
                    let local_f = f.clone();
                    let ent = self.fns.entry(name).or_insert(Vec::new());
                    (*ent).push(FnType::InternalFn(local_f));                                
                }

                for o in os {
                    x = self.eval_stmt(scope, &o);
                }
                x
            }
            Err(_) => Err(EvalError::FunctionArgMismatch)
        }
    }

    pub fn register_default_lib(engine: &mut Engine) {
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
                    ($op as fn(x: $y, y: $y)->$y).register($engine, $x);
                )*
            )
        }

        macro_rules! reg_cmp {
            ($engine:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
                $(
                    ($op as fn(x: $y, y: $y)->bool).register($engine, $x);
                )*
            )
        }

        fn add<T: Add>(x: T, y: T) -> <T as Add>::Output { x + y }
        fn sub<T: Sub>(x: T, y: T) -> <T as Sub>::Output { x - y }
        fn mul<T: Mul>(x: T, y: T) -> <T as Mul>::Output { x * y }
        fn div<T: Div>(x: T, y: T) -> <T as Div>::Output { x / y }
        fn lt<T: Ord>(x: T, y: T) -> bool { x < y }
        fn lte<T: Ord>(x: T, y: T) -> bool { x <= y }
        fn gt<T: Ord>(x: T, y: T) -> bool { x > y }
        fn gte<T: Ord>(x: T, y: T) -> bool { x >= y }
        fn eq<T: Eq>(x: T, y: T) -> bool { x == y }
        fn ne<T: Eq>(x: T, y: T) -> bool { x != y }
        fn and(x: bool, y: bool) -> bool { x && y }
        fn or(x: bool, y: bool) -> bool { x || y }

        reg_op!(engine, "+", add, i32, i64, u32, u64, f32, f64);
        reg_op!(engine, "-", sub, i32, i64, u32, u64, f32, f64);
        reg_op!(engine, "*", mul, i32, i64, u32, u64, f32, f64);
        reg_op!(engine, "/", div, i32, i64, u32, u64, f32, f64);

        reg_cmp!(engine, "<", lt, i32, i64, u32, u64);
        reg_cmp!(engine, "<=", lte, i32, i64, u32, u64);
        reg_cmp!(engine, ">", gt, i32, i64, u32, u64);
        reg_cmp!(engine, ">=", gte, i32, i64, u32, u64);
        reg_cmp!(engine, "==", eq, i32, i64, u32, u64, bool);
        reg_cmp!(engine, "!=", ne, i32, i64, u32, u64, bool);

        reg_op!(engine, "||", or, bool);
        reg_op!(engine, "&&", and, bool);
    }

    pub fn new() -> Engine {
        let mut engine = Engine { 
            fns: HashMap::new()
        };

        Engine::register_default_lib(&mut engine);

        engine
    }
}


#[test]
fn test_number_literal() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval("65".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 65);
    }
    else {
        assert!(false);
    }
}

#[test]
fn test_addition() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval("60 + 5".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 65);
    }
    else {
        assert!(false);
    }
}

#[test]
fn test_bool_op1() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval("true && (false || true)".to_string()).unwrap().downcast::<bool>() {
        assert_eq!(*result, true);
    }
    else {
        assert!(false);
    }
}

#[test]
fn test_bool_op2() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval("false && (false || true)".to_string()).unwrap().downcast::<bool>() {
        assert_eq!(*result, false);
    }
    else {
        assert!(false);
    }
}

#[test]
fn test_op_prec() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval("var x = 0; if x == 10 || true { x = 1} x".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 1);
    }
    else {
        assert!(false);
    }
}

#[test]
fn test_if() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval("if true { 55 }".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 55);
    }
    else {
        assert!(false);
    }

    if let Ok(result) = engine.eval("if false { 55 } else { 44 }".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 44);
    }
    else {
        assert!(false);
    }

    if let Ok(result) = engine.eval("if true { 55 } else { 44 }".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 55);
    }
    else {
        assert!(false);
    }
}

#[test]

fn test_var_scope() {
    let mut engine = Engine::new();
    let mut scope: Scope = Vec::new();

    if let Ok(_) = engine.eval_with_scope(&mut scope, "var x = 4 + 5".to_string()) { } else { assert!(false); }    

    if let Ok(result) = engine.eval_with_scope(&mut scope, "x".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 9);
    }
    else {
        assert!(false);
    }    

    if let Ok(_) = engine.eval_with_scope(&mut scope, "x = x + 1; x = x + 2;".to_string()) { } else { assert!(false); }

    if let Ok(result) = engine.eval_with_scope(&mut scope, "x".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 12);
    }
    else {
        assert!(false);
    }

    if let Ok(_) = engine.eval_with_scope(&mut scope, "{var x = 3}".to_string()) { } else { assert!(false); }

    if let Ok(result) = engine.eval_with_scope(&mut scope, "x".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 12);
    }
    else {
        assert!(false);
    }
}

#[test]
fn test_method_call() {
    #[derive(Debug, Clone)]
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

    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    &(TestStruct::update as fn(&mut TestStruct)->()).register(&mut engine, "update");
    &(TestStruct::new as fn()->TestStruct).register(&mut engine, "new_ts");

    if let Ok(result) = engine.eval("var x = new_ts(); x.update(); x".to_string()).unwrap().downcast::<TestStruct>() {
        assert_eq!(result.x, 1001);
    }
    else {
        assert!(false);
    }

}

#[test]
fn test_internal_fn() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval("fn addme(a, b) { a+b } addme(3, 4)".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 7);
    }
    else {
        assert!(false);
    }
}

#[test]
fn test_big_internal_fn() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval("fn mathme(a, b, c, d, e, f) { a - b * c + d * e - f } mathme(100, 5, 2, 9, 6, 32)".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 112);
    }
    else {
        assert!(false);
    }
}

#[test]
fn test_string() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval("\"Test string: \\u2764\"".to_string()).unwrap().downcast::<String>() {
        assert_eq!(*result, "Test string: ❤");
    }
    else {
        assert!(false);
    }
}
