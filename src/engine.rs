use std::collections::HashMap;
use std::error::Error;
use std::any::Any;
use std::boxed::Box;
use std::fmt;

use parser::{lex, parse, Expr, Stmt };
use fn_register::FnRegister;

use std::ops::{Add, Sub, Mul, Div};
use std::cmp::{Ord, Eq};

#[derive(Debug)]
pub enum EvalError {
    FunctionNotFound,
    FunctionArgMismatch,
    FunctionCallNotSupported,
    IfGuardMismatch,
    VariableNotFound
}

impl Error for EvalError {
    fn description(&self) -> &str {
        match *self {
            EvalError::FunctionNotFound => "Function not found",
            EvalError::FunctionArgMismatch => "Function argument types do not match",
            EvalError::FunctionCallNotSupported => "Function call with > 2 argument not supported",
            EvalError::IfGuardMismatch => "If guards expect boolean expression",
            EvalError::VariableNotFound => "Variable not found",
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

fn add<T: Add>(x: T, y: T) -> <T as Add>::Output {
    x + y
}

fn sub<T: Sub>(x: T, y: T) -> <T as Sub>::Output {
    x - y
}

fn mul<T: Mul>(x: T, y: T) -> <T as Mul>::Output {
    x * y
}

fn div<T: Div>(x: T, y: T) -> <T as Div>::Output {
    x / y
}

fn lt<T: Ord>(x: T, y: T) -> bool {
    x < y
}

fn lte<T: Ord>(x: T, y: T) -> bool {
    x <= y
}

fn gt<T: Ord>(x: T, y: T) -> bool {
    x > y
}

fn gte<T: Ord>(x: T, y: T) -> bool {
    x >= y
}

fn eq<T: Eq>(x: T, y: T) -> bool {
    x == y
}

fn ne<T: Eq>(x: T, y: T) -> bool {
    x != y
}

fn and(x: bool, y: bool) -> bool {
    x && y
}

fn or(x: bool, y: bool) -> bool {
    x || y
}

pub struct Engine {
    pub fns_arity_3: HashMap<String, Vec<Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>>>,
    pub fns_arity_2: HashMap<String, Vec<Box<Fn(&mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>>>,
    pub fns_arity_1: HashMap<String, Vec<Box<Fn(&mut Box<Any>)->Result<Box<Any>, EvalError>>>>,
    pub fns_arity_0: HashMap<String, Vec<Box<Fn()->Result<Box<Any>, EvalError>>>>,
    pub scope: Vec<(String, Box<Any>)>
}

impl Engine {
    pub fn call_fn_0_arg(fns_arity_0: &HashMap<String, Vec<Box<Fn()->Result<Box<Any>, EvalError>>>>, name: &str) -> Result<Box<Any>, EvalError> {
        match fns_arity_0.get(name) {
            Some(vf) => {
                for f in vf {
                    let invoke = f();
                    match invoke {
                        Ok(v) => return Ok(v),
                        _ => ()
                    }
                };
                Err(EvalError::FunctionArgMismatch)
            },
            None => Err(EvalError::FunctionNotFound)
        }
    }

    pub fn call_fn_1_arg(fns_arity_1: &HashMap<String, Vec<Box<Fn(&mut Box<Any>)->Result<Box<Any>, EvalError>>>>, 
        name: &str, arg1: &mut Box<Any>) -> Result<Box<Any>, EvalError> {
        
        match fns_arity_1.get(name) {
            Some(vf) => {
                for f in vf {
                    let invoke = f(arg1);
                    match invoke {
                        Ok(v) => return Ok(v),
                        _ => ()
                    }
                };
                Err(EvalError::FunctionArgMismatch)
            }
            None => Err(EvalError::FunctionNotFound)
        }
    }

    pub fn call_fn_2_arg(fns_arity_2: &HashMap<String, Vec<Box<Fn(&mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>>>, 
        name: &str, arg1: &mut Box<Any>, arg2: &mut Box<Any>) -> Result<Box<Any>, EvalError> {
        
        match fns_arity_2.get(name) {
            Some(vf) => {
                for f in vf {
                    let invoke = f(arg1, arg2);
                    match invoke {
                        Ok(v) => return Ok(v),
                        _ => ()
                    }
                };
                Err(EvalError::FunctionArgMismatch)
            }
            None => Err(EvalError::FunctionNotFound)
        }
    }

    pub fn call_fn_3_arg(fns_arity_3: &HashMap<String, Vec<Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>>>, 
        name: &str, arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>) -> Result<Box<Any>, EvalError> {
        
        match fns_arity_3.get(name) {
            Some(vf) => {
                for f in vf {
                    let invoke = f(arg1, arg2, arg3);
                    match invoke {
                        Ok(v) => return Ok(v),
                        _ => ()
                    }
                };
                Err(EvalError::FunctionArgMismatch)
            }
            None => Err(EvalError::FunctionNotFound)
        }
    }

    fn register_type<T: Clone+Any>(&mut self) {
        fn clone_helper<T: Clone>(t:T)->T { t.clone() };

        &(clone_helper as fn(T)->T).register(self, "clone");
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Box<Any>, EvalError> {
        match *expr {
            Expr::IntConst(i) => Ok(Box::new(i)),
            Expr::Identifier(ref id) => {
                for &mut (ref name, ref mut val) in &mut self.scope.iter_mut().rev() {
                    if *id == *name {
                        //Ideally, we wouldn't have to inline this call above
                        //let result = self.call_fn_1_arg("clone", val);
                        let result = match self.fns_arity_1.get("clone") {
                            Some(vf) => {
                                for f in vf {
                                    let invoke = f(val);
                                    match invoke {
                                        Ok(v) => return Ok(v),
                                        _ => ()
                                    }
                                };
                                Err(EvalError::FunctionArgMismatch)
                            }
                            None => Err(EvalError::FunctionNotFound)
                        };

                        return result;
                    }
                }
                Err(EvalError::VariableNotFound)
            }
            Expr::Assignment(ref id, ref rhs) => {
                match **id {
                    Expr::Identifier(ref n) => {
                        let rhs_val = try!(self.eval_expr(rhs));
                        for &mut (ref name, ref mut val) in &mut self.scope.iter_mut().rev() {
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
                    Engine::call_fn_0_arg(&self.fns_arity_0, &fn_name)
                }
                else if args.len() == 1 {
                    let mut arg = try!(self.eval_expr(&args[0]));

                    Engine::call_fn_1_arg(&self.fns_arity_1, &fn_name, &mut arg)
                }
                else if args.len() == 2 {
                    let mut arg1 = try!(self.eval_expr(&args[0]));
                    let mut arg2 = try!(self.eval_expr(&args[1]));

                    Engine::call_fn_2_arg(&self.fns_arity_2, &fn_name, &mut arg1, &mut arg2)
                }
                else if args.len() == 3 {
                    let mut arg1 = try!(self.eval_expr(&args[0]));
                    let mut arg2 = try!(self.eval_expr(&args[1]));
                    let mut arg3 = try!(self.eval_expr(&args[1]));

                    Engine::call_fn_3_arg(&self.fns_arity_3, &fn_name, &mut arg1, &mut arg2, &mut arg3)
                }
                else {
                    Err(EvalError::FunctionCallNotSupported)
                }
            }
            Expr::MethodCall(ref target, ref fn_name, ref args) => {
                if args.len() == 0 {
                    for &mut (ref name, ref mut val) in &mut self.scope.iter_mut().rev() {
                        if *target == *name {
                            return Engine::call_fn_1_arg(&self.fns_arity_1, &fn_name, val);
                        }
                    }
                    Err(EvalError::VariableNotFound)
                }
                else if args.len() == 1 {
                    let mut arg = try!(self.eval_expr(&args[0]));

                    for &mut (ref name, ref mut val) in &mut self.scope.iter_mut().rev() {
                        if *target == *name {
                            return Engine::call_fn_2_arg(&self.fns_arity_2, &fn_name, val, &mut arg);
                        }
                    }
                    Err(EvalError::VariableNotFound)
                }
                else if args.len() == 2 {
                    let mut arg1 = try!(self.eval_expr(&args[0]));
                    let mut arg2 = try!(self.eval_expr(&args[1]));

                    for &mut (ref name, ref mut val) in &mut self.scope.iter_mut().rev() {
                        if *target == *name {
                            return Engine::call_fn_3_arg(&self.fns_arity_3, &fn_name, val, &mut arg1, &mut arg2);
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

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<Box<Any>, EvalError> {
        match *stmt {
            Stmt::Expr(ref e) => {
                self.eval_expr(e)
            }
            Stmt::Block(ref b) => {
                let prev_len = self.scope.len();
                let mut last_result : Result<Box<Any>, EvalError> = Ok(Box::new(0));

                for s in b.iter() {
                    last_result = self.eval_stmt(s)
                }

                while self.scope.len() > prev_len {
                    self.scope.pop();
                }

                return last_result;
            }
            Stmt::If(ref guard, ref body) => {
                let guard_result = try!(self.eval_expr(guard));
                match guard_result.downcast::<bool>() {
                    Ok(g) => {
                        if *g {
                            self.eval_stmt(body)
                        }
                        else {
                            Ok(Box::new(()))
                        }
                    }
                    Err(_) => Err(EvalError::IfGuardMismatch)
                }
            }
            Stmt::While(ref guard, ref body) => {
                loop {
                    let guard_result = try!(self.eval_expr(guard));
                    match guard_result.downcast::<bool>() {
                        Ok(g) => {
                            if *g {
                                try!(self.eval_stmt(body));
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
                        let i = try!(self.eval_expr(v)); 
                        self.scope.push((name.clone(), i));
                    },
                    & None => {
                        self.scope.push((name.clone(), Box::new(())));
                    }
                };
                Ok(Box::new(()))
            }
        }
    }

    pub fn eval(&mut self, input: String) -> Result<Box<Any>, EvalError> {
        let tokens = lex(&input);

        let mut peekables = tokens.peekable();
        let tree = parse(&mut peekables);

        match tree {
            Ok((ref os, ref fns)) => {
                let mut x: Result<Box<Any>, EvalError> = Ok(Box::new(()));
                for f in fns {
                    if f.params.len() == 0 {
                        let local_f = f.clone();
                        let ent = self.fns_arity_0.entry(local_f.name).or_insert(Vec::new());
                        let wrapped : Box<Fn()->Result<Box<Any>, EvalError>> = 
                            Box::new(move || { Ok(Box::new(0)) } );
                        //move || { self.eval_stmt(&local_f.body) } 
                        (*ent).push(wrapped);
                    }
                }

                for o in os {
                    x = self.eval_stmt(&o)
                }
                x
            }
            Err(_) => Err(EvalError::FunctionArgMismatch),
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
            fns_arity_0: HashMap::new(), 
            fns_arity_1: HashMap::new(), 
            fns_arity_2: HashMap::new(), 
            fns_arity_3: HashMap::new(), 
            scope: Vec::new() 
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
fn test_bool_op() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval("true && (false || true)".to_string()).unwrap().downcast::<bool>() {
        assert_eq!(*result, true);
    }
    else {
        assert!(false);
    }

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
}

#[test]
fn test_var_scope() {
    let mut engine = Engine::new();

    if let Ok(_) = engine.eval("var x = 4 + 5".to_string()) { } else { assert!(false); }    

    if let Ok(result) = engine.eval("x".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 9);
    }
    else {
        assert!(false);
    }    

    if let Ok(_) = engine.eval("x = x + 1; x = x + 2;".to_string()) { } else { assert!(false); }

    if let Ok(result) = engine.eval("x".to_string()).unwrap().downcast::<i32>() {
        assert_eq!(*result, 12);
    }
    else {
        assert!(false);
    }

    if let Ok(_) = engine.eval("{var x = 3}".to_string()) { } else { assert!(false); }

    if let Ok(result) = engine.eval("x".to_string()).unwrap().downcast::<i32>() {
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
