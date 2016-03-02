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


pub enum Arity6 {
    ExternalFn(Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>),
    InternalFn(FnDef)
}

pub enum Arity5 {
    ExternalFn(Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>),
    InternalFn(FnDef)
}

pub enum Arity4 {
    ExternalFn(Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>),
    InternalFn(FnDef)
}

pub enum Arity3 {
    ExternalFn(Box<Fn(&mut Box<Any>, &mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>),
    InternalFn(FnDef)
}

pub enum Arity2 {
    ExternalFn(Box<Fn(&mut Box<Any>, &mut Box<Any>)->Result<Box<Any>, EvalError>>),
    InternalFn(FnDef)
}

pub enum Arity1 {
    ExternalFn(Box<Fn(&mut Box<Any>)->Result<Box<Any>, EvalError>>),
    InternalFn(FnDef)
}

pub enum Arity0 {
    ExternalFn(Box<Fn()->Result<Box<Any>, EvalError>>),
    InternalFn(FnDef)
}

pub struct Engine {
    pub fns_arity_6: HashMap<String, Vec<Arity6>>,
    pub fns_arity_5: HashMap<String, Vec<Arity5>>,
    pub fns_arity_4: HashMap<String, Vec<Arity4>>,
    pub fns_arity_3: HashMap<String, Vec<Arity3>>,
    pub fns_arity_2: HashMap<String, Vec<Arity2>>,
    pub fns_arity_1: HashMap<String, Vec<Arity1>>,
    pub fns_arity_0: HashMap<String, Vec<Arity0>>,
}

pub type Scope = Vec<(String, Box<Any>)>;

impl Engine {
    fn call_fn_0_arg(&self, name: &str) -> Result<Box<Any>, EvalError> {
        match self.fns_arity_0.get(name) {
            Some(vf) => {
                for arr_f in vf {
                    match arr_f {
                        & Arity0::ExternalFn(ref f) => {
                            let invoke = f();
                            match invoke {
                                Ok(v) => return Ok(v),
                                _ => ()
                            }                            
                        }
                        & Arity0::InternalFn(ref f) => {
                            let mut new_scope: Scope = Vec::new();
                            return self.eval_stmt(&mut new_scope, &*f.body);
                        }
                    }
                };
                Err(EvalError::FunctionArgMismatch)
            },
            None => Err(EvalError::FunctionNotFound)
        }
    }

    fn call_fn_1_arg(&self, name: &str, arg1: &mut Box<Any>) -> Result<Box<Any>, EvalError> {        
        match self.fns_arity_1.get(name) {
            Some(vf) => {
                for arr_f in vf {
                    match arr_f {
                        & Arity1::ExternalFn(ref f) => {
                            let invoke = f(arg1);
                            match invoke {
                                Ok(v) => return Ok(v),
                                _ => ()
                            }
                        }
                        & Arity1::InternalFn(ref f) => {
                            let mut new_scope: Scope = Vec::new();
                            let result = self.call_fn_1_arg("clone", arg1);
                            match result {
                                Ok(r) => new_scope.push((f.params[0].clone(), r)),
                                _ => return Err(EvalError::FunctionArgMismatch)
                            }                                
                            return self.eval_stmt(&mut new_scope, &*f.body);
                        }
                    }
                };
                Err(EvalError::FunctionArgMismatch)
            }
            None => Err(EvalError::FunctionNotFound)
        }
    }

    fn call_fn_2_arg(&self, name: &str, arg1: &mut Box<Any>, arg2: &mut Box<Any>) -> Result<Box<Any>, EvalError> {        
        match self.fns_arity_2.get(name) {
            Some(vf) => {
                for arr_f in vf {
                    match arr_f {
                        & Arity2::ExternalFn(ref f) => {
                            let invoke = f(arg1, arg2);
                            match invoke {
                                Ok(v) => return Ok(v),
                                _ => ()
                            }
                        }
                        & Arity2::InternalFn(ref f) => {
                            let mut new_scope: Scope = Vec::new();
                            let result1 = self.call_fn_1_arg("clone", arg1);
                            let result2 = self.call_fn_1_arg("clone", arg2);
                            match (result1, result2) {
                                (Ok(r1), Ok(r2)) => {
                                    new_scope.push((f.params[0].clone(), r1));
                                    new_scope.push((f.params[1].clone(), r2));
                                },
                                _ => return Err(EvalError::FunctionArgMismatch)
                            }                                
                            return self.eval_stmt(&mut new_scope, &*f.body);
                        }
                    }
                };
                Err(EvalError::FunctionArgMismatch)
            }
            None => Err(EvalError::FunctionNotFound)
        }
    }

    pub fn call_fn_3_arg(&self, name: &str, arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>) -> Result<Box<Any>, EvalError> {        
        match self.fns_arity_3.get(name) {
            Some(vf) => {
                for arr_f in vf {
                    match arr_f {
                        & Arity3::ExternalFn(ref f) => {
                            let invoke = f(arg1, arg2, arg3);
                            match invoke {
                                Ok(v) => return Ok(v),
                                _ => ()
                            }
                        }
                        & Arity3::InternalFn(ref f) => {
                            let mut new_scope: Scope = Vec::new();
                            let result1 = self.call_fn_1_arg("clone", arg1);
                            let result2 = self.call_fn_1_arg("clone", arg2);
                            let result3 = self.call_fn_1_arg("clone", arg3);
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
                    }
                };
                Err(EvalError::FunctionArgMismatch)
            }
            None => Err(EvalError::FunctionNotFound)
        }
    }

    pub fn call_fn_4_arg(&self, name: &str, arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>, arg4: &mut Box<Any>) -> 
        Result<Box<Any>, EvalError> {   

        match self.fns_arity_4.get(name) {
            Some(vf) => {
                for arr_f in vf {
                    match arr_f {
                        & Arity4::ExternalFn(ref f) => {
                            let invoke = f(arg1, arg2, arg3, arg4);
                            match invoke {
                                Ok(v) => return Ok(v),
                                _ => ()
                            }
                        }
                        & Arity4::InternalFn(ref f) => {
                            let mut new_scope: Scope = Vec::new();
                            let result1 = self.call_fn_1_arg("clone", arg1);
                            let result2 = self.call_fn_1_arg("clone", arg2);
                            let result3 = self.call_fn_1_arg("clone", arg3);
                            let result4 = self.call_fn_1_arg("clone", arg4);
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
                    }
                };
                Err(EvalError::FunctionArgMismatch)
            }
            None => Err(EvalError::FunctionNotFound)
        }
    }

    pub fn call_fn_5_arg(&self, name: &str, arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>, 
        arg4: &mut Box<Any>, arg5: &mut Box<Any>) -> Result<Box<Any>, EvalError> {

        match self.fns_arity_5.get(name) {
            Some(vf) => {
                for arr_f in vf {
                    match arr_f {
                        & Arity5::ExternalFn(ref f) => {
                            let invoke = f(arg1, arg2, arg3, arg4, arg5);
                            match invoke {
                                Ok(v) => return Ok(v),
                                _ => ()
                            }
                        }
                        & Arity5::InternalFn(ref f) => {
                            let mut new_scope: Scope = Vec::new();
                            let result1 = self.call_fn_1_arg("clone", arg1);
                            let result2 = self.call_fn_1_arg("clone", arg2);
                            let result3 = self.call_fn_1_arg("clone", arg3);
                            let result4 = self.call_fn_1_arg("clone", arg4);
                            let result5 = self.call_fn_1_arg("clone", arg5);
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
                    }
                };
                Err(EvalError::FunctionArgMismatch)
            }
            None => Err(EvalError::FunctionNotFound)
        }
    }

    pub fn call_fn_6_arg(&self, name: &str, arg1: &mut Box<Any>, arg2: &mut Box<Any>, arg3: &mut Box<Any>,
        arg4: &mut Box<Any>, arg5: &mut Box<Any>, arg6: &mut Box<Any>) -> Result<Box<Any>, EvalError> {

        match self.fns_arity_6.get(name) {
            Some(vf) => {
                for arr_f in vf {
                    match arr_f {
                        & Arity6::ExternalFn(ref f) => {
                            let invoke = f(arg1, arg2, arg3, arg4, arg5, arg6);
                            match invoke {
                                Ok(v) => return Ok(v),
                                _ => ()
                            }
                        }
                        & Arity6::InternalFn(ref f) => {
                            let mut new_scope: Scope = Vec::new();
                            let result1 = self.call_fn_1_arg("clone", arg1);
                            let result2 = self.call_fn_1_arg("clone", arg2);
                            let result3 = self.call_fn_1_arg("clone", arg3);
                            let result4 = self.call_fn_1_arg("clone", arg4);
                            let result5 = self.call_fn_1_arg("clone", arg5);
                            let result6 = self.call_fn_1_arg("clone", arg6);

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

    fn eval_expr(&self, scope: &mut Scope, expr: &Expr) -> Result<Box<Any>, EvalError> {
        match *expr {
            Expr::IntConst(i) => Ok(Box::new(i)),
            Expr::StringConst(ref s) => Ok(Box::new(s.clone())),
            Expr::Identifier(ref id) => {
                for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                    if *id == *name {                        
                        return self.call_fn_1_arg("clone", val);
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
                    self.call_fn_0_arg(&fn_name)
                }
                else if args.len() == 1 {
                    let mut arg = try!(self.eval_expr(scope, &args[0]));

                    self.call_fn_1_arg(&fn_name, &mut arg)
                }
                else if args.len() == 2 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));

                    self.call_fn_2_arg(&fn_name, &mut arg1, &mut arg2)
                }
                else if args.len() == 3 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));
                    let mut arg3 = try!(self.eval_expr(scope, &args[2]));

                    self.call_fn_3_arg(&fn_name, &mut arg1, &mut arg2, &mut arg3)
                }
                else if args.len() == 4 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));
                    let mut arg3 = try!(self.eval_expr(scope, &args[2]));
                    let mut arg4 = try!(self.eval_expr(scope, &args[3]));

                    self.call_fn_4_arg(&fn_name, &mut arg1, &mut arg2, &mut arg3, &mut arg4)
                }
                else if args.len() == 5 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));
                    let mut arg3 = try!(self.eval_expr(scope, &args[2]));
                    let mut arg4 = try!(self.eval_expr(scope, &args[3]));
                    let mut arg5 = try!(self.eval_expr(scope, &args[4]));

                    self.call_fn_5_arg(&fn_name, &mut arg1, &mut arg2, &mut arg3, &mut arg4, &mut arg5)
                }
                else if args.len() == 6 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));
                    let mut arg3 = try!(self.eval_expr(scope, &args[2]));
                    let mut arg4 = try!(self.eval_expr(scope, &args[3]));
                    let mut arg5 = try!(self.eval_expr(scope, &args[4]));
                    let mut arg6 = try!(self.eval_expr(scope, &args[5]));

                    self.call_fn_6_arg(&fn_name, &mut arg1, &mut arg2, &mut arg3, &mut arg4, &mut arg5, &mut arg6)
                }
                else {
                    Err(EvalError::FunctionCallNotSupported)
                }
            }
            Expr::MethodCall(ref target, ref fn_name, ref args) => {
                if args.len() == 0 {
                    for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                        if *target == *name {
                            return self.call_fn_1_arg(&fn_name, val);
                        }
                    }
                    Err(EvalError::VariableNotFound)
                }
                else if args.len() == 1 {
                    let mut arg = try!(self.eval_expr(scope, &args[0]));

                    for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                        if *target == *name {
                            return self.call_fn_2_arg(&fn_name, val, &mut arg);
                        }
                    }
                    Err(EvalError::VariableNotFound)
                }
                else if args.len() == 2 {
                    let mut arg1 = try!(self.eval_expr(scope, &args[0]));
                    let mut arg2 = try!(self.eval_expr(scope, &args[1]));

                    for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                        if *target == *name {
                            return self.call_fn_3_arg(&fn_name, val, &mut arg1, &mut arg2);
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
                            return self.call_fn_4_arg(&fn_name, val, &mut arg1, &mut arg2, &mut arg3);
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
                            return self.call_fn_5_arg(&fn_name, val, &mut arg1, &mut arg2, &mut arg3, &mut arg4);
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
                            return self.call_fn_6_arg(&fn_name, val, &mut arg1, &mut arg2, &mut arg3, &mut arg4, &mut arg5);
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
                    match f.params.len() {
                        0 => {
                            let name = f.name.clone();
                            let local_f = f.clone();
                            let ent = self.fns_arity_0.entry(name).or_insert(Vec::new());
                            (*ent).push(Arity0::InternalFn(local_f));                                
                        },
                        1 => {
                            let name = f.name.clone();
                            let local_f = f.clone();
                            let ent = self.fns_arity_1.entry(name).or_insert(Vec::new());
                            (*ent).push(Arity1::InternalFn(local_f));                                
                        },
                        2 => {
                            let name = f.name.clone();
                            let local_f = f.clone();
                            let ent = self.fns_arity_2.entry(name).or_insert(Vec::new());
                            (*ent).push(Arity2::InternalFn(local_f));                                
                        },
                        3 => {
                            let name = f.name.clone();
                            let local_f = f.clone();
                            let ent = self.fns_arity_3.entry(name).or_insert(Vec::new());
                            (*ent).push(Arity3::InternalFn(local_f));                                
                        },
                        4 => {
                            let name = f.name.clone();
                            let local_f = f.clone();
                            let ent = self.fns_arity_4.entry(name).or_insert(Vec::new());
                            (*ent).push(Arity4::InternalFn(local_f));                                
                        },
                        5 => {
                            let name = f.name.clone();
                            let local_f = f.clone();
                            let ent = self.fns_arity_5.entry(name).or_insert(Vec::new());
                            (*ent).push(Arity5::InternalFn(local_f));                                
                        },
                        6 => {
                            let name = f.name.clone();
                            let local_f = f.clone();
                            let ent = self.fns_arity_6.entry(name).or_insert(Vec::new());
                            (*ent).push(Arity6::InternalFn(local_f));                                
                        },
                        _ => return Err(EvalError::FunctionArityNotSupported)
                    }
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
            fns_arity_0: HashMap::new(), 
            fns_arity_1: HashMap::new(), 
            fns_arity_2: HashMap::new(), 
            fns_arity_3: HashMap::new(),
            fns_arity_4: HashMap::new(),
            fns_arity_5: HashMap::new(),
            fns_arity_6: HashMap::new()
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
