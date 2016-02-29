use std::collections::HashMap;
use std::error::Error;
use std::any::Any;
use std::boxed::Box;
use std::fmt;

use parser::{lex, parse, Expr, Stmt };
use fn_register::FnRegister;
use ops::{add_boxes, sub_boxes, mult_boxes, div_boxes};

#[derive(Debug)]
pub enum EvalError {
    FunctionNotFound,
    FunctionArgMismatch,
    FunctionCallNotSupported,
    TypeMismatchForOperands,
    IfGuardMismatch,
    VariableNotFound
}

impl Error for EvalError {
    fn description(&self) -> &str {
        match *self {
            EvalError::FunctionNotFound => "Function not found",
            EvalError::FunctionArgMismatch => "Function argument types do not match",
            EvalError::FunctionCallNotSupported => "Function call with > 1 argument not supported",
            EvalError::TypeMismatchForOperands => "Operand types incompatible with operator",
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

pub struct Engine {
    pub fns_arity_1: HashMap<String, Vec<Box<Fn(&mut Box<Any>)->Result<Box<Any>, EvalError>>>>,
    pub fns_arity_0: HashMap<String, Vec<Box<Fn()->Result<Box<Any>, EvalError>>>>,
    pub scope: Vec<(String, Box<Any>)>
}

impl Engine {
    pub fn call_fn_0_arg(&self, name: &str) -> Result<Box<Any>, EvalError> {
        match self.fns_arity_0.get(name) {
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

    pub fn call_fn_1_arg(&self, name: &str, arg1: &mut Box<Any>) -> Result<Box<Any>, EvalError> {
        match self.fns_arity_1.get(name) {
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
            Expr::Call(ref fn_name, ref args) => {
                if args.len() == 0 {
                    self.call_fn_0_arg(&fn_name)
                }
                else if args.len() == 1 {
                    let mut arg = try!(self.eval_expr(&args[0]));

                    self.call_fn_1_arg(&fn_name, &mut arg)
                }
                else {
                    Err(EvalError::FunctionCallNotSupported)
                }
            }
            Expr::Plus(ref a, ref b) => {
                let arg1 = try!(self.eval_expr(a));
                let arg2 = try!(self.eval_expr(b));
                add_boxes(arg1, arg2)
            }
            Expr::Minus(ref a, ref b) => {
                let arg1 = try!(self.eval_expr(a));
                let arg2 = try!(self.eval_expr(b));
                sub_boxes(arg1, arg2)
            }
            Expr::Multiply(ref a, ref b) => {
                let arg1 = try!(self.eval_expr(a));
                let arg2 = try!(self.eval_expr(b));
                mult_boxes(arg1, arg2)
            }
            Expr::Divide(ref a, ref b) => {
                let arg1 = try!(self.eval_expr(a));
                let arg2 = try!(self.eval_expr(b));
                div_boxes(arg1, arg2)
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
            Ok(os) => {
                let mut x: Result<Box<Any>, EvalError> = Ok(Box::new(()));
                for o in os {
                    x = self.eval_stmt(&o)
                }
                x
            }
            Err(_) => Err(EvalError::FunctionArgMismatch),
        }
    }

    pub fn new() -> Engine {
        let mut output = Engine { fns_arity_0: HashMap::new(), fns_arity_1: HashMap::new(), scope: Vec::new() };

        output.register_type::<i32>();
        output.register_type::<u32>();
        output.register_type::<i64>();
        output.register_type::<u64>();
        output.register_type::<f32>();
        output.register_type::<f64>();
        output.register_type::<String>();
        output.register_type::<char>();
        output.register_type::<bool>();

        output
    }
}

