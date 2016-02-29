use std::any::Any;
use std::boxed::Box;

use engine::{EvalError};

pub fn add_boxes(arg1: Box<Any>, arg2: Box<Any>) -> Result<Box<Any>, EvalError> {    
    match arg1.downcast::<i32>() {
        Ok(v1) => {
            if let Ok(v2) = arg2.downcast::<i32>() {
                return Ok(Box::new(*v1 + *v2))
            }
            else {
                return Err(EvalError::TypeMismatchForOperands)
            }
        },
        Err(arg1_again) => {
            match arg1_again.downcast::<u32>() {
                Ok(v1) => {
                    if let Ok(v2) = arg2.downcast::<u32>() {
                        return Ok(Box::new(*v1 + *v2))
                    }
                    else {
                        return Err(EvalError::TypeMismatchForOperands)
                    }
                },
                Err(arg1_again) => { 
                    match arg1_again.downcast::<i64>() {
                        Ok(v1) => {
                            if let Ok(v2) = arg2.downcast::<i64>() {
                                return Ok(Box::new(*v1 + *v2))
                            }
                            else {
                                return Err(EvalError::TypeMismatchForOperands)
                            }
                        },
                        Err(arg1_again) => { 
                            match arg1_again.downcast::<u64>() {
                                Ok(v1) => {
                                    if let Ok(v2) = arg2.downcast::<u64>() {
                                        return Ok(Box::new(*v1 + *v2))
                                    }
                                    else {
                                        return Err(EvalError::TypeMismatchForOperands)
                                    }
                                },
                                Err(arg1_again) => { 
                                    match arg1_again.downcast::<f32>() {
                                        Ok(v1) => {
                                            if let Ok(v2) = arg2.downcast::<f32>() {
                                                return Ok(Box::new(*v1 + *v2))
                                            }
                                            else {
                                                return Err(EvalError::TypeMismatchForOperands)
                                            }
                                        },
                                        Err(arg1_again) => { 
                                            match arg1_again.downcast::<f64>() {
                                                Ok(v1) => {
                                                    if let Ok(v2) = arg2.downcast::<f64>() {
                                                        return Ok(Box::new(*v1 + *v2))
                                                    }
                                                    else {
                                                        return Err(EvalError::TypeMismatchForOperands)
                                                    }
                                                },
                                                Err(_) => { 
                                                    return Err(EvalError::TypeMismatchForOperands) 
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn sub_boxes(arg1: Box<Any>, arg2: Box<Any>) -> Result<Box<Any>, EvalError> {    
    match arg1.downcast::<i32>() {
        Ok(v1) => {
            if let Ok(v2) = arg2.downcast::<i32>() {
                return Ok(Box::new(*v1 - *v2))
            }
            else {
                return Err(EvalError::TypeMismatchForOperands)
            }
        },
        Err(arg1_again) => {
            match arg1_again.downcast::<u32>() {
                Ok(v1) => {
                    if let Ok(v2) = arg2.downcast::<u32>() {
                        return Ok(Box::new(*v1 - *v2))
                    }
                    else {
                        return Err(EvalError::TypeMismatchForOperands)
                    }
                },
                Err(arg1_again) => { 
                    match arg1_again.downcast::<i64>() {
                        Ok(v1) => {
                            if let Ok(v2) = arg2.downcast::<i64>() {
                                return Ok(Box::new(*v1 - *v2))
                            }
                            else {
                                return Err(EvalError::TypeMismatchForOperands)
                            }
                        },
                        Err(arg1_again) => { 
                            match arg1_again.downcast::<u64>() {
                                Ok(v1) => {
                                    if let Ok(v2) = arg2.downcast::<u64>() {
                                        return Ok(Box::new(*v1 - *v2))
                                    }
                                    else {
                                        return Err(EvalError::TypeMismatchForOperands)
                                    }
                                },
                                Err(arg1_again) => { 
                                    match arg1_again.downcast::<f32>() {
                                        Ok(v1) => {
                                            if let Ok(v2) = arg2.downcast::<f32>() {
                                                return Ok(Box::new(*v1 - *v2))
                                            }
                                            else {
                                                return Err(EvalError::TypeMismatchForOperands)
                                            }
                                        },
                                        Err(arg1_again) => { 
                                            match arg1_again.downcast::<f64>() {
                                                Ok(v1) => {
                                                    if let Ok(v2) = arg2.downcast::<f64>() {
                                                        return Ok(Box::new(*v1 - *v2))
                                                    }
                                                    else {
                                                        return Err(EvalError::TypeMismatchForOperands)
                                                    }
                                                },
                                                Err(_) => { 
                                                    return Err(EvalError::TypeMismatchForOperands) 
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn mult_boxes(arg1: Box<Any>, arg2: Box<Any>) -> Result<Box<Any>, EvalError> {    
    match arg1.downcast::<i32>() {
        Ok(v1) => {
            if let Ok(v2) = arg2.downcast::<i32>() {
                return Ok(Box::new(*v1 * *v2))
            }
            else {
                return Err(EvalError::TypeMismatchForOperands)
            }
        },
        Err(arg1_again) => {
            match arg1_again.downcast::<u32>() {
                Ok(v1) => {
                    if let Ok(v2) = arg2.downcast::<u32>() {
                        return Ok(Box::new(*v1 * *v2))
                    }
                    else {
                        return Err(EvalError::TypeMismatchForOperands)
                    }
                },
                Err(arg1_again) => { 
                    match arg1_again.downcast::<i64>() {
                        Ok(v1) => {
                            if let Ok(v2) = arg2.downcast::<i64>() {
                                return Ok(Box::new(*v1 * *v2))
                            }
                            else {
                                return Err(EvalError::TypeMismatchForOperands)
                            }
                        },
                        Err(arg1_again) => { 
                            match arg1_again.downcast::<u64>() {
                                Ok(v1) => {
                                    if let Ok(v2) = arg2.downcast::<u64>() {
                                        return Ok(Box::new(*v1 * *v2))
                                    }
                                    else {
                                        return Err(EvalError::TypeMismatchForOperands)
                                    }
                                },
                                Err(arg1_again) => { 
                                    match arg1_again.downcast::<f32>() {
                                        Ok(v1) => {
                                            if let Ok(v2) = arg2.downcast::<f32>() {
                                                return Ok(Box::new(*v1 * *v2))
                                            }
                                            else {
                                                return Err(EvalError::TypeMismatchForOperands)
                                            }
                                        },
                                        Err(arg1_again) => { 
                                            match arg1_again.downcast::<f64>() {
                                                Ok(v1) => {
                                                    if let Ok(v2) = arg2.downcast::<f64>() {
                                                        return Ok(Box::new(*v1 * *v2))
                                                    }
                                                    else {
                                                        return Err(EvalError::TypeMismatchForOperands)
                                                    }
                                                },
                                                Err(_) => { 
                                                    return Err(EvalError::TypeMismatchForOperands) 
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn div_boxes(arg1: Box<Any>, arg2: Box<Any>) -> Result<Box<Any>, EvalError> {    
    match arg1.downcast::<i32>() {
        Ok(v1) => {
            if let Ok(v2) = arg2.downcast::<i32>() {
                return Ok(Box::new(*v1 / *v2))
            }
            else {
                return Err(EvalError::TypeMismatchForOperands)
            }
        },
        Err(arg1_again) => {
            match arg1_again.downcast::<u32>() {
                Ok(v1) => {
                    if let Ok(v2) = arg2.downcast::<u32>() {
                        return Ok(Box::new(*v1 / *v2))
                    }
                    else {
                        return Err(EvalError::TypeMismatchForOperands)
                    }
                },
                Err(arg1_again) => { 
                    match arg1_again.downcast::<i64>() {
                        Ok(v1) => {
                            if let Ok(v2) = arg2.downcast::<i64>() {
                                return Ok(Box::new(*v1 / *v2))
                            }
                            else {
                                return Err(EvalError::TypeMismatchForOperands)
                            }
                        },
                        Err(arg1_again) => { 
                            match arg1_again.downcast::<u64>() {
                                Ok(v1) => {
                                    if let Ok(v2) = arg2.downcast::<u64>() {
                                        return Ok(Box::new(*v1 / *v2))
                                    }
                                    else {
                                        return Err(EvalError::TypeMismatchForOperands)
                                    }
                                },
                                Err(arg1_again) => { 
                                    match arg1_again.downcast::<f32>() {
                                        Ok(v1) => {
                                            if let Ok(v2) = arg2.downcast::<f32>() {
                                                return Ok(Box::new(*v1 / *v2))
                                            }
                                            else {
                                                return Err(EvalError::TypeMismatchForOperands)
                                            }
                                        },
                                        Err(arg1_again) => { 
                                            match arg1_again.downcast::<f64>() {
                                                Ok(v1) => {
                                                    if let Ok(v2) = arg2.downcast::<f64>() {
                                                        return Ok(Box::new(*v1 / *v2))
                                                    }
                                                    else {
                                                        return Err(EvalError::TypeMismatchForOperands)
                                                    }
                                                },
                                                Err(_) => { 
                                                    return Err(EvalError::TypeMismatchForOperands) 
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
