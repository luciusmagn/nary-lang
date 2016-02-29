use std::io::prelude::*;
use std::error::Error;
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
pub enum ParseError {
    BadInput,
    InputPastEndOfFile,
    UnknownOperator,
    MissingRParen,
    MissingLCurly,
    MissingRCurly,
    MalformedCallExpr,
    VarExpectsIdentifier
}

impl Error for ParseError {
    fn description(&self) -> &str {
        match *self {
            ParseError::BadInput => "Unparseable characters in the input stream",
            ParseError::InputPastEndOfFile => "Input past end of file",
            ParseError::UnknownOperator => "Unknown operator",
            ParseError::MissingRParen => "Expected ')'",
            ParseError::MissingLCurly => "Expected '{'",
            ParseError::MissingRCurly => "Expected '}'",
            ParseError::MalformedCallExpr => "Call contains bad expression",
            ParseError::VarExpectsIdentifier => "'var' expects the name of a variable"
        }
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

#[derive(Debug)]
pub enum Stmt { If(Box<Expr>, Box<Stmt>), While(Box<Expr>, Box<Stmt>), Var(String, Option<Box<Expr>>),
    Block(Box<Vec<Stmt>>), Expr(Box<Expr>) }

#[derive(Debug)]
pub enum Expr { IntConst(i32), Identifier(String), Plus(Box<Expr>, Box<Expr>), Minus(Box<Expr>, Box<Expr>), 
    Multiply(Box<Expr>, Box<Expr>), Divide(Box<Expr>, Box<Expr>), Call(String, Box<Vec<Expr>>),
    Assignment(Box<Expr>, Box<Expr>), True, False }

#[derive(Debug)]
pub enum Token { Int(i32), Id(String), LCurly, RCurly, LParen, RParen, LSquare, RSquare,
    Plus, Minus, Multiply, Divide, Semicolon, Colon, Comma, Equals, True, False, Var, If, While }

pub struct TokenIterator<'a> {
    char_stream: Peekable<Chars<'a>>
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(c) = self.char_stream.next() {
            match c {
                '0'...'9' => {
                    let mut result = Vec::new();
                    result.push(c);

                    while let Some(&nxt) = self.char_stream.peek() {
                        match nxt {
                            '0'...'9' => { result.push(nxt); self.char_stream.next(); },
                            _ => break
                        }
                    }

                    let out : String = result.iter().cloned().collect();

                    if let Ok(val) = out.parse::<i32>() {
                        return Some(Token::Int(val));
                    }
                    return None;
                },
                'A'...'Z' | 'a'...'z' | '_' => {
                    let mut result = Vec::new();
                    result.push(c);

                    while let Some(&nxt) = self.char_stream.peek() {
                        match nxt {
                            '0'...'9' | 'A'...'Z' | 'a'...'z' | '_' => { 
                                result.push(nxt); self.char_stream.next(); },
                            _ => break
                        }
                    }

                    let out : String = result.iter().cloned().collect();

                    if out == "true" {
                        return Some(Token::True);
                    }
                    else if out == "false" {
                        return Some(Token::False);
                    }
                    else if out == "var" {
                        return Some(Token::Var);
                    }
                    else if out == "if" {
                        return Some(Token::If);
                    }
                    else if out == "while" {
                        return Some(Token::While);
                    }
                    else {
                        return Some(Token::Id(out));
                    }
                },
                '{' => { return Some(Token::LCurly); },
                '}' => { return Some(Token::RCurly); },
                '(' => { return Some(Token::LParen); },
                ')' => { return Some(Token::RParen); },
                '[' => { return Some(Token::LSquare); },
                ']' => { return Some(Token::RSquare); },
                '+' => { return Some(Token::Plus); },
                '-' => { return Some(Token::Minus); },
                '*' => { return Some(Token::Multiply); },
                '/' => { return Some(Token::Divide); },
                ';' => { return Some(Token::Semicolon); },
                ':' => { return Some(Token::Colon); },
                ',' => { return Some(Token::Comma); },
                '=' => { return Some(Token::Equals); },
                ' ' | '\n' | '\r' => (),
                _ => return None
            }
        }

        None
    }
}

pub fn lex<'a>(input: &'a String) -> TokenIterator<'a> {
    TokenIterator { char_stream: input.chars().peekable() }
}

fn get_precedence(token: &Token) -> i32 {
    match *token {
        Token::Equals => 10,
        Token::Plus => 20,
        Token::Minus => 20,
        Token::Multiply => 40,
        _ => -1
    }
}

fn parse_paren_expr<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    let expr = try!(parse_expr(input));

    match input.next() {
        Some(Token::RParen) => Ok(expr),
        _ => Err(ParseError::MissingRParen)
    }
}

fn parse_ident_expr<'a>(id: String, input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {    
    match input.peek() {
        Some(&Token::LParen) => (),
        _ => return Ok(Expr::Identifier(id))
    }

    input.next();

    let mut args = Vec::new();

    match input.peek() {
        Some(&Token::RParen) => {input.next(); return Ok(Expr::Call(id, Box::new(args)))},
        _ => ()
    }

    loop {
        if let Ok(arg) = parse_expr(input) {
            args.push(arg);
        }
        else {
            return Err(ParseError::MalformedCallExpr);
        }

        match input.peek() {
            Some(&Token::RParen) => {input.next(); return Ok(Expr::Call(id, Box::new(args)))},
            Some(&Token::Comma) => (),
            _ => return Err(ParseError::MalformedCallExpr)
        }

        input.next();
    }
}

fn parse_primary<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    if let Some(token) = input.next() {
        match token {
            Token::Int(ref x) => {Ok(Expr::IntConst(x.clone()))},
            Token::Id(ref s) => {parse_ident_expr(s.clone(), input)},
            Token::LParen => {parse_paren_expr(input)},
            Token::True => {Ok(Expr::True)},
            Token::False => {Ok(Expr::False)},
            _ => {println!("Can't parse: {:?}", token); Err(ParseError::BadInput)}
        }
    }
    else {
        Err(ParseError::InputPastEndOfFile)
    }
}

fn parse_binop<'a>(input: &mut Peekable<TokenIterator<'a>>, prec: i32, lhs: Expr) -> Result<Expr, ParseError> {
    let mut lhs_curr = lhs;

    loop {
        let mut curr_prec = -1;

        if let Some(curr_op) = input.peek() {            
            curr_prec = get_precedence(curr_op);
        }

        if curr_prec < prec {
            return Ok(lhs_curr);
        }

        if let Some(op_token) = input.next() {
            let mut rhs = try!(parse_primary(input));

            let mut next_prec = -1;
            
            if let Some(next_op) = input.peek() {
                next_prec = get_precedence(next_op);
            }

            if curr_prec < next_prec {
                rhs = try!(parse_binop(input, curr_prec+1, rhs));
            }

            lhs_curr = match op_token {
                Token::Plus => Expr::Plus(Box::new(lhs_curr), Box::new(rhs)),
                Token::Minus => Expr::Minus(Box::new(lhs_curr), Box::new(rhs)),
                Token::Multiply => Expr::Multiply(Box::new(lhs_curr), Box::new(rhs)),
                Token::Divide => Expr::Divide(Box::new(lhs_curr), Box::new(rhs)),
                Token::Equals => Expr::Assignment(Box::new(lhs_curr), Box::new(rhs)),
                _ => return Err(ParseError::UnknownOperator)
            };
        }
    }
}

fn parse_expr<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    let lhs = try!(parse_primary(input));

    parse_binop(input, 0, lhs)
}

fn parse_if<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let guard = try!(parse_expr(input));
    let body = try!(parse_block(input));

    Ok(Stmt::If(Box::new(guard), Box::new(body)))
}

fn parse_while<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let guard = try!(parse_expr(input));
    let body = try!(parse_block(input));

    Ok(Stmt::While(Box::new(guard), Box::new(body)))
}

fn parse_var<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let name = match input.next() {
        Some(Token::Id(ref s)) => s.clone(),
        _ => return Err(ParseError::VarExpectsIdentifier)
    };

    match input.peek() {
        Some(&Token::Equals) => {
            input.next();
            let initializer = try!(parse_expr(input));
            Ok(Stmt::Var(name, Some(Box::new(initializer))))
        }
        _ => Ok(Stmt::Var(name, None))
    }
}

fn parse_block<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    match input.peek() {
        Some(& Token::LCurly) => (),
        _ => return Err(ParseError::MissingLCurly)
    }

    input.next();

    let stmts = try!(parse_stmts(input, true));

    match input.peek() {
        Some(& Token::RCurly) => {input.next(); Ok(Stmt::Block(Box::new(stmts)))},
        _ => Err(ParseError::MissingRCurly)
    }
}

fn parse_expr_stmt<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    let expr = try!(parse_expr(input));    
    Ok(Stmt::Expr(Box::new(expr)))
}

fn parse_stmt<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    match input.peek() {
        Some(& Token::If) => parse_if(input),
        Some(& Token::While) => parse_while(input),
        Some(& Token::LCurly) => parse_block(input),
        Some(& Token::Var) => parse_var(input),
        _ => parse_expr_stmt(input)
    }
}

fn parse_stmts<'a>(input: &mut Peekable<TokenIterator<'a>>, check_for_rcurly: bool) -> Result<Vec<Stmt>, ParseError> {
    let mut result = Vec::new();

    if check_for_rcurly {
        match input.peek() {
            Some(& Token::RCurly) => return Ok(result),
            _ => ()
        }        
    }

    while let Some(_) = input.peek() {
        result.push(try!(parse_stmt(input)));
        match input.peek() {
            Some(& Token::Semicolon) => {input.next();},            
            _ => ()
        }

        if check_for_rcurly {
            match input.peek() {
                Some(& Token::RCurly) => return Ok(result),
                _ => ()
            }
        }
    }

    Ok(result)
}

pub fn parse<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Vec<Stmt>, ParseError> {
    let result = parse_stmts(input, false);
    result
}
