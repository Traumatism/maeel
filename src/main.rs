#![allow(clippy::derivable_impls)]

use std::cmp::PartialEq;
use std::collections::BTreeMap;
use std::env::args;
use std::fmt::Debug;
use std::fs::read_to_string;
use std::mem::replace;
use std::ops::{Add, BitAnd, BitOr, BitXor, Mul, Not, Sub};
use std::process::exit;
use std::slice::Iter;
use std::string::ToString;

const DEBUG: bool = false;

macro_rules! debug {
    ($content:expr) => {
        if DEBUG {
            println!("[DEBUG] {}", $content);
        }
    };
}

/// Tokens (type, line number)
#[derive(Debug)]
enum Token {
    Float(f64, u16),
    Integer(i64, u16),
    Str(String, u16),
    Identifier(String, u16),
    Bool(bool, u16),
    Let(u16),
    If(u16),
    Add(u16),
    Sub(u16),
    Mul(u16),
    And(u16),
    Or(u16),
    Xor(u16),
    Not(u16),
    Eq(u16),
    Separator,
    ProcStart,
    ProcEnd,
}

/// Clone trait implementation for each token
impl Clone for Token {
    fn clone(&self) -> Self {
        match self {
            Self::Float(x, l) => Self::Float(*x, *l),
            Self::Integer(n, l) => Self::Integer(*n, *l),
            Self::Str(c, l) => Self::Str(c.clone(), *l),
            Self::Identifier(c, l) => Self::Identifier(c.clone(), *l),
            Self::Bool(p, l) => Self::Bool(*p, *l),
            Self::Let(l) => Self::Let(*l),
            Self::If(l) => Self::If(*l),
            Self::Add(l) => Self::Add(*l),
            Self::Sub(l) => Self::Sub(*l),
            Self::Mul(l) => Self::Mul(*l),
            Self::And(l) => Self::And(*l),
            Self::Or(l) => Self::Or(*l),
            Self::Xor(l) => Self::Xor(*l),
            Self::Not(l) => Self::Not(*l),
            Self::Eq(l) => Self::Eq(*l),
            Self::Separator => Self::Separator,
            Self::ProcStart => Self::ProcStart,
            Self::ProcEnd => Self::ProcEnd,
        }
    }
}

/// Every single type VM can work with
enum VMType {
    Float(f64),
    Integer(i64),
    Str(String),
    Bool(bool),
    Array(Vec<VMType>),
}

/// PartialOrd trait implementation for all types (perform comparaisons)
impl PartialOrd for VMType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (VMType::Integer(m), VMType::Integer(n)) => Some(m.cmp(n)),
            (VMType::Float(x), VMType::Float(y)) => Some(x.total_cmp(y)),
            (VMType::Float(x), VMType::Integer(n)) => Some(x.total_cmp(&(*n as f64))),
            (VMType::Integer(n), VMType::Float(x)) => Some(x.total_cmp(&(*n as f64))),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

/// PartialEq trait implementation for all types
impl PartialEq for VMType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VMType::Str(a), VMType::Str(b)) => a == b,
            (VMType::Array(a), VMType::Array(b)) => a == b,
            (VMType::Float(x), VMType::Integer(n)) => *x == (*n as f64),
            (VMType::Integer(n), VMType::Float(x)) => (*n as f64) == *x,
            (VMType::Integer(m), VMType::Integer(n)) => m == n,
            (VMType::Float(x), VMType::Float(y)) => x == y,
            (VMType::Bool(p), VMType::Bool(q)) => p == q,
            _ => false,
        }
    }
}

/// Not operation trait implementation for all types
impl Not for VMType {
    type Output = VMType;

    fn not(self) -> Self::Output {
        match self {
            VMType::Float(x) => VMType::Float(x * -1.),
            VMType::Integer(n) => VMType::Integer(-n),
            VMType::Bool(p) => VMType::Bool(!p),
            a => panic!("can't invert {a:?}"),
        }
    }
}

/// Bitwise and operation trait implementation for all types
impl BitAnd for VMType {
    type Output = VMType;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p & q),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

/// Bitwise or operation trait implementation for all types
impl BitOr for VMType {
    type Output = VMType;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p | q),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

/// Bitwise xor operation trait implementation for all types
impl BitXor for VMType {
    type Output = VMType;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p ^ q),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

/// Sub operation trait implementation for all types
impl Sub for VMType {
    type Output = VMType;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m - n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x - y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 - x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x - n as f64),
            (a, b) => panic!("can't sub {a:?} and {b:?}"),
        }
    }
}

/// Mul operation trait implementation for all types
impl Mul for VMType {
    type Output = VMType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m * n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x * y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 * x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x * n as f64),
            (a, b) => panic!("can't mul {a:?} and {b:?}"),
        }
    }
}

/// Add operation trait implementation for all types
impl Add for VMType {
    type Output = VMType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Str(s), VMType::Str(t)) => VMType::Str(s + &t),
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m + n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x + y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 + x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x + n as f64),
            (other, VMType::Array(mut array)) => {
                array.push(other);
                VMType::Array(array)
            }
            (VMType::Array(mut array), other) => {
                array.push(other);
                VMType::Array(array)
            }
            (a, b) => panic!("can't add {a:?} and {b:?}"),
        }
    }
}

/// ToString trait implementation for all types (convert types => string)
impl ToString for VMType {
    fn to_string(&self) -> String {
        match self {
            VMType::Float(x) => x.to_string(),
            VMType::Integer(n) => n.to_string(),
            VMType::Str(c) => c.to_string(),
            VMType::Bool(p) => p.to_string(),
            VMType::Array(array) => format!("{array:?}"),
        }
    }
}

/// Debug trait implementation for all types (ability to display types)
impl Debug for VMType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

/// Cloe trait implementation for all types
impl Clone for VMType {
    fn clone(&self) -> Self {
        match self {
            Self::Bool(p) => Self::Bool(*p),
            Self::Float(x) => Self::Float(*x),
            Self::Integer(n) => Self::Integer(*n),
            Self::Str(c) => Self::Str(c.clone()),
            Self::Array(array) => Self::Array(array.clone()),
        }
    }
}

/// A frame in a stack
struct Frame(VMType, Option<Box<Frame>>);

impl Frame {
    pub fn new(value: VMType) -> Frame {
        Self(value, None)
    }
}

impl Clone for Frame {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

/// Stack data structure implementation
#[derive(Clone)]
struct Stack {
    pub head: Option<Frame>,
}

impl Default for Stack {
    fn default() -> Stack {
        Stack { head: None }
    }
}

impl Stack {
    /// Duplicate the head value
    pub fn dup(&mut self) {
        let a = self.pop().unwrap();
        self.push(a.clone());
        self.push(a)
    }
}

impl From<Vec<VMType>> for Stack {
    fn from(value: Vec<VMType>) -> Self {
        let mut new_stack = Stack::default();
        let mut cloned_value = value;

        cloned_value.reverse();

        cloned_value
            .iter()
            .for_each(|ele| new_stack.push(ele.clone()));

        new_stack
    }
}

impl Stack {
    /// Clear all the stack values
    pub fn clear(&mut self) {
        while self.head.is_some() {
            self.pop();
        }
    }

    /// Swap the two head values
    pub fn swap(&mut self) {
        let a = self.pop().unwrap();
        let b = self.pop().unwrap();

        self.push(a);
        self.push(b);
    }

    /// Push a new value to the stack
    pub fn push(&mut self, value: VMType) {
        let mut node = Frame::new(value);

        if let Some(stack) = replace(&mut self.head, None) {
            node.1 = Some(Box::new(stack))
        }

        self.head = Some(node);
    }

    /// Pop the head value and return it in an Option
    pub fn pop(&mut self) -> Option<VMType> {
        match replace(&mut self.head, None) {
            Some(stack) => {
                self.head = stack.1.map(|n| *n);
                Some(stack.0)
            }
            _ => None,
        }
    }
}

/// Iterate through values with the ability
/// to go back and get previous values
struct Peeker<T: Clone> {
    values: Vec<T>,
    cursor: usize,
}

impl<T: Clone> Iterator for Peeker<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if self.cursor >= self.values.len() {
            return None;
        }

        self.cursor += 1;

        Some(self.values[self.cursor - 1].to_owned())
    }
}

impl<T: Clone> Peeker<T> {
    pub fn new(values: Vec<T>) -> Self {
        Self { values, cursor: 0 }
    }

    pub fn previous(&mut self) -> Option<T> {
        self.cursor -= 2;
        self.next()
    }
}

/// Perform lexical analysis on the code
fn lex_into_tokens(code: &str) -> Vec<Token> {
    let mut chars = Peeker::new(code.chars().collect());
    let mut tokens = Vec::new();
    let mut line = 1;

    while let Some(char) = chars.next() {
        match char {
            '&' => tokens.push(Token::And(line)),
            '|' => tokens.push(Token::Or(line)),
            '^' => tokens.push(Token::Xor(line)),
            '+' => tokens.push(Token::Add(line)),
            '*' => tokens.push(Token::Mul(line)),
            '-' => tokens.push(Token::Sub(line)),
            '=' => tokens.push(Token::Eq(line)),
            '!' => tokens.push(Token::Not(line)),
            ' ' => (),
            // will be used to split the code into instructions
            ';' => tokens.push(Token::Separator),
            // will be used in error messages
            '\n' => line += 1,
            // comments
            '@' => {
                for next in chars.by_ref() {
                    if next == '\n' {
                        break;
                    }
                }
            }
            // strings
            '"' => {
                let mut content = String::new();

                for next in chars.by_ref() {
                    match next {
                        '"' => break,
                        _ => content.push(next),
                    }
                }

                tokens.push(Token::Str(content, line))
            }
            // identifiers
            'a'..='z' | '_' => {
                let mut content = String::from(char);

                while let Some(next) = chars.next() {
                    match next {
                        'a'..='z' | '_' => content.push(next),
                        _ => {
                            chars.previous();
                            break;
                        }
                    }
                }

                tokens.push(match &(*content) {
                    "true" => Token::Bool(true, line),
                    "false" => Token::Bool(false, line),
                    "end" => Token::Separator,
                    "proc" => Token::ProcStart,
                    "end_proc" => Token::ProcEnd,
                    "if" => Token::If(line),
                    "let" => Token::Let(line),
                    _ => Token::Identifier(content, line),
                });
            }
            // integers/floats
            '0'..='9' => {
                let mut content = String::from(char);
                let mut float = false;

                while let Some(next) = chars.next() {
                    match next {
                        '0'..='9' => content.push(next),
                        '.' => {
                            float = true;
                            content.push('.')
                        }
                        '_' => (),
                        _ => {
                            chars.previous();
                            break;
                        }
                    }
                }

                tokens.push(match float {
                    true => Token::Float(content.parse::<f64>().unwrap(), line),
                    false => Token::Integer(content.parse::<i64>().unwrap(), line),
                });
            }
            _ => panic!(), // unknown symbol ?
        }
    }

    tokens
}

/// Evaluate the instructions (runtime)
struct Parser {
    stop_proc_execution: bool,
    in_proc: bool,
    procs: BTreeMap<String, Vec<Token>>,
    vars: BTreeMap<String, VMType>,
    stack: Stack,
}

impl Default for Parser {
    fn default() -> Self {
        Self {
            stop_proc_execution: false,
            in_proc: false,
            procs: BTreeMap::default(),
            vars: BTreeMap::default(),
            stack: Stack::default(),
        }
    }
}

impl Parser {
    pub fn new(
        in_proc: bool,
        procs: BTreeMap<String, Vec<Token>>,
        vars: BTreeMap<String, VMType>,
        stack: Stack,
    ) -> Self {
        Self {
            stop_proc_execution: false,
            in_proc,
            procs,
            vars,
            stack,
        }
    }

    /// Parse a single instruction
    pub fn parse_instruction(&mut self, tokens: &mut Iter<Token>) {
        while let Some(token) = tokens.next() {
            match token.clone() {
                Token::Separator | Token::ProcEnd => (),

                Token::Bool(p, _) => self.stack.push(VMType::Bool(p)),

                Token::Str(content, _) => self.stack.push(VMType::Str(content)),

                Token::Integer(n, _) => self.stack.push(VMType::Integer(n)),

                Token::Float(x, _) => self.stack.push(VMType::Float(x)),

                // Parse new procedure definition
                Token::ProcStart => {
                    debug!("parsing proc...");

                    let proc_name = match tokens.next().unwrap() {
                        Token::Identifier(name, _) => name,
                        _ => panic!(),
                    };

                    debug!(format!(" -> {proc_name}"));

                    let mut proc_tokens = Vec::new();

                    for token in tokens.by_ref() {
                        match token {
                            Token::ProcEnd => break,
                            _ => proc_tokens.push(token.clone()),
                        }
                    }

                    debug!(format!("pushing new proc: {proc_name}"));

                    self.procs.insert(proc_name.clone(), proc_tokens);
                }

                // Parse if statement
                Token::If(line) => {
                    match self.stack.pop() {
                        Some(VMType::Bool(true)) => (),
                        Some(VMType::Bool(false)) => return,
                        _ => panic!(
                            "line {line}: `if` requires a boolean value on the top of the stack!"
                        ),
                    };
                }

                // Parse an add instruction
                //
                // 1 + 1 => 2
                // 1.0 + 1.0 => 2.0
                // 1.0 + 1 => 2.0
                Token::Add(line) => {
                    let a = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `add` requires two values on the top of the stack!")
                    });

                    let b = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `add` requires two values on the top of the stack!")
                    });

                    self.stack.push(a.add(b))
                }

                // Parse a mul instruction
                //
                // 2 * 1 => 2
                // 2.0 * 1.0 => 2.0
                // 2.0 * 1 => 2.0
                Token::Mul(line) => {
                    let a = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `mul` requires two values on the top of the stack!")
                    });

                    let b = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `mul` requires two values on the top of the stack!")
                    });

                    self.stack.push(a.mul(b))
                }

                Token::Let(line) => {
                    let name = match tokens.next() {
                        Some(Token::Identifier(name, _)) => name,
                        _ => panic!("line {line}: syntax: `let name value` with value of type int|float|string or a pop|dup instruction!"),
                    };

                    let value = match tokens.next() {
                        Some(Token::Str(content, _)) => VMType::Str(content.clone()),
                        Some(Token::Integer(n, _)) => VMType::Integer(*n),
                        Some(Token::Float(x, _)) => VMType::Float(*x),
                        Some(Token::Bool(p, _)) => VMType::Bool(*p),
                        Some(Token::Identifier(instruction, _)) => {
                            match &**instruction {
                                "pop" => self.stack.pop().unwrap_or_else(|| panic!("line {line}: let name pop requires a value on the top of the stack")),
                                "dup" => { self.stack.dup(); self.stack.pop().unwrap_or_else(|| panic!("line {line}: let name pop requires a value on the top of the stack")) }
                                _ => panic!("line {line}: syntax: `let name value;` with value of type int|float|string or a pop|dup instruction! (1)"), 
                            }
                        },
                        _ => panic!("line {line}: syntax: `let name value;` with value of type int|float|string or a pop|dup instruction! (2)"),
                    };

                    self.vars.insert(name.clone(), value);
                }

                // Parse a not instruction
                //
                // 0 => 1
                // 1 => 0
                Token::Not(line) => {
                    let p = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `not` requires one value on the top of the stack!")
                    });

                    self.stack.push(!p)
                }

                // Parse an and instruction
                //
                // 0 + 0 => 0
                // 0 + 1 => 0
                // 1 + 0 => 0
                // 1 + 1 => 1
                Token::And(line) => {
                    let p = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `and` requires two values on the top of the stack!")
                    });

                    let q = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `and` requires two values on the top of the stack!")
                    });

                    self.stack.push(p & q)
                }

                // Parse an or instruction
                //
                // 0 + 0 => 0
                // 0 + 1 => 1
                // 1 + 0 => 1
                // 1 + 1 => 1
                Token::Or(line) => {
                    let p = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `or` requires two values on the top of the stack!")
                    });

                    let q = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `or` requires two values on the top of the stack!")
                    });

                    self.stack.push(p | q)
                }

                // Parse a xor instruction
                //
                // 0 + 0 => 0
                // 0 + 1 => 1
                // 1 + 0 => 1
                // 1 + 1 => 1
                Token::Xor(line) => {
                    let p = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `xor` requires two values on the top of the stack!")
                    });

                    let q = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `xor` requires two values on the top of the stack!")
                    });

                    self.stack.push(p ^ q)
                }

                // Parse an equality instruction
                //
                // 0 + 0 => 1
                // 0 + 1 => 0
                // 1 + 0 => 0
                // 1 + 1 => 1
                Token::Eq(line) => {
                    let a = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `eq` requires two values on the top of the stack!")
                    });

                    let b = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `eq` requires two values on the top of the stack!")
                    });

                    self.stack.push(VMType::Bool(a == b))
                }

                // Parse a sub instruction
                //
                // 3 - 1 => 2
                // 3.0 - 1.0 => 2.0
                // 3.0 - 1 => 2.0
                Token::Sub(line) => {
                    let a = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `sub` requires two values on the top of the stack!")
                    });

                    let b = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `sub` requires two values on the top of the stack!")
                    });

                    self.stack.push(b.sub(a))
                }

                Token::Identifier(identifier, line) => {
                    match &*identifier {
                        "return" => {
                            if !self.in_proc {
                                panic!()
                            }

                            self.stop_proc_execution = true
                        }

                        "del" => {
                            let name = match tokens.next() {
                                Some(Token::Identifier(name, _)) => name,
                                _ => panic!("line {line}: syntax: `del name`"),
                            };

                            self.vars.remove(name);
                        }

                        "exit" => match self.stack.pop().unwrap() {
                            VMType::Integer(status) => exit(status as i32),
                            _ => panic!(),
                        },

                        // Parse a dup operation
                        "dup" => self.stack.dup(),

                        // Parse a swap operation
                        "swap" => self.stack.swap(),

                        // Parse a clear operation
                        "clear" => self.stack.clear(),

                        // Parse a pop operation
                        "pop" => {
                            self.stack.pop();
                        }

                        // Parse a sum instruction
                        //
                        // [a, b, c, ..., n] => a + b + c + ... + n
                        "sum" => match self.stack.pop() {
                            Some(VMType::Array(array)) => {
                                let sum = array
                                    .iter()
                                    .map(|element| match element {
                                            VMType::Integer(n) => *n as f64,
                                            VMType::Float(x) => *x,
                                            _ => panic!("line {line}: `sum` requires [integer|float] on the stack!"),
                                    })
                                    .sum::<f64>();

                                self.stack.push(VMType::Float(sum))
                            }
                            _ => {
                                panic!("line {line}: `sum` requires [integer|float] on the stack!")
                            }
                        },

                        // Parse a join instruction
                        //
                        // Join an array of string around a string
                        "join" => {
                            let Some(VMType::Str(join_string)) = self.stack.pop() else { panic!("line {line}: `join` requires a string and a [string] on the stack!") };

                            let joined = match self.stack.pop() {
                                Some(VMType::Array(array)) => array
                                    .iter()
                                    .map(|element: &VMType| -> String {
                                        match element {
                                            VMType::Str(content) => content.clone(),
                                            _ => panic!(
                                                "line {line}: `join` requires a string and a [string] on the stack!"
                                            ),
                                        }
                                    })
                                    .collect::<Vec<String>>()
                                    .join(&join_string),
                                _ => panic!("line {line}: `join` requires a string and a [string] on the stack!"),
                            };

                            self.stack.push(VMType::Str(joined));
                        }

                        // Parse a take instruction
                        //
                        // |a|b|c|3| => [a, b, c]
                        // |a|b|c|2| => [b, c]
                        "take" => match self.stack.pop() {
                            Some(VMType::Integer(n)) => {
                                let array = (0..n)
                                    .map(|_| {
                                        self.stack.pop().unwrap_or_else(|| {
                                            panic!("line {line}: no more values on the stack!")
                                        })
                                    })
                                    .collect();
                                self.stack.push(VMType::Array(array))
                            }
                            _ => panic!("line {line}: `take` requires an integer on the stack!"),
                        },

                        // Parse a reverse instruction
                        //
                        // [a, b, c] => [c, b, a]
                        "reverse" => match self.stack.pop() {
                            Some(VMType::Array(mut array)) => {
                                array.reverse();
                                self.stack.push(VMType::Array(array))
                            }
                            _ => {
                                panic!("line {line}: `reverse` expect an array on top of the stack")
                            }
                        },

                        // Parse a product instruction
                        //
                        // [a, b, c, ... , n] => a * b * c * ... * n
                        "product" => {
                            match self.stack.pop() {
                                Some(VMType::Array(array)) => {
                                    let product = array
                                        .iter()
                                        .map(|element| match element {
                                            VMType::Integer(n) => *n as f64,
                                            VMType::Float(x) => *x,
                                            _ => {
                                                panic!("line {line}: `product` requires [integer|float] on the stack!")
                                            }
                                        })
                                        .product::<f64>();

                                    self.stack.push(VMType::Float(product));
                                }
                                _ => {
                                    panic!("line {line}: `product` requires [integer|float] on the stack!")
                                }
                            }
                        }

                        // Parse a parse_int instruction
                        //
                        // Convert string to integers (TODO: implement floats)
                        "parse_int" => match self.stack.pop() {
                            Some(VMType::Str(string)) => self
                                .stack
                                .push(VMType::Integer(string.parse::<i64>().unwrap())),
                            _ => panic!("line {line}: `parse_int` requires string on the stack!"),
                        },

                        // Parse a range instruction
                        //
                        // Generate sets of integers
                        "range" | "erange" => {
                            let (b, a) = match (self.stack.pop(), self.stack.pop()) {
                                (Some(VMType::Integer(b1)), Some(VMType::Integer(a1))) => (b1, a1),
                                _ => {
                                    panic!(
                                        "line {line}: `range` requires two integers on the stack!"
                                    )
                                }
                            };

                            let range: Vec<i64> = match &*identifier {
                                "erange" => (a..=b).collect(),
                                "range" => (a..b).collect(),
                                _ => panic!(),
                            };

                            self.stack.push(VMType::Array(
                                range
                                    .iter()
                                    .map(|n| VMType::Integer(*n))
                                    .collect::<Vec<VMType>>(),
                            ))
                        }

                        // Parse a print/println instruction
                        //
                        // Print stack head to stdout
                        "print" | "println" => {
                            let e = self.stack.pop().unwrap_or_else(|| {
                                panic!("line {line}: `{identifier}` requires string|integer|float on the stack!")
                            });

                            (match &*identifier {
                                "println" => |content| println!("{content}"),
                                "print" => |content| print!("{content}"),
                                _ => panic!(),
                            })(e.to_string());

                            self.stack.push(e);
                        }

                        identifier => {
                            debug!(format!("looking for cached var: {identifier}"));

                            let v = self.vars.get(identifier);

                            if let Some(value) = v {
                                debug!(format!("pushing variable: {identifier}"));
                                self.stack.push(value.clone());
                                continue;
                            }

                            debug!(format!("looking for cached proc: {identifier}"));

                            let proc_tokens = self.procs.get(identifier).expect(identifier).iter();

                            debug!(format!("handling new proc: {identifier}"));

                            let mut instructions = Vec::<Vec<Token>>::default();
                            let mut current_instruction = Vec::<Token>::default();

                            for next in proc_tokens {
                                match next.clone() {
                                    Token::ProcStart => {
                                        current_instruction.push(next.clone());

                                        for next_p in tokens.by_ref() {
                                            match next_p {
                                                Token::ProcEnd => break,
                                                _ => current_instruction.push(next_p.clone()),
                                            }
                                        }

                                        current_instruction.push(Token::ProcEnd);
                                    }
                                    Token::Separator => {
                                        current_instruction.push(Token::Separator);
                                        instructions.push(current_instruction);
                                        current_instruction = Vec::default()
                                    }
                                    _ => current_instruction.push(next.clone()),
                                }
                            }

                            instructions.push(current_instruction);

                            let mut proc_parser = Parser::new(
                                true,
                                self.procs.clone(),
                                self.vars.clone(),
                                self.stack.clone(),
                            );

                            let instructions_iter = instructions.iter();

                            for instruction in instructions_iter {
                                if proc_parser.stop_proc_execution {
                                    debug!("stopping procedure execution...");
                                    break;
                                }

                                proc_parser.parse_instruction(&mut instruction.iter())
                            }

                            debug!("updating vars...");

                            self.stack = proc_parser.stack;
                            self.vars = proc_parser.vars;
                        }
                    }
                }
            }
        }
    }
}

fn main() {
    let args = args().collect::<Vec<String>>();
    let content = read_to_string(args.get(1).unwrap()).expect("Failed to open file");

    let binding = lex_into_tokens(&content);
    let mut tokens = binding.iter();

    let mut instructions = Vec::<Vec<Token>>::default();
    let mut current_instruction = Vec::<Token>::default();

    while let Some(next) = tokens.next() {
        match next.clone() {
            Token::ProcStart => {
                current_instruction.push(next.clone());

                for next_p in tokens.by_ref() {
                    match next_p {
                        Token::ProcEnd => break,
                        _ => current_instruction.push(next_p.clone()),
                    }
                }

                current_instruction.push(Token::ProcEnd);
            }
            Token::Separator => {
                current_instruction.push(Token::Separator);
                instructions.push(current_instruction);
                current_instruction = Vec::default()
            }
            _ => current_instruction.push(next.clone()),
        }
    }

    instructions.push(current_instruction);

    let mut parser = Parser::default();

    for instruction in instructions {
        parser.parse_instruction(&mut instruction.iter())
    }
}
