use std::cmp::PartialEq;
use std::collections::BTreeMap;
use std::env::args;
use std::fs::read_to_string;
use std::mem::replace;
use std::ops::{Add, BitAnd, BitOr, BitXor, Mul, Not, Sub};
use std::string::ToString;

/// Tokens (type, line number)
enum Token {
    Float(f64, u16),
    Integer(i64, u16),
    Str(String, u16),
    Identifier(String, u16),
    Bool(bool, u16),
    Separator,
}

/// Every single type VM can work with
enum VMType {
    Float(f64),
    Integer(i64),
    Str(String),
    Bool(bool),
    Array(Vec<VMType>),
}

impl PartialOrd for VMType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (VMType::Integer(m), VMType::Integer(n)) => Some(m.cmp(n)),
            (VMType::Float(x), VMType::Float(y)) => Some(x.total_cmp(y)),
            (VMType::Float(x), VMType::Integer(n)) => Some(x.total_cmp(&(*n as f64))),
            (VMType::Integer(n), VMType::Float(x)) => Some(x.total_cmp(&(*n as f64))),
            _ => panic!(),
        }
    }
}

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

impl Not for VMType {
    type Output = VMType;

    fn not(self) -> Self::Output {
        match self {
            VMType::Float(x) => VMType::Float(x * -1.),
            VMType::Integer(n) => VMType::Integer(-n),
            VMType::Bool(p) => VMType::Bool(!p),
            _ => panic!(),
        }
    }
}

impl BitAnd for VMType {
    type Output = VMType;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p & q),
            _ => panic!(),
        }
    }
}

impl BitOr for VMType {
    type Output = VMType;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p | q),
            _ => panic!(),
        }
    }
}

impl BitXor for VMType {
    type Output = VMType;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p ^ q),
            _ => panic!(),
        }
    }
}

impl Sub for VMType {
    type Output = VMType;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m - n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x - y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 - x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x - n as f64),
            _ => panic!(),
        }
    }
}

impl Mul for VMType {
    type Output = VMType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m * n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x * y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 * x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x * n as f64),
            _ => panic!(),
        }
    }
}

impl Add for VMType {
    type Output = VMType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Str(s), VMType::Str(t)) => VMType::Str(s + &t),
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m + n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x + y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 + x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x + n as f64),
            _ => panic!(),
        }
    }
}

impl ToString for VMType {
    fn to_string(&self) -> String {
        match self {
            VMType::Float(x) => x.to_string(),
            VMType::Integer(n) => n.to_string(),
            VMType::Str(c) => c.clone(),
            VMType::Bool(p) => p.to_string(),
            _ => panic!(),
        }
    }
}

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
struct Frame<T> {
    pub value: T,
    pub next: Option<Box<Frame<T>>>,
}

impl<T> Frame<T> {
    pub fn new(value: T) -> Frame<T> {
        Self { value, next: None }
    }
}

/// Stack data structure implementation
struct Stack<T> {
    pub head: Option<Frame<T>>,
}

impl<T> Default for Stack<T> {
    fn default() -> Stack<T> {
        Stack { head: None }
    }
}

impl<T: Clone> Stack<T> {
    /// Duplicate the head value
    pub fn dup(&mut self) {
        let a = self.fast_pop_1();
        self.push(a.clone());
        self.push(a)
    }
}

impl<T> Stack<T> {
    /// Clear all the stack values
    pub fn clear(&mut self) {
        while self.head.is_some() {
            self.fast_pop_2()
        }
    }

    /// Swap the two head values
    pub fn swap(&mut self) {
        let a = self.fast_pop_1();
        let b = self.fast_pop_1();

        self.push(a);
        self.push(b);
    }

    /// Push a new value to the stack
    pub fn push(&mut self, value: T) {
        let mut node = Frame::new(value);

        if let Some(stack) = replace(&mut self.head, None) {
            node.next = Some(Box::new(stack))
        }

        self.head = Some(node);
    }

    /// Pop the head value without returning it
    pub fn fast_pop_2(&mut self) {
        self.head = replace(&mut self.head, None).unwrap().next.map(|n| *n)
    }

    /// Pop the head value and return it (might result a runtime panic)
    pub fn fast_pop_1(&mut self) -> T {
        let stack = replace(&mut self.head, None).unwrap();
        self.head = stack.next.map(|n| *n);
        stack.value
    }

    /// Pop the head value and return it in an Option
    pub fn pop(&mut self) -> Option<T> {
        match replace(&mut self.head, None) {
            Some(stack) => {
                self.head = stack.next.map(|n| *n);
                Some(stack.value)
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
            ' ' => (),
            // will be used to split the code into instructions
            ';' => tokens.push(Token::Separator),
            // will be used in error messages
            '\n' => line += 1,
            // comments
            '(' => {
                for next in chars.by_ref() {
                    if next == ')' {
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
    procs: BTreeMap<String, Stack<Token>>,
    vars: BTreeMap<String, VMType>,
    stack: Stack<VMType>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            procs: BTreeMap::default(),
            vars: BTreeMap::default(),
            stack: Stack::default(),
        }
    }

    /// Parse a single instruction
    pub fn parse_instruction(&mut self, tokens: &mut Stack<Token>) {
        while let Some(token) = tokens.pop() {
            match token {
                Token::Separator => panic!("Separator error (shouldn't be happening)"),

                Token::Bool(p, _) => self.stack.push(VMType::Bool(p)),
                Token::Str(content, _) => self.stack.push(VMType::Str(content)),
                Token::Integer(n, _) => self.stack.push(VMType::Integer(n)),
                Token::Float(x, _) => self.stack.push(VMType::Float(x)),

                Token::Identifier(identifier, line) => match &*identifier {
                    "proc" => {
                        let proc_name = match tokens.fast_pop_1() {
                            Token::Identifier(name, _) => name,
                            _ => panic!("line {line}: syntax: `proc name`"),
                        };

                        match tokens.fast_pop_1() {
                            Token::Identifier(supposed_do, _) => match &*supposed_do {
                                "do" => (),
                                _ => panic!(),
                            },
                            _ => panic!(),
                        }

                        let mut proc_tokens = Stack::<Token>::default();

                        while let Some(token) = tokens.pop() {
                            match &token {
                                Token::Identifier(identifier, _) => match &**identifier {
                                    "end" => break,
                                    _ => proc_tokens.push(token),
                                },
                                _ => proc_tokens.push(token),
                            }
                        }

                        self.procs.insert(proc_name, proc_tokens);
                    }

                    "if" => {
                        match self.stack.fast_pop_1() {
                            VMType::Bool(true) => (),
                            VMType::Bool(false) => tokens.clear(),
                            _ => panic!(),
                        };
                    }

                    "let" => {
                        let name = match tokens.fast_pop_1() {
                            Token::Identifier(name, _) => name,
                            _ => panic!("line {line}: syntax: `let name value` with value of type int|float|string or a pop|dup instruction!"),
                        };

                        let value = match tokens.fast_pop_1() {
                            Token::Str(content, _) => VMType::Str(content),
                            Token::Integer(n, _) => VMType::Integer(n),
                            Token::Float(x, _) => VMType::Float(x),
                            Token::Bool(p, _) => VMType::Bool(p),
                            Token::Identifier(instruction, _) => {
                                match &*instruction {
                                    "pop" => self.stack.fast_pop_1(),
                                    "dup" => { self.stack.dup(); self.stack.fast_pop_1() }
                                    _ => panic!("line {line}: syntax: `let name value` with value of type int|float|string or a pop|dup instruction!"), 
                                }
                            },
                            _ => panic!("line {line}: syntax: `let name value` with value of type int|float|string or a pop|dup instruction!"),
                        };

                        self.vars.insert(name, value);
                    }

                    "dup" => self.stack.dup(),
                    "swap" => self.stack.swap(),
                    "exch" => self.stack.swap(),
                    "clear" => self.stack.clear(),
                    "pop" => self.stack.fast_pop_2(),
                    "drop" => self.stack.fast_pop_2(),
                    "eq" => self.parse_eq(line),
                    "or" => self.parse_or(line),
                    "xor" => self.parse_xor(line),
                    "add" => self.parse_add(line),
                    "sub" => self.parse_sub(line),
                    "mul" => self.parse_mul(line),
                    "sum" => self.parse_sum(line),
                    "not" => self.parse_not(line),
                    "and" => self.parse_and(line),
                    "join" => self.parse_join(line),
                    "take" => self.parse_take(line),
                    "reverse" => self.parse_reverse(line),
                    "product" => self.parse_product(line),
                    "parse_int" => self.parse_parse_int(line),
                    "range" | "erange" => self.parse_range(identifier, line),
                    "print" | "println" => self.parse_print(identifier, line),

                    identifier => self.stack.push(self.vars.get(identifier).unwrap().clone()),
                },
            }
        }
    }

    /// Parse an add instruction
    ///
    /// 1 + 1 => 2
    /// 1.0 + 1.0 => 2.0
    /// 1.0 + 1 => 2.0
    fn parse_add(&mut self, line: u16) {
        let a = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `add` requires two values on the top of the stack!")
        });

        let b = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `add` requires two values on the top of the stack!")
        });

        self.stack.push(a.add(b))
    }

    /// Parse a sub instruction
    ///
    /// 3 - 1 => 2
    /// 3.0 - 1.0 => 2.0
    /// 3.0 - 1 => 2.0
    fn parse_sub(&mut self, line: u16) {
        let a = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `sub` requires two values on the top of the stack!")
        });

        let b = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `sub` requires two values on the top of the stack!")
        });

        self.stack.push(b.sub(a))
    }

    /// Parse a mul instruction
    ///
    /// 2 * 1 => 2
    /// 2.0 * 1.0 => 2.0
    /// 2.0 * 1 => 2.0
    fn parse_mul(&mut self, line: u16) {
        let a = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `mul` requires two values on the top of the stack!")
        });

        let b = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `mul` requires two values on the top of the stack!")
        });

        self.stack.push(a.mul(b))
    }

    /// Parse an or instruction
    ///
    /// 0 + 0 => 0
    /// 0 + 1 => 1
    /// 1 + 0 => 1
    /// 1 + 1 => 1
    fn parse_or(&mut self, line: u16) {
        let p = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `or` requires two values on the top of the stack!")
        });

        let q = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `or` requires two values on the top of the stack!")
        });

        self.stack.push(p | q)
    }

    /// Parse an and instruction
    ///
    /// 0 + 0 => 0
    /// 0 + 1 => 0
    /// 1 + 0 => 0
    /// 1 + 1 => 1
    fn parse_and(&mut self, line: u16) {
        let p = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `and` requires two values on the top of the stack!")
        });

        let q = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `and` requires two values on the top of the stack!")
        });

        self.stack.push(p & q)
    }

    /// Parse a xor instruction
    ///
    /// 0 + 0 => 0
    /// 0 + 1 => 1
    /// 1 + 0 => 1
    /// 1 + 1 => 1
    fn parse_xor(&mut self, line: u16) {
        let p = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `xor` requires two values on the top of the stack!")
        });

        let q = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `xor` requires two values on the top of the stack!")
        });

        self.stack.push(p ^ q)
    }

    /// Parse an equality instruction
    ///
    /// 0 + 0 => 1
    /// 0 + 1 => 0
    /// 1 + 0 => 0
    /// 1 + 1 => 1
    fn parse_eq(&mut self, line: u16) {
        let a = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `eq` requires two values on the top of the stack!")
        });

        let b = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `eq` requires two values on the top of the stack!")
        });

        self.stack.push(VMType::Bool(a == b))
    }

    /// Parse a not instruction
    ///
    /// 0 => 1
    /// 1 => 0
    fn parse_not(&mut self, line: u16) {
        let p = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `not` requires one value on the top of the stack!")
        });

        self.stack.push(!p)
    }

    /// Parse a parse_int instruction
    ///
    /// Convert string to integers (TODO: implement floats)
    fn parse_parse_int(&mut self, line: u16) {
        match self.stack.pop() {
            Some(VMType::Str(string)) => self
                .stack
                .push(VMType::Integer(string.parse::<i64>().unwrap())),
            _ => panic!("line {line}: `parse_int` requires string on the stack!"),
        }
    }

    /// Parse a print/println instruction
    ///
    /// Print stack head to stdout
    fn parse_print(&mut self, identifier: String, line: u16) {
        let print = match &*identifier {
            "println" => |content| println!("{content}"),
            "print" => |content| print!("{content}"),
            _ => panic!(),
        };

        let e = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `{identifier}` requires string|integer|float on the stack!")
        });

        print(e.to_string());

        self.stack.push(e);
    }

    /// Parse a range instruction
    ///
    /// Generate sets of integers
    fn parse_range(&mut self, identifier: String, line: u16) {
        let (b, a) = match (self.stack.pop(), self.stack.pop()) {
            (Some(VMType::Integer(b1)), Some(VMType::Integer(a1))) => (b1, a1),
            _ => {
                panic!("line {line}: `range` requires two integers on the stack!")
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

    /// Parse a join instruction
    ///
    /// Join an array of string around a string
    fn parse_join(&mut self, line: u16) {
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

    /// Parse a take instruction
    ///
    /// |a|b|c|3| => [a, b, c]
    /// |a|b|c|2| => [b, c]
    fn parse_take(&mut self, line: u16) {
        match self.stack.pop() {
            Some(VMType::Integer(n)) => {
                let array = (0..n).map(|_| self.stack.fast_pop_1()).collect();
                self.stack.push(VMType::Array(array))
            }
            _ => panic!("line {line}: `take` requires an integer on the stack!"),
        }
    }

    /// Parse a reverse instruction
    ///
    /// [a, b, c] => [c, b, a]
    fn parse_reverse(&mut self, line: u16) {
        match self.stack.pop() {
            Some(VMType::Array(mut array)) => {
                array.reverse();
                self.stack.push(VMType::Array(array))
            }
            _ => panic!("line {line}: `reverse` expect an array on top of the stack"),
        }
    }

    /// Parse a sum instruction
    ///
    /// [a, b, c, ..., n] => a + b + c + ... + n
    fn parse_sum(&mut self, line: u16) {
        match self.stack.pop() {
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
            _ => panic!("line {line}: `sum` requires [integer|float] on the stack!"),
        }
    }

    /// Parse a product instruction
    ///
    /// [a, b, c, ... , n] => a * b * c * ... * n
    fn parse_product(&mut self, line: u16) {
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
}

fn main() {
    let args = args().collect::<Vec<String>>();
    let content = read_to_string(args.get(1).unwrap()).expect("Failed to open file");
    let mut tokens = lex_into_tokens(&content);
    let mut instructions = Stack::default();
    let mut current_instruction = Stack::default();

    while let Some(next) = tokens.pop() {
        match next {
            Token::Separator => {
                instructions.push(current_instruction);
                current_instruction = Stack::default()
            }
            _ => current_instruction.push(next),
        }
    }

    instructions.push(current_instruction);

    let mut parser = Parser::new();

    while let Some(mut instruction) = instructions.pop() {
        parser.parse_instruction(&mut instruction)
    }
}
