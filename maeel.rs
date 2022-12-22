/// Tokens (type, line number)
#[derive(Debug)]
enum Token {
    Float(f64, u16),
    Integer(i64, u16),
    Str(String, u16),
    Identifier(String, u16),
    Separator,
}

// Every single type VM can work with
enum VMType {
    Float(f64),
    Integer(i64),
    Str(String),
    Array(Vec<VMType>),
}

impl Clone for VMType {
    fn clone(&self) -> Self {
        match self {
            Self::Float(x) => Self::Float(*x),
            Self::Integer(n) => Self::Integer(*n),
            Self::Str(c) => Self::Str(c.clone()),
            Self::Array(array) => Self::Array(array.clone()),
        }
    }
}

/// VM (manage instructions stack and variables)
#[derive(Default)]
#[allow(dead_code)]
struct VM {
    pub procs: std::collections::BTreeMap<String, Stack<Token>>,
    pub vars: std::collections::BTreeMap<String, VMType>,
    pub stack: Stack<VMType>,
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

        if let Some(stack) = std::mem::replace(&mut self.head, None) {
            node.next = Some(Box::new(stack))
        }

        self.head = Some(node);
    }

    /// Pop the head value without returning it
    pub fn fast_pop_2(&mut self) {
        self.head = std::mem::replace(&mut self.head, None)
            .unwrap()
            .next
            .map(|n| *n)
    }

    /// Pop the head value and return it (might result a runtime panic)
    pub fn fast_pop_1(&mut self) -> T {
        let stack = std::mem::replace(&mut self.head, None).unwrap();
        self.head = stack.next.map(|n| *n);
        stack.value
    }

    /// Pop the head value and return it in an Option
    pub fn pop(&mut self) -> Option<T> {
        match std::mem::replace(&mut self.head, None) {
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

/// Parse a token stack into a stack of instructions
fn parse_into_instructions(tokens: &mut Vec<Token>) -> Stack<Stack<Token>> {
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
    instructions
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

                tokens.push(Token::Identifier(content, line))
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
    instructions: Stack<Stack<Token>>,
    vm: VM,
}

impl Parser {
    pub fn new(instructions: Stack<Stack<Token>>, vm: VM) -> Self {
        Self { instructions, vm }
    }

    /// Evaluate the instructions
    pub fn parse(&mut self) {
        while let Some(mut instruction) = self.instructions.pop() {
            self.parse_instruction(&mut instruction)
        }
    }

    /// Parse a single instruction
    fn parse_instruction(&mut self, tokens: &mut Stack<Token>) {
        while let Some(token) = tokens.pop() {
            match token {
                Token::Separator => panic!("Separator error (shouldn't be happening)"),

                Token::Str(content, _) => self.vm.stack.push(VMType::Str(content)),

                Token::Integer(n, _) => self.vm.stack.push(VMType::Integer(n)),

                Token::Float(x, _) => self.vm.stack.push(VMType::Float(x)),

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

                        self.vm.procs.insert(proc_name, proc_tokens);
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
                            Token::Identifier(instruction, _) => {
                                match &*instruction {
                                    "pop" => self.vm.stack.fast_pop_1(),
                                    "dup" => { self.vm.stack.dup(); self.vm.stack.fast_pop_1() }
                                    _ => panic!("line {line}: syntax: `let name value` with value of type int|float|string or a pop|dup instruction!"), 
                                }
                            },
                            _ => panic!("line {line}: syntax: `let name value` with value of type int|float|string or a pop|dup instruction!"),
                        };

                        self.vm.vars.insert(name, value);
                    }

                    "dup" => self.vm.stack.dup(),
                    "swap" => self.vm.stack.swap(),
                    "clear" => self.vm.stack.clear(),
                    "pop" => self.vm.stack.fast_pop_2(),
                    "range" | "erange" => self.parse_range(identifier, line),
                    "print" | "println" => self.parse_print(identifier, line),
                    "parse_int" => self.parse_parse_int(line),
                    "sum" => self.parse_sum(line),
                    "join" => self.parse_join(line),
                    "take" => self.parse_take(line),
                    "reverse" => self.parse_reverse(line),
                    "product" => self.parse_product(line),

                    identifier => self
                        .vm
                        .stack
                        .push(self.vm.vars.get(identifier).unwrap().clone()),
                },
            }
        }
    }

    fn parse_parse_int(&mut self, line: u16) {
        match self.vm.stack.pop() {
            Some(VMType::Str(string)) => self
                .vm
                .stack
                .push(VMType::Integer(string.parse::<i64>().unwrap())),
            _ => panic!("line {line}: `parse_int` requires string on the stack!"),
        }
    }

    /// Parse a print/println instruction
    fn parse_print(&mut self, identifier: String, line: u16) {
        let print = match &*identifier {
            "println" => |content| println!("{content}"),
            "print" => |content| print!("{content}"),
            _ => panic!(),
        };

        let e = self.vm.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `{identifier}` requires string|integer|float on the stack!")
        });

        match &e {
            VMType::Float(x) => print(x.to_string()),
            VMType::Integer(n) => print(n.to_string()),
            VMType::Str(c) => print(c.to_string()),
            _ => panic!("line {line}: `{identifier}` requires string|integer|float on the stack!"),
        }

        self.vm.stack.push(e);
    }

    /// Parse a range instruction
    fn parse_range(&mut self, identifier: String, line: u16) {
        let (b, a) = match (self.vm.stack.pop(), self.vm.stack.pop()) {
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

        self.vm.stack.push(VMType::Array(
            range
                .iter()
                .map(|n| VMType::Integer(*n))
                .collect::<Vec<VMType>>(),
        ))
    }

    /// Parse a join instruction
    fn parse_join(&mut self, line: u16) {
        let Some(VMType::Str(join_string)) = self.vm.stack.pop() else { panic!("line {line}: `join` requires a string and a [string] on the stack!") };

        let joined = match self.vm.stack.pop() {
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

        self.vm.stack.push(VMType::Str(joined));
    }

    /// Parse a take instruction
    fn parse_take(&mut self, line: u16) {
        match self.vm.stack.pop() {
            Some(VMType::Integer(n)) => {
                let array = (0..n).map(|_| self.vm.stack.fast_pop_1()).collect();
                self.vm.stack.push(VMType::Array(array))
            }
            _ => panic!("line {line}: `take` requires an integer on the stack!"),
        }
    }

    /// Parse a reverse instruction
    fn parse_reverse(&mut self, line: u16) {
        match self.vm.stack.pop() {
            Some(VMType::Array(mut array)) => {
                array.reverse();
                self.vm.stack.push(VMType::Array(array))
            }
            _ => panic!("line {line}: `reverse` expect an array on top of the stack"),
        }
    }

    /// Parse a sum instruction
    fn parse_sum(&mut self, line: u16) {
        match self.vm.stack.pop() {
            Some(VMType::Array(array)) => {
                let sum = array
                    .iter()
                    .map(|element| match element {
                        VMType::Integer(n) => *n as f64,
                        VMType::Float(x) => *x,
                        _ => panic!("line {line}: `sum` requires [integer|float] on the stack!"),
                    })
                    .sum::<f64>();

                self.vm.stack.push(VMType::Float(sum))
            }
            _ => panic!("line {line}: `sum` requires [integer|float] on the stack!"),
        }
    }

    /// Parse a product instruction
    fn parse_product(&mut self, line: u16) {
        match self.vm.stack.pop() {
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

                self.vm.stack.push(VMType::Float(product));
            }
            _ => {
                panic!("line {line}: `product` requires [integer|float] on the stack!")
            }
        }
    }
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let content = std::fs::read_to_string(args.get(1).unwrap()).expect("Failed to open file");

    Parser::new(
        parse_into_instructions(&mut lex_into_tokens(&content)),
        VM::default(),
    )
    .parse();
}
