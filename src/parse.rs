use crate::enums::{Token, VMType};
use crate::vm::{Stack, VM};

pub struct Parser {
    instructions: Stack<Stack<Token>>,
    vm: VM,
}

impl Parser {
    pub fn new(instructions: Stack<Stack<Token>>, vm: VM) -> Self {
        Self { instructions, vm }
    }

    pub fn parse(&mut self) {
        while let Some(mut instruction) = self.instructions.pop() {
            self.parse_instruction(&mut instruction)
        }
    }

    fn parse_instruction(&mut self, tokens: &mut Stack<Token>) {
        while let Some(token) = tokens.pop() {
            match token {
                Token::Separator => panic!("Separator error (shouldn't be happening)"),
                Token::Str(content, _) => self.vm.stack.push(VMType::Str(content)),
                Token::Integer(n, _) => self.vm.stack.push(VMType::Integer(n)),
                Token::Float(x, _) => self.vm.stack.push(VMType::Float(x)),
                Token::Identifier(identifier, line) => match &*identifier {
                    "let" => {
                        let name = match tokens.fast_pop_1() {
                            Token::Identifier(name, _) => name,
                            _ => panic!("line {line}: syntax: `let name value` with value of type int|float|string"),
                        };

                        let value = match tokens.fast_pop_1() {
                            Token::Str(content, _) => VMType::Str(content),
                            Token::Integer(n, _) => VMType::Integer(n),
                            Token::Float(x, _) => VMType::Float(x),
                            _ => panic!("line {line}: syntax: `let name value` with value of type int|float|string"),
                        };

                        self.vm.vars.insert(name, value);
                    }
                    "dup" => self.vm.stack.dup(),
                    "swap" => self.vm.stack.swap(),
                    "clear" => self.vm.stack.clear(),
                    "pop" => {
                        self.vm.stack.pop().unwrap_or_else(|| {
                            panic!("line {line}: `pop` requires at least one value on the stack!");
                        });
                    }
                    "range" | "erange" => self.parse_range(identifier, line),
                    "print" | "println" => self.parse_print(identifier, line),
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

    fn parse_take(&mut self, line: u16) {
        match self.vm.stack.pop() {
            Some(VMType::Integer(n)) => {
                let array = (0..n).map(|_| self.vm.stack.fast_pop_1()).collect();
                self.vm.stack.push(VMType::Array(array))
            }
            _ => panic!("line {line}: `take` requires an integer on the stack!"),
        }
    }

    fn parse_reverse(&mut self, line: u16) {
        match self.vm.stack.pop() {
            Some(VMType::Array(mut array)) => {
                array.reverse();
                self.vm.stack.push(VMType::Array(array))
            }
            _ => panic!("line {line}: `reverse` expect an array on top of the stack"),
        }
    }

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
