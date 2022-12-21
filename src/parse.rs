use crate::enums::{Token, VMType};
use crate::vm::Stack;

pub struct Parser {
    instructions: Stack<Stack<Token>>,
    vm: Stack<VMType>,
}

impl Parser {
    pub fn new(instructions: Stack<Stack<Token>>, vm: Stack<VMType>) -> Self {
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
                Token::Separator => panic!(),
                Token::Str(content, _) => self.vm.push(VMType::Str(content)),
                Token::Integer(n, _) => self.vm.push(VMType::Integer(n)),
                Token::Float(x, _) => self.vm.push(VMType::Float(x)),
                Token::Identifier(identifier, line) => match &*identifier {
                    "dup" => self.vm.dup(),
                    "swap" => self.vm.swap(),
                    "clear" => self.vm.clear(),
                    "pop" => self.vm.fast_pop_2(),
                    "range" | "erange" => self.parse_range(identifier, line),
                    "print" | "println" => self.parse_print(identifier, line),
                    "sum" => self.parse_sum(line),
                    "join" => self.parse_join(line),
                    "take" => self.parse_take(line),
                    "reverse" => self.parse_reverse(line),
                    "product" => self.parse_product(line),
                    identifier => panic!("line {line}: unkown identifier: `{identifier}`!"),
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

        let e = self.vm.pop().unwrap_or_else(|| {
            panic!("line {line}: `{identifier}` requires string|integer|float on the stack!")
        });

        match &e {
            VMType::Float(x) => print(x.to_string()),
            VMType::Integer(n) => print(n.to_string()),
            VMType::Str(c) => print(c.to_string()),
            _ => panic!("line {line}: `{identifier}` requires string|integer|float on the stack!"),
        }

        self.vm.push(e);
    }

    fn parse_range(&mut self, identifier: String, line: u16) {
        let (b, a) = match (self.vm.pop(), self.vm.pop()) {
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

        self.vm.push(VMType::Array(
            range
                .iter()
                .map(|n| VMType::Integer(*n))
                .collect::<Vec<VMType>>(),
        ))
    }

    fn parse_join(&mut self, line: u16) {
        let Some(VMType::Str(join_string)) = self.vm.pop() else { panic!("line {line}: `join` requires a string and a [string] on the stack!") };

        let joined = match self.vm.pop() {
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

        self.vm.push(VMType::Str(joined));
    }

    fn parse_take(&mut self, line: u16) {
        match self.vm.pop() {
            Some(VMType::Integer(n)) => {
                let array = (0..n).map(|_| self.vm.fast_pop_1()).collect();
                self.vm.push(VMType::Array(array))
            }
            _ => panic!("line {line}: `take` requires an integer on the stack!"),
        }
    }

    fn parse_reverse(&mut self, line: u16) {
        match self.vm.pop() {
            Some(VMType::Array(mut array)) => {
                array.reverse();
                self.vm.push(VMType::Array(array))
            }
            _ => panic!("line {line}: `reverse` expect an array on top of the stack"),
        }
    }

    fn parse_sum(&mut self, line: u16) {
        match self.vm.pop() {
            Some(VMType::Array(array)) => {
                let sum = array
                    .iter()
                    .map(|element| match element {
                        VMType::Integer(n) => *n as f64,
                        VMType::Float(x) => *x,
                        _ => panic!("line {line}: `sum` requires [integer|float] on the stack!"),
                    })
                    .sum::<f64>();

                self.vm.push(VMType::Float(sum))
            }
            _ => panic!("line {line}: `sum` requires [integer|float] on the stack!"),
        }
    }

    fn parse_product(&mut self, line: u16) {
        match self.vm.pop() {
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

                self.vm.push(VMType::Float(product));
            }
            _ => {
                panic!("line {line}: `product` requires [integer|float] on the stack!")
            }
        }
    }
}
