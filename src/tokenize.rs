use crate::enums::Token;
use crate::vm::Stack;

pub struct Peeker<T: Clone> {
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

pub fn parse_into_instructions(tokens: &mut Vec<Token>) -> Stack<Stack<Token>> {
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

pub fn lex_into_tokens(code: &str) -> Vec<Token> {
    let mut chars = Peeker::new(code.chars().collect());
    let mut tokens = Vec::new();
    let mut line = 1;

    while let Some(char) = chars.next() {
        match char {
            ' ' => (),
            ';' => tokens.push(Token::Separator),
            '\n' => line += 1,
            '(' => {
                for next in chars.by_ref() {
                    if next == ')' {
                        break;
                    }
                }
            }
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
            'a'..='z' => {
                let mut content = String::from(char);

                while let Some(next) = chars.next() {
                    match next {
                        'a'..='z' => content.push(next),
                        _ => {
                            chars.previous();
                            break;
                        }
                    }
                }

                tokens.push(Token::Identifier(content, line))
            }
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
            _ => panic!(),
        }
    }

    tokens
}
