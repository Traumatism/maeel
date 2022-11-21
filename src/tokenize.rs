use crate::utils::Peeker;
use crate::vm::Stack;

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
}

#[derive(Debug)]
pub enum Token {
    Dump,
    Erase,
    Newline,
    String(String),
    Number(isize),
    Identifier(String),
    Operator(Operator),
}

pub fn parse_into_instructions(tokens: &mut Stack<Token>) -> Stack<Stack<Token>> {
    let mut instructions = Stack::new();
    let mut current_instruction = Stack::new();

    while let Some(next) = tokens.pop() {
        match next {
            Token::Newline => {
                instructions.push(current_instruction);
                current_instruction = Stack::new()
            }
            _ => current_instruction.push(next),
        }
    }

    instructions.push(current_instruction);

    instructions
}

pub fn lex_into_tokens(code: &str) -> Stack<Token> {
    let mut chars = Peeker::new(code.chars().collect());
    let mut tokens = Stack::new();

    while let Some(char) = chars.next() {
        match char {
            ' ' => {}
            '@' => tokens.push(Token::Erase),
            '%' => tokens.push(Token::Dump),
            '*' => tokens.push(Token::Operator(Operator::Mul)),
            '+' => tokens.push(Token::Operator(Operator::Plus)),
            '-' => tokens.push(Token::Operator(Operator::Minus)),
            '\n' => tokens.push(Token::Newline),
            '"' => {
                let mut content = String::new();

                while let Some(next) = chars.next() {
                    if next == '"' {
                        break;
                    }

                    content.push(next)
                }

                tokens.push(Token::String(content))
            }
            'a'..='z' => {
                let mut content = String::from(char);

                while let Some(next) = chars.next() {
                    if ('a'..='z').contains(&next) {
                        content.push(next)
                    } else {
                        chars.previous();
                        break;
                    }
                }

                tokens.push(Token::Identifier(content))
            }
            '0'..='9' => {
                let mut content = String::from(char);

                while let Some(next) = chars.next() {
                    if ('0'..='9').contains(&next) {
                        content.push(next)
                    } else {
                        chars.previous();
                        break;
                    }
                }

                tokens.push(Token::Number(content.parse::<isize>().unwrap()));
            }
            _ => panic!(),
        }
    }

    tokens
}
