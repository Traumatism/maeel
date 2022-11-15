use crate::stack::Stack;
use crate::utils::Peeker;

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum Token {
    Dump,
    Number(isize),
    Identifier(String),
    Operator(Operator),
}

pub fn tokenize(code: &str) -> Stack<Token> {
    let mut chars = Peeker::new(code.chars().collect());
    let mut tokens = Stack::new();

    while let Some(char) = chars.next() {
        match char {
            ' ' | '\n' => {}
            '%' => tokens.push(Token::Dump),
            '+' => tokens.push(Token::Operator(Operator::Plus)),
            '-' => tokens.push(Token::Operator(Operator::Minus)),
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
