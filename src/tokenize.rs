use crate::enums::Token;
use crate::utils::Peeker;
use crate::vm::Stack;

pub fn parse_into_instructions(tokens: &mut Stack<Token>) -> Stack<Stack<Token>> {
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

pub fn lex_into_tokens(code: &str) -> Stack<Token> {
    let mut chars = Peeker::new(code.chars().collect());
    let mut tokens = Stack::default();

    let mut line = 0;

    while let Some(char) = chars.next() {
        match char {
            ' ' => {}
            '\n' => {
                line += 1;
                tokens.push(Token::Separator)
            }
            '*' => {
                while let Some(next) = chars.next() {
                    if next == '*' {
                        break;
                    }
                }
            }
            '"' => {
                let mut content = String::new();

                while let Some(next) = chars.next() {
                    if next == '"' {
                        break;
                    }

                    content.push(next)
                }

                tokens.push(Token::String(content, line))
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

                tokens.push(Token::Identifier(content, line))
            }
            '0'..='9' => {
                let mut content = String::from(char);
                let mut float = false;

                while let Some(next) = chars.next() {
                    if ('0'..='9').contains(&next) {
                        content.push(next)
                    } else if next == '.' {
                        content.push('.');
                        float = true
                    } else {
                        chars.previous();
                        break;
                    }
                }

                if float {
                    tokens.push(Token::Float(content.parse::<f64>().unwrap(), line))
                } else {
                    tokens.push(Token::Integer(content.parse::<i64>().unwrap(), line))
                }
            }
            _ => panic!(),
        }
    }

    tokens
}
