use crate::enums::token::Token;
use std::collections::HashMap;

pub fn extract_instructions(tokens: Vec<Token>) -> Vec<Vec<Token>> {
    let mut instructions = Vec::<Vec<Token>>::default();
    let mut current_instruction = Vec::<Token>::default();

    for next in tokens {
        match next.clone() {
            Token::BlockEnd => {
                current_instruction.push(Token::BlockEnd);
                instructions.push(current_instruction);
                current_instruction = Vec::default()
            }
            _ => current_instruction.push(next.clone()),
        }
    }

    instructions.push(current_instruction);

    instructions
}

pub fn extract_blocks(tokens: Vec<Token>) -> Vec<Token> {
    let mut output = Vec::new();
    let mut tokens_iter = tokens.iter();

    while let Some(token) = tokens_iter.next() {
        match token {
            Token::BlockStart => {
                let mut block_tokens = Vec::new();
                let mut need_recursion = false;
                let mut n = 0;

                for token_0 in tokens_iter.by_ref() {
                    match token_0 {
                        Token::BlockEnd => match n {
                            0 => break,
                            _ => {
                                n -= 1;
                                block_tokens.push(token_0.clone())
                            }
                        },
                        Token::BlockStart => {
                            n += 1;
                            need_recursion = true;
                            block_tokens.push(token_0.clone())
                        }
                        _ => block_tokens.push(token_0.clone()),
                    }
                }

                match need_recursion {
                    true => output.push(Token::Block(extract_blocks(block_tokens))),
                    false => output.push(Token::Block(block_tokens)),
                }
            }
            _ => output.push(token.clone()),
        }
    }

    output
}

/// Lex an identifier
pub fn lex_identifier(identifier: &str) -> Token {
    let mut keywords = HashMap::new();

    keywords.insert("true", Token::Bool(true));
    keywords.insert("false", Token::Bool(false));

    keywords.insert("do", Token::BlockStart);
    keywords.insert("end", Token::BlockEnd);

    keywords.insert("proc", Token::ProcStart);

    keywords.insert("dup", Token::Dup);
    keywords.insert("pop", Token::Pop);
    keywords.insert("clear", Token::Clear);
    keywords.insert("swap", Token::Swap);

    keywords.insert("for", Token::For);
    keywords.insert("take", Token::Take);
    keywords.insert("reverse", Token::Reverse);
    keywords.insert("del", Token::Del);
    keywords.insert("if", Token::If);
    keywords.insert("let", Token::Let);

    keywords.insert("or", Token::Or);
    keywords.insert("and", Token::And);
    keywords.insert("not", Token::Not);
    keywords.insert("xor", Token::Xor);

    keywords.insert("eq", Token::Eq);
    keywords.insert("return", Token::Return);

    let token = keywords.get(identifier);

    token
        .unwrap_or(&Token::Identifier(String::from(identifier)))
        .clone()
}

/// Lex a single character
pub fn lex_single_char(chr: char) -> Token {
    match chr {
        '&' => Token::And,
        '|' => Token::Or,
        '^' => Token::Xor,
        '+' => Token::Add,
        '*' => Token::Mul,
        '/' => Token::Div,
        '-' => Token::Sub,
        '=' => Token::Eq,
        '%' => Token::Modulo,
        '!' => Token::Not,
        _ => panic!(),
    }
}

/// Lex code
pub fn lex_into_tokens(code: &str) -> Vec<Token> {
    let mut chars = code.chars().collect::<Vec<char>>();
    chars.reverse();

    let mut tokens = Vec::new();

    while let Some(chr) = chars.pop() {
        match chr {
            ' ' | '\n' | '(' | ')' => (),
            '@' => {
                while let Some(next) = chars.pop() {
                    if next == '\n' {
                        break;
                    }
                }
            }
            '"' => {
                let mut content = String::new();

                while let Some(next) = chars.pop() {
                    match next {
                        '"' => break,
                        _ => content.push(next),
                    }
                }

                tokens.push(Token::Str(content))
            }
            'a'..='z' | '_' => {
                let mut content = String::from(chr);

                while let Some(next) = chars.pop() {
                    match next {
                        'a'..='z' | '_' => content.push(next),
                        _ => {
                            chars.push(next);
                            break;
                        }
                    }
                }

                tokens.push(lex_identifier(&content).clone());
            }
            '0'..='9' => {
                let mut content = String::from(chr);
                let mut float = false;

                while let Some(next) = chars.pop() {
                    match next {
                        '0'..='9' => content.push(next),
                        '.' => {
                            float = true;
                            content.push('.')
                        }
                        '_' => (),
                        _ => {
                            chars.push(next);
                            break;
                        }
                    }
                }

                tokens.push(match float {
                    true => Token::Float(content.parse::<f64>().unwrap()),
                    false => Token::Integer(content.parse::<i64>().unwrap()),
                });
            }
            _ => tokens.push(lex_single_char(chr)),
        }
    }

    tokens
}
