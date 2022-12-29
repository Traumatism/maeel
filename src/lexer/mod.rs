use crate::enums::token::Token;
use std::collections::HashMap;

/// Lex an identifier
pub fn lex_identifier(identifier: &str, line: u16) -> Token {
    let mut keywords = HashMap::new();
    keywords.insert("true", Token::Bool(true, line));
    keywords.insert("false", Token::Bool(false, line));
    keywords.insert("end", Token::Separator);
    keywords.insert("proc", Token::ProcStart);
    keywords.insert("end_proc", Token::ProcEnd);
    keywords.insert("dup", Token::Dup);
    keywords.insert("pop", Token::Pop);
    keywords.insert("clear", Token::Clear);
    keywords.insert("swap", Token::Swap);
    keywords.insert("for", Token::For(line));
    keywords.insert("take", Token::Take(line));
    keywords.insert("reverse", Token::Reverse(line));
    keywords.insert("del", Token::Del(line));
    keywords.insert("return", Token::Return(line));
    keywords.insert("if", Token::If(line));
    keywords.insert("let", Token::Let(line));
    keywords.insert("or", Token::Or(line));
    keywords.insert("and", Token::And(line));
    keywords.insert("not", Token::Not(line));
    keywords.insert("xor", Token::Xor(line));
    keywords.insert("eq", Token::Eq(line));

    *keywords.get(identifier).unwrap_or(&Token::Identifier(String::from(identifier), line))
}

/// Lex a single character
pub fn lex_single_char(chr: char, line: u16) -> Token {
    match chr {
        '&' => Token::And(line),
        '|' => Token::Or(line),
        '^' => Token::Xor(line),
        '+' => Token::Add(line),
        '*' => Token::Mul(line),
        '/' => Token::Div(line),
        '-' => Token::Sub(line),
        '=' => Token::Eq(line),
        '%' => Token::Modulo(line),
        '!' => Token::Not(line),
        ';' => Token::Separator,
        _ => panic!("line {line}: unknown symbol {chr}"),
    }
}

/// Lex code
pub fn lex_into_tokens(code: &str) -> Vec<Token> {
    let mut chars = code.chars().collect::<Vec<char>>();
    chars.reverse();

    let mut tokens = Vec::new();
    let mut line = 1;

    for chr in chars {
        match chr {
            '\n' => line += 1,
            ' ' => (),
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

                tokens.push(Token::Str(content, line))
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

                tokens.push(lex_identifier(&content, line));
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
                    true => Token::Float(content.parse::<f64>().unwrap(), line),
                    false => Token::Integer(content.parse::<i64>().unwrap(), line),
                });
            }
            _ => tokens.push(lex_single_char(chr, line)),
        }
    }

    tokens
}
