use crate::enums::token::Token;

/// Lex an identifier
pub fn lex_identifier(identifier: &str, line: u16) -> Token {
    match identifier {
        "true" => Token::Bool(true, line),
        "false" => Token::Bool(false, line),
        "end" => Token::Separator,
        "proc" => Token::ProcStart,
        "end_proc" => Token::ProcEnd,
        "dup" => Token::Dup,
        "pop" => Token::Pop,
        "clear" => Token::Clear,
        "swap" => Token::Swap,
        "for" => Token::For(line),
        "take" => Token::Take(line),
        "reverse" => Token::Reverse(line),
        "del" => Token::Del(line),
        "return" => Token::Return(line),
        "if" => Token::If(line),
        "let" => Token::Let(line),
        "or" => Token::Or(line),
        "and" => Token::And(line),
        "not" => Token::Not(line),
        "xor" => Token::Xor(line),
        "eq" => Token::Eq(line),
        _ => Token::Identifier(String::from(identifier), line),
    }
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

    while let Some(chr) = chars.pop() {
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
