use crate::enums::token::Token;

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
    match identifier {
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "do" => Token::BlockStart,
        "end" => Token::BlockEnd,
        "proc" => Token::ProcStart,
        "dup" => Token::Dup,
        "pop" => Token::Pop,
        "clear" => Token::Clear,
        "swap" => Token::Swap,
        "for" => Token::For,
        "take" => Token::Take,
        "reverse" => Token::Reverse,
        "del" => Token::Del,
        "if" => Token::If,
        "let" => Token::Let,
        "or" => Token::Or,
        "and" => Token::And,
        "not" => Token::Not,
        "xor" => Token::Xor,
        "eq" => Token::Eq,
        "return" => Token::Return,
        _ => Token::Identifier(String::from(identifier)),
    }
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
            ' ' | '\n' => (),
            '(' | ')' => (),
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

                tokens.push(lex_identifier(&content));
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
