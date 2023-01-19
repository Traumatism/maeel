use crate::enums::token::Token;
use std::collections::HashMap;

/// Extract instructions from tokens
pub fn extract_instructions(tokens: Vec<Token>) -> Vec<Vec<Token>> {
    let mut instructions = Vec::default();
    let mut current_instruction = Vec::default();

    for token in tokens {
        match token.clone() {
            Token::BlockEnd(line) => {
                current_instruction.push(Token::BlockEnd(line));
                instructions.push(current_instruction);
                current_instruction = Vec::default()
            }
            _ => current_instruction.push(token.clone()),
        }
    }

    instructions.push(current_instruction);
    instructions
}

/// Extract code blocks from tokens
pub fn extract_blocks(tokens: Vec<Token>) -> Vec<Token> {
    let mut output = Vec::new();
    let mut tokens_iter = tokens.iter();

    while let Some(token) = tokens_iter.next() {
        match token {
            Token::BlockStart(line) => {
                let mut block_tokens = Vec::new();
                let mut recurse = false;
                let mut n = 0;

                for token_0 in tokens_iter.by_ref() {
                    match token_0 {
                        Token::BlockEnd(_) => match n {
                            0 => break,
                            _ => {
                                n -= 1;
                                block_tokens.push(token_0.clone())
                            }
                        },
                        Token::BlockStart(_) => {
                            n += 1;
                            recurse = true;
                            block_tokens.push(token_0.clone())
                        }
                        _ => block_tokens.push(token_0.clone()),
                    }
                }

                match recurse {
                    true => output.push(Token::Block(extract_blocks(block_tokens), *line)),
                    false => output.push(Token::Block(block_tokens, *line)),
                }
            }
            _ => output.push(token.clone()),
        }
    }

    output
}

/// Lex an identifier
pub fn lex_identifier(identifier: &str, line: u16) -> Token {
    match identifier {
        "true" => Token::Bool(true, line),
        "false" => Token::Bool(false, line),
        "or" => Token::Or(line),
        "and" => Token::And(line),
        "not" => Token::Not(line),
        "xor" => Token::Xor(line),
        "eq" => Token::Eq(line),
        "do" => Token::BlockStart(line),
        "end" => Token::BlockEnd(line),
        "proc" => Token::ProcStart(line),
        "dup" => Token::Dup(line),
        "pop" => Token::Pop(line),
        "clear" => Token::Clear(line),
        "swap" => Token::Swap(line),
        "over" => Token::Over(line),
        "rot" => Token::Rotate(line),
        "take" => Token::Take(line),
        "len" => Token::Len(line),
        "for" => Token::For(line),
        "del" => Token::Del(line),
        "if" => Token::If(line),
        "let" => Token::Let(line),
        "return" => Token::Return(line),
        "while" => Token::While(line),
        _ => Token::Identifier(String::from(identifier), line),
    }
}

/// Lex a single character
pub fn lex_single_char(chr: char, line: u16) -> Token {
    let mut symbols = HashMap::new();

    symbols.insert('&', Token::And(line));
    symbols.insert('^', Token::Xor(line));
    symbols.insert('+', Token::Add(line));
    symbols.insert('*', Token::Mul(line));
    symbols.insert('/', Token::Div(line));
    symbols.insert('-', Token::Sub(line));
    symbols.insert('=', Token::Eq(line));
    symbols.insert('%', Token::Modulo(line));
    symbols.insert('!', Token::Not(line));
    symbols.insert('<', Token::Lt(line));
    symbols.insert('>', Token::Gt(line));

    let token = symbols.get(&chr);

    token
        .unwrap_or_else(|| panic!("line {line}: Unkown symbol {chr}"))
        .clone()
}

/// Lex code
pub fn lex_into_tokens(code: &str) -> Vec<Token> {
    let mut chars = code.chars().collect::<Vec<char>>();
    chars.reverse();

    let mut tokens = Vec::new();
    let mut line = 1;

    while let Some(chr) = chars.pop() {
        match chr {
            ' ' | '(' | ')' => (),

            '\n' => line += 1,

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

                tokens.push(lex_identifier(&content, line).clone());
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
                    true => Token::Float(
                        content.parse::<f64>().unwrap_or_else(|_| {
                            panic!("line {line}: Failed to parse `{content}` into a float")
                        }),
                        line,
                    ),
                    false => Token::Integer(
                        content.parse::<i64>().unwrap_or_else(|_| {
                            panic!("line {line}: Failed to parse `{content}` into an integer")
                        }),
                        line,
                    ),
                });
            }
            _ => tokens.push(lex_single_char(chr, line)),
        }
    }

    tokens
}
