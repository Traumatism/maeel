use crate::token::Token;

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

pub fn extract_blocks(tokens: Vec<Token>) -> Vec<Token> {
    let mut output = Vec::new();
    let mut tokens_iter = tokens.iter();

    while let Some(token) = tokens_iter.next() {
        output.push(match token {
            Token::BlockStart(line) => {
                let mut block_tokens = Vec::new();
                let mut recurse = false;
                let mut n = 0;

                for token_0 in tokens_iter.by_ref() {
                    block_tokens.push(match token_0 {
                        Token::BlockEnd(_) => match n {
                            0 => break,
                            _ => {
                                n -= 1;
                                token_0.clone()
                            }
                        },

                        Token::BlockStart(_) => {
                            n += 1;
                            recurse = true;

                            token_0.clone()
                        }
                        _ => token_0.clone(),
                    })
                }

                match recurse {
                    true => Token::Block(extract_blocks(block_tokens), *line),
                    false => Token::Block(block_tokens, *line),
                }
            }
            _ => token.clone(),
        })
    }

    output
}

/// Turn an identifier into a token
///
/// # Argument
/// `identifier` - Identifier to turn into a token
/// `line` - Line the identifier is at
///
pub fn lex_identifier(identifier: &str, line: u16) -> Token {
    match identifier {
        "true" => Token::Bool(true, line),
        "false" => Token::Bool(false, line),

        "sub" => Token::Sub(line),
        "add" => Token::Add(line),
        "mul" => Token::Mul(line),
        "mod" => Token::Modulo(line),
        "div" => Token::Div(line),
        "divq" => Token::DivQ(line),

        "and" => Token::And(line),
        "or" => Token::Or(line),
        "xor" => Token::Xor(line),
        "not" => Token::Not(line),

        "if" => Token::If(line),
        "for" => Token::For(line),
        "while" => Token::While(line),

        "eq" => Token::Eq(line),
        "gt" => Token::Gt(line),
        "lt" => Token::Lt(line),

        "rot" => Token::Rotate(line),
        "clear" => Token::Clear(line),
        "over" => Token::Over(line),
        "take" => Token::Take(line),
        "swap" => Token::Swap(line),
        "del" => Token::Del(line),
        "dup" => Token::Dup(line),
        "pop" => Token::Pop(line),
        "len" => Token::Len(line),

        "let" => Token::Let(line),
        "proc" => Token::ProcStart(line),
        "return" => Token::Return(line),

        "do" => Token::BlockStart(line),
        "end" => Token::BlockEnd(line),
        _ => Token::Identifier(String::from(identifier), line),
    }
}

/// Turn a character/symbol into a token
///
/// # Argument
/// `chr` - Character to turn into a token
/// `line` - Line the character is at
///
pub fn lex_single_char(chr: char, line: u16) -> Token {
    match chr {
        '-' => Token::Sub(line),
        '+' => Token::Add(line),
        '*' => Token::Mul(line),
        '%' => Token::Modulo(line),
        '/' => Token::Div(line),
        '|' => Token::DivQ(line),

        '&' => Token::And(line),
        '~' => Token::Or(line),
        '^' => Token::Xor(line),
        '!' => Token::Not(line),

        '?' => Token::If(line),
        '§' => Token::For(line),
        '$' => Token::While(line),

        '=' => Token::Eq(line),
        '>' => Token::Gt(line),
        '<' => Token::Lt(line),

        ':' => Token::BlockStart(line),
        ';' => Token::BlockEnd(line),

        _ => panic!("line {line}: Unkown symbol {chr}"),
    }
}

/// Lex a whole code into tokens
///
/// # Arguments
/// `code` - Code to turn into tokens
///
pub fn lex_into_tokens(code: &str) -> Vec<Token> {
    let mut chars = code.chars().collect::<Vec<char>>();
    chars.reverse();

    let mut tokens = Vec::new();
    let mut line = 1;

    while let Some(chr) = chars.pop() {
        let token = match chr {
            ' ' | '(' | ')' => None,

            '\n' => {
                line += 1;

                None
            }

            '@' => {
                while let Some(next) = chars.pop() {
                    if next == '\n' {
                        break;
                    }
                }

                None
            }

            '"' => {
                let mut content = String::new();

                while let Some(next) = chars.pop() {
                    match next {
                        '"' => break,
                        _ => content.push(next),
                    }
                }

                Some(Token::Str(content, line))
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

                Some(lex_identifier(&content, line).clone())
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

                Some(match float {
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
                })
            }
            _ => Some(lex_single_char(chr, line)),
        };

        if let Some(token) = token {
            tokens.push(token)
        }
    }

    tokens
}

#[cfg(test)]
mod lexing_tests {
    use super::{lex_identifier, lex_into_tokens, Token};

    #[test]
    fn test_lex_identifiers() {
        assert_eq!(
            lex_identifier("foo", 0),
            Token::Identifier(String::from("foo"), 0)
        );

        assert_eq!(
            lex_identifier("bar", 0),
            Token::Identifier(String::from("bar"), 0)
        );

        assert_eq!(lex_identifier("add", 0), Token::Add(0));
        assert_eq!(lex_identifier("mul", 0), Token::Mul(0));
    }

    #[test]
    fn test_lex_into_tokens_simple() {
        let expression = "1 2.5\n+ \"hello\" -";

        assert_eq!(
            lex_into_tokens(expression),
            vec![
                Token::Integer(1, 1),
                Token::Float(2.5, 1),
                Token::Add(2),
                Token::Str(String::from("hello"), 2),
                Token::Sub(2),
            ]
        )
    }
}
