use crate::token::Token;

pub fn extract_instructions(tokens: Vec<Token>) -> Vec<Vec<Token>> {
    let mut instructions = Vec::default();
    let mut current_instruction = Vec::default();

    for token in tokens {
        match token.clone() {
            Token::BlockEnd => {
                current_instruction.push(Token::BlockEnd);
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
            Token::BlockStart => {
                let mut block_tokens = Vec::new();
                let mut recurse = false;
                let mut n = 0;

                for token_0 in tokens_iter.by_ref() {
                    block_tokens.push(match token_0 {
                        Token::BlockEnd => match n {
                            0 => break,
                            _ => {
                                n -= 1;
                                token_0.clone()
                            }
                        },

                        Token::BlockStart => {
                            n += 1;
                            recurse = true;

                            token_0.clone()
                        }
                        _ => token_0.clone(),
                    })
                }

                match recurse {
                    true => Token::Block(extract_blocks(block_tokens)),
                    false => Token::Block(block_tokens),
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
pub fn lex_identifier(identifier: &str) -> Token {
    match identifier {
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),

        "sub" => Token::Sub,
        "add" => Token::Add,
        "mul" => Token::Mul,
        "mod" => Token::Modulo,
        "div" => Token::Div,
        "divq" => Token::DivQ,

        "and" => Token::And,
        "or" => Token::Or,
        "xor" => Token::Xor,
        "not" => Token::Not,

        "if" => Token::If,
        "for" => Token::For,
        "while" => Token::While,

        "eq" => Token::Eq,
        "gt" => Token::Gt,
        "lt" => Token::Lt,

        "rot" => Token::Rotate,
        "clear" => Token::Clear,
        "over" => Token::Over,
        "take" => Token::Take,
        "swap" => Token::Swap,
        "del" => Token::Del,
        "dup" => Token::Dup,
        "pop" => Token::Pop,
        "len" => Token::Len,

        "let" => Token::Let,
        "proc" => Token::ProcStart,
        "return" => Token::Return,

        "do" => Token::BlockStart,
        "end" => Token::BlockEnd,
        _ => Token::Identifier(String::from(identifier)),
    }
}

/// Turn a character/symbol into a token
///
/// # Argument
/// `chr` - Character to turn into a token
/// `line` - Line the character is at
///
pub fn lex_single_char(chr: char) -> Token {
    match chr {
        '-' => Token::Sub,
        '+' => Token::Add,
        '*' => Token::Mul,
        '%' => Token::Modulo,
        '/' => Token::Div,
        '|' => Token::DivQ,

        '&' => Token::And,
        '~' => Token::Or,
        '^' => Token::Xor,
        '!' => Token::Not,

        '?' => Token::If,
        '§' => Token::For,
        '$' => Token::While,

        '=' => Token::Eq,
        '>' => Token::Gt,
        '<' => Token::Lt,

        ':' => Token::BlockStart,
        ';' => Token::BlockEnd,

        _ => panic!(),
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

                Some(Token::Str(content))
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

                Some(lex_identifier(&content).clone())
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
                    true => Token::Float(content.parse::<f64>().unwrap_or_else(|_| {
                        panic!("line {line}: Failed to parse `{content}` into a float")
                    })),
                    false => Token::Integer(content.parse::<i64>().unwrap_or_else(|_| {
                        panic!("line {line}: Failed to parse `{content}` into an integer")
                    })),
                })
            }
            _ => Some(lex_single_char(chr)),
        };

        if let Some(token) = token {
            tokens.push(token)
        }
    }

    tokens
}
