use maeel_common::tokens::{Token, TokenData};

use std::iter::once;
use std::mem::take;
use std::slice::Iter;

pub fn extract_instructions(tokens: Vec<TokenData>) -> Vec<Vec<TokenData>> {
    let (acc, current) =
        tokens
            .into_iter()
            .fold((vec![], vec![]), |(mut acc, mut current), token_data| {
                match token_data.token {
                    Token::BlockEnd => {
                        current.push(TokenData::new(Token::BlockEnd, token_data.line));
                        acc.push(take(&mut current));
                    }
                    _ => current.push(token_data),
                }
                (acc, current)
            });

    let mut instructions = acc;

    if !current.is_empty() {
        instructions.push(current);
    }

    instructions
}

pub fn extract_block_tokens(tokens_iter: &mut Iter<TokenData>) -> (Vec<TokenData>, bool) {
    let mut n = 0;

    let (block_tokens, recurse) = tokens_iter
        .by_ref()
        .take_while(|token_data| match &token_data.token {
            Token::BlockEnd if n == 0 => false,
            Token::BlockEnd => {
                n -= 1;
                true
            }
            Token::BlockStart => {
                n += 1;
                true
            }
            _ => true,
        })
        .cloned()
        .fold((vec![], false), |(mut acc, recurse), token_data| {
            match token_data.token {
                Token::BlockStart | Token::BlockEnd => (),
                _ => acc.push(token_data.clone()),
            }
            (acc, recurse || token_data.token == Token::BlockStart)
        });
    (block_tokens, recurse)
}

pub fn extract_blocks(tokens: &[TokenData]) -> Vec<TokenData> {
    let mut output = vec![];
    let mut tokens_iter = tokens.iter();

    while let Some(token_data) = tokens_iter.next() {
        output.push({
            match &token_data.token {
                Token::BlockStart => {
                    let (block_tokens, recurse) = extract_block_tokens(&mut tokens_iter);

                    match recurse {
                        true => TokenData::new(
                            Token::Block(extract_blocks(&block_tokens)),
                            token_data.line,
                        ),

                        false => TokenData::new(Token::Block(block_tokens), token_data.line),
                    }
                }
                _ => token_data.clone(),
            }
        })
    }

    output
}

macro_rules! lex_identifier {
    ($identifier:expr) => {
        match $identifier.as_str() {
            "if" => Token::If,
            "not" => Token::Not,
            "let" => Token::Let,
            "for" => Token::For,
            "dup" => Token::Dup,
            "pop" => Token::Pop,
            "over" => Token::Over,
            "take" => Token::Take,
            "swap" => Token::Swap,
            "rot" => Token::Rot,
            "clear" => Token::Clear,
            "while" => Token::While,
            "end" => Token::BlockEnd,
            "return" => Token::Return,
            "do" => Token::BlockStart,
            "proc" => Token::ProcStart,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "include" => Token::Include,
            _ => Token::Identifier($identifier.into()),
        }
    };
}

macro_rules! lex_single_char {
    ($chr:expr) => {
        match $chr {
            '=' => Token::Eq,
            '>' => Token::Gt,
            '<' => Token::Lt,
            '!' => Token::Not,
            '-' => Token::Sub,
            '+' => Token::Add,
            '*' => Token::Mul,
            '/' => Token::Div,
            '%' => Token::Mod,
            _ => panic!(),
        }
    };
}

/// The lex_into_tokens function takes a string code as input, and
/// returns a vector of Tokens representing the lexical tokens
/// found in the input string.
pub fn lex_into_tokens(code: &str) -> Vec<TokenData> {
    let mut chars = code.chars().peekable();
    let mut tokens = Vec::new();
    let mut line = 1;

    while let Some(chr) = chars.next() {
        match chr {
            ' ' | '(' | ')' => continue,

            '\n' => line += 1,

            '"' => {
                let content_vec: Vec<char> = chars.by_ref().take_while(|&c| c != '"').collect();
                let mut content = String::with_capacity(content_vec.len());

                let mut i = 0;
                while i < content_vec.len() {
                    let c = content_vec[i];
                    i += 1;

                    content.push(if c == '\\' {
                        if let Some(next_c) = content_vec.get(i) {
                            i += 1;

                            match next_c {
                                'n' => '\n',
                                'r' => '\r',
                                't' => '\t',
                                '\\' => '\\',
                                '"' => '"',
                                _ => panic!("Invalid escape sequence: \\{}", next_c),
                            }
                        } else {
                            panic!("Incomplete escape sequence");
                        }
                    } else {
                        c
                    })
                }

                tokens.push(TokenData::new(Token::Str(content), line));
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                let content = once(chr)
                    .chain(
                        chars
                            .by_ref()
                            .take_while(|&c| c.is_alphanumeric() || c == '_'),
                    )
                    .collect::<String>();

                tokens.push(TokenData::new(lex_identifier!(&content), line));
            }

            '0'..='9' => {
                let content = once(chr)
                    .chain(
                        chars
                            .by_ref()
                            .take_while(|&c| c.is_digit(10) || c == '.' || c == '_'),
                    )
                    .collect::<String>();

                if content.matches('.').count() > 1 {
                    panic!("Invalid float: {}", content);
                }

                let token = if content.contains('.') {
                    Token::Float(content.parse().unwrap())
                } else {
                    Token::Integer(content.parse().unwrap())
                };

                tokens.push(TokenData::new(token, line));
            }

            _ => {
                tokens.push(TokenData::new(lex_single_char!(chr), line));
            }
        }
    }

    tokens
}
