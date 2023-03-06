use maeel_common::tokens::{Token, TokenData};

/// Extract blocks from tokens
pub fn extract_block_tokens(
    tokens_iter: &mut std::slice::Iter<TokenData>,
) -> (Vec<TokenData>, bool) {
    let mut block_tokens = vec![];
    let mut recurse = false;
    let mut n = 0;

    for token_data in tokens_iter.by_ref() {
        let token = token_data.token.clone();

        match token {
            Token::BlockEnd if n == 0 => break,
            Token::BlockEnd => {
                n -= 1;
                block_tokens.push(token_data.clone());
            }
            Token::BlockStart => {
                n += 1;
                recurse = true;
                block_tokens.push(token_data.clone());
            }
            _ => block_tokens.push(token_data.clone()),
        }
    }

    (block_tokens, recurse)
}

pub fn extract_blocks(tokens: &[TokenData]) -> Vec<TokenData> {
    let mut output = vec![];
    let mut tokens_iter = tokens.iter();

    while let Some(token_data) = tokens_iter.next() {
        output.push({
            let token = token_data.token.clone();

            match token {
                Token::BlockStart => {
                    let (block_tokens, recurse) = extract_block_tokens(&mut tokens_iter);
                    match recurse {
                        true => TokenData::new(
                            Token::Block(extract_blocks(&block_tokens)),
                            token_data.line,
                            token_data.pos,
                        ),
                        false => TokenData::new(
                            Token::Block(block_tokens),
                            token_data.line,
                            token_data.pos,
                        ),
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
            "get" => Token::Get,
            "swap" => Token::Swap,
            "rot" => Token::Rot,
            "clear" => Token::Clear,
            "while" => Token::While,
            "end" => Token::BlockEnd,
            "do" => Token::BlockStart,
            "proc" => Token::ProcStart,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "include" => Token::Include,
            _ => Token::Identifier(String::from($identifier)),
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
    let mut pos = 0;

    while let Some(chr) = chars.next() {
        pos += 1;

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

                tokens.push(TokenData::new(Token::Str(content), line, pos));
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                let content = std::iter::once(chr)
                    .chain(
                        chars
                            .by_ref()
                            .take_while(|&c| c.is_alphanumeric() || c == '_'),
                    )
                    .collect::<String>();

                tokens.push(TokenData::new(lex_identifier!(&content), line, pos));
            }

            '0'..='9' => {
                let content = std::iter::once(chr)
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

                tokens.push(TokenData::new(token, line, pos));
            }

            _ => {
                tokens.push(TokenData::new(lex_single_char!(chr), line, pos));
            }
        }
    }

    tokens
}
