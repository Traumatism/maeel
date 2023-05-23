#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Block(Vec<Token>),  // (...)
    String(String),     // "..."
    Identifier(String), // abc
    Integer(i64),       // 123
    Float(f64),         // 12.3
    Call,               // &
    Add,                // +
    Sub,                // -
    Mul,                // *
    Mod,                // %
    Div,                // /
    Eq,                 // =
    Gt,                 // >
    Lt,                 // <
    Get,                // get
    Clear,              // clear
    Assignment,         // ->
    FuncStart,          // fun
    ArrayStart,         // {
    ArrayEnd,           // }
    BlockStart,         // (
    BlockEnd,           // )
    If,                 // =>
    For,                // for
    While,              // while
}

fn extract_blocks(tokens: &[Token]) -> Vec<Token> {
    let mut output = Vec::new();
    let mut tokens_iter = tokens.iter();

    while let Some(token) = tokens_iter.next() {
        output.push(match token {
            Token::BlockStart => {
                let mut depth = 1;
                let mut block_tokens = Vec::new();

                while depth > 0 {
                    match tokens_iter.next() {
                        Some(Token::BlockEnd) => {
                            block_tokens.push(Token::BlockEnd);
                            depth -= 1
                        }

                        Some(Token::BlockStart) => {
                            block_tokens.push(Token::BlockStart);
                            depth += 1
                        }

                        Some(token) => block_tokens.push(token.clone()),

                        None => break,
                    }
                }

                Token::Block(extract_blocks(&block_tokens))
            }

            token => token.clone(),
        })
    }

    output
}

macro_rules! take_with_predicate {
    ($character:expr, $characters:expr, $p:expr) => {{
        let content = std::iter::once($character)
            .chain($characters.clone().take_while($p))
            .collect::<String>();

        (1..content.len()).for_each(|_| {
            $characters.next().unwrap();
        });

        content
    }};
}

pub fn lex_into_tokens(code: &str) -> Vec<Token> {
    // Code block depth
    let mut depth = 0;

    // Output tokens
    let mut tokens = Vec::default();

    // Peekable, so we can look the next value without
    // poping it
    let mut characters = code.chars().peekable();

    while let Some(character) = characters.next() {
        match character {
            // Comments are ignored
            '|' => {
                for character in characters.by_ref() {
                    if character != '\n' {
                        continue;
                    }
                    break;
                }
            }

            // Whitespaces are ignored
            ' ' | '\n' => continue,

            '(' => {
                tokens.push(Token::BlockStart);
                depth += 1;
            }

            ')' => {
                tokens.push(Token::BlockEnd);
                depth -= 1;
            }

            // Lexify strings
            '"' => {
                let mut index = 0;

                let content_vec: Vec<char> = characters
                    .by_ref()
                    .take_while(|&character| character != '"')
                    .collect();

                let mut content = String::with_capacity(content_vec.len());

                while index < content_vec.len() {
                    let character = content_vec[index];

                    index += 1;

                    content.push(match (character, content_vec.get(index)) {
                        ('\\', Some(next_character)) => {
                            index += 1;

                            match next_character {
                                'n' => '\n',
                                'r' => '\r',
                                't' => '\t',
                                '\\' => '\\',
                                '"' => '"',
                                _ => {
                                    panic!(
                                        "Invalid escape \
                                                 sequence: \\{}",
                                        next_character
                                    )
                                }
                            }
                        }

                        ('\\', None) => {
                            panic!("Incomplete escape sequence")
                        }

                        _ => character,
                    });
                }

                tokens.push(Token::String(content))
            }

            // Lexify identifiers
            'a'..='z' | 'A'..='Z' | '_' => {
                // Identifier content
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_alphanumeric() || character == '_'
                });

                tokens.push(match content.as_str() {
                    "fun" => Token::FuncStart,
                    "while" => Token::While,
                    "for" => Token::For,
                    "get" => Token::Get,
                    "then" => Token::If,
                    "clear" => Token::Clear,
                    _ => Token::Identifier(content),
                });
            }

            // Lexify numerics
            '0'..='9' => {
                // Numeric content
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_ascii_digit() || character == '.' || character == '_'
                });

                // Numeric contains . => it is a float, otherwise,
                // it is an integer
                tokens.push(if content.contains('.') {
                    assert_eq!(content.matches('.').count(), 1); // Float must contain one point

                    Token::Float(content.parse().unwrap())
                } else {
                    Token::Integer(content.parse().unwrap())
                });
            }

            // Lexify equal symbol, or if
            '=' => match characters.peek() {
                Some('>') => {
                    tokens.push(Token::If);
                    characters.next();
                }

                _ => tokens.push(Token::Eq),
            },

            // Lexify minus symbol, or assignment
            '-' => match characters.peek() {
                Some('>') => {
                    tokens.push(Token::Assignment);
                    characters.next();
                }

                _ => tokens.push(Token::Sub),
            },

            _ => tokens.push(match character {
                '+' => Token::Add,
                '*' => Token::Mul,
                '/' => Token::Div,
                '%' => Token::Mod,
                '&' => Token::Call,
                '(' => Token::BlockStart,
                ')' => Token::BlockEnd,
                '{' => Token::ArrayStart,
                '}' => Token::ArrayEnd,
                '=' => Token::Eq,
                '<' => Token::Lt,
                '>' => Token::Gt,
                character => panic!("{character}"),
            }),
        }
    }

    // Code block depth should be equal to zero
    assert_eq!(depth, 0);

    // Extract code blocks from the tokens stream
    extract_blocks(tokens.as_slice())
}