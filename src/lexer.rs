use std::collections::VecDeque;

#[derive(Clone, Debug)]
pub enum Token {
    Block(Vec<Token>),
    String(String),
    Identifier(String),
    Integer(i64),
    Float(f32),
    Call,
    Plus,
    Minus,
    Times,
    Modulo,
    Divide,
    Equal,
    GreaterThan,
    LowerThan,
    Get,
    Clear,
    Assignment,
    FunctionDefinition,
    ArrayStart,
    ArrayEnd,
    BlockStart,
    BlockEnd,
    Then,
    For,
    While,
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

#[inline(always)]
pub fn lex_into_tokens(code: &str) -> Vec<Token> {
    let mut depth: u8 = 0; /* Code block depth */
    let mut tokens = VecDeque::new(); /* Output tokens */

    let mut characters = code.chars().peekable();

    while let Some(character) = characters.next() {
        match character {
            // Comments are ignored
            '|' => {
                for character in characters.by_ref() {
                    if character == '\n'
                    /* Comment ends at end-of-line */
                    {
                        break;
                    }
                }
            }

            ' ' | '\n' => continue,

            '(' => {
                tokens.push_back(Token::BlockStart);
                depth += 1;
            }

            ')' => {
                tokens.push_back(Token::BlockEnd);
                depth -= 1;
            }

            // Lexify strings
            '"' => {
                let content_vector: Vec<char> = characters
                    .by_ref()
                    .take_while(|&character| character != '"')
                    .collect();

                let mut index = 0;
                let mut content = String::with_capacity(content_vector.len());

                while index < content_vector.len() {
                    let character = content_vector[index];

                    index += 1;

                    content.push(match (character, content_vector.get(index)) {
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

                tokens.push_back(Token::String(content))
            }

            // Lexify identifiers
            'a'..='z' | 'A'..='Z' | '_' => {
                // Identifier content
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_alphanumeric() || character == '_'
                });

                tokens.push_back(match content.as_str() {
                    "fun" => Token::FunctionDefinition,
                    "while" => Token::While,
                    "for" => Token::For,
                    "get" => Token::Get,
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
                tokens.push_back(if content.contains('.') {
                    assert_eq!(content.matches('.').count(), 1); // Float must contain one point
                    Token::Float(content.parse().unwrap())
                } else {
                    Token::Integer(content.parse().unwrap())
                });
            }

            // Lexify equal symbol, or if
            '=' => match characters.peek() {
                Some('>') => {
                    tokens.push_back(Token::Then);
                    characters.next();
                }

                _ => tokens.push_back(Token::Equal),
            },

            // Lexify minus symbol, or assignment
            '-' => match characters.peek() {
                Some('>') => {
                    tokens.push_back(Token::Assignment);
                    characters.next();
                }

                _ => tokens.push_back(Token::Minus),
            },

            _ => tokens.push_back(match character {
                '+' => Token::Plus,
                '*' => Token::Times,
                '/' => Token::Divide,
                '%' => Token::Modulo,
                '=' => Token::Equal,
                '<' => Token::LowerThan,
                '>' => Token::GreaterThan,
                '(' => Token::BlockStart,
                ')' => Token::BlockEnd,
                '{' => Token::ArrayStart,
                '}' => Token::ArrayEnd,
                '&' => Token::Call,
                character => panic!("{character}"),
            }),
        }
    }

    // Code block depth should be equal to zero
    assert_eq!(depth, 0);

    let mut stack = Vec::default();
    let mut output = Vec::default();
    let mut block_tokens = Vec::default();

    for token in tokens.iter() {
        match token {
            Token::BlockStart => {
                stack.push(block_tokens);
                block_tokens = Vec::default();
            }

            Token::BlockEnd => {
                let nested_tokens = block_tokens.clone();

                match stack.pop() {
                    Some(previous_tokens) => {
                        block_tokens = previous_tokens;
                        block_tokens.push(Token::Block(nested_tokens));
                    }

                    _ => output.push(Token::Block(nested_tokens)),
                }
            }

            _ => block_tokens.push(token.clone()),
        }
    }

    output.append(&mut block_tokens);
    output
}
