use common::tokens::Token;

/// Transform a single character into a token
macro_rules! lex_single_char {
    ($character:expr) => {
        match $character {
            '-' => Token::Sub,
            '+' => Token::Add,
            '*' => Token::Mul,
            '/' => Token::Div,
            '%' => Token::Mod,
            '&' => Token::Call,
            'ζ' => Token::Clear,
            '→' | '⟶' | '@' => Token::Let,
            'λ' => Token::ProcStart,
            '⟹' | '⇒' => Token::If,
            'ω' => Token::While,
            'Ω' => Token::For,
            'α' => Token::Bool(true),
            'β' => Token::Bool(false),
            '(' => Token::BlockStart,
            ')' => Token::BlockEnd,
            '{' => Token::ArrayStart,
            '}' => Token::ArrayEnd,
            '[' => Token::IStart,
            ']' => Token::IEnd,
            'Γ' => Token::Get,
            '=' => Token::Eq,
            '<' => Token::Lt,
            '>' => Token::Gt,
            character => panic!("{character}"),
        }
    };
}

fn extract_blocks(tokens: &[Token]) -> Vec<Token>
{
    let mut output = Vec::new();
    let mut tokens_iter = tokens.iter();

    while let Some(token) = tokens_iter.next() {
        output.push(match token {
            Token::BlockStart => {
                let mut depth = 1_u8;
                let mut block_tokens = Vec::new();

                while depth > 0 {
                    match tokens_iter.next() {
                        // There is no more tokens
                        None => break,

                        // We find a block end, so we exit from
                        // current code block and need to decrease depth
                        Some(Token::BlockEnd) => {
                            block_tokens.push(Token::BlockEnd);
                            depth -= 1
                        }

                        // We find a block start, so we enter into a
                        // new code block and need to decrease depth
                        Some(Token::BlockStart) => {
                            block_tokens.push(Token::BlockStart);
                            depth += 1
                        }

                        Some(token) => {
                            block_tokens.push(token.clone())
                        }
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
            .chain(
                $characters
                    .clone()
                    .take_while($p),
            )
            .collect::<String>();

        // Apply changes to the iterator
        // (we cloned it previously)
        (1..content.len()).for_each(|_| {
            $characters.next().unwrap();
        });

        content
    }};
}

/// The function lexes a given code string into a vector of tokens.
///
/// Arguments:
///
/// * `code`: The `code` parameter is a string slice containing the code to be lexed into tokens.
///
/// Returns:
///
/// A vector of `Token`s, which are the result of lexing the input `code` string.
pub fn lex_into_tokens(code: &str) -> Vec<Token>
{
    let mut depth = 0;
    let mut tokens = Vec::default();
    let mut characters = code.chars().peekable();

    while let Some(character) = characters.next() {
        match character {
            // Ignore comments
            '|' => {
                for character in characters.by_ref() {
                    if character != '\n' {
                        continue
                    }

                    break
                }
            }

            // Ignore white-spaces
            ' ' | '\n' => continue,

            // Code block start
            '(' => {
                tokens.push(Token::BlockStart);

                depth += 1;
            }

            // Code block end
            ')' => {
                tokens.push(Token::BlockEnd);

                depth -= 1;
            }

            // Lex strings
            '"' => {
                let mut index = 0;

                let content_vec: Vec<char> = characters
                    .by_ref()
                    .take_while(|&character| character != '"')
                    .collect();

                let mut content =
                    String::with_capacity(content_vec.len());

                while index < content_vec.len() {
                    let character = content_vec[index];

                    index += 1;

                    content.push(
                        if character == '\\' {
                            if let Some(next_character) =
                                content_vec.get(index)
                            {
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
                            } else {
                                panic!("Incomplete escape sequence");
                            }
                        } else {
                            character
                        },
                    )
                }

                tokens.push(Token::Str(content))
            }

            // Lex identifiers
            'a'..='z' | 'A'..='Z' | '_' => {
                let content = take_with_predicate!(
                    character,
                    characters,
                    |&character| {
                        character.is_alphanumeric()
                            || character == '_'
                    }
                );

                let token = match content.as_str() {
                    "fun" => Token::ProcStart,
                    "while" => Token::While,
                    "for" => Token::For,
                    "then" => Token::If,
                    _ => Token::Identifier(content),
                };

                tokens.push(token)
            }

            // Lex integers
            '0'..='9' => {
                let content = take_with_predicate!(
                    character,
                    characters,
                    |&character| {
                        character.is_ascii_digit()
                            || character == '.'
                            || character == '_'
                    }
                );

                tokens.push(
                    if content.contains('.') {
                        assert_eq!(content.matches('.').count(), 1);
                        Token::Float(content.parse().unwrap())
                    } else {
                        Token::Integer(content.parse().unwrap())
                    },
                );
            }

            '=' => {
                match characters.peek() {
                    Some('>') => {
                        tokens.push(Token::If);
                        characters.next();
                    }
                    _ => tokens.push(Token::Eq),
                }
            }

            '-' => {
                match characters.peek() {
                    Some('>') => {
                        tokens.push(Token::Let);
                        characters.next();
                    }
                    _ => tokens.push(Token::Sub),
                }
            }

            // Lex a symbol
            _ => tokens.push(lex_single_char!(character)),
        }
    }

    assert_eq!(depth, 0);
    extract_blocks(tokens.as_slice())
}
