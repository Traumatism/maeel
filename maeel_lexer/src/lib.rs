use maeel_common::tokens::Token;

use std::f64::consts::PI;

/// Transform a single character into a token

macro_rules! lex_single_char {
    ($character:expr) => {
        match $character {
            '¬' | '!' => vec![Token::Not],

            '-' => vec![Token::Not, Token::Add],

            '∪' | '∨' | '+' => vec![Token::Add],

            '∧' | '*' => vec![Token::Mul],

            '⊕' | '⊻' => vec![
                Token::Over,
                Token::Over,
                Token::Mul,
                Token::Not,
                Token::Rot,
                Token::Rot,
                Token::Add,
                Token::Mul,
            ],

            '/' => vec![Token::Div],

            '%' => vec![Token::Mod],

            '∣' => vec![Token::Mod, Token::Integer(0), Token::Eq],

            '∤' => vec![
                Token::Mod,
                Token::Integer(0),
                Token::Eq,
                Token::Not,
            ],

            'ρ' => vec![Token::Pop],
            'δ' => vec![Token::Dup],

            'φ' => vec![Token::Rot],

            'θ' => {
                vec![Token::Swap, Token::Dup, Token::Rot, Token::Rot]
            }

            'σ' => vec![Token::Swap],
            'ζ' => vec![Token::Clear],

            '≕' | '→' | '⟶' => vec![Token::Let],

            'λ' => vec![Token::ProcStart],

            '⟹' | '⇒' => vec![Token::If],

            'ω' => vec![Token::While],
            'Ω' => vec![Token::For],

            'α' => vec![Token::Bool(true)],
            'β' => vec![Token::Bool(false)],

            '∅' => vec![Token::ArrayStart, Token::ArrayEnd],
            'ε' => vec![Token::Str(String::default())],
            'π' => vec![Token::Float(PI)],

            '(' => vec![Token::BlockStart],
            ')' => vec![Token::BlockEnd],

            '{' => vec![Token::ArrayStart],
            '}' => vec![Token::ArrayEnd],

            '[' => vec![Token::IStart],
            ']' => vec![Token::IEnd],

            'τ' => vec![Token::Take],

            'Γ' => vec![Token::Get],

            'Π' => vec![
                Token::Integer(1),
                Token::Swap,
                Token::For,
                Token::Block(vec![Token::Mul]),
            ],

            'Σ' => vec![
                Token::Integer(0),
                Token::Swap,
                Token::For,
                Token::Block(vec![Token::Add]),
            ],

            '#' => vec![
                Token::Integer(0),
                Token::Swap,
                Token::For,
                Token::Block(vec![
                    Token::Pop,
                    Token::Integer(1),
                    Token::Add,
                ]),
            ],

            '↓' => vec![Token::Integer(1), Token::Not, Token::Add],
            '↘' => vec![
                Token::Integer(1),
                Token::Not,
                Token::Add,
                Token::Let,
            ],

            '↑' => vec![Token::Integer(1), Token::Add],
            '↗' => vec![Token::Integer(1), Token::Add, Token::Let],

            '=' => vec![Token::Eq],
            '≠' => vec![Token::Eq, Token::Not],

            '±' | '∓' => vec![Token::Dup, Token::Not],

            '<' => vec![Token::Lt],
            '≤' | '⩽' => vec![
                Token::Over,
                Token::Over,
                Token::Lt,
                Token::Rot,
                Token::Rot,
                Token::Eq,
                Token::Add,
            ],

            '>' => vec![Token::Gt],
            '≥' | '⩾' => vec![
                Token::Over,
                Token::Over,
                Token::Gt,
                Token::Rot,
                Token::Rot,
                Token::Eq,
                Token::Add,
            ],

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

                    content.push(if character == '\\' {
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
                                _ => panic!(
                                    "Invalid escape sequence: \\{}",
                                    next_character
                                ),
                            }
                        } else {
                            panic!("Incomplete escape sequence");
                        }
                    } else {
                        character
                    })
                }

                tokens.push(Token::Str(content))
            }

            // Lex identifiers
            'a'..='z' | 'A'..='Z' | '_' => {
                let token = Token::Identifier(take_with_predicate!(
                    character,
                    characters,
                    |&c| c.is_alphanumeric() || c == '_'
                ));

                tokens.push(token)
            }

            // Lex integers
            '0'..='9' => {
                let content = take_with_predicate!(
                    character,
                    characters,
                    |&c| {
                        c.is_ascii_digit() || c == '.' || c == '_'
                    }
                );

                tokens.push(if content.contains('.') {
                    assert_eq!(content.matches('.').count(), 1);

                    Token::Float(content.parse().unwrap())
                } else {
                    Token::Integer(content.parse().unwrap())
                });
            }

            // Lex a symbol
            _ => lex_single_char!(character)
                .iter()
                .for_each(|token| tokens.push(token.clone())),
        }
    }

    assert_eq!(depth, 0);
    extract_blocks(tokens.as_slice())
}
