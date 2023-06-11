use std::collections::VecDeque;

use crate::vm::MaeelType;

#[derive(Clone, Debug)]
pub enum Token {
    Block(Vec<Token>),                               /* (...) */
    String(String),                                  /* "abc" */
    Identifier(String),                              /* abc */
    Integer(i64),                                    /* 123 */
    Float(f32),                                      /* 123.123 */
    BinaryOP(fn(MaeelType, MaeelType) -> MaeelType), /* T x T -> T */
    Dot,                                             /* . */
    At,                                              /* @ */
    Call,                                            /* & */
    Assignment,                                      /* -> */
    Then,                                            /* => */
    ArrayStart,                                      /* { */
    ArrayEnd,                                        /* } */
    BlockStart,                                      /* ( */
    BlockEnd,                                        /* ) */
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
    let mut depth = 0;
    let mut tokens = VecDeque::new();
    let mut characters = code.chars().peekable();

    while let Some(character) = characters.next() {
        match character {
            /* Parse comments */
            '|' => {
                for character in characters.by_ref() {
                    if character == '\n'
                    /* Comment ends at end-of-line */
                    {
                        break;
                    }
                }
            }

            /* Ignore whitespaces */
            ' ' | '\n' => continue,

            /* Code block start */
            '(' => {
                tokens.push_back(Token::BlockStart);
                depth += 1;
            }

            /* Code block end */
            ')' => {
                tokens.push_back(Token::BlockEnd);
                depth -= 1;
            }

            /* Parse strings */
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

            /* Parse identifiers */
            'a'..='z' | 'A'..='Z' | '_' => {
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_alphanumeric() || character == '_'
                });

                tokens.push_back(Token::Identifier(content));
            }

            /* Parse numerics (float/integers) */
            '0'..='9' => {
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_ascii_digit() || character == '.' || character == '_'
                });

                tokens.push_back(if content.contains('.') {
                    assert_eq!(content.matches('.').count(), 1);

                    Token::Float(content.parse().unwrap())
                } else {
                    Token::Integer(content.parse().unwrap())
                });
            }

            /* Parse equal/then */
            '=' => match characters.peek() {
                Some('>') =>
                /* Then */
                {
                    tokens.push_back(Token::Then);
                    characters.next();
                }

                _ =>
                /* Equal */
                {
                    tokens.push_back(Token::BinaryOP(|a, b| MaeelType::Integer((b == a) as i64)))
                }
            },

            /* Parse minus/assignment */
            '-' => match characters.peek() {
                Some('>') =>
                /* Assignment*/
                {
                    tokens.push_back(Token::Assignment);
                    characters.next();
                }

                _ =>
                /* Minus */
                {
                    tokens.push_back(Token::BinaryOP(|a, b| b - a))
                }
            },

            _ => tokens.push_back(match character {
                '+' => Token::BinaryOP(|a, b| b + a),
                '*' => Token::BinaryOP(|a, b| b * a),
                '/' => Token::BinaryOP(|a, b| b / a),
                '%' => Token::BinaryOP(|a, b| b % a),
                '<' => Token::BinaryOP(|a, b| MaeelType::Integer((b < a) as i64)),
                '>' => Token::BinaryOP(|a, b| MaeelType::Integer((b > a) as i64)),
                '(' => Token::BlockStart,
                ')' => Token::BlockEnd,
                '{' => Token::ArrayStart,
                '}' => Token::ArrayEnd,
                '&' => Token::Call,
                '.' => Token::Dot,
                '@' => Token::At,
                character => panic!("{character}"),
            }),
        }
    }

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
