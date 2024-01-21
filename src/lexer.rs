use crate::vm::{BinApp, MaeelType};

use std::iter::once;

#[derive(Clone)]
pub enum Token {
    Block(Vec<Token>),  /* (...) */
    String(String),     /* "abc" */
    Identifier(String), /* abc */
    Integer(i32),       /* 123 */
    Float(f32),         /* 123.123 */
    BinaryOP(BinApp),   /* T x T -> T */

    Colon,      /* : */
    Dot,        /* . */
    Call,       /* &, ! */
    Assignment, /* -> */
    Then,       /* => */
    ArrayStart, /* { */
    ArrayEnd,   /* } */
    BlockStart, /* ( */
    BlockEnd,   /* ) */
}

macro_rules! binary_op {
    ($operator:tt) => {
        Token::BinaryOP(|a, b| b $operator a)
    };
}

macro_rules! take_with_predicate {
    ($character:expr, $characters:expr, $p:expr) => {{
        let content = once($character)
            .chain($characters.clone().take_while($p))
            .collect::<String>();

        (1..content.len()).for_each(|_| {
            $characters.next();
        });

        content
    }};
}

pub fn lex_into_tokens(code: &str) -> Vec<Token> {
    let mut depth = 0;
    let mut tokens = Vec::default();
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
            ' ' | '\n' | '\t' => continue,

            /* Code block start */
            '(' => {
                tokens.push(Token::BlockStart);
                depth += 1;
            }

            /* Code block end */
            ')' => {
                tokens.push(Token::BlockEnd);
                depth -= 1;
            }

            /* Parse strings */
            '"' => {
                let content_vector = characters
                    .by_ref()
                    .take_while(|&character| character != '"')
                    .collect::<Vec<char>>();

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

                tokens.push(Token::String(content))
            }

            /* Parse identifiers */
            'a'..='z' | 'A'..='Z' | '_' => {
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_alphanumeric() || character == '_'
                });

                tokens.push(Token::Identifier(content));
            }

            /* Parse numerics (float/integers) */
            '0'..='9' => {
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_ascii_digit() || character == '.' || character == '_'
                });

                tokens.push(if content.contains('.') {
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
                    tokens.push(Token::Then);
                    characters.next();
                }

                Some(':') =>
                /* Assignment */
                {
                    tokens.push(Token::Assignment);
                    characters.next();
                }

                _ =>
                /* Equal */
                {
                    tokens.push(Token::BinaryOP(|a, b| MaeelType::Integer((b == a) as i32)))
                }
            },

            /* Parse minus/assignment */
            '-' => match characters.peek() {
                Some('>') =>
                /* Assignment*/
                {
                    tokens.push(Token::Assignment);
                    characters.next();
                }

                _ =>
                /* Minus */
                {
                    tokens.push(Token::BinaryOP(|a, b| b - a))
                }
            },

            _ => tokens.push(match character {
                '+' => binary_op!(+),
                '*' => binary_op!(*),
                '/' => binary_op!(/),
                '%' => binary_op!(%),
                '<' => Token::BinaryOP(|a, b| MaeelType::Integer((b < a) as i32)),
                '>' => Token::BinaryOP(|a, b| MaeelType::Integer((b > a) as i32)),
                '(' => Token::BlockStart,
                ')' => Token::BlockEnd,
                '{' => Token::ArrayStart,
                '}' => Token::ArrayEnd,
                '!' | '&' => Token::Call,
                '.' => Token::Dot,
                ':' => Token::Colon,
                character => panic!("{character}"),
            }),
        }
    }

    assert_eq!(depth, 0);

    let mut stack = Vec::default();
    let mut output = Vec::default();
    let mut temporary_tokens = Vec::default();

    for token in tokens.iter() {
        match token {
            Token::BlockStart => {
                stack.push(temporary_tokens);
                temporary_tokens = Vec::default();
            }

            Token::BlockEnd => {
                let nested_tokens = temporary_tokens.clone();

                match stack.pop() {
                    Some(previous_tokens) => {
                        temporary_tokens = previous_tokens;
                        temporary_tokens.push(Token::Block(nested_tokens));
                    }

                    _ => output.push(Token::Block(nested_tokens)),
                }
            }

            _ => temporary_tokens.push(token.clone()),
        }
    }

    output.append(&mut temporary_tokens);
    output
}
