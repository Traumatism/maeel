use crate::vm::{BinApp, MaeelType};

use std::{iter::once, ops::AddAssign};

#[derive(Clone, Debug)]
pub enum Token {
    Block(Vec<TokenData>), /* (...) */
    String(String),        /* "abc" */
    Identifier(String),    /* abc */
    Integer(i32),          /* 123 */
    Float(f32),            /* 123.123 */
    BinaryOP(BinApp),      /* T x T -> T */
    Colon,                 /* : */
    Dot,                   /* . */
    Call,                  /* &, ! */
    Assignment,            /* -> */
    Then,                  /* => */
    ArrayStart,            /* { */
    ArrayEnd,              /* } */
    BlockStart,            /* ( */
    BlockEnd,              /* ) */
}

pub type TokenData = (Token, u16);

macro_rules! empty_vec {
    () => {
        Vec::default()
    };
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

pub fn lex_into_tokens(code: &str) -> Vec<TokenData> {
    let mut depth = 0;
    let mut line = 1;
    let mut tokens = empty_vec!();
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

            '\n' => line.add_assign(1),

            /* Ignore whitespaces */
            ' ' | '\t' => continue,

            /* Code block start */
            '(' => {
                tokens.push((Token::BlockStart, line));
                depth += 1;
            }

            /* Code block end */
            ')' => {
                tokens.push((Token::BlockEnd, line));
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
                                        "{line}: invalid escape \
                                                 sequence: \\{}",
                                        next_character
                                    )
                                }
                            }
                        }

                        ('\\', None) => {
                            panic!("{line}: incomplete escape sequence")
                        }

                        _ => character,
                    });
                }

                tokens.push((Token::String(content), line))
            }

            /* Parse identifiers */
            'a'..='z' | 'A'..='Z' | '_' => tokens.push((
                Token::Identifier(take_with_predicate!(character, characters, |&character| {
                    character.is_alphanumeric() || character == '_'
                })),
                line,
            )),

            /* Parse numerics (float/integers) */
            '0'..='9' => {
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_ascii_digit() || character == '.' || character == '_'
                });

                tokens.push((
                    if content.contains('.') {
                        assert_eq!(content.matches('.').count(), 1);

                        Token::Float(content.parse().unwrap())
                    } else {
                        Token::Integer(content.parse().unwrap())
                    },
                    line,
                ));
            }

            /* Parse equal/then */
            '=' => match characters.peek() {
                Some('>') =>
                /* Then */
                {
                    tokens.push((Token::Then, line));
                    characters.next();
                }

                Some(':') =>
                /* Assignment */
                {
                    tokens.push((Token::Assignment, line));
                    characters.next();
                }

                _ =>
                /* Equal */
                {
                    tokens.push((
                        Token::BinaryOP(|a, b| MaeelType::Integer((b == a) as i32)),
                        line,
                    ))
                }
            },

            /* Parse minus/assignment */
            '-' => match characters.peek() {
                Some('>') =>
                /* Assignment*/
                {
                    tokens.push((Token::Assignment, line));
                    characters.next();
                }

                _ =>
                /* Minus */
                {
                    tokens.push((Token::BinaryOP(|a, b| b - a), line))
                }
            },

            _ => tokens.push((
                match character {
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

                    character => panic!("{line}: found unknown char: {character}"),
                },
                line,
            )),
        }
    }

    assert_eq!(depth, 0);

    let mut stack = empty_vec!();
    let mut output = empty_vec!();
    let mut temporary_tokens = empty_vec!();

    for token in tokens.iter() {
        match token {
            (Token::BlockStart, _) => {
                stack.push(temporary_tokens);

                temporary_tokens = empty_vec!();
            }

            (Token::BlockEnd, _) => {
                let nested_tokens = temporary_tokens.clone();

                match stack.pop() {
                    Some(previous_tokens) => {
                        temporary_tokens = previous_tokens;
                        temporary_tokens.push((Token::Block(nested_tokens), line));
                    }

                    _ => output.push((Token::Block(nested_tokens), line)),
                }
            }

            _ => temporary_tokens.push(token.clone()),
        }
    }

    output.append(&mut temporary_tokens);
    output
}
