use maeel_common::tokens::Token;

use std::f64::consts::PI;

/// Transform a single character into a token

macro_rules! lex_single_char {
    ($chr:expr) => {

        match $chr
        {
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

            '∤' => vec![Token::Mod, Token::Integer(0), Token::Eq, Token::Not],

            'ρ' => vec![Token::Pop],
            'δ' => vec![Token::Dup],

            'φ' => vec![Token::Rot],

            'θ' => vec![Token::Swap, Token::Dup, Token::Rot, Token::Rot],

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
                Token::Block(vec![Token::Pop, Token::Integer(1), Token::Add]),
            ],

            '↓' => vec![Token::Integer(1), Token::Not, Token::Add],
            '↘' => vec![Token::Integer(1), Token::Not, Token::Add, Token::Let],

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

            chr => panic!("{chr}"),
        }
    };
}

fn extract_blocks(tokens: &[Token]) -> Vec<Token>
{

    let mut output = Vec::new();

    let mut tokens_iter = tokens.iter();

    while let Some(token) = tokens_iter.next()
    {

        output.push(match token
        {
            Token::BlockStart =>
            {

                let mut block_tokens = Vec::new();

                let mut n = 1_u8;

                while n > 0
                {

                    match tokens_iter.next()
                    {
                        None => break,

                        Some(Token::BlockEnd) =>
                        {

                            block_tokens.push(Token::BlockEnd);

                            n -= 1
                        }

                        Some(Token::BlockStart) =>
                        {

                            block_tokens.push(Token::BlockStart);

                            n += 1
                        }

                        Some(token) => block_tokens.push(token.clone()),
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
    ($chr:expr, $chars:expr, $p:expr) => {{

        let content = std::iter::once($chr)
            .chain($chars.clone().take_while($p))
            .collect::<String>();

        // Apply changes to the iterator
        // (we cloned it previously)
        (1..content.len()).for_each(|_| {

            $chars.next().unwrap();
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

    let mut chars = code.chars().peekable();

    let mut tokens = Vec::default();

    let mut depth = 0;

    while let Some(chr) = chars.next()
    {

        match chr
        {
            // Ignore white-spaces
            ' ' | '\n' => continue,

            '(' =>
            {

                tokens.push(Token::BlockStart);

                depth += 1;
            }

            ')' =>
            {

                tokens.push(Token::BlockEnd);

                depth -= 1;
            }

            // Lex strings
            '"' =>
            {

                let content_vec: Vec<char> = chars.by_ref().take_while(|&c| c != '"').collect();

                let mut content = String::with_capacity(content_vec.len());

                let mut i = 0;

                while i < content_vec.len()
                {

                    let c = content_vec[i];

                    i += 1;

                    content.push(
                        if c == '\\'
                        {

                            if let Some(next_c) = content_vec.get(i)
                            {

                                i += 1;

                                match next_c
                                {
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    '\\' => '\\',
                                    '"' => '"',
                                    _ => panic!("Invalid escape sequence: \\{}", next_c),
                                }
                            }
                            else
                            {

                                panic!("Incomplete escape sequence");
                            }
                        }
                        else
                        {

                            c
                        },
                    )
                }

                tokens.push(Token::Str(content))
            }

            // Lex identifiers
            'a'..='z' | 'A'..='Z' | '_' =>
            {

                let content =
                    take_with_predicate!(chr, chars, |&c| c.is_alphanumeric() || c == '_');

                tokens.push(Token::Identifier(content))
            }

            // Lex integers
            '0'..='9' =>
            {

                let content = take_with_predicate!(chr, chars, |&c| {

                    c.is_ascii_digit() || c == '.' || c == '_'
                });

                tokens.push(
                    if content.contains('.')
                    {

                        assert_eq!(content.matches('.').count(), 1);

                        Token::Float(content.parse().unwrap())
                    }
                    else
                    {

                        Token::Integer(content.parse().unwrap())
                    },
                );
            }

            // Lex a symbol
            _ => lex_single_char!(chr)
                .iter()
                .for_each(|token| tokens.push(token.clone())),
        }
    }

    assert_eq!(depth, 0);

    extract_blocks(tokens.as_slice())
}
