use std::f64::consts::PI;
use std::iter::once;

use maeel_common::tokens::Token;

/// Transform a single character into a token
macro_rules! lex_single_char {
    ($chr:expr) => {
        match $chr {
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
            'ψ' => vec![Token::Rot],
            'θ' => vec![Token::Over],
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

pub fn extract_blocks(tokens: &[Token]) -> Vec<Token> {
    let mut output = Vec::default(); // Contains the output (blocks and other tokens)
    let mut tokens_iter = tokens.iter(); // Iterate through tokens

    while let Some(token) = tokens_iter.next() {
        output.push({
            match token {
                Token::BlockStart => {
                    let (block_tokens, recurse) = {
                        let mut n = 0; // Amount of nesting
                        let mut recurse = false; // Parse blocks recursively in case of nested blocks
                        let mut block_tokens = Vec::default(); // Contains current block tokens

                        while let Some(token) = tokens_iter.next() {
                            match token {
                                // End of the top block
                                Token::BlockEnd if n == 0 => break,

                                // End of a block embedded in the top block
                                Token::BlockEnd => {
                                    n -= 1;
                                    block_tokens.push(token.clone());
                                }

                                // Beginning of a new block
                                Token::BlockStart => {
                                    n += 1;
                                    recurse = true;
                                    block_tokens.push(token.clone());
                                }

                                // Other token
                                _ => block_tokens.push(token.clone()),
                            }
                        }

                        (block_tokens, recurse)
                    };

                    // Recurse if, and only if, BlockStart is present in the current block
                    match recurse {
                        true => Token::Block(extract_blocks(&block_tokens)),
                        false => Token::Block(block_tokens),
                    }
                }

                _ => token.clone(),
            }
        })
    }

    output
}

pub fn lex_into_tokens(code: &str) -> Vec<Token> {
    let mut chars = code.chars().peekable();
    let mut tokens = Vec::default();

    while let Some(chr) = chars.next() {
        match chr {
            // Ignore white-spaces
            ' ' | '\n' => continue,

            // Lex strings
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

                tokens.push(Token::Str(content))
            }

            // Lex identifiers
            'a'..='z' | 'A'..='Z' | '_' => {
                let content = once(chr)
                    .chain(
                        chars
                            .clone()
                            .take_while(|&c| c.is_alphanumeric() || c == '_'),
                    )
                    .collect::<String>();

                // Apply changes to the iterator
                // (we cloned it previously)
                (1..content.len()).for_each(|_| {
                    chars.next().unwrap();
                });

                tokens.push(Token::Identifier(content))
            }

            // Lex integers
            '0'..='9' => {
                let content = once(chr)
                    .chain(
                        chars
                            .clone()
                            .take_while(|&c| c.is_ascii_digit() || c == '.' || c == '_'),
                    )
                    .collect::<String>();

                // Apply changes to the iterator
                // (we cloned it previously)
                (1..content.len()).for_each(|_| {
                    chars.next().unwrap();
                });

                // Float should have an unique dot
                if content.matches('.').count() > 1 {
                    panic!("Invalid float: {}", content)
                }

                tokens.push(if content.contains('.') {
                    Token::Float(content.parse().unwrap())
                } else {
                    Token::Integer(content.parse().unwrap())
                });
            }

            // Lex a symbol
            _ => lex_single_char!(chr)
                .iter()
                .for_each(|token| tokens.push(token.clone())),
        }
    }

    tokens
}
