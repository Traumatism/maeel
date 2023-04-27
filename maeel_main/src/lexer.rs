use maeel_common::tokens::Token;

macro_rules! lex_single_char {
    ($chr:expr) => {
        match $chr {
            '-' => vec![Token::Sub],
            '¬' | '!' => vec![Token::Not],
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
            '≕' | '→' | '⟶' => vec![Token::Let],
            'λ' => vec![Token::ProcStart],
            'ρ' => vec![Token::Pop],
            'δ' => vec![Token::Dup],
            'ψ' => vec![Token::Rot],
            'θ' => vec![Token::Over],
            'σ' => vec![Token::Swap],
            '⟹' | '⇒' => vec![Token::If],
            'ω' => vec![Token::While],
            'Ω' => vec![Token::For],
            'α' => vec![Token::Bool(true)],
            'β' => vec![Token::Bool(false)],
            'ε' => vec![Token::Str(String::new())],
            'π' => vec![Token::Float(3.14159265359)],
            '∅' => vec![Token::ArrayStart, Token::ArrayEnd],
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
            '↓' => vec![Token::Integer(1), Token::Sub],
            '↘' => vec![Token::Integer(1), Token::Sub, Token::Let],
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

/// Extract differents blocks from a list of tokens
pub fn extract_blocks(tokens: &[Token]) -> Vec<Token> {
    let mut output = vec![];
    let mut tokens_iter = tokens.iter();

    while let Some(token) = tokens_iter.next() {
        output.push({
            match token {
                Token::BlockStart => {
                    let (block_tokens, recurse) = {
                        let mut block_tokens = vec![];
                        let mut n = 0;
                        let mut recurse = false;

                        for token in tokens_iter.by_ref() {
                            match token {
                                // end of the top block
                                Token::BlockEnd if n == 0 => break,
                                // end of a block embeded in the top block
                                Token::BlockEnd => {
                                    n -= 1;
                                    block_tokens.push(token.clone());
                                }
                                // begining of a new block
                                Token::BlockStart => {
                                    n += 1;
                                    recurse = true;
                                    block_tokens.push(token.clone());
                                }
                                _ => block_tokens.push(token.clone()),
                            }
                        }

                        (block_tokens, recurse)
                    };

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

/// The lex_into_tokens function takes a string code as input, and
/// returns a vector of Tokens representing the lexical tokens
/// found in the input string.
pub fn lex_into_tokens(code: &str) -> Vec<Token> {
    let mut chars = code.chars().peekable();
    let mut tokens = Vec::new();

    while let Some(chr) = chars.next() {
        match chr {
            ' ' | '\n' => continue,

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

                tokens.push(Token::Str(content));
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                let content = std::iter::once(chr)
                    .chain(
                        chars
                            .clone()
                            .take_while(|&c| c.is_alphanumeric() || c == '_'),
                    )
                    .collect::<String>();

                (1..content.len()).for_each(|_| {
                    chars.next().unwrap();
                });

                tokens.push(Token::Identifier(content));
            }

            '0'..='9' => {
                let content = std::iter::once(chr)
                    .chain(
                        chars
                            .clone()
                            .take_while(|&c| c.is_digit(10) || c == '.' || c == '_'),
                    )
                    .collect::<String>();

                (1..content.len()).for_each(|_| {
                    chars.next().unwrap();
                });

                if content.matches('.').count() > 1 {
                    panic!("Invalid float: {}", content);
                }

                let token = if content.contains('.') {
                    Token::Float(content.parse().unwrap())
                } else {
                    Token::Integer(content.parse().unwrap())
                };

                tokens.push(token);
            }

            _ => {
                lex_single_char!(chr)
                    .iter()
                    .for_each(|token| tokens.push(token.clone()));
            }
        }
    }

    tokens
}
