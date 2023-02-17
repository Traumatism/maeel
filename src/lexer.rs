/// The `Token` enum stores all tokens that the
/// lexer can identify
///
/// The `Clone` trait is implemented, which allows
/// the enum members to be easily copied and cloned.
///
#[derive(Clone, Debug)]
pub enum Token {
    Block(Vec<Token>),
    Str(String),
    Integer(i64),
    Identifier(String),
    Float(f64),
    Bool(bool),
    Sub,
    Add,
    Mul,
    Mod,
    Div,
    Not,
    Eq,
    Gt,
    Lt,
    Clear,
    Over,
    Take,
    Swap,
    Del,
    Dup,
    Pop,
    Let,
    ProcStart,
    Return,
    BlockStart,
    BlockEnd,
    If,
    For,
    While,
}

pub fn extract_instructions(tokens: Vec<Token>) -> Vec<Vec<Token>> {
    let mut instructions = Vec::default();
    let mut current_instruction = Vec::default();

    for token in tokens {
        match token.clone() {
            Token::BlockEnd => {
                current_instruction.push(Token::BlockEnd);
                instructions.push(current_instruction);
                current_instruction = Vec::default()
            }
            _ => current_instruction.push(token.clone()),
        }
    }

    instructions.push(current_instruction);
    instructions
}

pub fn extract_block_tokens(tokens_iter: &mut std::slice::Iter<Token>) -> (Vec<Token>, bool) {
    let mut block_tokens = Vec::new();
    let mut recurse = false;
    let mut n = 0;

    for token in tokens_iter.by_ref() {
        match token {
            Token::BlockEnd => match n {
                0 => break,
                _ => {
                    n -= 1;
                    block_tokens.push(token.clone());
                }
            },
            Token::BlockStart => {
                n += 1;
                recurse = true;
                block_tokens.push(token.clone());
            }
            _ => block_tokens.push(token.clone()),
        }
    }

    (block_tokens, recurse)
}

pub fn extract_blocks(tokens: &[Token]) -> Vec<Token> {
    let mut output = Vec::new();
    let mut tokens_iter = tokens.iter();

    while let Some(token) = tokens_iter.next() {
        output.push(match token {
            Token::BlockStart => {
                let (block_tokens, recurse) = extract_block_tokens(&mut tokens_iter);
                match recurse {
                    true => Token::Block(extract_blocks(&block_tokens)),
                    false => Token::Block(block_tokens),
                }
            }
            _ => token.clone(),
        })
    }

    output
}

macro_rules! lex_identifier {
    ($identifier:expr) => {
        match $identifier.as_str() {
            "if" => Token::If,
            "not" => Token::Not,
            "let" => Token::Let,
            "for" => Token::For,
            "del" => Token::Del,
            "dup" => Token::Dup,
            "pop" => Token::Pop,
            "over" => Token::Over,
            "take" => Token::Take,
            "swap" => Token::Swap,
            "clear" => Token::Clear,
            "while" => Token::While,
            "end" => Token::BlockEnd,
            "return" => Token::Return,
            "do" => Token::BlockStart,
            "proc" => Token::ProcStart,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            _ => Token::Identifier(String::from($identifier)),
        }
    };
}

macro_rules! lex_single_char {
    ($chr:expr) => {
        match $chr {
            '=' => Token::Eq,
            '>' => Token::Gt,
            '<' => Token::Lt,
            '!' => Token::Not,
            '-' => Token::Sub,
            '+' => Token::Add,
            '*' => Token::Mul,
            '/' => Token::Div,
            '%' => Token::Mod,
            _ => panic!(),
        }
    };
}

/// The lex_into_tokens function takes a string code as input, and
/// returns a vector of Tokens representing the lexical tokens
/// found in the input string.
///
pub fn lex_into_tokens(code: &str) -> Vec<Token> {
    let mut chars = code.chars().collect::<Vec<char>>();
    chars.reverse();

    let mut tokens = Vec::new();

    while let Some(chr) = chars.pop() {
        let token = match chr {
            ' ' | '(' | ')' | '\n' => None,

            '"' => {
                let mut content = String::new();

                while let Some(next) = chars.pop() {
                    match next {
                        '"' => break,
                        _ => content.push(next),
                    }
                }

                Some(Token::Str(content))
            }

            'a'..='z' | '_' => {
                let mut content = String::from(chr);

                while let Some(next) = chars.pop() {
                    match next {
                        'a'..='z' | '_' => content.push(next),
                        _ => {
                            chars.push(next);
                            break;
                        }
                    }
                }

                Some(lex_identifier!(&content))
            }

            '0'..='9' => {
                let mut content = String::from(chr);
                let mut float = false;

                while let Some(next) = chars.pop() {
                    match next {
                        '0'..='9' => content.push(next),

                        '.' if float => {
                            panic!()
                        }

                        '.' => {
                            float = true;
                            content.push('.')
                        }

                        '_' => (),

                        _ => {
                            chars.push(next);
                            break;
                        }
                    }
                }

                Some(match float {
                    true => Token::Float(content.parse::<f64>().unwrap()),
                    false => Token::Integer(content.parse::<i64>().unwrap()),
                })
            }
            _ => Some(lex_single_char!(chr)),
        };

        if let Some(token) = token {
            tokens.push(token)
        }
    }

    tokens
}
