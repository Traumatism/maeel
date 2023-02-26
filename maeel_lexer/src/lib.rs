mod token;
pub use token::Token;

pub fn extract_instructions(tokens: Vec<Token>) -> Vec<Vec<Token>> {
    let mut instructions = vec![];
    let mut current_instruction = vec![];

    for token in tokens {
        match token {
            Token::BlockEnd => {
                current_instruction.push(Token::BlockEnd);
                instructions.push(std::mem::take(&mut current_instruction));
            }
            _ => current_instruction.push(token),
        }
    }

    instructions.push(current_instruction);
    instructions
}

pub fn extract_block_tokens(tokens_iter: &mut std::slice::Iter<Token>) -> (Vec<Token>, bool) {
    let mut block_tokens = vec![];
    let mut recurse = false;
    let mut n = 0;

    for token in tokens_iter.by_ref() {
        match token {
            Token::BlockEnd if n == 0 => break,
            Token::BlockEnd => {
                n -= 1;
                block_tokens.push(token.clone());
            }
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
    let mut output = vec![];
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
            "dup" => Token::Dup,
            "pop" => Token::Pop,
            "over" => Token::Over,
            "take" => Token::Take,
            "swap" => Token::Swap,
            "rot" => Token::Rot,
            "clear" => Token::Clear,
            "while" => Token::While,
            "end" => Token::BlockEnd,
            "return" => Token::Return,
            "do" => Token::BlockStart,
            "proc" => Token::ProcStart,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "include" => Token::Include,
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
pub fn lex_into_tokens(code: &str) -> Vec<Token> {
    let mut chars = code.chars().peekable();
    let mut tokens = Vec::new();

    while let Some(chr) = chars.next() {
        match chr {
            ' ' | '(' | ')' | '\n' => continue,

            '"' => {
                let content = chars.by_ref().take_while(|&c| c != '"').collect::<String>();

                tokens.push(Token::Str(content));
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                let content = std::iter::once(chr)
                    .chain(
                        chars
                            .by_ref()
                            .take_while(|&c| c.is_alphanumeric() || c == '_'),
                    )
                    .collect::<String>();

                tokens.push(lex_identifier!(&content));
            }

            '0'..='9' => {
                let content = std::iter::once(chr)
                    .chain(
                        chars
                            .by_ref()
                            .take_while(|&c| c.is_digit(10) || c == '.' || c == '_'),
                    )
                    .collect::<String>();

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
                tokens.push(lex_single_char!(chr));
            }
        }
    }

    tokens
}
