use maeel_common::tokens::Token;
use maeel_common::tokens::TokenData;
use maeel_common::vmtypes::VMType;

use std::collections::HashMap;
use std::env::args;
use std::fs::read_to_string;
use std::io::Result;
use std::ops::Not;
use std::process::exit;
use std::slice::Iter;

/// Extract tokens from a single block
pub fn extract_block_tokens(
    tokens_iter: &mut std::slice::Iter<TokenData>,
) -> (Vec<TokenData>, bool) {
    let mut block_tokens = vec![];
    let mut n = 0;
    let mut recurse = false;

    for token_data in tokens_iter.by_ref() {
        let token = token_data.token.clone();

        match token {
            // end of the top block
            Token::BlockEnd if n == 0 => break,
            // end of a block embeded in the top block
            Token::BlockEnd => {
                n -= 1;
                block_tokens.push(token_data.clone());
            }
            // begining of a new block
            Token::BlockStart => {
                n += 1;
                recurse = true;
                block_tokens.push(token_data.clone());
            }
            _ => block_tokens.push(token_data.clone()),
        }
    }

    (block_tokens, recurse)
}

/// Extract differents blocks from a list of tokens
pub fn extract_blocks(tokens: &[TokenData]) -> Vec<TokenData> {
    let mut output = vec![];
    let mut tokens_iter = tokens.iter();

    while let Some(token_data) = tokens_iter.next() {
        output.push({
            let token = token_data.token.clone();

            match token {
                Token::BlockStart => {
                    let (block_tokens, recurse) = {
                        {
                            let mut block_tokens = vec![];
                            let mut n = 0;
                            let mut recurse = false;

                            for token_data in tokens_iter.by_ref() {
                                let token = token_data.token.clone();

                                match token {
                                    // end of the top block
                                    Token::BlockEnd if n == 0 => break,
                                    // end of a block embeded in the top block
                                    Token::BlockEnd => {
                                        n -= 1;
                                        block_tokens.push(token_data.clone());
                                    }
                                    // begining of a new block
                                    Token::BlockStart => {
                                        n += 1;
                                        recurse = true;
                                        block_tokens.push(token_data.clone());
                                    }
                                    _ => block_tokens.push(token_data.clone()),
                                }
                            }

                            (block_tokens, recurse)
                        }
                    };

                    match recurse {
                        true => TokenData::new(
                            Token::Block(extract_blocks(&block_tokens)),
                            token_data.line,
                            token_data.pos,
                        ),
                        false => TokenData::new(
                            Token::Block(block_tokens),
                            token_data.line,
                            token_data.pos,
                        ),
                    }
                }
                _ => token_data.clone(),
            }
        })
    }

    output
}

macro_rules! lex_identifier {
    ($identifier:expr) => {
        match $identifier.as_str() {
            "not" => Token::Not,
            "clear" => Token::Clear,
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
            '@' => Token::Let,
            '(' => Token::BlockStart,
            ')' => Token::BlockEnd,
            'λ' => Token::ProcStart,
            'ρ' => Token::Pop,
            'δ' => Token::Dup,
            'θ' => Token::Over,
            'σ' => Token::Swap,
            'ψ' => Token::Rot,
            '?' => Token::If,
            'ω' => Token::While,
            'Σ' => Token::For,
            'τ' => Token::Take,
            'Γ' => Token::Get,
            'α' => Token::Bool(true),
            'β' => Token::Bool(false),
            _ => panic!(),
        }
    };
}

/// The lex_into_tokens function takes a string code as input, and
/// returns a vector of Tokens representing the lexical tokens
/// found in the input string.
pub fn lex_into_tokens(code: &str) -> Vec<TokenData> {
    let mut chars = code.chars().peekable();
    let mut tokens = Vec::new();
    let mut line = 1;
    let mut pos = 0;

    while let Some(chr) = chars.next() {
        pos += 1;

        match chr {
            ' ' => continue,

            '\n' => line += 1,

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

                tokens.push(TokenData::new(Token::Str(content), line, pos));
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                let content = std::iter::once(chr)
                    .chain(
                        chars
                            .by_ref()
                            .take_while(|&c| c.is_alphanumeric() || c == '_'),
                    )
                    .collect::<String>();

                tokens.push(TokenData::new(lex_identifier!(&content), line, pos));
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

                tokens.push(TokenData::new(token, line, pos));
            }

            _ => {
                tokens.push(TokenData::new(lex_single_char!(chr), line, pos));
            }
        }
    }

    tokens
}

macro_rules! next {
    ($tokens:expr, "identifier") => {{
        let next = $tokens.next().unwrap();
        match &next.token {
            Token::Identifier(value) => value.clone(),
            _ => panic!("Expected identifier"),
        }
    }};

    ($tokens:expr, "block") => {{
        let next = $tokens.next().unwrap();
        match &next.token {
            Token::Block(block) => block.to_vec(),
            a => panic!("Expected block, got {:?}", a),
        }
    }};
}

macro_rules! binary_op {
    ($data:expr, $operator:tt) => {{
        let (a, b) = ($data.pop().unwrap(), $data.pop().unwrap());
        $data.push(b $operator a);
    }};
    ($data:expr, $operator:tt, $vmtype:expr) => {{
        let (a, b) = ($data.pop().unwrap(), $data.pop().unwrap());
        $data.push($vmtype(b $operator a))
    }};
}

macro_rules! emit_error {
    ($file_name:expr, $line:expr, $pos:expr, $message:expr) => {{
        println!("{}:{}:{} {}", $file_name, $line, $pos, $message);
        exit(1);
    }};
}

/// Handle a bunch of tokens
pub fn process_tokens<'a>(
    file_name: &'a str,
    tokens: &'a mut Iter<TokenData>,
    data: &'a mut Vec<VMType>,
    vars: &'a mut HashMap<String, VMType>,
    procs: &'a mut HashMap<String, Vec<TokenData>>,
) -> Result<(
    &'a mut Vec<VMType>,
    &'a mut HashMap<String, VMType>,
    &'a mut HashMap<String, Vec<TokenData>>,
)> {
    while let Some(token_data) = tokens.next() {
        let token = token_data.token.clone();
        let line = token_data.line;
        let pos = token_data.pos;

        match token.clone() {
            Token::Include => {}

            // There shouldn't be BlockStart or BlockEnd since it should be removed
            // after the extraction of blocks
            Token::BlockStart | Token::BlockEnd => panic!(),

            // If it's a new code block, handle it
            Token::Block(tokens) => {
                process_tokens(file_name, &mut tokens.iter(), data, vars, procs)?;
            }

            // Push a string
            Token::Str(content) => data.push(VMType::Str(content)),

            // Push a boolean
            Token::Bool(content) => data.push(VMType::Bool(content)),

            // Push a float
            Token::Float(content) => data.push(VMType::Float(content)),

            // Push an integer
            Token::Integer(content) => data.push(VMType::Integer(content)),

            // Rotate the three top elements
            Token::Rot => {
                if data.len() < 3 {
                    emit_error!(
                        file_name,
                        line,
                        pos,
                        "`rot` requires 3 values on the stack!"
                    )
                }

                let (top, over_0, over_1) = (
                    data.pop().unwrap(),
                    data.pop().unwrap(),
                    data.pop().unwrap(),
                );

                data.push(over_0);
                data.push(top);
                data.push(over_1);
            }

            // Swap the two stack top elements
            Token::Swap => {
                if data.len() < 2 {
                    emit_error!(
                        file_name,
                        line,
                        pos,
                        "`swap` requires 2 values on the stack!"
                    )
                }

                let (top, over) = (data.pop().unwrap(), data.pop().unwrap());

                data.push(top);
                data.push(over)
            }

            // Puhsh the elemnt on the stack top on the
            // stack top
            Token::Dup => {
                if data.len() < 1 {
                    emit_error!(file_name, line, pos, "`dup` requires 1 value on the stack!")
                }

                data.push(data.last().cloned().unwrap())
            }

            // Push the element over the stack top on the
            // stack top
            Token::Over => {
                if data.len() < 2 {
                    emit_error!(
                        file_name,
                        line,
                        pos,
                        "`over` requires 2 values on the stack!"
                    )
                }

                data.push(data[data.len() - 2].to_owned())
            }

            // Clear the stack
            Token::Clear => data.clear(),

            // (a > b)?
            Token::Gt => binary_op!(data, >, VMType::Bool),

            // (a < b)?
            Token::Lt => binary_op!(data, <, VMType::Bool),

            // (a = b)?
            Token::Eq => binary_op!(data, ==, VMType::Bool),

            // a - b
            Token::Sub => binary_op!(data, -),

            // a + b
            Token::Add => binary_op!(data, +),

            // a * b
            Token::Mul => binary_op!(data, *),

            // a / b
            Token::Div => binary_op!(data, /),

            // a mod b
            Token::Mod => binary_op!(data, %),

            Token::Pop => {
                if data.len() < 1 {
                    emit_error!(file_name, line, pos, "`pop` requires 1 value on the stack!")
                }

                data.pop();
            }

            Token::ProcStart => {
                procs.insert(next!(tokens, "identifier"), next!(tokens, "block"));
            }

            Token::Not => {
                if data.len() < 1 {
                    emit_error!(file_name, line, pos, "`not` requires 1 value on the stack!")
                }

                let p = data.pop().unwrap();
                data.push(p.not())
            }

            Token::Get => match (data.pop(), data.pop()) {
                (Some(VMType::Integer(n)), Some(VMType::Array(array))) => {
                    data.push(array.get(n as usize).unwrap().clone());
                }
                _ => emit_error!(
                    file_name,
                    line,
                    pos,
                    "`get` requires 1 integer and 1 array on the stack!"
                ),
            },

            Token::Take => match data.pop() {
                Some(VMType::Integer(n)) => {
                    let array = (0..n)
                        .map(|_| {
                            data.pop().unwrap_or_else(|| {
                                emit_error!(
                                    file_name,
                                    line,
                                    pos,
                                    format!("`{} take` requires {} elements on the stack!", n, n)
                                )
                            })
                        })
                        .collect();
                    data.push(VMType::Array(array));
                }
                _ => emit_error!(
                    file_name,
                    line,
                    pos,
                    "`take` requires 1 integer `n` and `n` elements on the stack!"
                ),
            },

            Token::Identifier(identifier) => {
                match identifier.as_str() {
                    "argv" => data.push(VMType::Array(
                        std::env::args().map(|arg| VMType::Str(arg)).collect(),
                    )),

                    "print" => {
                        if data.len() < 1 {
                            emit_error!(
                                file_name,
                                line,
                                pos,
                                "`print` requires 1 value on the stack!"
                            )
                        }

                        let message = data.last().unwrap().to_string();

                        print!("{}", message);
                    }

                    identifier => match vars.get(identifier) {
                        Some(value) => data.push(value.clone()),
                        None => {
                            let tokens = procs.get(identifier).expect(identifier).clone();
                            process_tokens(file_name, &mut tokens.iter(), data, vars, procs)?;
                        }
                    },
                };
            }

            Token::Let => {
                let name = next!(tokens, "identifier");
                let next = tokens.next().unwrap();
                let value = match &next.token {
                    Token::Str(content) => VMType::Str(content.clone()),
                    Token::Bool(p) => VMType::Bool(*p),
                    Token::Float(x) => VMType::Float(*x),
                    Token::Integer(n) => VMType::Integer(*n),
                    Token::Pop => data.pop().unwrap(),
                    Token::Over => data[data.len() - 2].clone(),
                    Token::Dup => data.last().cloned().unwrap(),
                    o => {
                        emit_error!(file_name, line, pos, format!("`let <name> <value>` accepts (as value) strings, booleans, floats, integers, pop, over, dup. Not {:?}", o))
                    }
                };

                vars.insert(name, value);
            }

            Token::While => {
                let tokens = next!(tokens, "block");
                while let VMType::Bool(true) = data.pop().unwrap() {
                    process_tokens(file_name, &mut tokens.iter(), data, vars, procs)?;
                }
            }

            Token::For => {
                let tokens = next!(tokens, "block");
                match data.pop() {
                    Some(VMType::Array(array)) => array.iter().for_each(|element| {
                        data.push(element.clone());
                        process_tokens(file_name, &mut tokens.iter(), data, vars, procs).unwrap();
                    }),
                    _ => emit_error!(
                        file_name,
                        line,
                        pos,
                        "`for` requires an array on the stack!"
                    ),
                }
            }

            Token::If => {
                if let Some(VMType::Bool(true)) = data.pop() {
                    process_tokens(
                        file_name,
                        &mut next!(tokens, "block").iter(),
                        data,
                        vars,
                        procs,
                    )?;
                } else {
                    tokens.next().unwrap();
                }
            }
        }
    }

    Ok((data, vars, procs))
}

fn main() -> Result<()> {
    let args = args().collect::<Vec<String>>();

    let content =
        read_to_string(args.get(1).expect("Please provide a file")).expect("Failed to open file");

    let tokens = lex_into_tokens(&content);

    if !tokens.iter().any(|e| matches!(e.token, Token::Include)) {
        process_tokens(
            args.get(1).unwrap(),
            &mut extract_blocks(&lex_into_tokens(&content)).iter(),
            Vec::new().as_mut(),
            &mut HashMap::new(),
            &mut HashMap::new(),
        )?;
    } else {
        let mut tokens_backup = Vec::new();
        let mut tokens_iter = tokens.iter();

        while let Some(token_data) = tokens_iter.next() {
            let token = token_data.token.clone();

            match token {
                Token::Include => {
                    let next_token = tokens_iter.next().unwrap();

                    let path = match &next_token.token {
                        Token::Str(path) => path,
                        _ => panic!(),
                    };

                    let include_content = read_to_string(path).unwrap();

                    tokens_backup.append(&mut lex_into_tokens(&include_content))
                }
                _ => tokens_backup.push(token_data.clone()),
            }
        }

        process_tokens(
            args.get(1).expect("Please provide a file"),
            &mut extract_blocks(&lex_into_tokens(&content)).iter(),
            Vec::new().as_mut(),
            &mut HashMap::new(),
            &mut HashMap::new(),
        )?;
    }

    Ok(())
}
