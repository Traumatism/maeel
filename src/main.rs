use std::collections::HashMap;
use std::io::Read;
use std::io::Result;

#[derive(Clone)]
pub enum VMType {
    Float(f64),
    Integer(i64),
    Str(String),
    Bool(bool),
    Array(Vec<VMType>),
    Function(Vec<Token>),
}

impl std::fmt::Display for VMType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VMType::Function(tokens) => write!(f, "{:?}", tokens),
            VMType::Float(x) => write!(f, "{}", x),
            VMType::Integer(x) => write!(f, "{}", x),
            VMType::Str(x) => write!(f, "{}", x),
            VMType::Bool(x) => write!(f, "{}", x),
            VMType::Array(xs) => {
                write!(f, "{{")?;
                for (i, x) in xs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }

                    write!(f, "{}", x)?;
                }

                write!(f, "}}")?;

                Ok(())
            }
        }
    }
}

impl PartialOrd for VMType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (VMType::Integer(a), VMType::Integer(b)) => Some(a.cmp(b)),
            (VMType::Float(a), VMType::Float(b)) => Some(a.total_cmp(b)),
            (VMType::Integer(a), VMType::Float(b)) | (VMType::Float(b), VMType::Integer(a)) => {
                Some(b.total_cmp(&(*a as f64)))
            }
            (..) => panic!(),
        }
    }
}

impl PartialEq for VMType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VMType::Str(a), VMType::Str(b)) => a == b,
            (VMType::Array(a), VMType::Array(b)) => a == b,
            (VMType::Float(a), VMType::Integer(b)) => *a == (*b as f64),
            (VMType::Integer(a), VMType::Float(b)) => (*a as f64) == *b,
            (VMType::Integer(a), VMType::Integer(b)) => a == b,
            (VMType::Float(a), VMType::Float(b)) => a == b,
            (VMType::Bool(a), VMType::Bool(b)) => a == b,
            _ => false,
        }
    }
}

impl std::ops::Sub for VMType {
    type Output = VMType;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Integer(a - b),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a - b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 - b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a - b as f64),
            (..) => panic!(),
        }
    }
}

impl std::ops::Mul for VMType {
    type Output = VMType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Integer(a * b),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a * b),
            (VMType::Float(b), VMType::Integer(a)) | (VMType::Integer(a), VMType::Float(b)) => {
                VMType::Float(b * a as f64)
            }
            (..) => panic!(),
        }
    }
}

impl std::ops::Add for VMType {
    type Output = VMType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Str(a), VMType::Str(b)) => VMType::Str(a + &b),
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Integer(a + b),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a + b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 + b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a + b as f64),
            (other, VMType::Array(mut array)) | (VMType::Array(mut array), other) => {
                array.push(other);
                VMType::Array(array)
            }
            (..) => panic!(),
        }
    }
}

impl std::ops::Rem for VMType {
    type Output = VMType;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Integer(a % b),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a % b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 % b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a % b as f64),
            (..) => panic!(),
        }
    }
}

impl std::ops::Div for VMType {
    type Output = VMType;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Float(a as f64 / b as f64),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a / b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 / b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a / b as f64),
            (..) => panic!(),
        }
    }
}

#[macro_export]
macro_rules! next {
    ($tokens:expr,"identifier") => {{
        match $tokens.next().unwrap() {
            Token::Identifier(value) => value.clone(),
            _ => panic!(),
        }
    }};
    ($tokens:expr,"block") => {{
        match $tokens.next().unwrap() {
            Token::Block(block) => block.to_vec(),
            _ => panic!(),
        }
    }};
}

macro_rules! perform_binary_op {
    ($data:expr, $operator:tt) => {{
        let (a, b) = ($data.pop().unwrap(), $data.pop().unwrap());
        $data.push(b $operator a)
    }};

    ($data:expr, $operator:tt, $vmtype:expr) => {{
        let (a, b) = ($data.pop().unwrap(), $data.pop().unwrap());
        $data.push($vmtype(b $operator a))
    }};
}

macro_rules! parse_identifiers_list {
    ($tokens:expr) => {{
        let mut identifiers = Vec::default();

        loop {
            match $tokens.next() {
                Some(Token::Identifier(field)) => identifiers.push(field.clone()),
                Some(Token::IEnd) => break,
                _ => panic!(),
            }
        }

        identifiers
    }};
}

pub fn parse_array<'a>(
    tokens: &'a mut std::slice::Iter<Token>,
    data: &'a mut Vec<VMType>,
    globals: &'a mut HashMap<String, VMType>,
) {
    let mut array = Vec::default();

    loop {
        match tokens.next().unwrap().clone() {
            Token::ArrayEnd => break,
            Token::ArrayStart => {
                parse_array(tokens, data, globals);
                array.push(data.pop().unwrap())
            }
            Token::Str(value) => array.push(VMType::Str(value)),
            Token::Bool(value) => array.push(VMType::Bool(value)),
            Token::Float(value) => array.push(VMType::Float(value)),
            Token::Block(expr) => array.push(VMType::Function(expr)),
            Token::Integer(value) => array.push(VMType::Integer(value)),
            Token::Identifier(identifier) => match globals.get(&identifier) {
                Some(value) => array.push(value.clone()),
                None => panic!(),
            },
            _ => panic!(),
        }
    }

    data.push(VMType::Array(array))
}

pub fn process_tokens<'a>(
    tokens: &'a mut std::slice::Iter<Token>,
    data: &'a mut Vec<VMType>,
    globals: &'a mut HashMap<String, VMType>,
    functions: &'a mut HashMap<String, Vec<Token>>,
) -> Result<&'a mut Vec<VMType>> {
    let mut locals = HashMap::new();

    while let Some(token) = tokens.next() {
        match token {
            Token::Instruction(_) => todo!(),

            Token::Call => match data.pop() {
                Some(VMType::Function(tokens)) => {
                    process_tokens(&mut tokens.iter(), data, &mut globals.clone(), functions)?;
                }
                _ => panic!(),
            },

            Token::ProcStart => {
                let name = next!(tokens, "identifier");

                let function_block = match tokens.next() {
                    Some(Token::Block(function_block)) => function_block.clone(),
                    Some(Token::IStart) => {
                        let mut function_block = Vec::default();

                        parse_identifiers_list!(tokens)
                            .iter()
                            .rev()
                            .for_each(|identifier| {
                                function_block.append(&mut vec![
                                    Token::Let,
                                    Token::Identifier(identifier.clone()),
                                ])
                            });

                        function_block.append(&mut next!(tokens, "block"));
                        function_block
                    }
                    _ => panic!(),
                };

                functions.insert(name, function_block);
            }

            Token::While => {
                let tokens = next!(tokens, "block");

                while let Some(VMType::Bool(true)) = data.pop() {
                    process_tokens(&mut tokens.iter(), data, globals, functions).unwrap();
                }
            }

            Token::For => {
                let tokens = next!(tokens, "block");

                match data.pop() {
                    Some(VMType::Array(xs)) => {
                        for element in xs {
                            data.push(element);
                            process_tokens(&mut tokens.iter(), data, globals, functions).unwrap();
                        }
                    }

                    Some(VMType::Str(string)) => {
                        for element in string.chars().map(String::from).collect::<Vec<String>>() {
                            data.push(VMType::Str(element));
                            process_tokens(&mut tokens.iter(), data, globals, functions).unwrap();
                        }
                    }

                    _ => panic!(),
                }
            }

            Token::If => {
                let tokens = next!(tokens, "block");

                if let Some(VMType::Bool(true)) = data.pop() {
                    process_tokens(&mut tokens.iter(), data, globals, functions).unwrap();
                }
            }

            Token::ArrayStart => parse_array(tokens, data, globals),

            Token::Let => match tokens.next() {
                Some(Token::Identifier(name)) => {
                    match name.chars().collect::<Vec<char>>().first() {
                        Some('_') => locals.insert(name.clone(), data.pop().unwrap()),
                        Some(_) => globals.insert(name.clone(), data.pop().unwrap()),
                        None => panic!(),
                    };
                }

                o => panic!("{o:?}"),
            },

            Token::Gt => perform_binary_op!(data, >, VMType::Bool),
            Token::Lt => perform_binary_op!(data, <, VMType::Bool),
            Token::Eq => perform_binary_op!(data, ==, VMType::Bool),
            Token::Add => perform_binary_op!(data, +),
            Token::Sub => perform_binary_op!(data, -),
            Token::Mul => perform_binary_op!(data, *),
            Token::Div => perform_binary_op!(data, /),
            Token::Mod => perform_binary_op!(data, %),
            Token::Clear => data.clear(),
            Token::Str(content) => data.push(VMType::Str(content.clone())),
            Token::Bool(content) => data.push(VMType::Bool(*content)),
            Token::Float(content) => data.push(VMType::Float(*content)),
            Token::Integer(content) => data.push(VMType::Integer(*content)),
            Token::Block(tokens) => data.push(VMType::Function(tokens.clone())),
            Token::Get => match (data.pop(), data.pop()) {
                (Some(VMType::Integer(index)), Some(VMType::Array(array))) => {
                    data.push(array.get(index as usize).unwrap().clone());
                }
                (Some(VMType::Integer(index)), Some(VMType::Str(string))) => {
                    data.push(VMType::Str(
                        string
                            .chars()
                            .map(String::from)
                            .collect::<Vec<String>>()
                            .get(index as usize)
                            .unwrap()
                            .clone(),
                    ));
                }
                _ => panic!(),
            },

            Token::Identifier(identifier) => match identifier.as_str() {
                "print" => print!("{}", data.last().unwrap()),
                "read" => {
                    let (Some(VMType::Integer(bytes)), Some(VMType::Str(path))) = (data.pop(), data.pop()) else {
                        panic!()
                    };

                    assert!(bytes >= 0);

                    let mut file = std::fs::File::open(path)?;
                    let mut buf = vec![0u8; bytes as usize];
                    file.read_exact(&mut buf)?;

                    data.push(VMType::Array(
                        buf.iter()
                            .map(|byte| VMType::Integer(*byte as i64))
                            .collect(),
                    ));
                }

                "include" => {
                    let Some(VMType::Str(target)) = data.pop() else {
                            panic!()
                        };

                    let content = match target.as_str() {
                        "std" => include_str!("../stdlib/std.maeel").to_string(),
                        _ => std::fs::read_to_string(format!("{}.maeel", target.replace('.', "/")))
                            .expect("Failed to include file"),
                    };

                    process_tokens(
                        &mut lex_into_tokens(&content).iter(),
                        &mut Vec::default(),
                        globals,
                        functions,
                    )?;
                }

                identifier => {
                    match (globals.get(identifier), locals.get(identifier)) {
                        (None, Some(value)) => {
                            data.push(value.clone());
                            continue;
                        }
                        (Some(value), None) => {
                            data.push(value.clone());
                            continue;
                        }
                        (Some(_), Some(_)) => panic!(),
                        (..) => {}
                    }

                    process_tokens(
                        &mut functions.get(identifier).expect(identifier).clone().iter(),
                        data,
                        &mut globals.clone(),
                        functions,
                    )?;
                }
            },

            Token::BlockStart | Token::ArrayEnd | Token::IStart | Token::IEnd => {
                panic!()
            }

            Token::BlockEnd => {}
        };
    }

    Ok(data)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Block(Vec<Token>),
    Str(String),
    Identifier(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
    Instruction(String),
    Call,
    Add,
    Sub,
    Mul,
    Mod,
    Div,
    Eq,
    Gt,
    Lt,
    Get,
    Clear,
    Let,
    ProcStart,
    ArrayStart,
    ArrayEnd,
    BlockStart,
    BlockEnd,
    IStart,
    IEnd,
    If,
    For,
    While,
}

fn extract_blocks(tokens: &[Token]) -> Vec<Token> {
    let mut output = Vec::new();
    let mut tokens_iter = tokens.iter();

    while let Some(token) = tokens_iter.next() {
        output.push(match token {
            Token::BlockStart => {
                let mut depth = 1_u8;
                let mut block_tokens = Vec::new();

                while depth > 0 {
                    match tokens_iter.next() {
                        Some(Token::BlockEnd) => {
                            block_tokens.push(Token::BlockEnd);
                            depth -= 1
                        }
                        Some(Token::BlockStart) => {
                            block_tokens.push(Token::BlockStart);
                            depth += 1
                        }
                        Some(token) => block_tokens.push(token.clone()),
                        None => break,
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
    let mut tokens = Vec::default();
    let mut characters = code.chars().peekable();

    while let Some(character) = characters.next() {
        match character {
            '|' => {
                for character in characters.by_ref() {
                    if character != '\n' {
                        continue;
                    }

                    break;
                }
            }

            '\'' => {
                let mut instruction = String::new();

                for character in characters.by_ref() {
                    if character != '\'' {
                        instruction.push(character);
                        continue;
                    }

                    break;
                }

                tokens.push(Token::Instruction(instruction))
            }

            ' ' | '\n' => continue,

            '(' => {
                tokens.push(Token::BlockStart);

                depth += 1;
            }

            ')' => {
                tokens.push(Token::BlockEnd);

                depth -= 1;
            }

            '"' => {
                let mut index = 0;

                let content_vec: Vec<char> = characters
                    .by_ref()
                    .take_while(|&character| character != '"')
                    .collect();

                let mut content = String::with_capacity(content_vec.len());

                while index < content_vec.len() {
                    let character = content_vec[index];

                    index += 1;

                    content.push(if character == '\\' {
                        if let Some(next_character) = content_vec.get(index) {
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
                        } else {
                            panic!("Incomplete escape sequence");
                        }
                    } else {
                        character
                    })
                }

                tokens.push(Token::Str(content))
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_alphanumeric() || character == '_'
                });

                let token = match content.as_str() {
                    "fun" => Token::ProcStart,
                    "while" => Token::While,
                    "for" => Token::For,
                    "get" => Token::Get,
                    "then" => Token::If,
                    "clear" => Token::Clear,
                    _ => Token::Identifier(content),
                };

                tokens.push(token)
            }

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

            '=' => match characters.peek() {
                Some('>') => {
                    tokens.push(Token::If);
                    characters.next();
                }
                _ => tokens.push(Token::Eq),
            },

            '-' => match characters.peek() {
                Some('>') => {
                    tokens.push(Token::Let);
                    characters.next();
                }
                _ => tokens.push(Token::Sub),
            },

            _ => tokens.push(match character {
                '-' => Token::Sub,
                '+' => Token::Add,
                '*' => Token::Mul,
                '/' => Token::Div,
                '%' => Token::Mod,
                '&' => Token::Call,
                'α' => Token::Bool(true),
                'β' => Token::Bool(false),
                '(' => Token::BlockStart,
                ')' => Token::BlockEnd,
                '{' => Token::ArrayStart,
                '}' => Token::ArrayEnd,
                '[' => Token::IStart,
                ']' => Token::IEnd,
                '=' => Token::Eq,
                '<' => Token::Lt,
                '>' => Token::Gt,
                character => panic!("{character}"),
            }),
        }
    }

    assert_eq!(depth, 0);
    extract_blocks(tokens.as_slice())
}

fn main() -> Result<()> {
    let content = std::fs::read_to_string(
        std::env::args()
            .collect::<Vec<String>>()
            .get(1)
            .expect("Please provide a file"),
    )
    .expect("Failed to open file");

    process_tokens(
        &mut lex_into_tokens(&content).iter(),
        &mut Vec::default(),
        &mut HashMap::default(),
        &mut HashMap::default(),
    )?;

    Ok(())
}
