use std::collections::HashMap;
use std::io::Read;
use std::io::Result;

#[derive(Clone)]
pub enum VMType {
    Float(f64),
    Integer(i64),
    String(String),
    Array(Vec<VMType>),
    Function(Vec<Token>),
}

impl std::fmt::Display for VMType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        /*
        TODO: implement a function to check the type of the top element
        of the array. Should make function overloading (like print) easier.
        So their could be a print function for integer, a print function for strings
        etc.
        */

        match self {
            // Write a string to stdout (impossible to implement in maeel, by design)
            VMType::String(x) => write!(f, "{}", x),

            // Write an anonymous function to stdout (impossible to implement in maeel)
            VMType::Function(tokens) => write!(f, "{:?}", tokens),

            // Write a float to stdout (float2str[maeel] is in dev)
            VMType::Float(x) => write!(f, "{}", x),

            // Write an integer to stdout (int2str[maeel] is in dev)
            VMType::Integer(x) => write!(f, "{}", x),

            // Write an array to stdout
            VMType::Array(xs) => {
                write!(f, "{{")?;

                xs.iter().enumerate().for_each(|(i, x)| {
                    if i > 0 {
                        write!(f, " ").unwrap();
                    }

                    write!(f, "{}", &x).unwrap();
                });

                write!(f, "}}")
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

            (a, b) => panic!("Cannot compare {a} and {b}"),
        }
    }
}

impl PartialEq for VMType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VMType::String(a), VMType::String(b)) => a == b,
            (VMType::Array(a), VMType::Array(b)) => a == b,
            (VMType::Float(a), VMType::Integer(b)) => *a == (*b as f64),
            (VMType::Integer(a), VMType::Float(b)) => (*a as f64) == *b,
            (VMType::Integer(a), VMType::Integer(b)) => a == b,
            (VMType::Float(a), VMType::Float(b)) => a == b,

            _ => false,
        }
    }
}

impl std::ops::Sub for VMType {
    type Output = VMType;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int - int
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m - n),

            // float - float
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x - y),

            // int - float
            (VMType::Float(x), VMType::Integer(m)) | (VMType::Integer(m), VMType::Float(x)) => {
                VMType::Float(m as f64 - x)
            }

            (a, b) => panic!("Cannot substract {a} and {b}"),
        }
    }
}

impl std::ops::Mul for VMType {
    type Output = VMType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int * int
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m * n),

            // float * float
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x * y),

            // float * float
            (VMType::Float(x), VMType::Integer(m)) | (VMType::Integer(m), VMType::Float(x)) => {
                VMType::Float(x * m as f64)
            }

            (a, b) => panic!("Cannot multiply {a} and {b}"),
        }
    }
}

impl std::ops::Add for VMType {
    type Output = VMType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // str + str
            (VMType::String(a), VMType::String(b)) => VMType::String(a + &b),

            // int + int
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m + n),

            // float + float
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x + y),

            // int + float
            (VMType::Integer(m), VMType::Float(x)) | (VMType::Float(x), VMType::Integer(m)) => {
                VMType::Float(m as f64 + x)
            }

            // array + e and e + array
            (other, VMType::Array(mut xs)) | (VMType::Array(mut xs), other) => {
                xs.push(other);
                VMType::Array(xs)
            }

            (a, b) => panic!("Cannot add {a} and {b}"),
        }
    }
}

impl std::ops::Rem for VMType {
    type Output = VMType;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int % int
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m % n),

            // float % float
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x % y),

            // int % float
            (VMType::Integer(m), VMType::Float(x)) => VMType::Float(m as f64 % x),

            // float % int
            (VMType::Float(x), VMType::Integer(m)) => VMType::Float(x % m as f64),

            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}

impl std::ops::Div for VMType {
    type Output = VMType;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int / int
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Float(m as f64 / n as f64),

            // float / float
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x / y),

            // int / float
            (VMType::Integer(m), VMType::Float(x)) => VMType::Float(m as f64 / x),

            // float / int
            (VMType::Float(x), VMType::Integer(m)) => VMType::Float(x / m as f64),

            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}

macro_rules! next {
    // Grab the next identifier
    ($tokens:expr, "identifier") => {{
        match $tokens.next().unwrap() {
            Token::Identifier(value) => value.clone(),
            other => panic!("Expected identifier, got {other:?}"),
        }
    }};

    // Grab the next code block
    ($tokens:expr, "block") => {{
        match $tokens.next().unwrap() {
            Token::Block(block) => block.to_vec(),
            other => panic!("Expected code block, got {other:?}"),
        }
    }};
}

macro_rules! perform_binary_op {
    ($data:expr, $operator:tt) => {{
        let (a, b) = (
            $data.pop().expect("Binary operation expect 2 values on the stack"),
            $data.pop().expect("Binary operation expect 2 values on the stack")
        );

        $data.push(b $operator a)
    }};

    ($data:expr, $operator:tt, $vmtype:expr) => {{
        let (a, b) = (
            $data.pop().expect("Binary operation expect 2 values on the stack"),
            $data.pop().expect("Binary operation expect 2 values on the stack")
        );

        $data.push($vmtype((b $operator a) as i64))
    }};
}

pub fn parse_xs<'a>(
    tokens: &'a mut std::slice::Iter<Token>,
    data: &'a mut Vec<VMType>,
    globals: &'a mut HashMap<String, VMType>,
    functions: &'a mut HashMap<String, Vec<Token>>,
    locals: &'a mut HashMap<String, VMType>,
) {
    let mut xs = Vec::default();

    loop {
        match tokens.next().unwrap() {
            Token::ArrayEnd => break,

            // Recursion for xss of xss
            Token::ArrayStart => {
                parse_xs(tokens, data, globals, functions, locals);
                xs.push(data.pop().unwrap())
            }

            Token::Str(value) => xs.push(VMType::String(value.clone())),

            Token::Float(value) => xs.push(VMType::Float(*value)),

            Token::Block(expr) => xs.push(VMType::Function(expr.clone())),

            Token::Integer(value) => xs.push(VMType::Integer(*value)),

            Token::Identifier(identifier) => {
                match (globals.get(identifier), locals.get(identifier)) {
                    // Found in locals
                    (None, Some(value)) => {
                        xs.push(value.clone()); // Push the variable content
                        continue;
                    }

                    // Found in globals
                    (Some(value), None) => {
                        xs.push(value.clone()); // Push the variable content
                        continue;
                    }

                    // Both in locals and globals
                    (Some(_), Some(_)) => {
                        panic!("{identifier} is both in globals and locals, bruh!")
                    }

                    // Must be in functions
                    (..) => {}
                }

                xs.push(VMType::Function(
                    functions
                        .get(identifier)
                        .unwrap_or_else(|| panic!("{identifier} isn't in scope!"))
                        .clone(),
                ));
            }

            other => panic!("Found unexpected token while parsing array: {other:?}"),
        }
    }

    data.push(VMType::Array(xs))
}

pub fn process_tokens<'a>(
    tokens: &'a mut std::slice::Iter<Token>,  /* Program tokens */
    data: &'a mut Vec<VMType>,                /* Program data stack */
    globals: &'a mut HashMap<String, VMType>, /* Global variables */
    functions: &'a mut HashMap<String, Vec<Token>>, /* Global functions */
) -> Result<&'a mut Vec<VMType>> {
    let mut locals = HashMap::new();

    while let Some(token) = tokens.next() {
        match token {
            // Call anonymous functions
            Token::Call => match data.pop() {
                Some(VMType::Function(tokens)) => {
                    process_tokens(&mut tokens.iter(), data, &mut globals.clone(), functions)?;
                }
                Some(other) => panic!("Cannot call {other}"),
                None => panic!("Nothing to call"),
            },

            // Parse a function definition
            Token::FuncStart => {
                let name = next!(tokens, "identifier");

                let mut parameters = Vec::default();
                let mut final_block = Vec::default();
                let mut function_block = Vec::default();

                loop {
                    let next_token = tokens.next();

                    match next_token {
                        Some(Token::Block(block)) => {
                            block
                                .iter()
                                .for_each(|token| function_block.push(token.clone()));

                            break;
                        }

                        Some(Token::Identifier(_)) => {
                            parameters.push(next_token.unwrap().clone());
                        }

                        other => panic!("{other:?}"),
                    }
                }

                parameters.reverse();

                for parameter in parameters {
                    final_block.push(Token::Assignment);
                    final_block.push(parameter)
                }

                final_block.append(&mut function_block);

                functions.insert(name, final_block);
            }

            // Parse while statement
            Token::While => {
                // While requires a code block to execute
                let tokens = next!(tokens, "block");

                while let Some(VMType::Integer(1)) = data.pop() {
                    process_tokens(&mut tokens.iter(), data, globals, functions).unwrap();
                }
            }

            // Parse for statement
            Token::For => {
                // For requires a code block to execute
                let tokens = next!(tokens, "block");

                // For requires an indexable on the stack top
                match data.pop() {
                    Some(VMType::Array(xs)) => {
                        xs.iter().for_each(|x| {
                            data.push(x.clone());
                            process_tokens(&mut tokens.iter(), data, globals, functions).unwrap();
                        });
                    }

                    Some(VMType::String(string)) => {
                        string.chars().for_each(|x| {
                            data.push(VMType::String(String::from(x)));
                            process_tokens(&mut tokens.iter(), data, globals, functions).unwrap();
                        });
                    }

                    Some(other) => panic!("{other} is not indexable!"),

                    None => panic!("Nothing to index!"),
                }
            }

            // Parse if statement
            Token::If => {
                // If requires a code block to execute
                let tokens = next!(tokens, "block");

                // Check if stack top value is a TRUE value
                if let Some(VMType::Integer(1)) = data.pop() {
                    process_tokens(&mut tokens.iter(), data, globals, functions).unwrap();
                }
            }

            // Parse an xs (recursive => separated function)
            Token::ArrayStart => parse_xs(tokens, data, globals, functions, &mut locals),

            // Assign the stack top value to the next token
            Token::Assignment => match tokens.next() {
                Some(Token::Identifier(name)) => {
                    match name.chars().collect::<Vec<char>>().first() {
                        // Variable name does start with _ <=> Local variable
                        Some('_') => locals.insert(name.clone(), data.pop().unwrap()),

                        // Variable name does not start with _ <=> Global variable
                        Some(_) => globals.insert(name.clone(), data.pop().unwrap()),

                        // No variable name provided
                        None => panic!("Variable name is missing!"),
                    };
                }

                // We want an identifier to assign value to
                Some(other) => panic!("Can't assign a value to a(n) {other:?}!"),

                // We want at least a token after an assignment
                None => panic!("Nothing to assign to!"),
            },

            // Process the 'is greater than' binary operation
            Token::Gt => perform_binary_op!(data, >, VMType::Integer),

            // Process the 'is lower than' binary operation
            Token::Lt => perform_binary_op!(data, <, VMType::Integer),

            // Process the 'is equal to' binary operation
            Token::Eq => perform_binary_op!(data, ==, VMType::Integer),

            // Process the 'add' binary operation
            Token::Add => perform_binary_op!(data, +),

            // Process the 'substract' binary operation
            Token::Sub => perform_binary_op!(data, -),

            // Process the 'multiply' binary operation
            Token::Mul => perform_binary_op!(data, *),

            // Process the 'divide' binary operation
            Token::Div => perform_binary_op!(data, /),

            // Process the 'modulo' binary operation
            Token::Mod => perform_binary_op!(data, %),

            // Clear the data stack
            Token::Clear => data.clear(),

            // Push a string to the stack
            Token::Str(content) => data.push(VMType::String(content.clone())),

            // Push a float to the stack
            Token::Float(content) => data.push(VMType::Float(*content)),

            // Push an integer to the stack
            Token::Integer(content) => data.push(VMType::Integer(*content)),

            // Push an anonymous function to the stack
            Token::Block(tokens) => data.push(VMType::Function(tokens.clone())),

            // Get the n'th element of an indexable
            Token::Get => match (data.pop(), data.pop()) {
                (Some(VMType::Integer(index)), Some(VMType::Array(xs))) => {
                    data.push(xs.get(index as usize).unwrap().clone());
                }

                (Some(VMType::Integer(index)), Some(VMType::String(string))) => {
                    data.push(VMType::String(
                        string.chars().nth(index as usize).unwrap().to_string(),
                    ));
                }

                (Some(other), Some(VMType::Integer(_))) => panic!("{other} is not indexable!"),

                (..) => panic!("Nothing to index!"),
            },

            Token::Identifier(identifier) => match identifier.as_str() {
                "print" => print!("{}", data.last().unwrap()),

                "read" => {
                    let (Some(VMType::Integer(bytes)), Some(VMType::String(path))) = (data.pop(), data.pop()) else {
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
                    // Include requires a string on the stack
                    let Some(VMType::String(target)) = data.pop() else {
                        panic!()
                    };

                    let content = match target.as_str() {
                        "std" => include_str!("../stdlib/std.maeel").to_string(),

                        // Read file to include
                        _ => std::fs::read_to_string(format!("{}.maeel", target.replace('.', "/")))
                            .expect("Failed to include file"),
                    };

                    process_tokens(
                        &mut lex_into_tokens(&content).iter(),
                        &mut Vec::default(), // don't copy the data
                        globals,             // give a ref to the globals
                        functions,           // give a ref to the functions
                    )?;
                }

                identifier => {
                    match (globals.get(identifier), locals.get(identifier)) {
                        // Found in locals
                        (None, Some(value)) => {
                            data.push(value.clone()); // Push the variable content
                            continue;
                        }

                        // Found in globals
                        (Some(value), None) => {
                            data.push(value.clone()); // Push the variable content
                            continue;
                        }

                        // Both in locals and globals
                        (Some(_), Some(_)) => panic!("{identifier} isn't in scope!"),

                        // Must be in functions
                        (..) => {}
                    }

                    // Execute the function
                    process_tokens(
                        /* Extract function tokens */
                        &mut functions.get(identifier).expect(identifier).clone().iter(),
                        data,                   // give a ref to the data
                        &mut globals.clone(),   // copy the globals
                        &mut functions.clone(), // copy the functions
                    )?;
                }
            },

            // This should be here
            Token::BlockStart | Token::ArrayEnd => {
                panic!()
            }

            Token::BlockEnd => {}
        };
    }

    Ok(data)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Block(Vec<Token>),  // (...)
    Str(String),        // "..."
    Identifier(String), // abc
    Integer(i64),       // 123
    Float(f64),         // 12.3
    Call,               // &
    Add,                // +
    Sub,                // -
    Mul,                // *
    Mod,                // %
    Div,                // /
    Eq,                 // =
    Gt,                 // >
    Lt,                 // <
    Get,                // get
    Clear,              // clear
    Assignment,         // ->
    FuncStart,          // fun
    ArrayStart,         // {
    ArrayEnd,           // }
    BlockStart,         // (
    BlockEnd,           // )
    If,                 // =>
    For,                // for
    While,              // while
}

fn extract_blocks(tokens: &[Token]) -> Vec<Token> {
    let mut output = Vec::new();
    let mut tokens_iter = tokens.iter();

    while let Some(token) = tokens_iter.next() {
        output.push(match token {
            Token::BlockStart => {
                let mut depth = 1;
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
    // Code block depth
    let mut depth = 0;

    // Output tokens
    let mut tokens = Vec::default();

    // Peekable, so we can look the next value without
    // poping it
    let mut characters = code.chars().peekable();

    while let Some(character) = characters.next() {
        match character {
            // Comments are ignored
            '|' => {
                for character in characters.by_ref() {
                    if character != '\n' {
                        continue;
                    }
                    break;
                }
            }

            // Whitespaces are ignored
            ' ' | '\n' => continue,

            '(' => {
                tokens.push(Token::BlockStart);
                depth += 1;
            }

            ')' => {
                tokens.push(Token::BlockEnd);
                depth -= 1;
            }

            // Lexify strings
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

                    content.push(match (character, content_vec.get(index)) {
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

                tokens.push(Token::Str(content))
            }

            // Lexify identifiers
            'a'..='z' | 'A'..='Z' | '_' => {
                // Identifier content
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_alphanumeric() || character == '_'
                });

                tokens.push(match content.as_str() {
                    "fun" => Token::FuncStart,
                    "while" => Token::While,
                    "for" => Token::For,
                    "get" => Token::Get,
                    "then" => Token::If,
                    "clear" => Token::Clear,
                    _ => Token::Identifier(content),
                });
            }

            // Lexify numerics
            '0'..='9' => {
                // Numeric content
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_ascii_digit() || character == '.' || character == '_'
                });

                // Numeric contains . => it is a float, otherwise,
                // it is an integer
                tokens.push(if content.contains('.') {
                    assert_eq!(content.matches('.').count(), 1); // Float must contain one point

                    Token::Float(content.parse().unwrap())
                } else {
                    Token::Integer(content.parse().unwrap())
                });
            }

            // Lexify equal symbol, or if
            '=' => match characters.peek() {
                Some('>') => {
                    tokens.push(Token::If);
                    characters.next();
                }

                _ => tokens.push(Token::Eq),
            },

            // Lexify minus symbol, or assignment
            '-' => match characters.peek() {
                Some('>') => {
                    tokens.push(Token::Assignment);
                    characters.next();
                }

                _ => tokens.push(Token::Sub),
            },

            _ => tokens.push(match character {
                '+' => Token::Add,
                '*' => Token::Mul,
                '/' => Token::Div,
                '%' => Token::Mod,
                '&' => Token::Call,
                '(' => Token::BlockStart,
                ')' => Token::BlockEnd,
                '{' => Token::ArrayStart,
                '}' => Token::ArrayEnd,
                '=' => Token::Eq,
                '<' => Token::Lt,
                '>' => Token::Gt,
                character => panic!("{character}"),
            }),
        }
    }

    assert_eq!(depth, 0);

    // Extract code blocks from the tokens stream
    extract_blocks(tokens.as_slice())
}

fn main() -> Result<()> {
    // Read program content
    let content = std::fs::read_to_string(
        std::env::args() // Shell args
            .collect::<Vec<String>>()
            .get(1)
            .expect("Please provide a file"),
    )
    .expect("Failed to open file");

    // Initial run
    process_tokens(
        &mut lex_into_tokens(&content).iter(),
        &mut Vec::default(),     // data stack
        &mut HashMap::default(), // globals (variables)
        &mut HashMap::default(), // functions
    )?;

    Ok(())
}
