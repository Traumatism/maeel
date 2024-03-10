use std::fs::File;
use std::io::Read;

type MAEEL_INT_SIZE /* Encode maeel integers on 32 bits */ = i32;
type MAEEL_FLOAT_SIZE /* Encode maeel floats on 32 bits */ = f32;

type TokenData
    /* Token and its file name, line */
    = (Token, String, u16);

macro_rules! expect_token {
    ($token:tt, $tokens:expr, $fl:expr, $line:expr) => {{
        match $tokens.pop() {
            Some((Token::$token(value), _, _)) => value,

            Some((other, other_file, other_line)) => {
                emit_error!(
                    other_file,
                    other_line,
                    format!("expected {:?}, got {other:?}", TokenRepr::$token)
                )
            }

            None => emit_error!(
                $fl,
                $line,
                format!("expected {:?}, got EOF", TokenRepr::$token)
            ),
        }
    }};
}

macro_rules! expect_stack {
    ($tpe:tt, $stack:expr, $fl:expr, $line:expr) => {{
        match $stack.pop() {
            Ok(Cord::$tpe(value)) => value,
            Ok(other) => {
                emit_error!(
                    $fl,
                    $line,
                    format!("expected {:?} on the stack, got {other:?}", CordRepr::$tpe)
                )
            }
            Err(_) => {
                emit_error!(
                    $fl,
                    $line,
                    format!("expected {:?}, got EOF", CordRepr::$tpe)
                )
            }
        }
    }};
}

macro_rules! emit_error {
    ($fl:expr, $line:expr, $message:expr) => {{
        println!("\n{}:{} {}", $fl, $line, $message);
        std::process::exit(1);
    }};
}

macro_rules! take_with_predicate {
    ($character:expr, $characters:expr, $p:expr) => {{
        let content = std::iter::once($character)
            .chain($characters.clone().take_while($p))
            .collect::<String>();

        (1..content.len()).for_each(|_| {
            $characters.next();
        });

        content
    }};
}

macro_rules! binary_op {
    ($app:expr, $self:expr, $file:expr, $line:expr) => {{
        let output = $app(
            /* Left handside */
            $self.pop().unwrap_or_else(|_| {
                emit_error!($file, $line, "stack is empty! (binary operation LHS)")
            }),
            /* Right handside */
            $self.pop().unwrap_or_else(|_| {
                emit_error!($file, $line, "stack is empty! (binary operation RHS)")
            }),
        );

        /* Push op(LHS, RHS) */
        $self.push(output)
    }};
}

/* Types that are used by the VM */
#[derive(Debug)]
enum Cord {
    Float(MAEEL_FLOAT_SIZE),
    Integer(MAEEL_INT_SIZE),
    String(String),
    Array(Vec<Self>),
    Function((std::rc::Rc<[TokenData]>, bool)),
}

/* Used for error messages */
#[derive(Debug)]
#[allow(unused)]
enum CordRepr {
    Float,
    Integer,
    String,
    Array,
    Function,
}

/* Tokens used by the lexer */
#[derive(Clone, Debug)]
enum Token {
    Block(Vec<TokenData>),
    String(String),
    Identifier(String),
    Integer(MAEEL_INT_SIZE),
    Float(MAEEL_FLOAT_SIZE),
    Sym(char),
    MaeelAssignment,
}

/* Used for error messages */
#[derive(Debug)]
#[allow(unused)]
enum TokenRepr {
    Block,
    String,
    Identifier,
    Integer,
    Float,
    Sym,
    MaeelAssignment,
}

struct Guitar {
    value: Cord,
    next: *mut Guitar,
}

impl Guitar {
    fn new(value: Cord) -> *mut Self {
        Box::into_raw(Box::new(Guitar {
            value,
            next: std::ptr::null_mut(),
        }))
    }
}

struct BocchiVM {
    head: *mut Guitar,
    included: Vec<String>,
}

impl BocchiVM {
    fn parse_array(&mut self, tokens: &mut Vec<TokenData>) {
        let mut xs = Vec::default();

        while let Some(token_data) = tokens.pop() {
            let (token, file, line) = (token_data.0, token_data.1, token_data.2);

            match token {
                Token::Sym('}') /* Array's end */ => break,

                Token::Sym('{') /* lil bro is dealing with matrices >:c */ => {
                    self.parse_array(tokens);

                    xs.push(self.pop().unwrap_or_else(|_| {
                        emit_error!(file, line, "stack is empty! (array in array)")
                    }));
                }

                /* Just a value in the array */
                Token::String(_) | Token::Integer(_) | Token::Float(_) => xs.push(token.into()),

                /* Nah you cannot access variables/functions while defining an array hihi */
                _ => emit_error!(file, line, "unknown token found while parsing array"),
            }
        }

        self.push(Cord::Array(xs))
    }

    fn process_tokens(
        &mut self,
        tokens: &mut Vec<TokenData>,
        vars: &mut std::collections::HashMap<String, Cord>,
        funs: &mut std::collections::HashMap<String, (std::rc::Rc<[TokenData]>, bool)>,
    ) {
        tokens.reverse(); /* BAKA stuff */

        while let Some(token_data) = tokens.pop() {
            let (token, file, line) = (token_data.0, token_data.1, token_data.2);

            match token {
                Token::Sym('+') => binary_op!(|a, b| b + a, self, &file, line),

                Token::Sym('-') => binary_op!(|a, b| b - a, self, &file, line),

                Token::Sym('*') => binary_op!(|a, b| b * a, self, &file, line),

                Token::Sym('/') => binary_op!(|a, b| b / a, self, &file, line),

                Token::Sym('%') => binary_op!(|a, b| b % a, self, &file, line),

                Token::Sym('=') => binary_op!(|a, b| Cord::Integer((b == a) as MAEEL_INT_SIZE), self, &file, line),

                Token::Sym('<') => binary_op!(|a, b| Cord::Integer((b < a) as MAEEL_INT_SIZE), self, &file, line),

                Token::Sym('>') => binary_op!(|a, b| Cord::Integer((b > a) as MAEEL_INT_SIZE), self, &file, line),

                Token::Sym('{') /* Parse a sussy array */ => self.parse_array(tokens),

                /* Push a string/float/integer */
                Token::String(_) | Token::Float(_) | Token::Integer(_) => self.push(token.into()),

                Token::Block(mut block) /* Push a code block as inline function */ => {
                    block.reverse(); /* meow */
                    self.push(Cord::Function((block.as_slice().into(), true)))
                }

                Token::Sym('@') /* Push a code block as non-inline function */ => {
                    let mut block = expect_token!(Block, tokens, file, line);
                    block.reverse(); /* meow */
                    self.push(Cord::Function((block.as_slice().into(), false)))
                }

                Token::Sym(':') /* Push a code block from a function as an inline function */ => {
                    let fun_name /* Function name */ = expect_token!(Identifier, tokens, file, line);

                    let fun /* Function object */ = funs.get(&fun_name).unwrap_or_else(|| {
                        emit_error!(file, line, format!("undefined function: {fun_name:?}"))
                    });

                    self.push(Cord::Function((
                        fun.0.clone(), /* Function tokens */
                        fun.1,         /* Is the function inline? */
                    )))
                }

                Token::Sym('!') /* Manually call a function */ => {
                    let fun /* Function object */ = expect_stack!(Function, self, file, line);

                    match fun.1 /* Function inline descriptor */ {
                        true => /* Push the tokens on the token stack */
                            fun.0.iter().for_each(|token| tokens.push(token.clone())),

                        false => /* Call a new tokens processor */
                            self.process_tokens(&mut fun.0.to_vec(), &mut vars.clone(), funs),
                    }
                }

                Token::Sym('?') /* Basically "if" statement */ => {
                    let temporary_token /* The boolean (actually an integer, anyways :3) */ = tokens.pop();

                    if expect_stack!(Integer, self, line, file) == 1 {
                        match temporary_token {
                            Some((Token::Block(temporary_tokens), _, _)) => temporary_tokens
                                .iter()
                                .rev()
                                .for_each(|token| tokens.push(token.clone())),

                            Some(temporary_token) => tokens.push(temporary_token),

                            None => emit_error!(file, line, "expected something after '=>'"),
                        }
                    }
                }

                Token::MaeelAssignment /* MaeelAssignment can be pushed by interpreter only */ => {
                    vars.insert(
                        expect_token!(Identifier, tokens, file, line),
                        self.pop()
                            .unwrap_or_else(|_| emit_error!(file, line, "stack is empty! (maeel)")),
                    );
                }

                Token::Sym('~') /* Assign stack top value to next identifier */ => {
                    let name = expect_token!(Identifier, tokens, file, line);

                    if name.starts_with("__") {
                        panic!()
                    }

                    vars.insert(
                        name,
                        self.pop()
                            .unwrap_or_else(|_| emit_error!(file, line, "stack is empty! (~)")),
                    );
                }

                Token::Sym(character) => panic!("{}", character),

                Token::Identifier(identifier) => match identifier.as_str() {
                    "puts" => print!("{}", self.pop().unwrap()),

                    "fun" => {
                        let mut fun_name = expect_token!(Identifier, tokens, file, line);

                        let is_inline = fun_name == "inline";

                        if is_inline {
                            fun_name = expect_token!(Identifier, tokens, file, line);
                        }

                        let mut fun_tokens = Vec::default();

                        while let Some(temporary_token) = tokens.pop() {
                            match temporary_token.clone() {
                                (Token::Block(temporary_tokens), _, _) => {
                                    fun_tokens.reverse();
                                    fun_tokens.extend(temporary_tokens);
                                    fun_tokens.reverse();

                                    break;
                                }

                                (Token::Identifier(_), file, line) => {
                                    fun_tokens.push(temporary_token);
                                    fun_tokens.push((Token::MaeelAssignment, file, line));
                                }

                                (other, other_file, other_line) => {
                                    emit_error!(other_file, other_line, format!("expected identifier(s) or a code block after 'fun {fun_name}'; got {other:?} instead."))
                                }
                            }
                        }

                        funs.insert(fun_name.clone(), (fun_tokens.as_slice().into(), is_inline));
                    }

                    "len" => {
                        let output = match self.pop() {
                            Ok(Cord::String(string)) => string.len(),
                            Ok(Cord::Array(xs)) => xs.len(),
                            Ok(other) => emit_error!(
                                file,
                                line,
                                format!("expected string or array, got {other:?}")
                            ),
                            Err(_) => emit_error!(file, line, "expected string or array, got EOS."),
                        };

                        self.push(Cord::Integer(output as MAEEL_INT_SIZE))
                    }

                    "get" => {
                        let index = expect_stack!(Integer, self, file, line) as usize;

                        match self.pop() {
                            Ok(Cord::Array(xs)) => self.push(
                                xs.get(index)
                                    .unwrap_or_else(|| {
                                        emit_error!(file, line, format!("unknown index: {index}"))
                                    })
                                    .clone(),
                            ),

                            Ok(Cord::String(string)) => self.push(Cord::String(
                                string
                                    .chars()
                                    .nth(index)
                                    .unwrap_or_else(|| {
                                        emit_error!(file, line, format!("unknown index: {index}"))
                                    })
                                    .to_string(),
                            )),

                            Ok(other) => emit_error!(file, line, format!("unindexable: {other:?}")),

                            _ => emit_error!(file, line, format!("unindexable: EOF")),
                        }
                    }

                    "read" => {
                        let buf_size = expect_stack!(Integer, self, file, line);
                        let mut buf = vec![0u8; buf_size as usize];

                        File::open(expect_stack!(String, self, file, line))
                            .unwrap()
                            .read_exact(&mut buf)
                            .unwrap();

                        let content_bytes = buf
                            .iter()
                            .map(|byte| Cord::Integer(*byte as MAEEL_INT_SIZE))
                            .collect();

                        self.push(Cord::Array(content_bytes))
                    }

                    "include" => {
                        let target = expect_stack!(String, self, file, line);

                        if self.included.contains(&target) {
                            continue
                        }

                        self.included.push(target.clone());

                        let content = match target.clone().as_str() {
                            "std" => include_str!("stdlib/std.maeel").to_string(),
                            "maeel" => include_str!("maeel.maeel").to_string(),
                            "logic" => include_str!("stdlib/logic.maeel").to_string(),
                            "array" => include_str!("stdlib/array.maeel").to_string(),
                            "fp" => include_str!("stdlib/fp.maeel").to_string(),
                            "math" => include_str!("stdlib/math.maeel").to_string(),
                            "string" => include_str!("stdlib/string.maeel").to_string(),
                            "unix" => include_str!("stdlib/unix.maeel").to_string(),
                            _ => std::fs::read_to_string(&target).unwrap_or_else(|_| {
                                emit_error!(file, line, "failed to include file")
                            }),
                        };

                        lex_into_tokens(&content, &target)
                            .iter()
                            .rev()
                            .for_each(|token| tokens.push(token.clone()))
                    }

                    identifier => {
                        if let Some(value) = vars.get(identifier)
                        /* Identifier may be a variable */
                        {
                            self.push(value.clone());
                            continue
                        }

                        if let Some(fun) = funs.get(identifier)
                        /* Not a variable? Then it must be a function! */
                        {
                            if fun.1
                            /* Inline function*/
                            {
                                fun.0.iter().for_each(|token| tokens.push(token.clone()));
                                continue
                            }

                            let mut fun_tokens = fun.0.clone().to_vec();

                            fun_tokens.reverse();

                            self.process_tokens(&mut fun_tokens, &mut vars.clone(), funs);

                            continue
                        }

                        /* Oops :3 */
                        emit_error!(file, line, format!("unknown identifier {identifier}"))
                    }
                },
            };
        }
    }

    /* Push an object to the stack */
    fn push(&mut self, value: Cord) {
        /* Create a new head with the value */
        let future_head = Guitar::new(value);

        if !self.head.is_null() {
            unsafe {
                /* Set current head to the new one "next" */
                (*future_head).next = self.head;
            }
        }

        self.head = future_head; /* Replace the head with the new one */
    }

    /* Drop-and-return the stack head */
    fn pop(&mut self) -> Result<Cord, Box<dyn std::error::Error>> {
        if self.head.is_null() {
            Err("Stack is empty".into())
        } else {
            let current_head = unsafe { Box::from_raw(self.head) };
            self.head = current_head.next;
            Ok(current_head.value)
        }
    }
}

impl Clone for Cord {
    fn clone(&self) -> Self {
        match self {
            Self::Float(a) => Self::Float(*a),
            Self::Integer(a) => Self::Integer(*a),
            Self::String(a) => Self::String(a.clone()),
            Self::Array(a) => Self::Array(a.clone()),
            Self::Function(a) => Self::Function(a.clone()),
        }
    }
}

impl std::fmt::Display for Cord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(x) => write!(f, "{}", x),
            Self::Function(_) => write!(f, "Function"),
            Self::Float(x) => write!(f, "{}", x),
            Self::Integer(x) => write!(f, "{}", x),
            Self::Array(xs) => {
                write!(f, "{{")?;
                xs.iter().enumerate().for_each(|(i, x)| {
                    if i > 0 {
                        write!(f, " ").unwrap()
                    }
                    write!(f, "{}", x).unwrap()
                });
                write!(f, "}}")
            }
        }
    }
}

impl PartialOrd for Cord {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => Some(a.cmp(b)),
            (Self::Float(a), Self::Float(b)) => Some(a.total_cmp(b)),
            (Self::Integer(a), Self::Float(b)) | (Self::Float(b), Self::Integer(a)) => {
                Some(b.total_cmp(&(*a as MAEEL_FLOAT_SIZE)))
            }

            (a, b) => panic!("Cannot compare {a} and {b}"),
        }
    }
}

/* Damn boilerplate starts here... */
impl PartialEq for Cord {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Array(a), Self::Array(b)) => a == b,

            (Self::Integer(a), Self::Float(b)) | (Self::Float(b), Self::Integer(a)) => {
                (*a as MAEEL_FLOAT_SIZE) == *b
            }

            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a == b,

            _ => false,
        }
    }
}

impl std::ops::Sub for Cord {
    type Output = Cord;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(m), Self::Integer(n)) => Self::Integer(m - n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x - y),
            (Self::Float(x), Self::Integer(m)) | (Self::Integer(m), Self::Float(x)) => {
                Self::Float(m as MAEEL_FLOAT_SIZE - x)
            }

            (a, b) => panic!("Cannot substract {a} and {b}"),
        }
    }
}

impl std::ops::Mul for Cord {
    type Output = Cord;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(m), Self::Integer(n)) => Self::Integer(m * n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x * y),
            (Self::Float(x), Self::Integer(m)) | (Self::Integer(m), Self::Float(x)) => {
                Self::Float(x * m as MAEEL_FLOAT_SIZE)
            }
            (Self::Integer(m), Self::String(s)) | (Self::String(s), Self::Integer(m)) => {
                Self::String(s.repeat(m as usize))
            }

            (a, b) => panic!("Cannot multiply {a} and {b}"),
        }
    }
}

impl std::ops::Add for Cord {
    type Output = Cord;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::String(a), Self::String(b)) => Self::String(a + &b),
            (Self::Integer(m), Self::Integer(n)) => Self::Integer(m + n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x + y),
            (Self::Integer(m), Self::Float(x)) | (Self::Float(x), Self::Integer(m)) => {
                Self::Float(m as MAEEL_FLOAT_SIZE + x)
            }
            (other, Self::Array(mut xs)) | (Self::Array(mut xs), other) => {
                xs.push(other);
                Self::Array(xs)
            }

            (a, b) => panic!("Cannot add {a} and {b}"),
        }
    }
}

impl std::ops::Rem for Cord {
    type Output = Cord;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(m), Self::Integer(n)) => Self::Integer(m % n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x % y),
            (Self::Integer(m), Self::Float(x)) => Self::Float(m as MAEEL_FLOAT_SIZE % x),
            (Self::Float(x), Self::Integer(m)) => Self::Float(x % m as MAEEL_FLOAT_SIZE),
            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}

impl std::ops::Div for Cord {
    type Output = Cord;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(m), Self::Integer(n)) => {
                Self::Float(m as MAEEL_FLOAT_SIZE / n as MAEEL_FLOAT_SIZE)
            }
            (Self::Float(x), Self::Float(y)) => Self::Float(x / y),
            (Self::Integer(m), Self::Float(x)) => Self::Float(m as MAEEL_FLOAT_SIZE / x),
            (Self::Float(x), Self::Integer(m)) => Self::Float(x / m as MAEEL_FLOAT_SIZE),
            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}

impl From<Token> for Cord {
    fn from(val: Token) -> Self {
        match val {
            Token::String(x) => Cord::String(x),
            Token::Integer(x) => Cord::Integer(x),
            Token::Float(x) => Cord::Float(x),
            Token::Block(x) => Cord::Function((x.as_slice().into(), false)),
            _ => panic!(),
        }
    }
}

fn lex_into_tokens(code: &str, file: &str) -> Vec<TokenData> {
    let mut depth = 0;
    let mut line = 1;
    let mut tokens = Vec::default();
    let mut characters = code.chars().peekable();

    while let Some(character) = characters.next() {
        match character {
            '#' => {
                characters.by_ref().find(|&c| c == '\n');
            }

            '\n' => line += 1,

            ' ' | '\t' => continue,

            '(' | ')' /* Code block (will parse later ^^) */ => {
                tokens.push((Token::Sym(character), file, line));
                depth += if character == '(' { 1 } else { -1 };
            }

            '"' /* Dirty strings */ => {
                let content_vector = characters
                    .by_ref()
                    .take_while(|&character| character != '"')
                    .collect::<Vec<char>>();

                let mut content = String::with_capacity(content_vector.len());

                let mut index = 0;

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
                                    emit_error!(
                                        file,
                                        line,
                                        format!("invalid escape sequence: \\{next_character}")
                                    )
                                }
                            }
                        }

                        ('\\', None) => emit_error!(file, line, "incomplete escape sequence"),

                        _ => character,
                    });
                }

                tokens.push((Token::String(content), file, line))
            }

            'a'..='z' | 'A'..='Z' | '_' | 'α'..='ω' => tokens.push((
                Token::Identifier(take_with_predicate!(character, characters, |&c| c
                    .is_alphanumeric()
                    || c == '_')),
                file,
                line,
            )),

            '0'..='9' => {
                let content = take_with_predicate!(character, characters, |&c| c.is_ascii_digit()
                    || c == '.'
                    || c == '_');

                tokens.push((
                    if content.contains('.') {
                        Token::Float(content.parse().unwrap())
                    } else {
                        Token::Integer(content.parse().unwrap())
                    },
                    file,
                    line,
                ));
            }

            _ => tokens.push((Token::Sym(character), file, line)),
        }
    }

    assert_eq!(depth, 0);

    let mut stack = Vec::default();
    let mut output = Vec::default();
    let mut temporary_tokens = Vec::default();

    for token in tokens.iter() {
        match token {
            (Token::Sym('('), _, _) => {
                stack.push(temporary_tokens);
                temporary_tokens = Vec::default();
            }

            (Token::Sym(')'), _, _) => {
                let nested_tokens = temporary_tokens.clone();

                match stack.pop() {
                    Some(previous_tokens) => {
                        temporary_tokens = previous_tokens;
                        temporary_tokens.push((Token::Block(nested_tokens), file.into(), line));
                    }

                    _ => output.push((Token::Block(nested_tokens), file.to_string(), line)),
                }
            }

            (other_token, other_file, other_line) => {
                temporary_tokens.push((other_token.clone(), other_file.to_string(), *other_line))
            }
        }
    }

    output.append(&mut temporary_tokens);
    output
}

fn main() {
    let file = std::env::args().nth(1).unwrap();

    BocchiVM {
        head: std::ptr::null_mut(),

        /* Already included maeel files
        (preventing to include the same file twice) */
        included: Vec::default(),
    }
    .process_tokens(
        &mut lex_into_tokens(&std::fs::read_to_string(&file).unwrap(), &file),
        &mut std::collections::HashMap::default(), /* Variables Hashmap */
        &mut std::collections::HashMap::default(), /* Functions Hashmap */
    )
}
