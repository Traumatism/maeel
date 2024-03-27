use std::fs::File;
use std::io::Read;

macro_rules! alias { ($name:ident, $value:expr) => { macro_rules! $name { () => { $value } } } }

alias!(MAEEL_FUN,           "fun");
alias!(MAEEL_INLINE,        "inline");
alias!(MAEEL_INCLUDE,       "include");
alias!(MAEEL_EARRAY,        "array");
alias!(MAEEL_PUTS,          "puts");
alias!(MAEEL_READ,          "read");
alias!(MAEEL_LEN,           "len");
alias!(MAEEL_GET,           "get");
alias!(MAEEL_FUN_PUSH,      '&');
alias!(MAEEL_STR,           '"');
alias!(MAEEL_DEF,           '~');
alias!(MAEEL_THEN,          '?');
alias!(MAEEL_EXEC,          '!');
alias!(MAEEL_ADD,           '+');
alias!(MAEEL_MUL,           '*');
alias!(MAEEL_SUB,           '-');
alias!(MAEEL_DIV,           '/');
alias!(MAEEL_MOD,           '%');
alias!(MAEEL_EQ,            '=');
alias!(MAEEL_GT,            '>');
alias!(MAEEL_LT,            '<');
alias!(MAEEL_COMMENT_START, '#');
alias!(MAEEL_BLOCK_START,   '(');
alias!(MAEEL_BLOCK_END,     ')');

type MAEEL_INT_SIZE /* Encode maeel integers on 32 bits */ = i32;

type MAEEL_FLOAT_SIZE /* Encode maeel floats on 32 bits */ = f32;

type Stack<T> /* Specify that a vec is used like a stack */ = Vec<T>;

type TokenData /* Token and its file name, line */ = (Token, String, u16);

type FunctionData /* Function tokens and inline descriptor */ = (std::rc::Rc<[TokenData]>, bool);

type Mapper<T> /* Map values of type T with their names */ = std::collections::HashMap<String, T>;

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
        std::process::exit(1)
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
    Function(FunctionData),
    String(String),
    Array(Vec<Self>),
}

/* Used for error messages */
#[derive(Debug)]
#[allow(unused)]
enum CordRepr {
    Float,
    Integer,
    Function,
    String,
    Array,
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

    /* Special tokens (used by VM) */
    MaeelAssignment,
}

/* A node in Maeel Stack VM */
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

/* Maeel Stack VM */
struct BocchiVM {
    head: *mut Guitar,
    included: Vec<String>,
}

impl BocchiVM {
    fn process_tokens(
        &mut self,
        tokens: &mut Vec<TokenData>,
        vars: &mut Mapper<Cord>,
        funs: &mut Mapper<FunctionData>,
        rev: bool,
    ) {
        if rev
        /* Sometimes we might act like the tokens vec was a stack */
        { tokens.reverse(); }

        while let Some(token_data) = tokens.pop() {
            let (token, file, line) = /* See `TokenData` */ (
                token_data.0,
                token_data.1,
                token_data.2
            );

            match token {
                | Token::Sym(MAEEL_ADD!()) => binary_op!(|a, b| b + a, self, &file, line),
                | Token::Sym(MAEEL_SUB!()) => binary_op!(|a, b| b - a, self, &file, line),
                | Token::Sym(MAEEL_MUL!()) => binary_op!(|a, b| b * a, self, &file, line),
                | Token::Sym(MAEEL_DIV!()) => binary_op!(|a, b| b / a, self, &file, line),
                | Token::Sym(MAEEL_MOD!()) => binary_op!(|a, b| b % a, self, &file, line),
                | Token::Sym(MAEEL_EQ!()) => binary_op!(|a, b| Cord::Integer((b == a) as MAEEL_INT_SIZE), self, &file, line),
                | Token::Sym(MAEEL_LT!()) => binary_op!(|a, b| Cord::Integer((b < a) as MAEEL_INT_SIZE), self, &file, line),
                | Token::Sym(MAEEL_GT!()) => binary_op!(|a, b| Cord::Integer((b > a) as MAEEL_INT_SIZE), self, &file, line),
                | Token::String(_) | Token::Float(_) | Token::Integer(_) => self.push(token.into()),
                | Token::Block(mut block) => {
                    block.reverse(); /* meow */
                    self.push(Cord::Function((block.as_slice().into(), true)))
                }

                | Token::Sym(MAEEL_FUN_PUSH!()) => {
                    let fun_name /* Function name */ = expect_token!(Identifier, tokens, file, line);

                    let fun /* Function object */ = funs.get(&fun_name).unwrap_or_else(|| {
                        emit_error!(file, line, format!("undefined function: {fun_name:?}"))
                    });

                    self.push(Cord::Function((
                        fun.0.clone(), /* Function tokens */ fun.1, /* Is the function inline? */
                    )))
                }

                | Token::Sym(MAEEL_EXEC!()) /* Manually call a function */ => {
                    let fun /* Function object */ = expect_stack!(Function, self, file, line);

                    match fun.1 /* Function inline descriptor */ {
                        true => /* Push the tokens on the token stack */
                            fun.0.iter().for_each(|token| tokens.push(token.clone())),

                        false => /* Call a new tokens processor */
                            self.process_tokens(&mut fun.0.to_vec(), &mut vars.clone(), funs, true),
                    }
                }

                | Token::Sym(MAEEL_THEN!()) /* Basically "if" statement */ => {
                    let temporary_token /* The boolean (actually an integer, anyways :3) */ = tokens.pop();

                    if expect_stack!(Integer, self, file, line) == 1 {
                        match temporary_token {
                            | Some((Token::Block(temporary_tokens), _, _)) => {
                                let temporary_tokens_len = temporary_tokens.len();

                                for index in 0..(temporary_tokens_len) {
                                    tokens.push(temporary_tokens.get(temporary_tokens_len - index - 1).unwrap().clone())
                                }
                            },
                            | Some(temporary_token) => tokens.push(temporary_token),
                            | None => emit_error!(file, line, "expected something after '?'"),
                        }
                    }
                }

                | Token::MaeelAssignment /* Can be pushed by interpreter only */ => {
                    vars.insert(
                        expect_token!(Identifier, tokens, file, line),
                        self.pop()
                            .unwrap_or_else(|_| emit_error!(file, line, "stack is empty! (maeel)")),
                    );
                }

                | Token::Sym(MAEEL_DEF!()) /* Assign stack top value to next identifier */ => {
                    let name = expect_token!(Identifier, tokens, file, line);

                    if name.starts_with("__")
                    /* This is used to provide private arguments inside inline functions */
                    { panic!() } /* There for it can't be used inside explicit definitions */

                    vars.insert(
                        name,
                        self.pop().unwrap_or_else(|_| emit_error!(file, line, "stack is empty!")),
                    );
                }

                | Token::Sym(character) => emit_error!(file, line, format!("unknown symbol: {character}.")),

                | Token::Identifier(identifier) => match identifier.as_str() {
                    MAEEL_PUTS!() => print!("{}", self.pop().unwrap()),

                    MAEEL_EARRAY!() => self.push(Cord::Array(Vec::new())),

                    MAEEL_FUN!() => {
                        let mut fun_name = expect_token!(Identifier, tokens, file, line);

                        let is_inline /* fun inline name */ = fun_name == MAEEL_INLINE!();

                        if is_inline { fun_name = expect_token!(Identifier, tokens, file, line) }

                        let mut fun_tokens = Vec::default();

                        while let Some(temporary_token) = tokens.pop() {
                            match temporary_token.clone() {
                                (Token::Block(temporary_tokens), _, _) => {
                                    fun_tokens.reverse(); /* uhm */
                                    fun_tokens.extend(temporary_tokens);
                                    fun_tokens.reverse(); /* never ask if maeel could be faster */

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

                    | MAEEL_LEN!() => {
                        let output = match self.pop() {
                            | Ok(Cord::String(string)) => string.len(),
                            | Ok(Cord::Array(xs)) => xs.len(),
                            | Ok(other) => emit_error!(file, line, format!("expected string or array, got {other:?}")),
                            | Err(_) => emit_error!(file, line, "expected string or array, got EOS."),
                        };

                        self.push(Cord::Integer(output as MAEEL_INT_SIZE))
                    }

                    | MAEEL_GET!() => {
                        let index = expect_stack!(Integer, self, file, line) as usize;

                        match self.pop() {
                            | Ok(Cord::Array(xs)) => self.push(
                                xs.get(index)
                                    .unwrap_or_else(|| {
                                        emit_error!(file, line, format!("unknown index: {index}"))
                                    })
                                    .clone(),
                            ),

                            | Ok(Cord::String(string)) => self.push(Cord::String(
                                string
                                    .chars()
                                    .nth(index)
                                    .unwrap_or_else(|| {
                                        emit_error!(file, line, format!("unknown index: {index}"))
                                    })
                                    .to_string(),
                            )),

                            | Ok(other) => emit_error!(file, line, format!("unindexable: {other:?}")),
                            | _ => emit_error!(file, line, format!("unindexable: EOF")),
                        }
                    }

                    | MAEEL_READ!() => {
                        let buf_size = expect_stack!(Integer, self, file, line);

                        let mut buf = vec![0u8; buf_size as usize];

                        /* Copy `buf_size` bytes from file to `buf` */
                        File::open(expect_stack!(String, self, file, line))
                            .unwrap()
                            .read_exact(&mut buf)
                            .unwrap();

                        /*  Convert 8 bytes size integers
                        to MAEEL_INT_SIZE integers */
                        let content_bytes = buf
                            .iter()
                            .map(|byte| Cord::Integer(*byte as MAEEL_INT_SIZE))
                            .collect();

                        self.push(Cord::Array(content_bytes))
                    }

                    | MAEEL_INCLUDE!() => {
                        let target = expect_stack!(String, self, file, line);

                        if self.included.contains(&target)
                        /* We have to make sure we are not including the
                        same file twice. NOTA: this is using relative path; might
                        be better if we used file signature instead. */
                        {
                            continue
                        }

                        self.included.push(target.to_string());

                        let content = match target.clone().as_str() {
                            | "maeel" => include_str!("maeel.maeel").to_string(),
                            _ => std::fs::read_to_string(&target).unwrap_or_else(|_| {
                                emit_error!(file, line, "failed to include file")
                            }),
                        };

                        let temporary_tokens = lex_into_tokens(&content, &target);
                        let temporary_tokens_len = temporary_tokens.len();

                        /* Push tokens from end to start (reverse) */
                        for index in 0..temporary_tokens_len {
                            tokens.push(temporary_tokens.get(
                                temporary_tokens_len - index - 1
                            ).unwrap().clone())
                        }
                    }

                    identifier => {
                        if let Some(value) = vars.get(identifier)
                        /* Variable */
                        { self.push(value.clone()) }

                        else if let Some(fun) = funs.get(identifier)
                        /* Function */
                        {
                            if fun.1
                            /* Inline function*/
                            { fun.0.iter().for_each(|token| tokens.push(token.clone())); }

                            else
                            /* Non-inline */
                            { self.process_tokens(&mut fun.0.clone().to_vec(), &mut vars.clone(), funs, false); }
                        }

                        else
                        /* Oops :3 */
                        { emit_error!(file, line, format!("unknown identifier {identifier}")) }
                    }
                },
            };
        }
    }

    /* Push an object to the stack */
    fn push(&mut self, value: Cord) {
        let future_head /* Create a new head with the value */ = Guitar::new(value);

        if !self.head.is_null() {
            unsafe /* Set current head to the new one "next" */
            { (*future_head).next  = self.head; }
        }

        self.head /* Replace current head with the new one */ = future_head;
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

/* yummy boilerplate :3 */

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
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::Integer(a), Self::Float(b)) | (Self::Float(b), Self::Integer(a)) => {
                (*a as MAEEL_FLOAT_SIZE) == *b
            }
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

fn lex_into_tokens(code: &str, file: &str) -> Stack<TokenData> {
    let mut depth = 0;
    let mut line = 1;
    let mut tokens = Vec::default();
    let mut characters = code.chars().peekable();

    while let Some(character) = characters.next() {
        match character {
            | '\n' /* Ignore new lines */ => line += 1,
            | ' ' | '\t' => /* Ignore whitespaces */ continue,

            | MAEEL_COMMENT_START!() => { characters.by_ref().find(|&c| c == '\n'); }

            | MAEEL_BLOCK_START!() | MAEEL_BLOCK_END!() => {
                tokens.push((Token::Sym(character), file, line));
                depth += if character == MAEEL_BLOCK_START!() { 1 } else { -1 };
            }

            | MAEEL_STR!() /* Dirty strings */ => {
                let content_vector = characters
                    .by_ref()
                    .take_while(|&character| character != MAEEL_STR!())
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
                                | 'n' => '\n',
                                | 't' => '\t',
                                | '\\' => '\\',
                                | MAEEL_STR!() => MAEEL_STR!(),
                                | _ => {
                                    emit_error!(
                                        file, line, format!("invalid escape sequence: \\{next_character}")
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

            | 'a'..='z' | 'A'..='Z' | '_' | 'α'..='ω' /* Create some variables :3 */ => tokens.push((
                Token::Identifier(take_with_predicate!(character, characters, |&c| c.is_alphanumeric() || c == '_')),
                file, line,
            )),

            | '0'..='9' /* Do some maths :3 */ => {
                let content = take_with_predicate!(character, characters, |&c| c.is_ascii_digit() || c == '.' || c == '_');

                tokens.push((
                    if content.contains('.') { Token::Float(content.parse().unwrap()) }
                    else { Token::Integer(content.parse().unwrap()) },
                    file, line,
                ));
            }

            _ => tokens.push((Token::Sym(character), file, line)),
        }
    }

    assert_eq!(depth, 0);

    /* We need to parse differents code blocks now */
    let mut stack = Vec::default();
    let mut output = Vec::default();
    let mut temporary_tokens = Vec::default();

    for token in tokens.iter() {
        match token {
            | (Token::Sym(MAEEL_BLOCK_START!()), _, _)
            /* Code block inside code block, meh */
            => {
                stack.push(temporary_tokens);
                temporary_tokens = Vec::default();
            }

            | (Token::Sym(MAEEL_BLOCK_END!()), _, _)
            /* Something is done, lets figure out what it is :3 */
            => {
                /* This operation must be veryyy expensive in time/memory usage */
                let mut nested_tokens = temporary_tokens.clone();

                match stack.pop() {
                    | Some(previous_tokens)
                    /* Finished to parse the code block inside current code block */
                    => {
                        temporary_tokens = previous_tokens;
                        temporary_tokens.push((Token::Block(nested_tokens), file.into(), line));
                    }

                    | None
                    /* Current code block parsing is done */
                    => {
                        nested_tokens.reverse();
                        output.push((Token::Block(nested_tokens), file.to_string(), line))
                    },
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
        included: Vec::default(), /* Preventing to include the same file twice */
    }
    .process_tokens(
        &mut lex_into_tokens(&std::fs::read_to_string(&file).unwrap(), &file),
        &mut std::collections::HashMap::default(), /* Variables Hashmap */
        &mut std::collections::HashMap::default(), /* Functions Hashmap */
        true,
    )
}
