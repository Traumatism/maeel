/*
This code was made without the help of any language
server and auto-formatter.

(Qualité Artisale)
*/

use std::fs::File;
use std::io::Read;

/* Assign names to literals */
macro_rules! alias { ($($name:ident, $value:expr),* $(,)?) => { $(macro_rules! $name {() => { $value }} )* } }

alias!(
    MAEEL_FUN,           "fun",
    MAEEL_INLINE,        "inline",
    MAEEL_INCLUDE,       "include",
    MAEEL_EARRAY,        "array",
    MAEEL_PUTS,          "puts",
    MAEEL_READ,          "read",
    MAEEL_LEN,           "len",
    MAEEL_GET,           "get",
    MAEEL_FUN_PUSH,      '&',
    MAEEL_STR,           '"',
    MAEEL_DEF,           '~',
    MAEEL_THEN,          '?',
    MAEEL_EXEC,          '!',
    MAEEL_ADD,           '+',
    MAEEL_MUL,           '*',
    MAEEL_SUB,           '-',
    MAEEL_DIV,           '/',
    MAEEL_MOD,           '%',
    MAEEL_EQ,            '=',
    MAEEL_GT,            '>',
    MAEEL_LT,            '<',
    MAEEL_BLOCK_START,   '(',
    MAEEL_BLOCK_END,     ')',
    MAEEL_COMMENT_START, '#',
);

type MAEEL_INT_SIZE   /* Encode maeel integers on 32 bits */          = i32;
type MAEEL_FLOAT_SIZE /* Encode maeel floats on 32 bits */            = f32;
type Stack<T>         /* Specify that a vec is used like a stack */   = Vec<T>;
type TokenData        /* Token and its file name, line */             = (Token, String, u16);
type FunData          /* Fun tokens and inline descriptor */          = (std::rc::Rc<[TokenData]>, bool);
type Mapper<T>        /* Map values of type T with their names */     = std::collections::HashMap<String, T>;

macro_rules! expect_token {
    ($token:tt, $tokens:expr, $fl:expr, $line:expr) => {{
        match $tokens.pop() {
            | Some((Token::$token(value), _, _)) => value,
            | Some((other, other_file, other_line)) => emit_error!(
                other_file, other_line, format!("expected {:?}, got {other:?}", TokenRepr::$token)
            ),
            | None => emit_error!($fl, $line, format!("expected {:?}, got EOF", TokenRepr::$token)),
        }
    }};
}

macro_rules! expect_stack {
    ($tpe:tt, $stack:expr, $fl:expr, $line:expr) => {{
        match $stack.pop() {
            | Ok(Cord::$tpe(value)) => value,
            | Ok(other) => emit_error!($fl, $line, format!("expected {:?} on the stack, got {other:?}", CordRepr::$tpe)),
            | Err(_) => emit_error!($fl, $line, format!("expected {:?}, got EOF", CordRepr::$tpe)),
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
        for _ in (1..content.len()) { $characters.next(); }
        content
    }};
}

macro_rules! binop {
    ($app:expr, $self:expr, $file:expr, $line:expr) => {{
        let output = $app(
            $self.pop().unwrap_or_else(|_| { emit_error!($file, $line, "stack is empty! (binary operation LHS)") }),
            $self.pop().unwrap_or_else(|_| { emit_error!($file, $line, "stack is empty! (binary operation RHS)") }),
        );

        $self.push(output)
    }};
}

/* Types that are used by the VM */
#[derive(Debug, Clone)]
enum Cord {
    Flt(MAEEL_FLOAT_SIZE),
    Int(MAEEL_INT_SIZE),
    Fun(FunData),
    Str(String),
    Lst(Vec<Self>),
}

/* Used for error messages */
#[derive(Debug)] enum CordRepr { Flt, Int, Fun, Str, Lst }

/* Tokens used by the lexer */
#[derive(Clone, Debug)]
enum Token {
    Block(Vec<TokenData>),
    Str(String),
    Name(String),
    Int(MAEEL_INT_SIZE),
    Flt(MAEEL_FLOAT_SIZE),
    Sym(char),
}

/* Used for error messages */
#[derive(Debug)] enum TokenRepr { Block, Str, Name, Int, Flt, Sym }

/* A node in Maeel Stack VM */
struct Guitar {
    value: Cord,
    next: *mut Guitar,
}

impl Guitar {
    fn new(value: Cord) -> *mut Self {
        Box::into_raw(Box::new(Guitar { value, next: std::ptr::null_mut() }))
    }
}

/* Maeel Stack VM */
struct BocchiVM {
    head: *mut Guitar,
}

impl BocchiVM {
    fn process_tokens(
        &mut self,
        tokens: &mut Vec<TokenData>,
        vars: &mut Mapper<Cord>,
        funs: &mut Mapper<FunData>,
        rev: bool,
    ) {
        if rev /* Sometimes we might act like the tokens vec was a stack */ { tokens.reverse(); }

        while let Some(token_data) = tokens.pop() {
            let (token, file, line) = /* See `TokenData` */ (
                token_data.0, token_data.1, token_data.2
            );

            match token {
                | Token::Sym(MAEEL_ADD!()) => binop!(|a, b: Cord| b.add(a), self, &file, line),
                | Token::Sym(MAEEL_SUB!()) => binop!(|a, b: Cord| b.sub(a), self, &file, line),
                | Token::Sym(MAEEL_MUL!()) => binop!(|a, b: Cord| b.mul(a), self, &file, line),
                | Token::Sym(MAEEL_DIV!()) => binop!(|a, b: Cord| b.div(a), self, &file, line),
                | Token::Sym(MAEEL_MOD!()) => binop!(|a, b: Cord| b.rem(a), self, &file, line),
                | Token::Sym(MAEEL_EQ!()) => binop!(|a, b| Cord::Int((b == a) as MAEEL_INT_SIZE), self, &file, line),
                | Token::Sym(MAEEL_LT!()) => binop!(|a, b| Cord::Int((b < a) as MAEEL_INT_SIZE), self, &file, line),
                | Token::Sym(MAEEL_GT!()) => binop!(|a, b| Cord::Int((b > a) as MAEEL_INT_SIZE), self, &file, line),
                | Token::Str(_) | Token::Flt(_) | Token::Int(_) => self.push(token.into()),
                | Token::Block(mut block) => {
                    block.reverse(); /* meow */
                    self.push(Cord::Fun((block.as_slice().into(), true)))
                }
                | Token::Sym(MAEEL_FUN_PUSH!()) => {
                    let fun_name /* Fun name */   = expect_token!(Name, tokens, file, line);
                    let fun      /* Fun object */ = funs.get(&fun_name).unwrap_or_else(|| {
                        emit_error!(file, line, format!("undefined function: {fun_name:?}"))
                    });

                    self.push(Cord::Fun((
                        fun.0.clone(), /* Fun tokens */
                        fun.1,         /* Is the function inline? */
                    )))
                }
                | Token::Sym(MAEEL_EXEC!()) /* Manually call a function */ => {
                    let fun /* Fun object */ = expect_stack!(Fun, self, file, line);

                    match fun.1 /* Fun inline descriptor */ {
                        | true => /* Push the tokens on the token stack */ fun.0.iter().for_each(|token| tokens.push(token.clone())),
                        | false => /* Call a new tokens processor */ self.process_tokens(
                            &mut fun.0.to_vec(), &mut vars.clone(), funs, true
                        ),
                    }
                }
                | Token::Sym(MAEEL_THEN!()) /* Basically "if" statement */ => {
                    let temporary_token /* The boolean (actually an integer, anyways :3) */ = tokens.pop();

                    if expect_stack!(Int, self, file, line) == 1 {
                        match temporary_token {
                            | Some((Token::Block(temporary_tokens), _, _)) => {
                                let temporary_tokens_len = temporary_tokens.len();
                                for index in 0..(temporary_tokens_len) { /* Pushing from end to start */
                                    tokens.push(temporary_tokens.get(temporary_tokens_len - index - 1).unwrap().clone())
                                }
                            },
                            | Some(temporary_token) => tokens.push(temporary_token),
                            | None => emit_error!(file, line, "expected something after '?'"),
                        }
                    }
                }
                | Token::Sym('§') /* Can be pushed by interpreter only */ => {
                    vars.insert(
                        expect_token!(Name, tokens, file, line),
                        self.pop().unwrap_or_else(|_| emit_error!(file, line, "stack is empty! (maeel)")),
                    );
                }
                | Token::Sym(MAEEL_DEF!()) /* Assign stack top value to next identifier */ => {
                    let name = expect_token!(Name, tokens, file, line);

                    if name.starts_with("__") /* Private field */ { panic!(/* TODO: make the error message */) }

                    vars.insert(
                        name, self.pop().unwrap_or_else(|_| emit_error!(file, line, "stack is empty!")),
                    );
                }
                | Token::Sym(character) => emit_error!(file, line, format!("unknown symbol: {character}.")),
                | Token::Name(identifier) => match identifier.as_str() {
                    | MAEEL_PUTS!() => print!("{}", self.pop().unwrap()),
                    | MAEEL_EARRAY!() => self.push(Cord::Lst(Vec::new())),
                    | MAEEL_FUN!() => {
                        let mut fun_name = expect_token!(Name, tokens, file, line);
                        let mut fun_tokens = Vec::default();

                        let is_inline /* fun inline name */ = fun_name == MAEEL_INLINE!();
                        if is_inline { fun_name = expect_token!(Name, tokens, file, line) }

                        while let Some(temporary_token) = tokens.pop() {
                            match temporary_token.clone() {
                                | (Token::Block(temporary_tokens), _, _) => {
                                    fun_tokens.reverse(); /* uhm */
                                    fun_tokens.extend(temporary_tokens);
                                    fun_tokens.reverse(); /* never ask if maeel could be faster */
                                    break;
                                }
                                | (Token::Name(_), file, line) => {
                                    fun_tokens.push(temporary_token);
                                    fun_tokens.push((Token::Sym('§'), file, line));
                                }
                                | (other, other_file, other_line) => {
                                    emit_error!(
                                        other_file,
                                        other_line,
                                        format!("expected identifier(s) or a code block after 'fun {fun_name}'; got {other:?} instead.")
                                    )
                                }
                            }
                        }

                        funs.insert(fun_name.clone(), (fun_tokens.as_slice().into(), is_inline));
                    }
                    | MAEEL_LEN!() => {
                        let output = match self.pop() {
                            | Ok(Cord::Str(string)) => string.len(),
                            | Ok(Cord::Lst(xs)) => xs.len(),
                            | Ok(other) => emit_error!(file, line, format!("expected string or array, got {other:?}")),
                            | Err(_) => emit_error!(file, line, "expected string or array, got EOS."),
                        } as MAEEL_INT_SIZE;

                        self.push(Cord::Int(output))
                    }
                    | MAEEL_GET!() => {
                        let index = expect_stack!(Int, self, file, line) as usize;

                        match self.pop() {
                            | Ok(Cord::Lst(xs)) => self.push(
                                xs.get(index)
                                    .unwrap_or_else(|| {emit_error!(file, line, format!("unknown index: {index}"))})
                                    .clone(),
                            ),
                            | Ok(Cord::Str(string)) => self.push(Cord::Str(
                                string
                                    .chars()
                                    .nth(index)
                                    .unwrap_or_else(|| {emit_error!(file, line, format!("unknown index: {index}"))})
                                    .to_string(),
                            )),
                            | Ok(other) => emit_error!(file, line, format!("unindexable: {other:?}")),
                            | _ => emit_error!(file, line, format!("unindexable: EOF")),
                        }
                    }
                    | MAEEL_READ!() => {
                        let buf_size = expect_stack!(Int, self, file, line);
                        let mut buf = vec![0u8; buf_size as usize];

                        File::open(expect_stack!(Str, self, file, line)) /* Copy `buf_size` bytes from file to `buf` */
                            .unwrap()
                            .read_exact(&mut buf)
                            .unwrap();

                        let content_bytes = buf /*  Convert 8 bytes size integers to MAEEL_INT_SIZE integers */
                            .iter()
                            .map(|byte| Cord::Int(*byte as MAEEL_INT_SIZE))
                            .collect();

                        self.push(Cord::Lst(content_bytes))
                    }
                    | MAEEL_INCLUDE!() /* This is bad */ => {
                        let target = expect_stack!(Str, self, file, line);

                        let content = match target.clone().as_str() {
                            | "maeel" => include_str!("maeel.maeel").to_string(),
                            | _ => std::fs::read_to_string(&target).unwrap_or_else(|_| {
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
                        if let Some(value) = vars.get(identifier) /* Variable */ { self.push(value.clone()) }
                        else if let Some(fun) = funs.get(identifier) /* Fun */ {
                            if fun.1 /* Inline function*/ { fun.0.iter().for_each(|token| tokens.push(token.clone())); }
                            else /* Non-inline */ { self.process_tokens(&mut fun.0.clone().to_vec(), &mut vars.clone(), funs, false); }
                        }
                        else /* Oops :3 */ { emit_error!(file, line, format!("unknown identifier {identifier}")) }
                    }
                },
            };
        }
    }

    /* Push an object to the stack */
    fn push(&mut self, value: Cord) {
        let future_head /* Create a new head with the value */ = Guitar::new(value);
        if !self.head.is_null() { unsafe /* Set current head to the new one "next" */ { (*future_head).next  = self.head; }}
        self.head /* Replace current head with the new one */ = future_head;
    }

    /* Drop-and-return the stack head */
    fn pop(&mut self) -> Result<Cord, Box<dyn std::error::Error>> {
        if self.head.is_null() { Err("Stack is empty".into()) }
        else {
            let current_head = unsafe { Box::from_raw(self.head) };
            self.head = current_head.next;
            Ok(current_head.value)
        }
    }
}

impl std::fmt::Display for Cord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Str(x) => write!(f, "{}", x),
            | Self::Fun(_) => write!(f, "Fun"),
            | Self::Flt(x) => write!(f, "{}", x),
            | Self::Int(x) => write!(f, "{}", x),
            | Self::Lst(xs) => {
                write!(f, "{{")?;
                xs.iter().enumerate().for_each(|(i, x)| {
                    if i > 0 { write!(f, " ").unwrap() }
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
            | (Self::Int(a), Self::Int(b)) => Some(a.cmp(b)),
            | (Self::Flt(a), Self::Flt(b)) => Some(a.total_cmp(b)),
            | (Self::Int(a), Self::Flt(b)) | (Self::Flt(b), Self::Int(a)) => {
                Some(b.total_cmp(&(*a as MAEEL_FLOAT_SIZE)))
            }
            | (a, b) => panic!("Cannot compare {a} and {b}"),
        }
    }
}

impl PartialEq for Cord {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            | (Self::Str(a), Self::Str(b)) => a == b,
            | (Self::Lst(a), Self::Lst(b)) => a == b,
            | (Self::Int(a), Self::Int(b)) => a == b,
            | (Self::Flt(a), Self::Flt(b)) => a == b,
            | (Self::Int(a), Self::Flt(b)) | (Self::Flt(b), Self::Int(a)) => (*a as MAEEL_FLOAT_SIZE) == *b,
            | _ => false,
        }
    }
}

impl Cord {
    fn sub(self, rhs: Self) -> Self {
        match (self, rhs) {
            | (Self::Int(m), Self::Int(n)) => Self::Int(m - n),
            | (Self::Flt(x), Self::Flt(y)) => Self::Flt(x - y),
            | (Self::Flt(x), Self::Int(m)) | (Self::Int(m), Self::Flt(x)) => Self::Flt(m as MAEEL_FLOAT_SIZE - x),
            | (a, b) => panic!("Cannot substract {a} and {b}"),
        }
    }

    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            | (Self::Int(m), Self::Int(n)) => Self::Int(m * n),
            | (Self::Flt(x), Self::Flt(y)) => Self::Flt(x * y),
            | (Self::Flt(x), Self::Int(m)) | (Self::Int(m), Self::Flt(x)) => Self::Flt(x * m as MAEEL_FLOAT_SIZE),
            | (Self::Int(m), Self::Str(s)) | (Self::Str(s), Self::Int(m)) => Self::Str(s.repeat(m as usize)),
            | (a, b) => panic!("Cannot multiply {a} and {b}"),
        }
    }

    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            | (Self::Str(a), Self::Str(b)) => Self::Str(a + &b),
            | (Self::Int(m), Self::Int(n)) => Self::Int(m + n),
            | (Self::Flt(x), Self::Flt(y)) => Self::Flt(x + y),
            | (Self::Int(m), Self::Flt(x)) | (Self::Flt(x), Self::Int(m)) => Self::Flt(m as MAEEL_FLOAT_SIZE + x),
            | (other, Self::Lst(mut xs)) | (Self::Lst(mut xs), other) => {
                xs.push(other);
                Self::Lst(xs)
            }
            | (a, b) => panic!("Cannot add {a} and {b}"),
        }
    }

    fn rem(self, rhs: Self) -> Self {
        match (self, rhs) {
            | (Self::Int(m), Self::Int(n)) => Self::Int(m % n),
            | (Self::Flt(x), Self::Flt(y)) => Self::Flt(x % y),
            | (Self::Int(m), Self::Flt(x)) => Self::Flt(m as MAEEL_FLOAT_SIZE % x),
            | (Self::Flt(x), Self::Int(m)) => Self::Flt(x % m as MAEEL_FLOAT_SIZE),
            | (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }

    fn div(self, rhs: Self) -> Self {
        match (self, rhs) {
            | (Self::Int(m), Self::Int(n)) => Self::Flt(m as MAEEL_FLOAT_SIZE / n as MAEEL_FLOAT_SIZE),
            | (Self::Flt(x), Self::Flt(y)) => Self::Flt(x / y),
            | (Self::Int(m), Self::Flt(x)) => Self::Flt(m as MAEEL_FLOAT_SIZE / x),
            | (Self::Flt(x), Self::Int(m)) => Self::Flt(x / m as MAEEL_FLOAT_SIZE),
            | (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}

impl From<Token> for Cord {
    fn from(val: Token) -> Self {
        match val {
            | Token::Str(x) => Cord::Str(x),
            | Token::Int(x) => Cord::Int(x),
            | Token::Flt(x) => Cord::Flt(x),
            | Token::Block(x) => Cord::Fun((x.as_slice().into(), false)),
            | _ => panic!(),
        }
    }
}

fn lex_into_tokens(code: &str, file: &str) -> Stack<TokenData> {
    let mut depth      = 0;
    let mut line       = 1;
    let mut tokens     = Vec::default();
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
                let content_vector = characters.by_ref().take_while(|&character| character != MAEEL_STR!()).collect::<Vec<char>>();
                let mut index = 0;
                let mut content = String::with_capacity(content_vector.len());

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
                                | _ => emit_error!(file, line, format!("invalid escape sequence: \\{next_character}"))
                            }
                        }

                        ('\\', None) => emit_error!(file, line, "incomplete escape sequence"),

                        _ => character,
                    });
                }

                tokens.push((Token::Str(content), file, line))
            }

            | 'a'..='z' | 'A'..='Z' | '_' | 'α'..='ω' /* Create some variables :3 */ => tokens.push((
                Token::Name(take_with_predicate!(character, characters, |&c| c.is_alphanumeric() || c == '_')),
                file, line,
            )),

            | '0'..='9' /* Do some maths :3 */ => {
                let content = take_with_predicate!(character, characters, |&c| c.is_ascii_digit() || c == '.' || c == '_');

                tokens.push((
                    if content.contains('.') { Token::Flt(content.parse().unwrap()) }
                    else { Token::Int(content.parse().unwrap()) },
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
            | (Token::Sym(MAEEL_BLOCK_START!()), _, _) /* Code block inside code block, meh */ => {
                stack.push(temporary_tokens);
                temporary_tokens = Vec::default();
            }
            | (Token::Sym(MAEEL_BLOCK_END!()), _, _) /* Something is done, lets figure out what it is :3 */ => {
                let mut nested_tokens = temporary_tokens.clone(); /* This operation must be veryyy expensive in time/memory usage */
                match stack.pop() {
                    | Some(previous_tokens) /* Finished to parse the code block inside current code block */ => {
                        temporary_tokens = previous_tokens;
                        temporary_tokens.push((Token::Block(nested_tokens), file.into(), line));
                    }
                    | None /* Current code block parsing is done */ => {
                        nested_tokens.reverse();
                        output.push((Token::Block(nested_tokens), file.to_string(), line))
                    },
                }
            }
            | (other_token, other_file, other_line) => temporary_tokens.push((other_token.clone(), other_file.to_string(), *other_line)),
        }
    }

    output.append(&mut temporary_tokens);
    output
}

fn main() {
    let file = std::env::args().nth(1).unwrap();

    BocchiVM { head: std::ptr::null_mut() }
    .process_tokens(
        &mut lex_into_tokens(&std::fs::read_to_string(&file).unwrap(), &file),
        &mut std::collections::HashMap::default(), /* Variables Hashmap */
        &mut std::collections::HashMap::default(), /* Funs Hashmap */
        true,
    )
}
