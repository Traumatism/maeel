/*
This code was made without the help of any language
server and auto-formatter.

(Qualité Artisale)
*/

use std::fs::File;
use std::io::Read;

/* Assign names to literals */
macro_rules! alias {
    ($($name:ident, $value:expr),* $(,)?) => { $(macro_rules! $name {() => { $value }} )* }
}

alias!(
    M_FUN,           "fun",
    M_INLINE,        "inline",
    M_INCLUDE,       "include",
    M_ELIST,         "list",
    M_PUTS,          "puts",
    M_READ,          "read",
    M_LEN,           "len",
    M_GET,           "get",
    M_FUN_PUSH,      '&',
    M_STR,           '"',
    M_DEF,           '~',
    M_THEN,          '?',
    M_EXEC,          '!',
    M_ADD,           '+',
    M_MUL,           '*',
    M_SUB,           '-',
    M_DIV,           '/',
    M_MOD,           '%',
    M_EQ,            '=',
    M_GT,            '>',
    M_LT,            '<',
    M_BLOCK_START,   '(',
    M_BLOCK_END,     ')',
    M_COMMENT_START, '#',
);

type M_INT_SIZE       /* Encode maeel integers on 32 bits        */   = i32;
type M_FLOAT_SIZE     /* Encode maeel floats on 32 bits          */   = f32;
type Stack<T>         /* Specify that a vec is used like a stack */   = Vec<T>;
type TokenData        /* Token and its file name, line           */   = (Token, String, u16);
type FunData          /* Fun tokens and inline descriptor        */   = (std::rc::Rc<[TokenData]>, bool);
type Mapper<T>        /* Map values of type T with their names   */   = std::collections::HashMap<String, T>;

macro_rules! expect_token {
    ($token:tt, $tokens:expr, $fl:expr, $line:expr) => {{
        match $tokens.pop() {
            | Some((Token::$token(value), _, _)) => value,
            | Some((other, file, line))          => emit_error!(file, line, format!("expected {:?}, got {other:?}", TokenRepr::$token)),
            | None                               => emit_error!($fl, $line, format!("expected {:?}, got EOF", TokenRepr::$token)),
        }
    }};
}

macro_rules! expect_stack {
    ($tpe:tt, $stack:expr, $fl:expr, $line:expr) => {{
        match $stack.pop() {
            | Ok(Cord::$tpe(value)) => value,
            | Ok(other)             => emit_error!($fl, $line, format!("expected {:?} on the stack, got {other:?}", CordRepr::$tpe)),
            | Err(_)                => emit_error!($fl, $line, format!("expected {:?}, got EOF", CordRepr::$tpe)),
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
    ($char:expr, $chars:expr, $p:expr) => {{
        let content = std::iter::once($char)
            .chain($chars.clone().take_while($p))
            .collect::<String>();
        for _ in (1..content.len()) { $chars.next(); }
        content
    }};
}

macro_rules! binop {
    ($app:expr, $self:expr, $file:expr, $line:expr) => {{
        let out = $app(
            $self.pop().unwrap_or_else(|_| { emit_error!($file, $line, "stack is empty! (binary operation LHS)") }),
            $self.pop().unwrap_or_else(|_| { emit_error!($file, $line, "stack is empty! (binary operation RHS)") }),
        );

        $self.push(out)
    }};
}

/* Types that are used by the VM */
#[derive(Debug, Clone)]
enum Cord {
    Flt(M_FLOAT_SIZE),
    Int(M_INT_SIZE),
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
    Int(M_INT_SIZE),
    Flt(M_FLOAT_SIZE),
    Sym(char),
}

/* Used for error messages */
#[derive(Debug)] enum TokenRepr { Block, Str, Name, Int, Flt, Sym }

/* A node in Maeel Stack VM */
struct Guitar { value: Cord, next: *mut Guitar }

impl Guitar {
    fn new(value: Cord) -> *mut Self {
        Box::into_raw(Box::new(Guitar { value, next: std::ptr::null_mut() }))
    }
}

/* Maeel Stack VM */
struct BocchiVM { head: *mut Guitar }

impl BocchiVM {
    fn process_tokens(
        &mut self,
        tokens: &mut Vec<TokenData>,
        vars: &mut Mapper<Cord>,
        funs: &mut Mapper<FunData>,
        rev: bool,
    ) {
        if rev /* Sometimes we might act like the tokens vec was a stack */ { tokens.reverse(); }

        while let Some((token, file, line)) = tokens.pop() {
            match token {
                | Token::Sym(M_ADD!()) => binop!(|a, b: Cord| b.add(a), self, &file, line),
                | Token::Sym(M_SUB!()) => binop!(|a, b: Cord| b.sub(a), self, &file, line),
                | Token::Sym(M_MUL!()) => binop!(|a, b: Cord| b.mul(a), self, &file, line),
                | Token::Sym(M_DIV!()) => binop!(|a, b: Cord| b.div(a), self, &file, line),
                | Token::Sym(M_MOD!()) => binop!(|a, b: Cord| b.rem(a), self, &file, line),
                | Token::Sym(M_EQ!()) => binop!(|a, b| Cord::Int((b == a) as M_INT_SIZE), self, &file, line),
                | Token::Sym(M_LT!()) => binop!(|a, b| Cord::Int((b < a) as M_INT_SIZE), self, &file, line),
                | Token::Sym(M_GT!()) => binop!(|a, b| Cord::Int((b > a) as M_INT_SIZE), self, &file, line),
                | Token::Str(_) | Token::Flt(_) | Token::Int(_) => self.push(token.into()),
                | Token::Block(mut block) => {
                    block.reverse(); /* meow */
                    self.push(Cord::Fun((block.as_slice().into(), true)))
                }
                | Token::Sym(M_FUN_PUSH!()) => {
                    let fun_name /* Fun name */   = expect_token!(Name, tokens, file, line);
                    let fun      /* Fun object */ = funs.get(&fun_name).unwrap_or_else(|| {
                        emit_error!(file, line, format!("undefined function: {fun_name:?}"))
                    });

                    self.push(Cord::Fun(fun.clone()))
                }
                | Token::Sym(M_EXEC!()) /* Manually call a function */ => {
                    let (fun_tokens, inline) = expect_stack!(Fun, self, file, line);

                    match inline {
                        | true  => fun_tokens.iter().for_each(|t| tokens.push(t.clone())),
                        | false => self.process_tokens(
                            &mut fun_tokens.to_vec(), &mut vars.clone(), funs, true
                        ),
                    }
                }
                | Token::Sym(M_THEN!()) /* Basically "if" statement */ => {
                    let tmp_tokens = expect_token!(Block, tokens, file, line);

                    if expect_stack!(Int, self, file, line) /* The boolean (actually an integer, anyways :3) */ == 1 {
                        let tmp_tokens_len = tmp_tokens.len();

                        for idx in 0..(tmp_tokens_len) { /* Pushing from end to start */
                            tokens.push(tmp_tokens.get(tmp_tokens_len - idx - 1).unwrap().clone())
                        }
                    }
                }
                | Token::Sym('§') /* Can be pushed by interpreter only */ => {
                    vars.insert(
                        expect_token!(Name, tokens, file, line),
                        self.pop().unwrap_or_else(|_| emit_error!(file, line, "stack is empty! (maeel)")),
                    );
                }
                | Token::Sym(M_DEF!()) /* Assign stack top value to next name */ => {
                    let name = expect_token!(Name, tokens, file, line);
                    if name.starts_with("__") /* Private field */ { panic!(/* TODO: make the error message */) }
                    vars.insert(name, self.pop().unwrap_or_else(|_| emit_error!(file, line, "stack is empty!")));
                }
                | Token::Sym(char) => emit_error!(file, line, format!("unknown symbol: {char}.")),
                | Token::Name(name) => match name.as_str() {
                    | M_PUTS!() => print!("{}", self.pop().unwrap() /* TODO: make an error message when stack is empty */),
                    | M_ELIST!() => self.push(Cord::Lst(Vec::new())),
                    | M_FUN!() => {
                        let mut fun_name   = expect_token!(Name, tokens, file, line);
                        let mut fun_tokens = Vec::default();
                        let is_inline      = fun_name == M_INLINE!();

                        if is_inline { fun_name = expect_token!(Name, tokens, file, line) }

                        while let Some(tmp_token) = tokens.pop() {
                            match tmp_token.clone() {
                                | (Token::Block(tmp_tokens), _, _) => {
                                    fun_tokens.reverse(); /* uhm */
                                    fun_tokens.extend(tmp_tokens);
                                    fun_tokens.reverse(); /* never ask if maeel could be faster */
                                    break; /* TODO: remove this break, f*ck breaks */
                                }
                                | (Token::Name(_), file, line) => {
                                    fun_tokens.push(tmp_token);
                                    fun_tokens.push((Token::Sym('§'), file, line));
                                }
                                | (other, file, line) => {
                                    emit_error!(
                                        file,
                                        line,
                                        format!("expected name(s) or a code block after 'fun {fun_name}'; got {other:?} instead.")
                                    )
                                }
                            }
                        }

                        funs.insert(fun_name.clone(), (fun_tokens.as_slice().into(), is_inline));
                    }
                    | M_LEN!() => {
                        let out = match self.pop() {
                            | Ok(Cord::Str(string)) => string.len(),
                            | Ok(Cord::Lst(xs))     => xs.len(),
                            | Ok(other)             => emit_error!(file, line, format!("expected string or list, got {other:?}")),
                            | Err(_)                => emit_error!(file, line, "expected string or list, got EOS."),
                        } as M_INT_SIZE;

                        self.push(Cord::Int(out))
                    }
                    | M_GET!() => {
                        let idx = expect_stack!(Int, self, file, line) as usize;

                        match self.pop() {
                            | Ok(Cord::Lst(xs)) => self.push(
                                xs.get(idx)
                                    .unwrap_or_else(|| {emit_error!(file, line, format!("unknown idx: {idx}"))})
                                    .clone(),
                            ),
                            | Ok(Cord::Str(string)) => self.push(Cord::Str(
                                string
                                    .chars()
                                    .nth(idx)
                                    .unwrap_or_else(|| {emit_error!(file, line, format!("unknown idx: {idx}"))})
                                    .to_string(),
                            )),
                            | Ok(other) => emit_error!(file, line, format!("unindexable: {other:?}")),
                            | _ => emit_error!(file, line, format!("unindexable: EOF")),
                        }
                    }
                    | M_READ!() => {
                        let buf_size = expect_stack!(Int, self, file, line);
                        let mut buf  = vec![0u8; buf_size as usize];

                        File::open(expect_stack!(Str, self, file, line)) /* Copy `buf_size` bytes from file to `buf` */
                            .unwrap()
                            .read_exact(&mut buf)
                            .unwrap();

                        let content_bytes = buf /*  Convert 8 bytes size integers to M_INT_SIZE integers */
                            .iter()
                            .map(|byte| Cord::Int(*byte as M_INT_SIZE))
                            .collect();

                        self.push(Cord::Lst(content_bytes))
                    }
                    | M_INCLUDE!() /* This is bad */ => {
                        let target = expect_stack!(Str, self, file, line);

                        let content = match target.clone().as_str() {
                            | "maeel" => include_str!("maeel.maeel").to_string(),
                            | _       => std::fs::read_to_string(&target).unwrap_or_else(|_| {
                                emit_error!(file, line, "failed to include file")
                            }),
                        };

                        let tmp_tokens     = lex_into_tokens(&content, &target);
                        let tmp_tokens_len = tmp_tokens.len();

                        /* Push tokens from end to start (reverse) */
                        for idx in 0..tmp_tokens_len {
                            tokens.push(tmp_tokens.get(tmp_tokens_len - idx - 1).unwrap().clone())
                        }
                    }
                    | name => {
                        if let Some(value) = vars.get(name) {
                            self.push(value.clone())
                        } else if let Some((fun_tokens, inline)) = funs.get(name) {
                            match inline {
                                | true  => fun_tokens.iter().for_each(|t| tokens.push(t.clone())),
                                | false => self.process_tokens(&mut fun_tokens.to_vec(), &mut vars.clone(), funs, false),
                            }
                        } else {
                            emit_error!(file, line, format!("unknown name {name}"))
                        }
                    }
                },
            };
        }
    }

    /* Push an object to the stack */
    fn push(&mut self, value: Cord) {
        let future_h = Guitar::new(value);

        if !self.head.is_null() {
            unsafe { (*future_h).next = self.head; }
        }

        self.head = future_h;
    }

    /* Drop-and-return the stack head */
    fn pop(&mut self) -> Result<Cord, Box<dyn std::error::Error>> {
        match self.head.is_null() {
            | true  => Err("Stack is empty".into()),
            | false => {
                let current_h = unsafe { Box::from_raw(self.head) };
                self.head = current_h.next;
                Ok(current_h.value)
            }
        }
    }
}

impl std::fmt::Display for Cord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Str(x)  => write!(f, "{}", x),
            | Self::Fun(_)  => write!(f, "Fun"),
            | Self::Flt(x)  => write!(f, "{}", x),
            | Self::Int(x)  => write!(f, "{}", x),
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
                Some(b.total_cmp(&(*a as M_FLOAT_SIZE)))
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
            | (Self::Int(a), Self::Flt(b)) | (Self::Flt(b), Self::Int(a)) => (*a as M_FLOAT_SIZE) == *b,
            | _ => false,
        }
    }
}

impl Cord {
    fn sub(self, rhs: Self) -> Self {
        match (self, rhs) {
            | (Self::Int(m), Self::Int(n)) => Self::Int(m - n),
            | (Self::Flt(x), Self::Flt(y)) => Self::Flt(x - y),
            | (Self::Flt(x), Self::Int(m)) | (Self::Int(m), Self::Flt(x)) => Self::Flt(m as M_FLOAT_SIZE - x),
            | (a, b) => panic!("Cannot substract {a} and {b}"),
        }
    }

    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            | (Self::Int(m), Self::Int(n)) => Self::Int(m * n),
            | (Self::Flt(x), Self::Flt(y)) => Self::Flt(x * y),
            | (Self::Flt(x), Self::Int(m)) | (Self::Int(m), Self::Flt(x)) => Self::Flt(x * m as M_FLOAT_SIZE),
            | (Self::Int(m), Self::Str(s)) | (Self::Str(s), Self::Int(m)) => Self::Str(s.repeat(m as usize)),
            | (a, b) => panic!("Cannot multiply {a} and {b}"),
        }
    }

    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            | (Self::Str(a), Self::Str(b)) => Self::Str(a + &b),
            | (Self::Int(m), Self::Int(n)) => Self::Int(m + n),
            | (Self::Flt(x), Self::Flt(y)) => Self::Flt(x + y),
            | (Self::Int(m), Self::Flt(x)) | (Self::Flt(x), Self::Int(m)) => Self::Flt(m as M_FLOAT_SIZE + x),
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
            | (Self::Int(m), Self::Flt(x)) => Self::Flt(m as M_FLOAT_SIZE % x),
            | (Self::Flt(x), Self::Int(m)) => Self::Flt(x % m as M_FLOAT_SIZE),
            | (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }

    fn div(self, rhs: Self) -> Self {
        match (self, rhs) {
            | (Self::Int(m), Self::Int(n)) => Self::Flt(m as M_FLOAT_SIZE / n as M_FLOAT_SIZE),
            | (Self::Flt(x), Self::Flt(y)) => Self::Flt(x / y),
            | (Self::Int(m), Self::Flt(x)) => Self::Flt(m as M_FLOAT_SIZE / x),
            | (Self::Flt(x), Self::Int(m)) => Self::Flt(x / m as M_FLOAT_SIZE),
            | (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}

impl From<Token> for Cord {
    fn from(val: Token) -> Self {
        match val {
            | Token::Str(x)   => Cord::Str(x),
            | Token::Int(x)   => Cord::Int(x),
            | Token::Flt(x)   => Cord::Flt(x),
            | Token::Block(x) => Cord::Fun((x.as_slice().into(), false)),
            | _               => panic!(),
        }
    }
}

fn lex_into_tokens(code: &str, file: &str) -> Stack<TokenData> {
    let mut depth  = 0;
    let mut line   = 1;
    let mut tokens = Vec::default();
    let mut chars  = code.chars().peekable();

    while let Some(char) = chars.next() {
        match char {
            | '\n' => line += 1,
            | ' ' | '\t' => continue,
            | M_COMMENT_START!() => { chars.by_ref().find(|&c| c == '\n'); }
            | M_BLOCK_START!() | M_BLOCK_END!() => {
                tokens.push((Token::Sym(char), file, line));

                depth += match char == M_BLOCK_START!() {
                    | true  => 1,
                    | false => -1
                }
            }
            | M_STR!() /* Dirty strings */ => {
                let content_vector = chars.by_ref().take_while(|&char| char != M_STR!()).collect::<Vec<char>>();
                let mut idx        = 0;
                let mut content    = String::with_capacity(content_vector.len());

                while idx < content_vector.len() {
                    let char = content_vector[idx];
                    idx += 1;

                    content.push(match (char, content_vector.get(idx)) {
                        | ('\\', Some(next_char)) => {
                            idx += 1;
                            match next_char {
                                | 'n'      => '\n',
                                | 't'      => '\t',
                                | '\\'     => '\\',
                                | M_STR!() => M_STR!(),
                                | _        => emit_error!(file, line, format!("invalid escape sequence: \\{next_char}"))
                            }
                        }
                        | ('\\', None) => emit_error!(file, line, "incomplete escape sequence"),
                        | _ => char,
                    });
                }

                tokens.push((Token::Str(content), file, line))
            }
            | 'a'..='z' | 'A'..='Z' | '_' | 'α'..='ω' /* Create some variables :3 */ => tokens.push((
                Token::Name(take_with_predicate!(char, chars, |&c| c.is_alphanumeric() || c == '_')),
                file, line,
            )),
            | '0'..='9' /* Do some maths :3 */ => {
                let content = take_with_predicate!(char, chars, |&c| c.is_ascii_digit() || c == '.');

                tokens.push((
                    match content.contains('.') {
                        | true  => Token::Flt(content.parse().unwrap()),
                        | false => Token::Int(content.parse().unwrap())
                    },
                    file, line,
                ));
            }
            | _ => tokens.push((Token::Sym(char), file, line)),
        }
    }

    assert_eq!(depth, 0);

    /* We need to parse differents code blocks now */
    /* TODO: rework on this algorithm */
    let mut stack      = Vec::default();
    let mut out        = Vec::default();
    let mut tmp_tokens = Vec::default();

    for token in tokens.iter() {
        match token {
            | (Token::Sym(M_BLOCK_START!()), _, _) /* Code block inside code block, meh */ => {
                stack.push(tmp_tokens);
                tmp_tokens = Vec::default();
            }
            | (Token::Sym(M_BLOCK_END!()), _, _) /* Something is done, lets figure out what it is :3 */ => {
                let mut nested_tokens = tmp_tokens.clone(); /* This operation must be veryyy expensive in time/memory usage */
                match stack.pop() {
                    | Some(previous_tokens) /* Finished to parse the code block inside current code block */ => {
                        tmp_tokens = previous_tokens;
                        tmp_tokens.push((Token::Block(nested_tokens), file.into(), line));
                    }
                    | None /* Current code block parsing is done */ => {
                        nested_tokens.reverse();
                        out.push((Token::Block(nested_tokens), file.to_string(), line))
                    },
                }
            }
            | (token, file, line) => tmp_tokens.push((token.clone(), file.to_string(), *line)),
        }
    }

    out.append(&mut tmp_tokens);
    out
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
