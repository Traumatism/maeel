use std::fs::File;
use std::io::Read;

mod maeellex;

#[macro_use]
use maeellex::*;

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

macro_rules! binop {
    ($app:expr, $self:expr, $file:expr, $line:expr) => {{
        let output = $app(
            $self.pop().unwrap_or_else(|_| emit_error!($file, $line, "stack is empty! (binary operation LHS)")),
            $self.pop().unwrap_or_else(|_| emit_error!($file, $line, "stack is empty! (binary operation RHS)")),
        );

        $self.push(output)
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

/* A node in Maeel Stack VM */
struct Guitar { value: Cord, next: *mut Guitar /* Might be null ptr */ }

impl Guitar {
    fn new(value: Cord) -> *mut Self {
        let guitar_pointer = Box::new(
            Guitar { value, next: std::ptr::null_mut() }
        );

        Box::into_raw(guitar_pointer)
    }
}

/* Maeel Stack VM */
struct BocchiVM { head: *mut Guitar }

impl BocchiVM {
    fn process_tokens(
        &mut self,
        tokens: &mut Vec<TokenData>,
        variables: &mut Mapper<Cord>,
        functions: &mut Mapper<FunData>,
        reverse: bool,
    ) {
        if reverse /* Sometimes we might act like the tokens vec was a stack */ { tokens.reverse(); }

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
                    let function_name /* function name */   = expect_token!(Name, tokens, file, line);
                    let function      /* function object */ = functions.get(&function_name).unwrap_or_else(|| {
                        emit_error!(file, line, format!("undefined function: {function_name:?}"))
                    });

                    self.push(Cord::Fun(function.clone()))
                }
                | Token::Sym(M_EXEC!()) /* Manually call a function */ => {
                    let (function_tokens, inline) = expect_stack!(Fun, self, file, line);

                    match inline {
                        | true  => function_tokens.iter().for_each(|t| tokens.push(t.clone())),
                        | false => self.process_tokens(
                            &mut function_tokens.to_vec(), &mut variables.clone(), functions, true
                        ),
                    }
                }
                | Token::Sym(M_THEN!()) /* Basically "if" statement */ => {
                    let temp_tokens = expect_token!(Block, tokens, file, line);

                    if expect_stack!(Int, self, file, line) /* The boolean (actually an integer, anyways :3) */ == 1 {
                        let temp_tokens_length = temp_tokens.len();

                        for index in 0..(temp_tokens_length) { /* Pushing from end to start */
                            tokens.push(temp_tokens.get(temp_tokens_length - index - 1).unwrap().clone())
                        }
                    }
                }
                | Token::Sym(M_FORCE_DEF!()) /* Can be pushed by interpreter only */ => {
                    variables.insert(
                        expect_token!(Name, tokens, file, line),
                        self.pop().unwrap_or_else(|_| emit_error!(file, line, "stack is empty! (maeel)")),
                    );
                }
                | Token::Sym(M_DEF!()) /* Assign stack top value to next name */ => {
                    let name = expect_token!(Name, tokens, file, line);
                    if name.starts_with("__") /* Private field */ { panic!(/* TODO: make the error message */) }
                    variables.insert(name, self.pop().unwrap_or_else(|_| emit_error!(file, line, "stack is empty!")));
                }
                | Token::Sym(char) => emit_error!(file, line, format!("unknown symbol: {char}.")),
                | Token::Name(name) => match name.as_str() {
                    | M_PUTS!() => print!("{}", self.pop().unwrap() /* TODO: make an error message when stack is empty */),
                    | M_ELIST!() => self.push(Cord::Lst(Vec::new())),
                    | M_FUN!() => {
                        let mut function_name   = expect_token!(Name, tokens, file, line);
                        let mut function_tokens = Vec::default();
                        let is_inline           = function_name == M_INLINE!();

                        if is_inline { function_name = expect_token!(Name, tokens, file, line) }

                        while let Some(temp_token) = tokens.pop() {
                            match temp_token.clone() {
                                | (Token::Block(temp_tokens), _, _) => {
                                    function_tokens.reverse(); /* uhm */
                                    function_tokens.extend(temp_tokens);
                                    function_tokens.reverse(); /* never ask if maeel could be faster */
                                    break; /* TODO: remove this break, f*ck breaks */
                                }
                                | (Token::Name(_), file, line) => {
                                    function_tokens.push(temp_token);
                                    function_tokens.push((Token::Sym('§'), file, line));
                                }
                                | (other, file, line) => {
                                    emit_error!(
                                        file,
                                        line,
                                        format!("expected name(s) or a code block after 'function {function_name}'; got {other:?} instead.")
                                    )
                                }
                            }
                        }

                        functions.insert(function_name.clone(), (function_tokens.as_slice().into(), is_inline));
                    }
                    | M_LEN!() => {
                        let length = match self.pop() {
                            | Ok(Cord::Str(string)) => string.len(),
                            | Ok(Cord::Lst(xs))     => xs.len(),
                            | Ok(other)             => emit_error!(file, line, format!("expected string or list, got {other:?}")),
                            | Err(_)                => emit_error!(file, line, "expected string or list, got EOS."),
                        } as M_INT_SIZE;

                        self.push(Cord::Int(length))
                    }
                    | M_GET!() => {
                        let index = expect_stack!(Int, self, file, line) as usize;

                        match self.pop() {
                            | Ok(Cord::Lst(xs)) => self.push(
                                xs.get(index)
                                    .unwrap_or_else(|| emit_error!(file, line, format!("unknown index: {index}")))
                                    .clone(),
                            ),
                            | Ok(Cord::Str(string)) => self.push(Cord::Str(
                                string
                                    .chars()
                                    .nth(index)
                                    .unwrap_or_else(|| emit_error!(file, line, format!("unknown index: {index}")))
                                    .to_string(),
                            )),
                            | Ok(other) => emit_error!(file, line, format!("unindexable: {other:?}")),
                            | _ => emit_error!(file, line, format!("unindexable: EOF")),
                        }
                    }
                    | M_READ!() => {
                        let buffer_size = expect_stack!(Int, self, file, line);
                        let mut buffer  = vec![0u8; buffer_size as usize];

                        File::open(expect_stack!(Str, self, file, line)) /* Copy `buffer_size` bytes from file to `buf` */
                            .unwrap()
                            .read_exact(&mut buffer)
                            .unwrap();

                        let content_bytes = buffer /*  Convert 8 bytes size integers to M_INT_SIZE integers */
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

                        let temp_tokens     = lex_into_tokens(&content, &target);
                        let temp_tokens_length = temp_tokens.len();

                        /* Push tokens from end to start (reverse) */
                        for index in 0..temp_tokens_length {
                            tokens.push(temp_tokens.get(temp_tokens_length - index - 1).unwrap().clone())
                        }
                    }
                    | name => {
                        if let Some(value) = variables.get(name) {
                            self.push(value.clone())
                        } else if let Some((function_tokens, inline)) = functions.get(name) {
                            match inline {
                                | true  => function_tokens.iter().for_each(|t| tokens.push(t.clone())),
                                | false => self.process_tokens(&mut function_tokens.to_vec(), &mut variables.clone(), functions, false),
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
        let future_head = Guitar::new(value);

        if !self.head.is_null() {
            unsafe { (*future_head).next = self.head; }
        }

        self.head = future_head;
    }

    /* Drop-and-return the stack head */
    fn pop(&mut self) -> Result<Cord, Box<dyn std::error::Error>> {
        match self.head.is_null() {
            | true  => Err("Stack is empty".into()),
            | false => {
                let current_head = unsafe { Box::from_raw(self.head) };
                self.head = current_head.next;
                Ok(current_head.value)
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

fn main() {
    let file = std::env::args().nth(1).unwrap();

    BocchiVM { head: std::ptr::null_mut() }
    .process_tokens(
        &mut lex_into_tokens(&std::fs::read_to_string(&file).unwrap(), &file),
        &mut std::collections::HashMap::default(), /* Variables Hashmap */
        &mut std::collections::HashMap::default(), /* functions Hashmap */
        true,
    )
}
