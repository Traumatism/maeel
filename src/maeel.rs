use std::cmp::Ordering;
use std::collections::HashMap;
use std::env::args;
use std::error::Error;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fs::read_to_string;
use std::iter::once;
use std::ptr::null_mut;
use std::rc::Rc;

type Stack<T> /* Specify that a vec is used like a stack */ = Vec<T>;
type Mapper<T> /* Map values of type T with their names */ = HashMap<String, T>;
type TokenData /* Token and its file name, line */ = (Token, String, u16);
type FunData /* Fun tokens and inline descriptor */ = (Rc<[TokenData]>, bool);

macro_rules! expect_token {
    ($t:tt, $ts:expr, $f:expr, $l:expr) => {{
        match $ts.pop() {
            Some((Token::$t(value), _, _)) => value,
            Some((other, file, line)) => emit_error!(
                file,
                line,
                format!("expected {:?}, got {other:?}", TokenRepr::$t)
            ),
            None => emit_error!($f, $l, format!("expected {:?}, got EOF", TokenRepr::$t)),
        }
    }};
}

macro_rules! expect_stack {
    ($t:tt, $s:expr, $f:expr, $l:expr) => {{
        match $s.pop() {
            Ok(Cord::$t(v)) => v,
            Ok(o) => emit_error!(
                $f,
                $l,
                format!("expected {:?} on the stack, got {o:?}", CordRepr::$t)
            ),
            Err(_) => emit_error!($f, $l, format!("expected {:?}, got EOF", CordRepr::$t)),
        }
    }};
}

macro_rules! binop {
    ($a:expr, $s:expr, $f:expr, $l:expr) => {{
        let output = $a(
            $s.pop()
                .unwrap_or_else(|_| emit_error!($f, $l, "stack is empty! (binary operation LHS)")),
            $s.pop()
                .unwrap_or_else(|_| emit_error!($f, $l, "stack is empty! (binary operation RHS)")),
        );

        $s.push(output)
    }};
}

macro_rules! emit_error {
    ($fl:expr, $line:expr, $message:expr) => {{
        println!("\n{}:{} {}", $fl, $line, $message);
        panic!();
    }};
}

macro_rules! take_with_predicate {
    ($char:expr, $chars:expr, $p:expr) => {{
        let content: String = once($char).chain($chars.clone().take_while($p)).collect();
        (1..content.len()).for_each(|_| {
            $chars.next();
        });
        content
    }};
}

/* Types that are used by the VM */
#[derive(Debug, Clone)]
enum Cord {
    Float(f32),
    Int(i32),
    Fun(FunData),
    Str(String),
    List(Vec<Self>),
}

/* Used for error messages */
#[derive(Debug)]
enum CordRepr {
    Int,
    Fun,
}

/* Tokens used by the lexer */
#[derive(Clone, Debug, PartialEq)]
enum Token {
    Block(Stack<TokenData>),
    Str(String),
    Name(String),
    Int(i32),
    Float(f32),
    Symbol(char),
}

macro_rules! True {
    ($l:expr, $f:expr) => {
        Cord::Fun((
            [(Token::Name("drop".to_string()), $f, $l)]
                .as_slice()
                .into(),
            true,
        ))
    };
}

macro_rules! False {
    ($l:expr, $f:expr) => {
        Cord::Fun((
            [
                (Token::Name("drop".to_string()), $f, $l),
                (Token::Name("swap".to_string()), $f, $l),
            ]
            .as_slice()
            .into(),
            true,
        ))
    };
}

/* Used for error messages */
#[derive(Debug)]
enum TokenRepr {
    Str,
    Name,
}

fn lex_into_tokens(code: &str, file: &str) -> Stack<TokenData> {
    let mut depth = 0;
    let mut line = 1;
    let mut tokens = Vec::default();
    let mut chars = code.chars().peekable();

    while let Some(char) = chars.next() {
        match char {
            '\n' => line += 1,
            ' ' | '\t' => continue,
            '#' => {
                take_with_predicate!(char, chars, |&c| c != '\n');
                line += 1;
            }
            '(' | ')' => {
                tokens.push((Token::Symbol(char), file, line));
                depth += if char == '(' { 1 } else { -1 };
            }
            '"' => {
                let content_vector = chars
                    .by_ref()
                    .take_while(|&char| char != '"')
                    .collect::<Vec<char>>();

                let mut index = 0;
                let mut content = String::with_capacity(content_vector.len());

                while index < content_vector.len() {
                    let char = content_vector[index];
                    index += 1;

                    content.push(match (char, content_vector.get(index)) {
                        ('\\', Some(next_char)) => {
                            index += 1;

                            match next_char {
                                'n' => '\n',
                                't' => '\t',
                                '\\' => '\\',
                                '"' => '"',
                                _ => emit_error!(
                                    file,
                                    line,
                                    format!("invalid escape sequence: \\{next_char}")
                                ),
                            }
                        }
                        ('\\', None) => emit_error!(file, line, "incomplete escape sequence"),
                        _ => char,
                    });
                }

                tokens.push((Token::Str(content), file, line))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                tokens.push((
                    Token::Name(
                        take_with_predicate!(char, chars, |&c| c.is_alphanumeric() || c == '_')
                    ),
                    file,
                    line,
                ))
            }
            '0'..='9' => {
                let content =
                    take_with_predicate!(char, chars, |&c| c.is_ascii_digit() || c == '.');

                tokens.push((
                    match content.contains('.') {
                        true => Token::Float(content.parse().unwrap()),
                        false => Token::Int(content.parse().unwrap()),
                    },
                    file,
                    line,
                ));
            }
            _ => tokens.push((Token::Symbol(char), file, line)),
        }
    }

    assert_eq!(depth, 0);

    /* We need to parse differents code blocks now */
    let mut stack = Vec::default();
    let mut output = Vec::default();
    let mut temp_tokens = Vec::default();

    for token in tokens.iter() {
        match token {
            (Token::Symbol('('), _, _) /* Code block inside code block, meh */ => {
                stack.push(temp_tokens);
                temp_tokens = Vec::default();
            }
            (Token::Symbol(')'), _, _) /* Something is done, lets figure out what it is :3 */ => {
                let mut nested_tokens = temp_tokens.clone(); /* This operation must be veryyy expensive in time/memory usage */

                match stack.pop() {
                    Some(previous_tokens) /* Finished to parse the code block inside current code block */ => {
                        temp_tokens = previous_tokens;
                        temp_tokens.push((Token::Block(nested_tokens), file.into(), line));
                    }
                    None /* Current code block parsing is done */ => {
                        nested_tokens.reverse();
                        output.push((Token::Block(nested_tokens), file.to_string(), line))
                    },
                }
            }
            (token, file, line) => temp_tokens.push((token.clone(), file.to_string(), *line)),
        }
    }

    output.append(&mut temp_tokens);
    output
}

/* A node in Maeel Stack VM */
struct Guitar {
    value: Cord,
    next: *mut Guitar, /* Might be null ptr */
}

impl Guitar {
    fn new(value: Cord) -> *mut Self {
        Box::into_raw(Box::new(Guitar {
            value: value,
            next: null_mut(),
        }))
    }
}

/* Maeel Stack VM */
struct BocchiVM {
    head: *mut Guitar,
}

impl BocchiVM {
    fn process_tokens(
        &mut self,
        tokens: &mut Stack<TokenData>,
        variables: &mut Mapper<Cord>,
        functions: &mut Mapper<FunData>,
        reverse: bool,
    ) {
        /* Sometimes we might act like the tokens vec was a stack */
        if reverse {
            tokens.reverse();
        }

        let match_bool = |i: bool, l: u16, f: &String| match i {
            false => False!(l, f.clone()),
            true => True!(l, f.clone()),
        };

        while let Some((token, file, line)) = tokens.pop() {
            match token {
                Token::Str(_) | Token::Float(_) | Token::Int(_) => self.push(match token {
                    Token::Str(x) => Cord::Str(x),
                    Token::Int(x) => Cord::Int(x),
                    _ => unreachable!(),
                }),
                Token::Symbol('+') => binop!(|a, b: Cord| b.add(a), self, &file, line),
                Token::Symbol('-') => binop!(|a, b: Cord| b.sub(a), self, &file, line),
                Token::Symbol('*') => binop!(|a, b: Cord| b.mul(a), self, &file, line),
                Token::Symbol('/') => binop!(|a, b: Cord| b.div(a), self, &file, line),
                Token::Symbol('%') => binop!(|a, b: Cord| b.rem(a), self, &file, line),
                Token::Symbol('=') => binop!(|a, b| match_bool(b == a, line, &file), self, &file, line),
                Token::Symbol('<') => binop!(|a, b| match_bool(b < a, line, &file), self, &file, line),
                Token::Symbol('>') => binop!(|a, b| match_bool(b > a, line, &file), self, &file, line),
                Token::Block(mut block) => {
                    block.reverse(); /* meow */
                    self.push(Cord::Fun((block.as_slice().into(), true)))
                }
                Token::Symbol('!') /* Manually call a function */ => {
                    let (function_tokens, is_inline) = expect_stack!(Fun, self, file, line);

                    match is_inline {
                        true => function_tokens.iter().for_each(|t| tokens.push(t.clone())),
                        false => self.process_tokens(
                            &mut function_tokens.to_vec(), &mut variables.clone(), functions, true
                        )
                    }
                }
                Token::Symbol(':') | Token::Symbol('~') /* Assign stack top value to next name */ => {
                    let name = expect_token!(Name, tokens, file, line);
                    let value = self.pop()
                        .unwrap_or_else(|_| emit_error!(file, line, "stack is empty!"));
                    variables.insert(name, value);
                }
                Token::Symbol(char) => emit_error!(file, line, format!("unknown symbol: {char}.")),
                Token::Name(name) => match name.as_str() {
                    "puts" => print!("{}", self.pop().unwrap()),
                    "list" => self.push(Cord::List(Vec::default())),
                    "fun" => {
                        let mut function_name = expect_token!(Name, tokens, file, line);
                        let mut function_tokens = Vec::default();
                        let is_inline = function_name == "inline";

                        if is_inline { function_name = expect_token!(Name, tokens, file, line) }

                        while let Some(temp_token) = tokens.pop() {
                            match temp_token.clone() {
                                (Token::Block(temp_tokens), _, _) => {
                                    function_tokens.reverse(); /* uhm */
                                    function_tokens.extend(temp_tokens);
                                    function_tokens.reverse(); /* never ask if maeel could be faster */
                                    break; /* TODO: remove this break, f*ck breaks */
                                }
                                (Token::Name(_), file, line) => {
                                    function_tokens.push(temp_token);
                                    function_tokens.push((Token::Symbol('~'), file, line));
                                }
                                (other, file, line) => {
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
                    "len" => {
                        let length = match self.pop() {
                            Ok(Cord::Str(string)) => Cord::Int(string.len() as i32),
                            Ok(Cord::List(xs)) => Cord::Int(xs.len() as i32),
                            Ok(other) => emit_error!(file, line, format!("expected string or list, got {other:?}")),
                            Err(_) => emit_error!(file, line, "expected string or list, got EOS.")
                        };

                        self.push(length)
                    }
                    "get" => {
                        let index = expect_stack!(Int, self, file, line) as usize;

                        match self.pop() {
                            Ok(Cord::List(xs)) => self.push(
                                xs.get(index)
                                    .unwrap_or_else(|| emit_error!(file, line, format!("unknown index: {index}")))
                                    .clone()
                            ),
                            Ok(Cord::Str(string)) => self.push(Cord::Str(
                                string
                                    .chars()
                                    .nth(index)
                                    .unwrap_or_else(|| emit_error!(file, line, format!("unknown index: {index}")))
                                    .to_string()
                            )),
                            Ok(other) => emit_error!(file, line, format!("unindexable: {other:?}")),
                            _ => emit_error!(file, line, format!("unindexable: EOF")),
                        }
                    }
                    "include" /* This is bad */ => {
                        let target = expect_token!(Str, tokens, file, line);
                        let content = match target.clone().as_str() {
                            "maeel" => include_str!("maeel.maeel").to_string(),
                            _ => read_to_string(&target).unwrap_or_else(|_| {
                                emit_error!(file, line, "failed to include file")
                            }),
                        };

                        let temp_tokens = lex_into_tokens(&content, &target);
                        let temp_tokens_length = temp_tokens.len();

                        /* Push tokens from end to start (reverse) */
                        for index in 0..temp_tokens_length {
                            tokens.push(temp_tokens.get(temp_tokens_length - index - 1).unwrap().clone())
                        }
                    }
                    name => {
                        if let Some(value) = variables.get(name) {
                            self.push(value.clone())
                        } else if let Some((function_tokens, inline)) = functions.get(name) {
                            match inline {
                                true => function_tokens
                                    .iter()
                                    .for_each(|t| tokens.push(t.clone())),
                                false => self.process_tokens(
                                    &mut function_tokens.to_vec(), &mut variables.clone(), functions, false
                                )
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
            unsafe {
                (*future_head).next = self.head;
            }
        }

        self.head = future_head;
    }

    /* Drop-and-return the stack head */
    fn pop(&mut self) -> Result<Cord, Box<dyn Error>> {
        match self.head.is_null() {
            true => Err("Stack is empty".into()),
            false => {
                let current_head = unsafe { Box::from_raw(self.head) };
                self.head = current_head.next;
                Ok(current_head.value)
            }
        }
    }
}

impl Display for Cord {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(x) => write!(f, "{}", x),
            Self::Fun(_) => write!(f, "Fun"),
            Self::Float(x) => write!(f, "{}", x),
            Self::Int(x) => write!(f, "{}", x),
            Self::List(xs) => {
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
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Some(a.cmp(b)),
            (Self::Float(a), Self::Float(b)) => Some(a.total_cmp(b)),
            (Self::Int(a), Self::Float(b)) | (Self::Float(b), Self::Int(a)) => {
                Some(b.total_cmp(&(*a as f32)))
            }
            (_, _) => unreachable!(),
        }
    }
}

impl PartialEq for Cord {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(a), Self::Str(b)) => a == b,
            (Self::List(a), Self::List(b)) => a == b,
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::Int(a), Self::Float(b)) | (Self::Float(b), Self::Int(a)) => (*a as f32) == *b,
            (Self::Fun(a), Self::Fun(b)) => a == b, /* perf issue */
            _ => false,
        }
    }
}

impl Cord {
    fn sub(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(m), Self::Int(n)) => Self::Int(m - n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x - y),
            (Self::Float(x), Self::Int(m)) | (Self::Int(m), Self::Float(x)) => {
                Self::Float(m as f32 - x)
            }
            (_, _) => unreachable!(),
        }
    }

    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(m), Self::Int(n)) => Self::Int(m * n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x * y),
            (Self::Float(x), Self::Int(m)) | (Self::Int(m), Self::Float(x)) => {
                Self::Float(x * m as f32)
            }
            (Self::Int(m), Self::Str(s)) | (Self::Str(s), Self::Int(m)) => {
                Self::Str(s.repeat(m as usize))
            }
            (_, _) => unreachable!(),
        }
    }

    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Str(a), Self::Str(b)) => Self::Str(a + &b),
            (Self::Int(m), Self::Int(n)) => Self::Int(m + n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x + y),
            (Self::Int(m), Self::Float(x)) | (Self::Float(x), Self::Int(m)) => {
                Self::Float(m as f32 + x)
            }
            (other, Self::List(mut xs)) | (Self::List(mut xs), other) => {
                xs.push(other);
                Self::List(xs)
            }
            (_, _) => unreachable!(),
        }
    }

    fn rem(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(m), Self::Int(n)) => Self::Int(m % n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x % y),
            (Self::Int(m), Self::Float(x)) => Self::Float(m as f32 % x),
            (Self::Float(x), Self::Int(m)) => Self::Float(x % m as f32),
            (_, _) => unreachable!(),
        }
    }

    fn div(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(m), Self::Int(n)) => Self::Float(m as f32 / n as f32),
            (Self::Float(x), Self::Float(y)) => Self::Float(x / y),
            (Self::Int(m), Self::Float(x)) => Self::Float(m as f32 / x),
            (Self::Float(x), Self::Int(m)) => Self::Float(x / m as f32),
            (_, _) => unreachable!(),
        }
    }
}

fn main() {
    let file = args().nth(1).unwrap();
    let file_content = &read_to_string(&file).unwrap();

    BocchiVM { head: null_mut() }.process_tokens(
        &mut lex_into_tokens(file_content, &file),
        &mut HashMap::default(), /* Variables Hashmap */
        &mut HashMap::default(), /* functions Hashmap */
        true,
    )
}
