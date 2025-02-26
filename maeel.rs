use std::cmp::Ordering;
use std::collections::HashMap;
use std::env::args;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fs::read_to_string;
use std::iter::once;
use std::panic;
use std::panic::PanicInfo;
use std::ptr::null_mut;
use std::rc::Rc;

type TokenData = (Token, String, u16);

macro_rules! expect_token {
    ($t:tt, $s:expr, $f:expr, $l:expr) => {{
        match $s.pop() {
            Some((Token::$t(value), _, _)) => value,
            Some((other, file, line)) => panic!("{}:{} got unexpected {:?}", file, line, other),
            None => {
                panic!("{}:{} got unexpected EOF", $f, $l)
            }
        }
    }};
}

macro_rules! expect_stack {
    ($t:tt, $s:expr, $f:expr, $l:expr) => {{
        match $s.pop() {
            Some(Cord::$t(value)) => value,
            Some(other) => panic!("{}:{} got unexpected {:?} on the stack", $f, $l, other),
            None => panic!("{}:{} got unexpected EOF", $f, $l),
        }
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

macro_rules! name {
    ($n:expr) => {
        Token::Name($n.into())
    };
}

macro_rules! match_bool {
    ($b:expr, $line:expr, $fl:expr) => {{
        let fake_boolean = if $b {
            [(name!("drop"), $fl, $line)].as_slice().into()
        } else {
            [(name!("drop"), $fl, $line), (name!("swap"), $fl, $line)]
                .as_slice()
                .into()
        };

        Cord::Fun(fake_boolean)
    }};
}

/* Types that are used by the VM */
#[derive(Debug, Clone)]
enum Cord {
    /* Primitive types */
    Float(f32),
    Int(i128),
    Str(String),

    /* Composed types */
    List(Vec<Self> /* Lists are mutable */),
    Fun(Rc<[TokenData]> /* Funs tokens are unmutable */),
}

/* Tokens used by the lexer */
#[derive(Clone, Debug, PartialEq)]
enum Token {
    Symbol(char),
    Name(String),
    Int(i128),
    Float(f32),
    Str(String),
    Block(Vec<TokenData>),
}

/* Turn raw code into an ordered vector of
TokenData (e.g. Token) */
fn lex_into_tokens(
    code: &str, /* Code to parse */
    file: &str, /* Arbitrary, used for error messages */
) -> Vec<TokenData> {
    let mut depth = 0;
    let mut line = 1;
    let mut tokens = Vec::default();
    let mut chars = code.chars();

    while let Some(chr) = chars.next() {
        match chr {
            '\n' => line += 1,
            ' ' | '\t' => continue,
            '#' => {
                take_with_predicate!(chr, chars, |&c| c != '\n');
                line += 1;
            }
            '(' | ')' => {
                tokens.push((Token::Symbol(chr), file, line));

                depth += match chr {
                    '(' => 1,
                    ')' => -1,
                    _ => unreachable!(),
                }
            }
            '"' => {
                let content_vector = chars
                    .by_ref()
                    .take_while(|&chr| chr != '"')
                    .collect::<Vec<char>>();

                let mut content = String::with_capacity(content_vector.len());
                let mut index = 0;

                while index < content_vector.len() {
                    let chr = content_vector[index];

                    index += 1;

                    let escaped = match (chr, content_vector.get(index)) {
                        ('\\', Some(next_char)) => {
                            index += 1;

                            match next_char {
                                'n' => '\n',
                                't' => '\t',
                                '\\' => '\\',
                                '"' => '"',
                                _ => panic!("{}:{} invalid escape sequence", file, line),
                            }
                        }
                        ('\\', None) => panic!("{}:{} incomplete escape sequence", file, line),
                        _ => chr,
                    };

                    content.push(escaped)
                }

                tokens.push((Token::Str(content), file, line))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let content =
                    take_with_predicate!(chr, chars, |&c| c.is_alphanumeric() || c == '_');

                tokens.push((Token::Name(content), file, line))
            }
            '0'..='9' => {
                let content = /* integer/float as a string */
                take_with_predicate!(chr, chars, |&c| c.is_ascii_digit() || c == '.');

                let tokenized = if content.contains('.') {
                    Token::Float(content.parse().unwrap())
                } else {
                    Token::Int(content.parse().unwrap())
                };

                tokens.push((tokenized, file, line))
            }
            _ => tokens.push((Token::Symbol(chr), file, line)),
        }
    }

    assert_eq!(depth, 0);

    /* We need to parse differents code blocks now */
    let mut stack = Vec::with_capacity(tokens.len());

    let mut output = Vec::default();
    let mut tmp_tokens = Vec::default();

    for token in tokens.iter() {
        match token {
            (Token::Symbol('('), _, _) /* Start of block or sub-block */ => {
                stack.push(tmp_tokens); /* Backing up temp tokens */
                tmp_tokens = Vec::default() /* Clearing temp tokens */
            }

            (Token::Symbol(')'), _, _) /* End of block or sub-block */ => {
                let mut nested_tokens = tmp_tokens.clone();

                match stack.pop() {
                    Some(previous_tokens) /* Finished to parse the sub-block */ => {
                        tmp_tokens = previous_tokens;

                        tmp_tokens.push((
                            Token::Block(nested_tokens),
                            file.into(),
                            line
                        ))
                    }

                    None /* Current code block parsing is done */ => {
                        nested_tokens.reverse();

                        output.push((
                            Token::Block(nested_tokens),
                            file.to_string(),
                            line
                        ))
                    }
                }
            }

            (token, file, line) => tmp_tokens.push((
                token.clone(),
                file.to_string(),
                *line
            ))
        }
    }

    output.append(&mut tmp_tokens);

    output
}

/* A node in Maeel Stack VM */
struct Guitar {
    value: Cord,
    next: *mut Guitar, /* null by default */
}

impl Guitar {
    fn new(value: Cord) -> *mut Self {
        let guitar = Guitar {
            value: value,
            next: null_mut(),
        };

        let guitar_heap = Box::new(guitar);

        Box::into_raw(guitar_heap) /* Guitar pointer */
    }
}

/* Maeel Stack VM (basically a LinkedList) */
struct BocchiVM {
    head: *mut Guitar,
}

impl BocchiVM {
    fn process_tokens(
        &mut self,
        tokens: &mut Vec<TokenData>,
        vars: &mut HashMap<String, Cord>,
        funs: &mut HashMap<String, (Rc<[TokenData]>, bool)>,
    ) {
        while let Some((token, file, line)) = tokens.pop() {
            match token {
                Token::Str(_) | Token::Float(_) | Token::Int(_) => {
                    let cord = match token {
                        Token::Str(x) => Cord::Str(x),
                        Token::Int(x) => Cord::Int(x),
                        Token::Float(x) => Cord::Float(x),
                        _ => unreachable!(),
                    };

                    self.push(cord)
                }
                Token::Symbol('+') => self.add(),
                Token::Symbol('-') => self.sub(),
                Token::Symbol('*') => self.mul(),
                Token::Symbol('/') => self.div(),
                Token::Symbol('=') => {
                    let (rhs, lhs) = (self.pop().unwrap(), self.pop().unwrap());

                    let cord = match_bool!(lhs == rhs, line, file.clone());

                    self.push(cord)
                }
                Token::Symbol('<') => {
                    let (rhs, lhs) = (self.pop().unwrap(), self.pop().unwrap());

                    let cord = match_bool!(lhs < rhs, line, file.clone());

                    self.push(cord)
                }
                Token::Symbol('>') => {
                    let (rhs, lhs) = (self.pop().unwrap(), self.pop().unwrap());

                    let cord = match_bool!(lhs > rhs, line, file.clone());

                    self.push(cord)
                }
                Token::Block(mut block) => {
                    block.reverse();

                    let fake_fun = Cord::Fun(block.as_slice().into());

                    self.push(fake_fun)
                }
                Token::Symbol('!') => {
                    expect_stack!(Fun, self, file, line)
                        .iter()
                        .for_each(|t| tokens.push(t.clone()));
                }
                Token::Symbol(':') => {
                    let name = expect_token!(Name, tokens, file, line);

                    let value = self
                        .pop()
                        .unwrap_or_else(|| panic!("{}:{} stack is empty", file, line));

                    vars.insert(name, value);
                }
                Token::Symbol(chr) => panic!("{}:{} unknown symbol: {}", file, line, chr),
                Token::Name(name) => {
                    match name.as_str() {
                        "puts" => {
                            print!("{}", self.pop().unwrap())
                        }

                        "list" => {
                            let cord = Cord::List(Vec::default());
                            self.push(cord)
                        }

                        "fun" => {
                            let (fun_name, inline) =
                            match expect_token!(Name, tokens, file, line).as_str()
                            {
                                "inline" => {
                                    (expect_token!(Name, tokens, file, line), true)
                                }

                                other => (other.to_string(), false)
                            };

                            let mut fun_tokens = Vec::default();

                            while let Some(tmp_token) = tokens.pop()
                            {
                                match tmp_token.clone()
                                {
                                    (Token::Block(tmp_tokens), _, _) => {
                                        fun_tokens.reverse();
                                        fun_tokens.extend(tmp_tokens);
                                        fun_tokens.reverse();
                                        break
                                    }
                                    (Token::Name(_), file, line) => {
                                        fun_tokens.push(tmp_token);
                                        fun_tokens.push((Token::Symbol(':'), file, line))
                                    }
                                    (_, file, line) => panic!(
                                        "{}:{} expected param(s) or a code block after 'fun {}'",
                                        file,
                                        line,
                                        fun_name
                                    )
                                }
                            }

                            funs.insert(
                                fun_name,
                                (fun_tokens.as_slice().into(), inline)
                            );
                        }

                        "len" => {
                            let cord = match self.pop() {
                                | Some(Cord::Str(string)) => Cord::Int(string.len() as i128),
                                | Some(Cord::List(xs)) => Cord::Int(xs.len() as i128),
                                | Some(other) => panic!(
                                    "{}:{} expected string or list, got {:?}",
                                    file,
                                    line,
                                    other
                                ),
                                | None => panic!(
                                    "{}:{} expected string or list, got EOS",
                                    file,
                                    line
                                )
                            };

                            self.push(cord)
                        }

                        "get" => {
                            let index = expect_stack!(Int, self, file, line) as usize;

                            let cord = match self.pop()
                            {
                                Some(Cord::List(xs)) => xs.get(index)
                                    .unwrap_or_else(
                                        || panic!("{}:{} unknown index: {}", file, line, index)
                                    )
                                    .clone(),

                                Some(Cord::Str(string)) => Cord::Str(
                                    string
                                        .chars()
                                        .nth(index)
                                        .unwrap_or_else(
                                            || panic!("{}:{} unknown index: {}", file, line, index)
                                        )
                                        .to_string()
                                ),

                                Some(other) => panic!("{}:{} unindexable: {:?}", file, line, other),

                                None => panic!("{}:{} unindexable: EOF", file, line)
                            };

                            self.push(cord)
                        }

                        "include" /* This is bad */ => {
                            let target = expect_token!(Str, tokens, file, line);

                            let content = /* Code to include */
                            match target.clone().as_str()
                            {
                                "maeel" => include_str!("maeel.maeel").to_string(),
                                _ => read_to_string(&target)
                                    .unwrap_or_else(
                                        |_| panic!("{}:{} failed to include file", file, line)
                                    )
                            };

                            let tmp_tokens = lex_into_tokens(&content, &target);
                            let tmp_tokens_len = tmp_tokens.len();

                            for index in 0..tmp_tokens_len
                            {
                                let tmp_token = tmp_tokens
                                    .get(tmp_tokens_len - index - 1)
                                    .unwrap();

                                tokens.push(tmp_token.clone())
                            }
                        }

                        name => {
                            if let Some(value) = vars.get(name) {
                                self.push(value.clone())
                            }
                            else if let Some((fun_tokens, inline)) = funs.get(name)
                            {
                                if *inline {
                                    fun_tokens
                                        .iter()
                                        .for_each(|t| tokens.push(t.clone()))
                                } else {
                                    self.process_tokens(
                                        &mut fun_tokens.to_vec(),
                                        &mut vars.clone(),
                                        funs
                                    )
                                }
                            } else {
                                panic!("{}:{} unknown name {}", file, line, name)
                            }
                        }
                    }
                }
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
    fn pop(&mut self) -> Option<Cord> {
        if self.head.is_null() {
            None
        } else {
            let current_head = unsafe { Box::from_raw(self.head) };

            self.head = current_head.next; /* Update the head */

            Some(current_head.value)
        }
    }

    fn sub(&mut self) {
        let (rhs, lhs) = (self.pop(), self.pop());

        let output = match (lhs.unwrap(), rhs.unwrap()) {
            (Cord::Int(m), Cord::Int(n)) => Cord::Int(m - n),
            (Cord::Float(x), Cord::Float(y)) => Cord::Float(x - y),
            (Cord::Float(x), Cord::Int(m)) | (Cord::Int(m), Cord::Float(x)) => {
                Cord::Float(m as f32 - x)
            }
            (_, _) => unreachable!(),
        };

        self.push(output)
    }

    fn mul(&mut self) {
        let (rhs, lhs) = (self.pop(), self.pop());

        let output = match (lhs.unwrap(), rhs.unwrap()) {
            (Cord::Int(m), Cord::Int(n)) => Cord::Int(m * n),
            (Cord::Float(x), Cord::Float(y)) => Cord::Float(x * y),
            (Cord::Float(x), Cord::Int(m)) | (Cord::Int(m), Cord::Float(x)) => {
                Cord::Float(x * m as f32)
            }
            (Cord::Int(m), Cord::Str(s)) | (Cord::Str(s), Cord::Int(m)) => {
                Cord::Str(s.repeat(m as usize))
            }
            (_, _) => unreachable!(),
        };

        self.push(output)
    }

    fn add(&mut self) {
        let (rhs, lhs) = (self.pop(), self.pop());

        let output = match (lhs.unwrap(), rhs.unwrap()) {
            (Cord::Str(a), Cord::Str(b)) => Cord::Str(a + &b),
            (Cord::Int(m), Cord::Int(n)) => Cord::Int(m + n),
            (Cord::Float(x), Cord::Float(y)) => Cord::Float(x + y),
            (Cord::Int(m), Cord::Float(x)) | (Cord::Float(x), Cord::Int(m)) => {
                Cord::Float(m as f32 + x)
            }
            (other, Cord::List(mut xs)) | (Cord::List(mut xs), other) => {
                xs.push(other);

                Cord::List(xs)
            }
            (_, _) => unreachable!(),
        };

        self.push(output)
    }

    fn div(&mut self) {
        let (rhs, lhs) = (self.pop(), self.pop());

        let output = match (lhs.unwrap(), rhs.unwrap()) {
            (Cord::Int(a), Cord::Int(b)) => Cord::Float(a as f32 / b as f32),
            (Cord::Float(a), Cord::Float(b)) => Cord::Float(a / b),
            (Cord::Int(a), Cord::Float(b)) => Cord::Float(a as f32 / b),
            (Cord::Float(a), Cord::Int(b)) => Cord::Float(a / b as f32),
            (_, _) => unreachable!(),
        };

        self.push(output)
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

fn main() {
    let custom_panic_handler = /* Rust default is bloated */
    |panic_info: &PanicInfo| {
        println!("{}", panic_info)
    };

    panic::set_hook(Box::new(custom_panic_handler));

    let file = args().nth(1).expect("Please provide a file to execute");
    let file_content = read_to_string(&file).expect("Invalid file");

    let mut tokens = lex_into_tokens(&file_content, &file);

    tokens.reverse();

    BocchiVM { head: null_mut() }.process_tokens(
        &mut tokens,
        &mut HashMap::default(), /* Variables Hashmap */
        &mut HashMap::default(), /* Functions Hashmap */
    )
}
