use std::cmp::Ordering;
use std::collections::HashMap;
use std::env::args;
use std::error::Error;
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
        match $s.pop()
        {
            | Some((Token::$t(value), _, _)) => value,
            | Some((other, file, line)) => panic!(
                "{}:{} got unexpected {:?}",
                file, line, other
            ),
            | None =>
                panic!("{}:{} got unexpected EOF", $f, $l),
        }
    }};
}

macro_rules! expect_stack {
    ($t:tt, $s:expr, $f:expr, $l:expr) => {{
        match $s.pop()
        {
            | Ok(Cord::$t(value)) => value,
            | Ok(other) => panic!(
                "{}:{} got unexpected {:?} on the stack",
                $f, $l, other
            ),
            | Err(_) => panic!("{}:{} got unexpected EOF", $f, $l)
        }
    }};
}

macro_rules! take_with_predicate {
    ($char:expr, $chars:expr, $p:expr) => {{
        let content: String = once($char)
            .chain($chars.clone().take_while($p))
            .collect();
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
    ($b:expr, $line:expr, $fl:expr) => {
        Cord::Fun(
            if $b
            {
                [(name!("drop"), $fl, $line)]
                    .as_slice()
                    .into()
            }
            else
            {
                [
                    (name!("drop"), $fl, $line),
                    (name!("swap"), $fl, $line)
                ]
                .as_slice()
                .into()
            }
        )
    };
}

/* Types that are used by the VM */
#[derive(Debug, Clone)]
enum Cord
{
    /* Primitive types */
    Float(f32),
    Int(i128),
    Str(String),

    /* Composed types */
    List(Vec<Self>),
    Fun(Rc<[TokenData]>)
}

/* Tokens used by the lexer */
#[derive(Clone, Debug, PartialEq)]
enum Token
{
    Symbol(char),
    Name(String),
    Int(i128),
    Float(f32),
    Str(String),
    Block(Vec<TokenData>)
}

/* Turn raw code into an ordered vector of
TokenData (e.g. Token) */
fn lex_into_tokens(
    code: &str, /* Code to parse */
    file: &str  /* Arbitrary, used for error messages */
) -> Vec<TokenData>
{
    let mut depth = 0;
    let mut line = 1;
    let mut tokens = Vec::default();
    let mut chars = code.chars().peekable();

    while let Some(chr) = chars.next()
    {
        match chr
        {
            | '\n' => line += 1,

            | ' ' | '\t' => continue,

            | '#' =>
            {
                take_with_predicate!(chr, chars, |&c| c
                    != '\n');
                line += 1;
            }

            | '(' | ')' =>
            {
                tokens.push((
                    Token::Symbol(chr),
                    file,
                    line
                ));

                depth += /* Code block ((((depth)))) */ match chr
                    {
                        '(' => 1,
                        ')' => -1,
                        _ => unreachable!()
                    }
            }

            | '"' =>
            {
                let content_vector = chars
                    .by_ref()
                    .take_while(|&chr| chr != '"')
                    .collect::<Vec<char>>();

                let mut content = String::with_capacity(
                    content_vector.len()
                );

                let mut index = 0;

                while index < content_vector.len()
                {
                    let chr = content_vector[index];

                    index += 1;

                    content.push(match (chr, content_vector.get(index))
                    {
                        | ('\\', Some(next_char)) =>
                        {
                            index += 1;

                            match next_char
                            {
                                | 'n' => '\n',
                                | 't' => '\t',
                                | '\\' => '\\',
                                | '"' => '"',
                                | _ => panic!("{}:{} invalid escape sequence", file, line)
                            }
                        }

                        | ('\\', None) => panic!("{}:{} incomplete escape sequence", file, line),

                        | _ => chr
                    });
                }

                tokens.push((
                    Token::Str(content),
                    file,
                    line
                ))
            }

            | 'a'..='z' | 'A'..='Z' | '_' =>
            {
                let content = take_with_predicate!(
                    chr,
                    chars,
                    |&c| c.is_alphanumeric() || c == '_'
                );

                tokens.push((
                    Token::Name(content),
                    file,
                    line
                ))
            }

            | '0'..='9' =>
            {
                let content = /* integer/float as a string */
                    take_with_predicate!(chr, chars, |&c| c.is_ascii_digit() || c == '.');

                let tokenized = /* string -> maeel Token (types are infered) */
                    if content.contains('.') {
                        Token::Float(content.parse().unwrap())
                    } else {
                       Token::Int(content.parse().unwrap())
                    };

                tokens.push((tokenized, file, line))
            }

            | _ => tokens.push((
                Token::Symbol(chr),
                file,
                line
            ))
        }
    }

    assert_eq!(depth, 0);

    /* We need to parse differents code blocks now */
    let mut stack = Vec::with_capacity(tokens.len());

    let mut output = Vec::default();
    let mut tmp_tokens = Vec::default();

    for token in tokens.iter()
    {
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
struct Guitar
{
    value: Cord,
    next: *mut Guitar /* Might be null ptr */
}

impl Guitar
{
    fn new(value: Cord) -> *mut Self
    {
        let guitar = Guitar {
            value: value,
            next: null_mut()
        };

        let guitar_heap = Box::new(guitar);

        Box::into_raw(guitar_heap) /* Guitar pointer */
    }
}

/* Maeel Stack VM (basically a LinkedList) */
struct BocchiVM
{
    head: *mut Guitar
}

impl BocchiVM
{
    fn process_tokens(
        &mut self,
        tokens: &mut Vec<TokenData>,
        vars: &mut HashMap<String, Cord>,
        funs: &mut HashMap<String, (Rc<[TokenData]>, bool)>
    )
    {
        while let Some((token, file, line)) = tokens.pop()
        {
            match token {
                Token::Str(_) | Token::Float(_) | Token::Int(_) => self.push(match token {
                    | Token::Str(x) => Cord::Str(x),
                    | Token::Int(x) => Cord::Int(x),
                    | Token::Float(x) => Cord::Float(x),
                    | _ => unreachable!()
                }),

                Token::Symbol('+') => self.add(),

                Token::Symbol('-') => self.sub(),

                Token::Symbol('*') => self.mul(),

                Token::Symbol('/') => self.div(),

                Token::Symbol('=') => {
                    let (rhs, lhs) = (self.pop().unwrap(), self.pop().unwrap());

                    self.push(match_bool!(lhs == rhs, line, file.clone()))
                }

                Token::Symbol('<') => {
                    let (rhs, lhs) = (self.pop().unwrap(), self.pop().unwrap());

                    self.push(match_bool!(lhs < rhs, line, file.clone()))
                }

                Token::Symbol('>') => {
                    let (rhs, lhs) = (self.pop().unwrap(), self.pop().unwrap());

                    self.push(match_bool!(lhs > rhs, line, file.clone()))
                }

                Token::Block(mut block) => {
                    block.reverse(); /* meow */

                    self.push(Cord::Fun(block.as_slice().into()))
                }

                Token::Symbol('!') => /* Push tokens from a block on the tokens stack */
                    expect_stack!(Fun, self, file, line)
                        .iter()
                        .for_each(|t| tokens.push(t.clone())),

                Token::Symbol(':') | Token::Symbol('~') /* Assign stack top value to next name */ => {
                    let name = expect_token!(Name, tokens, file, line);

                    let value = self.pop()
                        .unwrap_or_else(|_| panic!("{}:{} stack is empty", file, line));

                    vars.insert(name, value);
                }

                Token::Symbol(chr) => panic!("{}:{} unknown symbol: {}", file, line, chr),

                Token::Name(name) => match name.as_str() {
                    "puts" => print!("{}", self.pop().unwrap()),

                    "list" => self.push(Cord::List(Vec::default())),

                    "fun" => {
                        let (fun_name, is_inline) =
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
                                    fun_tokens.reverse(); /* uhm */
                                    fun_tokens.extend(tmp_tokens);
                                    fun_tokens.reverse(); /* never ask if maeel could be faster */
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
                            (fun_tokens.as_slice().into(), is_inline)
                        );
                    }

                    "len" => match self.pop() {
                        Ok(Cord::Str(string)) => self.push(Cord::Int(string.len() as i128)),
                        Ok(Cord::List(xs)) => self.push(Cord::Int(xs.len() as i128)),
                        Ok(other) => panic!(
                            "{}:{} expected string or list, got {:?}",
                            file,
                            line,
                            other
                        ),
                        Err(_) => panic!(
                            "{}:{} expected string or list, got EOS",
                            file,
                            line
                        )
                    },

                    "get" => {
                        let index = expect_stack!(Int, self, file, line) as usize;

                        match self.pop()
                        {
                            Ok(Cord::List(xs)) => self.push(
                                xs.get(index)
                                    .unwrap_or_else(|| panic!("{}:{} unknown index: {}", file, line, index))
                                    .clone()
                            ),

                            Ok(Cord::Str(string)) => self.push(Cord::Str(
                                string
                                    .chars()
                                    .nth(index)
                                    .unwrap_or_else(|| panic!("{}:{} unknown index: {}", file, line, index))
                                    .to_string()
                            )),
                            Ok(other) => panic!("{}:{} unindexable: {:?}", file, line, other),
                            _ => panic!("{}:{} unindexable: EOF", file, line)
                        }
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
                        if let Some(value) = vars.get(name)
                        {
                            self.push(value.clone())
                        }
                        else if let Some((fun_tokens, inline)) = funs.get(name)
                        {
                            if *inline
                            {
                                fun_tokens
                                    .iter()
                                    .for_each(|t| tokens.push(t.clone()))
                            }
                            else
                            {
                                self.process_tokens(
                                    &mut fun_tokens.to_vec(),
                                    &mut vars.clone(),
                                    funs
                                )
                            }
                        }
                        else
                        {
                            panic!("{}:{} unknown name {}", file, line, name)
                        }
                    }
                }
            };
        }
    }

    /* Push an object to the stack */
    fn push(&mut self, value: Cord)
    {
        let future_head = Guitar::new(value);

        if !self.head.is_null()
        {
            unsafe /* still safe tho */ {
                (*future_head).next = self.head;
            }
        }

        self.head = future_head;
    }

    /* Drop-and-return the stack head */
    fn pop(&mut self) -> Result<Cord, Box<dyn Error>>
    {
        if self.head.is_null()
        {
            Err("Stack is empty".into())
        }
        else
        {
            let current_head = /* Safe because of the previous check */
                unsafe { Box::from_raw(self.head) };

            self.head = current_head.next; /* Update the head */

            Ok(current_head.value)
        }
    }

    fn sub(&mut self)
    {
        let (rhs, lhs) = (self.pop(), self.pop());

        self.push(match (lhs.unwrap(), rhs.unwrap())
        {
            | (Cord::Int(m), Cord::Int(n)) =>
                Cord::Int(m - n),
            | (Cord::Float(x), Cord::Float(y)) =>
                Cord::Float(x - y),
            | (Cord::Float(x), Cord::Int(m))
            | (Cord::Int(m), Cord::Float(x)) =>
                Cord::Float(m as f32 - x),
            | (_, _) => unreachable!()
        })
    }

    fn mul(&mut self)
    {
        let (rhs, lhs) = (self.pop(), self.pop());

        self.push(match (lhs.unwrap(), rhs.unwrap())
        {
            | (Cord::Int(m), Cord::Int(n)) =>
                Cord::Int(m * n),
            | (Cord::Float(x), Cord::Float(y)) =>
                Cord::Float(x * y),
            | (Cord::Float(x), Cord::Int(m))
            | (Cord::Int(m), Cord::Float(x)) =>
                Cord::Float(x * m as f32),
            | (Cord::Int(m), Cord::Str(s))
            | (Cord::Str(s), Cord::Int(m)) =>
                Cord::Str(s.repeat(m as usize)),
            | (_, _) => unreachable!()
        })
    }

    fn add(&mut self)
    {
        let (rhs, lhs) = (self.pop(), self.pop());

        self.push(match (lhs.unwrap(), rhs.unwrap())
        {
            | (Cord::Str(a), Cord::Str(b)) =>
                Cord::Str(a + &b),
            | (Cord::Int(m), Cord::Int(n)) =>
                Cord::Int(m + n),
            | (Cord::Float(x), Cord::Float(y)) =>
                Cord::Float(x + y),
            | (Cord::Int(m), Cord::Float(x))
            | (Cord::Float(x), Cord::Int(m)) =>
                Cord::Float(m as f32 + x),
            | (other, Cord::List(mut xs))
            | (Cord::List(mut xs), other) =>
            {
                xs.push(other);

                Cord::List(xs)
            }
            | (_, _) => unreachable!()
        })
    }

    fn div(&mut self)
    {
        let (rhs, lhs) = (self.pop(), self.pop());

        self.push(match (lhs.unwrap(), rhs.unwrap())
        {
            | (Cord::Int(a), Cord::Int(b)) =>
                Cord::Float(a as f32 / b as f32),
            | (Cord::Float(a), Cord::Float(b)) =>
                Cord::Float(a / b),
            | (Cord::Int(a), Cord::Float(b)) =>
                Cord::Float(a as f32 / b),
            | (Cord::Float(a), Cord::Int(b)) =>
                Cord::Float(a / b as f32),
            | (_, _) => unreachable!()
        })
    }
}

impl Display for Cord
{
    fn fmt(&self, f: &mut Formatter<'_>)
        -> std::fmt::Result
    {
        match self
        {
            | Self::Str(x) => write!(f, "{}", x),
            | Self::Fun(_) => write!(f, "Fun"),
            | Self::Float(x) => write!(f, "{}", x),
            | Self::Int(x) => write!(f, "{}", x),
            | Self::List(xs) =>
            {
                write!(f, "{{")?;
                xs.iter().enumerate().for_each(|(i, x)| {
                    if i > 0
                    {
                        write!(f, " ").unwrap()
                    }
                    write!(f, "{}", x).unwrap()
                });
                write!(f, "}}")
            }
        }
    }
}

impl PartialOrd for Cord
{
    fn partial_cmp(&self, other: &Self)
        -> Option<Ordering>
    {
        match (self, other)
        {
            | (Self::Int(a), Self::Int(b)) =>
                Some(a.cmp(b)),
            | (Self::Float(a), Self::Float(b)) =>
                Some(a.total_cmp(b)),
            | (Self::Int(a), Self::Float(b))
            | (Self::Float(b), Self::Int(a)) =>
                Some(b.total_cmp(&(*a as f32))),
            | (_, _) => unreachable!()
        }
    }
}

impl PartialEq for Cord
{
    fn eq(&self, other: &Self) -> bool
    {
        match (self, other)
        {
            | (Self::Str(a), Self::Str(b)) => a == b,
            | (Self::List(a), Self::List(b)) => a == b,
            | (Self::Int(a), Self::Int(b)) => a == b,
            | (Self::Float(a), Self::Float(b)) => a == b,
            | (Self::Int(a), Self::Float(b))
            | (Self::Float(b), Self::Int(a)) =>
                (*a as f32) == *b,
            | (Self::Fun(a), Self::Fun(b)) => a == b, /* perf issue */
            | _ => false
        }
    }
}

fn main()
{
    let custom_panic_handler = /* Rust default is bloated */
        |panic_info: &PanicInfo| {
            println!("{}", panic_info)
        };

    panic::set_hook(Box::new(custom_panic_handler));

    let file = args()
        .nth(1)
        .expect("Please provide a file to execute");

    let file_content =
        read_to_string(&file).expect("Invalid file");

    let mut tokens = lex_into_tokens(&file_content, &file);

    tokens.reverse();

    BocchiVM { head: null_mut() }.process_tokens(
        &mut tokens,
        &mut HashMap::default(), /* Variables Hashmap */
        &mut HashMap::default()  /* Functions Hashmap */
    )
}
