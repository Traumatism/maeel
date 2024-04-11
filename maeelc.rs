/*
This is STILL in development phase !! (partially works)

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

#[derive(Debug)]
enum BinOp { Add, Sub, Mul, Div, Mod, Eq, Lt, Gt }

#[derive(Debug)]
enum BuiltIn { Puts, Get, Len }

#[derive(Debug)]
enum Instruction {
    BinOp(BinOp),
    Push(Cord),
    CallStack,
    CallIf(Vec<Instruction>),
    VarPush(String),
    BuiltInCall(BuiltIn),
    FunPush((String, Vec<Instruction>, bool)),
    FunCall(String),
    VarCall(String),
}

fn process_tokens(
    tokens: &mut Vec<TokenData>,
    rev: bool,
) -> Vec<Instruction> {
    if rev /* Sometimes we might act like the tokens vec was a stack */ { tokens.reverse(); }

    let mut instructions = Vec::new();

    while let Some((token, file, line)) = tokens.pop() {
        match token {
            | Token::Sym(M_ADD!()) => instructions.push(Instruction::BinOp(BinOp::Add)),
            | Token::Sym(M_SUB!()) => instructions.push(Instruction::BinOp(BinOp::Sub)),
            | Token::Sym(M_MUL!()) => instructions.push(Instruction::BinOp(BinOp::Mul)),
            | Token::Sym(M_DIV!()) => instructions.push(Instruction::BinOp(BinOp::Div)),
            | Token::Sym(M_MOD!()) => instructions.push(Instruction::BinOp(BinOp::Mod)),
            | Token::Sym(M_EQ!()) => instructions.push(Instruction::BinOp(BinOp::Eq)),
            | Token::Sym(M_LT!()) => instructions.push(Instruction::BinOp(BinOp::Lt)),
            | Token::Sym(M_GT!()) => instructions.push(Instruction::BinOp(BinOp::Gt)),
            | Token::Str(_) | Token::Flt(_) | Token::Int(_) => instructions.push(Instruction::Push(token.into())),
            | Token::Block(mut block) => {
                block.reverse(); /* meow */
                instructions.push(Instruction::Push(Cord::Fun((block.as_slice().into(), true))))
            }
            | Token::Sym(M_FUN_PUSH!()) => {
                /* let fun_name /* Fun name */   = expect_token!(Name, tokens, file, line);
                 let fun      /* Fun object */ = funs.get(&fun_name).unwrap_or_else(|| {
                    emit_error!(file, line, format!("undefined function: {fun_name:?}"))
                });

                instructions.push(Instruction::Push(Cord::Fun(fun.clone())))
                */
            }
            | Token::Sym(M_EXEC!()) /* Manually call a function */ => {
                instructions.push(Instruction::CallStack)
            }
            | Token::Sym(M_THEN!()) /* Basically "if" statement */ => {
                let mut tmp_tokens = expect_token!(Block, tokens, file, line);
                instructions.push(Instruction::CallIf(process_tokens(&mut tmp_tokens, false)))
            }
            | Token::Sym('§') /* Can be pushed by interpreter only */ => {
                instructions.push(Instruction::VarPush(expect_token!(Name, tokens, file, line)))
            }
            | Token::Sym(M_DEF!()) /* Assign stack top value to next name */ => {
                let name = expect_token!(Name, tokens, file, line);
                if name.starts_with("__") /* Private field */ { panic!(/* TODO: make the error message */) }
                instructions.push(Instruction::VarPush(name))
            }
            | Token::Sym(char) => emit_error!(file, line, format!("unknown symbol: {char}.")),
            | Token::Name(name) => match name.as_str() {
                | M_PUTS!() => instructions.push(Instruction::BuiltInCall(BuiltIn::Puts)), 
                | M_ELIST!() => instructions.push(Instruction::Push(Cord::Lst(Vec::new()))),
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
                    instructions.push(Instruction::FunPush((fun_name.clone(), process_tokens((&mut fun_tokens).into(), false), is_inline)));
                }
                | M_LEN!() => instructions.push(Instruction::BuiltInCall(BuiltIn::Len)),
                | M_GET!() => instructions.push(Instruction::BuiltInCall(BuiltIn::Get)),
                | name => {
                    if (&name).starts_with("f") {
                        instructions.push(Instruction::FunCall(name.to_string()))
                    } else {
                        instructions.push(Instruction::VarCall(name.to_string()))
                    }
                }
            },
        };
    }

    instructions
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

    let is = process_tokens(
        &mut lex_into_tokens(&std::fs::read_to_string(&file).unwrap(), &file),
        true,
    );

    for i in is {
        println!("{:?}", i);
    }
}
