use std::cmp::Ordering;
use std::collections::HashMap;
use std::env::args;
use std::error::Error;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fs::read_to_string;
use std::fs::File;
use std::io::Read;
use std::iter::once;
use std::ptr::null_mut;
use std::rc::Rc;

type M_INT_SIZE   /* Encode maeel integers on 32 bits        */ = i32;
type M_FLOAT_SIZE /* Encode maeel floats on 32 bits          */ = f32;
type Stack<T>     /* Specify that a vec is used like a stack */ = Vec<T>;
type TokenData    /* Token and its file name, line           */ = (Token, String, u16);
type FunData      /* Fun tokens and inline descriptor        */ = (Rc<[TokenData]>, bool);
type FunDataB                                                   = (Rc<[TokenData]>, bool, Vec<String>, Vec<String>);
type Mapper<T>    /* Map values of type T with their names   */ = HashMap<String, T>;
type Tokens                                                     = Vec<TokenData>;

/* Assign names to literals */
#[macro_export]
macro_rules! alias {
    ($($name:ident, $value:expr),* $(,)?) => { $(#[macro_export] macro_rules! $name {() => { $value }} )* }
}

macro_rules! default_fun {
    () => {
        (Vec::new().as_slice().into(), true, Vec::new(), Vec::new())
    };
}

macro_rules! expect_token {
    ($t:tt, $ts:expr, $f:expr, $l:expr) => {{
        match $ts.pop()
        {
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
        match $s.pop()
        {
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

macro_rules! expect_stack_b {
    ($t:tt, $s:expr, $f:expr, $l:expr) => {{
        match $s.stack.pop()
        {
            Some(CordB::$t) => CordB::$t,
            Some(o) => emit_error!(
                $f,
                $l,
                format!("expected {:?} on the stack, got {o:?}", CordBRepr::$t)
            ),
            None => emit_error!($f, $l, format!("expected {:?}, got EOF", CordBRepr::$t)),
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

macro_rules! binop_b {
    ($a:expr, $s:expr, $f:expr, $l:expr) => {{
        let output = $a(
            $s.stack
                .pop()
                .unwrap_or_else(|| emit_error!($f, $l, "stack is empty! (binary operation LHS)")),
            $s.stack
                .pop()
                .unwrap_or_else(|| emit_error!($f, $l, "stack is empty! (binary operation RHS)")),
        );

        $s.stack.push(output)
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

alias!(
    M_STD_LIB, "maeel.maeel", /* Relative to repository root */
    M_FUN, "fun",
    M_INLINE, "inline",
    M_INCLUDE, "include",
    M_ELIST, "list",
    M_PUTS, "puts",
    M_READ, "read",
    M_LEN, "len",
    M_GET, "get",
    M_FUN_PUSH, '&',
    M_STR, '"',
    M_DEF, '~',
    M_FORCE_DEF, '§',
    M_THEN, '?',
    M_EXEC, '!',
    M_ADD, '+',
    M_MUL, '*',
    M_SUB, '-',
    M_DIV, '/',
    M_MOD, '%',
    M_EQ, '=',
    M_GT, '>',
    M_LT, '<',
    M_BLOCK_START, '(',
    M_BLOCK_END, ')',
    M_COMMENT_START, '#',
    M_TYPE_AN_START, '[',
    M_TYPE_AN_END, ']'
);

/* Types that are used by the VM */
#[derive(Debug, Clone)]
enum Cord {
    Float(M_FLOAT_SIZE),
    Int(M_INT_SIZE),
    Fun(FunData),
    Str(String),
    List(Vec<Self>)
}

/* Used for error messages */
#[derive(Debug)]
enum CordRepr {
    Float,
    Int,
    Fun,
    Str,
    List
}

/* Types that are used by the type checker */
#[derive(Debug, Clone)]
enum CordB {
    Float,
    Int,
    Str,
    List,
    Any,
    Fun(FunDataB)
}

#[derive(Debug, Clone)]
enum CordBRepr {
    Float,
    Int,
    Str,
    List,
    Any,
    Fun
}

#[derive(Debug)]
enum TCType {
    Input,
    Output
}

/* Tokens used by the lexer */
#[derive(Clone, Debug)]
pub enum Token {
    Block(Tokens),
    Str(String),
    Name(String),
    Int(M_INT_SIZE),
    Float(M_FLOAT_SIZE),
    Sym(char),
    Comment(String),
    Annotation(String)
}

/* Used for error messages */
#[derive(Debug)]
pub enum TokenRepr {
    Block,
    Str,
    Name,
    Int,
    Float,
    Sym,
    Comment,
    Annotation
}

enum BinOp {
    Add,
    Sub,
    Mul
}

enum BuiltIn {
    Puts,
    PutsI,
    Syscall(M_INT_SIZE)
}

enum PushT {
    Int(M_INT_SIZE),
    Str(String)
}

enum Instruction {
    BinOp(BinOp),
    Push(PushT),
    BuiltInCall(BuiltIn),
    Store(String),
    Load(String)
}

fn lex_into_tokens(code: &str, file: &str) -> Stack<TokenData> {
    let mut depth = 0;
    let mut line = 1;
    let mut tokens = Vec::default();
    let mut chars = code.chars().peekable();

    while let Some(char) = chars.next()
    {
        match char {
            '\n' => line += 1,
            ' ' | '\t' => continue,
            M_COMMENT_START!() => {
                let comment = take_with_predicate!(char, chars, |&c| c != '\n');
                tokens.push((Token::Comment(comment), file, line));
                line += 1;
            }
            M_BLOCK_START!() | M_BLOCK_END!() => {
                tokens.push((Token::Sym(char), file, line));
                depth += if char == M_BLOCK_START!() { 1 } else { -1 };
            }
            M_TYPE_AN_START!() => {
                let annotation = chars
                    .by_ref()
                    .take_while(|&char| char != M_TYPE_AN_END!())
                    .collect();

                tokens.push((Token::Annotation(annotation), file, line));
            }
            M_STR!() => {
                let content_vector = chars
                    .by_ref()
                    .take_while(|&char| char != M_STR!())
                    .collect::<Vec<char>>();

                let mut index = 0;
                let mut content = String::with_capacity(content_vector.len());

                while index < content_vector.len()
                {
                    let char = content_vector[index];
                    index += 1;

                    content.push(match (char, content_vector.get(index))
                    {
                        ('\\', Some(next_char)) => {
                            index += 1;

                            match next_char {
                                'n' => '\n',
                                't' => '\t',
                                '\\' => '\\',
                                M_STR!() => M_STR!(),
                                _ => emit_error!(
                                    file,
                                    line,
                                    format!("invalid escape sequence: \\{next_char}")
                                )
                            }
                        }
                        ('\\', None) => emit_error!(file, line, "incomplete escape sequence"),
                        _ => char
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
                    line
                ))
            }
            '0'..='9' => {
                let content =
                    take_with_predicate!(char, chars, |&c| c.is_ascii_digit() || c == '.');

                tokens.push((
                    match content.contains('.')
                    {
                        true => Token::Float(content.parse().unwrap()),
                        false => Token::Int(content.parse().unwrap()),
                    },
                    file,
                    line
                ));
            }
            _ => tokens.push((Token::Sym(char), file, line))
        }
    }

    assert_eq!(depth, 0);

    /* We need to parse differents code blocks now */
    /* TODO: rework on this algorithm */
    let mut stack = Vec::default();
    let mut out = Vec::default();
    let mut temp_tokens = Vec::default();

    for token in tokens.iter()
    {
        match token {
            (Token::Sym(M_BLOCK_START!()), _, _) /* Code block inside code block, meh */ => {
                stack.push(temp_tokens);
                temp_tokens = Vec::default();
            }
            (Token::Sym(M_BLOCK_END!()), _, _) /* Something is done, lets figure out what it is :3 */ => {
                let mut nested_tokens = temp_tokens.clone(); /* This operation must be veryyy expensive in time/memory usage */
                match stack.pop() {
                    Some(previous_tokens) /* Finished to parse the code block inside current code block */ => {
                        temp_tokens = previous_tokens;
                        temp_tokens.push((Token::Block(nested_tokens), file.into(), line));
                    }
                    None /* Current code block parsing is done */ => {
                        nested_tokens.reverse();
                        out.push((Token::Block(nested_tokens), file.to_string(), line))
                    },
                }
            }
            (token, file, line) => temp_tokens.push((token.clone(), file.to_string(), *line)),
        }
    }

    out.append(&mut temp_tokens);
    out
}

fn compile_tokens(tokens: &mut Tokens, rev: bool)
{
    if rev
    /* Sometimes we might act like the tokens vec was a stack */
    {
        tokens.reverse();
    }

    let mut instructions = Vec::new();
    let mut symbol_table = HashMap::new() as HashMap<String, i64>;
    let mut stack_pointer = 0_i64; /* Initialize stack pointer */

    while let Some((token, file, line)) = tokens.pop()
    {
        match token {
            Token::Sym(M_ADD!()) => instructions.push(Instruction::BinOp(BinOp::Add)),
            Token::Sym(M_SUB!()) => instructions.push(Instruction::BinOp(BinOp::Sub)),
            Token::Sym(M_MUL!()) => instructions.push(Instruction::BinOp(BinOp::Mul)),
            Token::Int(value) => instructions.push(Instruction::Push(PushT::Int(value))),
            Token::Str(value) => {
                instructions.push(Instruction::Push(PushT::Str(value.clone())));
                symbol_table.insert(value.clone(), stack_pointer);
                stack_pointer += value.len() as i64 + 1;
            }
            Token::Sym('~') => {
                let name = expect_token!(Name, tokens, file, line);
                instructions.push(Instruction::Store(name.clone()));
                symbol_table.insert(name, stack_pointer);
                stack_pointer += 1;
            }
            Token::Name(name) => {
                if let Some(&pos) = symbol_table.get(&name)
                {
                    instructions.push(Instruction::Load(name.clone()));
                }
                else
                {
                    match name.as_str() {
                        M_PUTS!() => instructions.push(Instruction::BuiltInCall(BuiltIn::Puts)),
                        "putsi" => instructions.push(Instruction::BuiltInCall(BuiltIn::PutsI)),
                        "syscall" => {
                            let n = expect_token!(Int, tokens, file, line);
                            instructions.push(Instruction::BuiltInCall(BuiltIn::Syscall(n)));
                        }
                        _ => unreachable!()
                    }
                }
            }
            _ => unreachable!()
        }
    }

    println!("section .text");
    println!("extern printf");
    println!("global _start");
    println!("_start:");

    let mut strs = Vec::new();

    for instruction in &instructions {
        match instruction {
            Instruction::Push(pusht) => match pusht {
                PushT::Int(value) => {
                    println!("   ;; push(int) {value}");
                    println!("   push {value}");
                }
                PushT::Str(value) => {
                    println!("   ;; push(str)");
                    println!("   push str_{}", strs.len());
                    strs.push(value.clone());
                }
            },
            Instruction::BuiltInCall(fun) => match fun {
                BuiltIn::PutsI => {
                    println!("   ;; puts integer");
                    println!("   mov rdi, print_int_fmt");
                    println!("   pop rsi");
                    println!("   xor rax, rax");
                    println!("   call printf");
                }
                BuiltIn::Puts => {
                    println!("   ;; puts string");
                    println!("   mov rdi, print_str_fmt");
                    println!("   pop rsi");
                    println!("   xor rax, rax");
                    println!("   call printf");
                }
                BuiltIn::Syscall(n) => {
                    println!("   ;; syscall");
                    for i in 1..n + 1 {
                        print!("   pop ");

                        let reg = match i {
                            1 => "rax",
                            2 => "rdi",
                            3 => "rsi",
                            4 => "rdx",
                            5 => "r10",
                            6 => "r8",
                            7 => "r9",
                            _ => unreachable!()
                        };

                        println!("{reg}");
                    }

                    println!("   syscall");
                    println!("   push rax");
                }
                _ => unreachable!()
            },
            Instruction::Store(name) => {
                println!("   ;; store variable {}", name);
                println!("   pop rax");
                println!("   mov [rsp - {}], rax", symbol_table[name] + 8);
            }
            Instruction::Load(name) => {
                println!("   ;; load variable {}", name);
                println!("   mov rax, [rsp - {}]", symbol_table[name] + 8);
                println!("   push rax");
            }
            Instruction::BinOp(op) => match op {
                BinOp::Add => {
                    println!("   ;; add ");
                    println!("   pop rax ;; rax <- RHS");
                    println!("   pop rbx ;; rbx <- LHS");
                    println!("   add rax, rbx");
                    println!("   push rax");
                }
                BinOp::Mul => {
                    println!("   ;; mul ");
                    println!("   pop rax ;; rax <- RHS");
                    println!("   pop rbx ;; rbx <- LHS");
                    println!("   mul rbx");
                    println!("   push rax");
                }
                BinOp::Sub => {
                    println!("   ;; sub ");
                    println!("   pop rax ;; rax <- RHS");
                    println!("   pop rbx ;; rbx <- LHS");
                    println!("   sub rbx, rax");
                    println!("   push rbx");
                }
                _ => unreachable!()
            },
            _ => unreachable!()
        }
    }

    println!("   mov rax, 60   ;; exit(");
    println!("   xor rdi, rdi  ;;  0");
    println!("   syscall       ;; );");

    println!("section .data");
    println!("   print_int_fmt: db \"%d\", 10, 0");
    println!("   print_str_fmt: db \"%s\", 0");

    for (index, string) in strs.iter().enumerate()
    {
        println!(
            "   str_{}: db {}, 0",
            index,
            /* Prevent escape/injection issues */
            string
                .chars()
                .map(|c| format!("0x{:x}", c as u8))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

/* A node in Maeel Stack VM */
struct Guitar {
    value: Cord,
    next: *mut Guitar /* Might be null ptr */
}

impl Guitar {
    fn new(value: Cord) -> *mut Self {
        let guitar = Guitar {
            value: value, next: null_mut()
        };

        Box::into_raw(Box::new(guitar))
    }
}

/* Maeel Stack VM */
struct BocchiVM {
    head: *mut Guitar
}

impl BocchiVM {
    fn process_tokens(
        &mut self,
        tokens: &mut Tokens,
        variables: &mut Mapper<Cord>,
        functions: &mut Mapper<FunData>,
        reverse: bool
    )
    {
        if reverse
        /* Sometimes we might act like the tokens vec was a stack */
        {
            tokens.reverse();
        }

        while let Some((token, file, line)) = tokens.pop()
        {
            match token {
                Token::Comment(_) | Token::Annotation(_) => {},

                Token::Str(_) | Token::Float(_) | Token::Int(_) => self.push(token.into()),

                Token::Sym(M_ADD!()) => binop!(|a, b: Cord| b.add(a), self, &file, line),

                Token::Sym(M_SUB!()) => binop!(|a, b: Cord| b.sub(a), self, &file, line),

                Token::Sym(M_MUL!()) => binop!(|a, b: Cord| b.mul(a), self, &file, line),

                Token::Sym(M_DIV!()) => binop!(|a, b: Cord| b.div(a), self, &file, line),

                Token::Sym(M_MOD!()) => binop!(|a, b: Cord| b.rem(a), self, &file, line),

                Token::Sym(M_EQ!()) => binop!(|a, b| Cord::Int((b == a) as M_INT_SIZE), self, &file, line),

                Token::Sym(M_LT!()) => binop!(|a, b| Cord::Int((b < a) as M_INT_SIZE), self, &file, line),

                Token::Sym(M_GT!()) => binop!(|a, b| Cord::Int((b > a) as M_INT_SIZE), self, &file, line),

                Token::Block(mut block) => {
                    block.reverse(); /* meow */
                    self.push(Cord::Fun((block.as_slice().into(), true)))
                }

                Token::Sym(M_FUN_PUSH!()) => {
                    let function_name /* Function name */ = expect_token!(Name, tokens, file, line);
                    let function /* Function object */ = functions.get(&function_name).unwrap_or_else(|| {
                        emit_error!(file, line, format!("undefined function: {function_name:?}"))
                    });

                    self.push(Cord::Fun(function.clone()))
                }

                Token::Sym(M_EXEC!()) /* Manually call a function */ => {
                    let (function_tokens, is_inline) = expect_stack!(Fun, self, file, line);

                    match is_inline {
                        true => function_tokens.iter().for_each(|t| tokens.push(t.clone())),
                        false => self.process_tokens(
                            &mut function_tokens.to_vec(), &mut variables.clone(), functions, true
                        )
                    }
                }

                Token::Sym(M_THEN!()) /* Basically "if" statement */ => {
                    let temp_tokens = expect_token!(Block, tokens, file, line);

                    if expect_stack!(Int, self, file, line) /* The boolean (actually an integer, anyways :3) */ == 1 {
                        let temp_tokens_length = temp_tokens.len();

                        (0..temp_tokens_length).for_each(|index| { /* Pushing from end to start */
                            let temp_token = temp_tokens
                                .get(temp_tokens_length - index - 1)
                                .unwrap();

                            tokens.push(temp_token.clone())
                        });
                    }
                }

                Token::Sym(M_FORCE_DEF!()) /* Can be pushed by interpreter only */ => {
                    let name = expect_token!(Name, tokens, file, line);
                    let value = self.pop()
                        .unwrap_or_else(|_| emit_error!(file, line, "stack is empty! (maeel)"));

                    variables.insert(name, value);
                }

                Token::Sym(M_DEF!()) /* Assign stack top value to next name */ => {
                    let name = expect_token!(Name, tokens, file, line);
                    if name.starts_with("__") /* Private field */ { panic!(/* TODO: make the error message */) }
                    let value = self.pop()
                        .unwrap_or_else(|_| emit_error!(file, line, "stack is empty!"));
                    variables.insert(name, value);
                }

                Token::Sym(char) => emit_error!(file, line, format!("unknown symbol: {char}.")),

                Token::Name(name) => match name.as_str() {
                    M_PUTS!() => print!("{}", self.pop().unwrap() /* TODO: make an error message when stack is empty */),

                    M_ELIST!() => self.push(Cord::List(Vec::new())),

                    M_FUN!() => {
                        let mut function_name = expect_token!(Name, tokens, file, line);
                        let mut function_tokens = Vec::default();
                        let is_inline = function_name == M_INLINE!();

                        if is_inline { function_name = expect_token!(Name, tokens, file, line) }

                        while let Some(temp_token) = tokens.pop()
                        {
                            match temp_token.clone() {
                                (Token::Annotation(_), _, _) => {},
                                (Token::Block(temp_tokens), _, _) => {
                                    function_tokens.reverse(); /* uhm */
                                    function_tokens.extend(temp_tokens);
                                    function_tokens.reverse(); /* never ask if maeel could be faster */
                                    break; /* TODO: remove this break, f*ck breaks */
                                }
                                (Token::Name(_), file, line) => {
                                    function_tokens.push(temp_token);
                                    function_tokens.push((Token::Sym(M_FORCE_DEF!()), file, line));
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

                    M_LEN!() => {
                        let length = match self.pop() {
                            Ok(Cord::Str(string)) => string.len(),
                            Ok(Cord::List(xs)) => xs.len(),
                            Ok(other) => emit_error!(file, line, format!("expected string or list, got {other:?}")),
                            Err(_) => emit_error!(file, line, "expected string or list, got EOS.")
                        } as M_INT_SIZE;

                        self.push(Cord::Int(length))
                    }

                    M_GET!() => {
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

                    M_READ!() => {
                        let buffer_size = expect_stack!(Int, self, file, line);
                        let mut buffer = vec![0u8; buffer_size as usize];

                        File::open(expect_stack!(Str, self, file, line)) /* Copy `buffer_size` bytes from file to `buf` */
                            .unwrap()
                            .read_exact(&mut buffer)
                            .unwrap();

                        let content_bytes = buffer /*  Convert 8 bytes size integers to M_INT_SIZE integers */
                            .iter()
                            .map(|byte| Cord::Int(*byte as M_INT_SIZE))
                            .collect();

                        self.push(Cord::List(content_bytes))
                    }

                    M_INCLUDE!() /* This is bad */ => {
                        let target = expect_token!(Str, tokens, file, line);
                        let content = match target.clone().as_str() {
                            "maeel" => include_str!(M_STD_LIB!()).to_string(),
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
                        if let Some(value) = variables.get(name)
                        {
                            self.push(value.clone())
                        }
                        else if let Some((function_tokens, inline)) = functions.get(name)
                        {
                            match inline {
                                true => function_tokens
                                    .iter()
                                    .for_each(|t| tokens.push(t.clone())),
                                false => self.process_tokens(
                                    &mut function_tokens.to_vec(), &mut variables.clone(), functions, false
                                )
                            }
                        }
                        else
                        {
                            emit_error!(file, line, format!("unknown name {name}"))
                        }
                    }
                },
            };
        }
    }

    /* Push an object to the stack */
    fn push(&mut self, value: Cord)
    {
        let future_head = Guitar::new(value);
        if !self.head.is_null() {
            unsafe { (*future_head).next = self.head; }
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
                    if i > 0 { write!(f, " ").unwrap() }
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
            (Self::Int(a), Self::Float(b))
            | (Self::Float(b), Self::Int(a)) => {
                Some(b.total_cmp(&(*a as M_FLOAT_SIZE)))
            }
            (a, b) => panic!("Cannot compare {} and {}", a, b),
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
            (Self::Int(a), Self::Float(b))
            | (Self::Float(b), Self::Int(a)) => {
                (*a as M_FLOAT_SIZE) == *b
            }
            _ => false,
        }
    }
}

impl Cord {
    fn sub(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(m), Self::Int(n)) => Self::Int(m - n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x - y),
            (Self::Float(x), Self::Int(m))
            | (Self::Int(m), Self::Float(x)) => {
                Self::Float(m as M_FLOAT_SIZE - x)
            }
            (a, b) => panic!("Cannot substract {} and {}", a, b),
        }
    }

    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(m), Self::Int(n)) => Self::Int(m * n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x * y),
            (Self::Float(x), Self::Int(m))
            | (Self::Int(m), Self::Float(x)) => {
                Self::Float(x * m as M_FLOAT_SIZE)
            }
            (Self::Int(m), Self::Str(s))
            | (Self::Str(s), Self::Int(m)) => {
                Self::Str(s.repeat(m as usize))
            }
            (a, b) => panic!("Cannot multiply {} and {}", a, b),
        }
    }

    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Str(a), Self::Str(b)) => Self::Str(a + &b),
            (Self::Int(m), Self::Int(n)) => Self::Int(m + n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x + y),
            (Self::Int(m), Self::Float(x))
            | (Self::Float(x), Self::Int(m)) => {
                Self::Float(m as M_FLOAT_SIZE + x)
            }
            (other, Self::List(mut xs))
            | (Self::List(mut xs), other) => {
                xs.push(other);
                Self::List(xs)
            }
            (a, b) => panic!("Cannot add {} and {}", a, b),
        }
    }

    fn rem(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(m), Self::Int(n)) => Self::Int(m % n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x % y),
            (Self::Int(m), Self::Float(x)) => Self::Float(m as M_FLOAT_SIZE % x),
            (Self::Float(x), Self::Int(m)) => Self::Float(x % m as M_FLOAT_SIZE),
            (a, b) => panic!("Cannot divide {} and {}", a, b),
        }
    }

    fn div(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(m), Self::Int(n)) => Self::Float(m as M_FLOAT_SIZE / n as M_FLOAT_SIZE),
            (Self::Float(x), Self::Float(y)) => Self::Float(x / y),
            (Self::Int(m), Self::Float(x)) => Self::Float(m as M_FLOAT_SIZE / x),
            (Self::Float(x), Self::Int(m)) => Self::Float(x / m as M_FLOAT_SIZE),
            (a, b) => panic!("Cannot divide {} and {}", a, b),
        }
    }
}

impl From<Token> for Cord {
    fn from(val: Token) -> Self {
        match val {
            Token::Str(x) => Cord::Str(x),
            Token::Int(x) => Cord::Int(x),
            Token::Float(x) => Cord::Float(x),
            Token::Block(x) => Cord::Fun((x.as_slice().into(), false)),
            _ => panic!(),
        }
    }
}

fn print_tokens(tokens: Vec<(Token, String, u16)>, indents: usize) {
    for (token, file, line) in tokens {
        match token {
            Token::Block(new_tokens) => print_tokens(new_tokens, indents + 1),
            _ => println!("{}{file}:{line}:{token:?}", "  ".repeat(indents)),
        }
    }
}

struct BocchiVMB {
    stack: Vec<CordB>
}

impl BocchiVMB {
    fn process_tokens(
        &mut self,
        tokens: &mut Tokens,
        variables: &mut Mapper<CordB>,
        functions: &mut Mapper<FunDataB>,
        reverse: bool,
    ) {
        if reverse
        /* Sometimes we might act like the tokens vec was a stack */
        {
            tokens.reverse();
        }

        while let Some((token, file, line)) = tokens.pop() {
            match token {
                Token::Comment(_) | Token::Annotation(_) => {},

                Token::Sym(M_ADD!()) => binop_b!(|a, b: CordB| b.add(a), self, &file, line),

                Token::Sym(M_SUB!()) => binop_b!(|a, b: CordB| b.sub(a), self, &file, line),

                Token::Sym(M_MUL!()) => binop_b!(|a, b: CordB| b.mul(a), self, &file, line),

                Token::Sym(M_MOD!()) => binop_b!(|a, b: CordB| b.rem(a), self, &file, line),

                Token::Sym(M_DIV!()) => binop_b!(|a, b: CordB| CordB::Float, self, &file, line),

                Token::Sym(M_EQ!())
                | Token::Sym(M_LT!())
                | Token::Sym(M_GT!()) => binop_b!(|a, b| CordB::Int, self, &file, line),

                Token::Str(_)
                | Token::Float(_)
                | Token::Int(_) => self.stack.push(token.into()),

                Token::Block(mut block) => {
                    block.reverse(); /* meow */
                    self.stack.push(CordB::Fun((block.as_slice().into(), true, Vec::new(), Vec::new())))
                }

                Token::Sym(M_FUN_PUSH!()) => {
                    let function_name /* Function name */ = expect_token!(Name, tokens, file, line);
                    let function /* Function object */ = functions.get(&function_name).unwrap_or_else(|| {
                        emit_error!(file, line, format!("undefined function: {function_name:?}"))
                    });

                    self.stack.push(CordB::Fun(function.clone()))
                }

                Token::Sym(M_EXEC!()) /* Manually call a function */ => {
                    let (function_tokens, inline, input, output) = match self.stack.pop() {
                        Some(CordB::Fun(value)) => value,
                        Some(CordB::Any) => default_fun!(),
                        Some(other) => emit_error!(file, line, format!("expected {:?} on the stack, got {other:?}", CordBRepr::Fun)),
                        None => emit_error!(file, line, format!("expected {:?}, got EOF", CordBRepr::Fun)),
                    };

                    self.check_types(&input, &file, &line, TCType::Input);

                    match inline {
                        true => function_tokens.iter().for_each(|t| tokens.push(t.clone())),
                        false => self.process_tokens(
                            &mut function_tokens.to_vec(), &mut variables.clone(), functions, true
                        ),
                    }

                    self.check_types(&output, &file, &line, TCType::Output);
                }

                Token::Sym(M_THEN!()) /* Basically "if" statement */ => {
                    let temp_tokens = expect_token!(Block, tokens, file, line);
                    expect_stack_b!(Int, self, file, line);
                },

                Token::Sym(M_FORCE_DEF!()) /* Can be pushed by interpreter only */ => {
                    variables.insert(
                        expect_token!(Name, tokens, file, line),
                        self.stack.pop().unwrap_or_else(|| emit_error!(file, line, "stack is empty! (maeel)")),
                    );
                }

                Token::Sym(M_DEF!()) /* Assign stack top value to next name */ => {
                    let name = expect_token!(Name, tokens, file, line);
                    if name.starts_with("__") /* Private field */ { panic!(/* TODO: make the error message */) }
                    variables.insert(name, self.stack.pop().unwrap_or_else(|| emit_error!(file, line, "stack is empty!")));
                }

                Token::Sym(char) => emit_error!(file, line, format!("unknown symbol: {char}.")),

                Token::Name(name) => match name.as_str() {
                    "putsstack" => println!("{:?}", self.stack),

                    M_PUTS!() => { self.stack.pop().unwrap(); }

                    M_ELIST!() => self.stack.push(CordB::List),

                    M_FUN!() => {
                        let mut function_name = expect_token!(Name, tokens, file, line);
                        let mut function_tokens = Vec::default();
                        let is_inline = function_name == M_INLINE!();

                        if is_inline { function_name = expect_token!(Name, tokens, file, line) }

                        let mut input = Vec::default();
                        let mut output = Vec::default();

                        while let Some(temp_token) = tokens.pop() {
                            match temp_token.clone() {
                                (Token::Annotation(annotation), _, _) => {
                                    let mut iter = annotation.split(" -> ")
                                        .map(|p| p.split(" ").map(|s| s.to_string()).collect());

                                    input = iter.next().unwrap_or_else(|| vec![String::from("Any")]);
                                    output = iter.next().unwrap_or_else(|| vec![String::from("Any")]);
                                },

                                (Token::Block(temp_tokens), _, _) => {
                                    function_tokens.reverse(); /* uhm */
                                    function_tokens.extend(temp_tokens);
                                    function_tokens.reverse(); /* never ask if maeel could be faster */
                                    break; /* TODO: remove this break, f*ck breaks */
                                }
                                (Token::Name(_), file, line) => {
                                    function_tokens.push(temp_token);
                                    function_tokens.push((Token::Sym(M_FORCE_DEF!()), file, line));
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

                        functions.insert(function_name.clone(), (function_tokens.as_slice().into(), is_inline, input, output));
                    }

                    M_LEN!() => {
                        match self.stack.pop() {
                            Some(CordB::Str)
                            | Some(CordB::List) => CordB::Int,
                            Some(other) => CordB::Any,
                            None => emit_error!(file, line, "expected string or list, got EOS."),
                        };

                        self.stack.push(CordB::Int)
                    }

                    M_GET!() => self.stack.push(CordB::Any),

                    M_READ!() => self.stack.push(CordB::List),

                    M_INCLUDE!() /* This is bad */ => {
                        let target = expect_token!(Str, tokens, file, line);

                        let content = match target.clone().as_str() {
                            "maeel" => include_str!(M_STD_LIB!()).to_string(),
                            _ => read_to_string(&target).unwrap_or_else(|_| {
                                emit_error!(file, line, "failed to include file")
                            }),
                        };

                        let temp_tokens = lex_into_tokens(&content, &target);
                        let temp_tokens_length = temp_tokens.len();

                        for index in 0..temp_tokens_length {
                            tokens.push(temp_tokens.get(temp_tokens_length - index - 1).unwrap().clone())
                        }
                    }

                    name => {
                        if let Some(value) = variables.get(name)
                        {
                            self.stack.push(value.clone())
                        }
                        else if let Some((function_tokens, inline, input, output)) = functions.get(name)
                        {
                            self.check_types(input, &file, &line, TCType::Input);

                            match inline {
                                true => function_tokens.iter().for_each(|t| tokens.push(t.clone())),
                                false => self.process_tokens(
                                    &mut function_tokens.to_vec(),
                                    &mut variables.clone(),
                                    &mut functions.clone(),
                                    false
                                ),
                            }

                            self.check_types(output, &file, &line, TCType::Output);
                        }
                        else
                        {
                            println!("{file}:{line}: Unknown name: {name}")
                        }
                    }
                },
            };
        }
    }

    fn check_types(&self, expected: &Vec<String>, file: &str, line: &u16, tctype: TCType)
    {
        let stack_len = self.stack.len();
        let expected_len = expected.len();

        if stack_len < expected_len
        {
            println!("{file}:{line}: Typing error at {tctype:?}: stack size is lower than expected")
        }

        let part = self.stack[stack_len - expected_len..]
            .into_iter()
            .map(|c| c.to_string())
            .collect::<Vec<String>>();

        for index in 0..expected_len
        {
            let real = part.get(index).unwrap();
            let expected_cord = expected.get(index).unwrap();

            if expected_cord == "Any"
            || real == "Any"
            || real == expected_cord
            {
                continue;
            }

            let mut verified = false;

            if expected_cord.contains("|")
            {
                let expected_cords =
                    expected_cord.split("|").map(|s| s.to_string()).collect::<Vec<String>>();

                for cord in expected_cords
                {
                    if cord == "Any"
                    || real == &cord
                    {
                        verified = true
                    }
                }
            }

            if verified
            {
                continue
            }

            println!("{file}:{line}: Typing error at {tctype:?}: expected at stack pos {index}: {expected_cord:?}, got {real:?}")
        }
    }
}

impl Display for CordB
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result
    {
        match self
        {
            Self::Str => write!(f, "Str"),
            Self::Fun(_) => write!(f, "Fun"),
            Self::Float => write!(f, "Float"),
            Self::Int => write!(f, "Int"),
            Self::List => write!(f, "List"),
            Self::Any => write!(f, "Any"),
        }
    }
}

impl CordB
{
    fn sub(self, rhs: Self) -> Self
    {
        match (self, rhs)
        {
            (Self::Int, Self::Int) => Self::Int,
            (Self::Float, Self::Float)
            | (Self::Float, Self::Int)
            | (Self::Int, Self::Float) => Self::Float,
            _ => Self::Any,
        }
    }

    fn mul(self, rhs: Self) -> Self
    {
        match (self, rhs)
        {
            (Self::Int, Self::Int) => Self::Int,
            (Self::Float, Self::Float)
            | (Self::Float, Self::Int)
            | (Self::Int, Self::Float) => Self::Float,
            (Self::Int, Self::Str) | (Self::Str, Self::Int) => Self::Str,
            _ => Self::Any,
        }
    }

    fn add(self, rhs: Self) -> Self
    {
        match (self, rhs)
        {
            (Self::Str, Self::Str) => Self::Str,
            (Self::Int, Self::Int) => Self::Int,
            (Self::Float, Self::Float)
            | (Self::Int, Self::Float)
            | (Self::Float, Self::Int) => Self::Float,
            (_, Self::List)
            | (Self::List, _) => Self::List,
            _ => Self::Any,
        }
    }

    fn rem(self, rhs: Self) -> Self
    {
        match (self, rhs)
        {
            (Self::Int, Self::Int) => Self::Int,
            (Self::Float, Self::Float)
            | (Self::Int, Self::Float)
            | (Self::Float, Self::Int) => Self::Float,
            _ => Self::Any,
        }
    }
}

impl From<String> for CordB
{
    fn from(val: String) -> Self
    {
        match val.as_str()
        {
            "Str" => CordB::Str,
            "Int" => CordB::Int,
            "Float" => CordB::Float,
            "Any" => CordB::Any,
            "List" => CordB::List,
            "Fun" => CordB::Fun(default_fun!()),
            _ => panic!(),
        }
    }
}

impl From<Token> for CordB
{
    fn from(val: Token) -> Self
    {
        match val
        {
            Token::Str(_) => CordB::Str,
            Token::Int(_) => CordB::Int,
            Token::Float(_) => CordB::Float,
            Token::Block(x) => CordB::Fun((x.as_slice().into(), false, Vec::new(), Vec::new())),
            _ => panic!(),
        }
    }
}

fn main()
{
    let file = args().nth(1).unwrap();

    BocchiVM { head: null_mut() }.process_tokens(
        &mut lex_into_tokens(&read_to_string(&file).unwrap(), &file),
        &mut HashMap::default(), /* Variables Hashmap */
        &mut HashMap::default(), /* functions Hashmap */
        true,
    )
}
