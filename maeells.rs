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
                | Token::Comment(_) => {},
                | Token::Sym(M_ADD!()) => {},
                | Token::Sym(M_SUB!()) => {},
                | Token::Sym(M_MUL!()) => {},
                | Token::Sym(M_DIV!()) => {},
                | Token::Sym(M_MOD!()) => {},
                | Token::Sym(M_EQ!()) => {},
                | Token::Sym(M_LT!()) => {},
                | Token::Sym(M_GT!()) => {},
                | Token::Str(_) | Token::Flt(_) | Token::Int(_) => {},
                | Token::Block(mut block) => {}
                | Token::Sym(M_FUN_PUSH!()) => {}
                | Token::Sym(M_EXEC!()) => {}
                | Token::Sym(M_THEN!()) => {}
                
                | Token::Sym(M_FORCE_DEF!()) => {}
                | Token::Sym(M_DEF!()) => {}

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
                                    function_tokens.push((Token::Sym(M_FORCE_DEF!()), file, line));
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
                    | M_LEN!() => {}
                    | M_GET!() => {}
                    | M_READ!() => {}
                    | M_INCLUDE!() => {}
                    | name => {}
                },
            };
        }
    }

    /* Push an object to the stack */
    fn push(&mut self, value: Cord) {
        let future_head = Guitar::new(value);

        if !self.head.is_null() { unsafe { (*future_head).next = self.head; } }

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
    );
}
