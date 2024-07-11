use std::fs::File;
use std::io::Read;

mod maeellex;

#[macro_use]
use maeellex::*;

type FunDataB = (std::rc::Rc<[TokenData]>, bool, Vec<String>, Vec<String>);

macro_rules! expect_token {
    ($token:tt, $tokens:expr, $fl:expr, $line:expr) => {{
        match $tokens.pop() {
            Some((Token::$token(value), _, _)) => value,
            Some((other, file, line)) => emit_error!(
                file,
                line,
                format!("expected {:?}, got {other:?}", TokenRepr::$token)
            ),
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
            Ok(Cord::$tpe) => Cord::$tpe,
            Ok(other) => emit_error!(
                $fl,
                $line,
                format!("expected {:?} on the stack, got {other:?}", CordRepr::$tpe)
            ),
            Err(_) => emit_error!(
                $fl,
                $line,
                format!("expected {:?}, got EOF", CordRepr::$tpe)
            ),
        }
    }};
}

macro_rules! binop {
    ($app:expr, $self:expr, $file:expr, $line:expr) => {{
        let output = $app(
            $self.pop().unwrap_or_else(|_| {
                emit_error!($file, $line, "stack is empty! (binary operation LHS)")
            }),
            $self.pop().unwrap_or_else(|_| {
                emit_error!($file, $line, "stack is empty! (binary operation RHS)")
            }),
        );

        $self.push(output)
    }};
}

/* Types that are used by the VM */
#[derive(Debug, Clone)]
enum Cord {
    Flt,
    Int,
    Str,
    Lst,
    Unk,
    Nil,
    Fun(FunDataB),
}

#[derive(Debug, Clone)]
enum CordRepr {
    Flt,
    Int,
    Str,
    Lst,
    Unk,
    Nil,
    Fun,
}

/* Maeel Stack VM */
struct BocchiVM {
    stack: Vec<Cord>,
}

impl BocchiVM {
    fn process_tokens(
        &mut self,
        tokens: &mut Vec<TokenData>,
        variables: &mut Mapper<Cord>,
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
                Token::Sym(M_ADD!()) => binop!(|a, b: Cord| b.add(a), self, &file, line),
                Token::Sym(M_SUB!()) => binop!(|a, b: Cord| b.sub(a), self, &file, line),
                Token::Sym(M_MUL!()) => binop!(|a, b: Cord| b.mul(a), self, &file, line),
                Token::Sym(M_DIV!()) => binop!(|a, b: Cord| b.div(a), self, &file, line),
                Token::Sym(M_MOD!()) => binop!(|a, b: Cord| b.rem(a), self, &file, line),

                Token::Sym(M_EQ!()) => binop!(|a, b| Cord::Int, self, &file, line),
                Token::Sym(M_LT!()) => binop!(|a, b| Cord::Int, self, &file, line),
                Token::Sym(M_GT!()) => binop!(|a, b| Cord::Int, self, &file, line),

                Token::Str(_) | Token::Flt(_) | Token::Int(_) => self.push(token.into()),

                Token::Block(mut block) => {
                    block.reverse(); /* meow */
                    self.push(Cord::Fun((block.as_slice().into(), true, Vec::new(), Vec::new())))
                }

                Token::Sym(M_FUN_PUSH!()) => {
                    let function_name /* Function name */   = expect_token!(Name, tokens, file, line);
                    let function      /* Function object */ = functions.get(&function_name).unwrap_or_else(|| {
                        emit_error!(file, line, format!("undefined function: {function_name:?}"))
                    });

                    self.push(Cord::Fun(function.clone()))
                }

                Token::Sym(M_EXEC!()) /* Manually call a function */ => {
                    let (function_tokens, inline, input, output) = match self.pop() {
                        Ok(Cord::Fun(value)) => value,
                        Ok(Cord::Unk)        => (Vec::new().as_slice().into(), true, Vec::new(), Vec::new()),
                        Ok(other)            => emit_error!(file, line, format!("expected {:?} on the stack, got {other:?}", CordRepr::Fun)),
                        Err(_)               => emit_error!(file, line, format!("expected {:?}, got EOF", CordRepr::Fun)),
                    };

                    if !input.is_empty() {
                        let stack_len = self.stack.len();
                        let input_len = input.len();

                        if stack_len < input_len {
                            println!("{file}:{line}: Typing error: stack size is lower than expected")
                        }

                        let part = self.stack[stack_len - input_len..]
                            .into_iter()
                            .map(|c| c.to_string())
                            .collect::<Vec<String>>();

                        if part != input {
                            println!("{file}:{line}: Typing error: expected {input:?}, got {part:?}")
                        }

                        drop(part);
                    }

                    match inline {
                        true  => function_tokens.iter().for_each(|t| tokens.push(t.clone())),
                        false => self.process_tokens(
                            &mut function_tokens.to_vec(), &mut variables.clone(), functions, true
                        ),
                    }

                    if !output.is_empty() {
                        let stack_len = self.stack.len();
                        let output_len = output.len();

                        if stack_len < output_len {
                            println!("{file}:{line}: Typing error: stack size is lower than expected")
                        }

                        let part = self.stack[stack_len - output_len..]
                            .into_iter()
                            .map(|c| c.to_string())
                            .collect::<Vec<String>>();

                        if part != output {
                            println!("{file}:{line}: Typing error: expected {output:?}, got {part:?}")
                        }

                        drop(part);
                    }
                }

                Token::Sym(M_THEN!()) /* Basically "if" statement */ => {
                    let temp_tokens = expect_token!(Block, tokens, file, line);
                    expect_stack!(Int, self, file, line);
                },

                Token::Sym(M_FORCE_DEF!()) /* Can be pushed by interpreter only */ => {
                    variables.insert(
                        expect_token!(Name, tokens, file, line),
                        self.pop().unwrap_or_else(|_| emit_error!(file, line, "stack is empty! (maeel)")),
                    );
                }
                Token::Sym(M_DEF!()) /* Assign stack top value to next name */ => {
                    let name = expect_token!(Name, tokens, file, line);
                    if name.starts_with("__") /* Private field */ { panic!(/* TODO: make the error message */) }
                    variables.insert(name, self.pop().unwrap_or_else(|_| emit_error!(file, line, "stack is empty!")));
                }
                Token::Sym(char) => emit_error!(file, line, format!("unknown symbol: {char}.")),
                Token::Name(name) => match name.as_str() {
                    "putsstack" => println!("{:?}", self.stack),
                    M_PUTS!() => { self.pop().unwrap(); }
                    M_ELIST!() => self.push(Cord::Lst),
                    M_FUN!() => {
                        let mut function_name   = expect_token!(Name, tokens, file, line);
                        let mut function_tokens = Vec::default();
                        let is_inline           = function_name == M_INLINE!();

                        if is_inline { function_name = expect_token!(Name, tokens, file, line) }

                        let mut input = Vec::default();
                        let mut output = Vec::default();

                        while let Some(temp_token) = tokens.pop() {
                            match temp_token.clone() {
                                (Token::Annotation(annotation), _, _) => {
                                    let mut iter = annotation.split(" -> ")
                                        .map(|p| p.split(" ").map(|s| s.to_string()).collect::<Vec<String>>());
                                    
                                    input = iter.next().unwrap_or_else(|| Vec::default());
                                    output = iter.next().unwrap_or_else(|| Vec::default());
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
                        match self.pop() {
                            Ok(Cord::Str) | Ok(Cord::Lst) => Cord::Int,
                            Ok(other)                     => Cord::Unk,
                            Err(_)                        => emit_error!(file, line, "expected string or list, got EOS."),
                        };

                        self.push(Cord::Int)
                    }
                    M_GET!() => self.push(Cord::Unk),
                    M_READ!() => self.push(Cord::Lst),
                    M_INCLUDE!() /* This is bad */ => {
                        let target = expect_token!(Str, tokens, file, line);

                        let content = match target.clone().as_str() {
                            "maeel" => include_str!(M_STD_LIB!()).to_string(),
                            _       => std::fs::read_to_string(&target).unwrap_or_else(|_| {
                                emit_error!(file, line, "failed to include file")
                            }),
                        };

                        let temp_tokens        = lex_into_tokens(&content, &target);
                        let temp_tokens_length = temp_tokens.len();

                        /* Push tokens from end to start (reverse) */
                        for index in 0..temp_tokens_length {
                            tokens.push(temp_tokens.get(temp_tokens_length - index - 1).unwrap().clone())
                        }
                    }
                    name => {
                        if let Some(value) = variables.get(name) {
                            self.push(value.clone())
                        } else if let Some((function_tokens, inline, input, output)) = functions.get(name) {
                            if !input.is_empty() {
                                let stack_len = self.stack.len();
                                let input_len = input.len();

                                if stack_len < input_len {
                                    println!("{file}:{line}: Typing error: stack size is lower than expected")
                                }

                                let part = self.stack[stack_len - input_len..]
                                    .into_iter()
                                    .map(|c| c.to_string())
                                    .collect::<Vec<String>>();

                                if &part != input {
                                    println!("{file}:{line}: Typing error: expected {input:?}, got {part:?}")
                                }

                                drop(part);
                            }

                            match inline {
                                true  => function_tokens.iter().for_each(|t| tokens.push(t.clone())),
                                false => self.process_tokens(
                                    &mut function_tokens.to_vec(),
                                    &mut variables.clone(),
                                    &mut functions.clone(),
                                    false
                                ),
                            }

                            if !output.is_empty() {
                                let stack_len = self.stack.len();
                                let output_len = output.len();

                                if stack_len < output_len {
                                    println!("{file}:{line}: Typing error: stack size is lower than expected")
                                }

                                let part = self.stack[stack_len - output_len..]
                                    .into_iter()
                                    .map(|c| c.to_string())
                                    .collect::<Vec<String>>();

                                if &part != output {
                                    println!("{file}:{line}: Typing error: expected {output:?}, got {part:?}")
                                }

                                drop(part);
                            }
                        } else {
                            println!("{file}:{line}: Unknown name: {name}")
                        }
                    }
                },
            };
        }
    }

    /* Push an object to the stack */
    fn push(&mut self, value: Cord) {
        self.stack.push(value);
    }

    /* Drop-and-return the stack head */
    fn pop(&mut self) -> Result<Cord, Box<dyn std::error::Error>> {
        match self.stack.is_empty() {
            true => Err("Stack is empty".into()),
            false => Ok(self.stack.pop().unwrap()),
        }
    }
}

impl std::fmt::Display for Cord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str => write!(f, "Str"),
            Self::Fun(_) => write!(f, "Fun"),
            Self::Flt => write!(f, "Float"),
            Self::Int => write!(f, "Int"),
            Self::Lst => write!(f, "List"),
            Self::Unk => write!(f, "Unknown"),
            Self::Nil => write!(f, "Nil"),
        }
    }
}

impl Cord {
    fn sub(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int, Self::Int) => Self::Int,
            (Self::Flt, Self::Flt) | (Self::Flt, Self::Int) | (Self::Int, Self::Flt) => Self::Flt,
            _ => Self::Unk,
        }
    }

    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int, Self::Int) => Self::Int,
            (Self::Flt, Self::Flt) | (Self::Flt, Self::Int) | (Self::Int, Self::Flt) => Self::Flt,
            (Self::Int, Self::Str) | (Self::Str, Self::Int) => Self::Str,
            _ => Self::Unk,
        }
    }

    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Str, Self::Str) => Self::Str,
            (Self::Int, Self::Int) => Self::Int,
            (Self::Flt, Self::Flt) | (Self::Int, Self::Flt) | (Self::Flt, Self::Int) => Self::Flt,
            (_, Self::Lst) | (Self::Lst, _) => Self::Lst,
            _ => Self::Unk,
        }
    }

    fn rem(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int, Self::Int) => Self::Int,
            (Self::Flt, Self::Flt) | (Self::Int, Self::Flt) | (Self::Flt, Self::Int) => Self::Flt,
            _ => Self::Unk,
        }
    }

    fn div(self, rhs: Self) -> Self {
        Self::Flt
    }
}

impl From<String> for Cord {
    fn from(val: String) -> Self {
        match val.as_str() {
            "Str" => Cord::Str,
            "Int" => Cord::Int,
            "Float" => Cord::Flt,
            "Fun" => Cord::Fun((Vec::new().as_slice().into(), true, Vec::new(), Vec::new())),
            "Unk" => Cord::Unk,
            "Nil" => Cord::Nil,
            "List" => Cord::Lst,
            _ => panic!(),
        }
    }
}

impl From<Token> for Cord {
    fn from(val: Token) -> Self {
        match val {
            Token::Str(_) => Cord::Str,
            Token::Int(_) => Cord::Int,
            Token::Flt(_) => Cord::Flt,
            Token::Block(x) => Cord::Fun((x.as_slice().into(), false, Vec::new(), Vec::new())),
            _ => panic!(),
        }
    }
}

fn main() {
    let file = std::env::args().nth(1).unwrap();

    BocchiVM {
        stack: Vec::default(),
    }
    .process_tokens(
        &mut lex_into_tokens(&std::fs::read_to_string(&file).unwrap(), &file),
        &mut std::collections::HashMap::default(), /* Variables Hashmap */
        &mut std::collections::HashMap::default(), /* functions Hashmap */
        true,
    )
}
