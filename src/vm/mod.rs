use hashbrown::HashMap;

use crate::lexer::{lex_into_tokens, Token};

use std::error::Error;
use std::fs::read_to_string;
use std::fs::File;
use std::io::Read;
use std::ptr;

/* Function type */
pub type Fun = (Vec<Token>, bool);

/* Binary VM application */
pub type BinApp = fn(MaeelType, MaeelType) -> MaeelType;

/* Default VM function output */
type VMOutput<T> = Result<T, Box<dyn Error>>;

pub enum MaeelType {
    Float(f32),
    Integer(i32),
    String(String),
    Array(Vec<Self>),
    Function(Fun),
    Structure(HashMap<String, Self>),
}

struct Guitar<T> {
    value: T,             /* Node type */
    next: *mut Guitar<T>, /* Raw pointer to the next node */
}

impl<T> Guitar<T> {
    pub fn new(value: T) -> *mut Self {
        Box::into_raw(Box::new(Guitar {
            value,
            next: ptr::null_mut(), /* Mutable null pointer */
        }))
    }
}

pub struct BocchiVM {
    head: *mut Guitar<MaeelType>, /* Raw pointer to the head node */
}

impl Default for BocchiVM {
    fn default() -> Self {
        BocchiVM {
            head: ptr::null_mut(),
        }
    }
}

impl BocchiVM {
    /// Perform a binary operation
    fn binary_op(&mut self, app: BinApp) -> VMOutput<()> {
        let output = app(self.pop()?, self.pop()?);

        self.push(output)
    }

    fn parse_array(
        &mut self,
        tokens: &mut Vec<Token>,
        vars: &mut HashMap<String, MaeelType>,
    ) -> VMOutput<()> {
        let mut xs = Vec::default();

        while let Some(temporary_token) = tokens.pop() {
            match temporary_token {
                /* Stop parsing the array */
                Token::ArrayEnd => break,

                /* Parse an array inside an array */
                Token::ArrayStart => {
                    self.parse_array(tokens, vars)?;
                    xs.push(self.pop()?);
                }

                /* Push a string to the current array */
                Token::String(value) => xs.push(MaeelType::String(value)),

                /* Push a float to the current array */
                Token::Float(value) => xs.push(MaeelType::Float(value)),

                /* Push an integer to the current array */
                Token::Integer(value) => xs.push(MaeelType::Integer(value)),

                /* Push a code block to the current array */
                Token::Block(value) => xs.push(MaeelType::Function((value, false))),

                Token::Identifier(identifier) => match vars.get(&identifier) {
                    Some(value) => xs.push(value.clone()),

                    _ => panic!(),
                },

                _ => panic!(),
            }
        }

        /* Finally, push the array on the stack */
        self.push(MaeelType::Array(xs))?;

        Ok(())
    }

    pub fn process_tokens<'a>(
        &mut self,
        tokens: &'a mut Vec<Token>,                    /* Program tokens */
        vars: &'a mut HashMap<String, MaeelType>,      /* Global vars */
        funs: &'a mut HashMap<String, Fun>,            /* Global funs */
        structs: &'a mut HashMap<String, Vec<String>>, /* Global structs */
    ) -> VMOutput<()> {
        tokens.reverse();

        while let Some(token) = tokens.pop() {
            match token {
                Token::BlockStart | Token::BlockEnd | Token::ArrayEnd => panic!(),

                /* Parse arrays */
                Token::ArrayStart => self.parse_array(tokens, vars)?,

                /* Perform a binary operation */
                Token::BinaryOP(app) => self.binary_op(app)?,

                /* Push a string */
                Token::String(content) => self.push(MaeelType::String(content))?,

                /* Push a float */
                Token::Float(content) => self.push(MaeelType::Float(content))?,

                /* Push an integer */
                Token::Integer(content) => self.push(MaeelType::Integer(content))?,

                /* Push an anonymous function */
                Token::Block(content) => self.push(MaeelType::Function((content, false)))?,

                /* Access structures members */
                Token::Dot => match self.pop() {
                    Ok(MaeelType::Structure(structure)) => self.push(
                        structure
                            .get(&match tokens.pop() {
                                Some(Token::Identifier(value)) => value,

                                _ => panic!(),
                            })
                            .unwrap()
                            .clone(),
                    )?,

                    _ => panic!(),
                },

                /* Use functions as first class objects */
                Token::Colon => {
                    /* Function object */
                    let fun = funs
                        .get(&match tokens.pop() {
                            Some(Token::Identifier(value)) => value,

                            _ => panic!(),
                        })
                        .unwrap();

                    self.push(MaeelType::Function((
                        fun.0.to_vec(), /* Function tokens */
                        fun.1,          /* Function inline descriptor */
                    )))?
                }

                Token::Call => {
                    /* Function object */
                    let fun = match self.pop() {
                        Ok(MaeelType::Function(value)) => value,

                        _ => panic!(),
                    };

                    if fun.1
                    /* Inline function */
                    {
                        fun.0.iter().for_each(|token| tokens.push(token.clone()));
                    } else {
                        self.process_tokens(
                            &mut fun.0.clone(), /* Function tokens */
                            &mut vars.clone(),  /* Clone the variables */
                            funs,
                            structs,
                        )?
                    }
                }

                Token::Then => {
                    let temporary_token = tokens.pop();

                    match self.pop() {
                        Ok(MaeelType::Integer(1)) => match temporary_token {
                            Some(Token::Block(temporary_tokens)) => temporary_tokens
                                .iter()
                                .rev()
                                .for_each(|token| tokens.push(token.clone())),

                            Some(temporary_token) => tokens.push(temporary_token),

                            None => panic!(),
                        },

                        Ok(MaeelType::Integer(0)) => {}

                        _ => panic!(),
                    }
                }

                Token::Assignment => {
                    let name = match tokens.pop() {
                        Some(Token::Identifier(value)) => value,

                        _ => panic!(),
                    };

                    vars.insert(name, self.pop()?);
                }

                Token::Identifier(identifier) => match identifier.as_str() {
                    /* Print the top token */
                    "print" => print!("{}", self.peek()?),

                    /* Stop processing the tokens */
                    "break" => break,

                    /* Process "clear" VM operation */
                    "clear" => self.clear()?,

                    /* Process "fastpop" VM operation */
                    "drop" => self.fastpop()?,

                    /* Process "dup" VM operation */
                    "dup" => self.dup()?,

                    /* Process "swap" VM operation */
                    "swap" => self.swap()?,

                    /* Process "over" VM operation */
                    "over" => self.over()?,

                    /* Process "rotate" VM operation */
                    "rot" => self.rot()?,

                    "for" => {
                        let temporary_tokens = match tokens.pop() {
                            Some(Token::Block(value)) => value,

                            _ => panic!(),
                        };

                        match self.pop() {
                            Ok(MaeelType::Array(xs)) => {
                                xs.iter().for_each(|x| {
                                    self.push(x.clone()).unwrap();

                                    self.process_tokens(
                                        &mut temporary_tokens.clone(),
                                        vars,
                                        funs,
                                        structs,
                                    )
                                    .unwrap();
                                });
                            }

                            Ok(MaeelType::String(string)) => {
                                string.chars().for_each(|x| {
                                    self.push(MaeelType::String(x.to_string())).unwrap();

                                    self.process_tokens(
                                        &mut temporary_tokens.clone(),
                                        vars,
                                        funs,
                                        structs,
                                    )
                                    .unwrap();
                                });
                            }

                            _ => panic!(),
                        }
                    }

                    "while" => {
                        let temporary_tokens = match tokens.pop() {
                            Some(Token::Block(value)) => value,

                            _ => panic!(),
                        };

                        while match self.pop() {
                            Ok(MaeelType::Integer(1)) => true,  /* Continue looping */
                            Ok(MaeelType::Integer(0)) => false, /* Stop looping */
                            _ => panic!(),                      /* No boolean on the stack */
                        } {
                            self.process_tokens(&mut temporary_tokens.clone(), vars, funs, structs)?
                        }
                    }

                    "struct" => {
                        let struct_name = match tokens.pop() {
                            Some(Token::Identifier(value)) => value,
                            _ => panic!(),
                        };

                        let mut struct_fields = Vec::default();

                        while let Some(temporary_tokens) = tokens.pop() {
                            match temporary_tokens {
                                Token::Dot =>
                                /* Stop parsing structure fields on '.' */
                                {
                                    break;
                                }

                                Token::Identifier(identifier) => {
                                    struct_fields.push(identifier);
                                }

                                _ => panic!(),
                            }
                        }

                        struct_fields.reverse();

                        structs.insert(struct_name, struct_fields);
                    }

                    "fun" => {
                        let mut fun_name = match tokens.pop() {
                            Some(Token::Identifier(value)) => value,

                            _ => panic!(),
                        };

                        let mut is_inline = false;

                        if fun_name == "inline" {
                            is_inline = true;

                            fun_name = match tokens.pop() {
                                Some(Token::Identifier(value)) => value,

                                _ => panic!(),
                            }
                        }

                        let mut fun_tokens = Vec::default(); /* Final tokens */

                        while let Some(temporary_token) = tokens.pop() {
                            match temporary_token {
                                Token::Block(temporary_tokens) => {
                                    fun_tokens.reverse(); /* First reverse */

                                    fun_tokens.extend(temporary_tokens);

                                    fun_tokens.reverse(); /* Second reverse */

                                    break;
                                }

                                Token::Identifier(_) => {
                                    fun_tokens.push(temporary_token);
                                    fun_tokens.push(Token::Assignment);
                                }

                                _ => panic!(),
                            }
                        }

                        funs.insert(fun_name.clone(), (fun_tokens, is_inline));
                    }

                    "get" => {
                        let index = match self.pop() {
                            Ok(MaeelType::Integer(value)) => value as usize,

                            _ => panic!(),
                        };

                        match self.pop() {
                            Ok(MaeelType::Array(xs)) => self.push(xs.get(index).unwrap().clone()),

                            Ok(MaeelType::String(string)) => self.push(MaeelType::String(
                                string.chars().nth(index).unwrap().to_string(),
                            )),

                            Ok(other) => panic!("{other} is not indexable!"),

                            _ => panic!("Nothing to index!"),
                        }?
                    }

                    "read" => {
                        let bytes = match self.pop() {
                            Ok(MaeelType::Integer(value)) => value,

                            _ => panic!(),
                        };

                        let path = match self.pop() {
                            Ok(MaeelType::String(value)) => value,

                            _ => panic!(),
                        };

                        assert!(bytes >= 0);

                        let mut buf = vec![0u8; bytes as usize]; /* empty buffer */

                        File::open(path)?.read_exact(&mut buf)?; /* file content -> buffer */

                        self.push(MaeelType::Array(
                            buf.iter()
                                .map(|byte| MaeelType::Integer(*byte as i32))
                                .collect(),
                        ))?
                    }

                    "include" => {
                        let target = match self.pop() {
                            Ok(MaeelType::String(value)) => value,

                            _ => panic!(),
                        };

                        let content = match target.as_str() {
                            "std" => include_str!("../../stdlib/std.maeel").to_string(),

                            _ => read_to_string(target).expect("Failed to include file"),
                        };

                        lex_into_tokens(&content)
                            .iter()
                            .rev()
                            .for_each(|token| tokens.push(token.clone()))
                    }

                    identifier => {
                        if let Some(value) = vars.get(identifier)
                        /* Identifier is a variable */
                        {
                            self.push(value.clone())?;

                            continue;
                        }

                        if let Some(fun) = funs.get(identifier)
                        /* Identifier is a function */
                        {
                            if fun.1
                            /* Inline function */
                            {
                                fun.0.iter().for_each(|token| tokens.push(token.clone()));
                                continue;
                            }

                            let mut fun_tokens = fun.0.clone();

                            fun_tokens.reverse();

                            self.process_tokens(&mut fun_tokens, &mut vars.clone(), funs, structs)?;

                            continue;
                        }

                        if let Some(fields) = structs.get(identifier)
                        /* Identifier is a structure */
                        {
                            /* Future structure */
                            let mut structure =
                                HashMap::<String, MaeelType>::with_capacity(fields.len());

                            /* Map each field to a value of the stack */
                            fields.iter().for_each(|key| {
                                structure.insert(key.clone(), self.pop().unwrap());
                            });

                            /* Finally, push the structure */
                            self.push(MaeelType::Structure(structure))?;

                            continue;
                        }

                        panic!()
                    }
                },
            };
        }

        Ok(())
    }

    fn push(&mut self, value: MaeelType) -> VMOutput<()> {
        let future_head = Guitar::new(value); /* Create a new node */

        if !self.head.is_null() {
            unsafe {
                /* Set head as future_head next node */
                (*future_head).next = self.head;
            }
        }

        self.head = future_head; /* Replace head with future_head */

        Ok(())
    }

    fn pop(&mut self) -> VMOutput<MaeelType> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        let current_head = unsafe { Box::from_raw(self.head) };

        /* Replace the current head with her next node */
        self.head = current_head.next;

        Ok(current_head.value)
    }

    fn fastpop(&mut self) -> VMOutput<()> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        let current_head = unsafe { Box::from_raw(self.head) };

        /* Replace the current head with her next node */
        self.head = current_head.next;

        Ok(())
    }

    fn peek(&self) -> VMOutput<&MaeelType> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        Ok(unsafe { &(*self.head).value })
    }

    fn clear(&mut self) -> VMOutput<()> {
        while !self.head.is_null()
        /* Dropping all the next_node until head is not null */
        {
            self.head = unsafe { Box::from_raw(self.head) }.next
        }

        Ok(())
    }

    fn swap(&mut self) -> VMOutput<()> {
        if self.head.is_null() || unsafe { (*self.head).next.is_null() }
        /* Making sure the stack contains at least two values */
        {
            return Err("Not enough elements on the stack".into());
        }

        unsafe {
            ptr::swap(&mut (*self.head).value, &mut (*(*self.head).next).value);
        }

        Ok(())
    }

    fn dup(&mut self) -> VMOutput<()> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        self.push(unsafe { (*self.head).value.clone() })
    }

    fn over(&mut self) -> VMOutput<()> {
        if self.head.is_null() || unsafe { (*self.head).next.is_null() }
        /* Making sure the stack contains at least two values */
        {
            return Err("Stack has less than two elements".into());
        }

        self.push(
            /* Get the value under the stack top */
            unsafe { (*(*self.head).next).value.clone() },
        )
    }

    fn rot(&mut self) -> VMOutput<()> {
        if self.head.is_null()
            || unsafe { (*self.head).next.is_null() }
            || unsafe { (*(*self.head).next).next.is_null() }
        /* Making sure the stack contains at least three values */
        {
            return Err("Stack has less than three elements".into());
        }

        unsafe {
            let node1 /* top node */ = &mut *self.head;
            let node2 /* mid node */ = &mut *(*self.head).next;
            let node3 /* bot node */ = &mut *(*(*self.head).next).next;

            /* Store the node1 value in a temp variable,
            as we update its value first */
            let temp = ptr::read(&node1.value);

            ptr::swap /* V(top) <- V(mid) */ (&mut node1.value, &mut node2.value);
            ptr::swap /* V(mid) <- V(bot) */ (&mut node2.value, &mut node3.value);
            ptr::write /* V(bot) <- V(top) */ (&mut node3.value, temp);
        }

        Ok(())
    }
}

impl Clone for MaeelType {
    fn clone(&self) -> Self {
        match self {
            Self::Float(a) => Self::Float(*a),
            Self::Integer(a) => Self::Integer(*a),
            Self::String(a) => Self::String(a.clone()),
            Self::Array(a) => Self::Array(a.clone()),
            Self::Function(a) => Self::Function(a.clone()),
            Self::Structure(a) => Self::Structure(a.clone()),
        }
    }
}

impl std::fmt::Display for MaeelType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(x) => write!(f, "{}", x),
            Self::Function(_) => write!(f, "Function"),
            Self::Float(x) => write!(f, "{}", x),
            Self::Integer(x) => write!(f, "{}", x),
            Self::Array(xs) => {
                write!(f, "{{")?;

                xs.iter().enumerate().for_each(|(i, x)| {
                    if i > 0 {
                        write!(f, " ").unwrap();
                    }

                    write!(f, "{}", x).unwrap();
                });

                write!(f, "}}")
            }

            Self::Structure(x) => {
                write!(f, "{{")?;

                x.iter().enumerate().for_each(|(i, (k, v))| {
                    if i > 0 {
                        write!(f, " ").unwrap();
                    }

                    write!(f, "{}: {}", k, v).unwrap();
                });

                write!(f, "}}")
            }
        }
    }
}

impl PartialOrd for MaeelType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => Some(a.cmp(b)),
            (Self::Float(a), Self::Float(b)) => Some(a.total_cmp(b)),
            (Self::Integer(a), Self::Float(b)) | (Self::Float(b), Self::Integer(a)) => {
                Some(b.total_cmp(&(*a as f32)))
            }

            (a, b) => panic!("Cannot compare {a} and {b}"),
        }
    }
}

impl PartialEq for MaeelType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Array(a), Self::Array(b)) => a == b,
            (Self::Integer(a), Self::Float(b)) | (Self::Float(b), Self::Integer(a)) => {
                (*a as f32) == *b
            }
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a == b,

            _ => false,
        }
    }
}

impl std::ops::Sub for MaeelType {
    type Output = MaeelType;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(m), Self::Integer(n)) => Self::Integer(m - n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x - y),
            (Self::Float(x), Self::Integer(m)) | (Self::Integer(m), Self::Float(x)) => {
                Self::Float(m as f32 - x)
            }

            (a, b) => panic!("Cannot substract {a} and {b}"),
        }
    }
}

impl std::ops::Mul for MaeelType {
    type Output = MaeelType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(m), Self::Integer(n)) => Self::Integer(m * n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x * y),
            (Self::Float(x), Self::Integer(m)) | (Self::Integer(m), Self::Float(x)) => {
                Self::Float(x * m as f32)
            }
            (Self::Integer(m), Self::String(s)) | (Self::String(s), Self::Integer(m)) => {
                Self::String(s.repeat(m as usize))
            }

            (a, b) => panic!("Cannot multiply {a} and {b}"),
        }
    }
}

impl std::ops::Add for MaeelType {
    type Output = MaeelType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::String(a), Self::String(b)) => Self::String(a + &b),
            (Self::Integer(m), Self::Integer(n)) => Self::Integer(m + n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x + y),
            (Self::Integer(m), Self::Float(x)) | (Self::Float(x), Self::Integer(m)) => {
                Self::Float(m as f32 + x)
            }
            (other, Self::Array(mut xs)) | (Self::Array(mut xs), other) => {
                xs.push(other);
                Self::Array(xs)
            }

            (a, b) => panic!("Cannot add {a} and {b}"),
        }
    }
}

impl std::ops::Rem for MaeelType {
    type Output = MaeelType;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(m), Self::Integer(n)) => Self::Integer(m % n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x % y),
            (Self::Integer(m), Self::Float(x)) => Self::Float(m as f32 % x),
            (Self::Float(x), Self::Integer(m)) => Self::Float(x % m as f32),

            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}

impl std::ops::Div for MaeelType {
    type Output = MaeelType;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(m), Self::Integer(n)) => Self::Float(m as f32 / n as f32),
            (Self::Float(x), Self::Float(y)) => Self::Float(x / y),
            (Self::Integer(m), Self::Float(x)) => Self::Float(m as f32 / x),
            (Self::Float(x), Self::Integer(m)) => Self::Float(x / m as f32),

            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}
