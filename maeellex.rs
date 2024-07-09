
/* Assign names to literals */
#[macro_export]
macro_rules! alias {
    ($($name:ident, $value:expr),* $(,)?) => { $(#[macro_export] macro_rules! $name {() => { $value }} )* }
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
    M_FORCE_DEF,     '§',
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

pub type M_INT_SIZE       /* Encode maeel integers on 32 bits        */   = i32;
pub type M_FLOAT_SIZE     /* Encode maeel floats on 32 bits          */   = f32;
pub type Stack<T>         /* Specify that a vec is used like a stack */   = Vec<T>;
pub type TokenData        /* Token and its file name, line           */   = (Token, String, u16);
pub type FunData          /* Fun tokens and inline descriptor        */   = (std::rc::Rc<[TokenData]>, bool);
pub type Mapper<T>        /* Map values of type T with their names   */   = std::collections::HashMap<String, T>;

/* Tokens used by the lexer */
#[derive(Clone, Debug)]
pub enum Token {
    Block(Vec<TokenData>),
    Str(String),
    Name(String),
    Int(M_INT_SIZE),
    Flt(M_FLOAT_SIZE),
    Sym(char),
}

/* Used for error messages */
#[derive(Debug)] pub enum TokenRepr { Block, Str, Name, Int, Flt, Sym }

#[macro_export]
macro_rules! emit_error {
    ($fl:expr, $line:expr, $message:expr) => {{
        println!("\n{}:{} {}", $fl, $line, $message);
        std::process::exit(1)
    }};
}

#[macro_export]
macro_rules! take_with_predicate {
    ($char:expr, $chars:expr, $p:expr) => {{
        let content = std::iter::once($char)
            .chain($chars.clone().take_while($p))
            .collect::<String>();
        for _ in (1..content.len()) { $chars.next(); }
        content
    }};
}


pub fn lex_into_tokens(code: &str, file: &str) -> Stack<TokenData> {
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

    for token in lex_into_tokens(
        &std::fs::read_to_string(&file).unwrap(), &file
    ) {
        println!("{:?}", token)
    }
}
