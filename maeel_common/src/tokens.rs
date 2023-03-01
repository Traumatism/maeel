#[derive(Clone, Debug, PartialEq)]
pub struct TokenData {
    pub token: Token,
    pub line: u16,
}

impl TokenData {
    pub fn new(token: Token, line: u16) -> Self {
        Self { token, line }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Block(Vec<TokenData>),
    Str(String),
    Integer(i64),
    Identifier(String),
    Float(f64),
    Bool(bool),
    Sub,
    Add,
    Mul,
    Mod,
    Div,
    Not,
    Eq,
    Gt,
    Lt,
    Clear,
    Over,
    Take,
    Swap,
    Rot,
    Dup,
    Pop,
    Let,
    ProcStart,
    Return,
    BlockStart,
    BlockEnd,
    Include,
    If,
    For,
    While,
}