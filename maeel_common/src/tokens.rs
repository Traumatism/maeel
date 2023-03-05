#[derive(Clone, Debug, PartialEq)]
pub struct TokenData {
    pub token: Token,
    pub line: u16,
    pub pos: u16,
}

impl TokenData {
    pub fn new(token: Token, line: u16, pos: u16) -> Self {
        Self { token, line, pos }
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
    Get,
    Swap,
    Rot,
    Dup,
    Pop,
    Let,
    ProcStart,
    BlockStart,
    BlockEnd,
    Include,
    If,
    For,
    While,
}
