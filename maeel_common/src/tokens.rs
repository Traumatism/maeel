pub type Procedure<'a> = (&'a str, Vec<Token>);

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Block(Vec<Token>),
    Str(String),
    Integer(i64),
    Identifier(String),
    Float(f64),
    Bool(bool),

    Add,
    Mul,
    Mod,
    Div,

    Not,
    Eq,
    Gt,
    Lt,

    Take,
    Get,

    Clear,
    Swap,
    Over,
    Rot,
    Dup,
    Pop,

    Let,

    ProcStart,

    ArrayStart,
    ArrayEnd,

    IStart,
    IEnd,

    BlockStart,
    BlockEnd,

    If,
    For,
    While,
}
