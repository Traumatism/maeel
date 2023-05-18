#[derive(Clone, Debug, PartialEq)]
pub enum Token
{
    Block(Vec<Token>),
    Str(String),
    Identifier(String),
    Integer(i64),
    Float(f64),
    Bool(bool),

    Call,

    Add,
    Sub,
    Mul,
    Mod,
    Div,

    Eq,
    Gt,
    Lt,

    Get,
    Clear,

    Let,
    ProcStart,

    ArrayStart,
    ArrayEnd,

    BlockStart,
    BlockEnd,

    IStart,
    IEnd,

    If,
    For,
    While,
}
