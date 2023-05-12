pub type Procedure<'a> = (&'a str, Vec<Token>);

#[derive(Clone, Debug, PartialEq)]

pub enum Token
{
    Block(Vec<Token>),
    Str(String),
    Integer(i64),
    Identifier(String),
    Float(f64),
    Bool(bool),

    Call,

    Add,
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
