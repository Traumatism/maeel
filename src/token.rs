pub type Block = Vec<Token>;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Str(String, u16),
    Integer(i64, u16),
    Identifier(String, u16),
    Float(f64, u16),
    Bool(bool, u16),

    Sub(u16),
    Add(u16),
    Mul(u16),
    Modulo(u16),
    Div(u16),
    DivQ(u16),

    And(u16),
    Or(u16),
    Xor(u16),
    Not(u16),

    Eq(u16),
    Gt(u16),
    Lt(u16),

    Rotate(u16),
    Clear(u16),
    Over(u16),
    Take(u16),
    Swap(u16),
    Del(u16),
    Dup(u16),
    Pop(u16),
    Len(u16),

    Let(u16),
    ProcStart(u16),
    Return(u16),

    BlockStart(u16),
    Block(Block, u16),
    BlockEnd(u16),

    If(u16),
    For(u16),
    While(u16),
}
