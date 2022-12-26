/// All tokens lexer/parser can understand
#[derive(Debug, Clone)]
pub enum Token {
    Str(String, u16),        // "hello world"
    Integer(i64, u16),       // 123
    Identifier(String, u16), // helloworld
    Float(f64, u16),         // 123.4
    Bool(bool, u16),         // true|false
    Let(u16),                // let
    If(u16),                 // if
    Add(u16),                // +
    Sub(u16),                // -
    Mul(u16),                // *
    And(u16),                // &
    Or(u16),                 // |
    Xor(u16),                // ^
    Not(u16),                // !
    Eq(u16),                 // =
    Del(u16),                // del
    Take(u16),               // take
    Return(u16),             // return
    Reverse(u16),            // reverse
    Clear,                   // clear
    Dup,                     // dup
    Swap,                    // dup
    Pop,                     // dup
    Separator,               // end
    ProcStart,               // proc
    ProcEnd,                 // proc_end
}
