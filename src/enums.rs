#[derive(Debug)]
pub enum Token {
    Separator,
    String(String, u16),
    Integer(isize, u16),
    Float(f64, u16),
    Identifier(String, u16),
}

#[derive(Debug)]
pub enum VMType {
    Integer(isize),
    Float(f64),
    String(String),
    Array(Vec<VMType>),
}
