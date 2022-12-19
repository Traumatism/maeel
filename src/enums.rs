#[derive(Debug)]
pub enum Token {
    Separator,
    String(String),
    Integer(isize),
    Float(f64),
    Identifier(String),
}

#[derive(Debug)]
pub enum VMType {
    Integer(isize),
    Float(f64),
    String(String),
    Array(Vec<VMType>),
}
