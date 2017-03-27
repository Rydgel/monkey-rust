#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Illegal,
    EOF,
    // identifier and literals
    Ident(String),
    StringLiteral(String),
    IntLiteral(usize),
    // operators
    Assign,
    Plus,
    // reserved words
    Function,
    Let,
    // punctuations
    Comma,
    SemiColon,
    LParen,
    RParen,
    LBrace,
    RBrace
}
