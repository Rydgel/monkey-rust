#[derive(PartialEq, Debug)]
enum Token {
    Illegal,
    EOF,
    // identifier and literals
    Ident(String),
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
