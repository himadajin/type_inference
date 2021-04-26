#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Lt,
    Gt,

    Num,
    Plus,
    Star,
    OpenParen,
    CloseParen,

    Fun,
    RArrow,

    Identifier,

    EOF,
    Unknown,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
}

impl Token {
    pub fn new<T: Into<String>>(kind: TokenKind, literal: T) -> Token {
        Token {
            kind: kind,
            literal: literal.into(),
        }
    }
}
