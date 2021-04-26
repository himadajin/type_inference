use crate::token::{Token, TokenKind};
use std::str::Chars;

#[derive(Debug)]
pub struct Lexer<'a> {
    src: Chars<'a>,
    terminated: bool,
    ch: char,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &str) -> Lexer {
        let mut lexer = Lexer {
            src: src.chars(),
            terminated: false,
            ch: ' ',
        };

        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.terminated {
            return Token::new(TokenKind::EOF, self.ch);
        }

        let token = match self.ch {
            '<' => Token::new(TokenKind::Lt, self.ch),

            '>' => Token::new(TokenKind::Gt, self.ch),

            '-' => match self.peek_char() {
                '>' => {
                    self.read_char();
                    Token::new(TokenKind::RArrow, "->")
                }
                _ => unimplemented!("not implemented '-' token"),
            },

            '+' => Token::new(TokenKind::Plus, self.ch),
            '*' => Token::new(TokenKind::Star, self.ch),
            '(' => Token::new(TokenKind::OpenParen, self.ch),
            ')' => Token::new(TokenKind::CloseParen, self.ch),

            '0'..='9' => return Token::new(TokenKind::Num, self.read_number().unwrap()),

            _ => match self.read_str() {
                Some(word) => {
                    return match word.as_str() {
                        "fun" => Token::new(TokenKind::Fun, word),
                        _ => Token::new(TokenKind::Identifier, word),
                    }
                }
                None => panic!("cannot tokenize"),
            },
        };

        self.read_char();
        return token;
    }

    pub fn read_char(&mut self) -> char {
        if self.terminated {
            return self.ch;
        }

        match self.src.next() {
            Some(c) => {
                self.ch = c;
            }
            None => {
                self.ch = '\0';
                self.terminated = true;
            }
        }

        self.ch
    }

    pub fn read_number(&mut self) -> Option<String> {
        if self.terminated || !self.ch.is_digit(10) {
            return None;
        }

        let mut num_str = String::from(self.ch);
        loop {
            let c = self.read_char();
            if c.is_digit(10) {
                num_str.push(c);
            } else {
                break;
            }
        }

        Some(num_str)
    }

    pub fn read_str(&mut self) -> Option<String> {
        let is_letter = |c: char| c.is_ascii_alphanumeric() || c == '_';

        if self.terminated || !is_letter(self.ch) {
            return None;
        }

        let mut word = String::from(self.ch);
        loop {
            let c = self.read_char();
            if is_letter(c) {
                word.push(c);
            } else {
                break;
            }
        }

        Some(word)
    }

    pub fn peek_char(&self) -> char {
        let mut chars = self.src.clone();

        match chars.next() {
            Some(c) => c,
            None => '\0',
        }
    }

    pub fn skip_whitespace(&mut self) {
        while !self.terminated && self.ch.is_whitespace() {
            self.read_char();
        }
    }
}
