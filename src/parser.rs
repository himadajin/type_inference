use crate::ast::{Expr, Op};
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer) -> Parser {
        let cur = lexer.next_token();

        Parser {
            lexer: lexer,
            cur: cur,
        }
    }

    fn next_token(&mut self) {
        self.cur = self.lexer.next_token();
    }

    fn consume(&mut self, kind: TokenKind) -> bool {
        if self.cur.kind == kind {
            self.next_token();
            return true;
        }

        return false;
    }

    fn peek_kind(&self) -> TokenKind {
        self.cur.kind
    }

    fn expect(&mut self, kind: TokenKind) {
        if self.cur.kind != kind {
            panic!("expect {:?}, but got {:?}", kind, self.cur.kind)
        }

        self.next_token();
    }

    fn expect_number(&mut self) -> u32 {
        if self.cur.kind != TokenKind::Num {
            panic!("expect {:?}, but got {:?}", TokenKind::Num, self.cur.kind);
        }

        let num = self.cur.literal.parse().unwrap();
        self.next_token();

        return num;
    }

    fn expect_ident(&mut self) -> String {
        if self.cur.kind != TokenKind::Identifier {
            panic!(
                "expect {:?}, but got {:?}",
                TokenKind::Identifier,
                self.cur.kind
            );
        }

        let ident = self.cur.literal.clone();
        self.next_token();

        return ident;
    }

    pub fn expr(&mut self) -> Expr {
        self.add()
    }

    pub fn add(&mut self) -> Expr {
        let left = self.mul();

        if self.consume(TokenKind::Plus) {
            let right = self.add();
            return Expr::BinOp(Box::new(left), Op::Add, Box::new(right));
        }

        return left;
    }

    pub fn mul(&mut self) -> Expr {
        let left = self.primary();

        if self.consume(TokenKind::Star) {
            let right = self.mul();
            return Expr::BinOp(Box::new(left), Op::Mul, Box::new(right));
        }

        return left;
    }

    pub fn primary(&mut self) -> Expr {
        if self.consume(TokenKind::OpenParen) {
            if self.consume(TokenKind::Fun) {
                let id = self.expect_ident();
                self.expect(TokenKind::RArrow);
                let expr = self.expr();
                self.expect(TokenKind::CloseParen);

                return Expr::Fun(id, Box::new(expr));
            }

            let expr = self.expr();
            self.expect(TokenKind::CloseParen);
            return expr;
        }

        match self.peek_kind() {
            TokenKind::Num => Expr::Num(self.expect_number()),
            TokenKind::True => Expr::Bool(true),
            TokenKind::False => Expr::Bool(false),
            TokenKind::Identifier => Expr::Val(self.expect_ident()),
            _ => {
                let fun = self.expr();
                let arg = self.expr();
                Expr::App(Box::new(fun), Box::new(arg))
            }
        }
    }
}