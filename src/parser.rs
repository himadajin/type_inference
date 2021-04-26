use std::u32;

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

    fn expect(&mut self, kind: TokenKind) -> Result<(), String> {
        if self.cur.kind != kind {
            return Err(format!("expect {:?}, but got {:?}", kind, self.cur.kind));
        }

        self.next_token();
        Ok(())
    }

    fn expect_number(&mut self) -> Result<u32, String> {
        if self.cur.kind != TokenKind::Num {
            return Err(format!(
                "expect {:?}, but got {:?}",
                TokenKind::Num,
                self.cur.kind
            ));
        }

        let num: u32 = self.cur.literal.parse().unwrap();
        self.next_token();

        return Ok(num);
    }

    fn expect_ident(&mut self) -> Result<String, String> {
        if self.cur.kind != TokenKind::Identifier {
            return Err(format!(
                "expect {:?}, but got {:?}",
                TokenKind::Identifier,
                self.cur.kind
            ));
        }

        let ident = self.cur.literal.clone();
        self.next_token();

        return Ok(ident);
    }

    pub fn expr(&mut self) -> Result<Expr, String> {
        let fun = self.add()?;
        match self.peek_kind() {
            TokenKind::EOF => Ok(fun),
            _ => {
                let arg = self.expr()?;
                Ok(Expr::App(Box::new(fun), Box::new(arg)))
            }
        }
    }

    pub fn add(&mut self) -> Result<Expr, String> {
        let left = self.mul()?;

        if self.consume(TokenKind::Plus) {
            let right = self.add()?;
            return Ok(Expr::BinOp(Box::new(left), Op::Add, Box::new(right)));
        }

        return Ok(left);
    }

    pub fn mul(&mut self) -> Result<Expr, String> {
        let left = self.primary()?;

        if self.consume(TokenKind::Star) {
            let right = self.mul()?;
            return Ok(Expr::BinOp(Box::new(left), Op::Mul, Box::new(right)));
        }

        return Ok(left);
    }

    pub fn primary(&mut self) -> Result<Expr, String> {
        if self.consume(TokenKind::OpenParen) {
            if self.consume(TokenKind::Fun) {
                let id = self.expect_ident()?;
                self.expect(TokenKind::RArrow)?;
                let expr = self.expr()?;
                self.expect(TokenKind::CloseParen)?;

                return Ok(Expr::Fun(id, Box::new(expr)));
            }

            let expr = self.expr();
            self.expect(TokenKind::CloseParen)?;
            return expr;
        }

        match self.peek_kind() {
            TokenKind::Num => Ok(Expr::Num(self.expect_number()?)),
            TokenKind::True => Ok(Expr::Bool(true)),
            TokenKind::False => Ok(Expr::Bool(false)),
            TokenKind::Identifier => Ok(Expr::Val(self.expect_ident()?)),
            _ => Err(format!("Unexpected token :{:?}", self.peek_kind())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use crate::lexer::*;

    fn test_expr(src: &str, expect: Expr) {
        match Parser::new(Lexer::new(src)).expr() {
            Ok(res) => assert_eq!(expect, res),
            Err(msg) => panic!(msg),
        }
    }

    #[test]
    fn parse_num() {
        test_expr("0", Expr::Num(0));
        test_expr("1", Expr::Num(1));
        test_expr("23", Expr::Num(23));
        test_expr("(0)", Expr::Num(0));
    }

    #[test]
    fn parse_val() {
        test_expr("x", Expr::Val(String::from("x")));
        test_expr("(x)", Expr::Val(String::from("x")));
    }

    #[test]
    fn parse_binary() {
        test_expr(
            "1 + 2",
            Expr::BinOp(Box::new(Expr::Num(1)), Op::Add, Box::new(Expr::Num(2))),
        );

        test_expr(
            "1 * 2",
            Expr::BinOp(Box::new(Expr::Num(1)), Op::Mul, Box::new(Expr::Num(2))),
        );

        test_expr(
            "1 + 2 * 3",
            Expr::BinOp(
                Box::new(Expr::Num(1)),
                Op::Add,
                Box::new(Expr::BinOp(
                    Box::new(Expr::Num(2)),
                    Op::Mul,
                    Box::new(Expr::Num(3)),
                )),
            ),
        );

        test_expr(
            "2 * (3 + 4)",
            Expr::BinOp(
                Box::new(Expr::Num(2)),
                Op::Mul,
                Box::new(Expr::BinOp(
                    Box::new(Expr::Num(3)),
                    Op::Add,
                    Box::new(Expr::Num(4)),
                )),
            ),
        );
    }

    #[test]
    fn parse_fun() {
        test_expr(
            "(fun x -> x)",
            Expr::Fun(String::from("x"), Box::new(Expr::Val(String::from("x")))),
        );

        test_expr(
            "(fun x -> x + 1)",
            Expr::Fun(
                String::from("x"),
                Box::new(Expr::BinOp(
                    Box::new(Expr::Val(String::from("x"))),
                    Op::Add,
                    Box::new(Expr::Num(1)),
                )),
            ),
        );

        test_expr(
            "(fun x -> (fun y -> x + y))",
            Expr::Fun(
                String::from("x"),
                Box::new(Expr::Fun(
                    String::from("y"),
                    Box::new(Expr::BinOp(
                        Box::new(Expr::Val(String::from("x"))),
                        Op::Add,
                        Box::new(Expr::Val(String::from("y"))),
                    )),
                )),
            ),
        );
    }

    // #[test]
    // fn parse_app() {
    //     test_expr(
    //         "(fun x -> x) 0",
    //         Expr::App(
    //             Box::new(Expr::Fun(
    //                 String::from("x"),
    //                 Box::new(Expr::Val(String::from("x"))),
    //             )),
    //             Box::new(Expr::Num(0)),
    //         ),
    //     );

    //     test_expr(
    //         "(fun x -> (fun y -> x + y)) 1 2",
    //         Expr::App(
    //             Box::new(Expr::App(
    //                 Box::new(Expr::Fun(
    //                     String::from("x"),
    //                     Box::new(Expr::Fun(
    //                         String::from("y"),
    //                         Box::new(Expr::BinOp(
    //                             Box::new(Expr::Val(String::from("x"))),
    //                             Op::Add,
    //                             Box::new(Expr::Val(String::from("y"))),
    //                         )),
    //                     )),
    //                 )),
    //                 Box::new(Expr::Num(1)),
    //             )),
    //             Box::new(Expr::Num(1)),
    //         ),
    //     );
    // }
}
