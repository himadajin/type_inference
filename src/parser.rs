use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub expr);

#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Op};
    use crate::parser::expr;

    fn parse(input: &str) -> Expr {
        expr::ExprParser::new().parse(input).unwrap()
    }

    #[test]
    fn parse_num() {
        assert_eq!(Expr::Num(1), parse("1"));
        assert_eq!(Expr::Num(1), parse("(1)"));
        assert_eq!(Expr::Num(1), parse("((1))"));
    }

    #[test]
    fn parse_bool() {
        assert_eq!(Expr::Bool(true), parse("true"));
        assert_eq!(Expr::Bool(false), parse("false"));
    }

    #[test]
    fn parse_val() {
        assert_eq!(Expr::Val("x".to_string()), parse("x"));
        assert_eq!(Expr::Val("hoge".to_string()), parse("hoge"));
    }

    #[test]
    fn parse_and() {
        assert_eq!(
            Expr::BinOp(
                Box::new(Expr::Bool(true)),
                Op::And,
                Box::new(Expr::Bool(false)),
            ),
            parse("true && false")
        );
    }

    #[test]
    fn parse_or() {
        assert_eq!(
            Expr::BinOp(
                Box::new(Expr::Bool(true)),
                Op::Or,
                Box::new(Expr::Bool(false)),
            ),
            parse("true || false")
        );
    }

    #[test]
    fn parse_relational() {
        assert_eq!(
            Expr::BinOp(Box::new(Expr::Num(1)), Op::Lt, Box::new(Expr::Num(2)),),
            parse("1 < 2")
        );

        assert_eq!(
            Expr::BinOp(Box::new(Expr::Num(1)), Op::Gt, Box::new(Expr::Num(2)),),
            parse("1 > 2")
        );
    }

    #[test]
    fn parse_logical_operation() {
        assert_eq!(
            Expr::BinOp(
                Box::new(Expr::Bool(true)),
                Op::Or,
                Box::new(Expr::BinOp(
                    Box::new(Expr::Bool(false)),
                    Op::And,
                    Box::new(Expr::Bool(true)),
                )),
            ),
            parse("true || false && true"),
        );

        assert_eq!(
            Expr::BinOp(
                Box::new(Expr::BinOp(
                    Box::new(Expr::Bool(true)),
                    Op::Or,
                    Box::new(Expr::Bool(false)),
                )),
                Op::And,
                Box::new(Expr::Bool(true)),
            ),
            parse("(true || false) && true"),
        );

        assert_eq!(
            Expr::BinOp(
                Box::new(Expr::BinOp(
                    Box::new(Expr::Num(1)),
                    Op::Lt,
                    Box::new(Expr::Num(2)),
                )),
                Op::And,
                Box::new(Expr::BinOp(
                    Box::new(Expr::Num(3)),
                    Op::Lt,
                    Box::new(Expr::Num(4)),
                )),
            ),
            parse("1 < 2 && 3 < 4")
        );

        assert_eq!(
            Expr::BinOp(
                Box::new(Expr::BinOp(
                    Box::new(Expr::Num(1)),
                    Op::Add,
                    Box::new(Expr::Num(2)),
                )),
                Op::Lt,
                Box::new(Expr::BinOp(
                    Box::new(Expr::Num(3)),
                    Op::Add,
                    Box::new(Expr::Num(4)),
                )),
            ),
            parse("1 + 2 < 3 + 4")
        )
    }

    #[test]
    fn parse_add() {
        assert_eq!(
            Expr::BinOp(Box::new(Expr::Num(1)), Op::Add, Box::new(Expr::Num(2)),),
            parse("1 + 2")
        );
    }

    #[test]
    fn parse_mul() {
        assert_eq!(
            Expr::BinOp(Box::new(Expr::Num(1)), Op::Mul, Box::new(Expr::Num(2)),),
            parse("1 * 2")
        );
    }

    #[test]
    fn parse_fun() {
        assert_eq!(
            Expr::Fun("x".to_string(), Box::new(Expr::Val("x".to_string()))),
            parse("(fun x -> x)")
        );

        assert_eq!(
            Expr::Fun(
                "x".to_string(),
                Box::new(Expr::Fun(
                    "y".to_string(),
                    Box::new(Expr::BinOp(
                        Box::new(Expr::Val("x".to_string())),
                        Op::Add,
                        Box::new(Expr::Val("y".to_string()))
                    ))
                ))
            ),
            parse("(fun x -> (fun y -> x + y))")
        );
    }

    #[test]
    fn parse_app() {
        assert_eq!(
            Expr::App(
                Box::new(Expr::Fun(
                    "x".to_string(),
                    Box::new(Expr::Val("x".to_string()))
                )),
                Box::new(Expr::Num(1))
            ),
            parse("(fun x -> x) 1")
        );

        assert_eq!(
            Expr::BinOp(
                Box::new(Expr::App(
                    Box::new(Expr::Fun(
                        "x".to_string(),
                        Box::new(Expr::Val("x".to_string()))
                    )),
                    Box::new(Expr::Val("x".to_string()))
                )),
                Op::Add,
                Box::new(Expr::Num(1))
            ),
            parse("(fun x -> x) x + 1")
        )
    }
}
