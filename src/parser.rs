use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub expr);

#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Op};
    use crate::parser::expr;

    fn parse(input: &str) -> Box<Expr> {
        expr::ExprParser::new().parse(input).unwrap()
    }

    #[test]
    fn parse_num() {
        assert_eq!(Box::new(Expr::Num(1)), parse("1"));
        assert_eq!(Box::new(Expr::Num(1)), parse("(1)"));
        assert_eq!(Box::new(Expr::Num(1)), parse("((1))"));
    }

    #[test]
    fn parse_bool() {
        assert_eq!(Box::new(Expr::Bool(true)), parse("true"));
        assert_eq!(Box::new(Expr::Bool(false)), parse("false"));
    }

    #[test]
    fn parse_add() {
        assert_eq!(
            Box::new(Expr::BinOp(
                Box::new(Expr::Num(1)),
                Op::Add,
                Box::new(Expr::Num(2)),
            )),
            parse("1+2")
        );
    }
}
