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
    fn parse_add() {
        assert_eq!(
            Expr::BinOp(Box::new(Expr::Num(1)), Op::Add, Box::new(Expr::Num(2)),),
            parse("1+2")
        );
    }

    #[test]
    fn parse_mul() {
        assert_eq!(
            Expr::BinOp(Box::new(Expr::Num(1)), Op::Mul, Box::new(Expr::Num(2)),),
            parse("1*2")
        );
    }
}
