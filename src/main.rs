use type_inference::{
    ast::{Expr, Op, Type},
    infer::{infer, Environment},
};

fn main() {
    let expr = Expr::App(
        Box::new(Expr::Fun(
            String::from("x"),
            Box::new(Expr::BinOp(
                Box::new(Expr::Val(String::from("x"))),
                Op::Add,
                Box::new(Expr::Num(1)),
            )),
        )),
        Box::new(Expr::Num(0)),
    );

    println!("{}", expr);

    let aexpr = infer(Environment::new(), expr);
    println!("{:?}", aexpr);
    println!("{}", Type::from(aexpr));
}
