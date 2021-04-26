use std::collections::HashSet;
use type_inference::{
    ast::{collect_ids, Expr, Op},
    infer::{annotate, collect_aexpr, Environment},
};

fn main() {
    let mut ids = HashSet::new();

    let expr = Expr::Fun(
        String::from("x"),
        Box::new(Expr::BinOp(
            Box::new(Expr::Val(String::from("x"))),
            Op::Add,
            Box::new(Expr::Num(1)),
        )),
    );

    collect_ids(&mut ids, &expr);

    let mut env = Environment::new(ids);
    let aexpr = annotate(&expr, &mut env);

    let mut constraints = Vec::new();
    collect_aexpr(&mut constraints, &aexpr);

    println!("{}", expr);
    println!("{:?}", aexpr);
    println!("{:?}", constraints);
}
