use std::env::args;

use type_inference::infer::{infer, Environment};
use type_inference::parser::expr;

fn main() {
    let arg = args().nth(1).unwrap();
    let expr = expr::ExprParser::new().parse(arg.as_str()).unwrap();
    let result = infer(Environment::new(), expr);
    println!("{}", result.to_pretty(8));
}
