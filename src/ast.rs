use std::{collections::HashSet, convert::From, fmt};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Op {
    Add,
    Mul,
    Gt,
    Lt,
    And,
    Or,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Mul => write!(f, "*"),
            Op::Gt => write!(f, ">"),
            Op::Lt => write!(f, "<"),
            Op::And => write!(f, "&&"),
            Op::Or => write!(f, "||"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Num,
    Bool,
    Fun(Box<Type>, Box<Type>),
    T(String),
}

impl<'a> From<&'a AExpr> for &'a Type {
    fn from(item: &'a AExpr) -> Self {
        match item {
            AExpr::Num(_, ty) => ty,
            AExpr::Bool(_, ty) => ty,
            AExpr::Val(_, ty) => ty,
            AExpr::BinOp(_, _, _, ty) => ty,
            AExpr::Fun(_, _, ty) => ty,
            AExpr::App(_, _, ty) => ty,
        }
    }
}

impl From<AExpr> for Type {
    fn from(item: AExpr) -> Self {
        match item {
            AExpr::Num(_, ty) => ty,
            AExpr::Bool(_, ty) => ty,
            AExpr::Val(_, ty) => ty,
            AExpr::BinOp(_, _, _, ty) => ty,
            AExpr::Fun(_, _, ty) => ty,
            AExpr::App(_, _, ty) => ty,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Num(i32),
    Bool(bool),
    Val(String),
    BinOp(Box<Expr>, Op, Box<Expr>),
    Fun(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Num(n) => write!(f, "{}", n),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Val(id) => write!(f, "{}", id),
            Expr::BinOp(lhs, op, rhs) => write!(f, "({} {} {})", lhs.as_ref(), op, rhs.as_ref()),
            Expr::Fun(arg, expr) => write!(f, "fun {} -> {}", arg, expr.as_ref()),
            Expr::App(fun, arg) => write!(f, "(({}) {})", fun, arg),
        }
    }
}

pub fn collect_ids(ids: &mut HashSet<String>, expr: &Expr) {
    match expr {
        Expr::Val(id) => {
            ids.insert(id.clone());
        }
        Expr::BinOp(lhs, _, rhs) => {
            collect_ids(ids, lhs);
            collect_ids(ids, rhs);
        }
        Expr::Fun(id, expr) => {
            ids.insert(id.clone());
            collect_ids(ids, expr);
        }
        Expr::App(fun, arg) => {
            collect_ids(ids, fun);
            collect_ids(ids, arg);
        }
        _ => (),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AExpr {
    Num(i32, Type),
    Bool(bool, Type),
    Val(String, Type),
    BinOp(Box<AExpr>, Op, Box<AExpr>, Type),
    Fun(String, Box<AExpr>, Type),
    App(Box<AExpr>, Box<AExpr>, Type),
}
