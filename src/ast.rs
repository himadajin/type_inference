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
    Fun { arg: Box<Type>, ret: Box<Type> },

    TyVar(TyId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyId(pub u32);

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Num => write!(f, "num"),
            Type::Bool => write!(f, "bool"),
            Type::Fun { arg, ret } => write!(f, "{}->{}", arg.as_ref(), ret.as_ref()),
            Type::TyVar(s) => write!(f, "a{}", s.0),
        }
    }
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
    Num(u32),
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
    Num(u32, Type),
    Bool(bool, Type),
    Val(String, Type),
    BinOp(Box<AExpr>, Op, Box<AExpr>, Type),
    Fun(String, Box<AExpr>, Type),
    App(Box<AExpr>, Box<AExpr>, Type),
}

impl fmt::Display for AExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AExpr::Num(n, t) => write!(f, "{}:{}", n, t),
            AExpr::Bool(b, t) => write!(f, "{}:{}", b, t),
            AExpr::Val(id, t) => write!(f, "{}:{}", id, t),
            AExpr::BinOp(lhs, op, rhs, t) => {
                write!(f, "({} {} {} ):{}", lhs.as_ref(), op, rhs.as_ref(), t)
            }
            AExpr::Fun(arg, expr, t) => match t {
                Type::Fun {
                    arg: argt,
                    ret: rest,
                } => {
                    write!(f, "(fun {}:{} -> {} ):{}", arg, argt, expr.as_ref(), rest)
                }
                _ => panic!("not a function"),
            },

            AExpr::App(fun, arg, t) => write!(f, "(({} ) {} ):{}", fun, arg, t),
        }
    }
}
