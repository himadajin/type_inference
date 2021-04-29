use core::panic;
use std::collections::{HashMap, HashSet};

use crate::ast::{collect_ids, AExpr, Expr, Op, Type};

pub struct Environment {
    ids: HashMap<String, Type>,
    type_params_count: u32,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            ids: HashMap::new(),
            type_params_count: 0,
        }
    }

    pub fn add_ids(&mut self, ids: HashSet<String>) {
        for id in ids {
            let tp = self.new_type_param();
            self.ids.insert(id, tp);
        }
    }

    pub fn get_id(&self, name: &String) -> Option<&Type> {
        self.ids.get(name)
    }

    fn new_type_param(&mut self) -> Type {
        let name = format!("a{}", self.type_params_count);
        self.type_params_count += 1;
        Type::T(name)
    }
}

pub fn annotate(expr: &Expr, env: &mut Environment) -> AExpr {
    match expr {
        Expr::Num(n) => AExpr::Num(*n, Type::Num),
        Expr::Bool(b) => AExpr::Bool(*b, Type::Bool),
        Expr::Val(x) => {
            let ty = env
                .get_id(x)
                .expect(format!("variable {} not defined", x).as_str());
            AExpr::Val(x.clone(), ty.clone())
        }
        Expr::BinOp(lhs, op, rhs) => {
            let alhs = annotate(lhs, env);
            let arhs = annotate(rhs, env);
            AExpr::BinOp(Box::new(alhs), *op, Box::new(arhs), env.new_type_param())
        }
        Expr::Fun(arg, expr) => {
            let arg_type = env
                .get_id(arg)
                .expect(format!("variable {} not defined", arg).as_str())
                .clone();
            let aexpr = annotate(expr, env);
            AExpr::Fun(
                arg.clone(),
                Box::new(aexpr),
                Type::Fun {
                    arg: Box::new(arg_type),
                    ret: Box::new(env.new_type_param()),
                },
            )
        }
        Expr::App(fun, arg) => {
            let afun = annotate(fun, env);
            let aarg = annotate(arg, env);
            AExpr::App(Box::new(afun), Box::new(aarg), env.new_type_param())
        }
    }
}

pub fn collect_aexpr(constraints: &mut Vec<(Type, Type)>, aexpr: &AExpr) {
    match aexpr {
        AExpr::BinOp(lhs, op, rhs, tp) => {
            collect_aexpr(constraints, lhs);
            collect_aexpr(constraints, rhs);

            let lhs_tp: &Type = lhs.as_ref().into();
            let rhs_tp: &Type = rhs.as_ref().into();

            match op {
                Op::Add | Op::Mul => {
                    constraints.push((lhs_tp.clone(), Type::Num));
                    constraints.push((rhs_tp.clone(), Type::Num));
                    constraints.push((tp.clone(), Type::Num));
                }
                Op::Gt | Op::Lt => {
                    constraints.push((lhs_tp.clone(), Type::Num));
                    constraints.push((rhs_tp.clone(), Type::Num));
                    constraints.push((tp.clone(), Type::Bool));
                }

                Op::And | Op::Or => {
                    constraints.push((lhs_tp.clone(), Type::Bool));
                    constraints.push((rhs_tp.clone(), Type::Bool));
                    constraints.push((tp.clone(), Type::Bool));
                }
            }
        }
        AExpr::Fun(_, ae, tp) => {
            collect_aexpr(constraints, ae);
            match tp {
                Type::Fun {
                    arg: _,
                    ret: ret_tp,
                } => {
                    let ae_tp: &Type = ae.as_ref().into();
                    constraints.push((ae_tp.clone(), ret_tp.as_ref().clone()));
                }
                _ => panic!("not a function"),
            }
        }
        AExpr::App(fun, arg, t) => {
            collect_aexpr(constraints, &fun);
            collect_aexpr(constraints, &arg);

            let fun_tp: &Type = fun.as_ref().into();
            match fun_tp {
                Type::Fun {
                    arg: argt,
                    ret: ret_type,
                } => {
                    constraints.push((t.clone(), ret_type.as_ref().clone()));

                    let arg_tp: &Type = arg.as_ref().into();
                    constraints.push((argt.as_ref().clone(), arg_tp.clone()));
                }
                Type::T(_) => {
                    let arg_tp: &Type = arg.as_ref().into();
                    constraints.push((
                        fun_tp.clone(),
                        Type::Fun {
                            arg: Box::new(arg_tp.clone()),
                            ret: Box::new(t.clone()),
                        },
                    ));
                }
                _ => panic!("incorrect function application"),
            }
        }
        _ => (),
    }
}

pub fn substitute(u: Type, x: &String, t: Type) -> Type {
    match t {
        Type::Num | Type::Bool => t,
        Type::T(c) => {
            if c == *x {
                u
            } else {
                Type::T(c)
            }
        }
        Type::Fun { arg: t1, ret: t2 } => {
            let ts1 = substitute(u.clone(), x, t1.as_ref().clone());
            let ts2 = substitute(u.clone(), x, t2.as_ref().clone());
            Type::Fun {
                arg: Box::new(ts1),
                ret: Box::new(ts2),
            }
        }
    }
}

pub fn apply(subs: &Vec<(String, Type)>, t: Type) -> Type {
    subs.iter()
        .fold(t, |acc, (x, u)| substitute(u.clone(), x, acc))
}

pub fn unify(mut constraints: Vec<(Type, Type)>) -> Vec<(String, Type)> {
    match constraints.pop() {
        Some((x, y)) => {
            let mut t2 = unify(constraints);
            let mut t1 = unify_one(apply(&t2, x), apply(&t2, y));

            t2.append(&mut t1);
            t2
        }

        None => Vec::new(),
    }
}

pub fn unify_one(tp1: Type, tp2: Type) -> Vec<(String, Type)> {
    match (tp1, tp2) {
        (Type::Num, Type::Num) | (Type::Bool, Type::Bool) => Vec::new(),
        (Type::T(x), z) | (z, Type::T(x)) => vec![(x, z)],
        (Type::Fun { arg: a, ret: b }, Type::Fun { arg: x, ret: y }) => {
            let mut res = Vec::new();
            res.append(&mut unify_one(a.as_ref().clone(), x.as_ref().clone()));
            res.append(&mut unify_one(b.as_ref().clone(), y.as_ref().clone()));
            res
        }

        (tp1, tp2) => panic!("mismatched types: ({:?}, {:?})", tp1, tp2),
    }
}

pub fn apply_aexpr(subs: &Vec<(String, Type)>, aexpr: AExpr) -> AExpr {
    match aexpr {
        AExpr::Num(n, t) => AExpr::Num(n, apply(subs, t)),
        AExpr::Bool(b, t) => AExpr::Bool(b, apply(subs, t)),
        AExpr::Val(s, t) => AExpr::Val(s, apply(subs, t)),
        AExpr::BinOp(lhs, op, rhs, t) => AExpr::BinOp(
            Box::new(apply_aexpr(subs, *lhs)),
            op,
            Box::new(apply_aexpr(subs, *rhs)),
            apply(subs, t),
        ),
        AExpr::Fun(id, e, t) => AExpr::Fun(id, Box::new(apply_aexpr(subs, *e)), apply(subs, t)),
        AExpr::App(fun, arg, t) => AExpr::App(
            Box::new(apply_aexpr(subs, *fun)),
            Box::new(apply_aexpr(subs, *arg)),
            apply(subs, t),
        ),
    }
}

pub fn infer(mut environment: Environment, expr: Expr) -> AExpr {
    let mut ids = HashSet::new();
    collect_ids(&mut ids, &expr);
    environment.add_ids(ids);

    let aexpr = annotate(&expr, &mut environment);
    println!("annotated: {}", aexpr);
    let mut constraints = Vec::new();
    collect_aexpr(&mut constraints, &aexpr);
    println!("constraints: ", );

    for (t1, t2) in &constraints {
        println!("{} = {}", t1, t2);
    }

    let subs = unify(constraints);

    apply_aexpr(&subs, aexpr)
}
