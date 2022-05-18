use core::panic;
use std::collections::{HashMap, HashSet};

use crate::{
    ast::{collect_ids, AExpr, BinOp, Expr, TyId, Type},
    result::{InferOperation, InferProcess, InferResult},
};

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
        let v = TyId(self.type_params_count);
        self.type_params_count += 1;
        Type::TyVar(v)
    }
}

pub fn annotate(expr: &Expr, env: &mut Environment) -> AExpr {
    match expr {
        Expr::Num(n) => AExpr::Num(*n, Type::Num),
        Expr::Bool(b) => AExpr::Bool(*b, Type::Bool),
        Expr::Val(x) => {
            let ty = env
                .get_id(x)
                .expect(format!("variable {:?} not defined", x).as_str());
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
                BinOp::Add | BinOp::Mul => {
                    constraints.push((lhs_tp.clone(), Type::Num));
                    constraints.push((rhs_tp.clone(), Type::Num));
                    constraints.push((tp.clone(), Type::Num));
                }
                BinOp::Gt | BinOp::Lt => {
                    constraints.push((lhs_tp.clone(), Type::Num));
                    constraints.push((rhs_tp.clone(), Type::Num));
                    constraints.push((tp.clone(), Type::Bool));
                }

                BinOp::And | BinOp::Or => {
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
                Type::TyVar(_) => {
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

pub fn substitute(u: Type, x: TyId, t: Type) -> (InferProcess, Type) {
    let mut process = Vec::new();
    let u_cloned = u.clone();
    let x_cloned = x.clone();
    let t_cloned = t.clone();

    let result = match t {
        Type::Num | Type::Bool => t,
        Type::TyVar(v) => {
            if v == x {
                u
            } else {
                Type::TyVar(v)
            }
        }
        Type::Fun { arg: t1, ret: t2 } => {
            let (p, ts1) = substitute(u.clone(), x, t1.as_ref().clone());
            process.push(p);
            let (p, ts2) = substitute(u.clone(), x, t2.as_ref().clone());
            process.push(p);
            Type::Fun {
                arg: Box::new(ts1),
                ret: Box::new(ts2),
            }
        }
    };

    (
        InferProcess {
            operation: InferOperation::Substitude {
                u: u_cloned,
                x: x_cloned,
                t: t_cloned,
                result: result.clone(),
            },
            process,
        },
        result,
    )
}

pub fn apply(subs: &Vec<(TyId, Type)>, t: Type) -> (InferProcess, Type) {
    let substitudes_clone = subs.clone();
    let ty_clone = t.clone();

    let (process, result) =
        subs.iter()
            .fold((Vec::new(), t.clone()), |(mut process, acc), (x, u)| {
                let (p, t) = substitute(u.clone(), *x, acc);
                process.push(p);
                (process, t)
            });

    (
        InferProcess {
            operation: InferOperation::Apply {
                substitudes: substitudes_clone,
                ty: ty_clone,
                result: result.clone(),
            },
            process,
        },
        result,
    )
}

pub fn unify(constraints: &mut Vec<(Type, Type)>) -> (InferProcess, Vec<(TyId, Type)>) {
    let mut process = Vec::new();
    let constraints_cloned = constraints.clone();
    let result = match constraints.pop() {
        Some((x, y)) => {
            let (p, mut t2) = unify(constraints);
            process.push(p);
            let mut t1 = {
                let (p, tp1) = apply(&t2, x);
                process.push(p);
                let (p, tp2) = apply(&t2, y);
                process.push(p);

                let (p, result) = unify_one(tp1, tp2);
                process.push(p);

                result
            };

            t2.append(&mut t1);

            t2
        }

        None => Vec::new(),
    };

    (
        InferProcess {
            operation: InferOperation::Unify {
                constraints: constraints_cloned,
                result: result.clone(),
            },
            process,
        },
        result,
    )
}

pub fn unify_one(tp1: Type, tp2: Type) -> (InferProcess, Vec<(TyId, Type)>) {
    let mut process = Vec::new();
    let ty1_cloned = tp1.clone();
    let ty2_cloned = tp2.clone();

    let result = match (tp1.clone(), tp2.clone()) {
        (Type::Num, Type::Num) | (Type::Bool, Type::Bool) => Vec::new(),
        (Type::TyVar(x), z) | (z, Type::TyVar(x)) => vec![(x, z)],
        (Type::Fun { arg: a, ret: b }, Type::Fun { arg: x, ret: y }) => {
            let mut subs = Vec::new();
            let (p, mut s) = unify_one(a.as_ref().clone(), x.as_ref().clone());
            process.push(p);
            subs.append(&mut s);
            // subs.push(s);

            let (p, mut s) = unify_one(b.as_ref().clone(), y.as_ref().clone());
            process.push(p);
            subs.append(&mut s);

            subs
        }

        (tp1, tp2) => panic!("mismatched types: ({:?}, {:?})", tp1, tp2),
    };

    (
        InferProcess {
            operation: InferOperation::UnifyOne {
                ty1: ty1_cloned,
                ty2: ty2_cloned,
                result: result.clone(),
            },
            process,
        },
        result,
    )
}

pub fn apply_aexpr(subs: &Vec<(TyId, Type)>, aexpr: AExpr) -> AExpr {
    match aexpr {
        AExpr::Num(n, t) => AExpr::Num(n, apply(subs, t).1),
        AExpr::Bool(b, t) => AExpr::Bool(b, apply(subs, t).1),
        AExpr::Val(s, t) => AExpr::Val(s, apply(subs, t).1),
        AExpr::BinOp(lhs, op, rhs, t) => AExpr::BinOp(
            Box::new(apply_aexpr(subs, *lhs)),
            op,
            Box::new(apply_aexpr(subs, *rhs)),
            apply(subs, t).1,
        ),
        AExpr::Fun(id, e, t) => AExpr::Fun(id, Box::new(apply_aexpr(subs, *e)), apply(subs, t).1),
        AExpr::App(fun, arg, t) => AExpr::App(
            Box::new(apply_aexpr(subs, *fun)),
            Box::new(apply_aexpr(subs, *arg)),
            apply(subs, t).1,
        ),
    }
}

pub fn infer(mut environment: Environment, expr: Expr) -> InferResult {
    let mut ids = HashSet::new();
    collect_ids(&mut ids, &expr);
    environment.add_ids(ids);

    let aexpr = annotate(&expr, &mut environment);

    let mut constraints = Vec::new();
    collect_aexpr(&mut constraints, &aexpr);
    let constraints_result = constraints.clone();

    let (process, subs) = unify(&mut constraints);

    let result = apply_aexpr(&subs, aexpr.clone());

    InferResult {
        input: expr,
        annotated: aexpr,
        constraints: constraints_result,
        process,
        output: result,
    }
}
