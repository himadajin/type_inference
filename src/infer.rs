use core::panic;
use std::collections::{HashMap, HashSet};

use crate::{
    ast::{collect_ids, AExpr, BinOp, Expr, TyId, Type},
    result::{InferOperation, InferResult, UnifyingStep},
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

pub fn substitute(u: Type, x: TyId, t: Type) -> Type {
    match t {
        Type::Num | Type::Bool => t,
        Type::TyVar(v) => {
            if v == x {
                u
            } else {
                Type::TyVar(v)
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

pub fn apply(operations: &mut Vec<InferOperation>, subs: &Vec<(TyId, Type)>, t: Type) -> Type {
    let result = subs
        .iter()
        .fold(t.clone(), |acc, (x, u)| substitute(u.clone(), *x, acc));
    operations.push(InferOperation::Apply(
        subs.clone(),
        t.clone(),
        result.clone(),
    ));
    result
}

pub fn unify(
    operations: &mut Vec<InferOperation>,
    constraints: &mut Vec<(Type, Type)>,
) -> Vec<(TyId, Type)> {
    operations.push(InferOperation::Unify(constraints.clone()));
    match constraints.pop() {
        Some((x, y)) => {
            let mut t2 = unify(operations, constraints);
            let mut t1 = {
                let tp1 = apply(operations, &t2, x.clone());
                let tp2 = apply(operations, &t2, y.clone());
                unify_one(operations, tp1, tp2)
            };

            t2.append(&mut t1);

            t2
        }

        None => Vec::new(),
    }
}

pub fn unify_one(operations: &mut Vec<InferOperation>, tp1: Type, tp2: Type) -> Vec<(TyId, Type)> {
    let result = match (tp1.clone(), tp2.clone()) {
        (Type::Num, Type::Num) | (Type::Bool, Type::Bool) => Vec::new(),
        (Type::TyVar(x), z) | (z, Type::TyVar(x)) => vec![(x, z)],
        (Type::Fun { arg: a, ret: b }, Type::Fun { arg: x, ret: y }) => {
            let mut res = Vec::new();
            res.append(&mut unify_one(
                operations,
                a.as_ref().clone(),
                x.as_ref().clone(),
            ));
            res.append(&mut unify_one(
                operations,
                b.as_ref().clone(),
                y.as_ref().clone(),
            ));
            res
        }

        (tp1, tp2) => panic!("mismatched types: ({:?}, {:?})", tp1, tp2),
    };
    operations.push(InferOperation::UnifyOne(tp1, tp2, result.clone()));

    result
}

pub fn apply_aexpr(subs: &Vec<(TyId, Type)>, aexpr: AExpr) -> AExpr {
    match aexpr {
        AExpr::Num(n, t) => AExpr::Num(n, apply(&mut Vec::new(), subs, t)),
        AExpr::Bool(b, t) => AExpr::Bool(b, apply(&mut Vec::new(), subs, t)),
        AExpr::Val(s, t) => AExpr::Val(s, apply(&mut Vec::new(), subs, t)),
        AExpr::BinOp(lhs, op, rhs, t) => AExpr::BinOp(
            Box::new(apply_aexpr(subs, *lhs)),
            op,
            Box::new(apply_aexpr(subs, *rhs)),
            apply(&mut Vec::new(), subs, t),
        ),
        AExpr::Fun(id, e, t) => AExpr::Fun(
            id,
            Box::new(apply_aexpr(subs, *e)),
            apply(&mut Vec::new(), subs, t),
        ),
        AExpr::App(fun, arg, t) => AExpr::App(
            Box::new(apply_aexpr(subs, *fun)),
            Box::new(apply_aexpr(subs, *arg)),
            apply(&mut Vec::new(), subs, t),
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

    let mut operations = Vec::new();
    let subs = unify(&mut operations, &mut constraints);

    let result = apply_aexpr(&subs, aexpr.clone());

    InferResult {
        input: expr,
        annotated: aexpr,
        constraints: constraints_result,
        operations,
        output: result,
    }
}
