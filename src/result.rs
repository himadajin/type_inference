use pretty::{Doc, RcDoc};

use crate::ast::{AExpr, Expr, TyId, Type};

pub struct InferResult {
    pub input: Expr,
    pub annotated: AExpr,
    pub constraints: Vec<(Type, Type)>,
    pub operations: Vec<InferOperation>,
    pub output: AExpr,
}

pub enum InferOperation {
    Unify(Vec<(Type, Type)>),
    UnifyOne(Type, Type, Vec<(TyId, Type)>),
    Apply(Vec<(TyId, Type)>, Type, Type),
}

impl InferOperation {
    pub fn to_doc(&self) -> RcDoc<()> {
        match &self {
            InferOperation::Unify(constraints) => RcDoc::text("Unify:")
                .append(Doc::space())
                .append(constraints_to_doc(constraints)),
            InferOperation::UnifyOne(t1, t2, result) => RcDoc::text("UnifyOne:")
                .append(Doc::space())
                .append(RcDoc::intersperse(
                    [
                        RcDoc::text("(")
                            .append(RcDoc::intersperse(
                                [t1.to_doc(), t2.to_doc()],
                                RcDoc::text(",").append(Doc::space()),
                            ))
                            .append(")"),
                        RcDoc::text("=>"),
                        substitudes_to_doc(result),
                    ],
                    Doc::space(),
                )),

            InferOperation::Apply(subs, ty, result) => RcDoc::text("Apply:")
                .append(Doc::space())
                .append(RcDoc::intersperse(
                    [
                        substitudes_to_doc(subs),
                        RcDoc::text("->"),
                        ty.to_doc(),
                        RcDoc::text("=>"),
                        result.to_doc(),
                    ],
                    Doc::space(),
                )),
        }
    }
}

pub struct UnifyingStep {
    pub constraint: (Type, Type),
    pub substitudes: Vec<(TyId, Type)>,
}

impl UnifyingStep {
    pub fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(")
            .append(constraint_to_doc(&self.constraint))
            .append(")")
            .append(Doc::hardline())
            .append(substitudes_to_doc(&self.substitudes))
    }
}

impl InferResult {
    pub fn to_doc(&self) -> RcDoc<()> {
        let offset = 4;
        RcDoc::intersperse(
            [
                RcDoc::text("input:")
                    .append(RcDoc::hardline().append(self.input.to_doc()).nest(offset)),
                RcDoc::text("annotated:").append(
                    RcDoc::hardline()
                        .append(self.annotated.to_doc())
                        .nest(offset),
                ),
                RcDoc::text("constraints:").append(
                    RcDoc::hardline()
                        .append(constraints_to_doc(&self.constraints))
                        .nest(offset),
                ),
                RcDoc::text("operations").append(
                    RcDoc::line()
                        .append(RcDoc::intersperse(
                            self.operations.iter().enumerate().map(|(nth, operation)| {
                                RcDoc::as_string(nth)
                                    .append(":")
                                    .append(Doc::line())
                                    .append(operation.to_doc())
                                    .nest(offset)
                            }),
                            Doc::line(),
                        ))
                        .nest(offset),
                ),
                // RcDoc::text("unifying_steps:").append(
                //     RcDoc::line()
                //         .append(RcDoc::intersperse(
                //             self.unifying_steps.iter().enumerate().map(|(nth, step)| {
                //                 RcDoc::as_string(nth)
                //                     .append(":")
                //                     .append(Doc::line())
                //                     .append(step.to_doc())
                //                     .nest(offset)
                //             }),
                //             Doc::line(),
                //         ))
                //         .nest(offset),
                // ),
                RcDoc::text("output:")
                    .append(RcDoc::hardline().append(self.output.to_doc()).nest(offset)),
            ],
            Doc::line(),
        )
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

fn constraints_to_doc(constraints: &Vec<(Type, Type)>) -> RcDoc<()> {
    RcDoc::text("[")
        .append(RcDoc::intersperse(
            constraints
                .iter()
                .map(|constraint| constraint_to_doc(constraint)),
            RcDoc::text(",").append(Doc::space()),
        ))
        .append("]")
}

fn constraint_to_doc(constraint: &(Type, Type)) -> RcDoc<()> {
    let (lhs, rhs) = constraint;
    RcDoc::intersperse([lhs.to_doc(), RcDoc::text("="), rhs.to_doc()], Doc::space())
}

fn substitudes_to_doc(subs: &Vec<(TyId, Type)>) -> RcDoc<()> {
    RcDoc::text("[")
        .append(RcDoc::intersperse(
            subs.iter().map(|sub| substitute_to_doc(sub)),
            RcDoc::text(",").append(Doc::space()),
        ))
        .append("]")
}

fn substitute_to_doc(sub: &(TyId, Type)) -> RcDoc<()> {
    let (id, ty) = sub;
    RcDoc::text("(")
        .append(RcDoc::intersperse(
            [RcDoc::text("a").append(RcDoc::as_string(id.0)), ty.to_doc()],
            RcDoc::text(",").append(Doc::space()),
        ))
        .append(")")
}
