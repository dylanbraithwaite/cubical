use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr, Var, FaceSystem};
use crate::context::Context;
use crate::interval::{IntervalDnf, IsNegated};
use crate::unwrap_pattern;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct App {
    pub func: Box<Term>,
    pub argument: Box<Term>,
}

impl App {
    pub fn new(func: Term, argument: Term) -> Self {
        App {
            func: Box::new(func),
            argument: Box::new(argument),
        }
    }
}

impl Evaluate for App {
    type Evaluated = Term;

    fn evaluate(self, ctx: &Context) -> Self::Evaluated {
        let func = self.func.evaluate(ctx);

        match func.expr {
            Expr::Lambda(lambda) => lambda.body
                .substitute_expr(self.argument.expr, 0)
                .evaluate(ctx),
            Expr::Comp(comp) => {
                let pi = unwrap_pattern! { &comp.space.expr; Expr::Pi(pi) => pi };
                let v = Expr::fill(
                        Var::new(0),
                        pi.source.clone().substitute_interval(
                            IntervalDnf::single(Var::new(0), IsNegated::Negated), 0),
                        FaceSystem::new([]),
                        *self.argument.clone()
                    )
                    .substitute_interval(
                        IntervalDnf::single(Var::new(0), IsNegated::Negated), 0
                    )
                    .evaluate(ctx);

                let v_term = Term::new(v.clone(), pi.source.expr.clone());

                let ctx_i = ctx.bind_interval_var("");

                let target_subst = pi.target
                    .clone()
                    .substitute_expr(v.increment_indices(), 0)
                    .evaluate(&ctx_i);

                let system = FaceSystem::from_iter(
                    comp.face_system.faces
                        .iter()
                        .cloned()
                        .map(|(face, expr)| (face, Expr::app(
                            Term::new(expr, comp.space.expr.clone()),
                            v_term.clone()
                        ))));
                let witness = Expr::app(
                    *comp.witness.clone(),
                    v_term.substitute_interval(IntervalDnf::Zero, 0));

                let target_subst_i0 = target_subst.expr.clone().substitute_interval(
                    IntervalDnf::Zero, 1);

                Term::new(
                    Expr::comp(target_subst, system, Term::new(witness, target_subst_i0.clone())),
                    target_subst_i0,
                )
            }
            _ => panic!()
        }
    }
}

impl Normalise for App {
    fn normalise(self, ctx: &Context) -> Self::Evaluated {
       self.evaluate(ctx).normalise(ctx)
    }
}

impl Substitute for App {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        App::new(self.func.substitute_expr(expr.clone(), var),
            self.argument.substitute_expr(expr, var))
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        App::new(self.func.substitute_interval(expr.clone(), var),
            self.argument.substitute_interval(expr, var))
    }
}

impl DeBruijnIndexed for App {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        App::new(self.func.increment_indices_from_by(start, amount),
            self.argument.increment_indices_from_by(start, amount))
    }
}

impl Display for App {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "({} {})", self.func.expr, self.argument.expr)
    }
}
