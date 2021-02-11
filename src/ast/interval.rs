use crate::ast::traits::*;
use crate::ast::{Expr, Var};
use crate::interval::{IntervalDnf, IsNegated, Conjunction};
use crate::context::Context;

impl Evaluate for IntervalDnf {
    type Evaluated = IntervalDnf;

    fn evaluate(self, ctx: &Context) -> IntervalDnf {
        self.normalise(ctx)
    }
}

impl Normalise for IntervalDnf {
    fn normalise(self, ctx: &Context) -> IntervalDnf {
        crate::interval::normalise_interval(ctx, &self)
    }
}

impl Substitute for IntervalDnf {
    type ExprSubst = IntervalDnf;

    fn substitute_expr(self, _: Expr, _: usize) -> Self {
        unimplemented!()
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        match self {
            IntervalDnf::One => IntervalDnf::One,
            IntervalDnf::Zero => IntervalDnf::Zero,
            IntervalDnf::Conjunction(mut terms) => {
                if terms.atoms.remove(&(Var::new(var), IsNegated::Negated)) {
                    unimplemented!("Interval joins not fully implemented")
                } else if terms.atoms.remove(&(Var::new(var), IsNegated::NotNegated)) {
                    IntervalDnf::meet(expr, IntervalDnf::Conjunction(terms))
                } else {
                    IntervalDnf::Conjunction(terms)
                }
            }
        }
    }
}

impl DeBruijnIndexed for IntervalDnf {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        match self {
            int @ IntervalDnf::Zero |
            int @ IntervalDnf::One => int,
            IntervalDnf::Conjunction(conj) => IntervalDnf::Conjunction(
                Conjunction {
                    atoms: conj.atoms
                        .into_iter()
                        .map(|(var, val)| (var.increment_indices_from_by(start, amount), val))
                        .collect()
                }
            )
        }
    }
}


