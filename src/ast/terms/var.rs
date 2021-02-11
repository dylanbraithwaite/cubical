use either::{Either};

use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::{Expr};
use crate::ast::traits::*;
use crate::context::Context;
use crate::var_target::VarTarget;
use crate::interval::{IntervalDnf, IsNegated};

#[derive(Hash, Copy, Clone, Debug, PartialEq, Eq)]
pub struct Var {
    pub debruijn_index: usize,
}

impl Var {
    pub fn new(debruijn_index: usize) -> Self {
        Var { debruijn_index }
    }
}

impl Evaluate for Var {
    type Evaluated = Either<Expr, IntervalDnf>;

    fn evaluate(self, ctx: &Context) -> Self::Evaluated {
        self.normalise(ctx)
    }
}

impl Normalise for Var {
    fn normalise(self, ctx: &Context) -> Self::Evaluated {
        // TODO: Lookup the value of the variable in the context
        match ctx.index(self.debruijn_index)
            .expect(&format!("Unknown variable: {}", self.debruijn_index))
        {
            VarTarget::Term(term) => Either::Left(term.expr.clone()),
            VarTarget::Interval(interval) => Either::Right(interval.clone()),
            VarTarget::BoundTerm(_) => Either::Left(Expr::Var(self)),
            VarTarget::BoundInterval => unimplemented!(),
        }
    }
}

impl Substitute for Var {
    type ExprSubst = Expr;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self::ExprSubst {
        use std::cmp::Ordering;
        match self.debruijn_index.cmp(&var) {
            Ordering::Less => Expr::Var(self),
            Ordering::Equal => expr,
            Ordering::Greater => Expr::var(self.debruijn_index - 1)
        }
    }

    type IntervalSubst = IntervalDnf;

    fn substitute_interval(self, interval: IntervalDnf, var: usize) -> IntervalDnf {
        use std::cmp::Ordering;
        match self.debruijn_index.cmp(&var) {
            Ordering::Less => IntervalDnf::single(self, IsNegated::NotNegated),
            Ordering::Equal => interval,
            Ordering::Greater => IntervalDnf::single(Var::new(self.debruijn_index - 1), IsNegated::NotNegated)
        }
    }
}

impl DeBruijnIndexed for Var {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        if self.debruijn_index >= start {
            Var::new(self.debruijn_index + amount)
        } else {
            self
        }
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "v{}", self.debruijn_index)
    }
}
