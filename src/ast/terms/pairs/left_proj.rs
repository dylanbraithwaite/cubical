use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr};
use crate::context::Context;
use crate::interval::IntervalDnf;
use crate::unwrap_pattern;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LeftProj {
    pub pair: Box<Term>,
}

impl LeftProj {
    pub fn new(pair: Term) -> Self {
        LeftProj {
            pair: Box::new(pair),
        }
    }
}

impl Evaluate for LeftProj {
    type Evaluated = Term;

    fn evaluate(self, ctx: &Context) -> Self::Evaluated {
        let pair = self.pair.evaluate(ctx);

        unwrap_pattern! {
            pair.expr; Expr::Pair(pair) => *pair.left
        }
    }
}

impl Normalise for LeftProj {
    fn normalise(self, ctx: &Context) -> Self::Evaluated {
        let pair = self.pair.normalise(ctx);

        unwrap_pattern! {
            pair.expr; Expr::Pair(pair) => *pair.left
        }
    }
}

impl Substitute for LeftProj {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self::ExprSubst {
        LeftProj::new(self.pair.substitute_expr(expr, var))
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        LeftProj::new(self.pair.substitute_interval(expr, var))
    }
}

impl DeBruijnIndexed for LeftProj {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        LeftProj::new(self.pair.increment_indices_from_by(start, amount))
    }
}

impl Display for LeftProj {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "ProjL {}", self.pair)
    }
}
