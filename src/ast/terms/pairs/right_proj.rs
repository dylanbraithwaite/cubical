use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr};
use crate::context::Context;
use crate::interval::IntervalDnf;

use crate::unwrap_pattern;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RightProj {
    pub pair: Box<Term>,
}

impl RightProj {
    pub fn new(pair: Term) -> Self {
        RightProj {
            pair: Box::new(pair),
        }
    }
}

impl Evaluate for RightProj {
    type Evaluated = Term;

    fn evaluate(self, ctx: &Context) -> Self::Evaluated {
        let pair = self.pair.evaluate(ctx);

        unwrap_pattern! {
            pair.expr; Expr::Pair(pair) => pair.right.substitute_expr(pair.left.expr, 0)
        }
    }
}

impl Normalise for RightProj {
    fn normalise(self, ctx: &Context) -> Self::Evaluated {
        let pair = self.pair.normalise(ctx);

        unwrap_pattern! {
            pair.expr; Expr::Pair(pair) => pair.right.substitute_expr(pair.left.expr, 0)
        }
    }
}

impl Substitute for RightProj {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self::ExprSubst {
        RightProj::new(self.pair.substitute_expr(expr, var))
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        RightProj::new(self.pair.substitute_interval(expr, var))
    }
}

impl DeBruijnIndexed for RightProj {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        RightProj::new(self.pair.increment_indices_from_by(start, amount))
    }
}

impl Display for RightProj {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "ProjL {}", self.pair)
    }
}
