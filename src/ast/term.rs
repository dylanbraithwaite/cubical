use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::Expr;
use crate::interval::IntervalDnf;
use crate::context::Context;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Term{
    pub expr: Expr,
    pub type_expr: Expr,
}

impl Term {
    pub fn new(expr: Expr, type_expr: Expr) -> Self {
        Term {
            expr,
            type_expr,
        }
    }

    pub fn new_type(expr: Expr) -> Self {
        Term {
            expr,
            type_expr: Expr::Type,
        }
    }
}

impl Evaluate for Term {
    type Evaluated = Term;

    fn evaluate(self, ctx: &Context) -> Term {
        Term::new(self.expr.evaluate(ctx), self.type_expr.evaluate(ctx))
    }
}

impl Normalise for Term {
    fn normalise(self, ctx: &Context) -> Term {
        Term::new(self.expr.normalise(ctx), self.type_expr.normalise(ctx))
    }
}

impl Substitute for Term {
    type ExprSubst = Term;

    fn substitute_expr(self, _: Expr, _: usize) -> Self {
        unimplemented!()
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        Term::new(
            self.expr.substitute_interval(expr.clone(), var),
            self.type_expr.substitute_interval(expr, var),
        )
    }
}

impl DeBruijnIndexed for Term {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        Term::new(
            self.expr.increment_indices_from_by(start, amount),
            self.type_expr.increment_indices_from_by(start, amount))
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "{} : {}", self.expr, self.type_expr)
    }
}
