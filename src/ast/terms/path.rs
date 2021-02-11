use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr};
use crate::context::Context;
use crate::interval::IntervalDnf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Path {
    pub space: Box<Term>,
    pub start: Box<Term>,
    pub end: Box<Term>,
}

impl Path {
    pub fn new(space: Term, start: Term, end: Term) -> Self {
        Path {
            space: Box::new(space),
            start: Box::new(start),
            end: Box::new(end),
        }
    }
}

impl Evaluate for Path {
    type Evaluated = Self;

    fn evaluate(self, _: &Context) -> Self {
        self
    }
}

impl Normalise for Path {
    fn normalise(self, ctx: &Context) -> Self {
        Path::new(
            self.space.normalise(ctx),
            self.start.normalise(ctx),
            self.end.normalise(ctx),
        )
    }
}

impl Substitute for Path {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        Path::new(self.space.substitute_expr(expr.clone(), var),
            self.start.substitute_expr(expr.clone(), var),
            self.end.substitute_expr(expr, var))
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        Path::new(self.space.substitute_interval(expr.clone(), var),
            self.start.substitute_interval(expr.clone(), var),
            self.end.substitute_interval(expr, var))
    }
}

impl DeBruijnIndexed for Path {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        Path::new(self.space.increment_indices_from_by(start, amount),
            self.start.increment_indices_from_by(start, amount),
            self.end.increment_indices_from_by(start, amount))
    }
}


impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "Path {} {} {}", self.space.expr, self.start.expr, self.end.expr)
    }
}
