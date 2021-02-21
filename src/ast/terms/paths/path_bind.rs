use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr};
use crate::context::Context;
use crate::interval::IntervalDnf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PathBind {
    pub body: Box<Term>,
}

impl PathBind {
    pub fn new(body: Term) -> Self {
        PathBind {
            body: Box::new(body),
        }
    }
}

impl Evaluate for PathBind {
    type Evaluated = Self;

    fn evaluate(self, _: &Context) -> Self {
        self
    }
}

impl Normalise for PathBind {
    fn normalise(self, ctx: &Context) -> Self {
        PathBind::new(self.body.normalise(&ctx.bind_interval_var("")))
    }
}

impl Substitute for PathBind {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        PathBind::new(self.body.substitute_expr(expr.increment_indices(), var+1))
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        PathBind::new(self.body.substitute_interval(expr.increment_indices(), var+1))
    }
}

impl DeBruijnIndexed for PathBind {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        PathBind::new(self.body.increment_indices_from_by(start+1, amount))
    }
}

impl Display for PathBind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "(PathBind _ => {})", self.body.expr)
    }
}

