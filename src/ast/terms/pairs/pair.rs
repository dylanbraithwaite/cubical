use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr};
use crate::context::Context;
use crate::interval::IntervalDnf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pair {
    pub left: Box<Term>,
    pub right: Box<Term>,
}

impl Pair {
    pub fn new(left: Term, right: Term) -> Self {
        Pair {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

impl Evaluate for Pair {
    type Evaluated = Self;

    fn evaluate(self, _: &Context) -> Pair {
        self
    }
}

impl Normalise for Pair {
    fn normalise(self, ctx: &Context) -> Pair {
        let left = self.left.normalise(ctx);
        let right = self.right.normalise(&ctx.define_term_var("", &left));

        Pair::new(left, right)
    }
}

impl Substitute for Pair {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        let left = self.left.substitute_expr(expr.clone(), var);

        let expr = expr.increment_indices();
        let right = self.right.substitute_expr(expr, var+1);

        Pair::new(left, right)
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        let left = self.left.substitute_interval(expr.clone(), var);

        let expr = expr.increment_indices();
        let right = self.right.substitute_interval(expr, var+1);

        Pair::new(left, right)
    }
}

impl DeBruijnIndexed for Pair {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        let left = self.left.increment_indices_from_by(start, amount);
        let right = self.right.increment_indices_from_by(start+1, amount);

        Pair::new(left, right)
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "({}, {})", self.left, self.right)
    }
}

