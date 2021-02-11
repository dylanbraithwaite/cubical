use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr};
use crate::context::Context;
use crate::interval::IntervalDnf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Sigma {
    pub left_type: Box<Term>,
    pub right_type: Box<Term>,
}

impl Sigma {
    pub fn new(left_type: Term, right_type: Term) -> Self {
        Sigma {
            left_type: Box::new(left_type),
            right_type: Box::new(right_type),
        }
    }
}

impl Evaluate for Sigma {
    type Evaluated = Self;

    fn evaluate(self, _: &Context) -> Sigma {
        self
    }

}

impl Normalise for Sigma {
    fn normalise(self, ctx: &Context) -> Sigma {
        let left_type = self.left_type.normalise(ctx);

        let right_type = self.right_type.normalise(
            &ctx.bind_term_var("", &left_type.expr));

        Sigma::new(left_type, right_type)
    }
}

impl Substitute for Sigma {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        let left = self.left_type.substitute_expr(expr.clone(), var);

        let expr = expr.increment_indices();
        let right = self.right_type.substitute_expr(expr, var+1);

        Sigma::new(left, right)
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        let left = self.left_type.substitute_interval(expr.clone(), var);

        let expr = expr.increment_indices();
        let right = self.right_type.substitute_interval(expr, var+1);

        Sigma::new(left, right)
    }
}

impl DeBruijnIndexed for Sigma {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        let left = self.left_type.increment_indices_from_by(start, amount);
        let right = self.right_type.increment_indices_from_by(start+1, amount);

        Sigma::new(left, right)
    }
}

impl Display for Sigma {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "Sigma (_: {}) {}", self.left_type, self.right_type)
    }
}
