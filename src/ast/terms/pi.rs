use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr};
use crate::context::Context;
use crate::interval::IntervalDnf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pi {
    pub source: Box<Term>,
    pub target: Box<Term>,
}

impl Pi {
    pub fn new(source: Term, target: Term) -> Self {
        Pi {
            source: Box::new(source),
            target: Box::new(target),
        }
    }
}

impl Evaluate for Pi {
    type Evaluated = Self;

    fn evaluate(self, _: &Context) -> Self {
        self
    }
}

impl Normalise for Pi {
    fn normalise(self, ctx: &Context) -> Self {
        let source = self.source.normalise(ctx);

        let ctx = ctx.bind_term_var("", &source.type_expr);
        let target = self.target.normalise(&ctx);

        Pi::new(source, target)
    }
}

impl Substitute for Pi {
    type ExprSubst = Pi;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        let source = self.source.substitute_expr(expr.clone(), var);

        let expr = expr.increment_indices();
        let target = self.target.substitute_expr(expr, var+1);

        Pi::new(source, target)
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        let source = self.source.substitute_interval(expr.clone(), var);

        let expr = expr.increment_indices();
        let target = self.target.substitute_interval(expr, var+1);

        Pi::new(source, target)
    }
}

impl DeBruijnIndexed for Pi {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        let source = self.source.increment_indices_from_by(start, amount);
        let target = self.target.increment_indices_from_by(start+1, amount);

        Pi::new(source, target)
    }
}

impl Display for Pi {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "(Pi (_ : {}) -> {})", self.source.expr, self.target)
    }
}
