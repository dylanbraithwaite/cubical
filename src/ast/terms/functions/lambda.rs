use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr};
use crate::context::Context;
use crate::interval::IntervalDnf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Lambda {
    pub source_type: Box<Term>,
    pub body: Box<Term>,
}

impl Lambda {
    pub fn new(source: Term, body: Term) -> Self {
        Lambda {
            source_type: Box::new(source),
            body: Box::new(body),
        }
    }
}

impl Evaluate for Lambda {
    type Evaluated = Self;

    fn evaluate(self, _: &Context) -> Self {
        self
    }
}

impl Normalise for Lambda {
    fn normalise(self, ctx: &Context) -> Self {
        let source_type = self.source_type.normalise(ctx);

        let ctx = ctx.bind_term_var("", &source_type.type_expr);
        let body = self.body.normalise(&ctx);

        Lambda::new(source_type, body)
    }
}

impl Substitute for Lambda {
    type ExprSubst = Lambda;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        let source_type = self.source_type.substitute_expr(expr.clone(), var);

        let expr = expr.increment_indices();
        let body = self.body.substitute_expr(expr, var+1);

        Lambda::new(source_type, body)
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        let source_type = self.source_type.substitute_interval(expr.clone(), var);

        let expr = expr.increment_indices();
        let body = self.body.substitute_interval(expr, var+1);

        Lambda::new(source_type, body)
    }
}

impl DeBruijnIndexed for Lambda {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        let source_type = self.source_type.increment_indices_from_by(start, amount);
        let body = self.body.increment_indices_from_by(start+1, amount);

        Lambda::new(source_type, body)
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "(Lambda (_ : {}) => {})", self.source_type.expr, self.body.expr)
    }
}
