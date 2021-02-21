use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr};
use crate::context::Context;
use crate::interval::IntervalDnf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PathApp {
    pub func: Box<Term>,
    pub argument: Box<IntervalDnf>,
}

impl PathApp {
    pub fn new(path: Term, argument: IntervalDnf) -> Self {
        PathApp {
            func: Box::new(path),
            argument: Box::new(argument),
        }
    }
}

impl Evaluate for PathApp {
    type Evaluated = Expr;

    fn evaluate(self, ctx: &Context) -> Expr {
        let path = self.func.evaluate(ctx);
        let interval = self.argument.evaluate(ctx);

        // If we have a PathBind expression, we can directly apply the application
        if let Expr::PathBind(path_bind) = path.expr {
            return path_bind.body.expr.substitute_interval(interval, 0)
        }

        // Else if the interval value is 0 or 1, just take an endpoint from the path type
        if let Expr::Path(path) = &path.type_expr {
            if interval == IntervalDnf::One {
                return path.end.expr.clone()
            } else if interval == IntervalDnf::Zero {
                return path.start.expr.clone()
            }
        }

        // Else just normalise the path bind
        Expr::path_app(path, interval)
    }
}

impl Normalise for PathApp {
    fn normalise(self, ctx: &Context) -> Expr {
        match self.evaluate(ctx) {
            Expr::PathApp(path_app) => Expr::path_app(path_app.func.normalise(ctx),
                path_app.argument.normalise(ctx)),
            expr => expr.normalise(ctx)
        }
    }
}

#[test]
fn test_normalise_path_app_to_endpoints() {
    // Path Unit a b
    let path_type = Expr::path(
        Term::new(Expr::UnitType, Expr::Type),
        Term::new(Expr::var(1), Expr::UnitType),
        Term::new(Expr::var(0), Expr::UnitType)
    );

    // [a: Unit, b: Unit, p: Path Unit a b]
    let ctx = Context::new()
        .bind_term_var("a", &Expr::UnitType)
        .bind_term_var("b", &Expr::UnitType)
        .bind_term_var("p", &path_type);

    // p 0
    let left_expr = Expr::path_app(
        Term::new(Expr::var(0), path_type.clone()),
        IntervalDnf::Zero);

    // p 1
    let right_expr = Expr::path_app(
        Term::new(Expr::var(0), path_type),
        IntervalDnf::One);

    // a: Unit, b: Unit, p: Path Unit a b |- p 0 == a
    let left_expr = left_expr.normalise(&ctx);
    assert_eq!(left_expr, Expr::var(1));

    // a: Unit, b: Unit, p: Path Unit a b |- p 1 == b
    let right_expr = right_expr.normalise(&ctx);
    assert_eq!(right_expr, Expr::var(0));
}

impl Substitute for PathApp {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        PathApp::new(self.func.substitute_expr(expr.clone(), var),
            self.argument.substitute_expr(expr, var))
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        PathApp::new(self.func.substitute_interval(expr.clone(), var),
            self.argument.substitute_interval(expr, var))
    }
}

impl DeBruijnIndexed for PathApp {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        PathApp::new(self.func.increment_indices_from_by(start, amount),
            self.argument.increment_indices_from_by(start, amount))
    }
}

impl Display for PathApp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "({} {:?})", self.func.expr, self.argument)
    }
}

