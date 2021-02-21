use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr, FaceSystem, Var};
use crate::context::Context;
use crate::interval::IntervalDnf;
use crate::unwrap_pattern;
use crate::util::types::ZeroOne;
use crate::face::Face;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pres {
    pub function: Box<Term>,
    pub face_system: FaceSystem,
    pub witness: Box<Term>,
}

impl Pres {
    pub fn new(function: Term, face_system: FaceSystem, witness: Term) -> Self {
        Pres {
            function: Box::new(function),
            face_system,
            witness: Box::new(witness),
        }
    }
}


impl Evaluate for Pres {
    type Evaluated = Expr;

    fn evaluate(self, ctx: &Context) -> Expr {
        let f_i0 = self.function.clone().substitute_interval(IntervalDnf::Zero, 0);

        let (f_src, f_tgt) = unwrap_pattern! {
            &f_i0.type_expr; Expr::Pi(pi) =>
                (*pi.source.clone(), *pi.target.clone())
        };
        let a0 = Term::new(Expr::app(f_i0, *self.witness.clone()), f_tgt.expr.clone());

        let v = Term::new(
            Expr::fill(
                Var::new(0),
                Term::new_type(f_src.expr.clone()),
                self.face_system.clone(),
                self.witness.clone().increment_indices()
            ),
            self.witness.type_expr.clone()
        );

        let mut face_system = self.face_system.clone();
        let new_face = Face::with_sides(&[(Var::new(0), ZeroOne::One)]);
        face_system.faces.push((new_face, Expr::app(*self.function.clone(), v)));

        Expr::path_bind(Term::new(
            Expr::comp(
                f_tgt.clone(),
                face_system,
                a0
            ),
            f_tgt.expr
        )).evaluate(ctx)
    }
}

impl Normalise for Pres {
    fn normalise(self, ctx: &Context) -> Expr {
        self.evaluate(ctx).normalise(ctx)
    }
}

impl Substitute for Pres {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        // Leave space for a new direction in the context
        let expr_line = expr.clone().increment_indices();
        Pres::new(
            self.function.substitute_expr(expr_line.clone(), var+1),
            self.face_system.substitute_expr(expr_line, var+1),
            self.witness.substitute_expr(expr, var))
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        // Leave space for a new direction in the context
        let expr_line = expr.clone().increment_indices();
        Pres::new(
            self.function.substitute_interval(expr_line.clone(), var+1),
            self.face_system.substitute_interval(expr_line, var+1),
            self.witness.substitute_interval(expr, var))
    }
}

impl DeBruijnIndexed for Pres {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        Pres::new(self.function.increment_indices_from_by(start+1, amount),
            self.face_system.increment_indices_from_by(start+1, amount),
            self.witness.increment_indices_from_by(start, amount))
    }
}

impl Display for Pres {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "Pres {} {} {}", self.function, self.face_system, self.witness)
    }
}
