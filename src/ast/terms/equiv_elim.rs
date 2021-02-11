use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr, FaceSystem, ContrElim};
use crate::context::Context;
use crate::interval::IntervalDnf;
use crate::unwrap_pattern;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EquivElim {
    pub equiv: Box<Term>,
    pub face_system: FaceSystem,
    pub witness: Box<Term>,
}

impl EquivElim {
    pub fn new(equiv: Term, face_system: FaceSystem, witness: Term) -> Self {
        EquivElim {
            equiv: Box::new(equiv),
            face_system,
            witness: Box::new(witness),
        }
    }
}


impl Evaluate for EquivElim {
    type Evaluated = Expr;

    fn evaluate(self, ctx: &Context) -> Expr {
        let equiv_sigma = unwrap_pattern! {
            &self.equiv.type_expr; Expr::Sigma(sigma) => sigma.clone()
        };
        let equiv_left_proj = Term::new(
            Expr::left_proj(*self.equiv.clone()),
            equiv_sigma.left_type.expr.clone()
        );
        let equiv_right_type = equiv_sigma.right_type.expr.substitute_expr(equiv_left_proj.expr, 0);

        let equiv_right_type_target = unwrap_pattern! {
            &equiv_right_type; Expr::Pi(pi) => pi.target.expr.clone()
        };

        let equiv_right_type_target = equiv_right_type_target
            .substitute_expr(self.witness.expr.clone(), 0);

        let contr_elim = ContrElim::new(
            Term::new(
                Expr::app(
                    Term::new(
                        Expr::right_proj(*self.equiv),
                        equiv_right_type),
                    *self.witness),
                equiv_right_type_target),
            self.face_system.clone()
        );

        contr_elim.evaluate(ctx)
    }
}

impl Normalise for EquivElim {
    fn normalise(self, ctx: &Context) -> Expr {
        self.evaluate(ctx).normalise(ctx)
    }
}

impl Substitute for EquivElim {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        EquivElim::new(self.equiv.substitute_expr(expr.clone(), var),
            self.face_system.substitute_expr(expr.clone(), var),
            self.witness.substitute_expr(expr, var))
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        EquivElim::new(self.equiv.substitute_interval(expr.clone(), var),
            self.face_system.substitute_interval(expr.clone(), var),
            self.witness.substitute_interval(expr, var))
    }
}

impl DeBruijnIndexed for EquivElim {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        EquivElim::new(self.equiv.increment_indices_from_by(start, amount),
            self.face_system.increment_indices_from_by(start, amount),
            self.witness.increment_indices_from_by(start, amount))
    }
}

impl Display for EquivElim {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "Equiv {} {} {}", self.equiv, self.face_system, self.witness)
    }
}

