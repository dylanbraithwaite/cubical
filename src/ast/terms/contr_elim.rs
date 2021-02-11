use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr, FaceSystem, Var};
use crate::context::Context;
use crate::interval::{IntervalDnf, IsNegated};
use crate::unwrap_pattern;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ContrElim {
    /// A proof of contractability is a term of type
    /// `Sigma (x: A) (Pi (y: A) -> Path A x y)`
    pub proof: Box<Term>,

    pub face_system: FaceSystem,
}

impl ContrElim {
    pub fn new(proof: Term, face_system: FaceSystem) -> Self {
        ContrElim {
            proof: Box::new(proof),
            face_system,
        }
    }
}

impl Evaluate for ContrElim {
    type Evaluated = Expr;

    fn evaluate(self, ctx: &Context) -> Self::Evaluated {
        // proof : (x: A) * ((y: A) -> Path A x y)
        let proof = self.proof.evaluate(ctx);

        let proof_ty = unwrap_pattern! {
            &proof.type_expr; Expr::Sigma(sigma) => sigma.clone()
        };

        let proof_lproj = Term::new(
            Expr::left_proj(proof.clone()),
            proof_ty.left_type.expr.clone());

        // The right hand type is dependent on the left. Subst the left projection
        // into right hand type to get a concrete type.
        let proof_right_type = proof_ty.right_type.substitute_expr(proof_lproj.expr.clone(), 0);

        let proof_rproj = Term::new(
            Expr::right_proj(proof),
            proof_right_type.expr);

        let space = *proof_ty.left_type;

        let face_system = FaceSystem::from_iter(
            self.face_system.faces
                .into_iter()
                .map(|(face, expr)| {
                    let contr_path = Expr::app(
                        proof_rproj.clone(),
                        Term::new(expr, space.expr.clone())
                    );
                    let contr_path_ty = unwrap_pattern! {
                        &proof_rproj.type_expr; Expr::Pi(pi) => pi.target.expr.clone()
                    };
                    let contr = Expr::path_app(
                        Term::new(contr_path, contr_path_ty),
                        IntervalDnf::single(Var::new(0), IsNegated::NotNegated)
                    );
                    (face, contr)
                })
        );

        Expr::comp(space, face_system, proof_lproj)
    }
}

impl Normalise for ContrElim {
    fn normalise(self, ctx: &Context) -> Self::Evaluated {
        self.evaluate(ctx).normalise(ctx)
    }
}

impl Substitute for ContrElim {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self::ExprSubst {
        ContrElim::new(self.proof.substitute_expr(expr.clone(), var),
            self.face_system.substitute_expr(expr, var))
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        ContrElim::new(self.proof.substitute_interval(expr.clone(), var),
            self.face_system.substitute_interval(expr, var))
    }
}

impl DeBruijnIndexed for ContrElim {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        ContrElim::new(self.proof.increment_indices_from_by(start, amount),
            self.face_system.increment_indices_from_by(start, amount))
    }
}

impl Display for ContrElim {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "Contr {} {}", self.proof, self.face_system)
    }
}

