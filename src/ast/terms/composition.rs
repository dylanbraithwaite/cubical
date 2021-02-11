use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr, FaceSystem, Path, Var, Sigma};
use crate::context::Context;
use crate::interval::{IntervalDnf, IsNegated};
use crate::util::types::ZeroOne;
use crate::face::Face;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Composition {
    pub face_system: FaceSystem,
    pub space: Box<Term>,
    /// The witness at i=0 to the partial terms in the face system being extensible.
    pub witness: Box<Term>,
}

impl Composition {
    pub fn new(space: Term, face_system: FaceSystem, witness: Term) -> Self {
        Composition {
            space: Box::new(space),
            face_system: face_system,
            witness: Box::new(witness),
        }
    }
}

impl Evaluate for Composition {
    type Evaluated = Expr;

    fn evaluate(self, ctx: &Context) -> Expr {
        let bound_ctx = &ctx.bind_interval_var("");

        let space = self.space.evaluate(bound_ctx);
        let system = self.face_system.evaluate(bound_ctx);
        let witness = self.witness.evaluate(ctx);

        if let Expr::System(system) = system {
            match space.expr {
                // pi types are handled in App::normalise
                Expr::Path(path) => eval_path_comp(path, system, witness),
                Expr::Sigma(sigma) => eval_sigma_comp(sigma, system, witness),
                _ => Expr::comp(space, system, witness)
            }
        } else {
            system.evaluate(&ctx.define_interval_var("", IntervalDnf::One))
        }
    }
}

impl Normalise for Composition {
    fn normalise(self, ctx: &Context) -> Expr {
        match self.evaluate(ctx) {
            Expr::Comp(comp) => {
                let bound_ctx = &ctx.bind_interval_var("");
                Expr::comp(comp.space.normalise(bound_ctx),
                    comp.face_system,
                    comp.witness.normalise(ctx))
            },
            other => other.normalise(ctx)
        }
    }
}

#[test]
fn test_normalise_composition_unit() {
    // Path Unit a b
    let path_type = Expr::path(
        Term::new(Expr::UnitType, Expr::Type),
        Term::new(Expr::var(1), Expr::UnitType),
        Term::new(Expr::var(0), Expr::UnitType)
    );

    //[a: Unit, b: Unit, p: Path Unit a b]
    let ctx = Context::new()
        .bind_term_var("a", &Expr::UnitType)
        .bind_term_var("b", &Expr::UnitType)
        .bind_term_var("p", &path_type);

    // Comp Unit [1 -> p i] a
    let expr = Expr::comp(
        Term::new(Expr::UnitType, Expr::Type),
        FaceSystem::new([
            (Face::Top, Expr::path_app(
                Term::new(Expr::var(1), path_type),
                IntervalDnf::single(Var::new(0), IsNegated::NotNegated)
            ))
        ]),
        Term::new(Expr::var(2), Expr::UnitType),
    );

    let expr = expr.normalise(&ctx);
    assert_eq!(expr, Expr::var(1))
}

impl Substitute for Composition {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        // Leave space for a new interval var
        let expr_line = expr.clone().increment_indices();
        Composition::new(self.space.substitute_expr(expr_line.clone(), var+1),
            self.face_system.substitute_expr(expr_line, var+1),
            self.witness.substitute_expr(expr, var))
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        let expr_line = expr.clone().increment_indices();
        Composition::new(self.space.substitute_interval(expr_line, var+1),
            self.face_system.substitute_interval(expr.clone(), var),
            self.witness.substitute_interval(expr, var))
    }
}

impl DeBruijnIndexed for Composition {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        Composition::new(self.space.increment_indices_from_by(start+1, amount),
            self.face_system.increment_indices_from_by(start+1, amount),
            self.witness.increment_indices_from_by(start, amount))
    }
}

impl Display for Composition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "Comp _ ({}) {} ({})", self.space, self.face_system, self.witness)
    }
}

// TODO: Tidy this up a bit
fn eval_path_comp(path_expr: Path, face_system: FaceSystem,
        witness: Term) -> Expr
{
    let path_i1 = path_expr.clone().substitute_interval(IntervalDnf::One, 1);
    let space_i1 = path_expr.space.expr.clone().substitute_interval(IntervalDnf::One, 1);

    let j_int = IntervalDnf::single(Var::new(0), IsNegated::NotNegated);
    let system = FaceSystem::from_iter(
        face_system.faces
            .iter()
            .cloned()
            .map(|(face, expr)| (face, Expr::path_app(
                Term::new(expr, Expr::Path(path_expr.clone())),
                j_int.clone()
            )))
            .chain(vec![
                (Face::with_sides(&[(Var::new(0), ZeroOne::Zero)]), path_expr.start.expr.clone()),
                (Face::with_sides(&[(Var::new(0), ZeroOne::One)]), path_expr.end.expr.clone()),
            ])
    );

    Expr::path_bind(
        Term::new(
            Expr::comp(
                path_expr.space.increment_indices(),
                system,
                Term::new(
                    Expr::path_app(witness.clone(), j_int),
                    space_i1
                )
            ),
            Expr::Path(path_i1)
        )
    )
}

fn eval_sigma_comp(sigma: Sigma, face_system: FaceSystem,
        witness: Term) -> Expr
{
    let left_type = *sigma.left_type.clone();
    let right_type = *sigma.right_type.clone();

    let left_system = FaceSystem::from_iter(
        face_system.faces
            .iter()
            .cloned()
            .map(|(face, expr)| (face,
                Expr::left_proj(Term::new(
                    expr,
                    Expr::Sigma(sigma.clone())
                ))
            ))
    );

    let right_system = FaceSystem::from_iter(
        face_system.faces
            .iter()
            .cloned()
            .map(|(face, expr)| (face,
                Expr::right_proj(Term::new(
                    expr,
                    Expr::Sigma(sigma.clone())
                ))
            ))
    );

    let witness_left_proj = Term::new(Expr::left_proj(witness.clone()), left_type.expr.clone());
    let witness_right_proj = Term::new(Expr::left_proj(witness.clone()), left_type.expr.clone());

    let a = Expr::fill(
        Var::new(0),
        left_type.clone(),
        left_system.clone(),
        witness_left_proj.clone(),
    );

    let right_type_subbed = right_type.substitute_expr(a, 0);

    let left_comp_ty = left_type.expr.clone().substitute_interval(IntervalDnf::One, 0);
    let right_comp_ty = right_type_subbed.expr.clone().substitute_interval(IntervalDnf::One, 0);

    let left_comp = Expr::comp(
        left_type.clone(),
        left_system.clone(),
        witness_left_proj.clone()
    );
    let right_comp = Expr::comp(
        right_type_subbed,
        right_system,
        witness_right_proj.clone()
    );

    Expr::pair(
        Term::new(left_comp, left_comp_ty),
        Term::new(right_comp, right_comp_ty)
    )
}
