use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::*;
use crate::util::types::ZeroOne;
use crate::interval::{IntervalDnf, IsNegated};
use crate::context::Context;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Var(Var),
    Type,
    System(FaceSystem),

    // Types
    Pi(Pi),
    Path(Path),
    Sigma(Sigma),
    UnitType,

    // Exprs
    PathBind(PathBind),
    Lambda(Lambda),
    App(App),
    PathApp(PathApp),
    Pair(Pair),
    LeftProj(LeftProj),
    RightProj(RightProj),
    Comp(Composition),
    Contr(ContrElim),
    Equiv(EquivElim),
    Pres(Pres),
    UnitVal,
}

impl Evaluate for Expr {
    type Evaluated = Expr;

    fn evaluate(self, ctx: &Context) -> Expr {
        match self {
            Expr::Var(x) => x.evaluate(ctx).unwrap_left(),
            Expr::Type => Expr::Type,
            Expr::App(app) => app.evaluate(ctx).expr,
            Expr::PathApp(path_app) => path_app.evaluate(ctx),
            Expr::Pi(pi) => Expr::Pi(pi.evaluate(ctx)),
            Expr::Lambda(lambda) => Expr::Lambda(lambda.evaluate(ctx)),
            Expr::Sigma(sigma) => Expr::Sigma(sigma.evaluate(ctx)),
            Expr::Pair(pair) => Expr::Pair(pair.evaluate(ctx)),
            Expr::LeftProj(lproj) => lproj.evaluate(ctx).expr,
            Expr::RightProj(rproj) => rproj.evaluate(ctx).expr,
            Expr::Path(path) => Expr::Path(path.evaluate(ctx)),
            Expr::PathBind(path_bind) => Expr::PathBind(path_bind.evaluate(ctx)),
            Expr::UnitType => Expr::UnitType,
            Expr::UnitVal => Expr::UnitVal,
            Expr::System(system) => system.evaluate(ctx),
            Expr::Comp(comp) => comp.evaluate(ctx),
            Expr::Contr(contr_elim) => contr_elim.evaluate(ctx),
            Expr::Equiv(equiv_elim) => equiv_elim.evaluate(ctx),
            Expr::Pres(pres) => pres.evaluate(ctx),
        }
    }
}

impl Normalise for Expr {
    fn normalise(self, ctx: &Context) -> Expr {
        match self {
            Expr::Var(x) => x.normalise(ctx).unwrap_left(),
            Expr::Type => Expr::Type,
            Expr::App(app) => app.normalise(ctx).expr,
            Expr::PathApp(path_app) => path_app.normalise(ctx),
            Expr::Pi(pi) => Expr::Pi(pi.normalise(ctx)),
            Expr::Lambda(lambda) => Expr::Lambda(lambda.normalise(ctx)),
            Expr::Sigma(sigma) => Expr::Sigma(sigma.normalise(ctx)),
            Expr::Pair(pair) => Expr::Pair(pair.normalise(ctx)),
            Expr::LeftProj(lproj) => lproj.normalise(ctx).expr,
            Expr::RightProj(rproj) => rproj.normalise(ctx).expr,
            Expr::Path(path) => Expr::Path(path.normalise(ctx)),
            Expr::PathBind(path_bind) => Expr::PathBind(path_bind.normalise(ctx)),
            Expr::UnitType => Expr::UnitType,
            Expr::UnitVal => Expr::UnitVal,
            Expr::System(system) => system.normalise(ctx),
            Expr::Comp(comp) => comp.normalise(ctx),
            Expr::Contr(contr_elim) => contr_elim.normalise(ctx),
            Expr::Equiv(equiv_elim) => equiv_elim.normalise(ctx),
            Expr::Pres(pres) => pres.normalise(ctx),
        }
    }
}

impl Substitute for Expr {
    type ExprSubst = Expr;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        match self {
            Expr::Var(v) => v.substitute_expr(expr, var),
            Expr::Type => Expr::Type,
            Expr::UnitType => Expr::UnitType,
            Expr::UnitVal => Expr::UnitVal,
            Expr::Pi(pi) => Expr::Pi(pi.substitute_expr(expr, var)),
            Expr::Lambda(lambda) => Expr::Lambda(lambda.substitute_expr(expr, var)),
            Expr::Sigma(sigma) => Expr::Sigma(sigma.substitute_expr(expr, var)),
            Expr::Pair(pair) => Expr::Pair(pair.substitute_expr(expr, var)),
            Expr::LeftProj(lproj) => Expr::LeftProj(lproj.substitute_expr(expr, var)),
            Expr::RightProj(rproj) => Expr::RightProj(rproj.substitute_expr(expr, var)),
            Expr::Path(path) => Expr::Path(path.substitute_expr(expr, var)),
            Expr::PathBind(path_bind) => Expr::PathBind(path_bind.substitute_expr(expr, var)),
            Expr::App(app) => Expr::App(app.substitute_expr(expr, var)),
            Expr::PathApp(app) => Expr::PathApp(app.substitute_expr(expr, var)),
            Expr::System(system) => Expr::System(system.substitute_expr(expr, var)),
            Expr::Comp(comp) => Expr::Comp(comp.substitute_expr(expr, var)),
            Expr::Contr(contr_elim) => Expr::Contr(contr_elim.substitute_expr(expr, var)),
            Expr::Pres(pres) => Expr::Pres(pres.substitute_expr(expr, var)),
            Expr::Equiv(equiv_elim) => Expr::Equiv(equiv_elim.substitute_expr(expr, var)),
        }
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        match self {
            Expr::Var(self_var) =>
                match self_var.debruijn_index.cmp(&var) {
                    std::cmp::Ordering::Less => self,
                    std::cmp::Ordering::Equal => panic!("Wrong variable kind: v{}", var),
                    std::cmp::Ordering::Greater => Expr::var(self_var.debruijn_index-1),
                },
            Expr::Type => Expr::Type,
            Expr::UnitType => Expr::UnitType,
            Expr::UnitVal => Expr::UnitVal,
            Expr::Pi(pi) => Expr::Pi(pi.substitute_interval(expr, var)),
            Expr::Lambda(lambda) => Expr::Lambda(lambda.substitute_interval(expr, var)),
            Expr::Sigma(sigma) => Expr::Sigma(sigma.substitute_interval(expr, var)),
            Expr::Pair(pair) => Expr::Pair(pair.substitute_interval(expr, var)),
            Expr::LeftProj(lproj) => Expr::LeftProj(lproj.substitute_interval(expr, var)),
            Expr::RightProj(rproj) => Expr::RightProj(rproj.substitute_interval(expr, var)),
            Expr::Path(path) => Expr::Path(path.substitute_interval(expr, var)),
            Expr::PathBind(path_bind) => Expr::PathBind(path_bind.substitute_interval(expr, var)),
            Expr::App(app) => Expr::App(app.substitute_interval(expr, var)),
            Expr::PathApp(app) => Expr::PathApp(app.substitute_interval(expr, var)),
            Expr::System(system) => Expr::System(system.substitute_interval(expr, var)),
            Expr::Comp(comp) => Expr::Comp(comp.substitute_interval(expr, var)),
            Expr::Contr(contr_elim) => Expr::Contr(contr_elim.substitute_interval(expr, var)),
            Expr::Pres(pres) => Expr::Pres(pres.substitute_interval(expr, var)),
            Expr::Equiv(equiv_elim) => Expr::Equiv(equiv_elim.substitute_interval(expr, var)),
        }
    }
}

impl DeBruijnIndexed for Expr {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        match self {
            Expr::Var(var) => Expr::Var(var.increment_indices_from_by(start, amount)),
            Expr::Type => Expr::Type,
            Expr::UnitType => Expr::UnitType,
            Expr::UnitVal => Expr::UnitVal,
            Expr::Pi(pi) => Expr::Pi(pi.increment_indices_from_by(start, amount)),
            Expr::Lambda(lambda) => Expr::Lambda(lambda.increment_indices_from_by(start, amount)),
            Expr::App(app) => Expr::App(app.increment_indices_from_by(start, amount)),
            Expr::Sigma(sigma) => Expr::Sigma(sigma.increment_indices_from_by(start, amount)),
            Expr::Pair(pair) => Expr::Pair(pair.increment_indices_from_by(start, amount)),
            Expr::LeftProj(lproj) => Expr::LeftProj(lproj.increment_indices_from_by(start, amount)),
            Expr::RightProj(rproj) => Expr::RightProj(rproj.increment_indices_from_by(start, amount)),
            Expr::Path(path) => Expr::Path(path.increment_indices_from_by(start, amount)),
            Expr::PathBind(path_bind) => Expr::PathBind(path_bind.increment_indices_from_by(start, amount)),
            Expr::PathApp(path_app) => Expr::PathApp(path_app.increment_indices_from_by(start, amount)),
            Expr::System(system) => Expr::System(system.increment_indices_from_by(start, amount)),
            Expr::Comp(comp) => Expr::Comp(comp.increment_indices_from_by(start, amount)),
            Expr::Contr(contr_elim) => Expr::Contr(contr_elim.increment_indices_from_by(start, amount)),
            Expr::Pres(pres) => Expr::Pres(pres.increment_indices_from_by(start, amount)),
            Expr::Equiv(equiv_elim) => Expr::Equiv(equiv_elim.increment_indices_from_by(start, amount)),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        match self {
            Expr::Var(v) => write!(f, "{}", v),
            Expr::Type => write!(f, "Type"),
            Expr::UnitType => write!(f, "Unit"),
            Expr::UnitVal => write!(f, "()"),
            Expr::Pi(pi) => write!(f, "{}", pi),
            Expr::Sigma(sigma) => write!(f, "{}", sigma),
            Expr::Lambda(lambda) => write!(f, "{}", lambda),
            Expr::Path(path) => write!(f, "{}", path),
            Expr::PathBind(path_bind) => write!(f, "{}", path_bind),
            Expr::App(app) => write!(f, "{}", app),
            Expr::Pair(pair) => write!(f, "{}", pair),
            Expr::LeftProj(left_proj) => write!(f, "{}", left_proj),
            Expr::RightProj(right_proj) => write!(f, "{}", right_proj),
            Expr::PathApp(app) => write!(f, "{}", app),
            Expr::System(system) => write!(f, "{}", system),
            Expr::Comp(comp) => write!(f, "{}", comp),
            Expr::Contr(contr_elim) => write!(f, "{}", contr_elim),
            Expr::Equiv(equiv_elim) => write!(f, "{}", equiv_elim),
            Expr::Pres(pres) => write!(f, "{}", pres),
        }
    }
}

impl Expr {
    pub fn var(debruijn_index: usize) -> Expr {
        Expr::Var(Var::new(debruijn_index))
    }

    pub fn system<T>(faces: T) -> Expr
        where T: Into<Vec<(Face, Expr)>>
    {
        Expr::System(FaceSystem::new(faces.into()))
    }

    pub fn pi(source: Term, target: Term) -> Expr {
        Expr::Pi(Pi::new(source, target))
    }

    pub fn path(space: Term, start: Term, end: Term) -> Expr {
        Expr::Path(Path::new(space, start, end))
    }

    pub fn path_bind(body: Term) -> Expr {
        Expr::PathBind(PathBind::new(body))
    }

    pub fn lambda(space: Term, body: Term) -> Expr {
        Expr::Lambda(Lambda::new(space, body))
    }

    pub fn app(func: Term, arg: Term) -> Expr {
        Expr::App(App::new(func, arg))
    }

    pub fn path_app(func: Term, arg: IntervalDnf) -> Expr {
        Expr::PathApp(PathApp::new(func, arg))
    }

    pub fn comp(space: Term, face_system: FaceSystem, witness: Term) -> Expr {
        Expr::Comp(Composition::new(space, face_system, witness))
    }

    pub fn fill(var: Var, space: Term, face_system: FaceSystem, witness: Term) -> Expr {
        let degeneracy = IntervalDnf::meet(
            IntervalDnf::single(var.increment_indices(), IsNegated::NotNegated),
            IntervalDnf::single(Var::new(0), IsNegated::NotNegated)
        );

        let mut face_system = face_system
            .increment_indices()
            .substitute_interval(degeneracy.clone(), var.debruijn_index+1);
        let new_face = Face::with_sides(&[(var, ZeroOne::Zero)]);
        face_system.faces.push((new_face, witness.expr.clone()));

        let space = space
            .increment_indices()
            .substitute_interval(degeneracy, var.debruijn_index+1);

        Expr::comp(space, face_system, witness)
    }

    // transp i A a = comp i A [] a
    pub fn transp(space: Term, witness: Term) -> Expr {
        let face_system = FaceSystem::new([]);
        Expr::Comp(Composition::new(space, face_system, witness))
    }

    /// Constructs a term
    /// `isContr A = Sigma (x: A) (Pi (y: A) -> Path A x y)`
    /// representing the contractibility of a space.
    pub fn contr_prop(space: Term) -> Expr {
        Expr::sigma (
            space.clone(),
            Term::new_type(Expr::pi (
                space.clone(),
                Term::new_type(Expr::path (
                    space.clone(),
                    Term::new(Expr::var(1), space.expr.clone()),
                    Term::new(Expr::var(0), space.expr)
                ))
            ))
        )
    }

    pub fn contr_elim(proof: Term, system: FaceSystem) -> Expr {
        Expr::Contr(ContrElim::new(proof, system))
    }

    pub fn sigma(left_type: Term, right_type: Term) -> Expr {
        Expr::Sigma(Sigma::new(left_type, right_type))
    }

    pub fn pair(left: Term, right: Term) -> Expr {
        Expr::Pair(Pair::new(left, right))
    }

    pub fn left_proj(pair: Term) -> Expr {
        Expr::LeftProj(LeftProj::new(pair))
    }

    pub fn right_proj(pair: Term) -> Expr {
        Expr::RightProj(RightProj::new(pair))
    }

    pub fn pres(function: Term, face_system: FaceSystem, witness: Term) -> Expr {
        Expr::Pres(Pres::new(function, face_system, witness))
    }

    /// Gives a term:
    /// isContr (Sigma (x: Source) Path A y (f x))
    /// in the context [ y: A ]
    fn equiv_prop_inner(source: Term, target:Term, func: Term) -> Expr {
        Expr::contr_prop(
            Term::new_type(Expr::sigma(
                source.clone(),
                Term::new_type(Expr::path(
                    target.clone(),
                    Term::new(Expr::var(1), target.expr.clone()),
                    Term::new(
                        Expr::app(
                            func,
                            Term::new(Expr::var(0), source.expr.clone())
                        ),
                        target.expr.clone()
                    )
                ))
            ))
        )
    }

    /// isEquiv T A f = Pi (y: A) -> isContr (Sigma (x: T) Path A y (f x))
    pub fn equiv_prop(source: Term, target: Term, func: Term) -> Expr {
        Expr::pi(
            target.clone(),
            Term::new_type(Expr::equiv_prop_inner(source, target, func))
        )
    }

    /// Equiv T A = Sigma (f: T -> A) isEquiv T A f
    pub fn equiv_type(source: Term, target: Term) -> Expr {
        let func_ty = Expr::pi(source.clone(), target.clone());
        Expr::sigma(
            Term::new_type(func_ty.clone()),
            Term::new_type(Expr::equiv_prop(
                source,
                target,
                Term::new(Expr::var(0), func_ty)
            ))
        )
    }

    pub fn equiv_elim(equiv: Term, system: FaceSystem, witness: Term) -> Expr {
        Expr::Equiv(EquivElim::new(equiv, system, witness))
    }
}
