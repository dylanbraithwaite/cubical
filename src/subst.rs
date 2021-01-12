use crate::ast::*;
use crate::interval::{IntervalDnf, IsNegated };
use crate::face::{Face, expand_interval_into_face};

fn subst_interval_in_interval(expr: IntervalDnf, var: Var, interval: IntervalDnf) -> IntervalDnf {
    match expr {
        IntervalDnf::One => IntervalDnf::One,
        IntervalDnf::Zero => IntervalDnf::Zero,
        IntervalDnf::Conjunction(mut terms) => {
            if terms.atoms.remove(&(var, IsNegated::Negated)) {
                unimplemented!("Interval joins not fully implemented")
            } else if terms.atoms.remove(&(var, IsNegated::NotNegated)) {
                IntervalDnf::meet(interval, IntervalDnf::Conjunction(terms))
            } else {
                IntervalDnf::Conjunction(terms)
            }
        }
    }
}

pub fn subst_interval_in_term(term: Term, var: Var, interval: IntervalDnf) -> Term {
    Term::new(
        subst_interval_in_expr(term.expr, var, interval.clone()),
        subst_interval_in_expr(term.type_expr, var, interval))
}

fn subst_interval_in_face(mut face: Face, var: Var, interval: IntervalDnf) -> Face {
    match &mut face {
        Face::Top => Face::Top,
        Face::Bottom => Face::Bottom,
        Face::Conjunctions { ones, zeroes } => {
            if ones.remove(&var) {
                Face::meet(face, expand_interval_into_face(&interval, false))
            } else if zeroes.remove(&var) {
                Face::meet(face, expand_interval_into_face(&interval, true))
            } else {
                face
            }
        }
    }
}

fn subst_interval_in_system(system: FaceSystem, var: Var, interval: IntervalDnf) -> FaceSystem {
    FaceSystem::from_iter(
        system.faces
            .into_iter()
            .map(|(face, expr)| (
                subst_interval_in_face(face, var, interval.clone()),
                subst_interval_in_expr(expr, var, interval.clone())
            ))
    )
}

pub fn subst_interval_in_expr(expr: Expr, var: Var, interval: IntervalDnf) -> Expr {
    match expr {
        Expr::Var(v) => Expr::Var(v),
        Expr::Type => Expr::Type,
        Expr::UnitType => Expr::UnitType,
        Expr::UnitVal => Expr::UnitVal,
        Expr::Pi(pi) => Expr::pi(
            subst_interval_in_term(*pi.source, var, interval.clone()),
            subst_interval_in_term(*pi.target, var.increment(), interval)),
        Expr::Lambda(lambda) => Expr::lambda(
            subst_interval_in_term(*lambda.source, var, interval.clone()),
            subst_interval_in_term(*lambda.body, var.increment(), interval)),
        Expr::Sigma(sigma) => Expr::sigma(
            subst_interval_in_term(*sigma.left_type, var, interval.clone()),
            subst_interval_in_term(*sigma.right_type, var.increment(), interval)),
        Expr::Pair(pair) => Expr::pair(
            subst_interval_in_term(*pair.left, var, interval.clone()),
            subst_interval_in_term(*pair.right, var, interval)),
        Expr::LeftProj(lproj) => Expr::left_proj(
            subst_interval_in_term(*lproj.pair, var, interval)),
        Expr::RightProj(rproj) => Expr::right_proj(
            subst_interval_in_term(*rproj.pair, var, interval)),
        Expr::Path(path) => Expr::path(
            subst_interval_in_term(*path.space, var, interval.clone()),
            subst_interval_in_term(*path.start, var, interval.clone()),
            subst_interval_in_term(*path.end, var, interval.clone())),
        Expr::PathBind(path_bind) => Expr::path_bind(
            subst_interval_in_term(*path_bind.body, var.increment(), interval)),
        Expr::App(app) => Expr::app(
            subst_interval_in_term(*app.func, var, interval.clone()),
            subst_interval_in_term(*app.argument, var, interval)),
        Expr::PathApp(app) => Expr::path_app(
            subst_interval_in_term(*app.func, var, interval.clone()),
            subst_interval_in_interval(*app.argument, var, interval)),
        Expr::System(system) => Expr::System(
            subst_interval_in_system(system, var, interval)),
        Expr::Comp(comp) => Expr::comp(
            subst_interval_in_term(*comp.space, var, interval.clone()),
            subst_interval_in_system(comp.face_system, var, interval.clone()),
            subst_interval_in_term(*comp.witness, var, interval)),
        Expr::Fill(kan_fill) => Expr::fill(
            kan_fill.var,
            subst_interval_in_term(*kan_fill.space, var, interval.clone()),
            subst_interval_in_system(kan_fill.face_system, var, interval.clone()),
            subst_interval_in_term(*kan_fill.witness, var, interval))
    }
}
