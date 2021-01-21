use crate::ast::*;
use crate::interval::{IntervalDnf, Conjunction};
use crate::face::{Face};
use crate::var_target::VarTarget;

pub fn increment_debruijn_index_in_var_target(num: usize, var_target: VarTarget) -> VarTarget {
    match var_target {
        VarTarget::Term(term) => VarTarget::Term(
            increment_debruijn_index_in_term(num, term)),
        VarTarget::Interval(int) => VarTarget::Interval(
            increment_debruijn_index_in_interval(num, int)),
        VarTarget::BoundTerm(ty) => VarTarget::BoundTerm(
            increment_debruijn_index(num, ty)),
        VarTarget::BoundInterval => VarTarget::BoundInterval,
    }
}

pub fn increment_debruijn_index_in_term(num: usize, expr: Term) -> Term {
    Term::new(
        increment_debruijn_index(num, expr.expr),
        increment_debruijn_index(num, expr.type_expr),
    )
}

pub fn increment_debruijn_index_in_interval(num: usize, interval: IntervalDnf) -> IntervalDnf {
    match interval {
        int @ IntervalDnf::Zero |
        int @ IntervalDnf::One => int,
        IntervalDnf::Conjunction(conj) => IntervalDnf::Conjunction(
            Conjunction {
                atoms: conj.atoms
                    .into_iter()
                    .map(|(var, val)| (var.increment_by(num), val))
                    .collect()
            }
        )
    }
}

pub fn increment_debruijn_index_in_face(num: usize, face: Face) -> Face {
    match face {
        Face::Top => Face::Top,
        Face::Bottom => Face::Bottom,
        Face::Conjunctions {ones, zeroes} => Face::Conjunctions {
            ones: ones.iter().map(|v| v.increment_by(num)).collect(),
            zeroes: zeroes.iter().map(|v| v.increment_by(num)).collect(),
        }
    }
}

pub fn increment_debruijn_index_in_system(num: usize, system: FaceSystem) -> FaceSystem {
    FaceSystem::from_iter(
        system.faces
            .into_iter()
            .map(|(face, expr)| (
                increment_debruijn_index_in_face(num, face),
                increment_debruijn_index(num, expr),
            ))
    )
}

pub fn increment_debruijn_index(num: usize, expr: Expr) -> Expr {
    match expr {
        Expr::Var(var) => Expr::Var(var.increment_by(num)),
        Expr::Type => Expr::Type,
        Expr::UnitType => Expr::UnitType,
        Expr::UnitVal => Expr::UnitVal,
        Expr::Pi(pi) => Expr::pi(
            increment_debruijn_index_in_term(num, *pi.source),
            increment_debruijn_index_in_term(num, *pi.target)),
        Expr::Lambda(lambda) => Expr::lambda(
            increment_debruijn_index_in_term(num, *lambda.source),
            increment_debruijn_index_in_term(num, *lambda.body)),
        Expr::App(app) => Expr::app(
            increment_debruijn_index_in_term(num, *app.func),
            increment_debruijn_index_in_term(num, *app.argument)),
        Expr::Sigma(sigma) => Expr::sigma(
            increment_debruijn_index_in_term(num, *sigma.left_type),
            increment_debruijn_index_in_term(num, *sigma.right_type)),
        Expr::Pair(pair) => Expr::pair(
            increment_debruijn_index_in_term(num, *pair.left),
            increment_debruijn_index_in_term(num, *pair.right)),
        Expr::LeftProj(lproj) => Expr::left_proj(
            increment_debruijn_index_in_term(num, *lproj.pair)),
        Expr::RightProj(rproj) => Expr::right_proj(
            increment_debruijn_index_in_term(num, *rproj.pair)),
        Expr::Path(path) => Expr::path(
            increment_debruijn_index_in_term(num, *path.space),
            increment_debruijn_index_in_term(num, *path.start),
            increment_debruijn_index_in_term(num, *path.end)),
        Expr::PathBind(path_bind) => Expr::path_bind(
            increment_debruijn_index_in_term(num, *path_bind.body)),
        Expr::PathApp(path_app) => Expr::path_app(
            increment_debruijn_index_in_term(num, *path_app.func),
            increment_debruijn_index_in_interval(num, *path_app.argument)),
        Expr::System(system) => Expr::System(
            increment_debruijn_index_in_system(num, system)),
        Expr::Comp(comp) => Expr::comp(
            increment_debruijn_index_in_term(num, *comp.space),
            increment_debruijn_index_in_system(num, comp.face_system),
            increment_debruijn_index_in_term(num, *comp.witness)),
        Expr::Fill(fill) => Expr::fill(
            fill.var.increment_by(num),
            increment_debruijn_index_in_term(num, *fill.space),
            increment_debruijn_index_in_system(num, fill.face_system),
            increment_debruijn_index_in_term(num, *fill.witness)),
        Expr::Contr(contr_elim) => Expr::contr_elim(
            increment_debruijn_index_in_term(num, *contr_elim.proof),
            increment_debruijn_index_in_system(num, contr_elim.face_system)),
        Expr::Pres(pres) => Expr::pres(
            increment_debruijn_index_in_term(num, *pres.function),
            increment_debruijn_index_in_system(num, pres.face_system),
            increment_debruijn_index_in_term(num, *pres.witness)),
    }
}
