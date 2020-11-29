use itertools::Itertools;

use crate::var_target::VarTarget;
use crate::ast::*;
use crate::interval::{Interval, IntervalDnf};
use crate::face::Face;

use crate::syntax::expr::Expr as Syntax;
use crate::syntax::expr::Face as FaceSyntax;

use crate::normalise::{normalise_expr, normalise_term};

use crate::context::Context;

#[derive(Debug)]
pub enum ElaborationErr {
    UnboundVariable,
    WrongVariableKind,
    TypeInTermPosition,
    TermInTypePosition,
    IntervalInValuePosition,
    IntervalInTypePosition,
    WrongFunctionArgumentType,
    ExpectedApplicableExpression,
    ExpectedInterval,
    PathEndpointHasWrongType,
    PartialFaceSystem,
    IncompatibleFaces,
    IncompatibleFaceTypes,
}

type ElaborationResult<T> = Result<T, ElaborationErr>;

pub fn elaborate_type(ctx: &Context, syntax: &Syntax) -> ElaborationResult<Term> {
    match syntax {
        Syntax::Var(name) => elaborate_type_var(ctx, name),
        Syntax::Pi((var_name, source, target)) => elaborate_pi(ctx, var_name, source, target),
        Syntax::Path(space, start, end) => elaborate_path(ctx, space, start, end),
        Syntax::UnitType => Ok(Term::new_type(Expr::UnitType)),
        Syntax::Lambda(_)
        | Syntax::App(_,_)
        | Syntax::PathBind(_,_)
        | Syntax::UnitVal => Err(ElaborationErr::TermInTypePosition),
        Syntax::IntervalInvolution(_)
        | Syntax::IntervalJoin(_,_)
        | Syntax::IntervalMeet(_,_)
        | Syntax::IntervalOne
        | Syntax::IntervalZero => Err(ElaborationErr::IntervalInTypePosition),
        Syntax::System(system) => elaborate_face_system(ctx, system),
    }
}

pub fn elaborate_term(ctx: &Context, syntax: &Syntax) -> ElaborationResult<Term> {
    match syntax {
        Syntax::Var(name) => elaborate_term_var(ctx, name),
        Syntax::Lambda((var, source, body)) => elaborate_lambda(ctx, var, source, body),
        Syntax::PathBind(var, body) => elaborate_path_bind(ctx, var, body),
        Syntax::App(e1, e2) => elaborate_app(ctx, e1, e2),
        Syntax::UnitVal => Ok(Term::new(Expr::UnitVal, Expr::UnitType)),
        Syntax::Pi(_)
        | Syntax::Path(_,_,_)
        | Syntax::UnitType => Err(ElaborationErr::TypeInTermPosition),
        Syntax::IntervalInvolution(_)
        | Syntax::IntervalJoin(_,_)
        | Syntax::IntervalMeet(_,_)
        | Syntax::IntervalOne
        | Syntax::IntervalZero => Err(ElaborationErr::IntervalInValuePosition),
        Syntax::System(system) => elaborate_face_system(ctx, system),
    }
}

/*
Typing rule for a face system: [f1: e1, f2: e2, ..., fn: en]

      ctx, fi ⊢                            ei : A
  ctx, fi, fj ⊢                      ei == ej : A
          ctx ⊢        f1 or f2 or .. fn == 1 : Face
------------------------------------------------------
          ctx ⊢ [f1: e1, f2: e2, ..., fn: en] : A
*/
fn elaborate_face_system(ctx: &Context, system: &[(FaceSyntax, Syntax)]) -> ElaborationResult<Term> {
    if !face_system_is_total(ctx, system) {
        return Err(ElaborationErr::PartialFaceSystem)
    }

    // Elaborate the face exprs and check every face in the face system has the same
    // type.
    let mut system_type = None;
    let mut faces = Vec::new();

    for (face, expr) in system {
        let mut sides_vec = Vec::new();
        for (name, zero_one) in face {
            match ctx.debruijnify(name) {
                Some((idx, VarTarget::BoundInterval)) => sides_vec.push((Var::new(idx), *zero_one)),
                _ => return Err(ElaborationErr::WrongVariableKind),
            }
        }
        let face = Face::with_sides(sides_vec);

        let ctx = &ctx.with_face_restriction(&face);
        let elaborated_side = elaborate_term(ctx, expr)?;

        let to_compare = system_type.get_or_insert_with(|| elaborated_side.type_expr.clone());
        if !exprs_equal_nf(ctx, to_compare, &elaborated_side.type_expr) {
            return Err(ElaborationErr::IncompatibleFaceTypes)
        }

        faces.push((face.clone(), elaborated_side.expr));
    }

    // Check that partially defined elements agree where they overlap
    for ((f1, e1), (f2, e2)) in faces.iter().tuple_combinations() {
        let ctx = &ctx
            .with_face_restriction(f1)
            .with_face_restriction(f2);

        if !exprs_equal_nf(ctx, e1, e2) {
           return Err(ElaborationErr::IncompatibleFaces)
        }
    }

    let face_system = FaceSystem::new(faces);
    let expr = Expr::System(face_system);
    // TODO
    // This fails on empty systems, but we don't do any type inference yet, so can't
    // do anything meaningful there without adding some type annotation syntax
    Ok(Term::new(expr, system_type.unwrap()))
}

// FIXME
/// UNIMPLEMENTED - Checks that the face formula consisting of the disjunction
/// over all faces in the system, is congruent to the top element of the face
/// lattice, modulo ctx.
fn face_system_is_total(_ctx: &Context, _system: &[(FaceSyntax, Syntax)]) -> bool {
    true
}

fn elaborate_term_var(ctx: &Context, var: &str) -> ElaborationResult<Term> {
    match ctx.debruijnify(var) {
        Some((index, VarTarget::BoundTerm(ty))) => {
            let expr = Expr::Var(Var::new(index));
            let term = Term::new(expr, ty);
            Ok(term)
        },

        Some((index, VarTarget::Term(term))) => {
            let expr = Expr::Var(Var::new(index));
            let term = Term::new(expr, term.type_expr);
            Ok(term)
        },
        Some(_) => Err(ElaborationErr::WrongVariableKind),
        None => Err(ElaborationErr::UnboundVariable),
    }
}

fn elaborate_interval_var(ctx: &Context, var: &str) -> ElaborationResult<Interval> {
    match ctx.debruijnify(var) {
        Some((index, VarTarget::BoundInterval))
        | Some((index, VarTarget::Interval(_))) => {
            let var = Var::new(index);
            let interval = Interval::Var(var);
            Ok(interval)
        },
        Some(_) => Err(ElaborationErr::WrongVariableKind),
        None => Err(ElaborationErr::UnboundVariable),
    }
}

fn elaborate_type_var(ctx: &Context, var: &str) -> ElaborationResult<Term> {
    match ctx.debruijnify(var) {
        Some((index, VarTarget::BoundTerm(Expr::Type)))
        // TODO: This doesn't work for type aliases:
        | Some((index, VarTarget::Term(_))) => {
            let var = Var::new(index);
            let expr = Expr::Var(var);
            Ok(Term::new_type(expr))
        }
        Some(_) => Err(ElaborationErr::WrongVariableKind),
        None => Err(ElaborationErr::UnboundVariable),
    }
}

fn elaborate_pi(ctx: &Context, var: &str, source: &Syntax, target: &Syntax)
    -> ElaborationResult<Term>
{
    let source = elaborate_type(ctx, source)?;
    let target = elaborate_type(
        &ctx.bind_term_var(var, &source.expr),
        target
    )?;

    let pi = Pi::new(source, target);
    let expr = Expr::Pi(pi);

    Ok(Term::new_type(expr))
}

fn elaborate_lambda(ctx: &Context, var: &str, source: &Syntax, body: &Syntax)
    -> ElaborationResult<Term>
{
    let source = elaborate_type(ctx, source)?;
    let body = elaborate_term(
        &ctx.bind_term_var(var, &source.expr),
        body
    )?;

    let body_type = Term::new_type(body.type_expr.clone());
    let pi = Pi::new(source.clone(), body_type);
    let lambda_type_expr = Expr::Pi(pi);

    let lambda = Lambda::new(source, body);
    let lambda_expr = Expr::Lambda(lambda);

    Ok(Term::new(lambda_expr, lambda_type_expr))
}

fn elaborate_app(ctx: &Context, e1: &Syntax, e2: &Syntax) -> ElaborationResult<Term> {
    let e1 = elaborate_term(ctx, e1)?;

    // TODO: normalise?
    match &e1.type_expr {
        Expr::Pi(pi) => {
            let e2 = elaborate_term(ctx, e2)?;
            if !exprs_equal_nf(ctx, &pi.source.expr, &e2.type_expr) {
                return Err(ElaborationErr::WrongFunctionArgumentType);
            }

            let expr_ty = pi.target.expr.clone();

            let appl = App::new(e1, e2);
            let expr = Expr::App(appl);
            let term = Term::new(expr, expr_ty);

            Ok(term)
        },
        Expr::Path(path) => {
            let e2 = elaborate_interval(ctx, e2)?;
            let expr_ty = path.space.expr.clone();

            let appl = PathApp::new(e1, e2);
            let expr = Expr::PathApp(appl);
            let term = Term::new(expr, expr_ty);

            Ok(term)
        }
        _ => {
            Err(ElaborationErr::ExpectedApplicableExpression)
        }
    }
}

fn elaborate_path(ctx: &Context, space: &Syntax, start: &Syntax, end: &Syntax) -> ElaborationResult<Term> {
    let space = elaborate_type(ctx, space)?;
    let start = elaborate_term(ctx, start)?;
    let end = elaborate_term(ctx, end)?;

    if start.type_expr != space.expr || end.type_expr != space.expr {
        return Err(ElaborationErr::PathEndpointHasWrongType);
    }

    let path_type = Path::new(space, start, end);
    let type_expr = Expr::Path(path_type);

    Ok(Term::new_type(type_expr))
}

fn elaborate_path_bind(ctx: &Context, var: &str, body: &Syntax) -> ElaborationResult<Term> {
    let body = elaborate_term(
        &ctx.bind_interval_var(var),
        body
    )?;

    let path_space = Term::new_type(body.type_expr.clone());

    let path_start = {
        let ctx = ctx.define_interval_var(var, IntervalDnf::Zero);
        normalise_term(&ctx, &body).unwrap()
    };

    let path_end = {
        let ctx = ctx.define_interval_var(var, IntervalDnf::One);
        normalise_term(&ctx, &body).unwrap()
    };

    let path_type = Path::new(path_space, path_start, path_end);
    let path_type_expr = Expr::Path(path_type);

    let path_bind = PathBind::new(body);
    let expr = Expr::PathBind(path_bind);

    Ok(Term::new(expr, path_type_expr))
}

fn parse_interval_from_expr(ctx: &Context, expr: &Syntax) -> ElaborationResult<Interval> {
    match expr {
        Syntax::Var(var) => elaborate_interval_var(ctx, var),
        Syntax::IntervalInvolution(e) => {
            let e = Box::new(parse_interval_from_expr(ctx, e)?);
            Ok(Interval::Involution(e))
        },
        Syntax::IntervalMeet(e1, e2) => {
            let e1 = Box::new(parse_interval_from_expr(ctx, e1)?);
            let e2 = Box::new(parse_interval_from_expr(ctx, e2)?);
            Ok(Interval::Meet(e1, e2))
        },
        Syntax::IntervalJoin(e1, e2) => {
            let e1 = Box::new(parse_interval_from_expr(ctx, e1)?);
            let e2 = Box::new(parse_interval_from_expr(ctx, e2)?);
            Ok(Interval::Join(e1, e2))
        },
        Syntax::IntervalOne => Ok(Interval::One),
        Syntax::IntervalZero => Ok(Interval::Zero),
        _ => Err(ElaborationErr::ExpectedInterval),
    }
}

fn elaborate_interval(ctx: &Context, expr: &Syntax) -> ElaborationResult<IntervalDnf> {
    let interval = parse_interval_from_expr(ctx, expr)?;
    Ok(IntervalDnf::new(interval))
}

// Normalises exprs and compares them
fn exprs_equal_nf(ctx: &Context, e1: &Expr, e2: &Expr) -> bool {
    if ctx.faces_inconsistent() { return true }

    let e1 = normalise_expr(&ctx, e1).unwrap();
    let e2 = normalise_expr(&ctx, e2).unwrap();

    e1 == e2
}