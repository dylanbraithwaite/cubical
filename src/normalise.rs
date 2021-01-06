use crate::var_target::VarTarget;
use crate::ast::*;
use crate::context::Context;
use crate::face::{Face, faces_congruent};

pub type EvalCtx = Context;

#[derive(Debug)]
pub enum EvalErr {
    WrongVariableKind,
    ApplicableDidNotReduce,
}

pub type EvalResult<T> = Result<T, EvalErr>;

pub fn normalise_term(ctx: &EvalCtx, term: &Term) -> EvalResult<Term> {
    let expr = normalise_expr(ctx, &term.expr)?;
    let type_expr = term.type_expr.clone();
    Ok(Term::new(expr, type_expr))
}

// Assumes that type checking of expr has completed and everything is consistent so far
// TODO: Take advantage of this more and just panic in cases where bad state is just a logic error?
pub fn normalise_expr(ctx: &EvalCtx, expr: &Expr) -> EvalResult<Expr> {
    match expr {
        Expr::Var(x) => match ctx.index(x.debruijn_index).unwrap() {
            VarTarget::Term(t) => normalise_expr(ctx, &t.expr),
            VarTarget::Interval(_) => Err(EvalErr::WrongVariableKind),
            VarTarget::BoundInterval => Err(EvalErr::WrongVariableKind),
            VarTarget::BoundTerm(_) => Ok(Expr::Var(*x)),
        },
        Expr::Type => Ok(Expr::Type),
        Expr::App(app) => {
            if let Expr::Lambda(lambda) = normalise_expr(ctx, &app.func.expr)? {
                let ctx = ctx.define_term_var(
                    &"".to_owned(),
                    &app.argument
                );
                normalise_expr(&ctx, &lambda.body.expr)
            } else {
                // TODO: Should we allow irreducable applications?
                // Currently we should never be able to get into a state where we are trying to reduce something like
                // [f : A -> B, x : A]  |-  f x : B
                // without a value bound to f in the context
                Err(EvalErr::ApplicableDidNotReduce)
            }
        },
        Expr::PathApp(path_app) => {
            // TODO: If the func is a variable with type (Path A x y) then should we normalise_expr it
            // to x/y when the argument normalise_exprs to 0/1?
            // Currently no because we don't allow irriducible applications, so we can
            // always "substitute" it into the body anyway

            if let Expr::PathBind(path_bind) = normalise_expr(ctx, &path_app.func.expr)? {
                let ctx = ctx.define_interval_var(
                    &"".to_owned(),
                    *path_app.argument.clone()
                );
                normalise_expr(&ctx, &path_bind.body.expr)
            } else {
                Err(EvalErr::ApplicableDidNotReduce)
            }
        },

        // Normalise under lambdas etc so we truly have a normal form for equality checking
        Expr::Pi(pi) => {
            let source = normalise_term(ctx, &pi.source)?;

            let ctx = ctx.bind_term_var(
                &"".to_owned(),
                &source.expr
            );
            let target = normalise_term(&ctx, &pi.target)?;

            Ok(Expr::pi(source, target))
        },
        Expr::Lambda(lambda) => {
            let source = normalise_term(ctx, &lambda.source)?;

            let ctx = ctx.bind_term_var(
                &"".to_owned(),
                &source.expr
            );
            let body = normalise_term(&ctx, &lambda.body)?;

            Ok(Expr::lambda(source, body))
        },
        Expr::Path(path) => {
            let space = normalise_term(ctx, &path.space)?;
            let start = normalise_term(ctx, &path.start)?;
            let end = normalise_term(ctx, &path.end)?;
            Ok(Expr::path(space, start, end))
        },
        Expr::PathBind(path_bind) => {
            let ctx = ctx.bind_interval_var(&"".to_owned());
            let body = normalise_term(&ctx, &path_bind.body)?;

            Ok(Expr::path_bind(body))
        },
        Expr::UnitType => Ok(Expr::UnitType),
        Expr::UnitVal => Ok(Expr::UnitVal),
        Expr::System(system) => normalise_system(ctx, system),
    }
}


fn normalise_system(ctx: &EvalCtx, system: &FaceSystem) -> EvalResult<Expr> {
    for (face, expr) in &system.faces {
        if faces_congruent(ctx, face, &Face::Top) {
            return Ok(expr.clone())
        }
    }

    Ok(Expr::System(system.clone()))
}
