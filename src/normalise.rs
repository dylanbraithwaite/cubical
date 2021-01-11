use crate::var_target::VarTarget;
use crate::ast::*;
use crate::context::Context;
use crate::face::{Face, faces_congruent};
use crate::interval::{IntervalDnf, IsNegated, normalise_interval};
use crate::subst::{subst_interval_in_expr, subst_interval_in_term};
use crate::util::types::ZeroOne;
use crate::debruijn::*;

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
        }
        Expr::Type => Ok(Expr::Type),
        Expr::App(app) => {
            match normalise_expr(ctx, &app.func.expr)? {
                Expr::Lambda(lambda) => {
                    let ctx = ctx.define_term_var("", &app.argument);
                    normalise_expr(&ctx, &lambda.body.expr)
                },
                Expr::Comp(comp) => {
                    // TODO: Needs tidied a lot!
                    if let Expr::Pi(pi) = &comp.space.expr {
                        let w = Expr::fill(
                            Var::new(0),
                            subst_interval_in_term(
                                *pi.source.clone(),
                                Var::new(0),
                                IntervalDnf::single(Var::new(0), IsNegated::Negated)),
                            FaceSystem::new([]),
                            *app.argument.clone()
                        );
                        let w = normalise_expr(ctx, &w)?;

                        let v = subst_interval_in_expr(w, Var::new(0),
                            IntervalDnf::single(Var::new(0), IsNegated::Negated));
                        let v = normalise_expr(ctx, &v)?;

                        let v_term = Term::new(v.clone(), pi.source.expr.clone());

                        let ctx_i = ctx.bind_interval_var("");
                        let target_subst = normalise_term(
                            &ctx_i.define_term_var("", &Term::new(
                                increment_debruijn_index(1, v.clone()),
                                increment_debruijn_index(1, pi.source.expr.clone()),
                            )),
                            &pi.target
                        )?;
                        let system = FaceSystem::from_iter(
                            comp.face_system.faces
                                .iter()
                                .cloned()
                                .map(|(face, expr)| (face, Expr::app(
                                    Term::new(expr, comp.space.expr.clone()),
                                    v_term.clone()
                                )))
                        );
                        let witness = Expr::app(
                            *comp.witness.clone(),
                            subst_interval_in_term(v_term, Var::new(0), IntervalDnf::Zero)
                        );
                        let target_subst_i0 = subst_interval_in_expr(target_subst.expr.clone(),
                            Var::new(1), IntervalDnf::Zero);
                        Ok(Expr::comp(target_subst, system, Term::new(witness, target_subst_i0)))
                    } else {
                        Err(EvalErr::ApplicableDidNotReduce)
                    }
                }
                _ => {
                    // TODO: Should we allow irreducable applications?
                    // Currently we should never be able to get into a state where we are trying to reduce something like
                    // [f : A -> B, x : A]  |-  f x : B
                    // without a value bound to f in the context
                    Err(EvalErr::ApplicableDidNotReduce)
                }
            }
        },
        Expr::PathApp(path_app) => {
            let path: &Term = &path_app.func;
            let interval = normalise_interval(ctx, &path_app.argument);


            let normalised_path = normalise_term(ctx, &path)?;

            // If we have a PathBind expression, we can directly apply the application
            if let Expr::PathBind(path_bind) = &normalised_path.expr {
                let ctx = ctx.define_interval_var(
                    &"".to_owned(),
                    interval
                );
                return normalise_expr(&ctx, &path_bind.body.expr)
            }

            // Else if the interval value is 0 or 1, just take an endpoint from the path type
            if let Expr::Path(path) = &normalised_path.type_expr {
                if interval == IntervalDnf::One {
                    return Ok(path.end.expr.clone())
                } else if interval == IntervalDnf::Zero {
                    return Ok(path.start.expr.clone())
                }
            }

            // Else just normalise the path bind
            Ok(Expr::path_app(normalised_path, interval.clone()))
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
        Expr::Comp(comp) => normalise_comp(ctx, comp),
        Expr::Fill(kan_fill) => normalise_kan_fill(ctx, kan_fill.clone()),
    }
}

// TODO: Tidy this up a bit
fn normalise_path_comp(ctx: &EvalCtx, path_expr: &Path, face_system: &FaceSystem, witness: &Term) -> EvalResult<Expr> {
    let path_i1 = subst_interval_in_expr(Expr::Path(path_expr.clone()), Var::new(1), IntervalDnf::One);
    let space_i1 = subst_interval_in_expr(path_expr.space.expr.clone(), Var::new(1), IntervalDnf::One);
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
    let expr = Expr::path_bind(
        Term::new(
            Expr::comp(
                increment_debruijn_index_in_term(1, *path_expr.space.clone()),
                system,
                Term::new(
                    Expr::path_app(witness.clone(), j_int),
                    space_i1
                )
            ),
            path_i1
        )
    );
    Ok(normalise_expr(ctx, &expr)?)
}

fn normalise_comp(ctx: &EvalCtx, comp: &Composition) -> EvalResult<Expr> {
    let bound_ctx = &ctx.bind_interval_var("");
    let space = normalise_term(bound_ctx, &comp.space)?;
    let system = normalise_system(bound_ctx, &comp.face_system)?;
    let witness = normalise_term(ctx, &comp.witness)?;

    if let Expr::System(system) = system {
        if let Expr::Path(path) = &space.expr {
            normalise_path_comp(ctx, path, &comp.face_system, &comp.witness)
        } else {
            Ok(Expr::comp(space, system, witness))
        }
    } else {
        normalise_expr(&ctx.define_interval_var("", IntervalDnf::One), &system)
    }
}

fn subst_degeneracy(expr: Expr, var: Var) -> Expr {
    let var = var.increment();
    let fresh_var = Var::new(0);

    let degeneracy = IntervalDnf::meet(
        IntervalDnf::single(var, IsNegated::NotNegated),
        IntervalDnf::single(fresh_var, IsNegated::NotNegated),
    );

    let expr = increment_debruijn_index(1, expr);
    subst_interval_in_expr(expr, var, degeneracy)
}

fn subst_degeneracy_into_system(face_system: FaceSystem, witness: Expr, var: Var) -> EvalResult<FaceSystem> {
    match subst_degeneracy(Expr::System(face_system), var) {
        Expr::System(mut face_system) => {
            let new_face = Face::with_sides(&[(var, ZeroOne::Zero)]);
            face_system.faces.push((new_face, witness));
            Ok(face_system)
        },
        _ => panic!()
    }
}

fn normalise_kan_fill(ctx: &EvalCtx, kan_fill: KanFill) -> EvalResult<Expr> {
    //let degeneracy_ctx = ctx.define_interval_var(&var, degeneracy);
    //let degeneracy_ctx = ctx.define_interval_var(kan_fill.var, interval: IntervalDnf)
    let var = kan_fill.var;
    let witness = *kan_fill.witness;
    let face_system = subst_degeneracy_into_system(kan_fill.face_system, witness.expr.clone(), var)?;
    let space = Term::new_type(subst_degeneracy(kan_fill.space.expr, var));
    let expr = Expr::comp(space, face_system, witness);
    normalise_expr(ctx, &expr)
}

fn normalise_system(ctx: &EvalCtx, system: &FaceSystem) -> EvalResult<Expr> {
    for (face, expr) in &system.faces {
        if faces_congruent(ctx, face, &Face::Top) {
            return Ok(expr.clone())
        }
    }

    let faces: Vec<(Face, Expr)> = system.faces
        .iter()
        // TODO: avoid unwrapping here?
        .map(|(face, expr)| (face.clone(), normalise_expr(ctx, expr).unwrap()))
        .collect();

    Ok(Expr::system(faces))
}
