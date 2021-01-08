use cons_list::ConsList;

use crate::syntax::expr::Variable;
use crate::interval::IntervalDnf;
use crate::ast::{Expr, Term, Var};
use crate::face::Face;
use crate::var_target::VarTarget;
use crate::debruijn::*;

#[derive(Clone, Debug)]
enum CtxEntry {
    Var(Variable, VarTarget),

    /// Restricts the context with a face formula. Face formulae induce congruences
    /// on the interval lattice. There is then an interval congruence associated
    /// with each context. This is the meet of the congruences associated with each
    /// of the face formulae in the context.
    Face(Face)
}

#[derive(Clone, Debug)]
pub struct Context(ConsList<CtxEntry>);

pub type DbnIndex = usize;

impl Context {
    pub fn new() -> Self {
        Context(ConsList::new())
    }

    pub fn debruijnify(&self, var: &str) -> Option<(DbnIndex, VarTarget)> {
        let Context(ctx) = self;

        ctx.iter()
            .flat_map(|entry| match entry {
                CtxEntry::Var(v, t) => Some((v, t)),
                CtxEntry::Face(_) => None,
            })
            .enumerate()
            .find(|(_, (u, _))| *u == var)
            .map(|(i, (_, v))| (i, v.clone()))
    }

    pub fn define_term_var(&self, v: &str, term: &Term) -> Context {
        let Context(ctx) = self;
        let key = v.to_owned();
        let val = VarTarget::Term(term.clone());
        let ctx = ctx.append(CtxEntry::Var(key, val));

        Context(ctx)
    }

    pub fn define_interval_var(&self, v: &str, interval: IntervalDnf) -> Context {
        let Context(ctx) = self;
        let key = v.to_owned();
        let val = VarTarget::Interval(interval);
        let ctx = ctx.append(CtxEntry::Var(key, val));

        Context(ctx)
    }

    pub fn bind_term_var(&self, v: &str, ty: &Expr) -> Context {
        let Context(ctx) = self;
        let ty = ty.clone();
        let key = v.to_owned();
        let val = VarTarget::BoundTerm(ty);
        let ctx = ctx.append(CtxEntry::Var(key, val));

        Context(ctx)
    }

    pub fn with_face_restriction(&self, face: &Face) -> Context {
        let Context(ctx) = self;
        let ctx = ctx.append(CtxEntry::Face(face.clone()));

        Context(ctx)
    }

    pub fn bind_interval_var(&self, v: &str) -> Context {
        let Context(ctx) = self;
        let key = v.to_owned();
        let val = VarTarget::BoundInterval;
        let ctx = ctx.append(CtxEntry::Var(key, val));

        Context(ctx)
    }

    pub fn index(&self, index: usize) -> Option<VarTarget> {
        let Context(ctx) = self;
        ctx.iter()
            .filter_map(|entry| match entry {
                CtxEntry::Var(_, VarTarget::Interval(int)) => Some(
                    VarTarget::Interval(increment_debruijn_index_in_interval(index, int.clone()))),
                CtxEntry::Var(_, VarTarget::Term(term)) => Some(
                    VarTarget::Term(increment_debruijn_index_in_term(index, term.clone()))),
                CtxEntry::Var(_, vt) => Some(vt.clone()),
                CtxEntry::Face(_) => None,
            })
            .nth(index)
    }

    pub fn combined_face_formula(&self) -> Face {
        let Context(ctx) = self;
        ctx
            .iter()
            .filter_map(|entry| match entry {
                CtxEntry::Var(_, _) => None,
                CtxEntry::Face(f) => Some(f),
            })
            .cloned()
            .fold(Face::Top, Face::meet)
    }

    pub fn faces_inconsistent(&self) -> bool {
        self.combined_face_formula() == Face::Bottom
    }

    pub fn intervals(&self) -> Vec<(Var, IntervalDnf)> {
        let Context(ctx) = self;
        ctx.iter()
            .enumerate()
            .filter_map(|(i, entry)| match entry {
                CtxEntry::Var(_, VarTarget::Interval(int)) => Some((Var::new(i), int.clone())),
                _ => None,
            })
            .collect()
    }
}

impl std::fmt::Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        for entry in &self.0 {
            match entry {
                CtxEntry::Face(face) => writeln!(f, "{}", face)?,
                CtxEntry::Var(v, vt) => writeln!(f, "{} {}", v, vt)?,
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for VarTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            VarTarget::BoundInterval => write!(f, ": I"),
            VarTarget::BoundTerm(e) => write!(f, ": {}", e),
            VarTarget::Interval(e) => write!(f, "= {} : I", e),
            VarTarget::Term(t) => write!(f, "= {}", t)
        }
    }
}
