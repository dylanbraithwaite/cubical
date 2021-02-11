use std::collections::HashSet;

use crate::ast::traits::*;
use crate::ast::Expr;
use crate::ast_types;
use crate::util::types::ZeroOne;
use crate::ast::Var;
use crate::context::Context;
use crate::interval::{IntervalDnf, IsNegated};

ast_types! {
    pub enum Face {
        Top,
        Conjunctions {
            ones: HashSet<Var>,
            zeroes: HashSet<Var>,
        },
        Bottom,
    }
}

impl Face {
    pub fn with_sides(sides: &[(Var, ZeroOne)]) -> Self {
        let ones = filter_vals(&sides, ZeroOne::One);
        let zeroes = filter_vals(&sides, ZeroOne::Zero);
        Face::Conjunctions { ones, zeroes }
    }

    pub fn meet(lhs: Face, rhs: Face) -> Self {
        let (mut ones, mut zeroes) = match lhs {
            Face::Top => return rhs,
            Face::Bottom => return Face::Bottom,
            Face::Conjunctions{ones, zeroes} => (ones, zeroes),
        };

        match rhs {
            Face::Bottom => return Face::Bottom,
            Face::Conjunctions{ones: r_ones, zeroes: r_zeroes} => {
                ones = &ones | &r_ones;
                zeroes = &zeroes | &r_zeroes;
            },
            Face::Top => ()
        };

        Face::conjunctions(ones, zeroes)
    }

    pub fn conjunctions(ones: HashSet<Var>, zeroes: HashSet<Var>) -> Self {
        if ones.is_empty() && zeroes.is_empty() {
            Face::Top
        } else if ones.is_disjoint(&zeroes) {
            Face::Conjunctions { ones, zeroes }
        } else {
            Face::Bottom
        }
    }

    pub fn lookup_var(&self, var: Var) -> Option<ZeroOne> {
        match self {
            Face::Top => None,
            Face::Bottom => Some(ZeroOne::Zero),
            Face::Conjunctions { ones, zeroes } => {
                if ones.contains(&var) {
                    Some(ZeroOne::One)
                } else if zeroes.contains(&var) {
                    Some(ZeroOne::Zero)
                } else {
                    None
                }
            }
        }
    }

}

pub fn expand_interval_into_face(interval: &IntervalDnf, negated: bool) -> Face {
    match &interval {
        IntervalDnf::One => if negated { Face::Bottom } else { Face::Top } ,
        IntervalDnf::Zero => if negated { Face::Top } else { Face::Bottom },
        IntervalDnf::Conjunction(conjs) => {
            let mut ones = HashSet::new();
            let mut zeroes = HashSet::new();

            for (var, is_negated) in &conjs.atoms {
                let is_negated = *is_negated == IsNegated::Negated;
                if is_negated == negated {
                    ones.insert(*var);
                } else {
                    zeroes.insert(*var);
                }
            }

            Face::Conjunctions { ones, zeroes }
        }
    }
}

impl Evaluate for Face {
    type Evaluated = Face;

    fn evaluate(self, ctx: &Context) -> Face {
        match self {
            Face::Top => Face::Top,
            Face::Bottom => Face::Bottom,
            Face::Conjunctions { mut ones, mut zeroes }=> {
                let mut accum = Face::Top;
                for (var, ref interval) in ctx.intervals() {
                    if ones.remove(&var) {
                        accum = Face::meet(accum, expand_interval_into_face(interval, false));
                    }
                    if zeroes.remove(&var) {
                        accum = Face::meet(accum, expand_interval_into_face(interval, true));
                    }
                }
                Face::meet(accum, Face::conjunctions(ones, zeroes))
            }
        }
    }
}

impl Normalise for Face {
    fn normalise(self, ctx: &Context) -> Face {
        self.evaluate(ctx)
    }
}

impl DeBruijnIndexed for Face {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Face {
        match self {
            Face::Top => Face::Top,
            Face::Bottom => Face::Bottom,
            Face::Conjunctions {ones, zeroes} => {
                Face::Conjunctions {
                    ones: ones.iter().map(|v| v.increment_indices_from_by(start, amount)).collect(),
                    zeroes: zeroes.iter().map(|v| v.increment_indices_from_by(start, amount)).collect(),
                }
            }
        }
    }
}

impl Substitute for Face {
    type ExprSubst = ();

    fn substitute_expr(self, _: Expr, _: usize) { }

    type IntervalSubst = Self;

    fn substitute_interval(mut self, expr: IntervalDnf, var: usize) -> Self {
        match &mut self {
            Face::Top => Face::Top,
            Face::Bottom => Face::Bottom,
            Face::Conjunctions { ones, zeroes } => {
                if ones.remove(&Var::new(var)) {
                    Face::meet(self, expand_interval_into_face(&expr, false))
                } else if zeroes.remove(&Var::new(var)) {
                    Face::meet(self, expand_interval_into_face(&expr, true))
                } else {
                    self
                }
            }
        }
    }
}

pub fn faces_congruent(ctx: &Context, lhs: &Face, rhs: &Face) -> bool {
    let face_restriction = ctx.combined_face_formula();

    let lhs = Face::meet(face_restriction.clone(), lhs.clone()).normalise(ctx);
    let rhs = Face::meet(face_restriction, rhs.clone()).normalise(ctx);

    lhs == rhs
}

fn filter_vals(sides: &[(Var, ZeroOne)], val: ZeroOne) -> HashSet<Var> {
    sides
        .iter()
        .filter_map(|(var, val_)| if val == *val_ { Some(*var) } else { None })
        .collect()
}
