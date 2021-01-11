use std::collections::HashSet;

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

/// For each interval defined in the context, substitute it into the face
fn normalise_face(ctx: &Context, face: &Face) -> Face {
    match face {
        Face::Top => Face::Top,
        Face::Bottom => Face::Bottom,
        Face::Conjunctions { ones, zeroes }=> {
            let mut ones = ones.clone();
            let mut zeroes = zeroes.clone();
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

pub fn faces_congruent(ctx: &Context, lhs: &Face, rhs: &Face) -> bool {
    let face_restriction = ctx.combined_face_formula();

    let lhs = Face::meet(face_restriction.clone(), lhs.clone());
    let lhs = normalise_face(ctx, &lhs);

    let rhs = Face::meet(face_restriction, rhs.clone());
    let rhs = normalise_face(ctx, &rhs);

    lhs == rhs
}

fn filter_vals(sides: &[(Var, ZeroOne)], val: ZeroOne) -> HashSet<Var> {
    sides
        .iter()
        .filter_map(|(var, val_)| if val == *val_ { Some(*var) } else { None })
        .collect()
}
