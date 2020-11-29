// Don't warn about partial-order conditions of the form !(a < b)
// It's clear in this problem domain that formulae are only partially ordered,
// so !(a < b) won't be confused with being equivalent to a >= b
#![allow(clippy::neg_cmp_op_on_partial_ord)]

use std::collections::BTreeSet;

use crate::util::assoc::Assoc;

use crate::ast::Var;
use crate::ast_types;

// Order vars by their de Bruijn indices
// HashMap does not implement Hash, so we have to use BTreeMaps to represent
// the conjunctive terms
impl PartialOrd for Var {
    fn partial_cmp(&self, other: &Var) -> Option<std::cmp::Ordering> {
        PartialOrd::partial_cmp(&self.debruijn_index, &other.debruijn_index)
    }
}

impl Ord for Var {
    fn cmp(&self, other: &Var) -> std::cmp::Ordering {
        Ord::cmp(&self.debruijn_index, &other.debruijn_index)
    }
}

impl PartialOrd for IsNegated {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use IsNegated::*;
        use std::cmp::Ordering;
        match (self, other) {
            (Negated, Negated)    => Some(Ordering::Equal),
            (Negated, NotNegated) => Some(Ordering::Less),
            (NotNegated, Negated) => Some(Ordering::Greater),
            (NotNegated, NotNegated) => Some(Ordering::Equal),
        }
    }
}

ast_types! {
    #[derive(Ord, Hash)]
    pub enum IsNegated {
        Negated,
        NotNegated,
    }

    #[derive(Hash)]
    pub struct Conjunction {
        pub atoms: BTreeSet<(Var, IsNegated)>,
    }

    pub enum IntervalDnf {
        Conjunction(Conjunction),
        //Conjunctions(HashSet<Conjunction>),
        Zero,
        One,
    }

    pub enum Interval {
        Var(Var),
        Involution(Box<Interval>),
        Meet(Box<Interval>, Box<Interval>),
        Join(Box<Interval>, Box<Interval>),
        Zero,
        One,
    }

    pub enum IntervalNnf {
        Var(Var),
        NegatedVar(Var),
        Meet(Box<IntervalNnf>, Box<IntervalNnf>),
        Join(Box<IntervalNnf>, Box<IntervalNnf>),
        Zero,
        One
    }
}

impl PartialOrd for Conjunction {
    fn partial_cmp(&self, other: &Conjunction) -> Option<std::cmp::Ordering> {
        let mut self_subset_other = true;
        let mut other_subset_self = true;

        for (var, val) in &self.atoms {
            if (&other.atoms).assoc(var) != Some(val.clone()) {
                self_subset_other = false;
                break
            }
        }

        for (var, val) in &other.atoms {
            if (&self.atoms).assoc(var) != Some(val.clone()) {
                other_subset_self = false;
                break
            }
        }

        if self_subset_other && other_subset_self {
            Some(std::cmp::Ordering::Equal)
        } else if self_subset_other {
            Some(std::cmp::Ordering::Less)
        } else if other_subset_self {
            Some(std::cmp::Ordering::Greater)
        } else {
            None
        }
    }
}


impl IntervalDnf {
    pub fn new(interval: Interval) -> Self {
        let interval = push_in_negations(&interval);
        IntervalDnf::new_from_nnf(interval)
    }

    pub fn new_from_nnf(interval: IntervalNnf) -> Self {
        match interval {
            IntervalNnf::Var(v) => IntervalDnf::single(v, IsNegated::NotNegated),
            IntervalNnf::NegatedVar(v) => IntervalDnf::single(v, IsNegated::Negated),
            IntervalNnf::Meet(e1, e2) => IntervalDnf::meet(
                IntervalDnf::new_from_nnf(*e1),
                IntervalDnf::new_from_nnf(*e2)
            ),
            IntervalNnf::Join(_, _) => panic!("Interval joins not implemented"),
            /*
            IntervalNnf::Join(e1, e2) => IntervalDnf::join(
                IntervalDnf::new_from_nnf(*e1),
                IntervalDnf::new_from_nnf(*e2)
            ),
            */
            IntervalNnf::Zero => IntervalDnf::Zero,
            IntervalNnf::One => IntervalDnf::One,
        }
    }

    pub fn single(var: Var, negated: IsNegated) -> Self {
        let mut atoms = BTreeSet::new();
        atoms.insert((var, negated));
        let conjunction = Conjunction { atoms };
        /*
        let mut conjunctions = HashSet::new();
        conjunctions.insert(conjunction);
        IntervalDnf::Conjunctions(conjunctions)
        */
        IntervalDnf::Conjunction(conjunction)
    }

    /*
    fn join(int1: Self, int2: Self) -> Self {
        /*  Short circuit the join:
             - 0 is an identity element
             - 1 is an absorptive element */
        let int1_conjs = match &int1 {
            IntervalDnf::Zero => return int2,
            IntervalDnf::One  => return IntervalDnf::One,
            IntervalDnf::Conjunctions(conjunctions) => conjunctions
        };
        let int2_conjs = match &int2 {
            IntervalDnf::Zero => return int1,
            IntervalDnf::One  => return IntervalDnf::One,
            IntervalDnf::Conjunctions(conjunctions) => conjunctions
        };

        /*  If a conjunction (b) is a (strict) subset of (a), then (a) is of the form (a = b ∧ c)
            Then a ∨ b
                = (b ∧ c) ∨ b
                = (b ∧ c) ∨ (b ∧ 1)
                = b ∧ (c ∨ 1)
                = b
            Hence we can drop (a) from the disjunction */
        let non_subsumed_conjs_1: HashSet<Conjunction> = int1_conjs
            .iter()
            .filter(|a| int2_conjs
                .iter()
                .all(|b| !(b < a))
            )
            .cloned()
            .collect();

        // Likewise, with a and b swapped
        let non_subsumed_conjs_2: HashSet<Conjunction> = int2_conjs
            .iter()
            .filter(|b| int1_conjs
                .iter()
                .all(|a| !(a < b))
            )
            .cloned()
            .collect();

        let conjunctions: HashSet<Conjunction> = &non_subsumed_conjs_1 | &non_subsumed_conjs_2;

        IntervalDnf::Conjunctions(conjunctions)
    }
    */

    fn meet(int1: Self, int2: Self) -> Self {
        /*  Short circuit the meet:
             - 0 is an absorptive element
             - 1 is an identity element */
        let int1_conjs = match &int1 {
            IntervalDnf::Zero => return IntervalDnf::Zero,
            IntervalDnf::One  => return int2,
            //IntervalDnf::Conjunctions(conjunctions) => conjunctions
            IntervalDnf::Conjunction(conjunction) => conjunction
        };
        let int2_conjs = match &int2 {
            IntervalDnf::Zero => return IntervalDnf::Zero,
            IntervalDnf::One  => return int1,
            //IntervalDnf::Conjunctions(conjunctions) => conjunctions
            IntervalDnf::Conjunction(conjunction) => conjunction
        };

        /*
        // Distribute the meet inside the joins
        let conjunctions = iproduct!(int1_conjs, int2_conjs)
            .map(|(c1, c2)| Conjunction{ atoms: &c1.atoms | &c2.atoms })
            .collect();
        IntervalDnf::Conjunctions(conjunctions)
        */

        let conj = Conjunction { atoms: &int1_conjs.atoms | &int2_conjs.atoms };
        IntervalDnf::Conjunction(conj)
    }
}

fn negate_expression(expr: &Interval) -> IntervalNnf {
    match expr {
        Interval::Var(v) => IntervalNnf::NegatedVar(*v),
        Interval::Involution(inner) => push_in_negations(inner),
        Interval::Meet(e1, e2) => IntervalNnf::Join(
            Box::new(negate_expression(&e1)),
            Box::new(negate_expression(&e2)),
        ),
        Interval::Join(e1, e2) => IntervalNnf::Meet(
            Box::new(negate_expression(&e1)),
            Box::new(negate_expression(&e2)),
        ),
        Interval::Zero => IntervalNnf::One,
        Interval::One => IntervalNnf::Zero,
    }
}

fn push_in_negations(expr: &Interval) -> IntervalNnf {
    match expr {
        Interval::Var(v) => IntervalNnf::Var(*v),
        Interval::Involution(inner) => negate_expression(inner),
        Interval::Meet(e1, e2) => IntervalNnf::Meet(
            Box::new(push_in_negations(&e1)),
            Box::new(push_in_negations(&e2)),
        ),
        Interval::Join(e1, e2) => IntervalNnf::Join(
            Box::new(push_in_negations(&e1)),
            Box::new(push_in_negations(&e2)),
        ),
        Interval::Zero => IntervalNnf::Zero,
        Interval::One => IntervalNnf::One,
    }
}
