use crate::interval::IntervalDnf;
use crate::ast::{Term, Expr};
use crate::ast::traits::DeBruijnIndexed;

/// Enumerates the kinds of thing that a variable can refer to
#[derive(Clone, Debug)]
pub enum VarTarget {
    /// Represents a variable, referring to a term for which the value is known,
    /// because it has been substituted during evaluation, or defined in the top
    /// level. The associated value is the term which the variable should refer to.
    Term(Term),

    /// Represents a variable, referring to an interval expression for which the
    /// value is known, because it has been substituted during evaluation, or
    /// defined in the top level. The associated value is the interval expression
    /// which the variable should refer to.
    Interval(IntervalDnf),

    /// Represents a variable, contained in the body of a lambda or pi expression,
    /// which has yet to be substituted for a value. The associated value represents
    /// the type of the variable.
    BoundTerm(Expr),

    /// Represents a variable, contained in the body of a path binding expression,
    /// which has yet to be substituted for a value.
    BoundInterval,
}

impl DeBruijnIndexed for VarTarget {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        match self {
            VarTarget::Term(term) => VarTarget::Term(
                term.increment_indices_from_by(start, amount)),
            VarTarget::Interval(int) => VarTarget::Interval(
                int.increment_indices_from_by(start, amount)),
            VarTarget::BoundTerm(ty) => VarTarget::BoundTerm(
                ty.increment_indices_from_by(start, amount)),
            VarTarget::BoundInterval => VarTarget::BoundInterval,
        }
    }
}
