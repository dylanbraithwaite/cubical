use crate::ast::Expr;
use crate::context::Context;
use crate::interval::IntervalDnf;

/// Represents a piece of abstract syntax which can be evaluated such as to
/// leave the top-most node in the syntax tree in a canonical form which can be
/// matched against (ie into a weak head-normal-form).
pub trait Evaluate {
    /// Sometimes (ie when normalising an App) the type of the term must change.
    /// This is the most specific type that can represent the result of normalising
    /// this AST node.
    type Evaluated;

    /// Evaluate the outer layer of an expression. Leaving expressions under lambdas
    /// etc as they are.
    fn evaluate(self, ctx: &Context) -> Self::Evaluated;
}

/// A syntax object which can be evaluated "more fully" leaving a proper normal form
/// which can be used to check for definitional equality.
pub trait Normalise: Evaluate {
    /// Evaluate an expression fully, including under lambdas. Leaving a canonical
    /// normal form.
    fn normalise(self, ctx: &Context) -> Self::Evaluated;
}

/// Syntax nodes which have free variables represented by De Bruijn indices
pub trait DeBruijnIndexed: Sized {
    /// Increment de Bruijn indices in the term which are at least equal to `start`
    /// by `amount`.
    /// This is required for example, when adjusting de Bruijn indices in an
    /// expression where new variables are bound.
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self;

    /// Increment de Bruijn indices in the term by `amount`.
    fn increment_indices_by(self, amount: usize) -> Self {
        self.increment_indices_from_by(0, amount)
    }

    /// Increment de Bruijn indices in the term by one.
    fn increment_indices(self) -> Self {
        self.increment_indices_by(1)
    }
}

// TODO: Should the interface here interact more with types for an expression?
/// A piece of syntax which admits a substitution operation.
pub trait Substitute: DeBruijnIndexed {
    /// The result of substituting an expression for a variable in this term. For
    /// most AST nodes this will be Self.
    type ExprSubst;

    /// Substitute `expr` into any variables in the expression matching the specified
    /// de Bruijn index.
    /// Assumes this will leave the expression well typed.
    fn substitute_expr(self, expr: Expr, var: usize) -> Self::ExprSubst;

    type IntervalSubst;

    /// Substitute `expr` into any interval variables in the expression matching the
    /// specified de Bruijn index.
    /// Assumes this will leave the expression well typed.
    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self::IntervalSubst;
}
