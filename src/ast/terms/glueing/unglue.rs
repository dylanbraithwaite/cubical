use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr, FaceSystem};
use crate::context::Context;
use crate::interval::IntervalDnf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Unglue {
    faces: FaceSystem,
    term: Box<Term>,
}

impl Unglue {
    pub fn new(faces: FaceSystem, term: Term) -> Self {
        Unglue {
            faces: faces,
            term: Box::new(term),
        }
    }
}

impl Evaluate for Unglue {
    type Evaluated = Self;

    fn evaluate(self, _: &Context) -> Self {
        unimplemented!()
    }
}

impl Normalise for Unglue {
    fn normalise(self, ctx: &Context) -> Self {
        unimplemented!()
    }
}

impl Substitute for Unglue {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        unimplemented!()
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        unimplemented!()
    }
}

impl DeBruijnIndexed for Unglue  {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        unimplemented!()
    }
}


impl Display for Unglue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "Unglue {} {}", self.faces, self.term)
    }
}
