use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr, FaceSystem};
use crate::context::Context;
use crate::interval::IntervalDnf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Glue {
    faces: FaceSystem,
    term: Box<Term>,
}

impl Glue {
    pub fn new(faces: FaceSystem, term: Term) -> Self {
        Glue {
            faces: faces,
            term: Box::new(term),
        }
    }
}

impl Evaluate for Glue {
    type Evaluated = Self;

    fn evaluate(self, _: &Context) -> Self {
        unimplemented!()
    }
}

impl Normalise for Glue {
    fn normalise(self, ctx: &Context) -> Self {
        unimplemented!()
    }
}

impl Substitute for Glue {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        unimplemented!()
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        unimplemented!()
    }
}

impl DeBruijnIndexed for Glue  {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        unimplemented!()
    }
}


impl Display for Glue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "Glue {} {}", self.faces, self.term)
    }
}
