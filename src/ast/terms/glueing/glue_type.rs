use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Term, Expr, FaceSystem};
use crate::context::Context;
use crate::interval::IntervalDnf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GlueType {
    faces: FaceSystem,
    space: Box<Term>,
}

impl GlueType {
    pub fn new(faces: FaceSystem, space: Term) -> Self {
        GlueType {
            faces: faces,
            space: Box::new(space),
        }
    }
}

impl Evaluate for GlueType {
    type Evaluated = Self;

    fn evaluate(self, _: &Context) -> Self {
        unimplemented!()
    }
}

impl Normalise for GlueType {
    fn normalise(self, ctx: &Context) -> Self {
        unimplemented!()
    }
}

impl Substitute for GlueType {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        unimplemented!()
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        unimplemented!()
    }
}

impl DeBruijnIndexed for GlueType  {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        unimplemented!()
    }
}


impl Display for GlueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "GlueType {} {}", self.faces, self.space)
    }
}

