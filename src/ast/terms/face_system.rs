use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::ast::traits::*;
use crate::ast::{Expr, Face};
use crate::context::Context;
use crate::interval::IntervalDnf;
use crate::face::faces_congruent;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FaceSystem {
    pub faces: Vec<(Face, Expr)>
}

impl FaceSystem {
    pub fn new<T>(faces: T) -> Self
        where T: Into<Vec<(Face, Expr)>>
    {
        FaceSystem {
            faces: faces.into()
        }
    }

    pub fn from_iter<T>(face_iter: T) -> Self
        where T: IntoIterator<Item = (Face, Expr)>
    {
        FaceSystem {
            faces: face_iter.into_iter().collect()
        }
    }
}


impl Evaluate for FaceSystem {
    type Evaluated = Expr;

    fn evaluate(self, ctx: &Context) -> Expr {
        for (face, expr) in &self.faces {
            if faces_congruent(ctx, face, &Face::Top) {
                return expr.clone()
            }
        }

        Expr::system(self.faces)
    }
}

impl Normalise for FaceSystem {
    fn normalise(self, ctx: &Context) -> Expr {
        for (face, expr) in &self.faces {
            if faces_congruent(ctx, face, &Face::Top) {
                return expr.clone()
            }
        }

        let faces: Vec<(Face, Expr)> = self.faces
            .into_iter()
            .map(|(face, expr)| (face.clone(), expr.normalise(ctx)))
            .collect();

        Expr::system(faces)
    }
}

impl Substitute for FaceSystem {
    type ExprSubst = Self;

    fn substitute_expr(self, expr: Expr, var: usize) -> Self {
        FaceSystem::from_iter(
            self.faces.into_iter().map(|(face, expr_)|
                (face, expr_.substitute_expr(expr.clone(), var+1))
            )
        )
    }

    type IntervalSubst = Self;

    fn substitute_interval(self, expr: IntervalDnf, var: usize) -> Self {
        FaceSystem::from_iter(
            self.faces.into_iter().map(|(face, expr_)|
                (
                    face.substitute_interval(expr.clone(), var),
                    expr_.substitute_interval(expr.clone().increment_indices(), var+1)
                )
            )
        )
    }
}

impl DeBruijnIndexed for FaceSystem {
    fn increment_indices_from_by(self, start: usize, amount: usize) -> Self {
        FaceSystem::from_iter(
            self.faces.into_iter().map(|(face, expr)|
                (
                    face.increment_indices_from_by(start, amount),
                    expr.increment_indices_from_by(start+1, amount)
                )
            )
        )
    }
}

impl Display for FaceSystem {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "[")?;
        for (face, expr) in &self.faces {
            write!(f, " {} -> {},", face, expr)?;
        }
        write!(f, " ]")
    }
}
