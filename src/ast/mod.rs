use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::interval::IntervalDnf;
use crate::face::Face;

pub mod traits;

mod terms {
    pub mod var;

    pub mod pi;
    pub mod lambda;
    pub mod app;

    pub mod sigma;
    pub mod pair;
    pub mod left_proj;
    pub mod right_proj;

    pub mod path;
    pub mod path_bind;
    pub mod path_app;

    pub mod face_system;
    pub mod composition;

    pub mod contr_elim;
    pub mod equiv_elim;
    pub mod pres_elim;
}

mod term;
mod expr;
mod interval;

pub use expr::Expr;
pub use term::Term;

pub use terms::var::Var;

pub use terms::pi::Pi;
pub use terms::lambda::Lambda;
pub use terms::app::App;

pub use terms::sigma::Sigma;
pub use terms::pair::Pair;
pub use terms::left_proj::LeftProj;
pub use terms::right_proj::RightProj;

pub use terms::path::Path;
pub use terms::path_app::PathApp;
pub use terms::path_bind::PathBind;

pub use terms::face_system::FaceSystem;
pub use terms::composition::Composition;

pub use terms::contr_elim::ContrElim;
pub use terms::equiv_elim::EquivElim;
pub use terms::pres_elim::Pres;

#[macro_export]
macro_rules! unwrap_pattern {
    ($in_expr: expr; $( $pattern: pat )|+ => $out_expr: expr) => {
        match $in_expr {
            $( $pattern )|+ => $out_expr,
            _ => panic!("Failed to unwrap pattern"),
        }
    };
}

#[macro_export]
macro_rules! ast_types {
    () => {};
    ($i: item $($is: item)*) => {
        #[derive(Clone, Debug, PartialEq, Eq)]
        $i

        ast_types! {
            $($is)*
        }
    };
}

impl Display for Face {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        write!(f, "(")?;
        match self {
            Face::Top => write!(f, "1")?,
            Face::Bottom => write!(f, "0")?,
            Face::Conjunctions{ones, zeroes} => {
                for var in ones {
                    write!(f, " {} = 1", var)?;
                }
                for var in zeroes {
                    write!(f, " {} = 0", var)?;
                }
            }
        }
        write!(f, " )")
    }
}

impl Display for IntervalDnf {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtErr> {
        match self {
            IntervalDnf::Zero => write!(f, "0"),
            IntervalDnf::One => write!(f, "1"),
            //IntervalDnf::Conjunctions(terms) => write!(f, "Or({:#?})", terms)
            IntervalDnf::Conjunction(term) => write!(f, "{:#?}", term)
        }
    }
}
