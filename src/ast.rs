use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::interval::IntervalDnf;
use crate::face::Face;

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

ast_types! {
    pub struct Term{
        pub expr: Expr,
        pub type_expr: Expr,
    }

    #[derive(Hash, Copy)]
    pub struct Var {
        pub debruijn_index: usize,
    }

    pub struct FaceSystem {
        pub faces: Vec<(Face, Expr)>
    }

    pub struct Pi {
        pub source: Box<Term>,
        pub target: Box<Term>,
    }

    pub struct Path {
        pub space: Box<Term>,
        pub start: Box<Term>,
        pub end: Box<Term>,
    }

    pub struct PathBind {
        pub body: Box<Term>,
    }

    pub struct Lambda {
        pub source: Box<Term>,
        pub body: Box<Term>,
    }

    pub struct App {
        pub func: Box<Term>,
        pub argument: Box<Term>,
    }

    pub struct PathApp {
        pub func: Box<Term>,
        pub argument: Box<IntervalDnf>,
    }


    pub enum Expr {
        Var(Var),
        Type,
        System(FaceSystem),

        // Types
        Pi(Pi),
        Path(Path),
        UnitType,

        // Exprs
        PathBind(PathBind),
        Lambda(Lambda),
        App(App),
        PathApp(PathApp),
        UnitVal,
    }
}

impl Expr {
    pub fn var(debruijn_index: usize) -> Expr {
        Expr::Var(Var::new(debruijn_index))
    }

    pub fn system<T>(faces: T) -> Expr
        where T: Into<Vec<(Face, Expr)>>
    {
        Expr::System(FaceSystem::new(faces.into()))
    }

    pub fn pi(source: Term, target: Term) -> Expr {
        Expr::Pi(Pi::new(source, target))
    }

    pub fn path(space: Term, start: Term, end: Term) -> Expr {
        Expr::Path(Path::new(space, start, end))
    }

    pub fn path_bind(body: Term) -> Expr {
        Expr::PathBind(PathBind::new(body))
    }

    pub fn lambda(space: Term, body: Term) -> Expr {
        Expr::Lambda(Lambda::new(space, body))
    }

    pub fn app(func: Term, arg: Term) -> Expr {
        Expr::App(App::new(func, arg))
    }

    pub fn path_app(func: Term, arg: IntervalDnf) -> Expr {
        Expr::PathApp(PathApp::new(func, arg))
    }
}

impl Var {
    pub fn new(index: usize) -> Self {
        Var {
            debruijn_index: index
        }
    }

    pub fn increment(&self) -> Self {
        Var {
            debruijn_index: self.debruijn_index + 1
        }
    }
}

impl Term {
    pub fn new(expr: Expr, type_expr: Expr) -> Self {
        Term {
            expr,
            type_expr,
        }
    }

    pub fn new_type(expr: Expr) -> Self {
        Term {
            expr,
            type_expr: Expr::Type,
        }
    }
}

// TODO: A derive macro for these?

impl FaceSystem {
    pub fn new(faces: Vec<(Face, Expr)>) -> Self {
        FaceSystem {
            faces
        }
    }
}

impl Pi {
    pub fn new(source: Term, target: Term) -> Self {
        Pi {
            source: Box::new(source),
            target: Box::new(target),
        }
    }
}

impl Path {
    pub fn new(space: Term, start: Term, end: Term) -> Self {
        Path {
            space: Box::new(space),
            start: Box::new(start),
            end: Box::new(end),
        }
    }
}

impl PathBind {
    pub fn new(body: Term) -> Self {
        PathBind {
            body: Box::new(body),
        }
    }
}

impl Lambda {
    pub fn new(source: Term, body: Term) -> Self {
        Lambda {
            source: Box::new(source),
            body: Box::new(body),
        }
    }
}

impl App {
    pub fn new(func: Term, argument: Term) -> Self {
        App {
            func: Box::new(func),
            argument: Box::new(argument),
        }
    }
}

impl PathApp {
    pub fn new(path: Term, argument: IntervalDnf) -> Self {
        PathApp {
            func: Box::new(path),
            argument: Box::new(argument),
        }
    }
}

type FmtResult = Result<(), FmtErr>;

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{} : {}", self.expr, self.type_expr)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Expr::Var(v) => write!(f, "{}", v),
            Expr::Type => write!(f, "Type"),
            Expr::UnitType => write!(f, "Unit"),
            Expr::UnitVal => write!(f, "()"),
            Expr::Pi(pi) => write!(f, "{}", pi),
            Expr::Lambda(lambda) => write!(f, "{}", lambda),
            Expr::Path(path) => write!(f, "{}", path),
            Expr::PathBind(path_bind) => write!(f, "{}", path_bind),
            Expr::App(app) => write!(f, "{}", app),
            Expr::PathApp(app) => write!(f, "{}", app),
            Expr::System(system) => write!(f, "{}", system),
        }
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "v{}", self.debruijn_index)
    }
}

impl Display for Pi {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "(Pi (_ : {}) -> {})", self.source.expr, self.target)
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "(Lambda (_ : {}) => {})", self.source.expr, self.body.expr)
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "Path {} {} {}", self.space.expr, self.start.expr, self.end.expr)
    }
}

impl Display for PathBind {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "(PathBind _ => {})", self.body.expr)
    }
}

impl Display for App {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "({} {})", self.func.expr, self.argument.expr)
    }
}

impl Display for PathApp {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "({} {:?})", self.func.expr, self.argument)
    }
}

impl Display for FaceSystem {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "[")?;
        for (face, expr) in &self.faces {
            write!(f, " {} -> {},", face, expr)?;
        }
        write!(f, " ]")
    }
}

impl Display for Face {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            IntervalDnf::Zero => write!(f, "0"),
            IntervalDnf::One => write!(f, "1"),
            //IntervalDnf::Conjunctions(terms) => write!(f, "Or({:#?})", terms)
            IntervalDnf::Conjunction(term) => write!(f, "Or({:#?})", term)
        }
    }
}
