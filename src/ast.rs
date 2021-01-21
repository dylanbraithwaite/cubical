use std::fmt::{Display, Formatter, Error as FmtErr};

use crate::interval::IntervalDnf;
use crate::face::Face;

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

    pub struct Sigma {
        pub left_type: Box<Term>,
        pub right_type: Box<Term>
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

    pub struct Pair {
        pub left: Box<Term>,
        pub right: Box<Term>,
    }

    pub struct LeftProj {
        pub pair: Box<Term>,
    }

    pub struct RightProj {
        pub pair: Box<Term>,
    }

    pub struct App {
        pub func: Box<Term>,
        pub argument: Box<Term>,
    }

    pub struct PathApp {
        pub func: Box<Term>,
        pub argument: Box<IntervalDnf>,
    }

    pub struct Composition {
        pub face_system: FaceSystem,
        pub space: Box<Term>,
        /// The witness at i=0 to the partial terms in the face system being extensible.
        pub witness: Box<Term>,
    }

    /// Composition binds an interval variable, 'removing' it from the context, and
    /// giving a term inhabiting the type at one endpoint of that direction ie: A[i/1].
    /// Kan filling on the other hand, leaves the variable free in the context, giving
    /// us a term inhabiting the interior of the cube, rather than just an endpoint
    pub struct KanFill {
        pub var: Var,

        pub face_system: FaceSystem,
        pub space: Box<Term>,
        pub witness: Box<Term>,
    }

    pub struct ContrElim {
        /// A proof of contractability is a term of type
        /// `Sigma (x: A) (Pi (y: A) -> Path A x y)`
        pub proof: Box<Term>,

        pub face_system: FaceSystem,
    }

    pub struct Pres {
        pub function: Box<Term>,
        pub face_system: FaceSystem,
        pub witness: Box<Term>,
    }

    pub enum Expr {
        Var(Var),
        Type,
        System(FaceSystem),

        // Types
        Pi(Pi),
        Path(Path),
        Sigma(Sigma),
        UnitType,

        // Exprs
        PathBind(PathBind),
        Lambda(Lambda),
        App(App),
        PathApp(PathApp),
        Pair(Pair),
        LeftProj(LeftProj),
        RightProj(RightProj),
        Comp(Composition),
        Fill(KanFill),
        Contr(ContrElim),
        Pres(Pres),
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

    pub fn comp(space: Term, face_system: FaceSystem, witness: Term) -> Expr {
        Expr::Comp(Composition::new(space, face_system, witness))
    }

    pub fn fill(var: Var, space: Term, face_system: FaceSystem, witness: Term) -> Expr {
        Expr::Fill(KanFill::new(var, space, face_system, witness))
    }

    // transp i A a = comp i A [] a
    pub fn transp(space: Term, witness: Term) -> Expr {
        let face_system = FaceSystem::new([]);
        Expr::Comp(Composition::new(space, face_system, witness))
    }

    /// Constructs a term
    /// `isContr A = Sigma (x: A) (Pi (y: A) -> Path A x y)`
    /// representing the contractibility of a space.
    pub fn contr_prop(space: Term) -> Expr {
        Expr::sigma (
            space.clone(),
            Term::new_type(Expr::pi (
                space.clone(),
                Term::new_type(Expr::path (
                    space.clone(),
                    Term::new(Expr::var(1), space.expr.clone()),
                    Term::new(Expr::var(0), space.expr)
                ))
            ))
        )
    }

    pub fn contr_elim(proof: Term, system: FaceSystem) -> Expr {
        Expr::Contr(ContrElim::new(proof, system))
    }

    pub fn sigma(left_type: Term, right_type: Term) -> Expr {
        Expr::Sigma(Sigma::new(left_type, right_type))
    }

    pub fn pair(left: Term, right: Term) -> Expr {
        Expr::Pair(Pair::new(left, right))
    }

    pub fn left_proj(pair: Term) -> Expr {
        Expr::LeftProj(LeftProj::new(pair))
    }

    pub fn right_proj(pair: Term) -> Expr {
        Expr::RightProj(RightProj::new(pair))
    }

    pub fn pres(function: Term, face_system: FaceSystem, witness: Term) -> Expr {
        Expr::Pres(Pres::new(function, face_system, witness))
    }
}

impl Var {
    pub fn new(index: usize) -> Self {
        Var {
            debruijn_index: index
        }
    }

    pub fn increment(&self) -> Self {
        self.increment_by(1)
    }

    pub fn increment_by(&self, num: usize) -> Self {
        Var {
            debruijn_index: self.debruijn_index + num
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

impl Pres {
    pub fn new(function: Term, face_system: FaceSystem, witness: Term) -> Self {
        Pres {
            function: Box::new(function),
            face_system,
            witness: Box::new(witness),
        }
    }
}

impl ContrElim {
    pub fn new(proof: Term, face_system: FaceSystem) -> Self {
        ContrElim {
            proof: Box::new(proof),
            face_system,
        }
    }
}

impl KanFill {
    pub fn new(var: Var, space: Term, face_system: FaceSystem, witness: Term) -> Self {
        KanFill {
            var,
            space: Box::new(space),
            face_system,
            witness: Box::new(witness),
        }
    }
}
impl Composition {

    pub fn new(space: Term, face_system: FaceSystem, witness: Term) -> Self {
        Composition {
            space: Box::new(space),
            face_system: face_system,
            witness: Box::new(witness),
        }
    }
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

impl Pi {
    pub fn new(source: Term, target: Term) -> Self {
        Pi {
            source: Box::new(source),
            target: Box::new(target),
        }
    }
}

impl Sigma {
    pub fn new(left_type: Term, right_type: Term) -> Self {
        Sigma {
            left_type: Box::new(left_type),
            right_type: Box::new(right_type),
        }
    }
}

impl Pair {
    pub fn new(left: Term, right: Term) -> Self {
        Pair {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

impl LeftProj {
    pub fn new(pair: Term) -> Self {
        LeftProj {
            pair: Box::new(pair),
        }
    }
}

impl RightProj {
    pub fn new(pair: Term) -> Self {
        RightProj {
            pair: Box::new(pair),
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
            Expr::Sigma(sigma) => write!(f, "{}", sigma),
            Expr::Lambda(lambda) => write!(f, "{}", lambda),
            Expr::Path(path) => write!(f, "{}", path),
            Expr::PathBind(path_bind) => write!(f, "{}", path_bind),
            Expr::App(app) => write!(f, "{}", app),
            Expr::Pair(pair) => write!(f, "{}", pair),
            Expr::LeftProj(left_proj) => write!(f, "{}", left_proj),
            Expr::RightProj(right_proj) => write!(f, "{}", right_proj),
            Expr::PathApp(app) => write!(f, "{}", app),
            Expr::System(system) => write!(f, "{}", system),
            Expr::Comp(comp) => write!(f, "{}", comp),
            Expr::Fill(kan_fill) => write!(f, "{}", kan_fill),
            Expr::Contr(contr_elim) => write!(f, "{}", contr_elim),
            Expr::Pres(pres) => write!(f, "{}", pres),
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

impl Display for Sigma {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "Sigma (_: {}) {}", self.left_type, self.right_type)
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "({}, {})", self.left, self.right)
    }
}

impl Display for LeftProj {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "ProjL {}", self.pair)
    }
}

impl Display for RightProj {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "ProjL {}", self.pair)
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

impl Display for Composition {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "Comp _ ({}) {} ({})", self.space, self.face_system, self.witness)
    }
}

impl Display for KanFill {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "Fill {} {} {} {}", self.var, self.space, self.face_system, self.witness)
    }
}

impl Display for ContrElim {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "Contr {} {}", self.proof, self.face_system)
    }
}

impl Display for Pres {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "Pres {} {} {}", self.function, self.face_system, self.witness)
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
            IntervalDnf::Conjunction(term) => write!(f, "{:#?}", term)
        }
    }
}
