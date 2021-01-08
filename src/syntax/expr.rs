use crate::util::types::ZeroOne;

pub type Abstraction = (String, ExprBox, ExprBox);
type ExprBox = Box<Expr>;

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Var(String),
    Pi(Abstraction),
    Lambda(Abstraction),
    App(ExprBox, ExprBox),
    UnitType,
    UnitVal,
    Path(ExprBox, ExprBox, ExprBox),
    PathBind(Variable, ExprBox),
    IntervalZero,
    IntervalOne,
    IntervalInvolution(ExprBox),
    IntervalMeet(ExprBox, ExprBox),
    IntervalJoin(ExprBox, ExprBox),
    System(Vec<(Face, Expr)>),
    Comp(String, ExprBox, Vec<(Face, Expr)>, ExprBox),
}

pub type Variable = String;

// TODO:
// We don't support face joins for now because it really simplifies computing
// congruences on the interval lattice. It looks like the original cubicaltt
// implementation referenced in the paper also skipped this.
// I don't think its a big deal, because systems are basically disjunctions of
// face formulae anyway.
// So it just has the effect of forcing us to specify any formulae in CNF.
pub type Face = Vec<(Variable, ZeroOne)>;

impl Expr {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}
