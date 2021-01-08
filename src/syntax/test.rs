use crate::util::types::ZeroOne;
use crate::syntax::expr::Expr;
use crate::syntax::parse_from_source;

#[test]
fn test_parse_lambda() {
    // Expr ::= Lambda TypedVar => Expr
    let src = "Lambda (foo : Unit) => foo";
    let expr = parse_from_source(src).unwrap();

    let expected = Expr::Lambda((
        "foo".to_owned(),
        Box::new(Expr::UnitType),
        Box::new(Expr::Var("foo".to_owned())),
    ));

    assert_eq!(expr, expected);
}

#[test]
fn test_parse_pi() {
    // Expr ::= Pi TypedVar -> Expr
    let src = "Pi (foo : Unit) -> Unit";
    let expr = parse_from_source(src).unwrap();

    let expected = Expr::Pi((
        "foo".to_owned(),
        Box::new(Expr::UnitType),
        Box::new(Expr::UnitType),
    ));

    assert_eq!(expr, expected);
}

#[test]
fn test_parse_path() {
    // Expr ::= Path ExprFactor ExprFactor ExprFactor
    let src = "Path Unit () ()";
    let expr = parse_from_source(src).unwrap();

    let expected = Expr::Path(
        Box::new(Expr::UnitType),
        Box::new(Expr::UnitVal),
        Box::new(Expr::UnitVal),
    );

    assert_eq!(expr, expected);
}

#[test]
fn test_parse_path_bind() {
    // Expr ::= Path ExprFactor ExprFactor ExprFactor
    let src = "PathBind foo => ()";
    let expr = parse_from_source(src).unwrap();

    let expected = Expr::PathBind(
        "foo".to_owned(),
        Box::new(Expr::UnitVal),
    );

    assert_eq!(expr, expected);
}

#[test]
fn test_parse_interval_formula() {
    let src = "Meet (Inv (Join 0 1)) 1";
    let expr = parse_from_source(src).unwrap();

    let expected = Expr::IntervalMeet(
        Box::new(Expr::IntervalInvolution(
            Box::new(Expr::IntervalJoin(
                Box::new(Expr::IntervalZero),
                Box::new(Expr::IntervalOne),
            )),
        )),
        Box::new(Expr::IntervalOne),
    );

    assert_eq!(expr, expected);
}

#[test]
fn test_parse_system() {
    let src = "[ (foo = 0, bar = 1) -> Lambda (x: Unit) => x, (bar = 0) -> Lambda (x :Unit) => ()]";
    let expr = parse_from_source(src).unwrap();

    let expected = Expr::System( vec![
        (
            vec![
                ("foo".to_owned(), ZeroOne::Zero),
                ("bar".to_owned(), ZeroOne::One),
            ],
            Expr::Lambda((
                "x".to_owned(),
                Box::new(Expr::UnitType),
                Box::new(Expr::Var("x".to_owned())),
            ))
        ),
        (
            vec![
                ("bar".to_owned(), ZeroOne::Zero),
            ],
            Expr::Lambda((
                "x".to_owned(),
                Box::new(Expr::UnitType),
                Box::new(Expr::UnitVal),
            ))
        ),
    ]);

    assert_eq!(expr, expected);
}

#[test]
fn test_parse_comp() {
    let src = "Comp foo (Pi (x: Unit) -> Unit) [(foo = 0) -> Lambda (x: Unit) => x, (foo = 1) -> Lambda (x: Unit) => ()] bar";
    let expr = parse_from_source(src).unwrap();

    let expected = Expr::Comp(
        "foo".to_owned(),
        Box::new(Expr::Pi((
            "x".to_owned(),
            Box::new(Expr::UnitType),
            Box::new(Expr::UnitType),
        ))),
        vec! [
            (
                vec![("foo".to_owned(), ZeroOne::Zero)],
                Expr::Lambda((
                    "x".to_owned(),
                    Box::new(Expr::UnitType),
                    Box::new(Expr::Var("x".to_owned())),
                ))
            ),
            (
                vec![("foo".to_owned(), ZeroOne::One)],
                Expr::Lambda((
                    "x".to_owned(),
                    Box::new(Expr::UnitType),
                    Box::new(Expr::UnitVal),
                ))
            ),
        ],
        Box::new(Expr::Var("bar".to_owned()))
    );

    assert_eq!(expr, expected);
}

#[test]
fn test_lambda_right_assoc() {
    let src = "Lambda (foo : Unit) => () ()";
    let expr = parse_from_source(src).unwrap();

    let expected = Expr::Lambda((
        "foo".to_owned(),
        Box::new(Expr::UnitType),
        Box::new(Expr::App(
            Box::new(Expr::UnitVal),
            Box::new(Expr::UnitVal),
        ))
    ));

    assert_eq!(expr, expected);
}

#[test]
fn test_app_left_assoc() {
    let src = "()()()";
    let expr = parse_from_source(src).unwrap();

    let expected = Expr::App(
        Box::new(Expr::App(
            Box::new(Expr::UnitVal),
            Box::new(Expr::UnitVal),
        )),
        Box::new(Expr::UnitVal),
    );

    assert_eq!(expr, expected);
}

#[test]
fn test_parenthesized_app() {
    let src = "()(()())";
    let expr = parse_from_source(src).unwrap();

    let expected = Expr::App(
        Box::new(Expr::UnitVal),
        Box::new(Expr::App(
            Box::new(Expr::UnitVal),
            Box::new(Expr::UnitVal),
        )),
    );

    assert_eq!(expr, expected);
}