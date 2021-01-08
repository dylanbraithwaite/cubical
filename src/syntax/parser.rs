use crate::util::types::ZeroOne;
use crate::syntax::expr::{Expr, Variable, Face};
use crate::syntax::token::Token;

use std::iter::Peekable;

pub fn parse_toks<'a>(src: &mut dyn Iterator<Item = Token<'a>>) -> ParseResult<Expr> {
    Parser::from_tok_iter(src)
        .parse_expr()
}

struct Parser<'a, T>
where
    T: Iterator<Item = Token<'a>>,
{
    tokens: Peekable<T>,
}

// TODO: This description of the grammar needs updated; the unit tests are
// more prescriptive of the grammar now

// forall and lambda bindings are right associative, but applications are left
// associative to allow for currying.

// The following functions implement a recursive descent parser for (approximately) this grammar
// Expr ::= Lambda TypedVar => Expr
// Expr ::= Pi TypedVar -> Expr
// Expr ::= Path ExprFactor ExprFactor ExprFactor
// Expr ::= PathBind VarName Expr
// Expr ::= ExprFactor+
// ExprFactor ::= Unit
// ExprFactor ::= Zero
// ExprFactor ::= One
// ExprFactor ::= ()
// ExprFactor ::= ( Expr )
// ExprFactor ::= VarName

// TypedVar ::= VarName : Expr


#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        actual: String,
        current_expr: &'static str,
    },
    EndOfStream,
}

fn handle_parse_err(next_tok: Option<Token>, expected: String, current_expr: &'static str) -> ParseError {
    match next_tok {
        Some(tok) => ParseError::UnexpectedToken {
            expected,
            actual: format!("{:?}", tok),
            current_expr,
        },
        None => ParseError::EndOfStream,
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

impl<'a, T> Parser<'a, T>
where
    T: Iterator<Item = Token<'a>>,
{
    fn from_tok_iter(iter: T) -> Parser<'a, T> {
        Parser {
            tokens: iter.peekable(),
        }
    }

    // UTILS

    fn next_tok(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }

    fn next_tok_is(&mut self, tok: Token) -> bool {
        self.next_tok() == Some(tok)
    }

    fn expect_tok(&mut self, tok: Token, current_expr: &'static str) -> ParseResult<()> {
        if self.next_tok() == Some(tok) {
            let _ = self.tokens.next();
            Ok(())
        } else {
            let expected = format!("{:?}", tok);
            let err = handle_parse_err(self.next_tok(), expected, current_expr);
            Err(err)
        }
    }

    fn consume_if_next_tok(&mut self, tok: Token) -> bool {
        if self.next_tok_is(tok) {
            let _ = self.tokens.next();
            true
        } else {
            false
        }
    }

    // Expr ::= Lambda TypedVar => Expr
    // Expr ::= Pi TypedVar -> Expr
    // Expr ::= Path ExprFactor ExprFactor ExprFactor
    // Expr ::= PathBind VarName Expr
    // Expr ::= ExprFactor+
    fn parse_expr(&mut self) -> ParseResult<Expr> {
        if self.next_tok_is(Token::Pi) {
            self.parse_expr_pi()
        } else if self.next_tok_is(Token::Lambda) {
            self.parse_expr_lambda()
        } else if self.next_tok_is(Token::Path) {
            self.parse_expr_path_type()
        } else if self.next_tok_is(Token::PathBind) {
            self.parse_expr_path_binder()
        } else if self.next_tok_is(Token::Comp) {
            self.parse_expr_comp()
        } else if self.next_tok_is(Token::Meet) {
            self.parse_expr_meet()
        } else if self.next_tok_is(Token::Join) {
            self.parse_expr_join()
        } else if self.next_tok_is(Token::Inv) {
            self.parse_expr_inv()
        } else {
            self.parse_expr_applications()
        }
    }

    // Expr ::= Lambda TypedVar => Expr
    fn parse_expr_lambda(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::Lambda, "lambda")?;
        let (var, var_type) = self.parse_typed_var_def()?;
        self.expect_tok(Token::ThickArrow, "lambda")?;
        let expr = self.parse_expr()?;
        Ok(Expr::Lambda((var, var_type.boxed(), expr.boxed())))
    }

    // Expr ::= Pi TypedVar -> Expr
    fn parse_expr_pi(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::Pi, "pi")?;
        let (var, var_type) = self.parse_typed_var_def()?;
        self.expect_tok(Token::ThinArrow, "pi")?;
        let expr = self.parse_expr()?;
        Ok(Expr::Pi((var, var_type.boxed(), expr.boxed())))
    }

    // Expr ::= Path ExprFactor ExprFactor ExprFactor
    fn parse_expr_path_type(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::Path, "path_type")?;
        let type_expr = self.parse_expr_factor()?.boxed();
        let src_expr = self.parse_expr_factor()?.boxed();
        let dst_expr = self.parse_expr_factor()?.boxed();
        Ok(Expr::Path(type_expr, src_expr, dst_expr))
    }

    // Expr ::= PathBind VarName => Expr
    fn parse_expr_path_binder(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::PathBind, "path_binder")?;
        let var_name = self.parse_var_name("path_binder")?;
        self.expect_tok(Token::ThickArrow, "path_binder")?;
        let body = self.parse_expr()?.boxed();
        Ok(Expr::PathBind(var_name, body))
    }

    fn parse_expr_comp(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::Comp, "comp")?;
        let var_name = self.parse_var_name("comp")?;
        let type_expr = self.parse_expr_factor()?.boxed();
        let faces = self.parse_face_system_inner()?;
        let witness_expr = self.parse_expr_factor()?.boxed();
        Ok(Expr::Comp(var_name, type_expr, faces, witness_expr))
    }

    fn parse_expr_meet(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::Meet, "meet")?;
        let i1 = self.parse_expr_factor()?.boxed();
        let i2 = self.parse_expr_factor()?.boxed();
        Ok(Expr::IntervalMeet(i1, i2))
    }

    fn parse_expr_join(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::Join, "join")?;
        let i1 = self.parse_expr_factor()?.boxed();
        let i2 = self.parse_expr_factor()?.boxed();
        Ok(Expr::IntervalJoin(i1, i2))
    }

    fn parse_expr_inv(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::Inv, "inv")?;
        let i = self.parse_expr_factor()?.boxed();
        Ok(Expr::IntervalInvolution(i))
    }

    // ExprFactor ::= One
    fn parse_interval_one(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::One, "1")?;
        Ok(Expr::IntervalOne)
    }

    // ExprFactor ::= Zero
    fn parse_interval_zero(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::Zero, "0")?;
        Ok(Expr::IntervalZero)
    }

    // ExprFactor ::= Unit
    fn parse_unit_type(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::UnitType, "unit_type")?;
        Ok(Expr::UnitType)
    }

    // ExprFactor ::= ()
    fn parse_unit_val(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::UnitVal, "unit_val")?;
        Ok(Expr::UnitVal)
    }

    fn parse_expr_face_system(&mut self) -> ParseResult<Expr> {
        let vec = self.parse_face_system_inner()?;
        Ok(Expr::System(vec))
    }

    fn parse_face_system_inner(&mut self) -> ParseResult<Vec<(Face, Expr)>> {
        self.expect_tok(Token::OpenBracket, "face_system")?;
        let mut vec = Vec::new();
        while !self.next_tok_is(Token::CloseBracket) {
            let face = self.parse_face()?;
            self.expect_tok(Token::ThinArrow, "face_system")?;
            let expr = self.parse_expr()?;
            vec.push((face, expr));
            self.consume_if_next_tok(Token::Comma);
        }
        self.expect_tok(Token::CloseBracket, "face_system")?;

        Ok(vec)
    }

    fn parse_face(&mut self) -> ParseResult<Face> {
        self.expect_tok(Token::OpenParen, "face")?;
        let mut vec = Vec::new();
        while !self.next_tok_is(Token::CloseParen) {
            let var = self.parse_var_name("face")?;
            self.expect_tok(Token::Equals, "face")?;
            let zero_or_one = self.parse_zero_or_one("face")?;
            vec.push((var, zero_or_one));
            self.consume_if_next_tok(Token::Comma);
        }

        self.expect_tok(Token::CloseParen, "face")?;
        Ok(vec)
    }

    fn parse_zero_or_one(&mut self, current_expr: &'static str) -> ParseResult<ZeroOne> {
        if self.consume_if_next_tok(Token::Zero) {
            Ok(ZeroOne::Zero)
        } else {
            self.expect_tok(Token::One, current_expr)?;
            Ok(ZeroOne::One)
        }
    }

    // Expr ::= ExprFactor+
    fn parse_expr_applications(&mut self) -> ParseResult<Expr> {
        // recursive descent parsers want to make right-associative derivations.
        // Here we rotate the generated AST to force left-associativity for applications
        // ie we need (f a b) to be parsed as ((f a) b), so currying works correctly

        let mut ret = self.parse_expr_factor()?;

        // TODO: Either this should be tok_is_start_of_expr_factor or
        // we should call parse_expr() here
        while tok_is_start_of_expr(self.next_tok()) {
            let expr = self.parse_expr_factor()?;
            ret = Expr::App(ret.boxed(), expr.boxed());
        }

        Ok(ret)
    }

    // ExprFactor ::= Unit
    // ExprFactor ::= Zero
    // ExprFactor ::= One
    // ExprFactor ::= ()
    // ExprFactor ::= ( Expr )
    // ExprFactor ::= VarName
    fn parse_expr_factor(&mut self) -> ParseResult<Expr> {
        if self.next_tok_is(Token::OpenParen) {
            self.parse_parenthesized_expr()
        } else if let Some(Token::VariableName(_)) = self.next_tok() {
            Ok(Expr::Var(self.parse_var_name("expr_factor")?))
        } else if self.next_tok_is(Token::One) {
            self.parse_interval_one()
        } else if self.next_tok_is(Token::Zero) {
            self.parse_interval_zero()
        } else if self.next_tok_is(Token::UnitType) {
            self.parse_unit_type()
        } else if self.next_tok_is(Token::UnitVal) {
            self.parse_unit_val()
        } else if self.next_tok_is(Token::OpenBracket) {
            self.parse_expr_face_system()
        } else {
            Err(handle_parse_err(self.next_tok(), "None".to_owned(), "expr_factor"))
        }
    }

    // ExprFactor ::= ( Expr )
    fn parse_parenthesized_expr(&mut self) -> ParseResult<Expr> {
        self.expect_tok(Token::OpenParen, "parenthesized_expr")?;
        let ret = self.parse_expr()?;
        self.expect_tok(Token::CloseParen, "parenthesized_expr")?;
        Ok(ret)
    }

    // TypedVar ::= (VarName : Expr)
    fn parse_typed_var_def(&mut self) -> ParseResult<(Variable, Expr)> {
        self.expect_tok(Token::OpenParen, "typed_var_def")?;
        let var_name = self.parse_var_name("typed_var_def")?;
        self.expect_tok(Token::Colon, "typed_var_def")?;
        let var_type = self.parse_expr()?;
        self.expect_tok(Token::CloseParen, "typed_var_def")?;
        Ok((var_name, var_type))
    }

    fn parse_var_name(&mut self, current_expr: &'static str) -> ParseResult<Variable> {
        if let Some(Token::VariableName(name)) = self.tokens.next() {
            Ok(name.to_owned())
        } else {
            Err(handle_parse_err(self.next_tok(), "Variable".to_owned(), current_expr))
        }
    }
}

fn tok_is_start_of_expr(tok: Option<Token>) -> bool {
    tok == Some(Token::UnitType)
        || tok == Some(Token::Lambda)
        || tok == Some(Token::Pi)
        || tok == Some(Token::OpenParen)
        || tok == Some(Token::UnitVal)
        || tok == Some(Token::UnitType)
        || tok == Some(Token::One)
        || tok == Some(Token::Zero)
        || matches!(tok, Some(Token::VariableName(_)))
}