use crate::context::Context;
use crate::elaborate::{ElaborationErr, elaborate_term};
use crate::normalise::{normalise_term, EvalErr};
use crate::syntax;
use crate::toplevel;
use crate::toplevel::command::Command;

#[derive(Debug)]
pub enum ReplErr {
    Parse(syntax::ParseError),
    Elab(ElaborationErr),
    TopLevel(toplevel::TopLevelErr),
    Eval(EvalErr),
}

pub fn execute_command(ctx: &mut Context, source: &str) -> Result<String, ReplErr> {
        use ReplErr::*;

        match toplevel::command::parse_command(source).map_err(TopLevel)? {
            Command::Eval(expr_source) => {
                let expr = syntax::parse_from_source(expr_source).map_err(Parse)?;
                let ast = elaborate_term(ctx, &expr).map_err(Elab)?;
                let normalised = normalise_term(&ctx, &ast).map_err(Eval)?;
                Ok(format!("{}", normalised.expr))
            },
            Command::TypeInfer(expr_source) => {
                let expr = syntax::parse_from_source(expr_source).map_err(Parse)?;
                match elaborate_term(ctx, &expr) {
                    Ok(ast) => Ok(format!("{}", ast.type_expr)),
                    Err(err) => Ok(format!("{:?}", err))
                }
            },
            Command::Define(var, expr_source) => {
                let expr = syntax::parse_from_source(expr_source).map_err(Parse)?;
                let ast = elaborate_term(ctx, &expr).map_err(Elab)?;
                let print_str = format!("{} = {}", var, ast);
                *ctx = ctx.define_term_var(&var, &ast);
                Ok(print_str)
            },
            Command::AssumeType(var) => {
                *ctx = ctx.bind_term_var(&var, &crate::ast::Expr::Type);
                Ok(format!("{} : Type", var))
            },
            Command::PrintCtx => {
                Ok(format!("{}", ctx))
            }
        }

    }