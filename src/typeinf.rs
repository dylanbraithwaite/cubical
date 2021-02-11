use crate::unwrap_pattern;
use crate::ast::*;
use crate::ast::traits::*;

pub fn infer_app(app: &App) -> Expr {
    let pi_type = app.func.type_expr.clone().substitute_expr(app.argument.expr.clone(), 0);

    unwrap_pattern!{
        pi_type; Expr::Pi(pi) => pi.target.expr.clone()
    }
}

/*
pub fn infer_left_proj(ctx: &Context, lproj: &LeftProj) -> Expr {
    let pair_type = normalise_expr(ctx, &lproj.pair.type_expr).unwrap();

    unwrap_pattern!{
        pair_type; Expr::Sigma(sigma) => sigma.left_type.expr.clone()
    }
}

pub fn infer_right_proj(ctx: &Context, rproj: &RightProj) -> Expr {
    let pair_type = normalise_expr(
        &ctx.define_term_var("", &rproj.pair.expr),
        &rproj.pair.type_expr
    ).unwrap();

    unwrap_pattern!{
        pair_type; Expr::Sigma(sigma) => sigma.right_type.expr.clone()
    }
}
*/
