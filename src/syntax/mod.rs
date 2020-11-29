pub mod token;
pub mod expr;
pub mod lexer;
pub mod parser;

pub use parser::ParseError;

pub fn parse_from_source(source: &str) -> parser::ParseResult<expr::Expr> {
    let mut toks = lexer::lex_src(source);
    parser::parse_toks(&mut toks)
}

#[cfg(test)]
mod test;
