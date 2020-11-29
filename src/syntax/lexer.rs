use crate::util::lexer::Lexer;
use crate::syntax::token::Token;

pub fn lex_src(source: &str) -> impl Iterator<Item = Token> {
    Lexer::new(source)
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Token<'src>> {
        // TODO: Interval terms
        None.or_else(|| self.lex_sigil(Token::ThickArrow, "=>"))
            .or_else(|| self.lex_sigil(Token::ThinArrow, "->"))
            .or_else(|| self.lex_sigil(Token::UnitVal, "()"))
            .or_else(|| self.lex_sigil(Token::CloseParen, ")"))
            .or_else(|| self.lex_sigil(Token::OpenParen, "("))
            .or_else(|| self.lex_sigil(Token::OpenBracket, "["))
            .or_else(|| self.lex_sigil(Token::CloseBracket, "]"))
            .or_else(|| self.lex_sigil(Token::Equals, "="))
            .or_else(|| self.lex_sigil(Token::Colon, ":"))
            .or_else(|| self.lex_sigil(Token::Comma, ","))
            .or_else(|| self.lex_sigil(Token::Zero, "0"))
            .or_else(|| self.lex_sigil(Token::One, "1"))
            .or_else(|| self.lex_keyword(Token::Lambda, "Lambda"))
            .or_else(|| self.lex_keyword(Token::Pi, "Pi"))
            .or_else(|| self.lex_keyword(Token::UnitType, "Unit"))
            .or_else(|| self.lex_keyword(Token::Path, "Path"))
            .or_else(|| self.lex_keyword(Token::PathBind, "PathBind"))
            .or_else(|| self.lex_keyword(Token::Meet, "Meet"))
            .or_else(|| self.lex_keyword(Token::Join, "Join"))
            .or_else(|| self.lex_keyword(Token::Inv, "Inv"))
            .or_else(|| self.lex_while(Token::VariableName, char::is_alphabetic))
    }
}