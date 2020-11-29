pub struct Lexer<'src> {
    source: &'src str,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Lexer { source }
    }

    fn trim_leading_whitespace(&mut self) {
        self.source = self.source.trim_start();
    }

    pub fn remaining_source(&self) -> &'src str {
        self.source
    }

    pub fn skip_sigil(&mut self, tok_str: &str) -> bool {
        self.trim_leading_whitespace();

        if self.source.starts_with(tok_str) {
            let index = tok_str.len();
            self.source = &self.source[index..];
            true
        } else {
            false
        }
    }

    pub fn lex_sigil<Token>(&mut self, tok: Token, tok_str: &str) -> Option<Token> {
        if self.skip_sigil(tok_str) {
            Some(tok)
        } else {
            None
        }
    }

    pub fn skip_keyword(&mut self, tok_str: &str) -> bool {
        self.trim_leading_whitespace();

        if self.source.starts_with(tok_str) {
            let index = tok_str.len();
            let rest = &self.source[index..];
            if rest.find(char::is_alphabetic) == Some(0) {
                // the token found is part of a longer string, ignore it
                false
            } else {
                self.source = rest;
                true
            }
        } else {
            false
        }
    }

    pub fn lex_keyword<Token>(&mut self, tok: Token, tok_str: &str) -> Option<Token> {
        if self.skip_keyword(tok_str) {
            Some(tok)
        } else {
            None
        }
    }

    pub fn skip_chars_while<P>(&mut self, pred: P) -> Option<&'src str>
        where
            P: Fn(char) -> bool + Copy
    {
        self.trim_leading_whitespace();
        if !self.source.starts_with(pred) { return None }

        if let Some(index) = self.source.find(|c: char| !pred(c)) {
            let (ident, source) = self.source.split_at(index);
            self.source = source;
            if ident.is_empty() {
                None
            } else {
                Some(ident)
            }
        } else {
            let ident = self.source;
            self.source = "";
            Some(ident)
        }
    }

    pub fn lex_while<C, P, Token>(&mut self, constructor: C, pred: P) -> Option<Token>
        where
            C: Fn(&'src str) -> Token,
            P: Fn(char) -> bool + Copy
    {
        Some(constructor(self.skip_chars_while(pred)?))
    }
}
