use crate::util::lexer::Lexer;
use crate::syntax::expr::Variable;
use crate::toplevel::TopLevelErr;

pub enum Command<'src> {
    Define(Variable, &'src str),
    Eval(&'src str),
    TypeInfer(&'src str),
    AssumeType(Variable),
    PrintCtx,
}

type TopLevelResult<T> = Result<T, TopLevelErr>;

pub fn parse_command(src: &str) -> TopLevelResult<Command> {
    let mut lexer = Lexer::new(src);

    if lexer.skip_keyword("eval") {
        let source = lexer.remaining_source();
        Ok(Command::Eval(source))
    } else if lexer.skip_keyword("typeof") {
        let source = lexer.remaining_source();
        Ok(Command::TypeInfer(source))
    } else if lexer.skip_keyword("define") {
        if let Some(var) = lexer.skip_chars_while(char::is_alphabetic) {
            let var = var.to_owned();
            if !lexer.skip_sigil(":=") {
                return Err(TopLevelErr::ExpectedAssignment)
            }

            let rest = lexer.remaining_source();
            Ok(Command::Define(var, rest))
        } else {
            Err(TopLevelErr::BadVariableName)
        }
    } else if lexer.skip_keyword("assumetype") {
        if let Some(var) = lexer.skip_chars_while(char::is_alphabetic) {
            Ok(Command::AssumeType(var.to_owned()))
        } else {
            Err(TopLevelErr::BadVariableName)
        }
    } else if lexer.skip_keyword("printctx") {
        Ok(Command::PrintCtx)
    } else {
        Err(TopLevelErr::UnknownCommand)
    }
}