use crate::{
    ast, error::Error, grammar::ProgramParser, lexer::Lexer, source::Location, token::Token,
};

macro_rules! parse_fn {
    ($vis:vis fn $name:ident -> $typ:ident) => {
        $vis fn $name(input: &str) -> Result<ast::$typ, Error> {
            let lexer = Lexer::new(input);
            let loc = Location::new(0, 0);
            let tokens = std::iter::once(Ok((loc, Token::$typ, loc))).chain(lexer);
            if let ast::Program::$typ(ret) =
                ProgramParser::new().parse(tokens).map_err(Error::from)?
            {
                Ok(ret)
            } else {
                unreachable!()
            }
        }
    }
}

parse_fn!(pub fn parse_module -> Module);
parse_fn!(pub fn parse_interactive -> Interactive);
parse_fn!(pub fn parse_eval -> Eval);
