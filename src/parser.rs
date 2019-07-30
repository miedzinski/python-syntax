use crate::{
    ast,
    grammar::ProgramParser,
    lexer::{LexError, LexErrorKind, Lexer},
    source::Location,
    token::Token,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParseError {
    pub location: Location,
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, location: Location) -> ParseError {
        ParseError { kind, location }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseErrorKind {
    UnexpectedToken,
    Eof,
    Lex(LexErrorKind),
}

impl From<lalrpop_util::ParseError<Location, Token, LexError>> for ParseError {
    fn from(e: lalrpop_util::ParseError<Location, Token, LexError>) -> ParseError {
        use lalrpop_util::ParseError::*;
        #[allow(unused_variables)]
        match e {
            InvalidToken { location } => unimplemented!("invalid token"),
            UnrecognizedEOF { location, expected } => unimplemented!("unrecognized eof"),
            UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => unimplemented!("unrecognized token {:?}, expected {:?}", token, expected),
            ExtraToken {
                token: (start, token, end),
            } => unimplemented!("extra token"),
            User { error } => ParseError::new(ParseErrorKind::Lex(error.kind), error.location),
        }
    }
}

macro_rules! parse_fn {
    ($vis:vis fn $name:ident -> $typ:ident) => {
        $vis fn $name(input: &str) -> Result<ast::$typ, ParseError> {
            let lexer = Lexer::new(input);
            let loc = Location::new(0, 0);
            let tokens = std::iter::once(Ok((loc, Token::$typ, loc))).chain(lexer);
            if let ast::Program::$typ(ret) =
                ProgramParser::new().parse(tokens).map_err(ParseError::from)?
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
