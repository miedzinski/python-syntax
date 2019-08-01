use std::fmt::{self, Display};

use crate::{
    ast,
    grammar::ProgramParser,
    lexer::{LexError, LexErrorKind, Lexer},
    source::Location,
    token::Token,
};

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub location: Location,
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, location: Location) -> ParseError {
        ParseError { kind, location }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    UnexpectedToken { token: Token, expected: Vec<String> },
    Lex(LexErrorKind),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("syntax error: ")?;
        match self {
            ParseErrorKind::UnexpectedToken { token, expected } => {
                write!(f, "unexpected {:?}, expected one of: ", token,)?;
                let mut it = expected.iter();
                write!(f, "{}", it.next().unwrap())?;
                for e in it {
                    write!(f, ", {}", e)?;
                }
            }
            ParseErrorKind::Lex(LexErrorKind::UnexpectedCharacter(c)) => {
                write!(f, "unexpected character {}", c)?;
            }
            ParseErrorKind::Lex(LexErrorKind::UnmatchedDedent) => {
                write!(f, "unmatched indentation")?;
            }
            ParseErrorKind::Lex(LexErrorKind::MixedTabsAndSpaces) => {
                write!(f, "inconsistent use of tabs and spaces")?;
            }
            ParseErrorKind::Lex(LexErrorKind::NonAsciiBytes { .. }) => {
                write!(f, "bytes can only contains ASCII characters")?;
            }
            ParseErrorKind::Lex(LexErrorKind::UnicodeDecode { .. }) => {
                write!(f, "malformed unicode escape")?;
            }
            ParseErrorKind::Lex(LexErrorKind::UnterminatedString) => {
                write!(f, "unterminated string")?;
            }
        }
        Ok(())
    }
}

impl From<lalrpop_util::ParseError<Location, Token, LexError>> for ParseError {
    fn from(e: lalrpop_util::ParseError<Location, Token, LexError>) -> ParseError {
        use lalrpop_util::ParseError::*;
        #[allow(unused_variables)]
        match e {
            e @ InvalidToken { .. } => {
                // https://github.com/lalrpop/lalrpop/blob/57f72944e39b680c343125289af455dfcd60d04a/lalrpop/src/lexer/intern_token/mod.rs#L207
                // This variant is used only by internal LALRPOP lexer,
                // since we have a custom one this is unimplemented.
                unreachable!("{:?}", e)
            }
            e @ UnrecognizedEOF { .. } => {
                // This is unreachable because our custom lexer outputs
                // EOF token. This will be handled in UnrecognizedToken variant.
                unreachable!("{:?}", e)
            }
            UnrecognizedToken {
                token: (start, token, _),
                expected,
            } => ParseError::new(ParseErrorKind::UnexpectedToken { token, expected }, start),
            e @ ExtraToken { .. } => {
                // This is unreachable because parse rules consume all tokens
                // until EOF.
                unreachable!("{:?}", e)
            }
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
