use std::{
    error,
    fmt::{self, Display},
};

use crate::{source::Location, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub location: Location,
    pub kind: ErrorKind,
}

impl Error {
    pub fn new(kind: ErrorKind, location: Location) -> Error {
        Error { kind, location }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    UnexpectedCharacter(char),
    UnmatchedDedent,
    MixedTabsAndSpaces,
    NonAsciiBytes { offset: usize },
    UnicodeDecode { offset: usize },
    UnterminatedString,
    UnexpectedToken { token: Token, expected: Vec<String> },
    UnexpectedEof,
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match self.kind {
            ErrorKind::UnexpectedCharacter(_) => "unexpected character",
            ErrorKind::UnmatchedDedent => "unmatched indentation",
            ErrorKind::MixedTabsAndSpaces => "inconsistent use of tabs and spaces",
            ErrorKind::NonAsciiBytes { .. } => "bytes can only contains ASCII characters",
            ErrorKind::UnicodeDecode { .. } => "malformed unicode escape",
            ErrorKind::UnterminatedString => "unterminated string",
            ErrorKind::UnexpectedToken { .. } => "unexpected token, expected one of: ",
            ErrorKind::UnexpectedEof => "unexpected EOF",
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::UnexpectedCharacter(c) => write!(f, "unexpected character \"{}\"", c),
            ErrorKind::UnmatchedDedent => write!(f, "unmatched indentation"),
            ErrorKind::MixedTabsAndSpaces => write!(f, "inconsistent use of tabs and spaces"),
            ErrorKind::NonAsciiBytes { .. } => {
                write!(f, "bytes can only contains ASCII characters")
            }
            ErrorKind::UnicodeDecode { .. } => write!(f, "malformed unicode escape"),
            ErrorKind::UnterminatedString => write!(f, "unterminated string"),
            ErrorKind::UnexpectedToken { token, expected } => {
                write!(f, "unexpected {}, expected one of: ", token,)?;
                let mut it = expected.iter();
                write!(f, "{}", it.next().unwrap())?;
                for e in it {
                    write!(f, ", {}", e)?;
                }
                Ok(())
            }
            ErrorKind::UnexpectedEof => write!(f, "unexpected EOF"),
        }
    }
}

impl From<lalrpop_util::ParseError<Location, Token, Error>> for Error {
    fn from(e: lalrpop_util::ParseError<Location, Token, Error>) -> Error {
        use lalrpop_util::ParseError::*;
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
                token: (start, Token::Eof, _),
                ..
            } => Error::new(ErrorKind::UnexpectedEof, start),
            UnrecognizedToken {
                token: (start, token, _),
                expected,
            } => Error::new(ErrorKind::UnexpectedToken { token, expected }, start),
            e @ ExtraToken { .. } => {
                // This is unreachable because parse rules consume all tokens
                // until EOF.
                unreachable!("{:?}", e)
            }
            User { error } => error,
        }
    }
}
