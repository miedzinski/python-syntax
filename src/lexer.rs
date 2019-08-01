use std::{
    collections::HashMap,
    convert::TryInto,
    fmt::{self, Display},
};

use num_bigint::BigUint;
use regex::{CaptureLocations, Regex};

use crate::{
    source::Location,
    token::{self, Token},
};

const TABSIZE: usize = 8;

pub type LexResult = Result<(Location, Token, Location), LexError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub location: Location,
}

impl LexError {
    fn new(kind: LexErrorKind, location: Location) -> LexError {
        LexError { kind, location }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LexErrorKind {
    UnexpectedCharacter(char),
    UnmatchedDedent,
    MixedTabsAndSpaces,
    NonAsciiBytes { offset: usize },
    UnicodeDecode { offset: usize },
    UnterminatedString,
}

impl Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for LexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexErrorKind::UnexpectedCharacter(c) => {
                write!(f, "unexpected character {}", c)
            }
            LexErrorKind::UnmatchedDedent => {
                write!(f, "unmatched indentation")
            }
            LexErrorKind::MixedTabsAndSpaces => {
                write!(f, "inconsistent use of tabs and spaces")
            }
            LexErrorKind::NonAsciiBytes { .. } => {
                write!(f, "bytes can only contains ASCII characters")
            }
            LexErrorKind::UnicodeDecode { .. } => {
                write!(f, "malformed unicode escape")
            },
            LexErrorKind::UnterminatedString => {
                write!(f, "unterminated string")
            },
        }
    }
}

struct ParseStringError {
    offset: usize,
}

impl ParseStringError {
    fn new(offset: usize) -> ParseStringError {
        ParseStringError { offset }
    }

    fn offset(&self) -> usize {
        self.offset
    }
}
pub struct Lexer<'a> {
    input: &'a str,
    exhausted: bool,
    location: Location,
    // Stack of indents with and without expanded tabs
    indents: Vec<(usize, usize)>,
    // Column without expanded tabs
    alt_col: usize,
    // Pending DEDENT tokens
    pending: usize,
    // Parentheses nesting level
    parens: isize,
    // Are we in the new logical line?
    newline: bool,
    caps: CaptureLocations,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.exhausted {
            Some(self.get_token())
        } else {
            None
        }
    }
}

lazy_static::lazy_static! {
    static ref TOKEN_RE: Regex = {
        fn escape_keys<V>(map: &HashMap<&str, V>) -> String {
            let mut tokens = map.keys().collect::<Vec<_>>();
            tokens.sort_unstable_by(|a, b| a.len().cmp(&b.len()).reverse());
            tokens
                .iter()
                .map(|x| regex::escape(x))
                .collect::<Vec<_>>()
                .join("|")
        }
        let pat = format!(
            r#"^(?x:
            (  # 1
                [\ \t\f]+
                | \\ (?: \r\n | [\r\n] )
                | \# .*
            )
            | ( \r\n | [\r\n] )  # 2
            | ( $ ) # 3
            | (  # 4
                (?:
                    (?: [0-9] (?: _? [0-9] )* )? \. [0-9] (?: _? [0-9] )*
                    | [0-9] (?: _? [0-9] )* \.?
                ) [eE] [+-]? [0-9] (?: _? [0-9] )*
                | (?: [0-9] (?: _? [0-9] )* )? \. [0-9] (?: _? [0-9] )*
                | [0-9] (?: _? [0-9] )* \.
            ) ( [jJ] )?  # 5
            | ( [0-9] (?: _? [0-9] )* [jJ] )  # 6
            | (  # 7
                (?: 0[bB] (?: _? [01] )+ )
                | (?: 0[oO] (?: _? [0-7] )+ )
                | (?: 0[xX] (?: _? [0-9a-fA-F] )+ )
                | (?: [1-9] (?: _? [0-9] )* | 0+ (?: _? 0 )* )
            )
            | ( (?-i: rf | fr | rb | br | [rfbu] )? )  # 8
            (?:
                ''' ( (?s: [^\\] | \\. )*? ) '''  # 9
                | """ ( (?s: [^\\] | \\. )*? ) """  # 10
                | ' ( (?s: [^\\\n'] | \\. )*? ) '  # 11
                | " ( (?s: [^\\\n"] | \\. )*? ) "  # 12
                | ( ['"] | ''' | """ )  # 13
            )
            | ( {symbols} )  # 14
            | ( {keywords} ) \b  # 15
            | ( [\p{{XID_Start}}_] \p{{XID_Continue}}* )  # 16
            )"#,
            symbols = escape_keys(&token::SYMBOLS),
            keywords = escape_keys(&token::KEYWORDS),
        );
        Regex::new(&pat).unwrap()
    };

    static ref BYTES_RE: Regex = Regex::new(
        r#"(?x) \\ (?:
        ( [\\'"abfnrtv] )
        | ( [1-7]{1,3} )
        | ( [0-9a-fA-F]{2} )
        | \n
        )"#).unwrap();
    static ref STR_RE: Regex = Regex::new(
        &[BYTES_RE.as_str(),
        r#"| \\ (?:
        N \{ (.*) \}
        | u ( [0-9a-fA-F]{4} )
        | U ( [0-9a-fA-F]{8} )
        )"#].concat()).unwrap();
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            input,
            exhausted: false,
            location: Location::new(1, 1),
            indents: vec![(1, 1)],
            alt_col: 1,
            pending: 0,
            parens: 0,
            newline: true,
            caps: TOKEN_RE.capture_locations(),
        }
    }

    fn consume(&mut self, nbytes: usize) -> &str {
        let (consumed, rest) = self.input.split_at(nbytes);
        self.input = rest;
        let mut bytes = consumed.bytes().peekable();
        while let Some(c) = bytes.next() {
            match c {
                b'\r' => {
                    if bytes.peek() == Some(&&b'\n') {
                        continue;
                    } else {
                        self.location = Location::new(self.location.line + 1, 1);
                        self.alt_col = 1;
                    }
                }
                b'\n' => {
                    self.location = Location::new(self.location.line + 1, 1);
                    self.alt_col = 1;
                }
                b'\t' => {
                    self.location = Location::new(
                        self.location.line,
                        self.location.column + TABSIZE - ((self.location.column - 1) % TABSIZE),
                    );
                    self.alt_col += 1;
                }
                _ => {
                    self.location = Location::new(self.location.line, self.location.column + 1);
                    self.alt_col += 1;
                }
            }
        }
        consumed
    }

    fn matches(&self, nth: usize) -> bool {
        self.caps.get(nth).is_some()
    }

    fn get_token(&mut self) -> LexResult {
        if self.pending > 0 {
            // Process pending DEDENT
            self.pending -= 1;
            let loc = Location::new(self.location.line, 0);
            return Ok((loc, Token::Dedent, loc));
        }

        if TOKEN_RE.captures_read(&mut self.caps, self.input).is_none() {
            self.exhausted = true;
            let c = self.input.chars().next().unwrap();
            return Err(LexError::new(
                LexErrorKind::UnexpectedCharacter(c),
                self.location,
            ));
        }

        if self.newline && !(self.matches(1) || self.matches(2)) {
            // Not a blank line, handle indentation
            let col = self.location.column;
            let (last, alt_last) = *self.indents.last().unwrap();
            let emit_loc = Location::new(self.location.line, col);
            if col == last {
                // No change
                if self.alt_col != alt_last {
                    // "Fix" alt_col so we can recover from error
                    self.alt_col = alt_last;
                    return Err(LexError::new(LexErrorKind::MixedTabsAndSpaces, emit_loc));
                }
            } else if col > last {
                // Indent
                if self.alt_col <= alt_last {
                    self.alt_col = alt_last;
                    return Err(LexError::new(LexErrorKind::MixedTabsAndSpaces, emit_loc));
                }
                self.indents.push((col, self.alt_col));
                return Ok((emit_loc, Token::Indent, emit_loc));
            } else {
                // Dedent
                while self.indents.len() > 1 && col < self.indents.last().unwrap().0 {
                    self.pending += 1;
                    self.indents.pop();
                }
                let (last, alt_last) = *self.indents.last().unwrap();
                if col != last {
                    self.exhausted = true;
                    return Err(LexError::new(LexErrorKind::UnmatchedDedent, emit_loc));
                }
                if self.alt_col != alt_last {
                    self.alt_col = alt_last;
                    return Err(LexError::new(LexErrorKind::MixedTabsAndSpaces, emit_loc));
                }
                return self.get_token();
            }
        }

        let start = self.location;
        let match_len = self.caps.get(0).unwrap().1;
        let lex = &self.input[..match_len];
        self.consume(match_len);
        let end = self.location;

        if self.matches(1) {
            // Skip whitespace, line continuations and comments
            return self.get_token();
        } else if self.matches(2) {
            // Newline
            if !self.newline && self.parens == 0 {
                self.newline = true;
                return Ok((start, Token::Newline, start));
            } else {
                // Blank line / implicitly joined lines
                return self.get_token();
            }
        }

        self.newline = false;

        if self.matches(3) {
            // EOF
            self.exhausted = true;
            Ok((start, Token::Eof, start))
        } else if self.matches(4) && !self.matches(5) {
            // Float
            Ok((start, Token::Float(Self::parse_float(lex).unwrap()), end))
        } else if self.matches(5) || self.matches(6) {
            // Imaginary (float)
            let lex = &lex[..lex.len() - 1]; // Strip trailing j
            Ok((
                start,
                Token::Imaginary(Self::parse_float(lex).unwrap()),
                end,
            ))
        } else if self.matches(7) {
            // Integer
            Ok((start, Token::Integer(Self::parse_int(lex).unwrap()), end))
        } else if self.matches(8) {
            // String
            if self.matches(13) {
                // Unterminated
                self.exhausted = true;
                return Err(LexError::new(LexErrorKind::UnterminatedString, start));
            }
            let prefix = self
                .caps
                .get(8)
                .map(|(start, end)| lex[start..end].to_ascii_lowercase());
            let raw = prefix.as_ref().map_or(false, |x| x.contains('r'));
            let formatted = prefix.as_ref().map_or(false, |x| x.contains('f'));
            let bytes = prefix.as_ref().map_or(false, |x| x.contains('b'));
            let value = self
                .caps
                .get(9)
                .or_else(|| self.caps.get(10))
                .or_else(|| self.caps.get(11))
                .or_else(|| self.caps.get(12))
                .map(|(start, end)| &lex[start..end])
                .unwrap();
            if bytes {
                let value = if !raw {
                    Self::parse_bytes(value).map_err(|e| {
                        LexError::new(LexErrorKind::NonAsciiBytes { offset: e.offset() }, start)
                    })?
                } else {
                    value.as_bytes().to_vec()
                };
                Ok((start, Token::Bytes(value), end))
            } else {
                let value = if !raw {
                    Self::parse_str(value).map_err(|e| {
                        LexError::new(LexErrorKind::UnicodeDecode { offset: e.offset() }, start)
                    })?
                } else {
                    value.to_string()
                };
                if formatted {
                    Ok((start, Token::FormattedString(value), end))
                } else {
                    Ok((start, Token::String(value), end))
                }
            }
        } else if self.matches(14) {
            // Operator or delimiter
            let tok = token::SYMBOLS.get(lex).unwrap();
            match tok {
                Token::ParenOpen | Token::BracketOpen | Token::BraceOpen => self.parens += 1,
                Token::ParenClose | Token::BracketClose | Token::BraceClose => self.parens -= 1,
                _ => (),
            }
            Ok((start, tok.clone(), end))
        } else if self.matches(15) {
            // Keyword
            let tok = token::KEYWORDS.get(lex).unwrap();
            Ok((start, tok.clone(), end))
        } else if self.matches(16) {
            // Identifier
            Ok((start, Token::Name(lex.into()), end))
        } else {
            unreachable!()
        }
    }

    fn parse_bytes(mut s: &str) -> Result<Vec<u8>, ParseStringError> {
        if let Some(offset) = s.bytes().position(|c| !c.is_ascii()) {
            return Err(ParseStringError::new(offset));
        }

        let mut result = vec![];
        let mut caps = BYTES_RE.capture_locations();

        while let Some(cap) = BYTES_RE.captures_read(&mut caps, s) {
            result.extend(s[..cap.start()].bytes());
            s = &s[cap.end()..];
            let escape = &cap.as_str()[1..];
            let value = if caps.get(1).is_some() {
                Self::ascii_escape(escape.as_bytes()[0])
            } else if caps.get(2).is_some() {
                u8::from_str_radix(escape, 8).unwrap()
            } else if caps.get(3).is_some() {
                u8::from_str_radix(escape, 16).unwrap()
            } else {
                // Line continuation
                continue;
            };
            result.push(value);
        }
        result.extend(s.bytes());

        Ok(result)
    }

    fn parse_str(mut s: &str) -> Result<String, ParseStringError> {
        let mut result = String::new();
        let mut caps = STR_RE.capture_locations();
        let mut offset = 0;

        while let Some(cap) = STR_RE.captures_read(&mut caps, s) {
            result.push_str(&s[..cap.start()]);
            s = &s[cap.end()..];
            let escape = &cap.as_str()[1..];
            let value = if caps.get(1).is_some() {
                Self::ascii_escape(escape.as_bytes()[0]) as char
            } else if caps.get(2).is_some() {
                u8::from_str_radix(escape, 8).unwrap() as char
            } else if caps.get(3).is_some() {
                u8::from_str_radix(escape, 16).unwrap() as char
            } else if caps.get(4).is_some() {
                unicode_names2::character(&escape[2..escape.len() - 1])
                    .ok_or_else(|| ParseStringError::new(offset))?
            } else if caps.get(5).is_some() || caps.get(6).is_some() {
                u32::from_str_radix(&escape[1..], 16)
                    .unwrap()
                    .try_into()
                    .map_err(|_| ParseStringError::new(offset))?
            } else {
                // Line continuation
                offset += 2;
                continue;
            };
            result.push(value);
            offset += cap.end();
        }
        result.push_str(s);

        Ok(result)
    }

    fn ascii_escape(c: u8) -> u8 {
        match c {
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            b'a' => b'\x07',
            b'b' => b'\x08',
            b'f' => b'\x0c',
            b'v' => b'\x0b',
            c => c,
        }
    }

    fn parse_int(s: &str) -> Option<BigUint> {
        let mut b = s.as_bytes();
        let radix = match b.get(1) {
            Some(b'b') | Some(b'B') => 2,
            Some(b'o') | Some(b'O') => 8,
            Some(b'x') | Some(b'X') => 16,
            _ => 10,
        };
        if radix != 10 {
            b = &b[2..];
        }
        if b[0] == b'_' {
            // BigUint::parse_bytes rejects leading underscore
            b = &b[1..];
        }
        BigUint::parse_bytes(b, radix)
    }

    fn parse_float(s: &str) -> Option<f64> {
        if !s.contains('_') {
            s.parse().ok()
        } else {
            s.replace('_', "").parse().ok()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token::*;

    fn lex(i: &str) -> Vec<Result<Token, LexErrorKind>> {
        Lexer::new(i)
            .map(|x| x.map(|x| x.1).map_err(|e| e.kind))
            .collect()
    }

    macro_rules! assert_lex {
        ($l:expr, $r:expr) => {{
            assert_eq!(lex($l), vec![Ok($r), Ok(Eof)])
        }};
    }

    #[test]
    fn test_lex_indent() {
        assert_eq!(
            lex("a\n b\nc"),
            vec![
                Ok(Name("a".into())),
                Ok(Newline),
                Ok(Indent),
                Ok(Name("b".into())),
                Ok(Newline),
                Ok(Dedent),
                Ok(Name("c".into())),
                Ok(Eof)
            ]
        );
        assert_eq!(
            lex("  a\n b"),
            vec![
                Ok(Indent),
                Ok(Name("a".into())),
                Ok(Newline),
                Err(LexErrorKind::UnmatchedDedent),
            ]
        );
        assert_eq!(
            lex("  \ta\n\tb"),
            vec![
                Ok(Indent),
                Ok(Name("a".into())),
                Ok(Newline),
                Err(LexErrorKind::MixedTabsAndSpaces),
                Ok(Name("b".into())),
                Ok(Eof)
            ]
        );
    }

    #[test]
    fn test_lex_newline() {
        assert_eq!(
            lex("a\n\r\nb\r"),
            vec![
                Ok(Name("a".into())),
                Ok(Newline),
                Ok(Name("b".into())),
                Ok(Newline),
                Ok(Eof),
            ]
        );
    }

    #[test]
    fn test_lex_integer() {
        assert_lex!("1234567890", Integer(1234567890u64.into()));
        assert_lex!("0b10", Integer(0b10u64.into()));
        assert_lex!("0o12345670", Integer(0o12345670u64.into()));
        assert_lex!("0x123456789abcdef0", Integer(0x123456789abcdef0u64.into()));
        assert_lex!("1_2_3_4_5", Integer(12345u64.into()));
        assert_lex!("0x_1_2_3", Integer(0x1_2_3u64.into()));
        assert_lex!("000", Integer(0u64.into()));
    }

    #[test]
    fn test_lex_float() {
        assert_lex!("3.14", Float(3.14));
        assert_lex!("10.", Float(10.));
        assert_lex!(".001", Float(0.001));
        assert_lex!("1e100", Float(1e100));
        assert_lex!("3.14e-10", Float(3.14e-10));
        assert_lex!("0e0", Float(0e0));
        assert_lex!("3.14_15_93", Float(3.14_15_93));
        assert_lex!("0.1e+10", Float(0.1e+10));
        assert_lex!("1.e1", Float(1.0e1));
    }

    #[test]
    fn lex_string() {
        assert_lex!("'foo'", String("foo".into()));
        assert_lex!("'foo\\tbar'", String("foo\tbar".into()));
        assert_lex!("'\\x'", String("\\x".into()));
        assert_lex!("'\\N{SNOWMAN}'", String("☃".into()));
        assert_lex!("b'\\N{SNOWMAN}'", Bytes(b"\\N{SNOWMAN}".to_vec()));
        assert_lex!("'\\\\\\'\"'", String("\\'\"".into()));
        assert_lex!("'a \\\nb'", String("a b".into()));
        assert_lex!("'''a \nb\\\nc'''", String("a \nbc".into()));
        assert_lex!("r'\\t\\\n\\''", String("\\t\\\n\\'".into()))
    }
}
