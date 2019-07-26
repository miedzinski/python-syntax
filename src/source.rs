#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Location {
    pub fn new(line: usize, column: usize) -> Location {
        Location { line, column }
    }
}

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}

impl Span {
    pub fn new(start: Location, end: Location) -> Span {
        Span { start, end }
    }
}

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub struct Spanned<T> {
    pub span: Span,
    pub kind: T,
}
