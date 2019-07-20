#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Location {
    line: usize,
    col: usize,
}

impl Location {
    pub fn new(line: usize, col: usize) -> Location {
        Location { line, col }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.col
    }
}
