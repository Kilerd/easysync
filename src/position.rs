#[derive(Debug)]
pub(crate) struct Position {
    ch: i32,
    line: i32,
}

impl Default for Position {
    fn default() -> Self {
        Self { ch: 0, line: 0 }
    }
}

impl Position {
    pub fn new(ch: i32, line: i32) -> Self {
        Position { ch, line }
    }
    pub fn before(&self, other: &Position) -> bool {
        (self.line < other.line) || (self.line == other.line && self.ch < other.ch)
    }
    pub fn equal(&self, other: &Position) -> bool {
        (self.line == other.line) && (self.ch == other.ch)
    }
    pub fn add(&mut self, chars: i32, lines: i32) {
        self.ch += chars;
        self.line += lines;
    }

    pub fn advance(&mut self, chars: i32, lines: i32) {
        self.add(chars, lines);
        if lines > 0 {
            self.ch = 0;
        }
    }
}

#[cfg(test)]
mod test {
    use crate::position::Position;

    #[test]
    fn test_new() {
        let position = Position::default();
        assert_eq!(position.ch, 0);
        assert_eq!(position.line, 0);
    }

    #[test]
    fn test_add() {
        let mut position = Position::new(1, 1);
        position.add(1, 0);
        assert_eq!(position.ch, 2);
        assert_eq!(position.line, 1);

        let mut position = Position::new(1, 1);
        position.add(1, 1);
        assert_eq!(position.ch, 2);
        assert_eq!(position.line, 2);
    }

    #[test]
    fn test_advance() {
        let mut position = Position::new(1, 1);
        position.advance(1, 1);
        assert_eq!(position.ch, 0);
        assert_eq!(position.line, 2);
    }

    #[test]
    fn test_compare() {
        assert!(Position::new(1, 0).before(&Position::new(0, 1)))
    }
}
