
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Loc {
    pub start: usize,
    pub end: usize,
    pub line: [usize; 2],
    pub line_start: [usize; 2],
}

impl Loc {
    pub fn new() -> Self {
        Loc {
            // The first byte of the span 
            start: 0,

            // The final byte of the span 
            end: 0,

            // The line the start and end cursors are one
            line: [1, 1],

            // The byte where the line starts
            line_start: [0,0],
        }
    }

    // Length of the span in bytes - not in chars
    fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn advance(&mut self, c: char) {
        self.end += c.len_utf8();
        if c == '\n' {
            // Advance the end
            self.line[1] += 1;
            self.line_start[1] = self.end + c.len_utf8();
        }
    }

    #[inline(always)]
    fn debug_assert_zero_length(&self) {
        debug_assert!(self.start == self.end);
        debug_assert!(self.line[0] == self.line[1]);
        debug_assert!(self.line_start[0] == self.line_start[1]);
        debug_assert!(self.len() == 0);
    }

    pub fn advance_start(&mut self, c: char) {
        let step = c.len_utf8();
        self.start += step;
        if c == '\n' {
            self.line[0] += 1;
            self.line_start[0] += step;
        }
        self.debug_assert_zero_length();
    }

    pub fn start_new(&self) -> Self {
        let mut new_span = self.clone();
        new_span.start = new_span.end;
        new_span.line[0] = new_span.line[1];
        new_span.line_start[0] = new_span.line_start[1];
        new_span
    }

    pub fn combine(&self, end: &Loc) -> Loc {
        Loc{
            start: self.start,
            end: end.end,
            line: [self.line[0], end.line[1]],
            line_start: [self.line_start[0], end.line_start[1]]
        }
    }
}
