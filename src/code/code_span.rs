use super::code_point::CodePoint;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CodeSpan {
    start: CodePoint,
    end: CodePoint,
}

impl CodeSpan {
    pub fn new(start: CodePoint, end: CodePoint) -> CodeSpan {
        CodeSpan {
            start,
            end,
        }
    }

    pub fn start(&self) -> CodePoint {
        self.start
    }

    pub fn end(&self) -> CodePoint {
        self.end
    }

    pub fn code_string(&self, lines: &Vec<&str>, max_len: usize) -> String {
        let line_start = self.start.line();
        let char_start = self.start.char();
        let line_end = self.end.line();
        let char_end = self.end.char();

        if line_start >= lines.len() {
            return "".to_owned();
        }

        if line_start > line_end {
            return "".to_owned();
        }

        let omit = |s: String| -> String { format!("{}...", &s[..(max_len - 3)]) };

        let mut s = "".to_owned();

        for l in line_start..(line_end + 1) {
            let line = lines.get(l).map(|line| line.trim()).unwrap_or("");

            let start = if l == line_start {
                char_start
            }
            else {
                0
            };

            let end = if l != line_end {
                line.len()
            }
            else {
                char_end
            };

            s += line.get(start..end).unwrap_or("");

            if s.len() > max_len {
                return omit(s);
            }
        }

        if s.len() > max_len {
            return omit(s);
        }

        return s;
    }

    pub fn debug_string(&self, lines: &Vec<&str>) -> String {
        let line_start = self.start.line();
        let char_start = self.start.char();
        let line_end = self.end.line();
        let char_end = self.end.char();

        if line_start >= lines.len() {
            return "".to_owned();
        }

        if line_start > line_end {
            return "".to_owned();
        }

        if line_start == line_end && char_start >= char_end {
            return "".to_owned();
        }

        let mut s = "".to_owned();

        for l in line_start..(line_end + 1) {
            let prepend = format!("{}: ", l + 1);
            let line = lines.get(l).map(|line| line.trim()).unwrap_or("");

            s += format!("{}{}\r\n", prepend, line).as_ref();

            let spaces = if l == line_start {
                prepend.len() + char_start
            }
            else {
                prepend.len()
            };

            let carets = if l != line_end {
                (prepend.len() + line.len()).checked_sub(spaces).unwrap_or(0)
            }
            else {
                (prepend.len() + char_end).checked_sub(spaces).unwrap_or(0)
            };

            s += format!("{}{}\r\n", " ".repeat(spaces), "^".repeat(carets)).as_ref();
        }

        return s;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_code_string() {
        let src = "hello\r\n    world!\r\n";
        let lines = src.lines().collect();
        assert_eq!(
            CodeSpan::new(
                CodePoint::new(0, 0),
                CodePoint::new(0, 5),
            ).code_string(&lines, 10),
            "hello",
        );
        assert_eq!(
            CodeSpan::new(
                CodePoint::new(1, 5),
                CodePoint::new(1, 7),
            ).code_string(&lines, 10),
            "",
        );
        assert_eq!(
            CodeSpan::new(
                CodePoint::new(0, 0),
                CodePoint::new(1, 6),
            ).code_string(&lines, 20),
            "helloworld!",
        );
        assert_eq!(
            CodeSpan::new(
                CodePoint::new(0, 0),
                CodePoint::new(1, 6),
            ).code_string(&lines, 10),
            "hellowo...",
        );
    }

    #[test]
    fn test_span_debug_string() {
        let src = "print \"hello\r\n    world!\"\r\n";
        let lines = src.lines().collect();
        assert_eq!(
            CodeSpan::new(
                CodePoint::new(0, 0),
                CodePoint::new(0, 5),
            ).debug_string(&lines),
            concat!(
                "1: print \"hello\r\n",
                "   ^^^^^\r\n",
            ),
        );
        assert_eq!(
            CodeSpan::new(
                CodePoint::new(0, 6),
                CodePoint::new(1, 7),
            ).debug_string(&lines),
            concat!(
                "1: print \"hello\r\n",
                "         ^^^^^^\r\n",
                "2: world!\"\r\n",
                "   ^^^^^^^\r\n",
            ),
        );
    }
}
