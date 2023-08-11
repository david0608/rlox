#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CodePoint {
    pub line: usize,
    pub char: usize,
}

impl CodePoint {
    pub fn new(line: usize, char: usize) -> CodePoint {
        CodePoint {
            line,
            char,
        }
    }

    pub fn char_str<'a, 'b, 'c>(&'a self, lines: &'b Vec<&'c str>) -> &'c str {
        return lines.get(self.line)
            .and_then(|l| l.trim().get(self.char..(self.char + 1)))
            .unwrap_or("");
    }

    pub fn indication_string(&self, lines: &Vec<&str>) -> String {
        let prepend = prepend_string(self.line);
        let mut out = format!(
            "{}{}\r\n",
            prepend,
            lines.get(self.line).map(|l| l.trim()).unwrap_or(""),
        );
        out += format!("{}^\r\n", " ".repeat(prepend.len() + self.char)).as_ref();
        return out;
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: CodePoint,
    pub end: CodePoint,
}

impl Span {
    pub fn new(start: CodePoint, end: CodePoint) -> Span {
        Span {
            start,
            end,
        }
    }

    pub fn token_string(&self, lines: &Vec<&str>, max_len: usize) -> String {
        if self.start.line >= lines.len() {
            return "".to_owned();
        }

        if self.start.line > self.end.line {
            return "".to_owned();
        }

        let omit = |s: String| -> String { format!("{}...", &s[..(max_len - 3)]) };

        if self.start.line == self.end.line {
            let out = lines.get(self.start.line)
                .and_then(|l| l.trim().get(self.start.char..self.end.char))
                .unwrap_or("")
                .to_owned();

            if out.len() > max_len {
                return omit(out);
            }
            else {
                return out;
            }
        }

        let mut out = lines.get(self.start.line)
            .and_then(|l| l.trim().get(self.start.char..))
            .unwrap_or("")
            .to_owned();

        if out.len() > max_len {
            return omit(out);
        }

        for l in (self.start.line + 1)..self.end.line {
            out += lines.get(l).map(|l| l.trim()).unwrap_or("");
            if out.len() > max_len {
                return omit(out);
            }
        }

        out += lines.get(self.end.line)
            .and_then(|l| l.trim().get(..self.end.char))
            .unwrap_or("");

        if out.len() > max_len {
            return omit(out);
        }

        return out;
    }

    pub fn indication_string(&self, lines: &Vec<&str>) -> String {
        if self.start.line >= lines.len() {
            return "".to_owned();
        }

        if self.start.line > self.end.line {
            return "".to_owned();
        }

        if self.start.line == self.end.line {
            if self.start.char >= self.end.char {
                return "".to_owned();
            }
            else {
                let prepend = prepend_string(self.start.line);
                let mut out = format!(
                    "{}{}\r\n",
                    prepend,
                    lines.get(self.start.line).map(|l| l.trim()).unwrap_or(""),
                );
                out += format!(
                    "{}{}\r\n",
                    " ".repeat(prepend.len() + self.start.char),
                    "^".repeat(self.end.char - self.start.char)
                ).as_ref();
                return out;
            }
        }

        let mut out = "".to_owned();

        let prepend = prepend_string(self.start.line);
        let line = lines.get(self.start.line).map(|l| l.trim()).unwrap_or("");
        out += format!("{}{}\r\n", prepend, line).as_ref();
        out += format!(
            "{}{}\r\n",
            " ".repeat(prepend.len() + self.start.char),
            "^".repeat(line.len() - self.start.char),
        ).as_ref();

        for l in (self.start.line + 1)..self.end.line {
            let prepend = prepend_string(l);
            let line = lines.get(l).map(|l| l.trim()).unwrap_or("");
            out += format!("{}{}\r\n", prepend, line).as_ref();
            out += format!(
                "{}{}\r\n",
                " ".repeat(prepend.len()),
                "^".repeat(line.len()),
            ).as_ref();
        }

        let prepend = prepend_string(self.end.line);
        let line = lines.get(self.end.line).map(|l| l.trim()).unwrap_or("");
        out += format!("{}{}\r\n", prepend, line).as_ref();
        out += format!(
            "{}{}\r\n",
            " ".repeat(prepend.len()),
            "^".repeat(self.end.char),
        ).as_ref();

        return out;
    }
}

fn prepend_string(line_index: usize) -> String {
    return format!("{}: ", line_index + 1);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prepens_string() {
        assert_eq!(prepend_string(0), "1: ");
        assert_eq!(prepend_string(1), "2: ");
        assert_eq!(prepend_string(2), "3: ");
    }

    #[test]
    fn test_codepoint_char_str() {
        let src = "hello\r\n    world!\r\n";
        let lines = src.lines().collect();
        assert_eq!(CodePoint::new(0, 0).char_str(&lines), "h");
        assert_eq!(CodePoint::new(0, 5).char_str(&lines), "");
        assert_eq!(CodePoint::new(1, 0).char_str(&lines), "w");
        assert_eq!(CodePoint::new(1, 5).char_str(&lines), "!");
        assert_eq!(CodePoint::new(2, 0).char_str(&lines), "");
        assert_eq!(CodePoint::new(2, 5).char_str(&lines), "");
    }

    #[test]
    fn test_codepoint_indication_string() {
        let src = "hello\r\n    world!\r\n";
        let lines = src.lines().collect();
        assert_eq!(
            CodePoint::new(0, 4).indication_string(&lines),
            concat!(
                "1: hello\r\n",
                "       ^\r\n",
            ),
        );
        assert_eq!(
            CodePoint::new(1, 4).indication_string(&lines),
            concat!(
                "2: world!\r\n",
                "       ^\r\n",
            ),
        );
        assert_eq!(
            CodePoint::new(2, 4).indication_string(&lines),
            concat!(
                "3: \r\n",
                "       ^\r\n",
            ),
        );
    }

    #[test]
    fn test_span_token_string() {
        let src = "hello\r\n    world!\r\n";
        let lines = src.lines().collect();
        assert_eq!(
            Span::new(
                CodePoint::new(0, 0),
                CodePoint::new(0, 5),
            ).token_string(&lines, 10),
            "hello",
        );
        assert_eq!(
            Span::new(
                CodePoint::new(1, 5),
                CodePoint::new(1, 7),
            ).token_string(&lines, 10),
            "",
        );
        assert_eq!(
            Span::new(
                CodePoint::new(0, 0),
                CodePoint::new(1, 6),
            ).token_string(&lines, 20),
            "helloworld!",
        );
    }

    #[test]
    fn test_span_indication_string() {
        let src = "print \"hello\r\n    world!\"\r\n";
        let lines = src.lines().collect();
        assert_eq!(
            Span::new(
                CodePoint::new(0, 0),
                CodePoint::new(0, 5),
            ).indication_string(&lines),
            concat!(
                "1: print \"hello\r\n",
                "   ^^^^^\r\n",
            ),
        );
        assert_eq!(
            Span::new(
                CodePoint::new(0, 6),
                CodePoint::new(1, 7),
            ).indication_string(&lines),
            concat!(
                "1: print \"hello\r\n",
                "         ^^^^^^\r\n",
                "2: world!\"\r\n",
                "   ^^^^^^^\r\n",
            ),
        );
    }
}
