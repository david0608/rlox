#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CodePoint {
    line: usize,
    char: usize,
}

impl CodePoint {
    pub fn new(line: usize, char: usize) -> CodePoint {
        CodePoint {
            line,
            char,
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn char(&self) -> usize {
        self.char
    }

    pub fn str<'a, 'b, 'c>(&'a self, lines: &'b Vec<&'c str>) -> &'c str {
        return lines.get(self.line)
            .and_then(|l| l.trim().get(self.char..(self.char + 1)))
            .unwrap_or("");
    }

    pub fn debug_string(&self, lines: &Vec<&str>) -> String {
        let prepend = format!("{}: ", self.line + 1);
        let mut s = format!(
            "{}{}\r\n",
            prepend,
            lines.get(self.line).map(|l| l.trim()).unwrap_or(""),
        );
        s += format!("{}^\r\n", " ".repeat(prepend.len() + self.char)).as_ref();
        return s;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_codepoint_str() {
        let src = "hello\r\n    world!\r\n";
        let lines = src.lines().collect();
        assert_eq!(CodePoint::new(0, 0).str(&lines), "h");
        assert_eq!(CodePoint::new(0, 5).str(&lines), "");
        assert_eq!(CodePoint::new(1, 0).str(&lines), "w");
        assert_eq!(CodePoint::new(1, 5).str(&lines), "!");
        assert_eq!(CodePoint::new(2, 0).str(&lines), "");
        assert_eq!(CodePoint::new(2, 5).str(&lines), "");
    }

    #[test]
    fn test_codepoint_debug_string() {
        let src = "hello\r\n    world!\r\n";
        let lines = src.lines().collect();
        assert_eq!(
            CodePoint::new(0, 4).debug_string(&lines),
            concat!(
                "1: hello\r\n",
                "       ^\r\n",
            ),
        );
        assert_eq!(
            CodePoint::new(1, 4).debug_string(&lines),
            concat!(
                "2: world!\r\n",
                "       ^\r\n",
            ),
        );
        assert_eq!(
            CodePoint::new(2, 4).debug_string(&lines),
            concat!(
                "3: \r\n",
                "       ^\r\n",
            ),
        );
    }
}
