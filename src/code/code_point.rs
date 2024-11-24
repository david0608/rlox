use crate::code::SourceCode;

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

    pub fn code_string<T: SourceCode>(&self, source_code: &T) -> String {
        source_code.get_line(self.line())
            .and_then(
                |l| l.trim().get(self.char()..(self.char() + 1))
            )
            .unwrap_or("")
            .to_string()
    }

    pub fn debug_string<T: SourceCode>(&self, source_code: &T) -> String {
        let prepend = format!("{}: ", self.line + 1);
        let mut s = format!(
            "{}{}\r\n",
            prepend,
            source_code.get_line(self.line).map(|l| l.trim()).unwrap_or(""),
        );
        s += format!("{}^\r\n", " ".repeat(prepend.len() + self.char)).as_ref();
        return s;
    }
}

#[cfg(test)]
mod tests {
    use crate::code::CodePoint;

    #[test]
    fn test_codepoint_str() {
        let src = "hello\r\n    world!\r\n";
        let lines: Vec<&str> = src.lines().collect();
        assert_eq!(CodePoint::new(0, 0).code_string(&lines), "h");
        assert_eq!(CodePoint::new(0, 5).code_string(&lines), "");
        assert_eq!(CodePoint::new(1, 0).code_string(&lines), "w");
        assert_eq!(CodePoint::new(1, 5).code_string(&lines), "!");
        assert_eq!(CodePoint::new(2, 0).code_string(&lines), "");
        assert_eq!(CodePoint::new(2, 5).code_string(&lines), "");
    }

    #[test]
    fn test_codepoint_debug_string() {
        let src = "hello\r\n    world!\r\n";
        let lines: Vec<&str> = src.lines().collect();
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
