use crate::code::Code;
use crate::code::code_span::CodeSpan;

#[derive(Debug, Clone, PartialEq)]
pub struct StringToken {
    literal: String,
    lexeme: String,
    code_span: CodeSpan,
}

impl StringToken {
    pub fn new(literal: &str, lexeme: &str, code_span: CodeSpan) -> StringToken {
        StringToken {
            literal: literal.to_owned(),
            lexeme: lexeme.to_owned(),
            code_span,
        }
    }

    pub fn literal(&self) -> &str {
        &self.literal
    }

    pub fn lexeme(&self) -> &str {
        &self.lexeme
    }
}

impl Code for StringToken {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}
