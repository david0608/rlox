use crate::code::{
    Code,
    code_span::CodeSpan,
};

#[derive(Debug, Clone, PartialEq)]
pub struct NumberToken {
    literal: f64,
    lexeme: String,
    code_span: CodeSpan,
}

impl NumberToken {
    pub fn new(literal: f64 ,lexeme: &str, code_span: CodeSpan) -> NumberToken {
        NumberToken {
            literal,
            code_span,
            lexeme: lexeme.to_owned(),
        }
    }

    pub fn literal(&self) -> f64 {
        self.literal
    }

    pub fn lexeme(&self) -> &str {
        &self.lexeme
    }
}

impl Code for NumberToken {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}
