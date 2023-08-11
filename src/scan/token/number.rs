use crate::scan::span::Span;

#[derive(Clone)]
pub struct NumberToken {
    literal: f64,
    lexeme: String,
    span: Span,
}

impl NumberToken {
    pub fn new(literal: f64 ,lexeme: &str, span: Span) -> NumberToken {
        NumberToken {
            literal,
            span,
            lexeme: lexeme.to_owned(),
        }
    }

    pub fn literal(&self) -> f64 {
        self.literal
    }

    pub fn lexeme(&self) -> &str {
        &self.lexeme
    }

    pub fn span(&self) -> Span {
        self.span
    }
}
