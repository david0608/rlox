use crate::scan::span::Span;

#[derive(Clone)]
pub struct StringToken {
    literal: String,
    lexeme: String,
    span: Span,
}

impl StringToken {
    pub fn new(literal: &str, lexeme: &str, span: Span) -> StringToken {
        StringToken {
            literal: literal.to_owned(),
            lexeme: lexeme.to_owned(),
            span,
        }
    }

    pub fn literal(&self) -> &str {
        &self.literal
    }

    pub fn lexeme(&self) -> &str {
        &self.lexeme
    }

    pub fn span(&self) -> Span {
        self.span
    }
}
