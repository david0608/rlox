use crate::scan::span::Span;

pub struct IdentifierToken {
    name: String,
    span: Span,
}

impl IdentifierToken {
    pub fn new(name: &str, span: Span) -> IdentifierToken {
        IdentifierToken {
            name: name.to_owned(),
            span,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn lexeme(&self) -> &str {
        &self.name
    }

    pub fn span(&self) -> Span {
        self.span
    }
}
