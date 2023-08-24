use crate::code::Code;
use crate::code::code_span::CodeSpan;

#[derive(Clone, Debug)]
pub struct IdentifierToken {
    name: String,
    code_span: CodeSpan,
}

impl IdentifierToken {
    pub fn new(name: &str, code_span: CodeSpan) -> IdentifierToken {
        IdentifierToken {
            name: name.to_owned(),
            code_span,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn lexeme(&self) -> &str {
        &self.name
    }
}

impl Code for IdentifierToken {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}
