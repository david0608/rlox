pub mod code_point;
pub mod code_span;
use code_span::CodeSpan;

pub trait Code {
    fn code_span(&self) -> CodeSpan;
}
