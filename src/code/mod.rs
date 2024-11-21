mod code_point;
mod code_span;

pub use code_point::CodePoint;
pub use code_span::CodeSpan;

pub trait Code {
    fn code_span(&self) -> &CodeSpan;

    fn to_string(&self) -> String;
}
