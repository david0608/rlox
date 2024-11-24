mod code_point;
mod code_span;

pub use code_point::CodePoint;
pub use code_span::CodeSpan;

pub trait Code {
    fn code_span(&self) -> &CodeSpan;

    fn to_string(&self) -> String;
}

pub trait Annotation {
    fn string<T: SourceCode>(&self, source_code: &T) -> String;
}

pub trait SourceCode {
    fn get_line(&self, line_index: usize) -> Option<&str>;

    fn annotation_string<T: Annotation>(&self, annotation: T) -> String;
}

impl SourceCode for Vec<&str> {
    fn get_line(&self, line_index: usize) -> Option<&str> {
        self.get(line_index).map(|v| *v)
    }

    fn annotation_string<T: Annotation>(&self, annotation: T) -> String {
        annotation.string(self)
    }
}
