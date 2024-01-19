use crate::code::code_span::CodeSpan;
use crate::value::Value;

pub trait LoxError {
    fn print(&self, src_lines: &Vec<&str>) -> String;
}

#[derive(Debug, PartialEq)]
pub enum RuntimeErrorEnum {
    InvalidNegate(Value),
    InvalidCompare(Value, Value),
    InvalidArithmetic(Value, Value),
    DivideByZero(Value, Value),
    VariableNotDeclared,
    ArgumentNumberMismatch(usize, usize),
    NotCallable(Value),
    MultipleDeclaration,
    RuntimeError,
    Unknown,
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    pub r#enum: RuntimeErrorEnum,
    pub code_span: Option<CodeSpan>,
    pub cause: Option<Box<RuntimeError>>,
}

#[macro_export]
macro_rules! runtime_error {
    (
        $enum:expr,
        ,
    ) => {
        RuntimeError {
            r#enum: $enum,
            code_span: None,
            cause: None,
        }
    };

    (
        $enum:expr,
        $code_span:expr,
    ) => {
        RuntimeError {
            r#enum: $enum,
            code_span: Some($code_span),
            cause: None,
        }
    };

    (
        $enum:expr,
        ,
        $cause:expr
    ) => {
        RuntimeError {
            r#enum: $enum,
            code_span: None,
            cause: Some(Box::new($cause)),
        }
    };

    (
        $enum:expr,
        $code_span:expr,
        $cause:expr
    ) => {
        RuntimeError {
            r#enum: $enum,
            code_span: Some($code_span),
            cause: Some(Box::new($cause)),
        }
    };
}
