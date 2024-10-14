use crate::{
    code::code_span::CodeSpan,
    value::Value,
};

pub trait LoxError {
    fn print(&self, src_lines: &Vec<&str>) -> String;
}

impl LoxError for RuntimeError {
    fn print(&self, src_lines: &Vec<&str>) -> String {
        match self.r#enum {
            RuntimeErrorEnum::InvalidNegate(ref v) => {
                let mut out = format!("RuntimeError: Invalid negate on value {}\r\n", v);
                out += self.code_span.debug_string(src_lines).as_ref();
                return out;
            }
            RuntimeErrorEnum::InvalidCompare(ref lv, ref rv) => {
                let mut out = format!("RuntimeError: Invalid compare of value {} and {}\r\n", lv, rv);
                out += self.code_span.debug_string(src_lines).as_ref();
                return out;
            }
            RuntimeErrorEnum::InvalidArithmetic(ref lv, ref rv) => {
                let mut out = format!("RuntimeError: Invalid arithmetic of value {} and {}\r\n", lv, rv);
                out += self.code_span.debug_string(src_lines).as_ref();
                return out;
            }
            RuntimeErrorEnum::DivideByZero => {
                let mut out = "RuntimeError: Divide by zero.\r\n".to_string();
                out += self.code_span.debug_string(src_lines).as_ref();
                return out;
            }
            RuntimeErrorEnum::ArgumentNumberMismatch(e, f) => {
                let mut out = format!("RuntimeError: Argument number mismatch. Expect {} but {} found.\r\n", e, f);
                out += self.code_span.debug_string(src_lines).as_ref();
                return out;
            }
            RuntimeErrorEnum::NotCallable(ref v) => {
                let mut out = format!("RuntimeError: Value {} is not callable.\r\n", v);
                out += self.code_span.debug_string(src_lines).as_ref();
                return out;
            }
            RuntimeErrorEnum::CanNotGetProperty(ref v) => {
                let mut out = format!("RuntimeError: Can not get property of value {}.\r\n", v);
                out += self.code_span.debug_string(src_lines).as_ref();
                return out;
            }
            RuntimeErrorEnum::CanNotSetProperty(ref v) => {
                let mut out = format!("RuntimeError: Can not set property of value {}.\r\n", v);
                out += self.code_span.debug_string(src_lines).as_ref();
                return out;
            }
            RuntimeErrorEnum::RuntimeError(ref error) => {
                return error.print(src_lines);
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum RuntimeErrorEnum {
    InvalidNegate(Value),
    InvalidCompare(Value, Value),
    InvalidArithmetic(Value, Value),
    DivideByZero,
    ArgumentNumberMismatch(usize, usize),
    NotCallable(Value),
    CanNotGetProperty(Value),
    CanNotSetProperty(Value),
    RuntimeError(Box<RuntimeError>),
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    pub r#enum: RuntimeErrorEnum,
    pub code_span: CodeSpan,
}

impl RuntimeError {
    pub fn new(r#enum: RuntimeErrorEnum, code_span: CodeSpan) -> Self {
        RuntimeError {
            r#enum,
            code_span
        }
    }

    pub fn wrap(error: RuntimeError, code_span: CodeSpan) -> Self {
        RuntimeError {
            r#enum: RuntimeErrorEnum::RuntimeError(Box::new(error)),
            code_span,
        }
    }
}
