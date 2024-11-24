use crate::{
    code::{
        CodePoint,
        CodeSpan,
        Annotation,
        SourceCode,
    },
    value::Value,
};

pub enum ScannerError {
    UnexpectedCharacter(CodePoint),
    ExpectCharacterNotFound(&'static str, CodePoint),
}

impl Annotation for ScannerError {
    fn string<T: SourceCode>(&self, source_code: &T) -> String {
        let (mut out, cp) = match self {
            ScannerError::UnexpectedCharacter(cp) => {
                (format!("ScannerError: Unexpected character: {}\r\n", cp.code_string(source_code)), cp)
            }
            ScannerError::ExpectCharacterNotFound(ec, cp) => {
                (format!("ScannerError: Expect character: {} but not found.\r\n", ec), cp)
            }
        };
        out += cp.debug_string(source_code).as_ref();
        return out;
    }
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    UnexpectedEnd(CodePoint),
    UnexpectedToken(CodeSpan),
    ExpectTokenMismatch(String, CodeSpan),
    ExpectTokenNotFound(String, CodePoint),
    ExpectIdentifier(CodeSpan),
    DuplicatedFunctionParameter(CodeSpan),
    ContextNotSupportBreak(CodeSpan),
    DuplicatedMethodDefinition(CodeSpan),
}

impl Annotation for ParserError {
    fn string<T: SourceCode>(&self, source_code: &T) -> String {
        match self {
            ParserError::UnexpectedEnd(cp) => {
                let mut out = "ParserError: Unexpected end.\r\n".to_owned();
                out += cp.debug_string(source_code).as_ref();
                return out;
            }
            ParserError::UnexpectedToken(s) => {
                let mut out = format!("ParserError: Unexpected token: {}\r\n", s.code_string(source_code, 10));
                out += s.debug_string(source_code).as_ref();
                return out;
            }
            ParserError::ExpectTokenMismatch(et, s) => {
                let mut out = format!(
                    "ParserError: Expect token: {} but found: {}\r\n",
                    et,
                    s.code_string(source_code, 10)
                );
                out += s.debug_string(source_code).as_ref();
                return out;
            }
            ParserError::ExpectTokenNotFound(et, cp) => {
                let mut out = format!("ParserError: Expect token: {} but not found.\r\n", et);
                out += cp.debug_string(source_code).as_ref();
                return out;
            }
            ParserError::ExpectIdentifier(s) => {
                let mut out = "ParserError: Expect identifier.\r\n".to_owned();
                out += s.debug_string(source_code).as_ref();
                return out;
            }
            ParserError::DuplicatedFunctionParameter(s) => {
                let mut out = "ParserError: Duplicated function parameter.\r\n".to_owned();
                out += s.debug_string(source_code).as_ref();
                return out;
            }
            ParserError::ContextNotSupportBreak(s) => {
                let mut out = "ParserError: Break statement is not supported in this context.\r\n".to_owned();
                out += s.debug_string(source_code).as_ref();
                return out;
            }
            ParserError::DuplicatedMethodDefinition(s) => {
                let mut out = "ParserError: Duplicated method definition.\r\n".to_owned();
                out += s.debug_string(source_code).as_ref();
                return out;
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ResolveErrorEnum {
    VariableNotDeclared,
    VariableHaveBeenDeclared,
}

#[derive(Debug, PartialEq)]
pub struct ResolveError {
    pub r#enum: ResolveErrorEnum,
    pub code_span: CodeSpan
}

impl ResolveError {
    pub (in crate) fn new(r#enum: ResolveErrorEnum, code_span: CodeSpan) -> Self {
        ResolveError {
            r#enum,
            code_span,
        }
    }
}

impl Annotation for ResolveError {
    fn string<T: SourceCode>(&self, source_code: &T) -> String {
        let mut out = match self.r#enum {
            ResolveErrorEnum::VariableNotDeclared => {
                "ResolveError: Variable not declared.\r\n".to_string()
            }
            ResolveErrorEnum::VariableHaveBeenDeclared => {
                "ResolveError: Variable have been declared.\r\n".to_string()
            }
        };
        out += self.code_span.debug_string(source_code).as_ref();
        return out;
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

impl Annotation for RuntimeError {
    fn string<T: SourceCode>(&self, source_code: &T) -> String {
        let mut out = match self.r#enum {
            RuntimeErrorEnum::InvalidNegate(ref v) => {
                format!("RuntimeError: Invalid negate on value {}\r\n", v)
            }
            RuntimeErrorEnum::InvalidCompare(ref lv, ref rv) => {
                format!("RuntimeError: Invalid compare of value {} and {}\r\n", lv, rv)
            }
            RuntimeErrorEnum::InvalidArithmetic(ref lv, ref rv) => {
                format!("RuntimeError: Invalid arithmetic of value {} and {}\r\n", lv, rv)
            }
            RuntimeErrorEnum::DivideByZero => {
                "RuntimeError: Divide by zero.\r\n".to_string()
            }
            RuntimeErrorEnum::ArgumentNumberMismatch(e, f) => {
                format!("RuntimeError: Argument number mismatch. Expect {} but {} found.\r\n", e, f)
            }
            RuntimeErrorEnum::NotCallable(ref v) => {
                format!("RuntimeError: Value {} is not callable.\r\n", v)
            }
            RuntimeErrorEnum::CanNotGetProperty(ref v) => {
                format!("RuntimeError: Can not get property of value {}.\r\n", v)
            }
            RuntimeErrorEnum::CanNotSetProperty(ref v) => {
                format!("RuntimeError: Can not set property of value {}.\r\n", v)
            }
            RuntimeErrorEnum::RuntimeError(ref error) => {
                error.string(source_code)
            }
        };
        out += self.code_span.debug_string(source_code).as_ref();
        return out;
    }
}
