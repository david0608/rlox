use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::visitor::print::Printable;
use crate::parse::expression::BoxedExpression;
use super::{
    Statement,
    BoxedStatement,
};
use crate::impl_debug_for_printable;

pub struct VarDeclareStatement {
    name: String,
    initializer: Option<BoxedExpression>,
    code_span: CodeSpan,
}

impl VarDeclareStatement {
    pub fn new(name: &str, initializer: Option<BoxedExpression>, code_span: CodeSpan) -> VarDeclareStatement {
        VarDeclareStatement {
            name: name.to_owned(),
            initializer,
            code_span,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn initializer(&self) -> Option<&BoxedExpression> {
        self.initializer.as_ref()
    }
}

impl Code for VarDeclareStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for VarDeclareStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            VarDeclareStatement::new(
                self.name(),
                self.initializer().map(|e| e.clone()),
                self.code_span(),
            )
        )
    }
}

impl_debug_for_printable!(VarDeclareStatement);

#[macro_export]
macro_rules! var_declare_statement {
    ( $name:expr, $initializer:expr, $code_span:expr ) => {
        Box::new(
            VarDeclareStatement::new(
                $name,
                $initializer,
                $code_span,
            )
        )
    };
}
