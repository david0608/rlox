use crate::scan::span::Span;
use crate::parse::expr::Expression;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::Stmt;

pub struct VarDeclareStatement {
    name: String,
    initializer: Option<Expression>,
    span: Span,
}

impl VarDeclareStatement {
    pub fn new(name: &str, initializer: Option<Expression>, span: Span) -> VarDeclareStatement {
        VarDeclareStatement {
            name: name.to_owned(),
            initializer,
            span,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn initializer(&self) -> Option<&Expression> {
        self.initializer.as_ref()
    }
}

impl Stmt for VarDeclareStatement {
    fn span(&self) -> Span {
        self.span
    }
}

impl_debug_for_printable!(VarDeclareStatement);

#[macro_export]
macro_rules! var_declare_statement {
    ( $name:expr, $initializer:expr, $span:expr ) => {
        Box::new(
            VarDeclareStatement::new(
                $name,
                $initializer,
                $span,
            )
        )
    };
}
