use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::parse::expression::BoxedExpression;
use super::{
    Statement,
    BoxedStatement,
};

pub struct IfStatement {
    condition: BoxedExpression,
    then_statement: BoxedStatement,
    else_statement: Option<BoxedStatement>,
    code_span: CodeSpan,
}

impl IfStatement {
    pub fn new(
        condition: BoxedExpression,
        then_statement: BoxedStatement,
        else_statement: Option<BoxedStatement>,
        code_span: CodeSpan,
    ) -> IfStatement
    {
        IfStatement {
            condition,
            then_statement,
            else_statement,
            code_span,
        }
    }

    pub fn condition(&self) -> &BoxedExpression {
        &self.condition
    }

    pub fn then_statement(&self) -> &BoxedStatement {
        &self.then_statement
    }

    pub fn else_statement(&self) -> Option<&BoxedStatement> {
        self.else_statement.as_ref()
    }
}

impl Code for IfStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for IfStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            IfStatement::new(
                self.condition().clone(),
                self.then_statement().clone(),
                self.else_statement().map(|s| s.clone()),
                self.code_span(),
            )
        )
    }
}

#[macro_export]
macro_rules! if_statement {
    ( $condition:expr, $then_statement:expr, $else_statement:expr, $code_span:expr ) => {
        Box::new(
            IfStatement::new(
                $condition,
                $then_statement,
                Some($else_statement),
                $code_span,
            )
        )
    };

    ( $condition:expr, $then_statement:expr, $code_span:expr ) => {
        Box::new(
            IfStatement::new(
                $condition,
                $then_statement,
                None,
                $code_span,
            )
        )
    };
}
