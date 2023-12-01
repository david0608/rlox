use crate::code::code_span::CodeSpan;
use crate::code::Code;
use super::{
    Statement,
    BoxedStatement,
};
use crate::scan::token::identifier::IdentifierToken;

pub struct FunDeclareStatement {
    name: IdentifierToken,
    parameters: Vec<IdentifierToken>,
    body: Vec<BoxedStatement>,
    code_span: CodeSpan,
}

impl FunDeclareStatement {
    pub fn new(
        name: IdentifierToken,
        parameters: Vec<IdentifierToken>,
        body: Vec<BoxedStatement>,
        code_span: CodeSpan,
    ) -> FunDeclareStatement
    {
        FunDeclareStatement {
            name,
            parameters,
            body,
            code_span,
        }
    }

    pub fn name(&self) -> &IdentifierToken {
        &self.name
    }

    pub fn parameters(&self) -> &Vec<IdentifierToken> {
        &self.parameters
    }

    pub fn body(&self) -> &Vec<BoxedStatement> {
        &self.body
    }
}

impl Code for FunDeclareStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for FunDeclareStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            FunDeclareStatement::new(
                self.name().clone(),
                self.parameters().clone(),
                self.body().clone(),
                self.code_span(),
            )
        )
    }
}

#[macro_export]
macro_rules! fun_declare_statement {
    ( $name:expr, $parameters:expr, $body:expr, $code_span:expr ) => {
        Box::new(
            FunDeclareStatement::new(
                $name,
                $parameters,
                $body,
                $code_span,
            )
        )
    };
}
