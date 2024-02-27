use std::rc::Rc;
use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::parse::expression::Expression;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use super::{
    Statement,
    AsStatement,
};

pub struct ReturnStatement {
    expression: Option<Expression>,
    code_span: CodeSpan,
}

impl ReturnStatement {
    pub fn new(
        expression: Option<Expression>,
        code_span: CodeSpan,
    ) -> ReturnStatement
    {
        ReturnStatement {
            expression,
            code_span,
        }
    }

    pub fn expression(&self) -> Option<&Expression> {
        self.expression.as_ref()
    }
}

impl Code for ReturnStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl AsStatement for ReturnStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        let expr = if let Some(e) = self.expression.as_ref() {
            Some(e.resolve(context)?)
        }
        else {
            None
        };
        return Ok(
            Statement(
                Rc::new(
                    ReturnStatement::new(
                        expr,
                        self.code_span.clone(),
                    )
                )
            )
        );
    }
}

#[macro_export]
macro_rules! return_statement {
    ( $expression:expr, $code_span:expr ) => {
        Statement(
            Rc::new(
                ReturnStatement::new(
                    $expression,
                    $code_span,
                )
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::parse::{
        expression::variable::VariableExpression,
        statement::r#return::ReturnStatement,
    };
    use crate::resolve::{
        ResolveError,
        ResolveErrorEnum,
    };
    use crate::utils::test_utils::{
        TestContext,
        parse_statement,
        parse_statement_unknown,
    };
    use crate::resolve_error;

    #[test]
    fn test_return_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let return_stmt = ctx.resolve_statement::<ReturnStatement>(
            parse_statement_unknown("return foo;").as_ref()
        )
            .unwrap();
        let var_expr = return_stmt.expression().unwrap().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 0);
    }

    #[test]
    fn test_return_statement_resolve_expression_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<ReturnStatement>("return foo;").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 7, 0, 10)
            )
        );
    }
}
