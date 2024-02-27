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

pub struct ExpressionStatement {
    expression: Expression,
    code_span: CodeSpan,
}

impl ExpressionStatement {
    pub fn new(expression: Expression, code_span: CodeSpan) -> ExpressionStatement {
        ExpressionStatement {
            expression,
            code_span,
        }
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}

impl Code for ExpressionStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl AsStatement for ExpressionStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        Ok(
            Statement(
                Rc::new(
                    ExpressionStatement::new(
                        self.expression.resolve(context)?,
                        self.code_span.clone(),
                    )
                )
            )
        )
    }
}

#[macro_export]
macro_rules! expression_statement {
    ( $expression:expr, $code_span:expr ) => {
        Statement(
            Rc::new(
                ExpressionStatement::new(
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
        statement::expression::ExpressionStatement,
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
    fn test_expression_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.resolve_context.begin();

        let expr_stmt = ctx.resolve_statement::<ExpressionStatement>(
            parse_statement_unknown("foo;").as_ref()
        )
            .unwrap();
        let var_expr = expr_stmt.expression().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 1);
    }

    #[test]
    fn test_expression_statement_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<ExpressionStatement>("foo;").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 0, 0, 3)
            )
        );
    }
}
