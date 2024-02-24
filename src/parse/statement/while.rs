use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::parse::expression::BoxedExpression;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use super::{
    Statement,
    BoxedStatement,
};

pub struct WhileStatement {
    condition: Option<BoxedExpression>,
    body: BoxedStatement,
    code_span: CodeSpan,
}

impl WhileStatement {
    pub fn new(
        condition: Option<BoxedExpression>,
        body: BoxedStatement,
        code_span: CodeSpan
    ) -> WhileStatement
    {
        WhileStatement {
            condition,
            body,
            code_span,
        }
    }

    pub fn condition(&self) -> Option<&BoxedExpression> {
        self.condition.as_ref()
    }

    pub fn body(&self) -> &BoxedStatement {
        &self.body
    }
}

impl Code for WhileStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for WhileStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            WhileStatement::new(
                self.condition().map(|e| e.clone()),
                self.body().clone(),
                self.code_span(),
            )
        )
    }

    fn resolve(&self, context: &mut ResolveCtx) -> Result<BoxedStatement, ResolveError> {
        let condition = if let Some(e) = self.condition.as_ref() {
            Some(e.resolve(context)?)
        }
        else {
            None
        };
        return Ok(
            Box::new(
                WhileStatement::new(
                    condition,
                    self.body.resolve(context)?,
                    self.code_span.clone(),
                )
            )
        );
    }
}

#[macro_export]
macro_rules! while_statement {
    ( $body:expr, $span:expr ) => {
        Box::new(
            WhileStatement::new(
                None,
                $body,
                $span,
            )
        )
    };

    ( $condition:expr, $body:expr, $span:expr ) => {
        Box::new(
            WhileStatement::new(
                Some($condition),
                $body,
                $span,
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::parse::{
        expression::variable::VariableExpression,
        statement::{
            block::BlockStatement,
            print::PrintStatement,
            r#while::WhileStatement,
        },
    };
    use crate::resolve::{
        ResolveError,
        ResolveErrorEnum,
    };
    use crate::utils::{
        AsAny,
        test_utils::{
            TestContext,
            parse_statement,
            parse_statement_unknown,
        },
    };
    use crate::{
        resolve_error,
        downcast_ref,
    };

    #[test]
    fn test_while_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let while_stmt = ctx.resolve_statement::<WhileStatement>(
            parse_statement_unknown("while (foo) { print foo; }").as_ref()
        )
            .unwrap();

        let cond_expr = downcast_ref!(while_stmt.condition().unwrap(), VariableExpression);
        assert_eq!(cond_expr.binding(), 0);

        let body_stmt = downcast_ref!(while_stmt.body(), BlockStatement);
        let print_stmt = downcast_ref!(body_stmt.statements()[0], PrintStatement);
        let var_expr = downcast_ref!(print_stmt.value(), VariableExpression);
        assert_eq!(var_expr.binding(), 1);
    }

    #[test]
    fn test_while_statement_resolve_condition_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<WhileStatement>("while (foo) { print foo; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 7, 0, 10)
            )
        );
    }

    #[test]
    fn test_while_statement_resolve_body_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<WhileStatement>("while (true) { print foo; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 21, 0, 24)
            )
        );
    }
}
