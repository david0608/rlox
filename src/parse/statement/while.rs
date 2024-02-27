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

pub struct WhileStatement {
    condition: Option<Expression>,
    body: Statement,
    code_span: CodeSpan,
}

impl WhileStatement {
    pub fn new(
        condition: Option<Expression>,
        body: Statement,
        code_span: CodeSpan
    ) -> WhileStatement
    {
        WhileStatement {
            condition,
            body,
            code_span,
        }
    }

    pub fn condition(&self) -> Option<&Expression> {
        self.condition.as_ref()
    }

    pub fn body(&self) -> &Statement {
        &self.body
    }
}

impl Code for WhileStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl AsStatement for WhileStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        let condition = if let Some(e) = self.condition.as_ref() {
            Some(e.resolve(context)?)
        }
        else {
            None
        };
        return Ok(
            Statement(
                Rc::new(
                    WhileStatement::new(
                        condition,
                        self.body.resolve(context)?,
                        self.code_span.clone(),
                    )
                )
            )
        );
    }
}

#[macro_export]
macro_rules! while_statement {
    ( $body:expr, $span:expr ) => {
        Statement(
            Rc::new(
                WhileStatement::new(
                    None,
                    $body,
                    $span,
                )
            )
        )
    };

    ( $condition:expr, $body:expr, $span:expr ) => {
        Statement(
            Rc::new(
                WhileStatement::new(
                    Some($condition),
                    $body,
                    $span,
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
    use crate::utils::test_utils::{
        TestContext,
        parse_statement,
        parse_statement_unknown,
    };
    use crate::resolve_error;

    #[test]
    fn test_while_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let while_stmt = ctx.resolve_statement::<WhileStatement>(
            parse_statement_unknown("while (foo) { print foo; }").as_ref()
        )
            .unwrap();

        let cond_expr = while_stmt.condition().unwrap().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(cond_expr.binding(), 0);

        let body_stmt = while_stmt.body().downcast_ref::<BlockStatement>().unwrap();
        let print_stmt = body_stmt.statements()[0].downcast_ref::<PrintStatement>().unwrap();
        let var_expr = print_stmt.value().downcast_ref::<VariableExpression>().unwrap();
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
