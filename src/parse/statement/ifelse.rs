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

    fn resolve(&self, context: &mut ResolveCtx) -> Result<BoxedStatement, ResolveError> {
        let condition = self.condition.resolve(context)?;
        let then = self.then_statement.resolve(context)?;
        let r#else = if let Some(stmt) = self.else_statement.as_ref() {
            Some(stmt.resolve(context)?)
        }
        else {
            None
        };
        return Ok(
            Box::new(
                IfStatement::new(
                    condition,
                    then,
                    r#else,
                    self.code_span.clone(),
                )
            )
        );
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

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::parse::{
        expression::variable::VariableExpression,
        statement::{
            block::BlockStatement,
            ifelse::IfStatement,
            print::PrintStatement,
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
    fn test_ifelse_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let ifelse_stmt = ctx.resolve_statement::<IfStatement>(
            parse_statement_unknown("if (foo) { print foo; } else { print foo; }").as_ref()
        )
            .unwrap();

        let var_expr = downcast_ref!(ifelse_stmt.condition, VariableExpression);
        assert_eq!(var_expr.binding(), 0);

        let block_stmt = downcast_ref!(ifelse_stmt.then_statement, BlockStatement);
        let print_stmt = downcast_ref!(block_stmt.statements()[0], PrintStatement);
        let var_expr = downcast_ref!(print_stmt.value(), VariableExpression);
        assert_eq!(var_expr.binding(), 1);

        let block_stmt = downcast_ref!(ifelse_stmt.else_statement.as_ref().unwrap(), BlockStatement);
        let print_stmt = downcast_ref!(block_stmt.statements()[0], PrintStatement);
        let var_expr = downcast_ref!(print_stmt.value(), VariableExpression);
        assert_eq!(var_expr.binding(), 1);
    }

    #[test]
    fn test_ifelse_statement_resolve_condition_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<IfStatement>("if (foo) { print foo; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 4, 0, 7)
            )
        );
    }

    #[test]
    fn test_ifelse_statement_resolve_then_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<IfStatement>("if (true) { print foo; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 18, 0, 21)
            )
        );
    }

    #[test]
    fn test_ifelse_statement_resolve_else_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<IfStatement>("if (true) { print true; } else { print foo; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 39, 0, 42)
            )
        );
    }
}
