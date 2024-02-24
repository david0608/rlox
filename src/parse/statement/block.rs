use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use super::{
    Statement,
    BoxedStatement,
};

pub struct BlockStatement {
    statements: Vec<BoxedStatement>,
    code_span: CodeSpan,
}

impl BlockStatement {
    pub fn new(statements: Vec<BoxedStatement>, code_span: CodeSpan) -> BlockStatement {
        BlockStatement {
            statements,
            code_span,
        }
    }

    pub fn statements(&self) -> &Vec<BoxedStatement> {
        &self.statements
    }
}

impl Code for BlockStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for BlockStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            BlockStatement::new(
                self.statements.clone(),
                self.code_span(),
            )
        )
    }

    fn resolve(&self, context: &mut ResolveCtx) -> Result<BoxedStatement, ResolveError> {
        context.begin();
        let stmts = self.statements.iter().map(|s| s.resolve(context)).collect();
        context.end();
        match stmts {
            Ok(stmts) => {
                return Ok(
                    Box::new(
                        BlockStatement::new(
                            stmts,
                            self.code_span.clone(),
                        )
                    )
                );
            }
            Err(e) => {
                return Err(e);
            }
        }
    }
}

#[macro_export]
macro_rules! block_statement {
    ( $statements:expr, $code_span:expr ) => {
        Box::new(
            BlockStatement::new(
                $statements,
                $code_span,
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
            expression::ExpressionStatement,
            var_declare::VarDeclareStatement,
        }
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
    fn test_block_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let block_stmt = ctx.resolve_statement::<BlockStatement>(
            parse_statement_unknown("{ var bar = foo; bar; }").as_ref()
        )
            .unwrap();

        let var_declare_stmt = downcast_ref!(block_stmt.statements()[0], VarDeclareStatement);
        let var_expr = downcast_ref!(var_declare_stmt.initializer().unwrap(), VariableExpression);
        assert_eq!(var_expr.binding(), 1);

        let expr_stmt = downcast_ref!(block_stmt.statements()[1], ExpressionStatement);
        let var_expr = downcast_ref!(expr_stmt.expression(), VariableExpression);
        assert_eq!(var_expr.binding(), 0);
    }

    #[test]
    fn test_block_statement_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<BlockStatement>("{ var foo = bar; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 12, 0, 15)
            )
        );
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<BlockStatement>("{ var foo = true; bar; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 18, 0, 21)
            )
        );
    }
}
