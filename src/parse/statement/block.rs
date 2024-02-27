use std::rc::Rc;
use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use super::{
    Statement,
    AsStatement,
};

pub struct BlockStatement {
    statements: Vec<Statement>,
    code_span: CodeSpan,
}

impl BlockStatement {
    pub fn new(statements: Vec<Statement>, code_span: CodeSpan) -> BlockStatement {
        BlockStatement {
            statements,
            code_span,
        }
    }

    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}

impl Code for BlockStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl AsStatement for BlockStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        context.begin();
        let stmts = self.statements.iter().map(|s| s.resolve(context)).collect();
        context.end();
        match stmts {
            Ok(stmts) => {
                return Ok(
                    Statement(
                        Rc::new(
                            BlockStatement::new(
                                stmts,
                                self.code_span.clone(),
                            )
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
        Statement(
            Rc::new(
                BlockStatement::new(
                    $statements,
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
    use crate::utils::test_utils::{
        TestContext,
        parse_statement,
        parse_statement_unknown,
    };
    use crate::resolve_error;

    #[test]
    fn test_block_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let block_stmt = ctx.resolve_statement::<BlockStatement>(
            parse_statement_unknown("{ var bar = foo; bar; }").as_ref()
        )
            .unwrap();

        let var_declare_stmt = block_stmt.statements()[0].downcast_ref::<VarDeclareStatement>().unwrap();
        let var_expr = var_declare_stmt.initializer().unwrap().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 1);

        let expr_stmt = block_stmt.statements()[1].downcast_ref::<ExpressionStatement>().unwrap();
        let var_expr = expr_stmt.expression().downcast_ref::<VariableExpression>().unwrap();
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
