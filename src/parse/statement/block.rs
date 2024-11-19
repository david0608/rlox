use std::{
    rc::Rc,
    cell::RefCell,
};
use crate::{
    code::{
        Code,
        code_span::CodeSpan,
    },
    parse::statement::Statement,
    environment::{
        Environment,
        EnvironmentT,
    },
    error::RuntimeError,
    execute::{
        Execute,
        ExecuteOk,
    },
    print::Print,
    resolve::{
        ResolveCtx,
        ResolveError,
    },
    impl_debug_for_printable
};

pub struct BlockStatement {
    statements: Vec<Rc<dyn Statement>>,
    code_span: CodeSpan,
}

impl BlockStatement {
    pub fn new(statements: Vec<Rc<dyn Statement>>, code_span: CodeSpan) -> BlockStatement {
        BlockStatement {
            statements,
            code_span,
        }
    }

    pub fn statements(&self) -> &Vec<Rc<dyn Statement>> {
        &self.statements
    }
}

impl Code for BlockStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for BlockStatement {
    fn print(&self) -> String {
        let strs = self.statements().iter().map(|s| s.print()).collect::<Vec<String>>();
        format!("{{{}}}", strs.join(" "))
    }
}

impl_debug_for_printable!(BlockStatement);

impl Execute for BlockStatement {
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        let env = env.new_child();
        for statement in self.statements() {
            let ok = statement.execute(&env)?;
            match ok {
                ExecuteOk::KeepGoing => {
                    // do nothing.
                }
                ExecuteOk::Break => {
                    return Ok(ExecuteOk::Break);
                }
                ExecuteOk::Return(v) => {
                    return Ok(ExecuteOk::Return(v));
                }
            }
        }
        return Ok(ExecuteOk::KeepGoing);
    }
}

impl Statement for BlockStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Statement>, ResolveError> {
        context.begin();
        let stmts = self.statements.iter().map(|s| s.resolve(context)).collect();
        context.end();
        match stmts {
            Ok(stmts) => {
                return Ok(
                    Rc::new(
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
        Rc::new(
            BlockStatement::new(
                $statements,
                $code_span,
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        code::code_span::new_code_span,
        parse::{
            expression::variable::VariableExpression,
            statement::{
                block::BlockStatement,
                expression::ExpressionStatement,
                var_declare::VarDeclareStatement,
            }
        },
        value::Value,
        environment::EnvironmentT,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
        execute::ExecuteOk,
        print::Print,
        resolve::{
            ResolveError,
            ResolveErrorEnum,
        },
        utils::{
            Downcast,
            test_utils::{
                TestContext,
                parse_statement,
                parse_statement_unknown,
            },
        },
    };

    #[test]
    fn test_block_statement_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("{var foo; var bar = true;}", "{var foo; var bar = true;}"),
            ("{var a; {a = true;}}", "{var a; {(= a true);}}"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_statement::<BlockStatement>(src).print(), expect);
        }
    }

    #[test]
    fn test_block_statement_execute_keep_going() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = 1;");
        assert_eq!(
            ctx.execute(
                parse_statement::<BlockStatement>(
                    "
                    {
                        var bar = 2;
                        foo = bar;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing)
        );
        assert_eq!(
            ctx.environment.get("foo", 0),
            Some(Value::Number(2.0)),
        );
    }

    #[test]
    fn test_block_statement_execute_break() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = 1;");
        assert_eq!(
            ctx.execute(
                parse_statement::<BlockStatement>(
                    "
                    {
                        foo = 2;
                        break;
                        foo = 3;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::Break)
        );
        assert_eq!(
            ctx.environment.get("foo", 0),
            Some(Value::Number(2.0))
        );
    }

    #[test]
    fn test_block_statement_execute_return() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = 1;");
        assert_eq!(
            ctx.execute(
                parse_statement::<BlockStatement>(
                    "
                    {
                        foo = 2;
                        return foo;
                        foo = 3;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::Return(Value::Number(2.0)))
        );
        assert_eq!(
            ctx.environment.get("foo", 0),
            Some(Value::Number(2.0))
        );
    }

    #[test]
    fn test_block_shadow() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = 1;");
        assert_eq!(
            ctx.execute(
                parse_statement::<BlockStatement>(
                    "
                    {
                        var foo = 2;
                        foo = 3;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing)
        );
        assert_eq!(
            ctx.environment.get("foo", 0),
            Some(Value::Number(1.0))
        );
    }

    #[test]
    fn test_block_execute_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<BlockStatement>(
                    "
                    {
                        var foo;
                        foo = true + 1;
                    }
                    "
                )
                    .as_ref()
            ),
            Err(
                RuntimeError::wrap(
                    RuntimeError::wrap(
                        RuntimeError::new(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            new_code_span(3, 6, 3, 14)
                        ),
                        new_code_span(3, 0, 3, 14),
                    ),
                    new_code_span(3, 0, 3, 15),
                )
            )
        );
    }

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
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 12, 0, 15)
            )
        );
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<BlockStatement>("{ var foo = true; bar; }").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 18, 0, 21)
            )
        );
    }
}
