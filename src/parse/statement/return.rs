use std::{
    rc::Rc,
    cell::RefCell,
};
use crate::{
    code::{
        Code,
        code_span::CodeSpan,
    },
    parse::{
        expression::Expression,
        statement::{
            Statement,
            AsStatement,
        }
    },
    scan::token::simple::RETURN_LEXEME,
    value::Value,
    environment::Environment,
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

impl Print for ReturnStatement {
    fn print(&self) -> String {
        if let Some(e) = self.expression() {
            format!("{} {};", RETURN_LEXEME, e.print())
        }
        else {
            format!("{};", RETURN_LEXEME)
        }
    }
}

impl_debug_for_printable!(ReturnStatement);

impl Execute for ReturnStatement {
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        if let Some(e) = self.expression() {
            match e.evaluate(env) {
                Ok(v) => {
                    return Ok(ExecuteOk::Return(v));
                }
                Err(e) => {
                    return Err(RuntimeError::wrap(e, self.code_span()));
                }
            }
        }
        else {
            return Ok(ExecuteOk::Return(Value::Nil));
        }
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
    use crate::{
        code::code_span::new_code_span,
        parse::{
            expression::variable::VariableExpression,
            statement::r#return::ReturnStatement,
        },
        value::Value,
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
        utils::test_utils::{
            TestContext,
            parse_statement,
            parse_statement_unknown,
        }
    };

    #[test]
    fn test_return_statement_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("return;", "return;"),
            ("return true;", "return true;"),
            ("return foo;", "return foo;"),
            ("return 1 + 1;", "return (+ 1 1);"),
            ("return bar();", "return (call bar );"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_statement::<ReturnStatement>(src).print(), expect);
        }
    }

    #[test]
    fn test_return_statement_execute() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<ReturnStatement>("return;").as_ref()
            )
                .unwrap(),
            ExecuteOk::Return(Value::Nil),
        );
        assert_eq!(
            ctx.execute(
                parse_statement::<ReturnStatement>("return true;").as_ref()
            )
                .unwrap(),
            ExecuteOk::Return(Value::Bool(true)),
        );
    }

    #[test]
    fn test_return_statement_execute_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(parse_statement::<ReturnStatement>("return true + 1;").as_ref()),
            Err(
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        new_code_span(0, 7, 0, 15),
                    ),
                    new_code_span(0, 0, 0, 16),
                )
            )
        );
    }

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
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 7, 0, 10)
            )
        );
    }
}
