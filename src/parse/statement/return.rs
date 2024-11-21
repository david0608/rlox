use std::{
    rc::Rc,
    cell::RefCell,
};
use crate::{
    code::{
        Code,
        CodeSpan,
    },
    parse::{
        expression::Expression,
        statement::{
            Statement,
            ExecuteOk,
        },
    },
    scan::token::simple::RETURN_LEXEME,
    value::Value,
    environment::Environment,
    error::RuntimeError,
    resolve::{
        ResolveCtx,
        ResolveError,
    },
};

#[derive(Debug)]
pub struct ReturnStatement {
    expression: Option<Rc<dyn Expression>>,
    code_span: CodeSpan,
}

impl ReturnStatement {
    pub fn new(
        expression: Option<Rc<dyn Expression>>,
        code_span: CodeSpan,
    ) -> ReturnStatement
    {
        ReturnStatement {
            expression,
            code_span,
        }
    }

    pub fn expression(&self) -> Option<&Rc<dyn Expression>> {
        self.expression.as_ref()
    }
}

impl Code for ReturnStatement {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        if let Some(e) = self.expression() {
            format!("{} {};", RETURN_LEXEME, e.to_string())
        }
        else {
            format!("{};", RETURN_LEXEME)
        }
    }
}

impl Statement for ReturnStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Statement>, ResolveError> {
        let expr = if let Some(e) = self.expression.as_ref() {
            Some(e.resolve(context)?)
        }
        else {
            None
        };
        return Ok(
            Rc::new(
                ReturnStatement::new(
                    expr,
                    self.code_span.clone(),
                )
            )
        );
    }

    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        if let Some(e) = self.expression() {
            match e.evaluate(env) {
                Ok(v) => {
                    return Ok(ExecuteOk::Return(v));
                }
                Err(e) => {
                    return Err(RuntimeError::wrap(e, self.code_span().clone()));
                }
            }
        }
        else {
            return Ok(ExecuteOk::Return(Value::Nil));
        }
    }
}

#[macro_export]
macro_rules! return_statement {
    ( $expression:expr, $code_span:expr ) => {
        Rc::new(
            ReturnStatement::new(
                $expression,
                $code_span,
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        code::{
            Code,
            CodeSpan,
        },
        parse::{
            expression::variable::VariableExpression,
            statement::{
                ExecuteOk,
                r#return::ReturnStatement,
            }
        },
        value::Value,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
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
            }
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
            assert_eq!(parse_statement::<ReturnStatement>(src).to_string(), expect);
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
                        CodeSpan::new(0, 7, 0, 15),
                    ),
                    CodeSpan::new(0, 0, 0, 16),
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
                CodeSpan::new(0, 7, 0, 10)
            )
        );
    }
}
