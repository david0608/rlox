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
        statement::Statement,
    },
    environment::Environment,
    error::RuntimeError,
    execute::{
        Execute,
        ExecuteOk,
    },
    resolve::{
        ResolveCtx,
        ResolveError,
    },
};

#[derive(Debug)]
pub struct ExpressionStatement {
    expression: Rc<dyn Expression>,
    code_span: CodeSpan,
}

impl ExpressionStatement {
    pub fn new(expression: Rc<dyn Expression>, code_span: CodeSpan) -> ExpressionStatement {
        ExpressionStatement {
            expression,
            code_span,
        }
    }

    pub fn expression(&self) -> &Rc<dyn Expression> {
        &self.expression
    }
}

impl Code for ExpressionStatement {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        format!("{};", self.expression().to_string())
    }
}

impl Execute for ExpressionStatement {
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        if let Err(e) = self.expression().evaluate(env) {
            return Err(RuntimeError::wrap(e, self.code_span().clone()));
        }
        else {
            return Ok(ExecuteOk::KeepGoing);
        }
    }
}

impl Statement for ExpressionStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Statement>, ResolveError> {
        Ok(
            Rc::new(
                ExpressionStatement::new(
                    self.expression.resolve(context)?,
                    self.code_span.clone(),
                )
            )
        )
    }
}

#[macro_export]
macro_rules! expression_statement {
    ( $expression:expr, $code_span:expr ) => {
        Rc::new(
            ExpressionStatement::new(
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
            statement::expression::ExpressionStatement,
        },
        value::Value,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
        execute::ExecuteOk,
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
        }
    };

    #[test]
    fn test_expression_statement_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("true;", "true;"),
            ("1 + 1;", "(+ 1 1);"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_statement::<ExpressionStatement>(src).to_string(), expect);
        }
    }

    #[test]
    fn test_expression_statement_execute() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(parse_statement::<ExpressionStatement>("true;").as_ref()),
            Ok(ExecuteOk::KeepGoing)
        );
    }

    #[test]
    fn test_expression_statement_execute_expression_evaluate_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<ExpressionStatement>("true + 1;").as_ref()
            ),
            Err(
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        CodeSpan::new(0, 0, 0, 8),
                    ),
                    CodeSpan::new(0, 0, 0, 9),
                )
            )
        );
    }

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
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                CodeSpan::new(0, 0, 0, 3)
            )
        );
    }
}
