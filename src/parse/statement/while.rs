use std::{
    rc::Rc,
    cell::RefCell,
    collections::HashSet,
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
    environment::Environment,
    error::{
        ResolveError,
        RuntimeError,
    },
};

#[derive(Debug)]
pub struct WhileStatement {
    condition: Option<Rc<dyn Expression>>,
    body: Rc<dyn Statement>,
    code_span: CodeSpan,
}

impl WhileStatement {
    pub fn new(
        condition: Option<Rc<dyn Expression>>,
        body: Rc<dyn Statement>,
        code_span: CodeSpan
    ) -> WhileStatement
    {
        WhileStatement {
            condition,
            body,
            code_span,
        }
    }

    pub fn condition(&self) -> Option<&Rc<dyn Expression>> {
        self.condition.as_ref()
    }

    pub fn body(&self) -> &Rc<dyn Statement> {
        &self.body
    }
}

impl Code for WhileStatement {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        if let Some(condition) = self.condition() {
            format!("while {} {}", condition.to_string(), self.body().to_string())
        }
        else {
            format!("while true {}", self.body().to_string())
        }
    }
}

impl Statement for WhileStatement {
    fn resolve(&self, context: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Statement>, ResolveError> {
        let condition = if let Some(e) = self.condition.as_ref() {
            Some(e.resolve(context)?)
        }
        else {
            None
        };
        return Ok(
            Rc::new(
                WhileStatement::new(
                    condition,
                    self.body.resolve(context)?,
                    self.code_span.clone(),
                )
            )
        );
    }

    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        loop {
            if let Some(condition) = self.condition() {
                match condition.evaluate(env) {
                    Ok(v) => {
                        if !v.is_truthy() {
                            return Ok(ExecuteOk::KeepGoing);
                        }
                    }
                    Err(e) => {
                        return Err(RuntimeError::wrap(e, self.code_span().clone()));
                    }
                }
            }

            match self.body().execute(env) {
                Ok(ExecuteOk::KeepGoing) => {
                    // do nothing.
                }
                Ok(ExecuteOk::Break) => {
                    return Ok(ExecuteOk::KeepGoing);
                }
                Ok(ExecuteOk::Return(v)) => {
                    return Ok(ExecuteOk::Return(v));
                }
                Err(err) => {
                    return Err(RuntimeError::wrap(err, self.code_span().clone()));
                }
            }
        }
    }
}

#[macro_export]
macro_rules! while_statement {
    ( $body:expr, $span:expr ) => {
        Rc::new(
            WhileStatement::new(
                None,
                $body,
                $span,
            )
        )
    };

    ( $condition:expr, $body:expr, $span:expr ) => {
        Rc::new(
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
    use crate::{
        code::{
            Code,
            CodeSpan,
        },
        parse::{
            expression::variable::VariableExpression,
            statement::{
                ExecuteOk,
                block::BlockStatement,
                print::PrintStatement,
                r#while::WhileStatement,
            }
        },
        value::Value,
        environment::EnvironmentT,
        error::{
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
    fn test_print_while_statement() {
        let tests: Vec<(&str, &str)> = vec![
            ("while (foo) print \"hello\";", "while foo print \"hello\";"),
            ("while (foo == true) print \"hello\";", "while (== foo true) print \"hello\";"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_statement::<WhileStatement>(src).to_string(), expect);
        }
    }

    #[test]
    fn test_while_statement_execute() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var sum = 0;");
        ctx.execute_src("var i = 3;");
        assert_eq!(
            ctx.execute(
                parse_statement::<WhileStatement>(
                    "
                    while (i > 0) {
                        sum = sum + i;
                        i = i - 1;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing),
        );
        assert_eq!(
            ctx.environment.get("sum", 0).unwrap(),
            Value::Number(6.0),
        );
    }

    #[test]
    fn test_while_statement_execute_break() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var i = 0;");
        assert_eq!(
            ctx.execute(
                parse_statement::<WhileStatement>(
                    "
                    while (i <= 3) {
                        if (i == 2) {
                            break;
                        }
                        else {
                            i = i + 1;
                        }
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing),
        );
        assert_eq!(
            ctx.environment.get("i", 0).unwrap(),
            Value::Number(2.0),
        );
    }

    #[test]
    fn test_while_statement_execute_return() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var i = 0;");
        assert_eq!(
            ctx.execute(
                parse_statement::<WhileStatement>(
                    "
                    while (i <= 3) {
                        if (i == 2) {
                            return i;
                        }
                        else {
                            i = i + 1;
                        }
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::Return(Value::Number(2.0))),
        );
        assert_eq!(
            ctx.environment.get("i", 0).unwrap(),
            Value::Number(2.0),
        );
    }

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
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                CodeSpan::new(0, 7, 0, 10)
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
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                CodeSpan::new(0, 21, 0, 24)
            )
        );
    }
}
