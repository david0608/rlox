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
        statement::Statement,
    },
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
    impl_debug_for_printable,
};

pub struct IfStatement {
    condition: Rc<dyn Expression>,
    then_statement: Rc<dyn Statement>,
    else_statement: Option<Rc<dyn Statement>>,
    code_span: CodeSpan,
}

impl IfStatement {
    pub fn new(
        condition: Rc<dyn Expression>,
        then_statement: Rc<dyn Statement>,
        else_statement: Option<Rc<dyn Statement>>,
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

    pub fn condition(&self) -> &Rc<dyn Expression> {
        &self.condition
    }

    pub fn then_statement(&self) -> &Rc<dyn Statement> {
        &self.then_statement
    }

    pub fn else_statement(&self) -> Option<&Rc<dyn Statement>> {
        self.else_statement.as_ref()
    }
}

impl Code for IfStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for IfStatement {
    fn print(&self) -> String {
        if let Some(else_statement) = self.else_statement() {
            format!(
                "if {} {} else {}",
                self.condition().print(),
                self.then_statement().print(),
                else_statement.print()
            )
        }
        else {
            format!(
                "if {} {}",
                self.condition().print(),
                self.then_statement().print()
            )
        }
    }
}

impl_debug_for_printable!(IfStatement);

impl Execute for IfStatement {
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        let condition = match self.condition().evaluate(env) {
            Ok(val) => val.is_truthy(),
            Err(err) => {
                return Err(RuntimeError::wrap(err, self.code_span()));
            }
        };
        let statement = if condition {
            Some(self.then_statement())
        }
        else {
            self.else_statement()
        };
        if let Some(stmt) = statement {
            match stmt.execute(env) {
                Ok(ok) => {
                    return Ok(ok);
                }
                Err(err) => {
                    return Err(RuntimeError::wrap(err, self.code_span()));
                }
            }
        }
        return Ok(ExecuteOk::KeepGoing);
    }
}

impl Statement for IfStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Statement>, ResolveError> {
        let condition = self.condition.resolve(context)?;
        let then = self.then_statement.resolve(context)?;
        let r#else = if let Some(stmt) = self.else_statement.as_ref() {
            Some(stmt.resolve(context)?)
        }
        else {
            None
        };
        return Ok(
            Rc::new(
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
        Rc::new(
            IfStatement::new(
                $condition,
                $then_statement,
                Some($else_statement),
                $code_span,
            )
        )
    };

    ( $condition:expr, $then_statement:expr, $code_span:expr ) => {
        Rc::new(
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
    use crate::{
        code::code_span::new_code_span,
        parse::{
            expression::variable::VariableExpression,
            statement::{
                block::BlockStatement,
                ifelse::IfStatement,
                print::PrintStatement,
            }
        },
        value::Value,
        environment::EnvironmentT,
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
        }
    };

    #[test]
    fn test_ifelse_statement_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("if (true) print \"hello\";", "if true print \"hello\";"),
            ("if (1 + 1 == 2) { print 1; } else { print 2; }", "if (== (+ 1 1) 2) {print 1;} else {print 2;}"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_statement::<IfStatement>(src).print(), expect);
        }
    }

    #[test]
    fn test_ifelse_statement_execute() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = 1;");
        assert_eq!(
            ctx.execute(parse_statement::<IfStatement>("if (true) foo = 2;").as_ref()),
            Ok(ExecuteOk::KeepGoing)
        );
        assert_eq!(
            ctx.environment.get("foo", 0).unwrap(),
            Value::Number(2.0)
        );
        assert_eq!(
            ctx.execute(
                parse_statement::<IfStatement>(
                    "
                    if (false) {
                        foo = 3;
                    } else {
                        foo = 4;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing)
        );
        assert_eq!(
            ctx.environment.get("foo", 0).unwrap(),
            Value::Number(4.0)
        );
    } 

    #[test]
    fn test_ifelse_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let ifelse_stmt = ctx.resolve_statement::<IfStatement>(
            parse_statement_unknown("if (foo) { print foo; } else { print foo; }").as_ref()
        )
            .unwrap();

        let var_expr = ifelse_stmt.condition().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 0);

        let block_stmt = ifelse_stmt.then_statement.downcast_ref::<BlockStatement>().unwrap();
        let print_stmt = block_stmt.statements()[0].downcast_ref::<PrintStatement>().unwrap();
        let var_expr = print_stmt.value().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 1);

        let block_stmt = ifelse_stmt.else_statement.as_ref().unwrap().downcast_ref::<BlockStatement>().unwrap();
        let print_stmt = block_stmt.statements()[0].downcast_ref::<PrintStatement>().unwrap();
        let var_expr = print_stmt.value().downcast_ref::<VariableExpression>().unwrap();
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
            ResolveError::new(
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
            ResolveError::new(
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
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 39, 0, 42)
            )
        );
    }
}
