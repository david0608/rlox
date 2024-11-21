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
    scan::token::simple::PRINT_LEXEME,
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
pub struct PrintStatement {
    value: Rc<dyn Expression>,
    code_span: CodeSpan,
}

impl PrintStatement {
    pub fn new(value: Rc<dyn Expression>, code_span: CodeSpan) -> PrintStatement {
        PrintStatement {
            value,
            code_span,
        }
    }

    pub fn value(&self) -> &Rc<dyn Expression> {
        &self.value
    }
}

impl Code for PrintStatement {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        format!("{} {};", PRINT_LEXEME, self.value().to_string())
    }
}

impl Execute for PrintStatement {
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        match self.value().evaluate(env) {
            Ok(v) => {
                println!("{}", v);
                return Ok(ExecuteOk::KeepGoing);
            }
            Err(e) => {
                return Err(RuntimeError::wrap(e, self.code_span().clone()));
            }
        }
    }
}

impl Statement for PrintStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Statement>, ResolveError> {
        Ok(
            Rc::new(
                PrintStatement::new(
                    self.value.resolve(context)?,
                    self.code_span.clone(),
                )
            )
        )
    }
}

#[macro_export]
macro_rules! print_statement {
    ( $expression:expr, $code_span:expr ) => {
        Rc::new(
            PrintStatement::new(
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
            statement::print::PrintStatement,
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
    fn test_print_statement_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("print foo;", "print foo;"),
            ("print 1 + 1;", "print (+ 1 1);"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_statement::<PrintStatement>(src).to_string(), expect);
        }
    }

    #[test]
    fn test_print_execute() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<PrintStatement>("print true;").as_ref(),
            )
                .is_ok(),
            true,
        );
    }

    #[test]
    fn test_print_execute_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(parse_statement::<PrintStatement>("print true + 1;").as_ref()),
            Err(
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        CodeSpan::new(0, 6, 0, 14),
                    ),
                    CodeSpan::new(0, 0, 0, 15),
                )
            )
        );
    }

    #[test]
    fn test_print_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let print_stmt = ctx.resolve_statement::<PrintStatement>(
            parse_statement_unknown("print foo;").as_ref()
        )
            .unwrap();
        let var_expr = print_stmt.value.downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 0);
    }

    #[test]
    fn test_print_statement_resolve_value_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<PrintStatement>("print foo;").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                CodeSpan::new(0, 6, 0, 9)
            )
        );
    }
}
