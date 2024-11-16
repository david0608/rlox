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
    scan::token::simple::PRINT_LEXEME,
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

pub struct PrintStatement {
    value: Expression,
    code_span: CodeSpan,
}

impl PrintStatement {
    pub fn new(value: Expression, code_span: CodeSpan) -> PrintStatement {
        PrintStatement {
            value,
            code_span,
        }
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}

impl Code for PrintStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for PrintStatement {
    fn print(&self) -> String {
        format!("{} {};", PRINT_LEXEME, self.value().print())
    }
}

impl_debug_for_printable!(PrintStatement);

impl Execute for PrintStatement {
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        match self.value().evaluate(env) {
            Ok(v) => {
                println!("{}", v);
                return Ok(ExecuteOk::KeepGoing);
            }
            Err(e) => {
                return Err(RuntimeError::wrap(e, self.code_span()));
            }
        }
    }
}

impl AsStatement for PrintStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        Ok(
            Statement(
                Rc::new(
                    PrintStatement::new(
                        self.value.resolve(context)?,
                        self.code_span.clone(),
                    )
                )
            )
        )
    }
}

#[macro_export]
macro_rules! print_statement {
    ( $expression:expr, $code_span:expr ) => {
        Statement(
            Rc::new(
                PrintStatement::new(
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
            statement::print::PrintStatement,
        },
        value::Value,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
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
    fn test_print_statement_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("print foo;", "print foo;"),
            ("print 1 + 1;", "print (+ 1 1);"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_statement::<PrintStatement>(src).print(), expect);
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
                        new_code_span(0, 6, 0, 14),
                    ),
                    new_code_span(0, 0, 0, 15),
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
                new_code_span(0, 6, 0, 9)
            )
        );
    }
}
