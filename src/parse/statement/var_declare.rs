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
    scan::token::{
        identifier::IdentifierToken,
        simple::VAR_LEXEME,
    },
    value::Value,
    environment::{
        Environment,
        EnvironmentOps,
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
        ResolveErrorEnum,
    },
    impl_debug_for_printable,
};

pub struct VarDeclareStatement {
    name: Rc<IdentifierToken>,
    initializer: Option<Expression>,
    code_span: CodeSpan,
}

impl VarDeclareStatement {
    pub fn new(
        name: Rc<IdentifierToken>,
        initializer: Option<Expression>,
        code_span: CodeSpan
    ) -> VarDeclareStatement
    {
        VarDeclareStatement {
            name,
            initializer,
            code_span,
        }
    }

    pub fn name(&self) -> &IdentifierToken {
        &self.name
    }

    pub fn initializer(&self) -> Option<&Expression> {
        self.initializer.as_ref()
    }
}

impl Code for VarDeclareStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for VarDeclareStatement {
    fn print(&self) -> String {
        if let Some(i) = self.initializer() {
            format!("{} {} = {};", VAR_LEXEME, self.name().name(), i.print())
        }
        else {
            format!("{} {};", VAR_LEXEME, self.name().name())
        }
    }
}

impl_debug_for_printable!(VarDeclareStatement);

impl Execute for VarDeclareStatement {
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        let mut value = Value::Nil;
        if let Some(i) = self.initializer() {
            match i.evaluate(env) {
                Ok(v) => value = v,
                Err(e) => {
                    return Err(RuntimeError::wrap(e, self.code_span()));
                }
            }
        };
        env.declare(self.name().name(), value)
            .expect("Variable name have been declared. This may not happen if the variable declaration statement have been successfully resolved.");
        return Ok(ExecuteOk::KeepGoing);
    }
}

impl AsStatement for VarDeclareStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        let initializer = if let Some(e) = self.initializer.as_ref() {
            Some(e.resolve(context)?)
        }
        else {
            None
        };
        if context.declare(self.name.name()).is_err() {
            return Err(
                ResolveError::new(
                    ResolveErrorEnum::VariableHaveBeenDeclared,
                    self.name.code_span()
                )
            );
        }
        return Ok(
            Statement(
                Rc::new(
                    VarDeclareStatement::new(
                        self.name.clone(),
                        initializer,
                        self.code_span.clone(),
                    )
                )
            )
        );
    }
}

#[macro_export]
macro_rules! var_declare_statement {
    ( $identifier:expr, $initializer:expr, $code_span:expr ) => {
        Statement(
            Rc::new(
                VarDeclareStatement::new(
                    $identifier,
                    $initializer,
                    $code_span,
                )
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
            statement::var_declare::VarDeclareStatement,
        },
        value::Value,
        environment::EnvironmentOps,
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
    fn test_print_vardeclare_statement() {
        let tests: Vec<(&str, &str)> = vec![
            ("var foo;", "var foo;"),
            ("var foo = true;", "var foo = true;"),
            ("var foo = 1 + 1;", "var foo = (+ 1 1);"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_statement::<VarDeclareStatement>(src).print(), expect);
        }
    }

    #[test]
    fn test_var_declare_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var bar;");
        ctx.resolve_context.begin();

        let var_declare_stmt = ctx.resolve_statement::<VarDeclareStatement>(
            parse_statement_unknown("var foo = bar;").as_ref()
        )
            .unwrap();
        let init_expr = var_declare_stmt.initializer().unwrap().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(init_expr.binding(), 1);
    }

    #[test]
    fn test_var_declare_statement_execute() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<VarDeclareStatement>("var foo;").as_ref()
            )
                .is_ok(),
            true,
        );
        assert_eq!(ctx.environment.has("foo", 0), true);
        assert_eq!(ctx.environment.get("foo", 0), Some(Value::Nil));
        assert_eq!(
            ctx.execute(
                parse_statement::<VarDeclareStatement>("var bar = 1 + 1;").as_ref()
            )
                .is_ok(),
            true,
        );
        assert_eq!(ctx.environment.has("bar", 0), true);
        assert_eq!(ctx.environment.get("bar", 0), Some(Value::Number(2.0)));
    }

    #[test]
    #[should_panic]
    fn test_var_declare_statement_duplicated_declare_panic() {
        let stmt = parse_statement::<VarDeclareStatement>("var foo;");
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(stmt.as_ref()).is_ok(),
            true
        );
        let _ = ctx.execute(stmt.as_ref());
    }

    #[test]
    fn test_var_declare_initializer_evaluate_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(parse_statement::<VarDeclareStatement>("var foo = true + 1;").as_ref()),
            Err(
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        new_code_span(0, 10, 0, 18),
                    ),
                    new_code_span(0, 0, 0, 19),
                )
            )
        );
    }

    #[test]
    fn test_var_declare_statement_resolve_initializer_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<VarDeclareStatement>("var foo = bar;").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 10, 0, 13)
            )
        );
    }

    #[test]
    fn test_var_declare_statement_resolve_has_been_declared_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<VarDeclareStatement>("var foo = true;").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableHaveBeenDeclared,
                new_code_span(0, 4, 0, 7)
            )
        );
    }
}
