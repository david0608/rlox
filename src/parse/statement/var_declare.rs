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
    scan::token::{
        identifier::IdentifierToken,
        simple::VAR_LEXEME,
    },
    value::Value,
    environment::{
        Environment,
        EnvironmentT,
    },
    error::{
        ResolveError,
        RuntimeError,
        ResolveErrorEnum,
    },
    resolve_context::ResolveContext,
};

#[derive(Debug)]
pub struct VarDeclareStatement {
    name: Rc<IdentifierToken>,
    initializer: Option<Rc<dyn Expression>>,
    code_span: CodeSpan,
}

impl VarDeclareStatement {
    pub fn new(
        name: Rc<IdentifierToken>,
        initializer: Option<Rc<dyn Expression>>,
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

    pub fn initializer(&self) -> Option<&Rc<dyn Expression>> {
        self.initializer.as_ref()
    }
}

impl Code for VarDeclareStatement {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        if let Some(i) = self.initializer() {
            format!("{} {} = {};", VAR_LEXEME, self.name().name(), i.to_string())
        }
        else {
            format!("{} {};", VAR_LEXEME, self.name().name())
        }
    }
}

impl Statement for VarDeclareStatement {
    fn resolve(&self, context: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Statement>, ResolveError> {
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
                    self.name.code_span().clone()
                )
            );
        }
        return Ok(
            Rc::new(
                VarDeclareStatement::new(
                    self.name.clone(),
                    initializer,
                    self.code_span.clone(),
                )
            )
        );
    }

    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        let mut value = Value::Nil;
        if let Some(i) = self.initializer() {
            match i.evaluate(env) {
                Ok(v) => value = v,
                Err(e) => {
                    return Err(RuntimeError::wrap(e, self.code_span().clone()));
                }
            }
        };
        env.declare(self.name().name(), value)
            .expect("Variable name have been declared. This may not happen if the variable declaration statement have been successfully resolved.");
        return Ok(ExecuteOk::KeepGoing);
    }
}

#[macro_export]
macro_rules! var_declare_statement {
    ( $identifier:expr, $initializer:expr, $code_span:expr ) => {
        Rc::new(
            VarDeclareStatement::new(
                $identifier,
                $initializer,
                $code_span,
            )
        )
    };
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
            statement::var_declare::VarDeclareStatement,
        },
        value::Value,
        environment::EnvironmentT,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
            ResolveError,
            ResolveErrorEnum,
        },
        resolve_context::ResolveContext,
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
    fn test_print_vardeclare_statement() {
        let tests: Vec<(&str, &str)> = vec![
            ("var foo;", "var foo;"),
            ("var foo = true;", "var foo = true;"),
            ("var foo = 1 + 1;", "var foo = (+ 1 1);"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_statement::<VarDeclareStatement>(src).to_string(), expect);
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
                        CodeSpan::new(0, 10, 0, 18),
                    ),
                    CodeSpan::new(0, 0, 0, 19),
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
                CodeSpan::new(0, 10, 0, 13)
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
                CodeSpan::new(0, 4, 0, 7)
            )
        );
    }
}
