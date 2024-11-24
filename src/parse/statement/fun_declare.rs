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
    parse::statement::{
        Statement,
        ExecuteOk,
    },
    scan::token::identifier::IdentifierToken,
    value::{
        Value,
        function::{
            Function,
            function_id,
        },
    },
    environment::{
        Environment,
        EnvironmentT,
    },
    error::{
        RuntimeError,
        ResolveError,
        ResolveErrorEnum,
    },
    resolve_context::ResolveContext,
};

#[derive(Debug)]
pub struct FunDeclareStatement {
    name: Rc<IdentifierToken>,
    parameters: Vec<Rc<IdentifierToken>>,
    body: Vec<Rc<dyn Statement>>,
    code_span: CodeSpan,
}

impl FunDeclareStatement {
    pub fn new(
        name: Rc<IdentifierToken>,
        parameters: Vec<Rc<IdentifierToken>>,
        body: Vec<Rc<dyn Statement>>,
        code_span: CodeSpan,
    ) -> FunDeclareStatement
    {
        FunDeclareStatement {
            name,
            parameters,
            body,
            code_span,
        }
    }

    pub fn name(&self) -> &Rc<IdentifierToken> {
        &self.name
    }

    pub fn parameters(&self) -> &Vec<Rc<IdentifierToken>> {
        &self.parameters
    }

    pub fn body(&self) -> &Vec<Rc<dyn Statement>> {
        &self.body
    }
}

impl Code for FunDeclareStatement {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        return format!(
            "fun {}({}) {{{}}}",
            self.name().name(),
            self.parameters().iter().map(|i| i.name().to_owned()).collect::<Vec<String>>().join(", "),
            self.body().iter().map(|s| s.to_string()).collect::<Vec<String>>().join(" "),
        );
    }
}

impl Statement for FunDeclareStatement {
    fn resolve(&self, context: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Statement>, ResolveError> {
        if context.declare(self.name.name()).is_err() {
            return Err(
                ResolveError::new(
                    ResolveErrorEnum::VariableHaveBeenDeclared,
                    self.name.code_span().clone()
                )
            );
        }
        context.begin();
        for p in self.parameters.iter() {
            if context.declare(p.name()).is_err() {
                context.end();
                return Err(
                    ResolveError::new(
                        ResolveErrorEnum::VariableHaveBeenDeclared,
                        p.code_span().clone()
                    )
                );
            }
        }
        let body = self.body.iter().map(|s| s.resolve(context)).collect();
        context.end();
        match body {
            Ok(body) => {
                return Ok(
                    Rc::new(
                        FunDeclareStatement::new(
                            self.name.clone(),
                            self.parameters.clone(),
                            body,
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

    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        env.declare(
            self.name().name(),
            Value::Function(
                Function::new(
                    function_id(),
                    self.name().clone(),
                    self.parameters().clone(),
                    self.body().clone(),
                    env.clone(),
                )
            )
        )
            .expect("Variable name have been declared. This may not happen if the function declaration statement have been successfully resolved.");
        return Ok(ExecuteOk::KeepGoing);
    }
}

#[macro_export]
macro_rules! fun_declare_statement {
    ( $identifier:expr, $parameters:expr, $body:expr, $code_span:expr ) => {
        Rc::new(
            FunDeclareStatement::new(
                $identifier,
                $parameters,
                $body,
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
            statement::{
                Statement,
                ExecuteOk,
                fun_declare::FunDeclareStatement,
                print::PrintStatement,
            },
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
            },
        },
    };

    #[test]
    fn test_fun_declare_statement_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("fun foo() {print \"hello\";}", "fun foo() {print \"hello\";}"),
            ("fun bar(a, b) {var c = a + b; print c;}", "fun bar(a, b) {var c = (+ a b); print c;}"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_statement::<FunDeclareStatement>(src).to_string(), expect);
        }
    }

    #[test]
    fn test_fun_declare_statement_execute() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<FunDeclareStatement>(
                    "
                    fun foo(a, b) {
                        var c = a + b;
                        print c;
                    }
                    "
                )
                    .as_ref(),
            ),
            Ok(ExecuteOk::KeepGoing),
        );

        let f = if let Value::Function(f) = ctx.environment.get("foo", 0).unwrap() {
            f
        }
        else {
            panic!("Value should be function.");
        };
        assert_eq!(f.name(), "foo");
        let params = f.parameters();
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name(), "a");
        assert_eq!(params[1].name(), "b");
        let body = f.body();
        assert_eq!(body.len(), 2);
        assert_eq!(body[0].to_string(), "var c = (+ a b);");
        assert_eq!(body[1].to_string(), "print c;");
    }

    #[test]
    #[should_panic]
    fn test_fun_declare_statement_execute_duplicated_declaration_panic() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = true;");
        let stmt = parse_statement::<FunDeclareStatement>(
            "
            fun foo(a, b) {
                print \"foo\";
            }
            "
        );
        let _ = stmt.execute(&ctx.environment);
    }

    #[test]
    fn test_fun_declare_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var i = 0;");

        let fun_declare_stmt = ctx.resolve_statement::<FunDeclareStatement>(
            parse_statement_unknown(
                "
                fun test(a) {
                    print a;
                    print i;
                }
                "
                )
                .as_ref()
        )
            .unwrap();

        let print_stmt = fun_declare_stmt.body[0].downcast_ref::<PrintStatement>().unwrap();
        let var_expr = print_stmt.value().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 0);

        let print_stmt = fun_declare_stmt.body()[1].downcast_ref::<PrintStatement>().unwrap();
        let var_expr = print_stmt.value().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 1);
    }

    #[test]
    fn test_fun_declare_statement_resolve_has_been_declared_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var test;");
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<FunDeclareStatement>("fun test() {}").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableHaveBeenDeclared,
                CodeSpan::new(0, 4, 0, 8)
            )
        );
    }

    #[test]
    fn test_fun_declare_statement_resolve_body_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<FunDeclareStatement>("fun test(a) { var a; }").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableHaveBeenDeclared,
                CodeSpan::new(0, 18, 0, 19)
            )
        );
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<FunDeclareStatement>("fun test() { var a = b; }").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                CodeSpan::new(0, 21, 0, 22)
            )
        );
    }
}
