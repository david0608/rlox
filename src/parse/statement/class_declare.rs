use std::{
    rc::Rc,
    collections::HashMap,
};
use crate::{
    code::{
        Code,
        code_span::CodeSpan,
    },
    parse::statement::{
        Statement,
        AsStatement,
    },
    scan::token::identifier::IdentifierToken,
    value::{
        Value,
        class::Class,
    },
    environment::{
        Environment,
        EnvironmentOps,
    },
    execute::{
        Execute,
        ExecuteResult,
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

#[derive(Debug, Clone)]
pub struct MethodDefinition {
    name: IdentifierToken,
    parameters: Vec<IdentifierToken>,
    body: Vec<Statement>,
    code_span: CodeSpan,
}

impl MethodDefinition {
    pub fn new(
        name: IdentifierToken,
        parameters: Vec<IdentifierToken>,
        body: Vec<Statement>,
        code_span: CodeSpan,
    )
        -> MethodDefinition
    {
        MethodDefinition {
            name,
            parameters,
            body,
            code_span,
        }
    }

    pub fn name(&self) -> &IdentifierToken {
        &self.name
    }

    pub fn parameters(&self) -> &Vec<IdentifierToken> {
        &self.parameters
    }

    pub fn body(&self) -> &Vec<Statement> {
        &self.body
    }

    pub fn code_span(&self) -> CodeSpan {
        self.code_span
    }

    pub fn resolve(&self, context: &mut ResolveCtx) -> Result<MethodDefinition, ResolveError> {
        context.begin();
        context.declare("this").expect("declare this");
        for p in self.parameters.iter() {
            if context.declare(p.name()).is_err() {
                context.end();
                return Err(
                    ResolveError::new(
                        ResolveErrorEnum::VariableHaveBeenDeclared,
                        p.code_span()
                    )
                );
            }
        }
        let body = self.body.iter().map(|s| s.resolve(context)).collect::<Result<Vec<Statement>, ResolveError>>()?;
        context.end();
        return Ok(
            MethodDefinition::new(
                self.name.clone(),
                self.parameters.clone(),
                body,
                self.code_span,
            )
        );
    }
}

impl std::fmt::Display for MethodDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({}) {{{}}}",
            self.name.name(),
            self.parameters.iter().map(|p| p.name().to_owned()).collect::<Vec<String>>().join(", "),
            self.body.iter().map(|s| s.print()).collect::<Vec<String>>().join(" "),
        )
    }
}

pub struct ClassDeclareStatement {
    name: IdentifierToken,
    method_definitions: Rc<HashMap<String, Rc<MethodDefinition>>>,
    code_span: CodeSpan,
}

impl ClassDeclareStatement {
    pub fn new(
        name: IdentifierToken,
        method_definitions: Rc<HashMap<String, Rc<MethodDefinition>>>,
        code_span: CodeSpan,
    ) -> ClassDeclareStatement
    {
        ClassDeclareStatement {
            name,
            method_definitions,
            code_span,
        }
    }

    pub fn name(&self) -> &IdentifierToken {
        &self.name
    }
}

impl Code for ClassDeclareStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for ClassDeclareStatement {
    fn print(&self) -> String {
        let mut methods = self.method_definitions.iter().collect::<Vec<(&String, &Rc<MethodDefinition>)>>();
        methods.sort_by_key(|m| m.0);
        format!(
            "class {} {{{}}}",
            self.name.name(),
            methods.iter().map(|m| format!("{}", m.1)).collect::<Vec<String>>().join(" "),
        )
    }
}

impl_debug_for_printable!(ClassDeclareStatement);

impl Execute for ClassDeclareStatement {
    fn execute(&self, env: &Environment) -> ExecuteResult {
        env.declare(
            self.name().name(),
            Value::Class(
                Class::new(
                    self.name.clone(),
                    env.clone(),
                    self.method_definitions.clone(),
                )
            )
        )
            .expect("Variable name have been declared. This may not happen if the class declaration statement have been successfully resolved.");
        return Ok(ExecuteOk::KeepGoing);
    }
}

impl AsStatement for ClassDeclareStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        if context.declare(self.name.name()).is_err() {
            return Err(
                ResolveError::new(
                    ResolveErrorEnum::VariableHaveBeenDeclared,
                    self.name.code_span()
                )
            );
        }
        let mut method_defs = HashMap::new();
        for (key, method) in self.method_definitions.iter() {
            method_defs.insert(key.to_owned(), Rc::new(method.resolve(context)?));
        }
        return Ok(
            Statement(
                Rc::new(
                    ClassDeclareStatement::new(
                        self.name.clone(),
                        Rc::new(method_defs),
                        self.code_span,
                    )
                )
            )
        );
    }
}

#[macro_export]
macro_rules! class_declare_statement {
    ( $identifier:expr, $method_definitions:expr, $code_span:expr ) => {
        Statement(
            Rc::new(
                ClassDeclareStatement::new(
                    $identifier,
                    $method_definitions,
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
            expression::
                variable::VariableExpression,
            statement::{
                class_declare::ClassDeclareStatement,
                print::PrintStatement,
            },
        },
        value::Value,
        environment::EnvironmentOps,
        execute::{
            Execute,
            ExecuteOk,
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
        },
    };

    #[test]
    fn test_class_declare_statement_print() {
        let stmt = parse_statement::<ClassDeclareStatement>(
            "
            class Foo {
                foo(a, b) {
                    return a + b;
                }
            }
            "
        );
        assert_eq!(
            stmt.print(),
            "class Foo {foo(a, b) {return (+ a b);}}"
        );
    }

    #[test]
    fn test_class_declare_statement_execute() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<ClassDeclareStatement>(
                    "
                    class Foo {
                        foo(a) {
                            print a;
                        }
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing)
        );
        let cls = match ctx.environment.get("Foo", 0).unwrap() {
            Value::Class(cls) => cls,
            _ => panic!("Expect Class value.")
        };
        assert_eq!(
            cls.name().name(),
            "Foo"
        );
        let m  = cls.method_definitions().get("foo").unwrap();
        assert_eq!(
            m.name().name(),
            "foo"
        );
        assert_eq!(
            m.parameters()[0].name(),
            "a"
        );
        assert_eq!(
            m.body()[0].downcast_ref::<PrintStatement>().unwrap()
                .value().downcast_ref::<VariableExpression>().unwrap()
                .from().name(),
                "a",
        );
    }

    #[test]
    #[should_panic]
    fn test_class_declare_statement_execute_duplicated_declaration_panic() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var Foo;");
        let stmt = parse_statement::<ClassDeclareStatement>(
            "
            class Foo {
                foo(a) {
                    print a;
                }
            }
            "
        );
        let _ = stmt.execute(&ctx.environment);
    }

    #[test]
    fn test_class_declare_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var bar;");
        let stmt = ctx.resolve_statement::<ClassDeclareStatement>(
            parse_statement_unknown(
                "
                class Foo {
                    foo(a) {
                        print this;
                        print a;
                        print bar;
                    }
                }
                "
            )
                .as_ref()
        )
            .unwrap();
        assert_eq!(ctx.resolve_context.find("bar"), Some(0));
        assert_eq!(
            stmt.method_definitions.get("foo").unwrap()
                .body().get(0).unwrap()
                .downcast_ref::<PrintStatement>().unwrap()
                .value().downcast_ref::<VariableExpression>().unwrap()
                .binding(),
            0
        );
        assert_eq!(
            stmt.method_definitions.get("foo").unwrap()
                .body().get(1).unwrap()
                .downcast_ref::<PrintStatement>().unwrap()
                .value().downcast_ref::<VariableExpression>().unwrap()
                .binding(),
            0
        );
        assert_eq!(
            stmt.method_definitions.get("foo").unwrap()
                .body().get(2).unwrap()
                .downcast_ref::<PrintStatement>().unwrap()
                .value().downcast_ref::<VariableExpression>().unwrap()
                .binding(),
            1
        );
    }

    #[test]
    fn test_class_declare_statement_resolve_var_declared_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var Foo;");
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<ClassDeclareStatement>("class Foo {}").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableHaveBeenDeclared,
                new_code_span(0, 6, 0, 9)
            )
        );
    }

    #[test]
    fn test_class_declare_statement_resolve_method_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<ClassDeclareStatement>(
                    "
                    class Foo {
                        foo() {
                            print foo;
                        }
                    }
                    ",
                )
                    .as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(3, 6, 3, 9)
            )
        );
    }
}
