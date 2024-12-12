use std::{
    rc::Rc,
    cell::RefCell,
    collections::{
        HashSet,
        HashMap,
    },
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
        }
    },
    scan::token::identifier::IdentifierToken,
    value::{
        Value,
        class::Class,
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
pub struct MethodDefinition {
    name: Rc<IdentifierToken>,
    parameters: Vec<Rc<IdentifierToken>>,
    body: Vec<Rc<dyn Statement>>,
    code_span: CodeSpan,
}

impl MethodDefinition {
    pub fn new(
        name: Rc<IdentifierToken>,
        parameters: Vec<Rc<IdentifierToken>>,
        body: Vec<Rc<dyn Statement>>,
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

    pub fn parameters(&self) -> &Vec<Rc<IdentifierToken>> {
        &self.parameters
    }

    pub fn body(&self) -> &Vec<Rc<dyn Statement>> {
        &self.body
    }

    pub fn code_span(&self) -> CodeSpan {
        self.code_span
    }

    pub fn resolve(&self, context: &mut Vec<HashSet<String>>) -> Result<MethodDefinition, ResolveError> {
        context.begin();
        context.declare("this").expect("declare this");
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
        let body = self.body.iter().map(|s| s.resolve(context)).collect::<Result<Vec<Rc<dyn Statement>>, ResolveError>>()?;
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
            self.body.iter().map(|s| s.to_string()).collect::<Vec<String>>().join(" "),
        )
    }
}

#[derive(Debug)]
pub struct ClassDeclareStatement {
    name: Rc<IdentifierToken>,
    super_class: Option<Rc<dyn Expression>>,
    method_definitions: Rc<HashMap<String, Rc<MethodDefinition>>>,
    code_span: CodeSpan,
}

impl ClassDeclareStatement {
    pub fn new(
        name: Rc<IdentifierToken>,
        super_class: Option<Rc<dyn Expression>>,
        method_definitions: Rc<HashMap<String, Rc<MethodDefinition>>>,
        code_span: CodeSpan,
    ) -> ClassDeclareStatement
    {
        ClassDeclareStatement {
            name,
            super_class,
            method_definitions,
            code_span,
        }
    }

    pub fn name(&self) -> &IdentifierToken {
        &self.name
    }

    pub fn super_class(&self) -> Option<Rc<dyn Expression>> {
        self.super_class.clone()
    }
}

impl Code for ClassDeclareStatement {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        let mut super_class = "".to_string();
        if let Some(expr) = self.super_class() {
            super_class = format!(" > {}", expr.to_string());
        }
        let mut methods = self.method_definitions.iter().collect::<Vec<(&String, &Rc<MethodDefinition>)>>();
        methods.sort_by_key(|m| m.0);
        format!(
            "class {}{} {{{}}}",
            self.name.name(),
            super_class,
            methods.iter().map(|m| format!("{}", m.1)).collect::<Vec<String>>().join(" "),
        )
    }
}

impl Statement for ClassDeclareStatement {
    fn resolve(&self, context: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Statement>, ResolveError> {
        let mut super_class: Option<Rc<dyn Expression>> = None;
        if let Some(super_class_expr) = self.super_class() {
            super_class = Some(super_class_expr.resolve(context)?);
        }
        if context.declare(self.name.name()).is_err() {
            return Err(
                ResolveError::new(
                    ResolveErrorEnum::VariableHaveBeenDeclared,
                    self.name.code_span().clone()
                )
            );
        }
        let mut method_defs = HashMap::new();
        for (key, method) in self.method_definitions.iter() {
            method_defs.insert(key.to_owned(), Rc::new(method.resolve(context)?));
        }
        return Ok(
            Rc::new(
                ClassDeclareStatement::new(
                    self.name.clone(),
                    super_class,
                    Rc::new(method_defs),
                    self.code_span,
                )
            )
        );
    }

    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        env.declare(
            self.name().name(),
            Value::Class(
                Rc::new(
                    Class::new(
                        self.name.clone(),
                        self.super_class(),
                        env.clone(),
                        self.method_definitions.clone(),
                    )
                )
            )
        )
            .expect("Variable name have been declared. This may not happen if the class declaration statement have been successfully resolved.");
        return Ok(ExecuteOk::KeepGoing);
    }
}

#[macro_export]
macro_rules! class_declare_statement {
    ( $identifier:expr, $super_class:expr, $method_definitions:expr, $code_span:expr ) => {
        Rc::new(
            ClassDeclareStatement::new(
                $identifier,
                $super_class,
                $method_definitions,
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
            expression::
                variable::VariableExpression,
            statement::{
                Statement,
                ExecuteOk,
                class_declare::ClassDeclareStatement,
                print::PrintStatement,
            },
        },
        value::Value,
        environment::EnvironmentT,
        error::{
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
            },
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
            stmt.to_string(),
            "class Foo {foo(a, b) {return (+ a b);}}"
        );
        let stmt = parse_statement::<ClassDeclareStatement>(
            "
            class Foo > Bar {
                foo() {
                    print \"foo\";
                }
            }
            "
        );
        assert_eq!(
            stmt.to_string(),
            "class Foo > Bar {foo() {print \"foo\";}}"
        );
    }

    #[test]
    fn test_class_declare_statement_execute() {
        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            class Bar {}
            "
        );
        assert_eq!(
            ctx.execute(
                parse_statement::<ClassDeclareStatement>(
                    "
                    class Foo > Bar {
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
            cls.super_class().as_ref().unwrap().evaluate(&ctx.environment).unwrap(),
            ctx.environment.get("Bar", 0).unwrap(),
        );
        assert_eq!(
            cls.name().name(),
            "Foo"
        );
        let m = cls.method_definition("foo").unwrap();
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
        ctx.execute_src(
            "
            class Bar {}
            "
        );
        let stmt = ctx.resolve_statement::<ClassDeclareStatement>(
            parse_statement_unknown(
                "
                class Foo > Bar {
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
        assert_eq!(ctx.resolve_context.find("Bar"), Some(0));
        assert_eq!(
            stmt.super_class().unwrap()
                .downcast_ref::<VariableExpression>().unwrap()
                .binding(),
            0
        );
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
    fn test_class_declare_statement_resolve_super_class_resolve_error() {
        let mut ctx = TestContext::new();
        let stmt = parse_statement::<ClassDeclareStatement>(
            "
            class Foo > Bar {}
            "
        );
        assert_eq!(
            ctx.resolve_statement_unknown(stmt.as_ref()).unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                CodeSpan::new(1, 12, 1, 15),
            )
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
                CodeSpan::new(0, 6, 0, 9)
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
                CodeSpan::new(3, 6, 3, 9)
            )
        );
    }
}
