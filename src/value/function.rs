use core::sync::atomic;
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::zip;
use crate::parse::statement::BoxedStatement;
use crate::scan::token::identifier::IdentifierToken;
use crate::value::Value;
use crate::call::{
    Call,
    CallResult,
    CallError,
};
use crate::environment::{
    Environment,
    new_child_environment,
    environment_declare,
};
use crate::execute::ExecuteOk;

static FUNCTION_COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(0);

pub fn function_id() -> usize {
    FUNCTION_COUNTER.fetch_add(1, atomic::Ordering::Relaxed)
}

#[derive(Clone, Debug)]
pub struct Function {
    id: usize,
    name: IdentifierToken,
    parameters: Vec<IdentifierToken>,
    body: Vec<BoxedStatement>,
    environment: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(
        id: usize,
        name: IdentifierToken,
        parameters: Vec<IdentifierToken>,
        body: Vec<BoxedStatement>,
        environment: &Rc<RefCell<Environment>>,
    ) -> Function
    {
        Function {
            id,
            name,
            parameters,
            body,
            environment: Rc::clone(environment),
        }
    }

    pub fn id(&self) -> usize {
        return self.id;
    }

    pub fn name(&self) -> &str {
        return self.name.name();
    }

    pub fn parameters(&self) -> &Vec<IdentifierToken> {
        return &self.parameters;
    }

    pub fn body(&self) -> &Vec<BoxedStatement> {
        return &self.body;
    }

    pub fn environment(&self) -> &Rc<RefCell<Environment>> {
        return &self.environment;
    }
}

impl std::cmp::PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Call for Function {
    fn call(&self, arguments: Vec<Value>) -> CallResult {
        let argn_expect = self.parameters.len();
        let argn_found = arguments.len();
        if argn_expect != argn_found {
            return Err(CallError::ArgumentNumberMismatch(argn_expect, argn_found));
        }
        let env = new_child_environment(self.environment());
        for (p, v) in zip(&self.parameters, arguments) {
            if environment_declare(&env, p.name(), v).is_err() {
                unreachable!();
            }
        }

        for stmt in &self.body {
            match stmt.execute(&env) {
                Ok(ExecuteOk::KeepGoing) => {
                    continue;
                }
                Ok(ExecuteOk::Break) => {
                    unreachable!();
                }
                Ok(ExecuteOk::Return(v)) => {
                    return Ok(v);
                }
                Err(err) => {
                    return Err(CallError::RuntimeError(err));
                }
            }
        }

        Ok(Value::Nil)
    }
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::CodeSpan;
    use crate::code::code_point::CodePoint;
    use crate::parse::Parse;
    use crate::scan::Scan;
    use crate::value::Value;
    use crate::call::{
        Call,
        CallError,
    };
    use crate::environment::{
        new_environment,
        environment_get_value,
    };
    use crate::error::{
        RuntimeError,
        RuntimeErrorEnum,
    };
    use crate::execute::ExecuteOk;
    use crate::runtime_error;

    #[test]
    fn test_function_call() {
        let (tokens, errors) =
            "
            fun foo(a, b) {
                return a + b;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        assert_eq!(stmts.len(), 1);
        let env = new_environment();
        stmts[0].execute(&env).expect("function declare");
        let f = environment_get_value(&env, "foo").unwrap();
        assert_eq!(
            f.call(vec![Value::Number(1.0), Value::Number(2.0)]),
            Ok(Value::Number(3.0)),
        );
    }

    #[test]
    fn test_function_call_argument_number_mismatch_error() {
        let (tokens, errors) =
            "
            fun foo(a, b) {
                return a + b;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        assert_eq!(stmts.len(), 1);
        let env = new_environment();
        stmts[0].execute(&env).expect("function declare");
        let f = environment_get_value(&env, "foo").unwrap();
        assert_eq!(
            f.call(vec![Value::Number(1.0)]),
            Err(CallError::ArgumentNumberMismatch(2, 1)),
        );
    }

    #[test]
    fn test_function_call_execute_error() {
        let (tokens, errors) =
            "
            fun foo() {
                return bar;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        assert_eq!(stmts.len(), 1);
        let env = new_environment();
        stmts[0].execute(&env).expect("function declare");
        let f = environment_get_value(&env, "foo").unwrap();
        assert_eq!(
            f.call(vec![]),
            Err(
                CallError::RuntimeError(
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        CodeSpan::new(
                            CodePoint::new(2, 0),
                            CodePoint::new(2, 11),
                        ),
                        runtime_error!(
                            RuntimeErrorEnum::VariableNotDeclared,
                            CodeSpan::new(
                                CodePoint::new(2, 7),
                                CodePoint::new(2, 10),
                            ),
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn test_function_call_scope_isolation() {
        let (tokens, errors) =
            "
            fun foo() {
                return bar;
            }
            {
                var bar = true;
                print foo();
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        assert_eq!(stmts.len(), 2);
        let env = new_environment();
        assert_eq!(
            stmts[0].execute(&env),
            Ok(ExecuteOk::KeepGoing),
        );
        assert_eq!(
            stmts[1].execute(&env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    CodeSpan::new(
                        CodePoint::new(6, 0),
                        CodePoint::new(6, 12),
                    ),
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        CodeSpan::new(
                            CodePoint::new(6, 6),
                            CodePoint::new(6, 11),
                        ),
                        runtime_error!(
                            RuntimeErrorEnum::RuntimeError,
                            CodeSpan::new(
                                CodePoint::new(2, 0),
                                CodePoint::new(2, 11),
                            ),
                            runtime_error!(
                                RuntimeErrorEnum::VariableNotDeclared,
                                CodeSpan::new(
                                    CodePoint::new(2, 7),
                                    CodePoint::new(2, 10),
                                ),
                            )
                        )
                    )
                )
            )
        );
    }
}
