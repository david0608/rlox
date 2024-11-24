use core::sync::atomic;
use std::{
    iter::zip,
    rc::Rc,
    cell::RefCell,
};
use crate::{
    parse::statement::{
        Statement,
        ExecuteOk,
    },
    scan::token::identifier::IdentifierToken,
    value::{
        Value,
        Call,
    },
    environment::{
        Environment,
        EnvironmentT,
    },
    error::RuntimeErrorEnum,
};

static FUNCTION_COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(0);

pub fn function_id() -> usize {
    FUNCTION_COUNTER.fetch_add(1, atomic::Ordering::Relaxed)
}

#[derive(Clone, Debug)]
pub struct Function {
    id: usize,
    #[allow(dead_code)]
    name: Rc<IdentifierToken>,
    parameters: Vec<Rc<IdentifierToken>>,
    body: Vec<Rc<dyn Statement>>,
    environment: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(
        id: usize,
        name: Rc<IdentifierToken>,
        parameters: Vec<Rc<IdentifierToken>>,
        body: Vec<Rc<dyn Statement>>,
        environment: Rc<RefCell<Environment>>,
    ) -> Function
    {
        Function {
            id,
            name,
            parameters,
            body,
            environment,
        }
    }

    #[allow(dead_code)]
    pub fn id(&self) -> usize {
        self.id
    }

    #[cfg(test)]
    pub fn name(&self) -> &str {
        self.name.name()
    }

    #[cfg(test)]
    pub fn parameters(&self) -> &Vec<Rc<IdentifierToken>> {
        &self.parameters
    }

    #[cfg(test)]
    pub fn body(&self) -> &Vec<Rc<dyn Statement>> {
        &self.body
    }

    #[allow(dead_code)]
    pub fn environment(&self) -> &Rc<RefCell<Environment>> {
        &self.environment
    }
}

impl Call for Function {
    fn call(&self, arguments: Vec<Value>) -> Result<Value, RuntimeErrorEnum> {
        let argn_expect = self.parameters.len();
        let argn_found = arguments.len();
        if argn_expect != argn_found {
            return Err(RuntimeErrorEnum::ArgumentNumberMismatch(argn_expect, argn_found));
        }
        let env = self.environment.new_child();
        for (p, v) in zip(&self.parameters, arguments) {
            if env.declare(p.name(), v).is_err() {
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
                    return Err(RuntimeErrorEnum::RuntimeError(Box::new(err)));
                }
            }
        }

        Ok(Value::Nil)
    }
}

impl std::cmp::PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        code::CodeSpan,
        value::{
            Value,
            Call,
        },
        environment::EnvironmentT,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
        utils::test_utils::TestContext
    };

    #[test]
    fn test_function_call() {
        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            fun foo(a, b) {
                return a + b;
            }
            "
        );
        let f = ctx.environment.get("foo", 0).unwrap();
        assert_eq!(
            f.call(vec![Value::Number(1.0), Value::Number(2.0)]),
            Ok(Value::Number(3.0))
        );
    }

    #[test]
    fn test_function_call_argument_number_mismatch_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            fun foo(a, b) {
                return a + b;
            }
            "
        );
        let f = ctx.environment.get("foo", 0).unwrap();
        assert_eq!(
            f.call(vec![Value::Number(1.0)]),
            Err(RuntimeErrorEnum::ArgumentNumberMismatch(2, 1)),
        );
    }

    #[test]
    fn test_function_call_execute_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            fun foo() {
                return true + 1;
            }
            "
        );
        let f = ctx.environment.get("foo", 0).unwrap();
        assert_eq!(
            f.call(vec![]),
            Err(
                RuntimeErrorEnum::RuntimeError(
                    Box::new(
                        RuntimeError::wrap(
                            RuntimeError::new(
                                RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                                CodeSpan::new(2, 7, 2, 15),
                            ),
                            CodeSpan::new(2, 0, 2, 16),
                        )
                    )
                )
            )
        );
    }
}
