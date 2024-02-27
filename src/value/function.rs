use core::sync::atomic;
use std::iter::zip;
use crate::parse::statement::Statement;
use crate::scan::token::identifier::IdentifierToken;
use crate::value::Value;
use crate::call::{
    Call,
    CallResult,
    CallError,
};
use crate::environment::{
    Environment,
    EnvironmentOps,
};
use crate::execute::ExecuteOk;

static FUNCTION_COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(0);

pub fn function_id() -> usize {
    FUNCTION_COUNTER.fetch_add(1, atomic::Ordering::Relaxed)
}

#[derive(Clone, Debug)]
pub struct Function {
    id: usize,
    #[allow(dead_code)]
    name: IdentifierToken,
    parameters: Vec<IdentifierToken>,
    body: Vec<Statement>,
    environment: Environment,
}

impl Function {
    pub fn new(
        id: usize,
        name: IdentifierToken,
        parameters: Vec<IdentifierToken>,
        body: Vec<Statement>,
        environment: Environment,
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
    pub fn parameters(&self) -> &Vec<IdentifierToken> {
        &self.parameters
    }

    #[cfg(test)]
    pub fn body(&self) -> &Vec<Statement> {
        &self.body
    }

    #[allow(dead_code)]
    pub fn environment(&self) -> &Environment {
        &self.environment
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
                    return Err(CallError::RuntimeError(err));
                }
            }
        }

        Ok(Value::Nil)
    }
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::value::Value;
    use crate::call::{
        Call,
        CallError,
    };
    use crate::environment::EnvironmentOps;
    use crate::error::{
        RuntimeError,
        RuntimeErrorEnum,
    };
    use crate::utils::test_utils::TestContext;
    use crate::runtime_error;

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
            Err(CallError::ArgumentNumberMismatch(2, 1)),
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
                CallError::RuntimeError(
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        new_code_span(2, 0, 2, 16),
                        runtime_error!(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            new_code_span(2, 7, 2, 15),
                        )
                    )
                )
            )
        );
    }
}
