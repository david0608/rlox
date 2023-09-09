use core::sync::atomic;
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::zip;
use super::call::{
    Call,
    CallResult,
    CallError,
};
use super::value::Value;
use crate::parse::statement::BoxedStatement;
use crate::scan::token::identifier::IdentifierToken;
use crate::execute::ExecuteOk;
use crate::scope::Scope;

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
}

impl std::cmp::PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Call for Function {
    fn call(&self, scope: &Rc<RefCell<Scope>>, arguments: Vec<Value>) -> CallResult {
        let argn_expect = self.parameters.len();
        let argn_found = arguments.len();
        if argn_expect != argn_found {
            return Err(CallError::ArgumentNumberMismatch(argn_expect, argn_found));
        }
        let fscope = Scope::new_isolate_child(scope).as_rc();
        for (p, v) in zip(&self.parameters, arguments) {
            if fscope.borrow_mut().declare(p.name(), v).is_err() {
                unreachable!();
            }
        }

        for stmt in &self.body {
            match stmt.execute(scope) {
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
                    return Err(CallError::ExecuteError(err));
                }
            }
        }

        Ok(Value::Nil)
    }
}
