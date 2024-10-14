use std::{
    rc::Rc,
    iter::zip,
};
use crate::{
    parse::statement::class_declare::MethodDefinition,
    value::{
        Value,
        object::Object,
    },
    call::{
        Call,
        CallError,
        CallResult,
    },
    environment::EnvironmentOps,
    execute::ExecuteOk,
};

#[derive(Clone, Debug)]
pub struct Method {
    definition: Rc<MethodDefinition>,
    this: Object,
}

impl Method {
    pub fn new(
        definition: Rc<MethodDefinition>,
        this: Object,
    ) -> Method
    {
        Method {
            definition,
            this,
        }
    }
}

impl std::cmp::PartialEq for Method {
    fn eq(&self, other: &Self) -> bool {
        self.this.class().id() == other.this.class().id()
        && self.definition.name().name() == other.definition.name().name()
    }
}

impl Call for Method {
    fn call(&self, arguments: Vec<Value>) -> CallResult {
        let argn_expect = self.definition.parameters().len();
        let argn_found = arguments.len();
        if argn_expect != argn_found {
            return Err(CallError::ArgumentNumberMismatch(argn_expect, argn_found));
        }
        let env = self.this.class().environment().new_child();
        if env.declare("this", Value::Object(self.this.clone())).is_err() {
            unreachable!();
        }
        for (p, v) in zip(self.definition.parameters(), arguments) {
            if env.declare(p.name(), v).is_err() {
                unreachable!();
            }
        }
        for stmt in self.definition.body() {
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
        return Ok(Value::Nil);
    }
}
