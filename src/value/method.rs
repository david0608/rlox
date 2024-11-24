use std::{
    rc::Rc,
    cell::RefCell,
    iter::zip,
};
use crate::{
    parse::statement::{
        ExecuteOk,
        class_declare::MethodDefinition,
    },
    value::{
        Value,
        Call,
        object::Object,
    },
    environment::EnvironmentT,
    error::RuntimeErrorEnum,
};

#[derive(Debug)]
pub struct Method {
    definition: Rc<MethodDefinition>,
    this: Rc<RefCell<Object>>,
}

impl Method {
    pub fn new(
        definition: Rc<MethodDefinition>,
        this: Rc<RefCell<Object>>,
    ) -> Method
    {
        Method {
            definition,
            this,
        }
    }
}

impl Call for Method {
    fn call(&self, arguments: Vec<Value>) -> Result<Value, RuntimeErrorEnum> {
        let argn_expect = self.definition.parameters().len();
        let argn_found = arguments.len();
        if argn_expect != argn_found {
            return Err(RuntimeErrorEnum::ArgumentNumberMismatch(argn_expect, argn_found));
        }
        let env = self.this.borrow().class().environment().new_child();
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
                    return Err(RuntimeErrorEnum::RuntimeError(Box::new(err)));
                }
            }
        }
        return Ok(Value::Nil);
    }
}

impl std::cmp::PartialEq for Method {
    fn eq(&self, other: &Self) -> bool {
        self.this.borrow().class().id() == other.this.borrow().class().id()
        && self.definition.name().name() == other.definition.name().name()
    }
}
