use std::{
    rc::Rc,
    collections::HashMap,
};
use core::sync::atomic;
use crate::parse::statement::class_declare::MethodDefinition;
use crate::scan::token::identifier::IdentifierToken;
use crate::value::{
    Value,
    object::Object,
};
use crate::call::{
    Call,
    CallResult,
};
use crate::environment::Environment;

static CLASS_COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(0);

pub fn class_id() -> usize {
    CLASS_COUNTER.fetch_add(1, atomic::Ordering::Relaxed)
}

#[derive(Clone, Debug)]
pub struct Class {
    id: usize,
    name: IdentifierToken,
    environment: Environment,
    method_definitions: Rc<HashMap<String, Rc<MethodDefinition>>>,
}

impl Class {
    pub fn new(
        name: IdentifierToken,
        environment: Environment,
        method_definitions: Rc<HashMap<String, Rc<MethodDefinition>>>,
    )
        -> Class
    {
        Class {
            id: class_id(),
            name,
            environment,
            method_definitions,
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn environment(&self) -> &Environment {
        &self.environment
    }

    pub fn method_definitions(&self) -> Rc<HashMap<String, Rc<MethodDefinition>>> {
        self.method_definitions.clone()
    }

}

impl std::cmp::PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Call for Class {
    fn call(&self, _: Vec<Value>) -> CallResult {
        Ok(
            Value::Object(
                Object::new(
                    self.clone(),
                )
            )
        )
    }
}