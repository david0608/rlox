use std::{
    rc::Rc,
    cell::RefCell,
    collections::HashMap,
};
use core::sync::atomic;
use crate::{
    parse::{
        expression::Expression,
        statement::class_declare::MethodDefinition,
    },
    scan::token::identifier::IdentifierToken,
    value::{
        Value,
        Call,
        object::Object,
    },
    environment::Environment,
    error::RuntimeErrorEnum,
};

static CLASS_COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(0);

pub fn class_id() -> usize {
    CLASS_COUNTER.fetch_add(1, atomic::Ordering::Relaxed)
}

#[derive(Debug)]
pub struct Class {
    id: usize,
    name: Rc<IdentifierToken>,
    super_class: Option<Rc<dyn Expression>>,
    environment: Rc<RefCell<Environment>>,
    method_definitions: Rc<HashMap<String, Rc<MethodDefinition>>>,
}

impl Class {
    pub fn new(
        name: Rc<IdentifierToken>,
        super_class: Option<Rc<dyn Expression>>,
        environment: Rc<RefCell<Environment>>,
        method_definitions: Rc<HashMap<String, Rc<MethodDefinition>>>,
    )
        -> Class
    {
        Class {
            id: class_id(),
            name,
            super_class,
            environment,
            method_definitions,
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn name(&self) -> &IdentifierToken {
        &self.name
    }

    pub fn super_class(&self) -> &Option<Rc<dyn Expression>> {
        &self.super_class
    }

    pub fn environment(&self) -> &Rc<RefCell<Environment>> {
        &self.environment
    }

    pub fn method_definitions(&self) -> &HashMap<String, Rc<MethodDefinition>> {
        &self.method_definitions
    }
}

impl Call for Rc<Class> {
    fn call(&self, _: Vec<Value>) -> Result<Value, RuntimeErrorEnum> {
        Ok(Value::Object(Rc::new(RefCell::new(Object::new(self.clone())))))
    }
}

impl std::cmp::PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
