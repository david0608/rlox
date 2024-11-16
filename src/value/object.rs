use std::{
    rc::Rc,
    collections::HashMap,
};
use crate::value::{
    Value,
    class::Class,
};

#[derive(Debug)]
pub struct Object {
    class: Rc<Class>,
    properties: HashMap<String, Value>,
}

impl Object {
    pub fn new(class: Rc<Class>) -> Object {
        Object {
            class,
            properties: HashMap::new(),
        }
    }

    pub fn class(&self) -> &Class {
        &self.class
    }

    pub fn properties(&self) -> &HashMap<String, Value> {
        &self.properties
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.properties.insert(name, value.clone());
    }
}

impl std::cmp::PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.properties == other.properties
    }
}
