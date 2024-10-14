use std::{
    rc::Rc,
    cell::RefCell,
    collections::HashMap,
};
use crate::value::{
    Value,
    class::Class,
    method::Method,
};

#[derive(Clone, Debug)]
pub struct Object {
    class: Class,
    properties: Rc<RefCell<HashMap<String, Value>>>,
}

impl Object {
    pub fn new(class: Class) -> Object {
        Object {
            class,
            properties: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn class(&self) -> &Class {
        &self.class
    }

    pub fn get(&self, name: &str) -> Value {
        if let Some(v) = self.properties.borrow().get(name) {
            return v.clone();
        }
        else if let Some(md) = self.class.method_definitions().get(name) {
            return Value::Method(
                Method::new(
                    md.clone(),
                    self.clone(),
                )
            );
        }
        else {
            return Value::Nil;
        }
    }

    pub fn set(&self, name: &str, value: Value) -> Value {
        self.properties.borrow_mut().insert(name.to_owned(), value.clone());
        value
    }
}

impl std::cmp::PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.properties == other.properties
    }
}
