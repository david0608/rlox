use std::{
    rc::Rc,
    cell::RefCell,
    collections::HashMap,
};
use crate::value::Value;

pub struct Environment {
    values: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Environment>>>,
}

pub trait EnvironmentT {
    fn new() -> Rc<RefCell<Environment>>;

    fn new_child(&self) -> Rc<RefCell<Environment>>;

    fn parent(&self) -> Option<Rc<RefCell<Environment>>>;

    fn has(&self, name: &str, depth: usize) -> bool;

    fn get(&self, name: &str, depth: usize) -> Option<Value>;

    fn set(&self, name: &str, depth: usize, value: Value) -> Result<(), ()>;

    fn declare(&self, name: &str, value: Value) -> Result<(), ()>;
}

impl EnvironmentT for Rc<RefCell<Environment>> {
    fn new() -> Rc<RefCell<Environment>> {
        Rc::new(
            RefCell::new(
                Environment {
                    values: HashMap::new(),
                    parent: None,
                }
            )
        )
    }

    fn new_child(&self) -> Rc<RefCell<Environment>> {
        Rc::new(
            RefCell::new(
                Environment {
                    values: HashMap::new(),
                    parent: Some(self.clone()),
                }
            )
        )
    }

    fn parent(&self) -> Option<Rc<RefCell<Environment>>> {
        if let Some(e) = self.borrow().parent.as_ref() {
            return Some(e.clone());
        }
        else {
            return None;
        }
    }

    fn has(&self, name: &str, depth: usize) -> bool {
        if depth > 0 {
            if let Some(e) = self.parent() {
                return e.has(name, depth - 1);
            }
            else {
                return false;
            }
        }
        else {
            return self.borrow().values.contains_key(name);
        }
    }

    fn get(&self, name: &str, depth: usize) -> Option<Value> {
        if depth > 0 {
            if let Some(e) = self.parent() {
                return e.get(name, depth - 1);
            }
            else {
                return None;
            }
        }
        else {
            if let Some(v) = self.borrow().values.get(name) {
                return Some(v.clone());
            }
            else {
                return None;
            }
        }
    }

    fn set(&self, name: &str, depth: usize, value: Value) -> Result<(), ()> {
        if depth > 0 {
            if let Some(e) = self.parent() {
                return e.set(name, depth - 1, value);
            }
            else {
                return Err(());
            }
        }
        else {
            if self.has(name, 0) {
                self.borrow_mut().values.insert(name.to_owned(), value);
                return Ok(());
            }
            else {
                return Err(());
            }
        }
    }

    fn declare(&self, name: &str, value: Value) -> Result<(), ()> {
        if self.has(name, 0) {
            return Err(());
        }
        else {
            self.borrow_mut().values.insert(name.to_owned(), value);
            return Ok(());
        }
    }
}

impl std::fmt::Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Omitted struct Environment>")
    }
}

#[cfg(test)]
mod tests {
    use std::{
        rc::Rc,
        cell::RefCell,
    };
    use crate::{
        value::Value,
        environment::{
            Environment,
            EnvironmentT,
        }
    };

    #[test]
    fn test_environment_has() {
        let penv = <Rc<RefCell<Environment>> as EnvironmentT>::new();
        penv.declare("foo", Value::Bool(true)).expect("declare foo.");
        let cenv = penv.new_child();
        cenv.declare("bar", Value::Bool(false)).expect("declare foo.");
        assert_eq!(penv.has("foo", 0), true);
        assert_eq!(penv.has("foo", 1), false);
        assert_eq!(penv.has("bar", 0), false);
        assert_eq!(penv.has("bar", 1), false);
        assert_eq!(cenv.has("foo", 0), false);
        assert_eq!(cenv.has("foo", 1), true);
        assert_eq!(cenv.has("bar", 0), true);
        assert_eq!(cenv.has("bar", 1), false);
    }

    #[test]
    fn test_environment_get() {
        let penv = <Rc<RefCell<Environment>> as EnvironmentT>::new();
        penv.declare("foo", Value::Bool(true)).expect("declare foo.");
        let cenv = penv.new_child();
        cenv.declare("bar", Value::Bool(false)).expect("declare foo.");
        assert_eq!(penv.get("foo", 0), Some(Value::Bool(true)));
        assert_eq!(penv.get("foo", 1), None);
        assert_eq!(penv.get("bar", 0), None);
        assert_eq!(penv.get("bar", 1), None);
        assert_eq!(cenv.get("foo", 0), None);
        assert_eq!(cenv.get("foo", 1), Some(Value::Bool(true)));
        assert_eq!(cenv.get("bar", 0), Some(Value::Bool(false)));
        assert_eq!(cenv.get("bar", 1), None);
    }

    #[test]
    fn test_environment_set() {
        let penv = <Rc<RefCell<Environment>> as EnvironmentT>::new();
        penv.declare("foo", Value::Number(0.0)).expect("declare foo.");
        let cenv = penv.new_child();
        cenv.declare("bar", Value::Number(1.0)).expect("declare foo.");
        assert_eq!(penv.set("foo", 0, Value::Number(1.0)), Ok(()));
        assert_eq!(penv.get("foo", 0), Some(Value::Number(1.0)));
        assert_eq!(penv.set("foo", 1, Value::Number(1.0)), Err(()));
        assert_eq!(penv.set("bar", 0, Value::Number(1.0)), Err(()));
        assert_eq!(penv.set("bar", 1, Value::Number(1.0)), Err(()));
        assert_eq!(cenv.set("foo", 0, Value::Number(2.0)), Err(()));
        assert_eq!(cenv.set("foo", 1, Value::Number(2.0)), Ok(()));
        assert_eq!(cenv.get("foo", 1), Some(Value::Number(2.0)));
        assert_eq!(cenv.set("bar", 0, Value::Number(2.0)), Ok(()));
        assert_eq!(cenv.get("bar", 0), Some(Value::Number(2.0)));
        assert_eq!(cenv.set("bar", 1, Value::Number(2.0)), Err(()));
    }

    #[test]
    fn test_declare() {
        let penv = <Rc<RefCell<Environment>> as EnvironmentT>::new();
        assert_eq!(penv.declare("foo", Value::Bool(true)), Ok(()));
        assert_eq!(penv.declare("foo", Value::Bool(true)), Err(()));
        let cenv = penv.new_child();
        assert_eq!(cenv.declare("foo", Value::Bool(false)), Ok(()));
        assert_eq!(cenv.declare("foo", Value::Bool(false)), Err(()));
    }
}
