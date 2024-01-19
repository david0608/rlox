use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use crate::value::Value;

#[derive(PartialEq, Debug)]
pub enum EnvironmentError {
    NotDeclared,
    MultipleDeclaration,
}

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_child(parent: &Rc<RefCell<Environment>>) -> Environment {
        Environment {
            values: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        }
    }
}

pub fn new_environment() -> Rc<RefCell<Environment>> {
    Rc::new(RefCell::new(Environment::new()))
}

pub fn new_child_environment(parent: &Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
    Rc::new(RefCell::new(Environment::new_child(parent)))
}

pub fn parent_environment(env: &Rc<RefCell<Environment>>) -> Option<Rc<RefCell<Environment>>> {
    if let Some(e) = env.borrow().parent.as_ref() {
        return Some(e.clone());
    }
    else {
        return None;
    }
}

pub fn environment_has_name(env: &Rc<RefCell<Environment>>, name: &str) -> bool {
    if env.borrow().values.contains_key(name) {
        return true;
    }
    else if let Some(penv) = parent_environment(env) {
        return environment_has_name(&penv, name);
    }
    else {
        return false;
    }
}

pub fn environment_get_value(env: &Rc<RefCell<Environment>>, name: &str) -> Result<Value, EnvironmentError> {
    if let Some(v) = env.borrow().values.get(name) {
        return Ok(v.clone());
    }
    else if let Some(penv) = parent_environment(env) {
        return environment_get_value(&penv, name);
    }
    else {
        return Err(EnvironmentError::NotDeclared);
    }
}

pub fn environment_set_value(env: &Rc<RefCell<Environment>>, name: &str, value: Value) -> Result<(), EnvironmentError> {
    if env.borrow().values.contains_key(name) {
        env.borrow_mut().values.insert(name.to_owned(), value);
        return Ok(());
    }
    else if let Some(penv) = parent_environment(env) {
        return environment_set_value(&penv, name, value);
    }
    else {
        return Err(EnvironmentError::NotDeclared);
    }
}

pub fn environment_declare(env: &Rc<RefCell<Environment>>, name: &str, value: Value) -> Result<(), EnvironmentError> {
    if env.borrow().values.contains_key(name) {
        return Err(EnvironmentError::MultipleDeclaration);
    }
    else {
        env.borrow_mut().values.insert(name.to_owned(), value);
        return Ok(());
    }
}

#[cfg(test)]
mod tests {
    use crate::value::Value;
    use crate::environment::{
        EnvironmentError,
        new_environment,
        new_child_environment,
        environment_has_name,
        environment_get_value,
        environment_set_value,
        environment_declare,
    };

    #[test]
    fn test_environment_has_name() {
        let penv = new_environment();
        environment_declare(&penv, "foo", Value::Bool(true)).expect("declare foo.");
        let cenv = new_child_environment(&penv);
        environment_declare(&cenv, "bar", Value::Bool(false)).expect("declare bar.");
        assert_eq!(environment_has_name(&penv, "foo"), true);
        assert_eq!(environment_has_name(&penv, "bar"), false);
        assert_eq!(environment_has_name(&cenv, "foo"), true);
        assert_eq!(environment_has_name(&cenv, "bar"), true);
    }

    #[test]
    fn test_environment_get_value() {
        let penv = new_environment();
        environment_declare(&penv, "foo", Value::Bool(true)).expect("declare foo.");
        let cenv = new_child_environment(&penv);
        environment_declare(&cenv, "bar", Value::Bool(false)).expect("declare bar.");
        assert_eq!(environment_get_value(&penv, "foo"), Ok(Value::Bool(true)));
        assert_eq!(environment_get_value(&penv, "bar"), Err(EnvironmentError::NotDeclared));
        assert_eq!(environment_get_value(&cenv, "foo"), Ok(Value::Bool(true)));
        assert_eq!(environment_get_value(&cenv, "bar"), Ok(Value::Bool(false)));
    }

    #[test]
    fn test_environment_set_value() {
        let penv = new_environment();
        environment_declare(&penv, "foo", Value::Number(0.0)).expect("declare foo.");
        let cenv = new_child_environment(&penv);
        environment_declare(&cenv, "bar", Value::Number(0.0)).expect("declare bar.");
        assert_eq!(environment_set_value(&penv, "foo", Value::Number(1.0)), Ok(()));
        assert_eq!(environment_get_value(&penv, "foo"), Ok(Value::Number(1.0)));
        assert_eq!(environment_set_value(&penv, "bar", Value::Number(1.0)), Err(EnvironmentError::NotDeclared));
        assert_eq!(environment_set_value(&cenv, "foo", Value::Number(2.0)), Ok(()));
        assert_eq!(environment_get_value(&cenv, "foo"), Ok(Value::Number(2.0)));
        assert_eq!(environment_set_value(&cenv, "bar", Value::Number(2.0)), Ok(()));
        assert_eq!(environment_get_value(&cenv, "bar"), Ok(Value::Number(2.0)));
    }

    #[test]
    fn test_declare() {
        let env = new_environment();
        environment_declare(&env, "foo", Value::Number(123.0)).expect("declare foo");
        assert_eq!(environment_get_value(&env, "foo"), Ok(Value::Number(123.0)));
        assert_eq!(environment_declare(&env, "foo", Value::Number(0.0)), Err(EnvironmentError::MultipleDeclaration));
    }

    #[test]
    fn test_shallow() {
        let penv = new_environment();
        environment_declare(&penv, "foo", Value::Bool(true)).expect("declare foo");
        let cenv = new_child_environment(&penv);
        environment_declare(&cenv, "foo", Value::Bool(false)).expect("declare bar");
        assert_eq!(environment_get_value(&penv, "foo"), Ok(Value::Bool(true)));
        assert_eq!(environment_get_value(&cenv, "foo"), Ok(Value::Bool(false)));
    }
}
