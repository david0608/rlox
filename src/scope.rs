use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use crate::value::Value;

#[derive(PartialEq, Debug)]
pub enum Error<'src> {
    NotDeclared(&'src str),
    MultipleDeclaration(&'src str)
}

#[derive(Debug)]
pub struct Scope {
    values: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            values: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_child(parent: &Rc<RefCell<Scope>>) -> Scope {
        Scope {
            values: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        }
    }

    pub fn as_rc(self) -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(self))
    }

    pub fn from_rc(rc: Rc<RefCell<Scope>>) -> Scope {
        Rc::try_unwrap(rc).unwrap().into_inner()
    }

    fn get_parent(&self) -> Option<Rc<RefCell<Scope>>> {
        if let Some(p) = self.parent.as_ref() {
            Some(Rc::clone(p))
        }
        else {
            None
        }
    }

    pub fn has_name(&self, name: &str) -> bool {
        if self.values.contains_key(name) {
            return true;
        }

        let mut p = self.get_parent();
        loop {
            if let Some(ps) = p {
                if ps.borrow().values.contains_key(name) {
                    return true;
                }
                else {
                    p = ps.borrow().get_parent();
                }
            }
            else {
                return false;
            }
        }
    }

    pub fn get_value<'src>(&self, name: &'src str) -> Result<Value, Error<'src>> {
        if let Some(v) = self.values.get(name) {
            return Ok(v.clone());
        }

        let mut p = self.get_parent();
        loop {
            if let Some(ps) = p {
                if let Some(v) = ps.borrow().values.get(name) {
                    return Ok(v.clone());
                }
                else {
                    p = ps.borrow().get_parent();
                }
            }
            else {
                return Err(Error::NotDeclared(name));
            }
        }
    }

    pub fn set_value<'src>(&mut self, name: &'src str, value: Value) -> Result<(), Error<'src>> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            return Ok(());
        }

        let mut p = self.get_parent();
        loop {
            if let Some(ps) = p {
                if ps.borrow().values.contains_key(name) {
                    ps.borrow_mut().values.insert(name.to_string(), value);
                    return Ok(());
                }
                else {
                    p = ps.borrow().get_parent();
                }
            }
            else {
                return Err(Error::NotDeclared(name));
            }
        }
    }

    pub fn declare<'src>(&mut self, name: &'src str, value: Value) -> Result<(), Error<'src>> {
        if self.values.contains_key(name) {
            return Err(Error::MultipleDeclaration(name));
        }
        else {
            self.values.insert(name.to_string(), value);
            return Ok(());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::value::Value;
    use super::{
        Scope,
        Error
    };

    #[test]
    fn test_scope() {
        let global = Scope::new().as_rc();
        assert_eq!(global.borrow_mut().declare("foo", Value::Number(123.0)).is_ok(), true);
        {
            let outer = Scope::new_child(&global).as_rc();
            assert_eq!(outer.borrow_mut().declare("bar", Value::Bool(true)).is_ok(), true);
            {
                let inner = Scope::new_child(&outer).as_rc();
                assert_eq!(inner.borrow_mut().declare("bar", Value::String("hello".to_string())).is_ok(), true);
                assert_eq!(inner.borrow().get_value("bar"), Ok(Value::String("hello".to_string())));
                assert_eq!(inner.borrow_mut().set_value("barr", Value::Nil), Err(Error::NotDeclared("barr")));
                assert_eq!(inner.borrow_mut().declare("bar", Value::Nil), Err(Error::MultipleDeclaration("bar")));
                assert_eq!(inner.borrow_mut().set_value("bar", Value::String("world".to_string())).is_ok(), true);
                assert_eq!(inner.borrow().get_value("bar"), Ok(Value::String("world".to_string())));
            }
            assert_eq!(outer.borrow().get_value("bar"), Ok(Value::Bool(true)));
            assert_eq!(outer.borrow_mut().set_value("foo", Value::Number(0.0)).is_ok(), true);
        }
        assert_eq!(global.borrow().get_value("foo"), Ok(Value::Number(0.0)));
    }
}
