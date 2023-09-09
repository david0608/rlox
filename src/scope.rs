use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use crate::evaluate::value::value::Value;

#[derive(PartialEq, Debug)]
pub enum ScopeError {
    NotDeclared,
    MultipleDeclaration,
    GlobalVariableMutationNotSupport,
}

#[derive(Debug)]
pub struct Scope {
    values: HashMap<String, Value>,
    global: Option<Rc<RefCell<Scope>>>,
    parent: Option<Rc<RefCell<Scope>>>,
}

impl<'stmt> Scope {
    pub fn new() -> Scope {
        Scope {
            values: HashMap::new(),
            global: None,
            parent: None,
        }
    }

    pub fn new_isolate_child(parent: &Rc<RefCell<Scope>>) -> Scope {
        Scope {
            values: HashMap::new(),
            global: Some(Rc::clone(parent.borrow().global.as_ref().unwrap_or(parent))),
            parent: None,
        }
    }

    pub fn new_child(parent: &Rc<RefCell<Scope>>) -> Scope {
        Scope {
            values: HashMap::new(),
            global: Some(Rc::clone(parent.borrow().global.as_ref().unwrap_or(parent))),
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
                break;
            }
        }

        if let Some(gs) = self.global.as_ref() {
            return gs.borrow().values.contains_key(name);
        }
        else {
            return false;
        }
    }

    pub fn get_value(&self, name: &str) -> Result<Value, ScopeError> {
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
                break;
            }
        }

        if let Some(gs) = self.global.as_ref() {
            if let Some(v) = gs.borrow().values.get(name) {
                return Ok(v.clone());
            }
        }

        return Err(ScopeError::NotDeclared);
    }

    pub fn set_value(&mut self, name: &str, value: Value) -> Result<(), ScopeError> {
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
                break;
            }
        }

        if let Some(gs) = self.global.as_ref() {
            if gs.borrow().values.contains_key(name) {
                return Err(ScopeError::GlobalVariableMutationNotSupport);
            }
        }

        return Err(ScopeError::NotDeclared);
    }

    pub fn declare(&mut self, name: &str, value: Value) -> Result<(), ScopeError> {
        if self.values.contains_key(name) {
            return Err(ScopeError::MultipleDeclaration);
        }
        else {
            self.values.insert(name.to_string(), value);
            return Ok(());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluate::value::value::Value;
    use super::{
        Scope,
        ScopeError
    };

    #[test]
    fn test_declare() {
        let s = Scope::new().as_rc();
        assert_eq!(s.borrow_mut().declare("foo", Value::Number(123.0)).is_ok(), true);
        assert_eq!(s.borrow().get_value("foo"), Ok(Value::Number(123.0)));
    }

    #[test]
    fn test_local() {
        let p = Scope::new().as_rc();
        {
            let c = Scope::new_child(&p).as_rc();
            assert_eq!(c.borrow_mut().declare("foo", Value::Bool(true)).is_ok(), true);
            assert_eq!(c.borrow().get_value("foo"), Ok(Value::Bool(true)));
        }
        assert_eq!(p.borrow().get_value("foo"), Err(ScopeError::NotDeclared));
    }

    #[test]
    fn test_global() {
        let g = Scope::new().as_rc();
        assert_eq!(g.borrow_mut().declare("foo", Value::Bool(true)).is_ok(), true);
        {
            let s = Scope::new_isolate_child(&g).as_rc();
            assert_eq!(s.borrow().get_value("foo"), Ok(Value::Bool(true)));
        }
    }

    #[test]
    fn test_isolation() {
        let g = Scope::new().as_rc();
        assert_eq!(g.borrow_mut().declare("foo", Value::Bool(true)).is_ok(), true);
        {
            let s1 = Scope::new_isolate_child(&g).as_rc();
            assert_eq!(s1.borrow_mut().declare("bar", Value::Bool(true)).is_ok(), true);
            {
                let s2 = Scope::new_isolate_child(&s1).as_rc();
                assert_eq!(s2.borrow().get_value("foo"), Ok(Value::Bool(true)));
                assert_eq!(s2.borrow().get_value("bar"), Err(ScopeError::NotDeclared));
            }
        }
    }

    #[test]
    fn test_multiple_declaration() {
        let p = Scope::new().as_rc();
        assert_eq!(p.borrow_mut().declare("foo", Value::Bool(true)).is_ok(), true);
        assert_eq!(p.borrow_mut().declare("foo", Value::Bool(true)), Err(ScopeError::MultipleDeclaration));
        {
            let c = Scope::new_child(&p).as_rc();
            assert_eq!(c.borrow_mut().declare("foo", Value::Number(123.0)).is_ok(), true);
            assert_eq!(c.borrow_mut().declare("foo", Value::Number(123.0)), Err(ScopeError::MultipleDeclaration));
            assert_eq!(c.borrow().get_value("foo"), Ok(Value::Number(123.0)));
        }
        assert_eq!(p.borrow().get_value("foo"), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_global_mutation() {
        let g = Scope::new().as_rc();
        assert_eq!(g.borrow_mut().declare("foo", Value::Bool(true)).is_ok(), true);
        {
            let l = Scope::new_isolate_child(&g).as_rc();
            assert_eq!(l.borrow_mut().set_value("foo", Value::Bool(false)), Err(ScopeError::GlobalVariableMutationNotSupport));
            assert_eq!(l.borrow().get_value("foo"), Ok(Value::Bool(true)));
        }
        assert_eq!(g.borrow_mut().set_value("foo", Value::Bool(false)).is_ok(), true);
        assert_eq!(g.borrow().get_value("foo"), Ok(Value::Bool(false)));
    }
}
