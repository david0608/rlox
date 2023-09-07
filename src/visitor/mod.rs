use std::rc::Rc;
use std::cell::RefCell;
use crate::scope::Scope;

pub mod evaluate;

pub mod execute;

pub mod scan;

pub mod parse;

pub trait Accept<'this, V, R> {
    fn accept(&'this self, visitor: V) -> R;
}

pub trait Visit<'that, T: ?Sized, R> {
    fn visit(value: &'that T) -> R;
}

impl<'this, T, V, R> Accept<'this, V, R> for T
    where
    T: ?Sized,
    V: Visit<'this, T, R>
{
    fn accept(&'this self, _visitor: V) -> R {
        V::visit(self)
    }
}

pub trait ScopeAccept<'this, V, R> {
    fn accept(&'this self, visitor: V, scope: &Rc<RefCell<Scope>>) -> R;
}

pub trait ScopeVisit<'that, T: ?Sized, R> {
    fn visit(value: &'that T, scope: &Rc<RefCell<Scope>>) -> R;
}

impl<'this, T, V, R> ScopeAccept<'this, V, R> for T
    where
    T: ?Sized,
    V: ScopeVisit<'this, T, R>
{
    fn accept(&'this self, _visitor: V, scope: &Rc<RefCell<Scope>>) -> R {
        V::visit(self, scope)
    }
}
