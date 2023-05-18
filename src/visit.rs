pub trait Accept<V, R> {
    fn accept(&self, visitor: V) -> R;
}

pub trait AcceptMut<V, R> {
    fn accept_mut(&mut self, visitor: V) -> R;
}

pub trait AcceptMut1<V, P1, R> {
    fn accept_mut(&mut self, _visitor: V, p1: P1) -> R;
}

pub trait Visit<E, R> {
    fn visit(e: &E) -> R;
}

pub trait VisitMut<E, R> {
    fn visit_mut(e: &mut E) -> R;
}

pub trait VisitMut1<E, P1, R> {
    fn visit_mut(e: &mut E, p1: P1) -> R;
}

impl<V, R, T> Accept<V, R> for T
    where
    V: Visit<T, R>
{
    fn accept(&self, _visitor: V) -> R {
        V::visit(self)
    }
}

impl<V, R, T> AcceptMut<V, R> for T
    where
    V: VisitMut<T, R>
{
    fn accept_mut(&mut self, _visitor: V) -> R {
        V::visit_mut(self)
    }
}

impl<V, P1, R, T> AcceptMut1<V, P1, R> for T
    where
    V: VisitMut1<T, P1, R>
{
    fn accept_mut(&mut self, _visitor: V, p1: P1) -> R {
        V::visit_mut(self, p1)
    }
}
