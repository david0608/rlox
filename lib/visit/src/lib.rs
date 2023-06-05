pub trait Accept<'a, V, R> {
    fn accept(&'a self, visitor: V) -> R;
}

pub trait Visit<'a, T: ?Sized, R> {
    fn visit(value: &'a T) -> R;
}

impl<'a, V, R, T> Accept<'a, V, R> for T
    where
    T: ?Sized,
    V: Visit<'a, T, R>
{
    fn accept(&'a self, _visitor: V) -> R {
        V::visit(self)
    }
}
