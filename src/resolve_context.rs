use std::collections::HashSet;

pub trait ResolveContext {
    fn new() -> Self;

    fn begin(&mut self);

    fn end(&mut self);

    fn find(&self, name: &str) -> Option<usize>;

    fn declare(&mut self, name: &str) -> Result<(), ()>;
}

impl ResolveContext for Vec<HashSet<String>> {
    fn new() -> Self {
        vec![HashSet::new()]
    }

    fn begin(&mut self) {
        self.push(HashSet::new())
    }

    fn end(&mut self) {
        self.pop();
    }

    fn find(&self, name: &str) -> Option<usize> {
        for (i, s) in self.iter().rev().enumerate() {
            if s.contains(name) {
                return Some(i);
            }
        }

        return None;
    }

    fn declare(&mut self, name: &str) -> Result<(), ()> {
        if let Some(s) = self.last_mut() {
            if s.contains(name) {
                return Err(());
            }
            else {
                s.insert(name.to_owned());
                return Ok(());
            }
        }
        else {
            unreachable!("ResolveContext should not be empty vec.");
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use crate::resolve_context::ResolveContext;

    #[test]
    fn test_resolve_context_new_begin_end() {
        let mut c = <Vec<HashSet<String>> as ResolveContext>::new();
        assert_eq!(
            c,
            vec![HashSet::new()],
        );
        c.begin();
        assert_eq!(
            c,
            vec![HashSet::new(), HashSet::new()],
        );
        c.end();
        assert_eq!(
            c,
            vec![HashSet::new()],
        );
    }

    #[test]
    fn test_resolve_context_find() {
        let mut c = <Vec<HashSet<String>> as ResolveContext>::new();
        c.declare("foo").expect("declare foo.");
        c.begin();
        c.declare("bar").expect("declare bar.");
        assert_eq!(c.find("foo"), Some(1));
        assert_eq!(c.find("bar"), Some(0));
        assert_eq!(c.find("hello"), None);
    }

    #[test]
    fn test_resolve_context_declare() {
        let mut c = <Vec<HashSet<String>> as ResolveContext>::new();
        c.declare("foo").expect("declare foo.");
        c.begin();
        c.declare("foo").expect("declare foo.");
        c.declare("bar").expect("declare bar.");
        assert_eq!(c[0].contains("foo"), true);
        assert_eq!(c[0].contains("bar"), false);
        assert_eq!(c[1].contains("foo"), true);
        assert_eq!(c[1].contains("bar"), true);
    }

    #[test]
    fn test_resolve_context_declare_variable_has_been_declared_error() {
        let mut c = <Vec<HashSet<String>> as ResolveContext>::new();
        c.declare("foo").expect("declare foo.");
        assert_eq!(c.declare("foo"), Err(()));
    }
}
