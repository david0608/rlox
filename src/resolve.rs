use std::collections::HashSet;
use crate::code::code_span::CodeSpan;

pub trait Resolve {
    type Target;

    fn resolve(&self, context: &mut ResolveCtx) -> Result<Self::Target, ResolveError>;
}

#[derive(Debug, PartialEq)]
pub struct ResolveCtx(Vec<HashSet<String>>);

impl ResolveCtx {
    pub fn new() -> Self {
        ResolveCtx(vec![HashSet::new()])
    }

    pub fn begin(&mut self) {
        self.0.push(HashSet::new());
    }

    pub fn end(&mut self) {
        self.0.pop();
    }

    pub fn find(&self, name: &str) -> Option<usize> {
        for (i, s) in self.0.iter().rev().enumerate() {
            if s.contains(name) {
                return Some(i);
            }
        }

        return None;
    }

    pub fn declare(&mut self, name: &str) -> Result<(), ()> {
        if let Some(s) = self.0.last_mut() {
            if s.contains(name) {
                return Err(());
            }
            else {
                s.insert(name.to_owned());
                return Ok(());
            }
        }
        else {
            unreachable!("ResolveCtx should not be empty vec.");
        }
    }

    #[cfg(test)]
    fn inner(&self) -> &Vec<HashSet<String>> {
        &self.0
    }
}

#[derive(Debug, PartialEq)]
pub enum ResolveErrorEnum {
    VariableNotDeclared,
    VariableHasBeenDeclared,
}

#[derive(Debug, PartialEq)]
pub struct ResolveError {
    pub r#enum: ResolveErrorEnum,
    pub code_span: CodeSpan
}

#[macro_export]
macro_rules! resolve_error {
    (
        $enum:expr,
        $code_span:expr
    ) => {
        ResolveError {
            r#enum: $enum,
            code_span: $code_span,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use crate::resolve::ResolveCtx;

    #[test]
    fn test_resolve_context_new_begin_end() {
        let mut c = ResolveCtx::new();
        assert_eq!(
            c,
            ResolveCtx(vec![HashSet::new()]),
        );
        c.begin();
        assert_eq!(
            c,
            ResolveCtx(vec![HashSet::new(), HashSet::new()]),
        );
        c.end();
        assert_eq!(
            c,
            ResolveCtx(vec![HashSet::new()]),
        );
    }

    #[test]
    fn test_resolve_context_find() {
        let mut c = ResolveCtx::new();
        c.declare("foo").expect("declare foo.");
        c.begin();
        c.declare("bar").expect("declare bar.");
        assert_eq!(c.find("foo"), Some(1));
        assert_eq!(c.find("bar"), Some(0));
        assert_eq!(c.find("hello"), None);
    }

    #[test]
    fn test_resolve_context_declare() {
        let mut c = ResolveCtx::new();
        c.declare("foo").expect("declare foo.");
        c.begin();
        c.declare("foo").expect("declare foo.");
        c.declare("bar").expect("declare bar.");
        assert_eq!(c.inner()[0].contains("foo"), true);
        assert_eq!(c.inner()[0].contains("bar"), false);
        assert_eq!(c.inner()[1].contains("foo"), true);
        assert_eq!(c.inner()[1].contains("bar"), true);
    }

    #[test]
    fn test_resolve_context_declare_variable_has_been_declared_error() {
        let mut c = ResolveCtx::new();
        c.declare("foo").expect("declare foo.");
        assert_eq!(c.declare("foo"), Err(()));
    }
}
