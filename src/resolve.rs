use std::{
    rc::Rc,
    collections::HashSet,
};
use crate::{
    code::code_span::CodeSpan,
    parse::statement::Statement,
    error::LoxError,
};

pub trait Resolve {
    fn resolve(&self, ctx: &mut ResolveCtx) -> Result<Vec<Rc<dyn Statement>>, ResolveError>;
}

impl Resolve for Vec<Rc<dyn Statement>> {
    fn resolve(&self, ctx: &mut ResolveCtx) -> Result<Vec<Rc<dyn Statement>>, ResolveError> {
        let mut stmts = Vec::new();
        for s in self {
            stmts.push(s.resolve(ctx)?);
        }
        return Ok(stmts);
    }
}

#[derive(Debug, PartialEq)]
pub struct ResolveCtx(Vec<HashSet<String>>);

impl ResolveCtx {
    pub fn new() -> Self {
        ResolveCtx(vec![HashSet::new()])
    }

    pub (in crate) fn begin(&mut self) {
        self.0.push(HashSet::new());
    }

    pub (in crate) fn end(&mut self) {
        self.0.pop();
    }

    pub (in crate) fn find(&self, name: &str) -> Option<usize> {
        for (i, s) in self.0.iter().rev().enumerate() {
            if s.contains(name) {
                return Some(i);
            }
        }

        return None;
    }

    pub (in crate) fn declare(&mut self, name: &str) -> Result<(), ()> {
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
    VariableHaveBeenDeclared,
}

#[derive(Debug, PartialEq)]
pub struct ResolveError {
    pub r#enum: ResolveErrorEnum,
    pub code_span: CodeSpan
}

impl ResolveError {
    pub (in crate) fn new(r#enum: ResolveErrorEnum, code_span: CodeSpan) -> Self {
        ResolveError {
            r#enum,
            code_span,
        }
    }
}

impl LoxError for ResolveError {
    fn print(&self, src_lines: &Vec<&str>) -> String {
        match self.r#enum {
            ResolveErrorEnum::VariableNotDeclared => {
                let mut out = "ResolveError: Variable not declared.\r\n".to_string();
                out += self.code_span.debug_string(src_lines).as_ref();
                return out;
            }
            ResolveErrorEnum::VariableHaveBeenDeclared => {
                let mut out = "ResolveError: Variable have been declared.\r\n".to_string();
                out += self.code_span.debug_string(src_lines).as_ref();
                return out;
            }
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
