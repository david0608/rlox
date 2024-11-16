use std::{
    rc::Rc,
    cell::RefCell,
};
use crate::{
    code::{
        Code,
        code_span::CodeSpan,
    },
    parse::statement::{
        Statement,
        AsStatement,
    },
    environment::Environment,
    error::RuntimeError,
    execute::{
        Execute,
        ExecuteOk,
    },
    print::Print,
    resolve::{
        ResolveCtx,
        ResolveError,
    },
    impl_debug_for_printable,
};

pub struct BreakStatement {
    code_span: CodeSpan,
}

impl BreakStatement {
    pub fn new(
        code_span: CodeSpan,
    ) -> BreakStatement
    {
        BreakStatement {
            code_span,
        }
    }
}

impl Code for BreakStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for BreakStatement {
    fn print(&self) -> String {
        "break;".to_owned()
    }
}

impl_debug_for_printable!(BreakStatement);

impl Execute for BreakStatement {
    fn execute(&self, _: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        return Ok(ExecuteOk::Break);
    }
}

impl AsStatement for BreakStatement {
    fn resolve(&self, _: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        Ok(
            Statement(
                Rc::new(
                    BreakStatement::new(
                        self.code_span.clone()
                    )
                )
            )
        )
    }
}

#[macro_export]
macro_rules! break_statement {
    ( $code_span:expr ) => {
        Statement(
            Rc::new(
                BreakStatement::new(
                    $code_span,
                )
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use crate::{
        code::code_span::new_code_span,
        parse::statement::{
            Statement,
            r#break::BreakStatement,
        },
        execute::ExecuteOk,
        print::Print,
        utils::test_utils::{
            TestContext,
            parse_statement,
        },
        break_statement,
    };

    #[test]
    fn test_break_statement_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("break;", "break;"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_statement::<BreakStatement>(src).print(), expect);
        }
    }

    #[test]
    fn test_break_statement_execute() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(parse_statement::<BreakStatement>("break;").as_ref()),
            Ok(ExecuteOk::Break)
        );
    }

    #[test]
    fn test_break_statement_resolve() {
        let stmt = break_statement!(new_code_span(0, 0, 0, 0));
        let mut ctx = TestContext::new();
        ctx.resolve_statement_unknown(stmt.as_ref()).expect("resolve break statement");
    }
}
