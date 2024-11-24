use std::{
    rc::Rc,
    cell::RefCell,
    collections::HashSet,
};
use crate::{
    code::{
        Code,
        CodeSpan,
    },
    parse::statement::{
        Statement,
        ExecuteOk,
    },
    environment::Environment,
    error::{
        RuntimeError,
        ResolveError,
    },
};

#[derive(Debug)]
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
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        "break;".to_owned()
    }
}

impl Statement for BreakStatement {
    fn resolve(&self, _: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Statement>, ResolveError> {
        Ok(
            Rc::new(
                BreakStatement::new(
                    self.code_span.clone()
                )
            )
        )
    }

    fn execute(&self, _: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        return Ok(ExecuteOk::Break);
    }
}

#[macro_export]
macro_rules! break_statement {
    ( $code_span:expr ) => {
        Rc::new(
            BreakStatement::new(
                $code_span,
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use crate::{
        code::{
            Code,
            CodeSpan,
        },
        parse::statement::{
            ExecuteOk,
            r#break::BreakStatement,
        },
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
            assert_eq!(parse_statement::<BreakStatement>(src).to_string(), expect);
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
        let stmt = break_statement!(CodeSpan::new(0, 0, 0, 0));
        let mut ctx = TestContext::new();
        ctx.resolve_statement_unknown(stmt.as_ref()).expect("resolve break statement");
    }
}
