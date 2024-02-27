use std::rc::Rc;
use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use super::{
    Statement,
    AsStatement,
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
    use crate::code::code_span::new_code_span;
    use crate::parse::statement::{
        Statement,
        r#break::BreakStatement,
    };
    use crate::utils::test_utils::TestContext;
    use crate::break_statement;

    #[test]
    fn test_break_statement_resolve() {
        let stmt = break_statement!(new_code_span(0, 0, 0, 0));
        let mut ctx = TestContext::new();
        ctx.resolve_statement_unknown(stmt.as_ref()).expect("resolve break statement");
    }
}
