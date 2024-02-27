use std::rc::Rc;
use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::parse::expression::Expression;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use super::{
    Statement,
    AsStatement,
};

pub struct PrintStatement {
    value: Expression,
    code_span: CodeSpan,
}

impl PrintStatement {
    pub fn new(value: Expression, code_span: CodeSpan) -> PrintStatement {
        PrintStatement {
            value,
            code_span,
        }
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}

impl Code for PrintStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl AsStatement for PrintStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        Ok(
            Statement(
                Rc::new(
                    PrintStatement::new(
                        self.value.resolve(context)?,
                        self.code_span.clone(),
                    )
                )
            )
        )
    }
}

#[macro_export]
macro_rules! print_statement {
    ( $expression:expr, $code_span:expr ) => {
        Statement(
            Rc::new(
                PrintStatement::new(
                    $expression,
                    $code_span,
                )
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::parse::{
        expression::variable::VariableExpression,
        statement::print::PrintStatement,
    };
    use crate::resolve::{
        ResolveError,
        ResolveErrorEnum,
    };
    use crate::utils::test_utils::{
        TestContext,
        parse_statement,
        parse_statement_unknown,
    };
    use crate::resolve_error;

    #[test]
    fn test_print_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let print_stmt = ctx.resolve_statement::<PrintStatement>(
            parse_statement_unknown("print foo;").as_ref()
        )
            .unwrap();
        let var_expr = print_stmt.value.downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 0);
    }

    #[test]
    fn test_print_statement_resolve_value_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<PrintStatement>("print foo;").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 6, 0, 9)
            )
        );
    }
}
