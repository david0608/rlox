use std::rc::Rc;
use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::parse::expression::Expression;
use crate::scan::token::identifier::IdentifierToken;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
    ResolveErrorEnum,
};
use crate::resolve_error;
use super::{
    Statement,
    AsStatement,
};

pub struct VarDeclareStatement {
    name: IdentifierToken,
    initializer: Option<Expression>,
    code_span: CodeSpan,
}

impl VarDeclareStatement {
    pub fn new(
        name: IdentifierToken,
        initializer: Option<Expression>,
        code_span: CodeSpan
    ) -> VarDeclareStatement
    {
        VarDeclareStatement {
            name,
            initializer,
            code_span,
        }
    }

    pub fn name(&self) -> &IdentifierToken {
        &self.name
    }

    pub fn initializer(&self) -> Option<&Expression> {
        self.initializer.as_ref()
    }
}

impl Code for VarDeclareStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl AsStatement for VarDeclareStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        let initializer = if let Some(e) = self.initializer.as_ref() {
            Some(e.resolve(context)?)
        }
        else {
            None
        };
        if context.declare(self.name.name()).is_err() {
            return Err(
                resolve_error!(
                    ResolveErrorEnum::VariableHasBeenDeclared,
                    self.name.code_span()
                )
            );
        }
        return Ok(
            Statement(
                Rc::new(
                    VarDeclareStatement::new(
                        self.name.clone(),
                        initializer,
                        self.code_span.clone(),
                    )
                )
            )
        );
    }
}

#[macro_export]
macro_rules! var_declare_statement {
    ( $identifier:expr, $initializer:expr, $code_span:expr ) => {
        Statement(
            Rc::new(
                VarDeclareStatement::new(
                    $identifier,
                    $initializer,
                    $code_span,
                )
            )
        )
    };
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::parse::{
        expression::variable::VariableExpression,
        statement::var_declare::VarDeclareStatement,
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
    fn test_var_declare_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var bar;");
        ctx.resolve_context.begin();

        let var_declare_stmt = ctx.resolve_statement::<VarDeclareStatement>(
            parse_statement_unknown("var foo = bar;").as_ref()
        )
            .unwrap();
        let init_expr = var_declare_stmt.initializer().unwrap().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(init_expr.binding(), 1);
    }

    #[test]
    fn test_var_declare_statement_resolve_initializer_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<VarDeclareStatement>("var foo = bar;").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 10, 0, 13)
            )
        );
    }

    #[test]
    fn test_var_declare_statement_resolve_has_been_declared_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<VarDeclareStatement>("var foo = true;").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableHasBeenDeclared,
                new_code_span(0, 4, 0, 7)
            )
        );
    }
}
