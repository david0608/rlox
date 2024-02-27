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

pub struct ForStatement {
    initializer: Option<Statement>,
    condition: Option<Expression>,
    increment: Option<Expression>,
    body: Statement,
    code_span: CodeSpan,
}

impl ForStatement {
    pub fn new(
        initializer: Option<Statement>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        body: Statement,
        code_span: CodeSpan,
    ) -> ForStatement
    {
        ForStatement {
            initializer,
            condition,
            increment,
            body,
            code_span,
        }
    }

    pub fn initializer(&self) -> Option<&Statement> {
        self.initializer.as_ref()
    }

    pub fn condition(&self) -> Option<&Expression> {
        self.condition.as_ref()
    }

    pub fn increment(&self) -> Option<&Expression> {
        self.increment.as_ref()
    }

    pub fn body(&self) -> &Statement {
        &self.body
    }
}

impl Code for ForStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl AsStatement for ForStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        context.begin();
        let initializer = if let Some(s) = self.initializer.as_ref() {
            match s.resolve(context) {
                Ok(s) => Some(s),
                Err(e) => {
                    context.end();
                    return Err(e);
                }
            }
        }
        else {
            None
        };
        let condition = if let Some(e) = self.condition.as_ref() {
            match e.resolve(context) {
                Ok(e) => Some(e),
                Err(e) => {
                    context.end();
                    return Err(e);
                }
            }
        }
        else {
            None
        };
        let increment = if let Some(e) = self.increment.as_ref() {
            match e.resolve(context) {
                Ok(e) => Some(e),
                Err(e) => {
                    context.end();
                    return Err(e);
                }
            }
        }
        else {
            None
        };
        let body = match self.body.resolve(context) {
            Ok(s) => s,
            Err(e) => {
                context.end();
                return Err(e);
            }
        };
        context.end();
        Ok(
            Statement(
                Rc::new(
                    ForStatement::new(
                        initializer,
                        condition,
                        increment,
                        body,
                        self.code_span.clone(),
                    )
                )
            )
        )
    }
}

#[macro_export]
macro_rules! for_statement {
    (
        $initializer:expr,
        $condition:expr,
        $increment:expr,
        $body:expr,
        $code_span:expr,
    ) => {
        Statement(
            Rc::new(
                ForStatement::new(
                    $initializer,
                    $condition,
                    $increment,
                    $body,
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
        expression::{
            assign::AssignExpression,
            binary::BinaryExpression,
            variable::VariableExpression
        },
        statement::{
            block::BlockStatement,
            expression::ExpressionStatement,
            r#for::ForStatement,
            var_declare::VarDeclareStatement,
        }
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
    fn test_for_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var j = 0;");

        let for_stmt = ctx.resolve_statement::<ForStatement>(
            parse_statement_unknown("for (var i = j; i < 10; i = i + 1) { j = i; }").as_ref()
        )
            .unwrap();

        let init_stmt = for_stmt.initializer().unwrap().downcast_ref::<VarDeclareStatement>().unwrap();
        let var_expr = init_stmt.initializer().unwrap().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 1);

        let cond_expr = for_stmt.condition().unwrap().downcast_ref::<BinaryExpression>().unwrap();
        let lhs_expr = cond_expr.lhs().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(lhs_expr.binding(), 0);

        let assign_expr = for_stmt.increment().unwrap().downcast_ref::<AssignExpression>().unwrap();
        assert_eq!(assign_expr.binding(), 0);
        let bin_expr = assign_expr.value().downcast_ref::<BinaryExpression>().unwrap();
        let lhs_expr = bin_expr.lhs().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(lhs_expr.binding(), 0);

        let body_stmt = for_stmt.body().downcast_ref::<BlockStatement>().unwrap();
        let expr_stmt = body_stmt.statements()[0].downcast_ref::<ExpressionStatement>().unwrap();
        let assign_expr = expr_stmt.expression().downcast_ref::<AssignExpression>().unwrap();
        assert_eq!(assign_expr.binding(), 2);

        let val_expr = assign_expr.value().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(val_expr.binding(), 1);
    }

    #[test]
    fn test_for_statement_resolve_initializer_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<ForStatement>("for (var i = j; i < 10; i = i + 1) { j = i; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 13, 0, 14)
            )
        );
    }

    #[test]
    fn test_for_statement_resolve_condition_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<ForStatement>("for (var i = 1; j < 10; i = i + 1) { j = i; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 16, 0, 17)
            )
        );
    }

    #[test]
    fn test_for_statement_resolve_increment_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<ForStatement>("for (var i = 1; i < 10; i = j + 1) { j = i; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 28, 0, 29)
            )
        );
    }

    #[test]
    fn test_for_statement_resolve_body_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<ForStatement>("for (var i = 1; i < 10; i = i + 1) { j = i; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 37, 0, 38)
            )
        );
    }
}
