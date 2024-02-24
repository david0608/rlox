use crate::code::code_span::CodeSpan;
use crate::code::Code;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
    ResolveErrorEnum,
};
use super::{
    Statement,
    BoxedStatement,
};
use crate::scan::token::identifier::IdentifierToken;
use crate::resolve_error;

pub struct FunDeclareStatement {
    name: IdentifierToken,
    parameters: Vec<IdentifierToken>,
    body: Vec<BoxedStatement>,
    code_span: CodeSpan,
}

impl FunDeclareStatement {
    pub fn new(
        name: IdentifierToken,
        parameters: Vec<IdentifierToken>,
        body: Vec<BoxedStatement>,
        code_span: CodeSpan,
    ) -> FunDeclareStatement
    {
        FunDeclareStatement {
            name,
            parameters,
            body,
            code_span,
        }
    }

    pub fn name(&self) -> &IdentifierToken {
        &self.name
    }

    pub fn parameters(&self) -> &Vec<IdentifierToken> {
        &self.parameters
    }

    pub fn body(&self) -> &Vec<BoxedStatement> {
        &self.body
    }
}

impl Code for FunDeclareStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for FunDeclareStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            FunDeclareStatement::new(
                self.name.clone(),
                self.parameters().clone(),
                self.body().clone(),
                self.code_span(),
            )
        )
    }

    fn resolve(&self, context: &mut ResolveCtx) -> Result<BoxedStatement, ResolveError> {
        if context.declare(self.name.name()).is_err() {
            return Err(
                resolve_error!(
                    ResolveErrorEnum::VariableHasBeenDeclared,
                    self.name.code_span()
                )
            );
        }
        context.begin();
        for p in self.parameters.iter() {
            if context.declare(p.name()).is_err() {
                context.end();
                return Err(
                    resolve_error!(
                        ResolveErrorEnum::VariableHasBeenDeclared,
                        p.code_span()
                    )
                );
            }
        }
        let body = self.body.iter().map(|s| s.resolve(context)).collect();
        context.end();
        match body {
            Ok(body) => {
                return Ok(
                    Box::new(
                        FunDeclareStatement::new(
                            self.name.clone(),
                            self.parameters.clone(),
                            body,
                            self.code_span.clone(),
                        )
                    )
                );
            }
            Err(e) => {
                return Err(e);
            }
        }
    }
}

#[macro_export]
macro_rules! fun_declare_statement {
    ( $identifier:expr, $parameters:expr, $body:expr, $code_span:expr ) => {
        Box::new(
            FunDeclareStatement::new(
                $identifier,
                $parameters,
                $body,
                $code_span,
            )
        )
    };
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::parse::{
        expression::variable::VariableExpression,
        statement::{
            fun_declare::FunDeclareStatement,
            print::PrintStatement,
        }
    };
    use crate::resolve::{
        ResolveError,
        ResolveErrorEnum,
    };
    use crate::utils::{
        AsAny,
        test_utils::{
            TestContext,
            parse_statement,
            parse_statement_unknown,
        },
    };
    use crate::{
        resolve_error,
        downcast_ref,
    };

    #[test]
    fn test_fun_declare_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var i = 0;");

        let fun_declare_stmt = ctx.resolve_statement::<FunDeclareStatement>(
            parse_statement_unknown(
                "
                fun test(a) {
                    print a;
                    print i;
                }
                "
                )
                .as_ref()
        )
            .unwrap();

        let print_stmt = downcast_ref!(fun_declare_stmt.body[0], PrintStatement);
        let var_expr = downcast_ref!(print_stmt.value(), VariableExpression);
        assert_eq!(var_expr.binding(), 0);

        let print_stmt = downcast_ref!(fun_declare_stmt.body()[1], PrintStatement);
        let var_expr = downcast_ref!(print_stmt.value(), VariableExpression);
        assert_eq!(var_expr.binding(), 1);
    }

    #[test]
    fn test_fun_declare_statement_resolve_has_been_declared_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var test;");
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<FunDeclareStatement>("fun test() {}").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableHasBeenDeclared,
                new_code_span(0, 4, 0, 8)
            )
        );
    }

    #[test]
    fn test_fun_declare_statement_resolve_body_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<FunDeclareStatement>("fun test(a) { var a; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableHasBeenDeclared,
                new_code_span(0, 18, 0, 19)
            )
        );
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<FunDeclareStatement>("fun test() { var a = b; }").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 21, 0, 22)
            )
        );
    }
}
