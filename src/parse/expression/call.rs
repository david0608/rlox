use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use super::{
    Expression,
    BoxedExpression,
};

pub struct CallExpression {
    callee: BoxedExpression,
    arguments: Vec<BoxedExpression>,
    code_span: CodeSpan,
}

impl CallExpression {
    pub fn new(
        callee: BoxedExpression,
        arguments: Vec<BoxedExpression>,
        code_span: CodeSpan,
    ) -> CallExpression
    {
        CallExpression {
            callee,
            arguments,
            code_span,
        }
    }

    pub fn callee(&self) -> &BoxedExpression {
        &self.callee
    }

    pub fn arguments(&self) -> &Vec<BoxedExpression> {
        &self.arguments
    }
}

impl Code for CallExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for CallExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            CallExpression::new(
                self.callee().clone(),
                self.arguments().clone(),
                self.code_span(),
            )
        )
    }

    fn resolve(&self, context: &mut ResolveCtx) -> Result<BoxedExpression, ResolveError> {
        Ok(
            Box::new(
                CallExpression::new(
                    self.callee.resolve(context)?,
                    self.arguments.iter().try_fold(
                        Vec::new(),
                        |mut args, a| {
                            args.push(a.resolve(context)?);
                            Ok(args)
                        },
                    )?,
                    self.code_span.clone(),
                )
            )
        )
    }
}

#[macro_export]
macro_rules! call_expression {
    ( $callee:expr, $arguments:expr, $code_span:expr ) => {
        Box::new(
            CallExpression::new(
                $callee,
                $arguments,
                $code_span
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::parse::expression::{
        call::CallExpression,
        variable::VariableExpression,
    };
    use crate::resolve::{
        ResolveError,
        ResolveErrorEnum,
    };
    use crate::utils::{
        AsAny,
        test_utils::{
            TestContext,
            parse_expression,
            parse_expression_unknown,
        }
    };
    use crate::{
        resolve_error,
        downcast_ref,
    };

    #[test]
    fn test_call_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.resolve_context.begin();
        ctx.execute_src("var bar;");
        ctx.resolve_context.begin();
        ctx.execute_src("fun hello(a, b) { return a + b; }");

        let call_expr = ctx.resolve_expression::<CallExpression>(
            parse_expression_unknown("hello(foo, bar)").as_ref()
        )
            .unwrap();
        assert_eq!(
            downcast_ref!(call_expr.callee(), VariableExpression).binding(),
            0
        );
        assert_eq!(
            downcast_ref!(call_expr.arguments()[0], VariableExpression).binding(),
            2
        );
        assert_eq!(
            downcast_ref!(call_expr.arguments()[1], VariableExpression).binding(),
            1
        );
    }

    #[test]
    fn test_call_expression_resolve_callee_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<CallExpression>("hello(foo, bar)").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 0, 0, 5)
            )
        );
    }

    #[test]
    fn test_call_expression_resolve_argument_resolve_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("fun hello(a, b) { return a + b; }");
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<CallExpression>("hello(foo, bar)").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 6, 0, 9)
            )
        );
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<CallExpression>("hello(1, bar)").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 9, 0, 12)
            )
        );
    }
}
