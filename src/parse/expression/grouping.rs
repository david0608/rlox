use std::{
    rc::Rc,
    cell::RefCell,
};
use crate::{
    code::{
        Code,
        CodeSpan,
    },
    parse::expression::Expression,
    value::Value,
    environment::Environment,
    error::RuntimeError,
    resolve::{
        ResolveCtx,
        ResolveError,
    },
};

#[derive(Debug)]
pub struct GroupingExpression {
    expression: Rc<dyn Expression>,
    code_span: CodeSpan,
}

impl GroupingExpression {
    pub fn new(expression: Rc<dyn Expression>, code_span: CodeSpan) -> GroupingExpression {
        GroupingExpression {
            expression,
            code_span,
        }
    }

    pub fn expression(&self) -> &Rc<dyn Expression> {
        &self.expression
    }
}

impl Code for GroupingExpression {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        format!("(group {})", self.expression().to_string())
    }
}

impl Expression for GroupingExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Expression>, ResolveError> {
        Ok(
            Rc::new(
                GroupingExpression::new(
                    self.expression.resolve(context)?,
                    self.code_span.clone(),
                )
            )
        )
    }

    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        match self.expression().evaluate(env) {
            Ok(v) => Ok(v),
            Err(e) => Err(RuntimeError::wrap(e, self.code_span))
        }
    }
}

#[macro_export]
macro_rules! grouping_expression {
    ( $expression:expr, $code_span:expr ) => {
        Rc::new(
            GroupingExpression::new(
                $expression,
                $code_span
            )
        )
    };
}

#[cfg(test)]
mod tests {
    use crate::{
        code::CodeSpan,
        parse::expression::{
            grouping::GroupingExpression,
            variable::VariableExpression,
        },
        value::Value,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
        resolve::{
            ResolveError,
            ResolveErrorEnum,
        },
        utils::{
            Downcast,
            test_utils::{
                TestContext,
                parse_expression,
                parse_expression_unknown,
                evaluate_src
            },
        }
    };

    #[test]
    fn test_grouping_expression_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("(123)", "(group 123)"),
            ("(1 + 2) * 3", "(* (group (+ 1 2)) 3)"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_expression_unknown(src).to_string(), expect);
        }
    }

    #[test]
    fn test_grouping_expression_evaluate() {
        assert_eq!(evaluate_src("(nil)"), Ok(Value::Nil));
        assert_eq!(evaluate_src("(true)"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("(123)"), Ok(Value::Number(123.0)));
        assert_eq!(evaluate_src("(\"hello\")"), Ok(Value::String("hello".to_owned())));
        assert_eq!(evaluate_src("2 * (3 - 1)"), Ok(Value::Number(4.0)));
    }

    #[test]
    fn test_grouping_expression_evaluate_error() {
        assert_eq!(
            evaluate_src("(true + 1)"),
            Err(
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        CodeSpan::new(0, 1, 0, 9),
                    ),
                    CodeSpan::new(0, 0, 0, 10)
                )
            )
        );
    }

    #[test]
    fn test_grouping_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let group_expr = ctx.resolve_expression::<GroupingExpression>(
            parse_expression_unknown("(foo)").as_ref()
        )
            .unwrap();
        assert_eq!(
            group_expr.expression().downcast_ref::<VariableExpression>().unwrap().binding(),
            0
        );
    }

    #[test]
    fn test_grouping_expression_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<GroupingExpression>("(foo)").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                CodeSpan::new(0, 1, 0, 4)
            )
        );
    }
}
