use std::{
    rc::Rc,
    cell::RefCell,
};
use crate::{
    code::{
        Code,
        code_span::CodeSpan,
    },
    parse::expression::Expression,
    value::Value,
    environment::Environment,
    error::RuntimeError,
    evaluate::Evaluate,
    print::Print,
    resolve::{
        ResolveCtx,
        ResolveError,
    },
    impl_debug_for_printable
};

#[derive(Clone, Copy)]
pub enum LogicalExpressionEnum {
    And,
    Or,
}

pub struct LogicalExpression {
    variant: LogicalExpressionEnum,
    lhs: Rc<dyn Expression>,
    rhs: Rc<dyn Expression>,
    code_span: CodeSpan,
}

impl LogicalExpression {
    pub fn new(
        variant: LogicalExpressionEnum,
        lhs: Rc<dyn Expression>,
        rhs: Rc<dyn Expression>,
        code_span: CodeSpan,
    ) -> LogicalExpression
    {
        LogicalExpression {
            variant,
            lhs,
            rhs,
            code_span,
        }
    }

    pub fn variant(&self) -> LogicalExpressionEnum {
        self.variant
    }

    pub fn lhs(&self) -> &Rc<dyn Expression> {
        &self.lhs
    }

    pub fn rhs(&self) -> &Rc<dyn Expression> {
        &self.rhs
    }
}

impl Code for LogicalExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for LogicalExpression {
    fn print(&self) -> String {
        match self.variant() {
            LogicalExpressionEnum::And => {
                format!("(and {} {})", self.lhs().print(), self.rhs().print())
            }
            LogicalExpressionEnum::Or => {
                format!("(or {} {})", self.lhs().print(), self.rhs().print())
            }
        }
    }
}

impl_debug_for_printable!(LogicalExpression);

impl Evaluate for LogicalExpression {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let lv = match self.lhs().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(RuntimeError::wrap(err, self.code_span()));
            }
        };

        match self.variant() {
            LogicalExpressionEnum::And => {
                if !lv.is_truthy() {
                    return Ok(lv);
                }
            }
            LogicalExpressionEnum::Or => {
                if lv.is_truthy() {
                    return Ok(lv);
                }
            }
        }

        let rv = match self.rhs().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(RuntimeError::wrap(err, self.code_span()));
            }
        };

        return Ok(rv);
    }
}

impl Expression for LogicalExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Expression>, ResolveError> {
        Ok(
            Rc::new(
                LogicalExpression::new(
                    self.variant,
                    self.lhs.resolve(context)?,
                    self.rhs.resolve(context)?,
                    self.code_span.clone(),
                )
            )
        )
    }
}

#[macro_export]
macro_rules! logical_expression {
    ( $variant:ident, $lhs:expr, $rhs:expr, $code_span:expr ) => {
        Rc::new(
            LogicalExpression::new(
                LogicalExpressionEnum::$variant,
                $lhs,
                $rhs,
                $code_span
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        code::code_span::new_code_span,
        parse::expression::{
            logical::LogicalExpression,
            variable::VariableExpression,
        },
        value::Value,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
        print::Print,
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
                evaluate_src,
            }
        }
    };

    #[test]
    fn test_logical_expression_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("x and y", "(and x y)"),
            ("x or y", "(or x y)"),
            ("x or y and z", "(or x (and y z))"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_expression::<LogicalExpression>(src).print(), expect);
        }
    }

    #[test]
    fn test_logical_expression_evaluate() {
        assert_eq!(evaluate_src("nil and \"foo\""), Ok(Value::Nil));
        assert_eq!(evaluate_src("nil or \"foo\""), Ok(Value::String("foo".to_owned())));
        assert_eq!(evaluate_src("true and 1"), Ok(Value::Number(1.0)));
        assert_eq!(evaluate_src("true or 1"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("false and 1"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("false or 1"), Ok(Value::Number(1.0)));
        assert_eq!(evaluate_src("1 and true"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("1 or true"), Ok(Value::Number(1.0)));
        assert_eq!(evaluate_src("0 and true"), Ok(Value::Number(0.0)));
        assert_eq!(evaluate_src("0 or true"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"foo\" and true"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"foo\" or true"), Ok(Value::String("foo".to_owned())));
        assert_eq!(evaluate_src("\"\" and true"), Ok(Value::String("".to_owned())));
        assert_eq!(evaluate_src("\"\" or true"), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_logical_expression_evaluate_operand_evaluate_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.evaluate(parse_expression::<LogicalExpression>("true + 1 or false").as_ref()),
            Err(
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        new_code_span(0, 0, 0, 8),
                    ),
                    new_code_span(0, 0, 0, 17),
                )
            )
        );
        assert_eq!(
            ctx.evaluate(parse_expression::<LogicalExpression>("false or 1 + false").as_ref()),
            Err(
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(false)),
                        new_code_span(0, 9, 0, 18),
                    ),
                    new_code_span(0, 0, 0, 18),
                )
            )
        );
    }

    #[test]
    fn test_logical_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.resolve_context.begin();
        ctx.execute_src("var bar;");

        let logical_expr = ctx.resolve_expression::<LogicalExpression>(
            parse_expression_unknown("foo or bar").as_ref()
        )
            .unwrap();
        assert_eq!(
            logical_expr.lhs().downcast_ref::<VariableExpression>().unwrap().binding(),
            1
        );
        assert_eq!(
            logical_expr.rhs().downcast_ref::<VariableExpression>().unwrap().binding(),
            0
        );
    }

    #[test]
    fn test_logical_expression_resolve_lhs_rhs_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<LogicalExpression>("bar or true").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 0, 0, 3)
            )
        );
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<LogicalExpression>("true or bar").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 8, 0, 11)
            )
        );
    }
}
