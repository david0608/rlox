use crate::code::Code;
use crate::value::Value;
use crate::call::{
    Call,
    CallError,
};
use crate::error::{
    RuntimeError,
    RuntimeErrorEnum,
};
use crate::parse::expression::assign::{
    AssignExpressionNotResolved,
    AssignExpression,
};
use crate::parse::expression::binary::{
    BinaryExpression,
    BinaryExpressionEnum,
};
use crate::parse::expression::call::CallExpression;
use crate::parse::expression::grouping::GroupingExpression;
use crate::parse::expression::literal::{
    LiteralExpression,
    LiteralExpressionEnum,
};
use crate::parse::expression::logical::{
    LogicalExpression,
    LogicalExpressionEnum,
};
use crate::parse::expression::unary::{
    UnaryExpression,
    UnaryExpressionEnum,
};
use crate::parse::expression::variable::{
    VariableExpressionNotResolved,
    VariableExpression,
};
use crate::environment::{
    Environment,
    EnvironmentOps,
};
use crate::runtime_error;

pub type EvaluateResult = std::result::Result<Value, RuntimeError>;

pub trait Evaluate {
    fn evaluate(&self, env: &Environment) -> EvaluateResult;
}

impl Evaluate for AssignExpressionNotResolved {
    fn evaluate(&self, _: &Environment) -> EvaluateResult {
        panic!("AssignExpressionNotResolved can not evaluate.");
    }
}

impl Evaluate for AssignExpression {
    fn evaluate(&self, env: &Environment) -> EvaluateResult {
        let v = match self.value().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        self.code_span(),
                        err
                    )
                );
            }
        };
        if env.set(
            self.to().name(),
            self.binding(),
            v.clone(),
        ).is_ok()
        {
            return Ok(v);
        }
        else {
            unreachable!("Variable not declared should not be runtime error.");
        }
    }
}

impl Evaluate for BinaryExpression {
    fn evaluate(&self, env: &Environment) -> EvaluateResult {
        let lhs = match self.lhs().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        self.code_span(),
                        err
                    )
                );
            }
        };
        let rhs = match self.rhs().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        self.code_span(),
                        err
                    )
                );
            }
        };
        match self.variant() {
            BinaryExpressionEnum::Equal => {
                Ok(Value::Bool(lhs == rhs))
            }
            BinaryExpressionEnum::NotEqual => {
                Ok(Value::Bool(lhs != rhs))
            }
            BinaryExpressionEnum::Less => {
                match (&lhs, &rhs) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Bool(l < r))
                    }
                    (Value::String(l), Value::String(r)) => {
                        Ok(Value::Bool(l < r))
                    }
                    _ => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::InvalidCompare(lhs, rhs),
                                self.code_span(),
                            )
                        )
                    }
                }
            }
            BinaryExpressionEnum::LessEqual => {
                match (&lhs, &rhs) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Bool(l <= r))
                    }
                    (Value::String(l), Value::String(r)) => {
                        Ok(Value::Bool(l <= r))
                    }
                    _ => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::InvalidCompare(lhs, rhs),
                                self.code_span(),
                            )
                        )
                    }
                }
            }
            BinaryExpressionEnum::Greater => {
                match (&lhs, &rhs) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Bool(l > r))
                    }
                    (Value::String(l), Value::String(r)) => {
                        Ok(Value::Bool(l > r))
                    }
                    _ => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::InvalidCompare(lhs, rhs),
                                self.code_span(),
                            )
                        )
                    }
                }
            }
            BinaryExpressionEnum::GreaterEqual => {
                match (&lhs, &rhs) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Bool(l >= r))
                    }
                    (Value::String(l), Value::String(r)) => {
                        Ok(Value::Bool(l >= r))
                    }
                    _ => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::InvalidCompare(lhs, rhs),
                                self.code_span(),
                            )
                        )
                    }
                }
            }
            BinaryExpressionEnum::Plus => {
                match (&lhs, &rhs) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l + r))
                    }
                    (Value::String(l), Value::String(r)) => {
                        Ok(Value::String(l.to_owned() + r))
                    }
                    _ => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::InvalidArithmetic(lhs, rhs),
                                self.code_span(),
                            )
                        )
                    }
                }
            }
            BinaryExpressionEnum::Minus => {
                match (&lhs, &rhs) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l - r))
                    }
                    _ => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::InvalidArithmetic(lhs, rhs),
                                self.code_span(),
                            )
                        )
                    }
                }
            }
            BinaryExpressionEnum::Multiply => {
                match (&lhs, &rhs) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l * r))
                    }
                    _ => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::InvalidArithmetic(lhs, rhs),
                                self.code_span(),
                            )
                        )
                    }
                }
            }
            BinaryExpressionEnum::Divide => {
                match (&lhs, &rhs) {
                    (Value::Number(l), Value::Number(r)) => {
                        if *r == 0.0 {
                            Err(
                                runtime_error!(
                                    RuntimeErrorEnum::DivideByZero(lhs, rhs),
                                    self.code_span(),
                                )
                            )
                        }
                        else {
                            Ok(Value::Number(l / r))
                        }
                    }
                    _ => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::InvalidArithmetic(lhs, rhs),
                                self.code_span(),
                            )
                        )
                    }
                }
            }
        }
    }
}

impl Evaluate for CallExpression {
    fn evaluate(&self, env: &Environment) -> EvaluateResult {
        let callee = match self.callee().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        self.code_span(),
                        err
                    )
                );
            }
        };
        let mut arguments = Vec::new();
        for a in self.arguments() {
            arguments.push(
                match a.evaluate(env) {
                    Ok(val) => val,
                    Err(err) => {
                        return Err(
                            runtime_error!(
                                RuntimeErrorEnum::RuntimeError,
                                self.code_span(),
                                err
                            )
                        );
                    }
                }
            );
        }
        match callee.call(arguments) {
            Ok(v) => {
                return Ok(v);
            }
            Err(err) => {
                match err {
                    CallError::ArgumentNumberMismatch(ec, pc) => {
                        return Err(
                            runtime_error!(
                                RuntimeErrorEnum::ArgumentNumberMismatch(ec, pc),
                                self.code_span(),
                            )
                        );
                    }
                    CallError::NotCallable => {
                        return Err(
                            runtime_error!(
                                RuntimeErrorEnum::NotCallable(callee),
                                self.code_span(),
                            )
                        );
                    }
                    CallError::RuntimeError(err) => {
                        return Err(
                            runtime_error!(
                                RuntimeErrorEnum::RuntimeError,
                                self.code_span(),
                                err
                            )
                        );
                    }
                }
            }
        }
    }
}

impl Evaluate for GroupingExpression {
    fn evaluate(&self, env: &Environment) -> EvaluateResult {
        self.expression().evaluate(env)
    }
}

impl Evaluate for LiteralExpression {
    fn evaluate(&self, _: &Environment) -> EvaluateResult {
        match self.variant() {
            LiteralExpressionEnum::Nil => Ok(Value::Nil),
            LiteralExpressionEnum::True => Ok(Value::Bool(true)),
            LiteralExpressionEnum::False => Ok(Value::Bool(false)),
            LiteralExpressionEnum::Number(t) => Ok(Value::Number(t.literal())),
            LiteralExpressionEnum::String(t) => Ok(Value::String(t.literal().to_string())),
        }
    }
}

impl Evaluate for LogicalExpression {
    fn evaluate(&self, env: &Environment) -> EvaluateResult {
        let lv = match self.lhs().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        self.code_span(),
                        err
                    )
                );
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
                return Err(
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        self.code_span(),
                        err
                    )
                );
            }
        };

        return Ok(rv);
    }
}

impl Evaluate for UnaryExpression {
    fn evaluate(&self, env: &Environment) -> EvaluateResult {
        let rhs = match self.rhs().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        self.code_span(),
                        err
                    )
                );
            }
        };
        match self.variant() {
            UnaryExpressionEnum::Negative => {
                match rhs {
                    Value::Nil
                    | Value::Bool(_)
                    | Value::String(_)
                    | Value::Function(_)
                    | Value::NativeFunction(_) => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::InvalidNegate(rhs),
                                self.code_span(),
                            )
                        )
                    }
                    Value::Number(rhs) => {
                        Ok(Value::Number(-rhs))
                    }
                }
            }
            UnaryExpressionEnum::Not => {
                Ok(Value::Bool(!rhs.is_truthy()))
            }
        }
    }
}

impl Evaluate for VariableExpressionNotResolved {
    fn evaluate(&self, _: &Environment) -> EvaluateResult {
        panic!("VariableExpressionNotResolved can not evaluate");
    }
}

impl Evaluate for VariableExpression {
    fn evaluate(&self, env: &Environment) -> EvaluateResult {
        if let Some(v) = env.get(
            self.from().name(),
            self.binding(),
        )
        {
            return Ok(v);
        }
        else {
            unreachable!("Variable not declared should not be runtime error.");
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::native::clock::add_native_clock;
    use crate::parse::expression::{
        assign::AssignExpression,
        binary::BinaryExpression,
        call::CallExpression,
        logical::LogicalExpression,
        unary::UnaryExpression,
        variable::VariableExpression,
    };
    use crate::value::Value;
    use crate::environment::EnvironmentOps;
    use crate::error::{
        RuntimeError,
        RuntimeErrorEnum,
    };
    use crate::evaluate::EvaluateResult;
    use crate::utils::test_utils::{
        TestContext,
        parse_expression,
        parse_expression_unknown,
    };
    use crate::runtime_error;

    fn evaluate_src(src: &str) -> EvaluateResult {
        let mut ctx = TestContext::new();
        ctx.evaluate(parse_expression_unknown(src).as_ref())
    }

    #[test]
    fn test_assign_expression() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = false;");
        let expr = ctx.resolve_expression::<AssignExpression>(
            parse_expression_unknown("foo = true").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn test_assign_expression_value_evaluate_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        let expr = ctx.resolve_expression::<AssignExpression>(
            parse_expression_unknown("foo = 1 < true").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()).unwrap_err(),
            runtime_error!(
                RuntimeErrorEnum::RuntimeError,
                new_code_span(0, 0, 0, 14),
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 6, 0, 14),
                )
            )
        );
    }

    #[test]
    #[should_panic]
    fn test_assign_expression_var_not_declared_panic() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.execute_src("var bar;");
        let expr = ctx.resolve_expression::<AssignExpression>(
            parse_expression_unknown("foo = bar").as_ref()
        )
            .unwrap();
        let mut ctx = TestContext::new();
        ctx.execute_src("var bar;");
        ctx.evaluate(expr.as_ref()).expect("evaluate");
    }

    #[test]
    fn test_binary_expression_operand_evaluate_error() {
        let mut ctx = TestContext::new();
        let expr = ctx.resolve_expression::<BinaryExpression>(
            parse_expression_unknown("1 + true == false").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(0, 0, 0, 17),
                    runtime_error!(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                        new_code_span(0, 0, 0, 8),
                    )
                )
            )
        );
        let expr = ctx.resolve_expression::<BinaryExpression>(
            parse_expression_unknown("true == false + 1").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(0, 0, 0, 17),
                    runtime_error!(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(false), Value::Number(1.0)),
                        new_code_span(0, 8, 0, 17),
                    )
                )
            )
        );
    }

    #[test]
    fn test_binary_expression_equal() {
        assert_eq!(evaluate_src("nil == nil"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("nil == true"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("nil == false"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("nil == 1"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("nil == 0"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("nil == \"hello\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("nil == \"\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("true == nil"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("true == true"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("true == false"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("true == 1"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("true == 0"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("true == \"hello\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("true == \"\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("false == nil"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("false == true"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("false == false"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("false == 1"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("false == 0"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("false == \"hello\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("false == \"\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("1 == nil"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("1 == true"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("1 == false"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("1 == 1"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("1 == 0"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("1 == \"hello\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("1 == \"\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("0 == nil"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("0 == true"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("0 == false"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("0 == 1"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("0 == 0"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("0 == \"hello\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("0 == \"\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"hello\" == nil"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"hello\" == true"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"hello\" == false"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"hello\" == 1"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"hello\" == 0"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"hello\" == \"hello\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"hello\" == \"\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"\" == nil"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"\" == true"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"\" == false"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"\" == 1"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"\" == 0"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"\" == \"hello\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"\" == \"\""), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_binary_expression_not_equal() {
        assert_eq!(evaluate_src("nil != nil"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("nil != true"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("nil != false"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("nil != 1"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("nil != 0"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("nil != \"hello\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("nil != \"\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("true != nil"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("true != true"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("true != false"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("true != 1"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("true != 0"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("true != \"hello\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("true != \"\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("false != nil"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("false != true"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("false != false"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("false != 1"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("false != 0"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("false != \"hello\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("false != \"\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("1 != nil"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("1 != true"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("1 != false"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("1 != 1"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("1 != 0"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("1 != \"hello\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("1 != \"\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("0 != nil"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("0 != true"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("0 != false"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("0 != 1"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("0 != 0"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("0 != \"hello\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("0 != \"\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"hello\" != nil"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"hello\" != true"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"hello\" != false"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"hello\" != 1"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"hello\" != 0"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"hello\" != \"hello\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"hello\" != \"\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"\" != nil"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"\" != true"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"\" != false"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"\" != 1"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"\" != 0"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"\" != \"hello\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"\" != \"\""), Ok(Value::Bool(false)));
    }

    #[test]
    fn test_binary_expression_less() {
        assert_eq!(
            evaluate_src("nil < nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil < true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil < 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil < \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("true < nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("true < true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true < 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("true < \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 < nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 < true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_src("1 < 0"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("1 < 1"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("1 < 2"), Ok(Value::Bool(true)));
        assert_eq!(
            evaluate_src("1 < \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" < nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" < true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" < 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(evaluate_src("\"hello\" < \"gello\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"hello\" < \"hello\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"hello\" < \"iello\""), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_binary_expression_less_equal() {
        assert_eq!(
            evaluate_src("nil <= nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil <= true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil <= 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil <= \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("true <= nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true <= true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_src("true <= 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("true <= \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 <= nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 <= true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(evaluate_src("1 <= 0"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("1 <= 1"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("1 <= 2"), Ok(Value::Bool(true)));
        assert_eq!(
            evaluate_src("1 <= \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" <= nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" <= true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" <= 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(evaluate_src("\"hello\" <= \"gello\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"hello\" <= \"hello\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"hello\" <= \"iello\""), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_binary_expression_greater() {
        assert_eq!(
            evaluate_src("nil > nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil > true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil > 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil > \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("true > nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("true > true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true > 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("true > \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 > nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 > true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_src("1 > 0"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("1 > 1"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("1 > 2"), Ok(Value::Bool(false)));
        assert_eq!(
            evaluate_src("1 > \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" > nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" > true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" > 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(evaluate_src("\"hello\" > \"gello\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"hello\" > \"hello\""), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("\"hello\" > \"iello\""), Ok(Value::Bool(false)));
    }

    #[test]
    fn test_binary_expression_greater_equal() {
        assert_eq!(
            evaluate_src("nil >= nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil >= true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil >= 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil >= \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("true >= nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true >= true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_src("true >= 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("true >= \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 >= nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 >= true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(evaluate_src("1 >= 0"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("1 >= 1"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("1 >= 2"), Ok(Value::Bool(false)));
        assert_eq!(
            evaluate_src("1 >= \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" >= nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" >= true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" >= 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(evaluate_src("\"hello\" >= \"gello\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"hello\" >= \"hello\""), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("\"hello\" >= \"iello\""), Ok(Value::Bool(false)));
    }

    #[test]
    fn test_binary_expression_plus() {
        assert_eq!(
            evaluate_src("nil + nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil + true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil + 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil + \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("true + nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("true + true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true + 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("true + \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 + nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 + true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_src("1 + 1"), Ok(Value::Number(2.0)));
        assert_eq!(
            evaluate_src("1 + \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" + nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" + true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" + 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(evaluate_src("\"hello\" + \"hello\""), Ok(Value::String("hellohello".to_owned())));
    }

    #[test]
    fn test_binary_expression_minus() {
        assert_eq!(
            evaluate_src("nil - nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil - true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil - 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil - \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("true - nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("true - true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true - 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("true - \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 - nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 - true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_src("1 - 1"), Ok(Value::Number(0.0)));
        assert_eq!(
            evaluate_src("1 - \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" - nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" - true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" - 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" - \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 17),
                )
            )
        );
    }

    #[test]
    fn test_binary_expression_multiply() {
        assert_eq!(
            evaluate_src("nil * nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil * true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil * 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil * \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("true * nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("true * true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true * 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("true * \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 * nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 * true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_src("2 * 2"), Ok(Value::Number(4.0)));
        assert_eq!(
            evaluate_src("1 * \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" * nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" * true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" * 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" * \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 17),
                )
            )
        );
    }

    #[test]
    fn test_binary_expression_divide() {
        assert_eq!(
            evaluate_src("nil / nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil / true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil / 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil / \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("true / nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("true / true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true / 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("true / \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 / nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 / true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_src("2 / 2"), Ok(Value::Number(1.0)));
        assert_eq!(
            evaluate_src("1 / 0"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::DivideByZero(Value::Number(1.0), Value::Number(0.0)),
                    new_code_span(0, 0, 0, 5),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 / \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" / nil"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" / true"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" / 1"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" / \"hello\""),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 17),
                )
            )
        );
    }

    #[test]
    fn test_call_expression() {
        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            fun foo(a, b) {
                return a + b;
            }
            "
        );
        assert_eq!(
            ctx.evaluate(parse_expression::<CallExpression>("foo(1, 2)").as_ref()),
            Ok(Value::Number(3.0))
        );
    }

    #[test]
    fn test_call_expression_callee_evaluate_error() {
        let mut ctx = TestContext::new();
        let expr = ctx.resolve_expression::<CallExpression>(
            parse_expression_unknown("(true + 1)(1, 2)").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(0, 0, 0, 16),
                    runtime_error!(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        new_code_span(0, 1, 0, 9),
                    )
                )
            )
        );
    }

    #[test]
    fn test_call_expression_argument_evaluate_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("fun foo(a) { }");
        let expr = ctx.resolve_expression::<CallExpression>(
            parse_expression_unknown("foo(true + 1);").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(0, 0, 0, 13),
                    runtime_error!(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        new_code_span(0, 4, 0, 12),
                    )
                )
            )
        );
    }

    #[test]
    fn test_call_expression_argument_number_mismatch_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            fun foo(a, b) {
                return a + b;
            }
            "
        );
        assert_eq!(
            ctx.evaluate(parse_expression::<CallExpression>("foo(1, 2, 3)").as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::ArgumentNumberMismatch(2, 3),
                    new_code_span(0, 0, 0, 12),
                )
            )
        );
    }

    #[test]
    fn test_call_expression_not_callable_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            var foo = true;
            "
        );
        assert_eq!(
            ctx.evaluate(parse_expression::<CallExpression>("foo()").as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::NotCallable(Value::Bool(true)),
                    new_code_span(0, 0, 0, 5),
                )
            ),
        );
    }

    #[test]
    fn test_call_expression_call_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            fun foo() {
                return true + 1;
            }
            "
        );
        let expr = ctx.resolve_expression::<CallExpression>(
            parse_expression_unknown("foo()").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(0, 0, 0, 5),
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        new_code_span(2, 0, 2, 16),
                        runtime_error!(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            new_code_span(2, 7, 2, 15),
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn test_grouping_expression() {
        assert_eq!(evaluate_src("(nil)"), Ok(Value::Nil));
        assert_eq!(evaluate_src("(true)"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("(123)"), Ok(Value::Number(123.0)));
        assert_eq!(evaluate_src("(\"hello\")"), Ok(Value::String("hello".to_owned())));
        assert_eq!(evaluate_src("2 * (3 - 1)"), Ok(Value::Number(4.0)));
        assert_eq!(
            evaluate_src("(true + 1)"),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 1, 0, 9),
                )
            )
        );
    }

    #[test]
    fn test_literal_expression() {
        assert_eq!(evaluate_src("nil"), Ok(Value::Nil));
        assert_eq!(evaluate_src("true"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("false"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("123"), Ok(Value::Number(123.0)));
        assert_eq!(evaluate_src("1.23"), Ok(Value::Number(1.23)));
        assert_eq!(evaluate_src("\"hello\""), Ok(Value::String("hello".to_owned())));
    }

    #[test]
    fn test_logical_expression_operand_evaluate_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.evaluate(parse_expression::<LogicalExpression>("true + 1 or false").as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(0, 0, 0, 17),
                    runtime_error!(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        new_code_span(0, 0, 0, 8),
                    )
                )
            )
        );
        assert_eq!(
            ctx.evaluate(parse_expression::<LogicalExpression>("false or 1 + false").as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(0, 0, 0, 18),
                    runtime_error!(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(false)),
                        new_code_span(0, 9, 0, 18),
                    )
                )
            )
        );
    }

    #[test]
    fn test_logical_expression() {
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
    fn test_unary_expression_right_evaluate_error() {
        let mut ctx = TestContext::new();
        let expr = parse_expression::<UnaryExpression>("!(true + 1)");
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(0, 0, 0, 11),
                    runtime_error!(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        new_code_span(0, 2, 0, 10),
                    )
                )
            )
        );
    }

    #[test]
    fn test_unary_expression() {
        let mut ctx = TestContext::new();
        add_native_clock(&mut ctx.resolve_context, &ctx.environment);
        ctx.execute_src(
            "
            fun foo() {
                print \"hello\";
            }
            "
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("-nil").as_ref()
            ),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(Value::Nil),
                    new_code_span(0, 0, 0, 4),
                )
            ),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("-true").as_ref()
            ),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(Value::Bool(true)),
                    new_code_span(0, 0, 0, 5),
                )
            ),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("-\"hello\"").as_ref()
            ),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 8),
                )
            ),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("-foo").as_ref()
            ),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(ctx.environment.get("foo", 0).unwrap()),
                    new_code_span(0, 0, 0, 4),
                )
            ),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("-clock").as_ref()
            ),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(ctx.environment.get("clock", 0).unwrap()),
                    new_code_span(0, 0, 0, 6),
                )
            ),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("-1.0").as_ref()
            ),
            Ok(Value::Number(-1.0)),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("!true").as_ref()
            ),
            Ok(Value::Bool(false)),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("!false").as_ref()
            ),
            Ok(Value::Bool(true)),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("!1.0").as_ref()
            ),
            Ok(Value::Bool(false)),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("!0.0").as_ref()
            ),
            Ok(Value::Bool(true)),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("!\"hello\"").as_ref()
            ),
            Ok(Value::Bool(false)),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("!\"\"").as_ref()
            ),
            Ok(Value::Bool(true)),
        );
    }

    #[test]
    fn test_variable_expression() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = true;");
        let expr = ctx.resolve_expression::<VariableExpression>(
            parse_expression_unknown("foo").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Ok(Value::Bool(true)),
        );
    }

    #[test]
    #[should_panic]
    fn test_variable_expression_not_declare_panic() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        let expr = ctx.resolve_expression::<VariableExpression>(
            parse_expression_unknown("foo").as_ref()
        )
            .unwrap();
        let mut ctx = TestContext::new();
        ctx.evaluate(expr.as_ref()).expect("evaluate");
    }
}
