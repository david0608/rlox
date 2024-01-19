use std::rc::Rc;
use std::cell::RefCell;
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
use crate::parse::expression::assign::AssignExpression;
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
use crate::parse::expression::variable::VariableExpression;
use crate::environment::{
    Environment,
    EnvironmentError,
    environment_get_value,
    environment_set_value,
};
use crate::runtime_error;

pub type EvaluateResult = std::result::Result<Value, RuntimeError>;

pub trait Evaluate {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> EvaluateResult;
}

impl Evaluate for AssignExpression {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> EvaluateResult {
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
        match environment_set_value(env, self.name(), v.clone()) {
            Ok(_) => {
                Ok(v)
            }
            Err(err) => {
                match err {
                    EnvironmentError::NotDeclared => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::VariableNotDeclared,
                                self.code_span(),
                            )
                        )
                    }
                    _ => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::Unknown,
                                self.code_span(),
                            )
                        )
                    }
                }
            }
        }
    }
}

impl Evaluate for BinaryExpression {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> EvaluateResult {
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
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> EvaluateResult {
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
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> EvaluateResult {
        self.expression().evaluate(env)
    }
}

impl Evaluate for LiteralExpression {
    fn evaluate(&self, _: &Rc<RefCell<Environment>>) -> EvaluateResult {
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
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> EvaluateResult {
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
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> EvaluateResult {
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

impl Evaluate for VariableExpression {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> EvaluateResult {
        match environment_get_value(env, self.name()) {
            Ok(val) => {
                Ok(val)
            }
            Err(_) => {
                Err(
                    runtime_error!(
                        RuntimeErrorEnum::VariableNotDeclared,
                        self.code_span(),
                    )
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use std::cell::RefCell;
    use crate::code::code_point::CodePoint;
    use crate::code::code_span::CodeSpan;
    use crate::native::clock::add_native_clock;
    use crate::value::Value;
    use crate::parse::Parse;
    use crate::parse::parser::Parser;
    use crate::scan::Scan;
    use crate::environment::{
        Environment,
        new_environment,
        environment_get_value,
    };
    use crate::error::{
        RuntimeError,
        RuntimeErrorEnum,
    };
    use crate::evaluate::EvaluateResult;
    use crate::runtime_error;

    fn code_span(sl: usize, sc: usize, el: usize, ec: usize) -> CodeSpan {
        CodeSpan::new(CodePoint::new(sl, sc), CodePoint::new(el, ec))
    }

    fn execute_src(src: &str, env: &Rc<RefCell<Environment>>) {
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        for stmt in stmts {
            stmt.execute(env).unwrap();
        }
    }

    fn evaluate_expression(src: &str, env: &Rc<RefCell<Environment>>) -> EvaluateResult {
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        Parser::new(&tokens).expression().unwrap().evaluate(env)
    }


    #[test]
    fn test_assign_expression() {
        let env = new_environment();
        execute_src("var foo = false;", &env);
        assert_eq!(
            evaluate_expression("foo = true", &env),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn test_assign_expression_evaluate_error() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("foo = bar", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(0, 0, 0, 9),
                    runtime_error!(
                        RuntimeErrorEnum::VariableNotDeclared,
                        code_span(0, 6, 0, 9),
                    )
                )
            )
        );
    }

    #[test]
    fn test_assign_expression_variable_not_declared_error() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("foo = true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::VariableNotDeclared,
                    code_span(0, 0, 0, 10),
                )
            )
        );
    }

    #[test]
    fn test_binary_expression_operand_evaluate_error() {
        let env = new_environment();
        execute_src("var foo = true;", &env);
        assert_eq!(
            evaluate_expression("bar == foo", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(0, 0, 0, 10),
                    runtime_error!(
                        RuntimeErrorEnum::VariableNotDeclared,
                        code_span(0, 0, 0, 3),
                    )
                )
            )
        );
        assert_eq!(
            evaluate_expression("foo == bar", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(0, 0, 0, 10),
                    runtime_error!(
                        RuntimeErrorEnum::VariableNotDeclared,
                        code_span(0, 7, 0, 10),
                    )
                )
            )
        );
    }

    #[test]
    fn test_binary_expression_equal() {
        let env = new_environment();
        assert_eq!(evaluate_expression("nil == nil", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("nil == true", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("nil == false", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("nil == 1", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("nil == 0", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("nil == \"hello\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("nil == \"\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true == nil", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true == true", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true == false", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true == 1", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true == 0", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true == \"hello\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true == \"\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false == nil", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false == true", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false == false", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false == 1", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false == 0", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false == \"hello\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false == \"\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 == nil", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 == true", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 == false", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 == 1", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 == 0", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 == \"hello\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 == \"\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 == nil", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 == true", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 == false", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 == 1", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 == 0", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 == \"hello\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 == \"\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" == nil", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" == true", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" == false", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" == 1", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" == 0", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" == \"hello\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" == \"\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == nil", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == true", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == false", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == 1", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == 0", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == \"hello\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == \"\"", &env), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_binary_expression_not_equal() {
        let env = new_environment();
        assert_eq!(evaluate_expression("nil != nil", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("nil != true", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("nil != false", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("nil != 1", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("nil != 0", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("nil != \"hello\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("nil != \"\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true != nil", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true != true", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true != false", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true != 1", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true != 0", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true != \"hello\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true != \"\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false != nil", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false != true", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false != false", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false != 1", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false != 0", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false != \"hello\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false != \"\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 != nil", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 != true", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 != false", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 != 1", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 != 0", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 != \"hello\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 != \"\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 != nil", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 != true", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 != false", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 != 1", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 != 0", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 != \"hello\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 != \"\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" != nil", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" != true", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" != false", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" != 1", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" != 0", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" != \"hello\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" != \"\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != nil", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != true", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != false", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != 1", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != 0", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != \"hello\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != \"\"", &env), Ok(Value::Bool(false)));
    }

    #[test]
    fn test_binary_expression_less() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("nil < nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil < true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil < 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil < \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true < nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true < true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true < 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true < \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 < nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 < true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_expression("1 < 0", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 < 1", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 < 2", &env), Ok(Value::Bool(true)));
        assert_eq!(
            evaluate_expression("1 < \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" < nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" < true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" < 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(evaluate_expression("\"hello\" < \"gello\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" < \"hello\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" < \"iello\"", &env), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_binary_expression_less_equal() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("nil <= nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil <= true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil <= 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil <= \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true <= nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true <= true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true <= 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true <= \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 <= nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 <= true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(evaluate_expression("1 <= 0", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 <= 1", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 <= 2", &env), Ok(Value::Bool(true)));
        assert_eq!(
            evaluate_expression("1 <= \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" <= nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" <= true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" <= 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(evaluate_expression("\"hello\" <= \"gello\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" <= \"hello\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" <= \"iello\"", &env), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_binary_expression_greater() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("nil > nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil > true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil > 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil > \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true > nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true > true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true > 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true > \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 > nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 > true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_expression("1 > 0", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 > 1", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 > 2", &env), Ok(Value::Bool(false)));
        assert_eq!(
            evaluate_expression("1 > \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" > nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" > true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" > 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(evaluate_expression("\"hello\" > \"gello\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" > \"hello\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" > \"iello\"", &env), Ok(Value::Bool(false)));
    }

    #[test]
    fn test_binary_expression_greater_equal() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("nil >= nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil >= true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil >= 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil >= \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true >= nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true >= true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true >= 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true >= \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 >= nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 >= true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(evaluate_expression("1 >= 0", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 >= 1", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 >= 2", &env), Ok(Value::Bool(false)));
        assert_eq!(
            evaluate_expression("1 >= \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" >= nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" >= true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" >= 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(evaluate_expression("\"hello\" >= \"gello\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" >= \"hello\"", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" >= \"iello\"", &env), Ok(Value::Bool(false)));
    }

    #[test]
    fn test_binary_expression_plus() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("nil + nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil + true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil + 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil + \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true + nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true + true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true + 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true + \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 + nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 + true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_expression("1 + 1", &env), Ok(Value::Number(2.0)));
        assert_eq!(
            evaluate_expression("1 + \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" + nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" + true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" + 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(evaluate_expression("\"hello\" + \"hello\"", &env), Ok(Value::String("hellohello".to_owned())));
    }

    #[test]
    fn test_binary_expression_minus() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("nil - nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil - true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil - 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil - \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true - nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true - true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true - 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true - \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 - nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 - true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_expression("1 - 1", &env), Ok(Value::Number(0.0)));
        assert_eq!(
            evaluate_expression("1 - \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" - nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" - true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" - 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" - \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 17),
                )
            )
        );
    }

    #[test]
    fn test_binary_expression_multiply() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("nil * nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil * true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil * 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil * \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true * nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true * true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true * 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true * \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 * nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 * true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_expression("2 * 2", &env), Ok(Value::Number(4.0)));
        assert_eq!(
            evaluate_expression("1 * \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" * nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" * true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" * 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" * \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 17),
                )
            )
        );
    }

    #[test]
    fn test_binary_expression_divide() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("nil / nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil / true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil / 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil / \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true / nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true / true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true / 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true / \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 / nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 / true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_expression("2 / 2", &env), Ok(Value::Number(1.0)));
        assert_eq!(
            evaluate_expression("1 / 0", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::DivideByZero(Value::Number(1.0), Value::Number(0.0)),
                    code_span(0, 0, 0, 5),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 / \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" / nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" / true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" / 1", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" / \"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 17),
                )
            )
        );
    }

    #[test]
    fn test_call_expression() {
        let env = new_environment();
        execute_src(
            "
            fun foo(a, b) {
                return a + b;
            }
            ",
            &env,
        );
        assert_eq!(
            evaluate_expression("foo(1, 2)", &env),
            Ok(Value::Number(3.0)),
        );
    }

    #[test]
    fn test_call_expression_callee_evaluate_error() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("foo(1, 2)", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(0, 0, 0, 9),
                    runtime_error!(
                        RuntimeErrorEnum::VariableNotDeclared,
                        code_span(0, 0, 0, 3),
                    )
                )
            ),
        );
    }

    #[test]
    fn test_call_expression_argument_evaluate_error() {
        let env = new_environment();
        execute_src(
            "
            fun foo(a, b) {
                return a + b;
            }
            ",
            &env,
        );
        assert_eq!(
            evaluate_expression("foo(a, b)", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(0, 0, 0, 9),
                    runtime_error!(
                        RuntimeErrorEnum::VariableNotDeclared,
                        code_span(0, 4, 0, 5),
                    )
                )
            ),
        );
    }

    #[test]
    fn test_call_expression_argument_number_mismatch_error() {
        let env = new_environment();
        execute_src(
            "
            fun foo(a, b) {
                return a + b;
            }
            ",
            &env,
        );
        assert_eq!(
            evaluate_expression("foo(1, 2, 3)", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::ArgumentNumberMismatch(2, 3),
                    code_span(0, 0, 0, 12),
                )
            ),
        );
    }

    #[test]
    fn test_call_expression_not_callable_error() {
        let env = new_environment();
        execute_src(
            "
            var foo = true;
            ",
            &env,
        );
        assert_eq!(
            evaluate_expression("foo()", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::NotCallable(Value::Bool(true)),
                    code_span(0, 0, 0, 5),
                )
            ),
        );
    }

    #[test]
    fn test_call_expression_runtime_error() {
        let env = new_environment();
        execute_src(
            "
            fun foo() {
                return bar;
            }
            ",
            &env,
        );
        assert_eq!(
            evaluate_expression("foo()", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(0, 0, 0, 5),
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        code_span(2, 0, 2, 11),
                        runtime_error!(
                            RuntimeErrorEnum::VariableNotDeclared,
                            code_span(2, 7, 2, 10),
                        )
                    )
                )
            ),
        );
    }

    #[test]
    fn test_grouping_expression() {
        let env = new_environment();
        assert_eq!(evaluate_expression("(nil)", &env), Ok(Value::Nil));
        assert_eq!(evaluate_expression("(true)", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("(123)", &env), Ok(Value::Number(123.0)));
        assert_eq!(evaluate_expression("(\"hello\")", &env), Ok(Value::String("hello".to_owned())));
        assert_eq!(evaluate_expression("2 * (3 - 1)", &env), Ok(Value::Number(4.0)));
        assert_eq!(
            evaluate_expression("(true + 1)", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 1, 0, 9),
                )
            )
        );
    }

    #[test]
    fn test_literal_expression() {
        let env = new_environment();
        assert_eq!(evaluate_expression("nil", &env), Ok(Value::Nil));
        assert_eq!(evaluate_expression("true", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("123", &env), Ok(Value::Number(123.0)));
        assert_eq!(evaluate_expression("1.23", &env), Ok(Value::Number(1.23)));
        assert_eq!(evaluate_expression("\"hello\"", &env), Ok(Value::String("hello".to_owned())));
    }

    #[test]
    fn test_logical_expression_left_evaluate_error() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("foo or true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(0, 0, 0, 11),
                    runtime_error!(
                        RuntimeErrorEnum::VariableNotDeclared,
                        code_span(0, 0, 0, 3),
                    )
                )
            )
        );
    }

    #[test]
    fn test_logical_expression_right_evaluate_error() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("true and foo", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(0, 0, 0, 12),
                    runtime_error!(
                        RuntimeErrorEnum::VariableNotDeclared,
                        code_span(0, 9, 0, 12),
                    )
                )
            )
        );
    }

    #[test]
    fn test_logical_expression() {
        let env = new_environment();
        assert_eq!(evaluate_expression("nil and \"foo\"", &env), Ok(Value::Nil));
        assert_eq!(evaluate_expression("nil or \"foo\"", &env), Ok(Value::String("foo".to_owned())));
        assert_eq!(evaluate_expression("true and 1", &env), Ok(Value::Number(1.0)));
        assert_eq!(evaluate_expression("true or 1", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false and 1", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false or 1", &env), Ok(Value::Number(1.0)));
        assert_eq!(evaluate_expression("1 and true", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 or true", &env), Ok(Value::Number(1.0)));
        assert_eq!(evaluate_expression("0 and true", &env), Ok(Value::Number(0.0)));
        assert_eq!(evaluate_expression("0 or true", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"foo\" and true", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"foo\" or true", &env), Ok(Value::String("foo".to_owned())));
        assert_eq!(evaluate_expression("\"\" and true", &env), Ok(Value::String("".to_owned())));
        assert_eq!(evaluate_expression("\"\" or true", &env), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_unary_expression_right_evaluate_error() {
        let env = new_environment();
        assert_eq!(
            evaluate_expression("!foo", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(0, 0, 0, 4),
                    runtime_error!(
                        RuntimeErrorEnum::VariableNotDeclared,
                        code_span(0, 1, 0, 4),
                    )
                )
            ),
        );
    }

    #[test]
    fn test_unary_expression() {
        let env = new_environment();
        add_native_clock(&env);
        execute_src(
            "
            fun foo() {
                print \"hello\";
            }
            ",
            &env
        );
        assert_eq!(
            evaluate_expression("-nil", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(Value::Nil),
                    code_span(0, 0, 0, 4),
                )
            ),
        );
        assert_eq!(
            evaluate_expression("-true", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(Value::Bool(true)),
                    code_span(0, 0, 0, 5),
                )
            ),
        );
        assert_eq!(
            evaluate_expression("-\"hello\"", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 8),
                )
            ),
        );
        assert_eq!(
            evaluate_expression("-foo", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(environment_get_value(&env, "foo").unwrap()),
                    code_span(0, 0, 0, 4),
                )
            ),
        );
        assert_eq!(
            evaluate_expression("-clock", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(environment_get_value(&env, "clock").unwrap()),
                    code_span(0, 0, 0, 6),
                )
            ),
        );
        assert_eq!(evaluate_expression("-1.0", &env), Ok(Value::Number(-1.0)));
        assert_eq!(evaluate_expression("!nil", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("!true", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("!false", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("!1.0", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("!0.0", &env), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("!\"hello\"", &env), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("!\"\"", &env), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_variable_expression() {
        let env = new_environment();
        execute_src("var foo = true;", &env);
        assert_eq!(evaluate_expression("foo", &env), Ok(Value::Bool(true)));
        assert_eq!(
            evaluate_expression("bar", &env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::VariableNotDeclared,
                    code_span(0, 0, 0, 3),
                ),
            ),
        );
    }
}
