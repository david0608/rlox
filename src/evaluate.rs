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
use crate::scope::{
    Scope,
    ScopeError,
};
use crate::runtime_error;

pub type EvaluateResult = std::result::Result<Value, RuntimeError>;

pub trait Evaluate {
    fn evaluate(&self, scope: &Rc<RefCell<Scope>>) -> EvaluateResult;
}

impl Evaluate for AssignExpression {
    fn evaluate(&self, scope: &Rc<RefCell<Scope>>) -> EvaluateResult {
        let v = match self.value().evaluate(scope) {
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
        match scope.borrow_mut().set_value(self.name(), v.clone()) {
            Ok(_) => {
                Ok(v)
            }
            Err(err) => {
                match err {
                    ScopeError::NotDeclared => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::VariableNotDeclared,
                                self.code_span(),
                            )
                        )
                    }
                    ScopeError::GlobalVariableMutationNotSupport => {
                        Err(
                            runtime_error!(
                                RuntimeErrorEnum::GlobalVariableMutation,
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
    fn evaluate(&self, scope: &Rc<RefCell<Scope>>) -> EvaluateResult {
        let lhs = match self.lhs().evaluate(scope) {
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
        let rhs = match self.rhs().evaluate(scope) {
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
    fn evaluate(&self, scope: &Rc<RefCell<Scope>>) -> EvaluateResult {
        let callee = match self.callee().evaluate(scope) {
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
                match a.evaluate(scope) {
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
        match callee.call(scope, arguments) {
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
    fn evaluate(&self, scope: &Rc<RefCell<Scope>>) -> EvaluateResult {
        self.expression().evaluate(scope)
    }
}

impl Evaluate for LiteralExpression {
    fn evaluate(&self, _scope: &Rc<RefCell<Scope>>) -> EvaluateResult {
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
    fn evaluate(&self, scope: &Rc<RefCell<Scope>>) -> EvaluateResult {
        let lv = match self.lhs().evaluate(scope) {
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

        let rv = match self.rhs().evaluate(scope) {
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
    fn evaluate(&self, scope: &Rc<RefCell<Scope>>) -> EvaluateResult {
        let rhs = match self.rhs().evaluate(scope) {
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
    fn evaluate(&self, scope: &Rc<RefCell<Scope>>) -> EvaluateResult {
        match scope.borrow().get_value(self.name()) {
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
    use crate::error::{
        RuntimeError,
        RuntimeErrorEnum,
    };
    use crate::evaluate::EvaluateResult;
    use crate::scope::Scope;
    use crate::runtime_error;

    fn code_span(sl: usize, sc: usize, el: usize, ec: usize) -> CodeSpan {
        CodeSpan::new(CodePoint::new(sl, sc), CodePoint::new(el, ec))
    }

    fn create_scope() -> Rc<RefCell<Scope>> {
        Scope::new().as_rc()
    }

    fn execute_src(src: &str, scope: &Rc<RefCell<Scope>>) {
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        for stmt in stmts {
            stmt.execute(scope).unwrap();
        }
    }

    fn evaluate_expression(src: &str, scope: &Rc<RefCell<Scope>>) -> EvaluateResult {
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        Parser::new(&tokens).expression().unwrap().evaluate(scope)
    }


    #[test]
    fn test_assign_expression() {
        let scope = create_scope();
        execute_src("var foo = false;", &scope);
        assert_eq!(
            evaluate_expression("foo = true", &scope),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn test_assign_expression_evaluate_error() {
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("foo = bar", &scope),
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
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("foo = true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::VariableNotDeclared,
                    code_span(0, 0, 0, 10),
                )
            )
        );
    }

    #[test]
    fn test_assign_expression_global_variable_mutation_not_support_error() {
        let scope = create_scope();
        execute_src("var foo = false;", &scope);
        let scope = Scope::new_isolate_child(&scope).as_rc();
        execute_src("print foo;", &scope);
        assert_eq!(
            evaluate_expression("foo = true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::GlobalVariableMutation,
                    code_span(0, 0, 0, 10),
                )
            )
        );
    }

    #[test]
    fn test_binary_expression_operand_evaluate_error() {
        let scope = create_scope();
        execute_src("var foo = true;", &scope);
        assert_eq!(
            evaluate_expression("bar == foo", &scope),
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
            evaluate_expression("foo == bar", &scope),
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
        let scope = create_scope();
        assert_eq!(evaluate_expression("nil == nil", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("nil == true", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("nil == false", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("nil == 1", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("nil == 0", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("nil == \"hello\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("nil == \"\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true == nil", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true == true", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true == false", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true == 1", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true == 0", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true == \"hello\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true == \"\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false == nil", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false == true", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false == false", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false == 1", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false == 0", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false == \"hello\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false == \"\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 == nil", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 == true", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 == false", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 == 1", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 == 0", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 == \"hello\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 == \"\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 == nil", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 == true", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 == false", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 == 1", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 == 0", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 == \"hello\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 == \"\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" == nil", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" == true", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" == false", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" == 1", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" == 0", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" == \"hello\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" == \"\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == nil", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == true", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == false", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == 1", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == 0", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == \"hello\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"\" == \"\"", &scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_binary_expression_not_equal() {
        let scope = create_scope();
        assert_eq!(evaluate_expression("nil != nil", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("nil != true", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("nil != false", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("nil != 1", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("nil != 0", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("nil != \"hello\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("nil != \"\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true != nil", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true != true", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("true != false", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true != 1", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true != 0", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true != \"hello\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("true != \"\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false != nil", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false != true", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false != false", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false != 1", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false != 0", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false != \"hello\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false != \"\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 != nil", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 != true", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 != false", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 != 1", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 != 0", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 != \"hello\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 != \"\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 != nil", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 != true", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 != false", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 != 1", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 != 0", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("0 != \"hello\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("0 != \"\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" != nil", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" != true", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" != false", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" != 1", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" != 0", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" != \"hello\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" != \"\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != nil", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != true", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != false", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != 1", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != 0", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != \"hello\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"\" != \"\"", &scope), Ok(Value::Bool(false)));
    }

    #[test]
    fn test_binary_expression_less() {
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("nil < nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil < true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil < 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil < \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true < nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true < true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true < 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true < \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 < nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 < true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_expression("1 < 0", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 < 1", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 < 2", &scope), Ok(Value::Bool(true)));
        assert_eq!(
            evaluate_expression("1 < \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" < nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" < true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" < 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(evaluate_expression("\"hello\" < \"gello\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" < \"hello\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" < \"iello\"", &scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_binary_expression_less_equal() {
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("nil <= nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil <= true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil <= 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil <= \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true <= nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true <= true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true <= 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true <= \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 <= nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 <= true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(evaluate_expression("1 <= 0", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 <= 1", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 <= 2", &scope), Ok(Value::Bool(true)));
        assert_eq!(
            evaluate_expression("1 <= \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" <= nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" <= true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" <= 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(evaluate_expression("\"hello\" <= \"gello\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" <= \"hello\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" <= \"iello\"", &scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_binary_expression_greater() {
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("nil > nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil > true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil > 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil > \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true > nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true > true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true > 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true > \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 > nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 > true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_expression("1 > 0", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 > 1", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("1 > 2", &scope), Ok(Value::Bool(false)));
        assert_eq!(
            evaluate_expression("1 > \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" > nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" > true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" > 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(evaluate_expression("\"hello\" > \"gello\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" > \"hello\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("\"hello\" > \"iello\"", &scope), Ok(Value::Bool(false)));
    }

    #[test]
    fn test_binary_expression_greater_equal() {
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("nil >= nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil >= true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil >= 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil >= \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true >= nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true >= true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true >= 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true >= \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 >= nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 >= true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(evaluate_expression("1 >= 0", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 >= 1", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 >= 2", &scope), Ok(Value::Bool(false)));
        assert_eq!(
            evaluate_expression("1 >= \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" >= nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" >= true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" >= 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(evaluate_expression("\"hello\" >= \"gello\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" >= \"hello\"", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"hello\" >= \"iello\"", &scope), Ok(Value::Bool(false)));
    }

    #[test]
    fn test_binary_expression_plus() {
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("nil + nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil + true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil + 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil + \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true + nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true + true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true + 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true + \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 + nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 + true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_expression("1 + 1", &scope), Ok(Value::Number(2.0)));
        assert_eq!(
            evaluate_expression("1 + \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" + nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" + true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" + 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(evaluate_expression("\"hello\" + \"hello\"", &scope), Ok(Value::String("hellohello".to_owned())));
    }

    #[test]
    fn test_binary_expression_minus() {
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("nil - nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil - true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil - 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil - \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true - nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true - true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true - 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true - \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 - nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 - true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_expression("1 - 1", &scope), Ok(Value::Number(0.0)));
        assert_eq!(
            evaluate_expression("1 - \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" - nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" - true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" - 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" - \"hello\"", &scope),
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
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("nil * nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil * true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil * 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil * \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true * nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true * true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true * 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true * \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 * nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 * true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_expression("2 * 2", &scope), Ok(Value::Number(4.0)));
        assert_eq!(
            evaluate_expression("1 * \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" * nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" * true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" * 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" * \"hello\"", &scope),
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
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("nil / nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil / true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil / 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("nil / \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true / nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true / true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true / 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_expression("true / \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 / nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 / true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_expression("2 / 2", &scope), Ok(Value::Number(1.0)));
        assert_eq!(
            evaluate_expression("1 / 0", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::DivideByZero(Value::Number(1.0), Value::Number(0.0)),
                    code_span(0, 0, 0, 5),
                )
            )
        );
        assert_eq!(
            evaluate_expression("1 / \"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" / nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" / true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" / 1", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_expression("\"hello\" / \"hello\"", &scope),
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
        let scope = create_scope();
        execute_src(
            "
            fun foo(a, b) {
                return a + b;
            }
            ",
            &scope,
        );
        assert_eq!(
            evaluate_expression("foo(1, 2)", &scope),
            Ok(Value::Number(3.0)),
        );
    }

    #[test]
    fn test_call_expression_callee_evaluate_error() {
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("foo(1, 2)", &scope),
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
        let scope = create_scope();
        execute_src(
            "
            fun foo(a, b) {
                return a + b;
            }
            ",
            &scope,
        );
        assert_eq!(
            evaluate_expression("foo(a, b)", &scope),
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
        let scope = create_scope();
        execute_src(
            "
            fun foo(a, b) {
                return a + b;
            }
            ",
            &scope,
        );
        assert_eq!(
            evaluate_expression("foo(1, 2, 3)", &scope),
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
        let scope = create_scope();
        execute_src(
            "
            var foo = true;
            ",
            &scope,
        );
        assert_eq!(
            evaluate_expression("foo()", &scope),
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
        let scope = create_scope();
        execute_src(
            "
            fun foo() {
                return bar;
            }
            ",
            &scope,
        );
        assert_eq!(
            evaluate_expression("foo()", &scope),
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
        let scope = create_scope();
        assert_eq!(evaluate_expression("(nil)", &scope), Ok(Value::Nil));
        assert_eq!(evaluate_expression("(true)", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("(123)", &scope), Ok(Value::Number(123.0)));
        assert_eq!(evaluate_expression("(\"hello\")", &scope), Ok(Value::String("hello".to_owned())));
        assert_eq!(evaluate_expression("2 * (3 - 1)", &scope), Ok(Value::Number(4.0)));
        assert_eq!(
            evaluate_expression("(true + 1)", &scope),
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
        let scope = create_scope();
        assert_eq!(evaluate_expression("nil", &scope), Ok(Value::Nil));
        assert_eq!(evaluate_expression("true", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("123", &scope), Ok(Value::Number(123.0)));
        assert_eq!(evaluate_expression("1.23", &scope), Ok(Value::Number(1.23)));
        assert_eq!(evaluate_expression("\"hello\"", &scope), Ok(Value::String("hello".to_owned())));
    }

    #[test]
    fn test_logical_expression_left_evaluate_error() {
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("foo or true", &scope),
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
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("true and foo", &scope),
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
        let scope = create_scope();
        assert_eq!(evaluate_expression("nil and \"foo\"", &scope), Ok(Value::Nil));
        assert_eq!(evaluate_expression("nil or \"foo\"", &scope), Ok(Value::String("foo".to_owned())));
        assert_eq!(evaluate_expression("true and 1", &scope), Ok(Value::Number(1.0)));
        assert_eq!(evaluate_expression("true or 1", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("false and 1", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("false or 1", &scope), Ok(Value::Number(1.0)));
        assert_eq!(evaluate_expression("1 and true", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("1 or true", &scope), Ok(Value::Number(1.0)));
        assert_eq!(evaluate_expression("0 and true", &scope), Ok(Value::Number(0.0)));
        assert_eq!(evaluate_expression("0 or true", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"foo\" and true", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("\"foo\" or true", &scope), Ok(Value::String("foo".to_owned())));
        assert_eq!(evaluate_expression("\"\" and true", &scope), Ok(Value::String("".to_owned())));
        assert_eq!(evaluate_expression("\"\" or true", &scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_unary_expression_right_evaluate_error() {
        let scope = create_scope();
        assert_eq!(
            evaluate_expression("!foo", &scope),
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
        let scope = create_scope();
        add_native_clock(&scope);
        execute_src(
            "
            fun foo() {
                print \"hello\";
            }
            ",
            &scope
        );
        assert_eq!(
            evaluate_expression("-nil", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(Value::Nil),
                    code_span(0, 0, 0, 4),
                )
            ),
        );
        assert_eq!(
            evaluate_expression("-true", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(Value::Bool(true)),
                    code_span(0, 0, 0, 5),
                )
            ),
        );
        assert_eq!(
            evaluate_expression("-\"hello\"", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(Value::String("hello".to_owned())),
                    code_span(0, 0, 0, 8),
                )
            ),
        );
        assert_eq!(
            evaluate_expression("-foo", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(scope.borrow().get_value("foo").unwrap()),
                    code_span(0, 0, 0, 4),
                )
            ),
        );
        assert_eq!(
            evaluate_expression("-clock", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::InvalidNegate(scope.borrow().get_value("clock").unwrap()),
                    code_span(0, 0, 0, 6),
                )
            ),
        );
        assert_eq!(evaluate_expression("-1.0", &scope), Ok(Value::Number(-1.0)));
        assert_eq!(evaluate_expression("!nil", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("!true", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("!false", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("!1.0", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("!0.0", &scope), Ok(Value::Bool(true)));
        assert_eq!(evaluate_expression("!\"hello\"", &scope), Ok(Value::Bool(false)));
        assert_eq!(evaluate_expression("!\"\"", &scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn test_variable_expression() {
        let scope = create_scope();
        execute_src("var foo = true;", &scope);
        assert_eq!(evaluate_expression("foo", &scope), Ok(Value::Bool(true)));
        assert_eq!(
            evaluate_expression("bar", &scope),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::VariableNotDeclared,
                    code_span(0, 0, 0, 3),
                ),
            ),
        );
    }
}
