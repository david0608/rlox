use std::rc::Rc;
use std::cell::RefCell;
use crate::value::Value;
use crate::scope::{
    Scope,
    Error as ScopeError,
};
use crate::scan::span::Span;
use crate::parse::expr::Expr;
use crate::parse::expr::assign::AssignExpression;
use crate::parse::expr::binary::{
    BinaryExpression,
    BinaryExpressionEnum,
};
use crate::parse::expr::grouping::GroupingExpression;
use crate::parse::expr::literal::{
    LiteralExpression,
    LiteralExpressionEnum,
};
use crate::parse::expr::logical::{
    LogicalExpression,
    LogicalExpressionEnum,
};
use crate::parse::expr::unary::{
    UnaryExpression,
    UnaryExpressionEnum,
};
use crate::parse::expr::variable::VariableExpression;
use super::{
    ScopeVisit,
    ScopeAccept,
};

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidNegate(Span, Value),
    InvalidCompare(Span, Value, Value),
    InvalidArithmetic(Span, Value, Value),
    DivideByZero(Span, Value, Value),
    VariableNotDeclared(Span),
    GlobalVariableMutation(Span),
    Unknown(Span),
}

pub type EvaluateResult = std::result::Result<Value, Error>;

pub struct Evaluate;

pub trait Evaluable
    where
    Self: for<'this> ScopeAccept<'this, Evaluate, EvaluateResult>
{
    fn evaluate(&self, scope: &Rc<RefCell<Scope>>) -> EvaluateResult {
        self.accept(Evaluate, scope)
    }
}

impl<T> Evaluable for T
    where
    T: for<'this> ScopeAccept<'this, Evaluate, EvaluateResult>
{ }

impl<'that> ScopeVisit<'that, UnaryExpression, EvaluateResult> for Evaluate {
    fn visit(e: &'that UnaryExpression, s: &Rc<RefCell<Scope>>) -> EvaluateResult {
        match e.variant() {
            UnaryExpressionEnum::Negative => {
                let rhs = e.rhs().evaluate(s)?;
                match rhs {
                    Value::Nil => {
                        Err(Error::InvalidNegate(e.span(), rhs))
                    }
                    Value::Bool(_) => {
                        Err(Error::InvalidNegate(e.span(), rhs))
                    }
                    Value::Number(rhs) => {
                        Ok(Value::Number(-rhs))
                    }
                    Value::String(_) => {
                        Err(Error::InvalidNegate(e.span(), rhs))
                    }
                }
            }
            UnaryExpressionEnum::Not => {
                Ok(Value::Bool(!e.rhs().evaluate(s)?.is_truthy()))
            }
        }
    }
}

impl<'that> ScopeVisit<'that, BinaryExpression, EvaluateResult> for Evaluate {
    fn visit(e: &'that BinaryExpression, s: &Rc<RefCell<Scope>>) -> EvaluateResult {
        let lhs = e.lhs().evaluate(s)?;
        let rhs = e.rhs().evaluate(s)?;
        match e.variant() {
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
                        Err(Error::InvalidCompare(e.span(), lhs, rhs))
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
                        Err(Error::InvalidCompare(e.span(), lhs, rhs))
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
                        Err(Error::InvalidCompare(e.span(), lhs, rhs))
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
                        Err(Error::InvalidCompare(e.span(), lhs, rhs))
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
                        Err(Error::InvalidArithmetic(e.span(), lhs, rhs))
                    }
                }
            }
            BinaryExpressionEnum::Minus => {
                match (&lhs, &rhs) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l - r))
                    }
                    _ => {
                        Err(Error::InvalidArithmetic(e.span(), lhs, rhs))
                    }
                }
            }
            BinaryExpressionEnum::Multiply => {
                match (&lhs, &rhs) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l * r))
                    }
                    _ => {
                        Err(Error::InvalidArithmetic(e.span(), lhs, rhs))
                    }
                }
            }
            BinaryExpressionEnum::Divide => {
                match (&lhs, &rhs) {
                    (Value::Number(l), Value::Number(r)) => {
                        if *r == 0.0 {
                            Err(Error::DivideByZero(e.span(), lhs, rhs))
                        }
                        else {
                            Ok(Value::Number(l / r))
                        }
                    }
                    _ => {
                        Err(Error::InvalidArithmetic(e.span(), lhs, rhs))
                    }
                }
            }
        }
    }
}

impl<'that> ScopeVisit<'that, LiteralExpression, EvaluateResult> for Evaluate {
    fn visit(e: &'that LiteralExpression, _s: &Rc<RefCell<Scope>>) -> EvaluateResult {
        match e.variant() {
            LiteralExpressionEnum::Nil => Ok(Value::Nil),
            LiteralExpressionEnum::True => Ok(Value::Bool(true)),
            LiteralExpressionEnum::False => Ok(Value::Bool(false)),
            LiteralExpressionEnum::Number(t) => Ok(Value::Number(t.literal())),
            LiteralExpressionEnum::String(t) => Ok(Value::String(t.literal().to_string())),
        }
    }
}

impl<'that> ScopeVisit<'that, GroupingExpression, EvaluateResult> for Evaluate {
    fn visit(e: &'that GroupingExpression, s: &Rc<RefCell<Scope>>) -> EvaluateResult {
        e.expr().evaluate(s)
    }
}

impl<'that> ScopeVisit<'that, VariableExpression, EvaluateResult> for Evaluate {
    fn visit(e: &'that VariableExpression, s: &Rc<RefCell<Scope>>) -> EvaluateResult {
        s.borrow().get_value(e.name())
            .map_err(|_| Error::VariableNotDeclared(e.span()))
    }
}

impl<'that> ScopeVisit<'that, AssignExpression, EvaluateResult> for Evaluate {
    fn visit(e: &'that AssignExpression, s: &Rc<RefCell<Scope>>) -> EvaluateResult {
        let v = e.value().evaluate(s)?;
        match s.borrow_mut().set_value(e.name(), v.clone()) {
            Ok(_) => {
                Ok(v)
            }
            Err(err) => {
                match err {
                    ScopeError::NotDeclared => {
                        Err(Error::VariableNotDeclared(e.span()))
                    }
                    ScopeError::GlobalVariableMutationNotSupport => {
                        Err(Error::GlobalVariableMutation(e.span()))
                    }
                    _ => {
                        Err(Error::Unknown(e.span()))
                    }
                }
            }
        }
    }
}

impl<'that> ScopeVisit<'that, LogicalExpression, EvaluateResult> for Evaluate {
    fn visit(e: &'that LogicalExpression, scope: &Rc<RefCell<Scope>>) -> EvaluateResult {
        match e.variant() {
            LogicalExpressionEnum::And => {
                let lv = e.lhs().evaluate(scope)?;
                if !lv.is_truthy() {
                    return Ok(lv);
                }
                else {
                    return Ok(e.rhs().evaluate(scope)?);
                }
            }
            LogicalExpressionEnum::Or => {
                let lv = e.lhs().evaluate(scope)?;
                if lv.is_truthy() {
                    return Ok(lv);
                }
                else {
                    return Ok(e.rhs().evaluate(scope)?);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::value::Value;
    use crate::scope::Scope;
    use crate::scan::span::{
        Span,
        CodePoint,
    };
    use crate::parse::Parser;
    use crate::visitor::Scannable;
    use super::Error;

    fn src_span(src: &str) -> Span {
        Span::new(CodePoint::new(0, 0), CodePoint::new(0, src.len()))
    }

    macro_rules! test_value {
        ( nil ) => {
            Value::Nil
        };

        ( true ) => {
            Value::Bool(true)
        };

        ( false ) => {
            Value::Bool(false)
        };

        ( 1 ) => {
            Value::Number(1.0)
        };

        ( 0.0 ) => {
            Value::Number(0.0)
        };

        ( 1.0 ) => {
            Value::Number(1.0)
        };

        ( -1.0 ) => {
            Value::Number(-1.0)
        };

        ( 2.0 ) => {
            Value::Number(2.0)
        };

        ( 4.0 ) => {
            Value::Number(4.0)
        };

        ( "hello" ) => {
            Value::String("hello".to_owned())
        };

        ( "hellohello" ) => {
            Value::String("hellohello".to_owned())
        };
    }

    macro_rules! ok_test_case {
        ( $lhs:tt, $op:tt, $rhs:tt, $($val:tt)+ ) => {
            (
                stringify!($lhs$op$rhs),
                Ok(test_value!($($val)+)),
            )
        };

        ( $op:tt, $rhs:tt, $($val:tt)+ ) => {
            (
                stringify!($op$rhs),
                Ok(test_value!($($val)+)),
            )
        };
    }

    macro_rules! err_test_case {
        ( $lhs:tt, $op:tt, $rhs:tt, $err:tt ) => {
            (
                stringify!($lhs$op$rhs),
                Err(
                    Error::$err(
                        src_span(stringify!($lhs$op$rhs)),
                        test_value!($lhs),
                        test_value!($rhs),
                    )
                )
            )
        };

        ( $op:tt, $rhs:tt, $err:tt ) => {
            (
                stringify!($op$rhs),
                Err(
                    Error::$err(
                        src_span(stringify!($op$rhs)),
                        test_value!($rhs),
                    )
                )
            )
        };
    }

    #[test]
    fn test_evaluate() {
        let scope = Scope::new().as_rc();
        scope.borrow_mut().declare("foo", Value::Bool(true)).unwrap();

        let tests: Vec<(&str, Result<Value, Error>)> = vec![
            // Unary negative operations.
            err_test_case!(-, nil, InvalidNegate),
            err_test_case!(-, true, InvalidNegate),
            ok_test_case!(-, 1, -1.0),
            err_test_case!(-, "hello", InvalidNegate),
            // Unary not operations.
            ok_test_case!(!, nil, true),
            ok_test_case!(!, true, false),
            ok_test_case!(!, false, true),
            ok_test_case!(!, 1, false),
            ok_test_case!(!, 0,true),
            ok_test_case!(!, "hello", false),
            ok_test_case!(!, "", true),
            // Binary equal operations.
            ok_test_case!(nil, ==, nil, true),
            ok_test_case!(nil, ==, true, false),
            ok_test_case!(true, ==, true, true),
            ok_test_case!(false, ==, false, true),
            ok_test_case!(false, ==, true, false),
            ok_test_case!(1, ==, 1, true),
            ok_test_case!(1, ==, 2, false),
            ok_test_case!("hello", ==, "hello", true),
            // Binary not equal operations.
            ok_test_case!(nil, !=, nil, false),
            ok_test_case!(nil, !=, true, true),
            ok_test_case!(nil, !=, false, true),
            ok_test_case!(nil, !=, 1, true),
            ok_test_case!(nil, !=, 0, true),
            ok_test_case!(nil, !=, "hello", true),
            ok_test_case!(nil, !=, "", true),
            ok_test_case!(true, !=, nil, true),
            ok_test_case!(true, !=, false, true),
            ok_test_case!(true, !=, 1, true),
            ok_test_case!(true, !=, 0, true),
            ok_test_case!(true, !=, "hello", true),
            ok_test_case!(true, !=, "", true),
            ok_test_case!(false, !=, nil, true),
            ok_test_case!(false, !=, true, true),
            ok_test_case!(false, !=, 1, true),
            ok_test_case!(false, !=, 0, true),
            ok_test_case!(false, !=, "hello", true),
            ok_test_case!(false, !=, "", true),
            ok_test_case!(1, !=, nil, true),
            ok_test_case!(1, !=, true, true),
            ok_test_case!(1, !=, false, true),
            ok_test_case!(1, !=, 0, true),
            ok_test_case!(1, !=, "hello", true),
            ok_test_case!(1, !=, "", true),
            ok_test_case!(0, !=, nil, true),
            ok_test_case!(0, !=, true, true),
            ok_test_case!(0, !=, false, true),
            ok_test_case!(0, !=, 1, true),
            ok_test_case!(0, !=, "hello", true),
            ok_test_case!(0, !=, "", true),
            ok_test_case!("hello", !=, nil, true),
            ok_test_case!("hello", !=, true, true),
            ok_test_case!("hello", !=, false, true),
            ok_test_case!("hello", !=, 1, true),
            ok_test_case!("hello", !=, "", true),
            ok_test_case!("", !=, nil, true),
            ok_test_case!("", !=, true, true),
            ok_test_case!("", !=, false, true),
            ok_test_case!("", !=, 1, true),
            ok_test_case!("", !=, "hello", true),
            // Binary less operations.
            err_test_case!(nil, <, nil, InvalidCompare),
            err_test_case!(nil, <, true, InvalidCompare),
            err_test_case!(nil, <, 1, InvalidCompare),
            err_test_case!(nil, <, "hello", InvalidCompare),
            err_test_case!(true, <, nil, InvalidCompare),
            err_test_case!(true, <, true, InvalidCompare),
            err_test_case!(true, <, 1, InvalidCompare),
            err_test_case!(true, <, "hello", InvalidCompare),
            err_test_case!(1, <, nil, InvalidCompare),
            err_test_case!(1, <, true, InvalidCompare),
            ok_test_case!(1, <, 0, false),
            ok_test_case!(1, <, 1, false),
            ok_test_case!(1, <, 2, true),
            err_test_case!(1, < ,"hello", InvalidCompare),
            err_test_case!("hello", <, nil, InvalidCompare),
            err_test_case!("hello", <, true, InvalidCompare),
            err_test_case!("hello", <, 1, InvalidCompare),
            ok_test_case!("hello", <, "gello", false),
            ok_test_case!("hello", <, "hello", false),
            ok_test_case!("hello", <, "iello", true),
            // Binary less equal operations.
            err_test_case!(nil, <=, nil, InvalidCompare),
            err_test_case!(nil, <=, true, InvalidCompare),
            err_test_case!(nil, <=, 1, InvalidCompare),
            err_test_case!(nil, <=, "hello", InvalidCompare),
            err_test_case!(true, <=, nil, InvalidCompare),
            err_test_case!(true, <=, true, InvalidCompare),
            err_test_case!(true, <=, 1, InvalidCompare),
            err_test_case!(true, <=, "hello", InvalidCompare),
            err_test_case!(1, <=, nil, InvalidCompare),
            err_test_case!(1, <=, true, InvalidCompare),
            ok_test_case!(1, <=, 0, false),
            ok_test_case!(1, <=, 1, true),
            ok_test_case!(1, <=, 2, true),
            err_test_case!(1, <=, "hello", InvalidCompare),
            err_test_case!("hello", <=, nil, InvalidCompare),
            err_test_case!("hello", <=, true, InvalidCompare),
            err_test_case!("hello", <=, 1, InvalidCompare),
            ok_test_case!("hello", <=, "gello", false),
            ok_test_case!("hello", <=, "hello", true),
            ok_test_case!("hello", <=, "iello", true),
            // Binary greater operations.
            err_test_case!(nil, >, nil, InvalidCompare),
            err_test_case!(nil, >, true, InvalidCompare),
            err_test_case!(nil, >, 1, InvalidCompare),
            err_test_case!(nil, >, "hello", InvalidCompare),
            err_test_case!(true, >, nil, InvalidCompare),
            err_test_case!(true, >, true, InvalidCompare),
            err_test_case!(true, >, 1, InvalidCompare),
            err_test_case!(true, >, "hello", InvalidCompare),
            err_test_case!(1, >, nil, InvalidCompare),
            err_test_case!(1, >, true, InvalidCompare),
            ok_test_case!(1, >, 0, true),
            ok_test_case!(1, >, 1, false),
            ok_test_case!(1, >, 2, false),
            err_test_case!(1, >, "hello", InvalidCompare),
            err_test_case!("hello", >, nil, InvalidCompare),
            err_test_case!("hello", >, true, InvalidCompare),
            err_test_case!("hello", >, 1, InvalidCompare),
            ok_test_case!("hello", >, "gello", true),
            ok_test_case!("hello", >, "hello", false),
            ok_test_case!("hello", >, "iello", false),
            // Binary greater equal operations.
            err_test_case!(nil, >=, nil, InvalidCompare),
            err_test_case!(nil, >=, true, InvalidCompare),
            err_test_case!(nil, >=, 1, InvalidCompare),
            err_test_case!(nil, >=, "hello", InvalidCompare),
            err_test_case!(true, >=, nil, InvalidCompare),
            err_test_case!(true, >=, true, InvalidCompare),
            err_test_case!(true, >=, 1, InvalidCompare),
            err_test_case!(true, >=, "hello", InvalidCompare),
            err_test_case!(1, >=, nil, InvalidCompare),
            err_test_case!(1, >=, true, InvalidCompare),
            ok_test_case!(1, >=, 0, true),
            ok_test_case!(1, >=, 1, true),
            ok_test_case!(1, >=, 2, false),
            err_test_case!(1, >=, "hello", InvalidCompare),
            err_test_case!("hello", >=, nil, InvalidCompare),
            err_test_case!("hello", >=, true, InvalidCompare),
            err_test_case!("hello", >=, 1, InvalidCompare),
            ok_test_case!("hello", >=, "gello", true),
            ok_test_case!("hello", >=, "hello", true),
            ok_test_case!("hello", >=, "iello", false),
            // Binary plus operations.
            err_test_case!(nil, +, nil, InvalidArithmetic),
            err_test_case!(nil, +, true, InvalidArithmetic),
            err_test_case!(nil, +, 1, InvalidArithmetic),
            err_test_case!(nil, +, "hello", InvalidArithmetic),
            err_test_case!(true, +, nil, InvalidArithmetic),
            err_test_case!(true, +, true, InvalidArithmetic),
            err_test_case!(true, +, 1, InvalidArithmetic),
            err_test_case!(true, +, "hello", InvalidArithmetic),
            err_test_case!(1, +, nil, InvalidArithmetic),
            err_test_case!(1, +, true, InvalidArithmetic),
            ok_test_case!(1, +, 1, 2.0),
            err_test_case!(1, +, "hello", InvalidArithmetic),
            err_test_case!("hello", +, nil, InvalidArithmetic),
            err_test_case!("hello", +, true, InvalidArithmetic),
            err_test_case!("hello", +, 1, InvalidArithmetic),
            ok_test_case!("hello", +, "hello", "hellohello"),
            // Binary minus operations.
            err_test_case!(nil, -, nil, InvalidArithmetic),
            err_test_case!(nil, -, true, InvalidArithmetic),
            err_test_case!(nil, -, 1, InvalidArithmetic),
            err_test_case!(nil, -, "hello", InvalidArithmetic),
            err_test_case!(true, -, nil, InvalidArithmetic),
            err_test_case!(true, -, true, InvalidArithmetic),
            err_test_case!(true, -, 1, InvalidArithmetic),
            err_test_case!(true, -, "hello", InvalidArithmetic),
            err_test_case!(1, -, nil, InvalidArithmetic),
            err_test_case!(1, -, true, InvalidArithmetic),
            ok_test_case!(1, -, 1, 0.0),
            err_test_case!(1, -, "hello", InvalidArithmetic),
            err_test_case!("hello", -, nil, InvalidArithmetic),
            err_test_case!("hello", -, true, InvalidArithmetic),
            err_test_case!("hello", -, 1, InvalidArithmetic),
            err_test_case!("hello", -, "hello", InvalidArithmetic),
            // Binary multiply operations.
            err_test_case!(nil, *, nil, InvalidArithmetic),
            err_test_case!(nil, *, true, InvalidArithmetic),
            err_test_case!(nil, *, 1, InvalidArithmetic),
            err_test_case!(nil, *, "hello", InvalidArithmetic),
            err_test_case!(true, *, nil, InvalidArithmetic),
            err_test_case!(true, *, true, InvalidArithmetic),
            err_test_case!(true, *, 1, InvalidArithmetic),
            err_test_case!(true, *, "hello", InvalidArithmetic),
            err_test_case!(1, *, nil, InvalidArithmetic),
            err_test_case!(1, *, true, InvalidArithmetic),
            ok_test_case!(2, *, 2, 4.0),
            err_test_case!(1, *, "hello", InvalidArithmetic),
            err_test_case!("hello", *, nil, InvalidArithmetic),
            err_test_case!("hello", *, true, InvalidArithmetic),
            err_test_case!("hello", *, 1, InvalidArithmetic),
            err_test_case!("hello", *, "hello", InvalidArithmetic),
            // Binary divide operations.
            err_test_case!(nil, /, nil, InvalidArithmetic),
            err_test_case!(nil, /, true, InvalidArithmetic),
            err_test_case!(nil, /, 1, InvalidArithmetic),
            err_test_case!(nil, /, "hello", InvalidArithmetic),
            err_test_case!(true, /, nil, InvalidArithmetic),
            err_test_case!(true, /, true, InvalidArithmetic),
            err_test_case!(true, /, 1, InvalidArithmetic),
            err_test_case!(true, /, "hello", InvalidArithmetic),
            err_test_case!(1, /, nil, InvalidArithmetic),
            err_test_case!(1, /, true, InvalidArithmetic),
            ok_test_case!(2, /, 2, 1.0),
            err_test_case!(1, /, "hello", InvalidArithmetic),
            err_test_case!("hello", /, nil, InvalidArithmetic),
            err_test_case!("hello", /, true, InvalidArithmetic),
            err_test_case!("hello", /, 1, InvalidArithmetic),
            err_test_case!("hello", /, "hello", InvalidArithmetic),
            // Literals.
            ("nil", Ok(Value::Nil)),
            ("true", Ok(Value::Bool(true))),
            ("false", Ok(Value::Bool(false))),
            ("0", Ok(Value::Number(0.0))),
            ("0.0", Ok(Value::Number(0.0))),
            ("123", Ok(Value::Number(123.0))),
            ("1.23", Ok(Value::Number(1.230))),
            ("\"hello\"", Ok(Value::String("hello".to_owned()))),
            // Grouping.
            ("(nil)", Ok(Value::Nil)),
            ("(true)", Ok(Value::Bool(true))),
            ("(123)", Ok(Value::Number(123.0))),
            ("(\"hello\")", Ok(Value::String("hello".to_owned()))),
            ("2 * (3 - 1)", Ok(Value::Number(4.0))),
            // Variable.
            ("foo", Ok(Value::Bool(true))),
            ("!foo", Ok(Value::Bool(false))),
            ("!!foo", Ok(Value::Bool(true))),
            ("bar", Err(Error::VariableNotDeclared(src_span("bar")))),
            // Assignment.
            ("foo = false", Ok(Value::Bool(false))),
            ("foo = true", Ok(Value::Bool(true))),
            ("bar = true", Err(Error::VariableNotDeclared(src_span("bar = true")))),
            // Logical.
            ("1 or 2", Ok(Value::Number(1.0))),
            ("0 or 2", Ok(Value::Number(2.0))),
            ("1 and 2", Ok(Value::Number(2.0))),
            ("0 and 2", Ok(Value::Number(0.0))),
            ("0 or 1 and \"hello\"", Ok(Value::String("hello".to_string()))),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let expression = Parser::new(&tokens).expression().unwrap();
            match expect {
                Ok(v) => assert_eq!(expression.evaluate(&scope).unwrap(), v),
                Err(e) => assert_eq!(expression.evaluate(&scope).unwrap_err(), e),
            }
        }
    }
}
