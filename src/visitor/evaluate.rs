use std::rc::Rc;
use std::cell::RefCell;
use crate::value::Value;
use crate::scope::{
    Scope,
    Error as ScopeError,
};
use crate::parse::expr::{
    UnaryExpression,
    BinaryExpression,
    LiteralExpression,
    GroupingExpression,
    VariableExpression,
};
use super::{
    ScopeVisit,
    ScopeAccept,
};

#[derive(Debug)]
pub enum Error<'expr, 'src> {
    InvalidNegative(&'expr UnaryExpression<'src>),
    InvalidCompare(&'expr BinaryExpression<'src, 'src>),
    InvalidArithmetic(&'expr BinaryExpression<'src, 'src>),
    DivideByZero(&'expr BinaryExpression<'src, 'src>),
    VariableResolveFailed(&'expr VariableExpression<'src>, ScopeError<'src>),
}

pub type EvaluateResult<'expr, 'src> = std::result::Result<Value, Error<'expr, 'src>>;

pub struct Evaluate;

pub trait Evaluable<'src>
    where
    Self: for<'this> ScopeAccept<'this, Evaluate, EvaluateResult<'this, 'src>>
{
    fn evaluate<'this>(&'this self, scope: &Rc<RefCell<Scope>>) -> EvaluateResult<'this, 'src> {
        self.accept(Evaluate, scope)
    }
}

impl<'src, T> Evaluable<'src> for T
    where
    T: for<'this> ScopeAccept<'this, Evaluate, EvaluateResult<'this, 'src>>
{ }

impl<'that, 'src> ScopeVisit<'that, UnaryExpression<'src>, EvaluateResult<'that, 'src>> for Evaluate {
    fn visit(e: &'that UnaryExpression<'src>, s: &Rc<RefCell<Scope>>) -> EvaluateResult<'that, 'src> {
        match e {
            UnaryExpression::Negative(rhs) => {
                match rhs.evaluate(s)? {
                    Value::Nil => {
                        Err(Error::InvalidNegative(e))
                    }
                    Value::Bool(_) => {
                        Err(Error::InvalidNegative(e))
                    }
                    Value::Number(v) => {
                        Ok(Value::Number(-v))
                    }
                    Value::String(_) => {
                        Err(Error::InvalidNegative(e))
                    }
                }
            }
            UnaryExpression::Not(rhs) => {
                Ok(Value::Bool(!rhs.evaluate(s)?.is_truthy()))
            }
        }
    }
}

impl<'that, 'src> ScopeVisit<'that, BinaryExpression<'src, 'src>, EvaluateResult<'that, 'src>> for Evaluate {
    fn visit(e: &'that BinaryExpression<'src, 'src>, s: &Rc<RefCell<Scope>>) -> EvaluateResult<'that, 'src> {
        match e {
            BinaryExpression::Equal(lhs, rhs) => {
                Ok(Value::Bool(lhs.evaluate(s)? == rhs.evaluate(s)?))
            }
            BinaryExpression::NotEqual(lhs, rhs) => {
                Ok(Value::Bool(lhs.evaluate(s)? != rhs.evaluate(s)?))
            }
            BinaryExpression::Less(lhs, rhs) => {
                match (lhs.evaluate(s)?, rhs.evaluate(s)?) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Bool(l < r))
                    }
                    (Value::String(l), Value::String(r)) => {
                        Ok(Value::Bool(l < r))
                    }
                    _ => {
                        Err(Error::InvalidCompare(e))
                    }
                }
            }
            BinaryExpression::LessEqual(lhs, rhs) => {
                match (lhs.evaluate(s)?, rhs.evaluate(s)?) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Bool(l <= r))
                    }
                    (Value::String(l), Value::String(r)) => {
                        Ok(Value::Bool(l <= r))
                    }
                    _ => {
                        Err(Error::InvalidCompare(e))
                    }
                }
            }
            BinaryExpression::Greater(lhs, rhs) => {
                match (lhs.evaluate(s)?, rhs.evaluate(s)?) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Bool(l > r))
                    }
                    (Value::String(l), Value::String(r)) => {
                        Ok(Value::Bool(l > r))
                    }
                    _ => {
                        Err(Error::InvalidCompare(e))
                    }
                }
            }
            BinaryExpression::GreaterEqual(lhs, rhs) => {
                match (lhs.evaluate(s)?, rhs.evaluate(s)?) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Bool(l >= r))
                    }
                    (Value::String(l), Value::String(r)) => {
                        Ok(Value::Bool(l >= r))
                    }
                    _ => {
                        Err(Error::InvalidCompare(e))
                    }
                }
            }
            BinaryExpression::Plus(lhs, rhs) => {
                match (lhs.evaluate(s)?, rhs.evaluate(s)?) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l + r))
                    }
                    (Value::String(l), Value::String(r)) => {
                        Ok(Value::String(l + &r))
                    }
                    _ => {
                        Err(Error::InvalidArithmetic(e))
                    }
                }
            }
            BinaryExpression::Minus(lhs, rhs) => {
                match (lhs.evaluate(s)?, rhs.evaluate(s)?) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l - r))
                    }
                    _ => {
                        Err(Error::InvalidArithmetic(e))
                    }
                }
            }
            BinaryExpression::Multiply(lhs, rhs) => {
                match (lhs.evaluate(s)?, rhs.evaluate(s)?) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l * r))
                    }
                    _ => {
                        Err(Error::InvalidArithmetic(e))
                    }
                }
            }
            BinaryExpression::Divide(lhs, rhs) => {
                match (lhs.evaluate(s)?, rhs.evaluate(s)?) {
                    (Value::Number(l), Value::Number(r)) => {
                        if r == 0.0 {
                            Err(Error::DivideByZero(e))
                        }
                        else {
                            Ok(Value::Number(l / r))
                        }
                    }
                    _ => {
                        Err(Error::InvalidArithmetic(e))
                    }
                }
            }
        }
    }
}

impl<'that, 'src> ScopeVisit<'that, LiteralExpression<'src>, EvaluateResult<'that, 'src>> for Evaluate {
    fn visit(e: &'that LiteralExpression<'src>, _s: &Rc<RefCell<Scope>>) -> EvaluateResult<'that, 'src> {
        match e {
            LiteralExpression::Nil => Ok(Value::Nil),
            LiteralExpression::True => Ok(Value::Bool(true)),
            LiteralExpression::False => Ok(Value::Bool(false)),
            LiteralExpression::Number(t) => Ok(Value::Number(t.literal())),
            LiteralExpression::String(t) => Ok(Value::String(t.literal().to_string())),
        }
    }
}

impl<'that, 'src> ScopeVisit<'that, GroupingExpression<'src>, EvaluateResult<'that, 'src>> for Evaluate {
    fn visit(e: &'that GroupingExpression<'src>, s: &Rc<RefCell<Scope>>) -> EvaluateResult<'that, 'src> {
        e.0.evaluate(s)
    }
}

impl<'that, 'src> ScopeVisit<'that, VariableExpression<'src>, EvaluateResult<'that, 'src>> for Evaluate {
    fn visit(e: &'that VariableExpression<'src>, s: &Rc<RefCell<Scope>>) -> EvaluateResult<'that, 'src> {
        s.borrow().get_value(e.0.lexeme())
            .map_err(|err| Error::VariableResolveFailed(e, err))
    }
}

#[cfg(test)]
mod tests {
    use crate::value::Value;
    use crate::scope::Scope;
    use crate::visitor::Scannable;
    use crate::parse::Parser;

    #[test]
    fn test_evaluate() {
        let scope = Scope::new().as_rc();
        assert_eq!(scope.borrow_mut().declare("foo", Value::Bool(true)).is_ok(), true);
        let tests: Vec<(&str, Result<Value, &str>)> = vec![
            // Unary negative operations.
            ("-nil", Err("InvalidNegative((- nil))")),
            ("-true", Err("InvalidNegative((- true))")),
            ("-1", Ok(Value::Number(-1.0))),
            ("-\"hello\"", Err("InvalidNegative((- \"hello\"))")),
            // Unary not operations.
            ("!nil", Ok(Value::Bool(true))),
            ("!true", Ok(Value::Bool(false))),
            ("!false", Ok(Value::Bool(true))),
            ("!1", Ok(Value::Bool(false))),
            ("!0", Ok(Value::Bool(true))),
            ("!\"hello\"", Ok(Value::Bool(false))),
            ("!\"\"", Ok(Value::Bool(true))),
            // Binary equal operations.
            ("nil == nil", Ok(Value::Bool(true))),
            ("true == true", Ok(Value::Bool(true))),
            ("false == false", Ok(Value::Bool(true))),
            ("1 == 1", Ok(Value::Bool(true))),
            ("\"hello\" == \"hello\"", Ok(Value::Bool(true))),
            // Binary not equal operations.
            ("nil != true", Ok(Value::Bool(true))),
            ("nil != false", Ok(Value::Bool(true))),
            ("nil != 1", Ok(Value::Bool(true))),
            ("nil != 0", Ok(Value::Bool(true))),
            ("nil != \"hello\"", Ok(Value::Bool(true))),
            ("nil != \"\"", Ok(Value::Bool(true))),
            ("true != nil", Ok(Value::Bool(true))),
            ("true != false", Ok(Value::Bool(true))),
            ("true != 1", Ok(Value::Bool(true))),
            ("true != 0", Ok(Value::Bool(true))),
            ("true != \"hello\"", Ok(Value::Bool(true))),
            ("true != \"\"", Ok(Value::Bool(true))),
            ("false != nil", Ok(Value::Bool(true))),
            ("false != true", Ok(Value::Bool(true))),
            ("false != 1", Ok(Value::Bool(true))),
            ("false != 0", Ok(Value::Bool(true))),
            ("false != \"hello\"", Ok(Value::Bool(true))),
            ("false != \"\"", Ok(Value::Bool(true))),
            ("1 != nil", Ok(Value::Bool(true))),
            ("1 != true", Ok(Value::Bool(true))),
            ("1 != false", Ok(Value::Bool(true))),
            ("1 != 0", Ok(Value::Bool(true))),
            ("1 != \"hello\"", Ok(Value::Bool(true))),
            ("1 != \"\"", Ok(Value::Bool(true))),
            ("0 != nil", Ok(Value::Bool(true))),
            ("0 != true", Ok(Value::Bool(true))),
            ("0 != false", Ok(Value::Bool(true))),
            ("0 != 1", Ok(Value::Bool(true))),
            ("0 != \"hello\"", Ok(Value::Bool(true))),
            ("0 != \"\"", Ok(Value::Bool(true))),
            ("\"hello\" != nil", Ok(Value::Bool(true))),
            ("\"hello\" != true", Ok(Value::Bool(true))),
            ("\"hello\" != false", Ok(Value::Bool(true))),
            ("\"hello\" != 1", Ok(Value::Bool(true))),
            ("\"hello\" != 1", Ok(Value::Bool(true))),
            ("\"hello\" != \"\"", Ok(Value::Bool(true))),
            ("\"\" != nil", Ok(Value::Bool(true))),
            ("\"\" != true", Ok(Value::Bool(true))),
            ("\"\" != false", Ok(Value::Bool(true))),
            ("\"\" != 1", Ok(Value::Bool(true))),
            ("\"\" != 1", Ok(Value::Bool(true))),
            ("\"\" != \"hello\"", Ok(Value::Bool(true))),
            // Binary less operations.
            ("nil < nil", Err("InvalidCompare((< nil nil))")),
            ("nil < true", Err("InvalidCompare((< nil true))")),
            ("nil < 1", Err("InvalidCompare((< nil 1))")),
            ("nil < \"hello\"", Err("InvalidCompare((< nil \"hello\"))")),
            ("true < nil", Err("InvalidCompare((< true nil))")),
            ("true < true", Err("InvalidCompare((< true true))")),
            ("true < 1", Err("InvalidCompare((< true 1))")),
            ("true < \"hello\"", Err("InvalidCompare((< true \"hello\"))")),
            ("1 < nil", Err("InvalidCompare((< 1 nil))")),
            ("1 < true", Err("InvalidCompare((< 1 true))")),
            ("1 < 0", Ok(Value::Bool(false))),
            ("1 < 1", Ok(Value::Bool(false))),
            ("1 < 2", Ok(Value::Bool(true))),
            ("1 < \"hello\"", Err("InvalidCompare((< 1 \"hello\"))")),
            ("\"hello\" < nil", Err("InvalidCompare((< \"hello\" nil))")),
            ("\"hello\" < true", Err("InvalidCompare((< \"hello\" true))")),
            ("\"hello\" < 1", Err("InvalidCompare((< \"hello\" 1))")),
            ("\"hello\" < \"gello\"", Ok(Value::Bool(false))),
            ("\"hello\" < \"hello\"", Ok(Value::Bool(false))),
            ("\"hello\" < \"iello\"", Ok(Value::Bool(true))),
            // Binary less equal operations.
            ("nil <= nil", Err("InvalidCompare((<= nil nil))")),
            ("nil <= true", Err("InvalidCompare((<= nil true))")),
            ("nil <= 1", Err("InvalidCompare((<= nil 1))")),
            ("nil <= \"hello\"", Err("InvalidCompare((<= nil \"hello\"))")),
            ("true <= nil", Err("InvalidCompare((<= true nil))")),
            ("true <= true", Err("InvalidCompare((<= true true))")),
            ("true <= 1", Err("InvalidCompare((<= true 1))")),
            ("true <= \"hello\"", Err("InvalidCompare((<= true \"hello\"))")),
            ("1 <= nil", Err("InvalidCompare((<= 1 nil))")),
            ("1 <= true", Err("InvalidCompare((<= 1 true))")),
            ("1 <= 0", Ok(Value::Bool(false))),
            ("1 <= 1", Ok(Value::Bool(true))),
            ("1 <= 2", Ok(Value::Bool(true))),
            ("1 <= \"hello\"", Err("InvalidCompare((<= 1 \"hello\"))")),
            ("\"hello\" <= nil", Err("InvalidCompare((<= \"hello\" nil))")),
            ("\"hello\" <= true", Err("InvalidCompare((<= \"hello\" true))")),
            ("\"hello\" <= 1", Err("InvalidCompare((<= \"hello\" 1))")),
            ("\"hello\" <= \"gello\"", Ok(Value::Bool(false))),
            ("\"hello\" <= \"hello\"", Ok(Value::Bool(true))),
            ("\"hello\" <= \"iello\"", Ok(Value::Bool(true))),
            // Binary greater operations.
            ("nil > nil", Err("InvalidCompare((> nil nil))")),
            ("nil > true", Err("InvalidCompare((> nil true))")),
            ("nil > 1", Err("InvalidCompare((> nil 1))")),
            ("nil > \"hello\"", Err("InvalidCompare((> nil \"hello\"))")),
            ("true > nil", Err("InvalidCompare((> true nil))")),
            ("true > true", Err("InvalidCompare((> true true))")),
            ("true > 1", Err("InvalidCompare((> true 1))")),
            ("true > \"hello\"", Err("InvalidCompare((> true \"hello\"))")),
            ("1 > nil", Err("InvalidCompare((> 1 nil))")),
            ("1 > true", Err("InvalidCompare((> 1 true))")),
            ("1 > 0", Ok(Value::Bool(true))),
            ("1 > 1", Ok(Value::Bool(false))),
            ("1 > 2", Ok(Value::Bool(false))),
            ("1 > \"hello\"", Err("InvalidCompare((> 1 \"hello\"))")),
            ("\"hello\" > nil", Err("InvalidCompare((> \"hello\" nil))")),
            ("\"hello\" > true", Err("InvalidCompare((> \"hello\" true))")),
            ("\"hello\" > 1", Err("InvalidCompare((> \"hello\" 1))")),
            ("\"hello\" > \"gello\"", Ok(Value::Bool(true))),
            ("\"hello\" > \"hello\"", Ok(Value::Bool(false))),
            ("\"hello\" > \"iello\"", Ok(Value::Bool(false))),
            // Binary greater equal operations.
            ("nil >= nil", Err("InvalidCompare((>= nil nil))")),
            ("nil >= true", Err("InvalidCompare((>= nil true))")),
            ("nil >= 1", Err("InvalidCompare((>= nil 1))")),
            ("nil >= \"hello\"", Err("InvalidCompare((>= nil \"hello\"))")),
            ("true >= nil", Err("InvalidCompare((>= true nil))")),
            ("true >= true", Err("InvalidCompare((>= true true))")),
            ("true >= 1", Err("InvalidCompare((>= true 1))")),
            ("true >= \"hello\"", Err("InvalidCompare((>= true \"hello\"))")),
            ("1 >= nil", Err("InvalidCompare((>= 1 nil))")),
            ("1 >= true", Err("InvalidCompare((>= 1 true))")),
            ("1 >= 0", Ok(Value::Bool(true))),
            ("1 >= 1", Ok(Value::Bool(true))),
            ("1 >= 2", Ok(Value::Bool(false))),
            ("1 >= \"hello\"", Err("InvalidCompare((>= 1 \"hello\"))")),
            ("\"hello\" >= nil", Err("InvalidCompare((>= \"hello\" nil))")),
            ("\"hello\" >= true", Err("InvalidCompare((>= \"hello\" true))")),
            ("\"hello\" >= 1", Err("InvalidCompare((>= \"hello\" 1))")),
            ("\"hello\" >= \"gello\"", Ok(Value::Bool(true))),
            ("\"hello\" >= \"hello\"", Ok(Value::Bool(true))),
            ("\"hello\" >= \"iello\"", Ok(Value::Bool(false))),
            // Binary plus operations.
            ("nil + nil", Err("InvalidArithmetic((+ nil nil))")),
            ("nil + true", Err("InvalidArithmetic((+ nil true))")),
            ("nil + 1", Err("InvalidArithmetic((+ nil 1))")),
            ("nil + \"hello\"", Err("InvalidArithmetic((+ nil \"hello\"))")),
            ("true + nil", Err("InvalidArithmetic((+ true nil))")),
            ("true + true", Err("InvalidArithmetic((+ true true))")),
            ("true + 1", Err("InvalidArithmetic((+ true 1))")),
            ("true + \"hello\"", Err("InvalidArithmetic((+ true \"hello\"))")),
            ("1 + nil", Err("InvalidArithmetic((+ 1 nil))")),
            ("1 + true", Err("InvalidArithmetic((+ 1 true))")),
            ("1 + 1", Ok(Value::Number(2.0))),
            ("1 + \"hello\"", Err("InvalidArithmetic((+ 1 \"hello\"))")),
            ("\"hello\" + nil", Err("InvalidArithmetic((+ \"hello\" nil))")),
            ("\"hello\" + true", Err("InvalidArithmetic((+ \"hello\" true))")),
            ("\"hello\" + 1", Err("InvalidArithmetic((+ \"hello\" 1))")),
            ("\"hello\" + \"hello\"", Ok(Value::String("hellohello".to_string()))),
            // Binary minus operations.
            ("nil - nil", Err("InvalidArithmetic((- nil nil))")),
            ("nil - true", Err("InvalidArithmetic((- nil true))")),
            ("nil - 1", Err("InvalidArithmetic((- nil 1))")),
            ("nil - \"hello\"", Err("InvalidArithmetic((- nil \"hello\"))")),
            ("true - nil", Err("InvalidArithmetic((- true nil))")),
            ("true - true", Err("InvalidArithmetic((- true true))")),
            ("true - 1", Err("InvalidArithmetic((- true 1))")),
            ("true - \"hello\"", Err("InvalidArithmetic((- true \"hello\"))")),
            ("1 - nil", Err("InvalidArithmetic((- 1 nil))")),
            ("1 - true", Err("InvalidArithmetic((- 1 true))")),
            ("1 - 1", Ok(Value::Number(0.0))),
            ("1 - \"hello\"", Err("InvalidArithmetic((- 1 \"hello\"))")),
            ("\"hello\" - nil", Err("InvalidArithmetic((- \"hello\" nil))")),
            ("\"hello\" - true", Err("InvalidArithmetic((- \"hello\" true))")),
            ("\"hello\" - 1", Err("InvalidArithmetic((- \"hello\" 1))")),
            ("\"hello\" - \"hello\"", Err("InvalidArithmetic((- \"hello\" \"hello\"))")),
            // Binary multiply operations.
            ("nil * nil", Err("InvalidArithmetic((* nil nil))")),
            ("nil * true", Err("InvalidArithmetic((* nil true))")),
            ("nil * 1", Err("InvalidArithmetic((* nil 1))")),
            ("nil * \"hello\"", Err("InvalidArithmetic((* nil \"hello\"))")),
            ("true * nil", Err("InvalidArithmetic((* true nil))")),
            ("true * true", Err("InvalidArithmetic((* true true))")),
            ("true * 1", Err("InvalidArithmetic((* true 1))")),
            ("true * \"hello\"", Err("InvalidArithmetic((* true \"hello\"))")),
            ("1 * nil", Err("InvalidArithmetic((* 1 nil))")),
            ("1 * true", Err("InvalidArithmetic((* 1 true))")),
            ("2 * 2", Ok(Value::Number(4.0))),
            ("1 * \"hello\"", Err("InvalidArithmetic((* 1 \"hello\"))")),
            ("\"hello\" * nil", Err("InvalidArithmetic((* \"hello\" nil))")),
            ("\"hello\" * true", Err("InvalidArithmetic((* \"hello\" true))")),
            ("\"hello\" * 1", Err("InvalidArithmetic((* \"hello\" 1))")),
            ("\"hello\" * \"hello\"", Err("InvalidArithmetic((* \"hello\" \"hello\"))")),
            // Binary divide operations.
            ("nil / nil", Err("InvalidArithmetic((/ nil nil))")),
            ("nil / true", Err("InvalidArithmetic((/ nil true))")),
            ("nil / 1", Err("InvalidArithmetic((/ nil 1))")),
            ("nil / \"hello\"", Err("InvalidArithmetic((/ nil \"hello\"))")),
            ("true / nil", Err("InvalidArithmetic((/ true nil))")),
            ("true / true", Err("InvalidArithmetic((/ true true))")),
            ("true / 1", Err("InvalidArithmetic((/ true 1))")),
            ("true / \"hello\"", Err("InvalidArithmetic((/ true \"hello\"))")),
            ("1 / nil", Err("InvalidArithmetic((/ 1 nil))")),
            ("1 / true", Err("InvalidArithmetic((/ 1 true))")),
            ("2 / 2", Ok(Value::Number(1.0))),
            ("1 / \"hello\"", Err("InvalidArithmetic((/ 1 \"hello\"))")),
            ("\"hello\" / nil", Err("InvalidArithmetic((/ \"hello\" nil))")),
            ("\"hello\" / true", Err("InvalidArithmetic((/ \"hello\" true))")),
            ("\"hello\" / 1", Err("InvalidArithmetic((/ \"hello\" 1))")),
            ("\"hello\" / \"hello\"", Err("InvalidArithmetic((/ \"hello\" \"hello\"))")),
            // Literals.
            ("nil", Ok(Value::Nil)),
            ("true", Ok(Value::Bool(true))),
            ("false", Ok(Value::Bool(false))),
            ("0", Ok(Value::Number(0.0))),
            ("0.0", Ok(Value::Number(0.0))),
            ("123", Ok(Value::Number(123.0))),
            ("1.23", Ok(Value::Number(1.230))),
            ("\"hello\"", Ok(Value::String("hello".to_string()))),
            // Grouping.
            ("(nil)", Ok(Value::Nil)),
            ("(true)", Ok(Value::Bool(true))),
            ("(123)", Ok(Value::Number(123.0))),
            ("(\"hello\")", Ok(Value::String("hello".to_string()))),
            ("2 * (3 - 1)", Ok(Value::Number(4.0))),
            // Variable.
            ("foo", Ok(Value::Bool(true))),
            ("!foo", Ok(Value::Bool(false))),
            ("!!foo", Ok(Value::Bool(true))),
            ("bar", Err("VariableResolveFailed(bar, NotDeclared(\"bar\"))")),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let expression = Parser::new(&tokens).expression().unwrap();
            match expect {
                Ok(v) => assert_eq!(expression.evaluate(&scope).unwrap(), v),
                Err(e) => assert_eq!(format!("{:?}", expression.evaluate(&scope).unwrap_err()), e),
            }
        }
    }
}
