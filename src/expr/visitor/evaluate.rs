use crate::expr::{
    Expr,
    UnaryExpression,
    BinaryExpression,
    LiteralExpression,
    GroupingExpression,
};
use crate::visit::Visit;

#[derive(Debug)]
pub enum Error<'a, 'b> {
    InvalidNegative(&'a UnaryExpression<'b>),
    InvalidCompare(&'a BinaryExpression<'b, 'b>),
    InvalidArithmetic(&'a BinaryExpression<'b, 'b>),
    DivideByZero(String),
}

#[derive(PartialEq, Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(v) => *v,
            Value::Number(v) => *v != 0.0,
            Value::String(v) => v.len() != 0,
        }
    }
}

pub type EvaluateResult<'a, 'b> = std::result::Result<Value, Error<'a, 'b>>;

pub struct Evaluate;

impl<'a, 'b> Visit<'a, UnaryExpression<'b>, EvaluateResult<'a, 'b>> for Evaluate {
    fn visit(e: &'a UnaryExpression<'b>) -> EvaluateResult<'a, 'b> {
        match e {
            UnaryExpression::Negative(rhs) => {
                match rhs.evaluate()? {
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
                Ok(Value::Bool(!rhs.evaluate()?.is_truthy()))
            }
        }
    }
}

impl<'a, 'b> Visit<'a, BinaryExpression<'b, 'b>, EvaluateResult<'a, 'b>> for Evaluate {
    fn visit(e: &'a BinaryExpression<'b, 'b>) -> EvaluateResult<'a, 'b> {
        match e {
            BinaryExpression::Equal(lhs, rhs) => {
                Ok(Value::Bool(lhs.evaluate()? == rhs.evaluate()?))
            }
            BinaryExpression::NotEqual(lhs, rhs) => {
                Ok(Value::Bool(lhs.evaluate()? != rhs.evaluate()?))
            }
            BinaryExpression::Less(lhs, rhs) => {
                match (lhs.evaluate()?, rhs.evaluate()?) {
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
                match (lhs.evaluate()?, rhs.evaluate()?) {
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
                match (lhs.evaluate()?, rhs.evaluate()?) {
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
                match (lhs.evaluate()?, rhs.evaluate()?) {
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
                match (lhs.evaluate()?, rhs.evaluate()?) {
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
                match (lhs.evaluate()?, rhs.evaluate()?) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l - r))
                    }
                    _ => {
                        Err(Error::InvalidArithmetic(e))
                    }
                }
            }
            BinaryExpression::Multiply(lhs, rhs) => {
                match (lhs.evaluate()?, rhs.evaluate()?) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l * r))
                    }
                    _ => {
                        Err(Error::InvalidArithmetic(e))
                    }
                }
            }
            BinaryExpression::Divide(lhs, rhs) => {
                match (lhs.evaluate()?, rhs.evaluate()?) {
                    (Value::Number(l), Value::Number(r)) => {
                        if r == 0.0 {
                            Err(Error::DivideByZero(e.print()))
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

impl<'a, 'b> Visit<'a, LiteralExpression<'b>, EvaluateResult<'a, 'b>> for Evaluate {
    fn visit(e: &'a LiteralExpression<'b>) -> EvaluateResult<'a, 'b> {
        match e {
            LiteralExpression::Nil => Ok(Value::Nil),
            LiteralExpression::True => Ok(Value::Bool(true)),
            LiteralExpression::False => Ok(Value::Bool(false)),
            LiteralExpression::Number(t) => Ok(Value::Number(t.literal())),
            LiteralExpression::String(t) => Ok(Value::String(t.literal().to_string())),
        }
    }
}

impl<'a, 'b> Visit<'a, GroupingExpression<'b>, EvaluateResult<'a, 'b>> for Evaluate {
    fn visit(e: &'a GroupingExpression<'b>) -> EvaluateResult<'a, 'b> {
        e.0.evaluate()
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::Scanner;
    use crate::parser::Parser;
    use super::Value;

    #[test]
    fn test_value_equal() {
        let lvals = [
            Value::Nil,
            Value::Bool(true),
            Value::Bool(false),
            Value::Number(1.0),
            Value::Number(100.001),
            Value::String("hello".to_string()),
            Value::String("world".to_string()),
        ];
        let rvals = [
            Value::Nil,
            Value::Bool(true),
            Value::Bool(false),
            Value::Number(1.0),
            Value::Number(100.001),
            Value::String("hello".to_string()),
            Value::String("world".to_string()),
        ];
        let equals = [
            [true, false, false, false, false, false, false],
            [false, true, false, false, false, false, false],
            [false, false, true, false, false, false, false],
            [false, false, false, true, false, false, false],
            [false, false, false, false, true, false, false],
            [false, false, false, false, false, true, false],
            [false, false, false, false, false, false, true],
        ];
        for (i, equals) in equals.iter().enumerate() {
            for (j, equal) in equals.iter().enumerate() {
                assert_eq!(lvals[i] == rvals[j], *equal);
            }
        }
    }

    #[test]
    fn test_value_is_truthy() {
        let tests = [
            (Value::Nil, false),
            (Value::Bool(true), true),
            (Value::Bool(false), false),
            (Value::Number(1.0), true),
            (Value::Number(0.0), false),
            (Value::String("hello".to_string()), true),
            (Value::String("".to_string()), false),
        ];
        for (value, truthy) in tests {
            assert_eq!(value.is_truthy(), truthy);
        }
    }

    #[test]
    fn test_evaluate() {
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
        ];
        for (src, expect) in tests {
            let scanner = Scanner::scan(src);
            let expression = Parser::new(scanner.tokens()).expression().unwrap();
            match expect {
                Ok(v) => assert_eq!(expression.evaluate().unwrap(), v),
                Err(e) => assert_eq!(format!("{:?}", expression.evaluate().unwrap_err()), e),
            }
        }
    }
}
