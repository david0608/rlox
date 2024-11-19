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
    error::{
        RuntimeError,
        RuntimeErrorEnum,
    },
    evaluate::Evaluate,
    print::Print,
    resolve::{
        ResolveCtx,
        ResolveError,
    },
    impl_debug_for_printable,
};

#[derive(Clone, Copy)]
pub enum BinaryExpressionEnum {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Multiply,
    Divide,
}

pub struct BinaryExpression {
    variant: BinaryExpressionEnum,
    lhs: Rc<dyn Expression>,
    rhs: Rc<dyn Expression>,
    code_span: CodeSpan,
}

impl BinaryExpression {
    pub fn new(
        variant: BinaryExpressionEnum,
        lhs: Rc<dyn Expression>,
        rhs: Rc<dyn Expression>,
        code_span: CodeSpan,
    ) -> BinaryExpression
    {
        BinaryExpression {
            variant,
            lhs,
            rhs,
            code_span,
        }
    }

    pub fn variant(&self) -> BinaryExpressionEnum {
        self.variant
    }

    pub fn lhs(&self) -> &Rc<dyn Expression> {
        &self.lhs
    }

    pub fn rhs(&self) -> &Rc<dyn Expression> {
        &self.rhs
    }
}

impl Code for BinaryExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for BinaryExpression {
    fn print(&self) -> String {
        match self.variant() {
            BinaryExpressionEnum::Equal => format!("(== {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::NotEqual => format!("(!= {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::Less => format!("(< {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::LessEqual => format!("(<= {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::Greater => format!("(> {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::GreaterEqual => format!("(>= {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::Plus => format!("(+ {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::Minus => format!("(- {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::Multiply => format!("(* {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::Divide => format!("(/ {} {})", self.lhs().print(), self.rhs().print()),
        }
    }
}

impl_debug_for_printable!(BinaryExpression);

impl Evaluate for BinaryExpression {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let lhs = match self.lhs().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(RuntimeError::wrap(err, self.code_span()));
            }
        };
        let rhs = match self.rhs().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(RuntimeError::wrap(err, self.code_span()));
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
                            RuntimeError::new(
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
                            RuntimeError::new(
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
                            RuntimeError::new(
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
                            RuntimeError::new(
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
                            RuntimeError::new(
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
                            RuntimeError::new(
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
                            RuntimeError::new(
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
                                RuntimeError::new(
                                    RuntimeErrorEnum::DivideByZero,
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
                            RuntimeError::new(
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

impl Expression for BinaryExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Expression>, ResolveError> {
        Ok(
            Rc::new(
                BinaryExpression::new(
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
macro_rules! binary_expression {
    ( $variant:ident, $lhs:expr, $rhs:expr, $code_span:expr ) => {
        Rc::new(
            BinaryExpression::new(
                BinaryExpressionEnum::$variant,
                $lhs,
                $rhs,
                $code_span
            )
        )
    };
}

#[cfg(test)]
mod tests {
    use crate::{
        code::code_span::new_code_span,
        parse::expression::{
            binary::BinaryExpression,
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
    fn test_print_binary_expression() {
        let tests: Vec<(&str, &str)> = vec![
            ("1 == 2", "(== 1 2)"),
            ("1 != 2", "(!= 1 2)"),
            ("1 < 2", "(< 1 2)"),
            ("1 <= 2", "(<= 1 2)"),
            ("1 > 2", "(> 1 2)"),
            ("1 >= 2", "(>= 1 2)"),
            ("1 + 2", "(+ 1 2)"),
            ("1 - 2", "(- 1 2)"),
            ("1 * 2", "(* 1 2)"),
            ("1 / 2", "(/ 1 2)"),
        ];
        for (src, expect) in tests {
            assert_eq!(
                parse_expression::<BinaryExpression>(src).print(),
                expect,
            );
        }
    }

    #[test]
    fn test_binary_expression_equal_evaluate() {
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
    fn test_binary_expression_not_equal_evaluate() {
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
    fn test_binary_expression_less_evaluate() {
        assert_eq!(
            evaluate_src("nil < nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil < true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil < 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil < \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("true < nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("true < true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true < 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("true < \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 < nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 < true"),
            Err(
                RuntimeError::new(
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
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" < nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" < true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" < 1"),
            Err(
                RuntimeError::new(
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
    fn test_binary_expression_less_equal_evaluate() {
        assert_eq!(
            evaluate_src("nil <= nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil <= true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil <= 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil <= \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("true <= nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true <= true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_src("true <= 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("true <= \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 <= nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 <= true"),
            Err(
                RuntimeError::new(
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
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" <= nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" <= true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" <= 1"),
            Err(
                RuntimeError::new(
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
    fn test_binary_expression_greater_evaluate() {
        assert_eq!(
            evaluate_src("nil > nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil > true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil > 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil > \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("true > nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("true > true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true > 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("true > \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 > nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 > true"),
            Err(
                RuntimeError::new(
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
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" > nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" > true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" > 1"),
            Err(
                RuntimeError::new(
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
    fn test_binary_expression_greater_equal_evaluate() {
        assert_eq!(
            evaluate_src("nil >= nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil >= true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil >= 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil >= \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("true >= nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true >= true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_src("true >= 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("true >= \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 >= nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 >= true"),
            Err(
                RuntimeError::new(
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
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 12),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" >= nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" >= true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 15),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" >= 1"),
            Err(
                RuntimeError::new(
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
    fn test_binary_expression_plus_evaluate() {
        assert_eq!(
            evaluate_src("nil + nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil + true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil + 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil + \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("true + nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("true + true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true + 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("true + \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 + nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 + true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_src("1 + 1"), Ok(Value::Number(2.0)));
        assert_eq!(
            evaluate_src("1 + \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" + nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" + true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" + 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(evaluate_src("\"hello\" + \"hello\""), Ok(Value::String("hellohello".to_owned())));
    }

    #[test]
    fn test_binary_expression_minus_evaluate() {
        assert_eq!(
            evaluate_src("nil - nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil - true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil - 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil - \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("true - nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("true - true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true - 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("true - \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 - nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 - true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_src("1 - 1"), Ok(Value::Number(0.0)));
        assert_eq!(
            evaluate_src("1 - \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" - nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" - true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" - 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" - \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 17),
                )
            )
        );
    }

    #[test]
    fn test_binary_expression_multiply_evaluate() {
        assert_eq!(
            evaluate_src("nil * nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil * true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil * 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil * \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("true * nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("true * true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true * 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("true * \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 * nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 * true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_src("2 * 2"), Ok(Value::Number(4.0)));
        assert_eq!(
            evaluate_src("1 * \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" * nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" * true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" * 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" * \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 17),
                )
            )
        );
    }

    #[test]
    fn test_binary_expression_divide_evaluate() {
        assert_eq!(
            evaluate_src("nil / nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Nil),
                    new_code_span(0, 0, 0, 9),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil / true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Bool(true)),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil / 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::Number(1.0)),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("nil / \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Nil, Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("true / nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Nil),
                    new_code_span(0, 0, 0, 10),
                )
            )
        );
        assert_eq!(
            evaluate_src("true / true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Bool(true)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("true / 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(
            evaluate_src("true / \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 / nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Nil),
                    new_code_span(0, 0, 0, 7),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 / true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 0, 0, 8),
                )
            )
        );
        assert_eq!(evaluate_src("2 / 2"), Ok(Value::Number(1.0)));
        assert_eq!(
            evaluate_src("1 / 0"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::DivideByZero,
                    new_code_span(0, 0, 0, 5),
                )
            )
        );
        assert_eq!(
            evaluate_src("1 / \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" / nil"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Nil),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" / true"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Bool(true)),
                    new_code_span(0, 0, 0, 14),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" / 1"),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::Number(1.0)),
                    new_code_span(0, 0, 0, 11),
                )
            )
        );
        assert_eq!(
            evaluate_src("\"hello\" / \"hello\""),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidArithmetic(Value::String("hello".to_owned()), Value::String("hello".to_owned())),
                    new_code_span(0, 0, 0, 17),
                )
            )
        );
    }

    #[test]
    fn test_binary_expression_evaluate_operand_evaluate_error() {
        let mut ctx = TestContext::new();
        let expr = ctx.resolve_expression::<BinaryExpression>(
            parse_expression_unknown("1 + true == false").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Err(
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                        new_code_span(0, 0, 0, 8),
                    ),
                    new_code_span(0, 0, 0, 17),
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
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(false), Value::Number(1.0)),
                        new_code_span(0, 8, 0, 17),
                    ),
                    new_code_span(0, 0, 0, 17),
                )
            )
        );
    }

    #[test]
    fn test_binary_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.resolve_context.begin();
        ctx.execute_src("var bar;");

        let binary_expr = ctx.resolve_expression::<BinaryExpression>(
            parse_expression_unknown("foo == bar").as_ref()
        )
            .unwrap();
        assert_eq!(
            binary_expr.lhs().downcast_ref::<VariableExpression>().unwrap().binding(),
            1
        );
        assert_eq!(
            binary_expr.rhs().downcast_ref::<VariableExpression>().unwrap().binding(),
            0
        );
    }

    #[test]
    fn test_binary_expression_resolve_lhs_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<BinaryExpression>("foo == bar").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 0, 0, 3)
            )
        );
    }

    #[test]
    fn test_binary_expression_resolve_rhs_resolve_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<BinaryExpression>("foo == bar").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 7, 0, 10)
            )
        );
    }
}
