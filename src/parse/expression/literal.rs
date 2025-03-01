use std::{
    rc::Rc,
    cell::RefCell,
    collections::HashSet,
};
use crate::{
    code::{
        Code,
        CodeSpan,
    },
    parse::expression::Expression,
    scan::token::{
        number::NumberToken,
        simple::{
            TRUE_LEXEME,
            FALSE_LEXEME,
            NIL_LEXEME,
        },
        string::StringToken,
    },
    value::Value,
    environment::Environment,
    error::{
        RuntimeError,
        ResolveError,
    },
};

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralExpressionEnum {
    Number(Rc<NumberToken>),
    String(Rc<StringToken>),
    True,
    False,
    Nil
}

#[derive(Debug)]
pub struct LiteralExpression {
    variant: LiteralExpressionEnum,
    code_span: CodeSpan,
}

impl LiteralExpression {
    pub fn new(variant: LiteralExpressionEnum, code_span: CodeSpan) -> LiteralExpression {
        LiteralExpression {
            variant,
            code_span,
        }
    }

    pub fn variant(&self) -> LiteralExpressionEnum {
        self.variant.clone()
    }
}

impl Code for LiteralExpression {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        match self.variant() {
            LiteralExpressionEnum::True => TRUE_LEXEME.to_string(),
            LiteralExpressionEnum::False => FALSE_LEXEME.to_string(),
            LiteralExpressionEnum::Nil => NIL_LEXEME.to_string(),
            LiteralExpressionEnum::Number(t) => t.lexeme().to_string(),
            LiteralExpressionEnum::String(t) => t.lexeme().to_string(),
        }
    }
}

impl Expression for LiteralExpression {
    fn resolve(&self, _: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Expression>, ResolveError> {
        Ok(
            Rc::new(
                LiteralExpression::new(
                    self.variant.clone(),
                    self.code_span.clone(),
                )
            )
        )
    }

    fn evaluate(&self, _: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        match self.variant() {
            LiteralExpressionEnum::Nil => Ok(Value::Nil),
            LiteralExpressionEnum::True => Ok(Value::Bool(true)),
            LiteralExpressionEnum::False => Ok(Value::Bool(false)),
            LiteralExpressionEnum::Number(t) => Ok(Value::Number(t.literal())),
            LiteralExpressionEnum::String(t) => Ok(Value::String(t.literal().to_string())),
        }
    }
}

#[macro_export]
macro_rules! literal_expression {
    ( $variant:ident, $code_span:expr ) => {
        Rc::new(
            LiteralExpression::new(
                LiteralExpressionEnum::$variant,
                $code_span,
            )
        )
    };

    ( $variant:ident, $token:expr, $code_span:expr ) => {
        Rc::new(
            LiteralExpression::new(
                LiteralExpressionEnum::$variant($token),
                $code_span,
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        code::{
            Code,
            CodeSpan,
        },
        parse::expression::literal::{
            LiteralExpression,
            LiteralExpressionEnum,
        },
        value::Value,
        utils::test_utils::{
            TestContext,
            parse_expression,
            parse_expression_unknown,
            evaluate_src
        }
    };

    #[test]
    fn test_literal_expression_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("true", "true"),
            ("false", "false"),
            ("nil", "nil"),
            ("123.456", "123.456"),
            ("\"hello\"", "\"hello\""),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_expression::<LiteralExpression>(src).to_string(), expect);
        }
    }

    #[test]
    fn test_literal_expression_evaluate() {
        assert_eq!(evaluate_src("nil"), Ok(Value::Nil));
        assert_eq!(evaluate_src("true"), Ok(Value::Bool(true)));
        assert_eq!(evaluate_src("false"), Ok(Value::Bool(false)));
        assert_eq!(evaluate_src("123"), Ok(Value::Number(123.0)));
        assert_eq!(evaluate_src("1.23"), Ok(Value::Number(1.23)));
        assert_eq!(evaluate_src("\"hello\""), Ok(Value::String("hello".to_owned())));
    }

    #[test]
    fn test_literal_expression_resolve() {
        let mut ctx = TestContext::new();
        let literal_expr = ctx.resolve_expression::<LiteralExpression>(
            parse_expression_unknown("true").as_ref()
        )
            .unwrap();
        assert_eq!(
            literal_expr.variant(),
            LiteralExpressionEnum::True,
        );
        assert_eq!(
            literal_expr.code_span(),
            &CodeSpan::new(0, 0, 0, 4),
        );
    }
}
