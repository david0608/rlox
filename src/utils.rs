#[macro_export]
macro_rules! downcast_ref {
    ( $from:expr, $to:ty ) => {
        $from.as_any_ref().downcast_ref::<$to>().unwrap()
    }
}

#[macro_export]
macro_rules! box_downcast {
    ( $from:expr, $to:ty ) => {
        $from.as_box_any().downcast::<$to>().unwrap()
    }
}

use std::any::Any;

pub trait AsAny {
    fn as_any_ref(&self) -> &dyn Any;

    fn as_box_any(self) -> Box<dyn Any>;
}

#[cfg(test)]
pub mod test_utils {
    use crate::parse::{
        parser::Parser,
        expression::{
            Expression,
            BoxedExpression,
        },
        statement::{
            Statement,
            BoxedStatement,
        },
    };
    use crate::scan::Scan;
    use crate::environment::{
        Environment,
        EnvironmentOps,
    };
    use crate::evaluate::EvaluateResult;
    use crate::execute::ExecuteResult;
    use crate::resolve::{
        ResolveCtx,
        ResolveError,
    };
    use crate::utils::AsAny;

    pub fn parse_expression<T: Expression>(src: &str) -> Box<T> {
        box_downcast!(parse_expression_unknown(src), T)
    }

    pub fn parse_expression_unknown(src: &str) -> BoxedExpression {
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let mut parser = Parser::new(&tokens);
        parser.expression().unwrap()
    }

    pub fn parse_statement<T: Statement>(src: &str) -> Box<T> {
        box_downcast!(parse_statement_unknown(src), T)
    }

    pub fn parse_statement_unknown(src: &str) -> BoxedStatement {
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let mut parser = Parser::new(&tokens);
        parser.statement(true).unwrap()
    }

    pub struct TestContext {
        pub environment: Environment,
        pub resolve_context: ResolveCtx,
    }

    impl TestContext {
        pub fn new() -> TestContext {
            TestContext {
                environment: <Environment as EnvironmentOps>::new(),
                resolve_context: ResolveCtx::new(),
            }
        }

        pub fn resolve_expression<T>(
            &mut self,
            expr: &dyn Expression
        )
            -> Result<Box<T>, ResolveError>
        where
            T: Expression
        {
            self.resolve_expression_unknown(expr)
                .map(|expr| box_downcast!(expr, T))
        }

        pub fn resolve_expression_unknown(
            &mut self,
            expr: &dyn Expression
        )
            -> Result<BoxedExpression, ResolveError>
        {
            expr.resolve(&mut self.resolve_context)
        }

        pub fn resolve_statement<T>(
            &mut self,
            stmt: &dyn Statement
        )
            -> Result<Box<T>, ResolveError>
        where
            T: Statement
        {
            self.resolve_statement_unknown(stmt)
                .map(|stmt| box_downcast!(stmt, T))
        }

        pub fn resolve_statement_unknown(
            &mut self,
            stmt: &dyn Statement
        )
            -> Result<BoxedStatement, ResolveError>
        {
            stmt.resolve(&mut self.resolve_context)
        }

        pub fn evaluate(&mut self, expr: &dyn Expression) -> EvaluateResult {
            let expr = self.resolve_expression_unknown(expr).unwrap();
            expr.evaluate(&self.environment)
        }

        pub fn execute(&mut self, stmt: &dyn Statement) -> ExecuteResult {
            let stmt = self.resolve_statement_unknown(stmt).unwrap();
            stmt.execute(&self.environment)
        }

        pub fn execute_src(&mut self, src: &str) {
            let stmt = parse_statement_unknown(src);
            self.execute(stmt.as_ref()).expect("execute src");
        }
    }
}
