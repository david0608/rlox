use crate::parse::expr::Expression;
use crate::visitor::Printable;

use super::Statement;

pub struct IfStatement<'src> {
    pub condition: Expression<'src>,
    pub then_stmt: Statement<'src>,
    pub else_stmt: Option<Statement<'src>>,
}

impl std::fmt::Debug for IfStatement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

#[macro_export]
macro_rules! if_statement {
    ( $condition:expr, $then_stmt:expr, $else_stmt:expr ) => {
        Box::new(
            IfStatement {
                condition: $condition,
                then_stmt: $then_stmt,
                else_stmt: $else_stmt,
            }
        )
    }
}
