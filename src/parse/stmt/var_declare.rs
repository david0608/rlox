use crate::scan::token::IdentToken;
use crate::parse::expr::Expression;
use crate::visitor::Printable;

pub struct VarDeclareStatement<'src> {
    pub name: IdentToken<'src>,
    pub initializer: Option<Expression<'src>>,
}

impl std::fmt::Debug for VarDeclareStatement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

#[macro_export]
macro_rules! var_declare_statement {
    ( $name:expr, $initializer:expr ) => {
        Box::new(
            VarDeclareStatement {
                name: $name,
                initializer: $initializer,
            }
        )
    }
}
