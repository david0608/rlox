use crate::scan::token::IdentToken;
use crate::parse::expr::Expression;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;

pub struct VarDeclareStatement<'src> {
    pub name: IdentToken<'src>,
    pub initializer: Option<Expression<'src>>,
}

impl_debug_for_printable!(VarDeclareStatement<'_>);

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
