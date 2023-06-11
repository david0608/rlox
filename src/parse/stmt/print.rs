use crate::parse::expr::Expression;
use crate::visitor::Printable;

pub struct PrintStatement<'src>(pub Expression<'src>);

impl std::fmt::Debug for PrintStatement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

#[macro_export]
macro_rules! print_statement {
    ( $expr:expr ) => {
        Box::new(PrintStatement($expr))
    }
}
