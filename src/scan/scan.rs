use visit::{
    Visit,
    Accept,
};
use super::scanner::{
    Scanner,
    ScannerOutput,
};

struct Scan;

pub trait Scannable
    where
    Self: for<'a> Accept<'a, Scan, ScannerOutput<'a>>
{
    fn scan(&self) -> ScannerOutput {
        self.accept(Scan)
    }
}

impl<T> Scannable for T
    where
    T: for<'a> Accept<'a, Scan, ScannerOutput<'a>>
        + ?Sized
{ }

impl<'a> Visit<'a, str, ScannerOutput<'a>> for Scan {
    fn visit(value: &'a str) -> ScannerOutput<'a> {
        Scanner::scan(value)
    }
}
