use crate::scan::scanner::{
    Scanner,
    ScannerOutput,
};
use super::{
    Visit,
    Accept,
};

pub struct Scan;

pub trait Scannable
    where
    Self: for<'this> Accept<'this, Scan, ScannerOutput>
{
    fn scan(&self) -> ScannerOutput {
        self.accept(Scan)
    }
}

impl<T> Scannable for T
    where
    T: for<'this> Accept<'this, Scan, ScannerOutput>
        + ?Sized
{ }

impl Visit<'_, str, ScannerOutput> for Scan {
    fn visit(value: &str) -> ScannerOutput {
        Scanner::scan(value)
    }
}
