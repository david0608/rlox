use crate::scan::{
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
    Self: for<'this> Accept<'this, Scan, ScannerOutput<'this>>
{
    fn scan(&self) -> ScannerOutput {
        self.accept(Scan)
    }
}

impl<T> Scannable for T
    where
    T: for<'this> Accept<'this, Scan, ScannerOutput<'this>>
        + ?Sized
{ }

impl<'that> Visit<'that, str, ScannerOutput<'that>> for Scan {
    fn visit(value: &'that str) -> ScannerOutput<'that> {
        Scanner::scan(value)
    }
}
