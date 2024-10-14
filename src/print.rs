pub trait Print {
    fn print(&self) -> String;
}

#[macro_export]
macro_rules! impl_debug_for_printable {
    ( $target:ty ) => {
        impl std::fmt::Debug for $target {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.print())
            }
        }
    }
}
