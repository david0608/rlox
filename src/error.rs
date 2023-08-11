pub trait LoxError {
    fn print(&self, src_lines: &Vec<&str>) -> String;
}
